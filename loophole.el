;;; loophole.el --- Manage temporary key bindings -*- lexical-binding: t -*-

;; Copyright (C) 2020, 2021 0x60DF

;; Author: 0x60DF <0x60df@gmail.com>
;; Created: 30 Aug 2020
;; Version: 0.4.3
;; Keywords: convenience
;; URL: https://github.com/0x60df/loophole
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Loophole provides temporary key bindings management feature.
;; Keys can be set by interactive interface in disposable keymaps
;; which are automatically generated for temporary use.

;;; Code:

(require 'kmacro)

(declare-function seq-drop "seq")
(declare-function seq-every-p "seq")

(defgroup loophole nil
  "Manage temporary key bindings."
  :group 'convenience)

(defvar-local loophole--map-alist nil
  "Alist of keymaps for loophole.
Syntax is same as `minor-mode-map-alist', i.e. each element
looks like (STATE-VARIABLE . KEYMAP).  STATE-VARIABLE is a
symbol whose boolean value represents if the KEYMAP is
active or not.  KEYMAP is a keymap object.
STATE-VARIABLE, KEYMAP and map-variable which holds KEYMAP
must be unique for each element of this variable.")

(defvar loophole--buffer-list nil
  "List of buffers on which Loophole variables have local value.
`loophole-mode' maintains this variable as up to date.
When `loophole-mode' is disabled, this variable is set as t
in order to tell that this variable is not maintained.")

(defvar-local loophole--editing nil
  "Variable of keymap which is currently being edited.
Loophole binds keys in this keymap.  When this value is nil,
Loophole may generate new keymap and bind keys in it.")

(defvar loophole--suspended nil
  "Non-nil if Loophole is suspended manually.
Once Loophole is resumed, this comes back to nil.
To see true state of suspension, use
`loophole-suspending-p' instead of this variable.")

(defvar-local loophole--timer-alist nil
  "Alist of timer for disabling loophole map.
Each element looks like (MAP-VARIABLE . TIMER).
MAP-VARIABLE is a symbol which holds keymap.
TIMER is a timer for MAP-VARIABLE on current buffer.")

(defvar-local loophole--editing-timer nil
  "Timer for stopping editing loophole map.")

(defvar loophole--read-map-variable-help-condition
  '((index . 0) (last))
  "Condition of help for `loophole-read-map-variable'.")

(defvar loophole-write-lisp-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map emacs-lisp-mode-map)
    (define-key map (kbd "C-c C-c") #'loophole-finish-writing-lisp)
    (define-key map (kbd "C-c C-k") #'loophole-abort-writing-lisp)
    map)
  "Keymap for `loophole-write-lisp-mode'.")

(defvar loophole-kmacro-by-recursive-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c [") #'loophole-end-kmacro)
    (define-key map (kbd "C-c \\") #'loophole-abort-kmacro)
    map)
  "Keymap for `loophole-obtain-key-and-kmacro-by-recursive-edit'.
This map is enabled temporarily during
`loophole-obtain-key-and-kmacro-by-recursive-edit',
and activity of this map is controled by
`loophole-use-kmacro-by-recursive-edit-map'.")

(defvar loophole-base-map (make-sparse-keymap)
  "Base keymap for all Loophole maps.
This keymap will be inherited to all Loophole maps,
except for the case user explicitly decline inheritance
when `loophole-register'.")

(defcustom loophole-temporary-map-max 8
  "Maximum number of temporary keymaps.
When the number of bound temporary keymaps is
`loophole-temporary-map-max' or higher, generating new map
overwrites the disabled earliest used one, unregistered one
or enabled earliest used one."
  :group 'loophole
  :type 'integer)

(defcustom loophole-allow-keyboard-quit t
  "If non-nil, binding commands can be quit even while reading keys."
  :group 'loophole
  :type 'boolean)

(defcustom loophole-timer-delay (* 60 60)
  "Delay time in seconds for auto disabling timer."
  :group 'loophole
  :type 'number)

(defcustom loophole-editing-timer-delay (* 60 5)
  "Delay time in seconds for auto stopping editing timer."
  :group 'loophole
  :type 'number)

(defcustom loophole-command-by-lambda-form-format
  (concat "(lambda (&optional arg)\n"
          "  \"Temporary command on `loophole'.\"\n"
          "  (interactive \"P\")\n"
          "  (#))")
  "Format for writing lambda form buffer.
This is used by
`loophole-obtain-key-and-command-by-lambda-form'.
Character sequence (#) indicates where cursor will be
placed, and it will be removed when the format is inserted
in the buffer."
  :risky t
  :group 'loophole
  :type 'string)

(defcustom loophole-kmacro-by-read-key-finish-key (where-is-internal
                                                   'keyboard-quit nil t)
  "Key sequence to finish definition of keyboard macro.
This is used by
`loophole-obtain-key-and-kmacro-by-read-key'."
  :group 'loophole
  :type 'key-sequence)

(defcustom loophole-use-kmacro-by-recursive-edit-map t
  "Flag if `loophole-kmacro-by-recursive-edit-map' is enabled."
  :group 'loophole
  :type 'boolean)

(defcustom loophole-kmacro-by-recursive-edit-map-tag
  "kmacro[End: \\[loophole-end-kmacro], Abort: \\[loophole-abort-kmacro]]"
  "Tag string for `loophole-kmacro-by-recursive-edit-map'."
  :group 'loophole
  :type 'string)

(defcustom loophole-array-by-read-key-finish-key (where-is-internal
                                                   'keyboard-quit nil t)
  "Key sequence to finish inputting key sequence.
This is used by
`loophole-obtain-key-and-array-by-read-key'."
  :group 'loophole
  :type 'key-sequence)

(defcustom loophole-bind-command-order
  '(loophole-obtain-key-and-command-by-read-command
    loophole-obtain-key-and-command-by-key-sequence
    loophole-obtain-key-and-command-by-lambda-form
    loophole-obtain-key-and-object)
  "The priority list of methods to obtain key and command for binding.
`loophole-bind-command' refers this variable to select
obtaining method.  First element gets first priority.

Each element should return a list looks like (key command).
Optionally, return value can contain keymap to bind; in this
case, the list looks like (key command keymap)."
  :risky t
  :group 'loophole
  :type '(repeat symbol))

(defcustom loophole-bind-kmacro-order
  '(loophole-obtain-key-and-kmacro-by-recursive-edit
    loophole-obtain-key-and-kmacro-by-read-key
    loophole-obtain-key-and-kmacro-by-recall-record
    loophole-obtain-key-and-object)
  "The priority list of methods to obtain key and kmacro for binding.
`loophole-bind-kmacro' refers this variable to select
obtaining method.  First element gets first priority.

Each element should return a list looks like (key kmacro).
Optionally, return value can contain keymap to bind; in this
case, the list looks like (key kmacro keymap)."
  :risky t
  :group 'loophole
  :type '(repeat symbol))

(defcustom loophole-bind-array-order
  '(loophole-obtain-key-and-array-by-read-key
    loophole-obtain-key-and-array-by-read-string
    loophole-obtain-key-and-object)
  "The priority list of methods to obtain key and array for binding.
`loophole-bind-array' refers this variable to select
obtaining method.  First element gets first priority.

Each element should return a list looks like (key array).
Optionally, return value can contain keymap to bind; in this
case, the list looks like (key array keymap)."
  :risky t
  :group 'loophole
  :type '(repeat symbol))

(defcustom loophole-bind-keymap-order
  '(loophole-obtain-key-and-keymap-by-read-keymap-variable
    loophole-obtain-key-and-keymap-by-read-keymap-function
    loophole-obtain-key-and-object)
  "The priority list of methods to obtain key and keymap for binding.
`loophole-bind-keymap' refers this variable to select
obtaining method.  First element gets first priority.

Each element should return a list looks like (key keymap).
Optionally, return value can contain keymap to bind; in this
case, the list looks like (key keymap keymap)."
  :risky t
  :group 'loophole
  :type '(repeat symbol))

(defcustom loophole-bind-symbol-order
  '(loophole-obtain-key-and-symbol-by-read-keymap-function
    loophole-obtain-key-and-symbol-by-read-command
    loophole-obtain-key-and-object)
  "The priority list of methods to obtain key and symbol for binding.
`loophole-bind-symbol' refers this variable to select
obtaining method.  First element gets first priority.

Each element should return a list looks like (key symbol).
Optionally, return value can contain keymap to bind; in this
case, the list looks like (key symbol keymap)."
  :risky t
  :group 'loophole
  :type '(repeat symbol))

(defcustom loophole-set-key-order
  '(loophole-obtain-key-and-command-by-read-command
    loophole-obtain-key-and-kmacro-by-recursive-edit
    loophole-obtain-key-and-command-by-key-sequence
    loophole-obtain-key-and-kmacro-by-read-key
    loophole-obtain-key-and-command-by-lambda-form
    loophole-obtain-key-and-kmacro-by-recall-record
    loophole-obtain-key-and-object)
  "The priority list of methods to obtain key and object for binding.
`loophole-set-key' refers this to select obtaining method.
First element gets first priority.
Each element should return a list looks like (key object)."
  :risky t
  :group 'loophole
  :type '(repeat symbol))

(defcustom loophole-register-functions nil
  "Hook for `loophole-register'.
Functions added to this user option are called with one
argument, registered map variable."
  :group 'loophole
  :type 'hook)

(defcustom loophole-unregister-functions nil
  "Hook for `loophole-unregister'.
Functions added to this user option are called with one
argument, unregistered map variable."
  :group 'loophole
  :type 'hook)

(defcustom loophole-prioritize-functions nil
  "Hook for `loophole-prioritize'.
Functions added to this user option are called with one
argument, prioritized map variable."
  :group 'loophole
  :type 'hook)

(defcustom loophole-enable-functions nil
  "Hook for `loophole-enable'.
Functions added to this user option are called with one
argument, enabled map variable."
  :group 'loophole
  :type 'hook)

(defcustom loophole-disable-functions nil
  "Hook for `loophole-disable'.
Functions added to this user option are called with one
argument, disabled map variable."
  :group 'loophole
  :type 'hook)

(defcustom loophole-start-editing-functions nil
  "Hook for `loophole-start-editing'.
Functions added to this user option are called with one
argument, which is being edited map variable."
  :group 'loophole
  :type 'hook)

(defcustom loophole-stop-editing-functions nil
  "Hook for `loophole-stop-editing'.
Functions added to this user option are called with one
argument, which has been edited map variable."
  :group 'loophole
  :type 'hook)

(defcustom loophole-name-functions nil
  "Hook for `loophole-name'.
Functions added to this user option are called with one
argument, named new map variable."
  :group 'loophole
  :type 'hook)

(defcustom loophole-bind-hook nil
  "Hook for `loophole-bind-entry'.
Because binding commands including `loophole-set-key' and
`loophole-unset-key' finally call `loophole-bind-entry',
this hook is run with all of them."
  :group 'loophole
  :type 'hook)

(defcustom loophole-tag-sign "#"
  "String indicating tag string of loophole-map."
  :group 'loophole
  :type 'string)

(defcustom loophole-mode-lighter-base " L"
  "Lighter base string for mode line."
  :group 'loophole
  :type 'string)

(defcustom loophole-mode-lighter-editing-sign "+"
  "Lighter editing sign string for mode line."
  :group 'loophole
  :type 'string)

(defcustom loophole-mode-lighter-suspending-sign "-"
  "Lighter suspending sign string for mode line."
  :group 'loophole
  :type 'string)

(defcustom loophole-mode-lighter-use-face nil
  "Flag if lighter use face."
  :group 'loophole
  :type 'boolean)

(defface loophole-using
  '((t :inherit error))
  "Face used for section of lighter showing active loophole-map."
  :group 'loophole)

(defface loophole-editing
  '((t))
  "Face used for section of lighter showing editing loophole-map."
  :group 'loophole)

(defface loophole-suspending
  '((t :inherit shadow))
  "Face used for suffixes of lighter while loophole is suspending."
  :group 'loophole)

(defun loophole-map-variable-list ()
  "Return list of all keymap variables for loophole.
Elements are ordered according to `loophole--map-alist'."
  (mapcar (lambda (e)
            (get (car e) :loophole-map-variable))
          loophole--map-alist))

(defun loophole-state-variable-list ()
  "Return list of all keymap variables for loophole.
Elements are ordered according to `loophole--map-alist'."
  (mapcar #'car loophole--map-alist))

(defun loophole-key-equal (k1 k2)
  "Return t if two key sequences K1 and K2 are equivalent.
Specifically, this function get `key-description' of each
key, and compare them by `equal'."
  (equal (key-description k1) (key-description k2)))

(defun loophole-local-variable-if-set-list ()
  "Return list of symbols which is local variable if set."
  `(loophole--map-alist loophole--editing ,@(loophole-state-variable-list)))

(defun loophole-read-key (prompt)
  "Read and return key sequence for bindings.
PROMPT is a string for reading key.
If `loophole-allow-keyboard-quit' is non-nil,
\\[keyboard-quit] invokes `keyboard-quit'.
Otherwise, \\[keyboard-quit] will be returned as it is read."
  (let* ((menu-prompting nil)
         (key (read-key-sequence prompt nil t)))
    (or (vectorp key) (stringp key)
        (signal 'wrong-type-argument (list 'arrayp key)))
    (and loophole-allow-keyboard-quit
         (loophole-key-equal
          (vconcat (where-is-internal 'keyboard-quit nil t))
          (vconcat key))
         (keyboard-quit))
    key))

(defun loophole-read-map-variable (prompt &optional predicate)
  "`completing-read' existing map-variable and return read one.
PROMPT is used for `completing-read'.

If optional argument PREDICATE is non-nil, it should be
a function which filters candidates for `completing-read'.
Otherwise, bare `loophole-map-variable-list' is used for
candidates.
PREDICATE shoud get one argument, a map variable, and return
non-nil if that map variable should be used for candidates.

If there are no candidates after PREDICATE is applied to
`loophole-map-variable-list', this function finally signals
`user-error'."
  (let ((map-variable-list
         (if predicate
             (seq-filter predicate (loophole-map-variable-list))
           (loophole-map-variable-list)))
        (minibuffer-help-form
         '(let ((completions (all-completions
                              (minibuffer-contents)
                              minibuffer-completion-table
                              minibuffer-completion-predicate))
                (index (cdr
                        (assq 'index
                              loophole--read-map-variable-help-condition)))
                (last (cdr
                       (assq 'last
                             loophole--read-map-variable-help-condition))))
            (if completions
                (progn
                  (if (equal completions last)
                      (push `(index . ,(if (< index
                                              (- (length completions) 1))
                                           (1+ index)
                                         0))
                            loophole--read-map-variable-help-condition)
                    (push '(index . 0)
                          loophole--read-map-variable-help-condition)
                    (push `(last . ,completions)
                          loophole--read-map-variable-help-condition))
                  (let ((map-variable
                         (elt
                          completions
                          (cdr
                           (assq
                            'index
                            loophole--read-map-variable-help-condition)))))
                    (if map-variable (loophole-describe map-variable))))
              (push '(index . 0)
                    loophole--read-map-variable-help-condition)
              (push `(last . ,completions)
                    loophole--read-map-variable-help-condition)))))
    (unwind-protect
        (intern (completing-read prompt map-variable-list nil t))
      (setq loophole--read-map-variable-help-condition '((index . 0) (last)))
      (if (null map-variable-list)
          (user-error "There are no suitable loophole maps")))))

(defun loophole-map-variable-for-keymap (keymap)
  "Return map variable whose value is KEYMAP."
  (let* ((state-variable (car (rassq keymap loophole--map-alist)))
         (map-variable (get state-variable :loophole-map-variable)))
    (if (eq (symbol-value map-variable) keymap)
        map-variable)))

(defun loophole-map-variable-for-key-binding (key)
  "Return map variable for active KEY in `loophole--map-alist'."
  (get (car (seq-find (lambda (a)
                        (let ((binding (lookup-key (cdr a) key)))
                          (and (symbol-value (car a))
                               binding
                               (eq binding (key-binding key)))))
                      loophole--map-alist))
       :loophole-map-variable))

(defun loophole-suspending-p ()
  "Non-nil during suspending Loophole.
During suspension, `loophole--map-alist' is removed from
`emulation-mode-map-alists'.
Consequently, all loophole maps lose effect while its state
is preserved."
  (not (memq 'loophole--map-alist emulation-mode-map-alists)))

(defun loophole-start-timer (map-variable)
  "Setup or update timer for disabling MAP-VARIABLE."
  (let ((timer (cdr (assq map-variable loophole--timer-alist))))
    (if (timerp timer)
        (progn
          (timer-set-time timer (timer-relative-time nil loophole-timer-delay))
          (if (or (timer--triggered timer)
                  (not (member timer  timer-list)))
              (timer-activate timer)))
      (push `(,map-variable . ,(run-with-timer
                                loophole-timer-delay
                                nil
                                (lambda (map-variable buffer)
                                  (if (and (loophole-registered-p map-variable)
                                           (buffer-live-p buffer))
                                      (with-current-buffer buffer
                                        (loophole-disable map-variable)
                                        (force-mode-line-update))))
                                map-variable (current-buffer)))
            loophole--timer-alist))))

(defun loophole-stop-timer (map-variable)
  "Cancel timer for disabling MAP-VARIABLE."
  (let ((timer (cdr (assq map-variable loophole--timer-alist))))
    (if (and (timerp timer)
             (not (timer--triggered timer)))
        (cancel-timer timer))))

(defun loophole--remove-all-timers (map-variable)
  "Cancel and remove all timers for disabling MAP-VARIABLE."
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (when (local-variable-p 'loophole--timer-alist)
              (let ((timer (cdr (assq map-variable loophole--timer-alist))))
                (if (timerp timer) (cancel-timer timer)))
              (setq loophole--timer-alist
                    (seq-filter (lambda (cell)
                                  (not (eq (car cell) map-variable)))
                                loophole--timer-alist)))))
        (if (listp loophole--buffer-list)
            loophole--buffer-list
          (buffer-list))))

(defun loophole--change-map-variable-name-of-timer (map-variable
                                                       map-name &optional _tag)
  "Update `loophole--timer-alist' and timer when naming MAP-VARIABLE.
Updated ones refer to the symbol whose name is MAP-NAME
prefixed by loophole- and suffixed by -map.
All buffer local alists and timers are updated."
  (let ((named-map-variable (intern (format "loophole-%s-map" map-name))))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (if (local-variable-p 'loophole--timer-alist)
                  (let ((cell (assq map-variable loophole--timer-alist)))
                    (when cell
                      (setcar cell named-map-variable)
                      (let ((timer (cdr cell)))
                        (if (timerp timer)
                            (timer-set-function
                             timer
                             (lambda (map-variable buffer)
                               (if (and (loophole-registered-p map-variable)
                                        (buffer-live-p buffer))
                                   (with-current-buffer buffer
                                     (loophole-disable map-variable)
                                     (force-mode-line-update))))
                             (list named-map-variable (current-buffer))))))))))
          (if (listp loophole--buffer-list)
              loophole--buffer-list
            (buffer-list)))))

(defun loophole--stop-all-timers ()
  "Cancel all timers with saving active timers.
This function is intended to be added to
`loophole-mode-hook' for disabling `loophole-mode'."
  (put 'loophole--timer-alist :loophole-ghost-timer-list nil)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (if (local-variable-p 'loophole--timer-alist)
                (dolist (cell loophole--timer-alist)
                  (let ((timer (cdr cell)))
                    (when (and (timerp timer)
                               (not (timer--triggered timer)))
                      (if (memq timer timer-list)
                          (push timer (get 'loophole--timer-alist
                                           :loophole-ghost-timer-list)))
                      (cancel-timer timer)))))))
        (buffer-list)))

(defun loophole--revive-or-start-all-timers ()
  "Revive or start all timers for disabling loophole-map.
Timers which is saved as ghost when `loophole-mode' is
disabled are revived without updating trigering time.
Others are started.
This function is intended to be added to
`loophole-mode-hook' for enabling `loophole-mode'."
  (let ((ghost-timer-list
         (get 'loophole--timer-alist :loophole-ghost-timer-list)))
    (dolist (ghost-timer ghost-timer-list)
      (timer-activate ghost-timer))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (dolist (map-variable (loophole-map-variable-list))
                (if (and (symbol-value
                          (get map-variable :loophole-state-variable))
                         (not (memq (cdr (assq map-variable
                                               loophole--timer-alist))
                                    ghost-timer-list)))
                    (loophole-start-timer map-variable)))))
          loophole--buffer-list))
  (put 'loophole--timer-alist :loophole-ghost-timer-list nil))

(defun loophole-start-editing-timer ()
  "Setup or update timer for editing state."
  (if (timerp loophole--editing-timer)
      (progn
        (timer-set-time loophole--editing-timer
                        (timer-relative-time nil loophole-editing-timer-delay))
        (if (or (timer--triggered loophole--editing-timer)
                (not (member loophole--editing-timer  timer-list)))
            (timer-activate loophole--editing-timer)))
    (setq loophole--editing-timer
          (run-with-timer loophole-editing-timer-delay
                          nil
                          (lambda (buffer)
                            (if (buffer-live-p buffer)
                                (with-current-buffer buffer
                                  (loophole-stop-editing)
                                  (force-mode-line-update))))
                          (current-buffer)))))

(defun loophole-stop-editing-timer ()
  "Cancel timer for editing state."
  (if (and (timerp loophole--editing-timer)
           (not (timer--triggered loophole--editing-timer)))
      (cancel-timer loophole--editing-timer)))

(defun loophole--stop-all-editing-timers ()
  "Cancel all editing timers with saving active timers.
This function is intended to be added to
`loophole-mode-hook' for disabling `loophole-mode'."
  (put 'loophole--editing-timer :loophole-ghost-editing-timer-list nil)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (when (and (local-variable-p 'loophole--editing-timer)
                       (timerp loophole--editing-timer)
                       (not (timer--triggered loophole--editing-timer)))
              (if (memq loophole--editing-timer timer-list)
                  (push loophole--editing-timer
                        (get 'loophole--editing-timer
                             :loophole-ghost-editing-timer-list)))
              (cancel-timer loophole--editing-timer))))
        (buffer-list)))

(defun loophole--revive-or-start-all-editing-timers ()
  "Revive or start all timers for editing state.
Editing timers which is saved as ghost when `loophole-mode'
is disabled are revived without updating trigering time.
Others are started.
This function is intended to be added to
`loophole-mode-hook' for enabling `loophole-mode'."
  (let ((ghost-timer-list
         (get 'loophole--editing-timer :loophole-ghost-editing-timer-list)))
    (dolist (ghost-timer ghost-timer-list)
      (timer-activate ghost-timer))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (if (and (local-variable-p 'loophole--editing-timer)
                       (timerp loophole--editing-timer)
                       (not (memq loophole--editing-timer ghost-timer-list)))
                  (loophole-start-editing-timer))))
          loophole--buffer-list))
  (put 'loophole--editing-timer :loophole-ghost-editing-timer-list nil))

(defun loophole--follow-adding-local-variable (_symbol _newval operation where)
  "Update `loophole--buffer-list' for adding local variable.
This function is intented to be used for
`add-variable-watcher'.  Only while `loophole-mode' is
enabled, this function is added as variable watcher.
When OPERATION is set and WHERE is non-nil, WHERE is added
to `loophole--buffer-list'."
  (when (and (eq operation 'set)
             where
             (not (memq where loophole--buffer-list)))
    (push where loophole--buffer-list)
    (with-current-buffer where
      (add-hook 'change-major-mode-hook
                #'loophole--follow-killing-local-variable nil t)
      (add-hook 'kill-buffer-hook
                #'loophole--follow-killing-local-variable nil t))))

(defun loophole--follow-killing-local-variable ()
  "Update `loophole--buffer-list' for killing local variable.
This function is intended to be added to
`change-major-mode-hook' and `kill-buffer-hook'.
Only while `loophole-mode' is enabled, this functions is
added to the hooks above."
  (when (memq (current-buffer) loophole--buffer-list)
    (setq loophole--buffer-list (delq (current-buffer) loophole--buffer-list))
    (remove-hook 'change-major-mode-hook
                 #'loophole--follow-killing-local-variable t)
    (remove-hook 'kill-buffer-hook
                 #'loophole--follow-killing-local-variable t)))

(defun loophole--follow-global-state (symbol newval operation where)
  "Set all of local and default value of SYMBOL as NEWVAL.
This is done if OPERATION is set.
Local variables are scanned by using `loophole--buffer-list'
and WHERE.
This function is intended to be used for
`add-variable-watcher'.  Only while `loophole-mode' is
enabled, this function is added as variable watcher."
  (when (eq operation 'set)
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (set symbol newval)))
          (if where
              (remq where loophole--buffer-list)
            loophole--buffer-list))
    (if where (set-default symbol newval))
    (force-mode-line-update t)))

(defun loophole-registered-p (map-variable &optional state-variable)
  "Return non-nil if MAP-VARIABLE is registered to loophole.
If optional argument STATE-VARIABLE is not nil,
Return non-nil if both MAP-VARIABLE and STATE-VARIABLE are
registered, and they are associated."
  (and (if state-variable
           (eq state-variable (get map-variable :loophole-state-variable))
         (setq state-variable (get map-variable :loophole-state-variable)))
       (eq map-variable (get state-variable :loophole-map-variable))
       (assq state-variable (default-value 'loophole--map-alist))))

(defun loophole-register (map-variable state-variable &optional tag
                                       global without-base-map)
  "Register the set of MAP-VARIABLE and STATE-VARIABLE to loophole.
Optional argument TAG is a tag string which may be shown in
mode line.  TAG should not contain `loophole-tag-sign',
because tag will be prefixed by `loophole-tag-sign' on the
mode-line.
If optional argument GLOBAL is non-nil, MAP-VARIABLE and
STATE-VARIABLE are registered as globalized Loophole map.
It means default value and all buffer local value of
STATE-VARIABLE will be changed synchronously.
When using globalized Loophole map, STATE-VARIABLE should
have dynamic scope, i.e. should be defined by `defvar',
`defconst' or `defcustom'.
Unless WITHOUT-BASE-MAP is non-nil, `loophole-base-map' is
set as parent keymap for MAP-VARIABLE.

If called interactively, read MAP-VARIABLE, STATE-VARIABLE.
When called with prefix argument, read TAG and ask user if
MAP-VARIABLE is registered as GLOBAL and WITHOUT-BASE-MAP."
  (interactive
   (let* ((arg-map-variable
           (intern (completing-read "Map-variable: "
                                    obarray
                                    (lambda (s)
                                      (and (boundp s) (not (keywordp s))
                                           (keymapp (symbol-value s)))))))
          (arg-state-variable
           (intern (completing-read "State-variable: "
                                    obarray
                                    (lambda (s)
                                      (and (boundp s) (not (keywordp s)))))))
          (arg-tag (if current-prefix-arg
                       (read-string
                        (format "Tag for keymap %s: " arg-map-variable))))
          (arg-global (if current-prefix-arg (y-or-n-p "Global? ")))
          (arg-without-base-map (if current-prefix-arg
                                    (y-or-n-p "Without base map? "))))
     (list arg-map-variable arg-state-variable arg-tag
           arg-global arg-without-base-map)))
  (cond ((loophole-registered-p map-variable state-variable)
         (user-error "Specified variables are already registered: %s, %s"
                     map-variable state-variable))
        ((memq map-variable (loophole-map-variable-list))
         (user-error "Specified map-variable is already used: %s"
                     map-variable))
        ((memq state-variable (loophole-state-variable-list))
         (user-error "Specified state-variable is already used: %s"
                     state-variable))
        ((seq-find (lambda (a)
                     (let ((keymap (cdr a)))
                       (eq keymap (symbol-value map-variable))))
                   loophole--map-alist)
         (user-error
          "Specified map-variable holds keymap which is already used: %s"
          state-variable)))
  (put map-variable :loophole-state-variable state-variable)
  (put state-variable :loophole-map-variable map-variable)
  (put map-variable :loophole-tag tag)
  (put map-variable :loophole-global (not (null global)))
  (unless (local-variable-if-set-p state-variable)
    (make-variable-buffer-local state-variable))
  (when (listp loophole--buffer-list)
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (if (local-variable-p state-variable)
                  (add-to-list 'loophole--buffer-list buffer nil #'eq))))
          (buffer-list))
    (add-variable-watcher state-variable
                          #'loophole--follow-adding-local-variable)
    (if global
        (add-variable-watcher state-variable #'loophole--follow-global-state)))
  (unless without-base-map
    (set-keymap-parent (symbol-value map-variable) loophole-base-map))
  (setq-default loophole--map-alist
                (cons `(,state-variable . ,(symbol-value map-variable))
                      (default-value 'loophole--map-alist)))
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (if (local-variable-p 'loophole--map-alist)
                (push `(,state-variable . ,(symbol-value map-variable))
                      loophole--map-alist))))
        (if (listp loophole--buffer-list)
            loophole--buffer-list
          (buffer-list)))
  (run-hook-with-args 'loophole-register-functions map-variable))

(defun loophole-unregister (map-variable &optional keep-parent-map)
  "Unregister MAP-VARIABLE from loophole.
If an optional argument KEEP-PARENT-MAP is non-nil, parent
of MAP-VARIABLE will not be removed.

If called interactively with prefix argument, it is assigned
to KEEP-PARENT-MAP."
  (interactive
   (list (loophole-read-map-variable "Unregister keymap:") current-prefix-arg))
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (if (and (local-variable-p 'loophole--editing)
                     (eq loophole--editing map-variable))
                (loophole-stop-editing))))
        (if (listp loophole--buffer-list)
            loophole--buffer-list
          (buffer-list)))
  (let ((state-variable (get map-variable :loophole-state-variable)))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (if (local-variable-p 'loophole--map-alist)
                  (setq loophole--map-alist
                        (seq-filter (lambda (cell)
                                      (not (eq (car cell) state-variable)))
                                    loophole--map-alist)))))
          (if (listp loophole--buffer-list)
            loophole--buffer-list
          (buffer-list)))
    (setq-default loophole--map-alist
                  (seq-filter (lambda (cell)
                                  (not (eq (car cell) state-variable)))
                              (default-value 'loophole--map-alist)))
    (when (listp loophole--buffer-list)
      (mapc (lambda (buffer)
              (with-current-buffer buffer
                (if (and (local-variable-p state-variable)
                         (seq-every-p (lambda (variable)
                                        (not (local-variable-p variable)))
                                      (loophole-local-variable-if-set-list)))
                    (setq loophole--buffer-list
                          (delq buffer loophole--buffer-list)))))
            loophole--buffer-list)
      (remove-variable-watcher state-variable
                               #'loophole--follow-adding-local-variable)
      (if (get map-variable :loophole-global)
          (remove-variable-watcher state-variable
                                   #'loophole--follow-global-state)))
    (unless keep-parent-map
      (set-keymap-parent (symbol-value map-variable) nil))
    (put map-variable :loophole-global nil)
    (put map-variable :loophole-tag nil)
    (put state-variable :loophole-map-variable nil)
    (put map-variable :loophole-state-variable nil))
  (run-hook-with-args 'loophole-unregister-functions map-variable))

(defun loophole-prioritize (map-variable)
  "Give first priority to MAP-VARIABLE.
This is done by move the entry in `loophole--map-alist' to
the front."
  (interactive (list (loophole-read-map-variable "Prioritize keymap: ")))
  (let ((state-variable (get map-variable :loophole-state-variable)))
    (when state-variable
      (unless (eq (assq state-variable loophole--map-alist)
                  (car loophole--map-alist))
        (setq loophole--map-alist
              (cons `(,state-variable . ,(symbol-value map-variable))
                    (seq-filter (lambda (cell)
                                  (not (eq (car cell) state-variable)))
                                loophole--map-alist)))
        (setq-default
         loophole--map-alist
         (cons `(,state-variable . ,(symbol-value map-variable))
               (seq-filter (lambda (cell)
                             (not (eq (car cell) state-variable)))
                           (default-value 'loophole--map-alist)))))
      (run-hook-with-args 'loophole-prioritize-functions map-variable))))

(defun loophole-generate ()
  "Return Loophole map variable whose value is newly generated keymap.

Name of map variable is loophole-n-map.
If the number of temporary keymap is
`loophole-temporary-map-max' or higher, disabled earliest
used one, unregistered one or enabled earliest used one will
be overwritten by new sparse keymap.

This function also binds state variable for map variable.
State variable is named as map-variable-state."
  (let* ((map-variable
          (let ((reversal-map-variable-list
                 (reverse (mapcar (lambda (e)
                                    (get (car e) :loophole-map-variable))
                                  (default-value 'loophole--map-alist)))))
            (letrec ((find-nonbound-temporary-map-variable
                      (lambda (i)
                        (let ((s (intern (format "loophole-%d-map" i))))
                          (cond ((< loophole-temporary-map-max i) nil)
                                ((boundp s)
                                 (funcall
                                  find-nonbound-temporary-map-variable (1+ i)))
                                (t (progn
                                     (put s 'variable-documentation
                                          "Keymap for temporary use.
Generated by `loophole-generate'.")
                                     s))))))
                     (find-earliest-used-disabled-temporary-map-variable
                      (lambda ()
                        (seq-find
                         (lambda (map-var)
                           (and (seq-every-p
                                 (lambda (buffer)
                                   (with-current-buffer buffer
                                     (let ((state-var
                                            (get map-var
                                                 :loophole-state-variable)))
                                       (not (symbol-value state-var)))))
                                 (if (listp loophole--buffer-list)
                                     loophole--buffer-list
                                   (buffer-list)))
                                (string-match
                                 "loophole-[0-9]+-map"
                                 (symbol-name map-var))))
                         reversal-map-variable-list)))
                     (find-orphan-temporary-map-variable
                      (lambda (i)
                        (let ((s (intern
                                  (format "loophole-%d-map" i))))
                          (cond ((< loophole-temporary-map-max i) nil)
                                ((memq s reversal-map-variable-list)
                                 (funcall
                                  find-orphan-temporary-map-variable (1+ i)))
                                (t s)))))
                     (find-earliest-used-temporary-map-variable
                      (lambda ()
                        (seq-find (lambda (map-var)
                                    (string-match
                                     "loophole-[0-9]+-map"
                                     (symbol-name map-var)))
                                  reversal-map-variable-list))))
              (or (funcall find-nonbound-temporary-map-variable 1)
                  (funcall find-earliest-used-disabled-temporary-map-variable)
                  (funcall find-orphan-temporary-map-variable 1)
                  (funcall find-earliest-used-temporary-map-variable)
                  (error
                   "Loophole maps or `loophole--map-alist' might be broken")))))
         (state-variable (let ((s (intern (concat (symbol-name map-variable)
                                                  "-state"))))
                           (put s 'variable-documentation
                                (format "State of `%s'.
Generated by `loophole-generate'." map-variable))
                           s))
         (tag (replace-regexp-in-string
               "loophole-\\([0-9]+\\)-map" "\\1"
               (symbol-name map-variable))))
    (set map-variable (make-sparse-keymap))
    (unless (local-variable-if-set-p state-variable)
      (make-variable-buffer-local state-variable))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (when (and (local-variable-p state-variable)
                         (boundp state-variable)
                         (symbol-value state-variable))
                (set state-variable nil)
                (force-mode-line-update))))
          (if (listp loophole--buffer-list)
            loophole--buffer-list
          (buffer-list)))
    (unless (loophole-registered-p map-variable state-variable)
      (loophole-register map-variable state-variable tag))
    map-variable))

(defun loophole-ready-map ()
  "Return available temporary keymap.
If currently editing keymap exists, return it; otherwise
generate new one, prepare it, and return it."
  (let ((map-variable
         (cond (loophole--editing loophole--editing)
               (t (let ((generated (loophole-generate)))
                    (loophole-prioritize generated)
                    (loophole-start-editing generated)
                    (set (get generated :loophole-state-variable) t)
                    (if (and (listp loophole--buffer-list)
                             (not (local-variable-p 'loophole--map-alist)))
                      (setq loophole--map-alist loophole--map-alist))
                    generated)))))
    (symbol-value map-variable)))

(defun loophole-enable (map-variable)
  "Enable the keymap stored in MAP-VARIABLE."
  (interactive
   (list (loophole-read-map-variable
          "Enable keymap temporarily: "
          (lambda (map-variable)
            (not (symbol-value (get map-variable :loophole-state-variable)))))))
  (if (loophole-registered-p map-variable)
      (let ((state-variable (get map-variable :loophole-state-variable)))
        (set state-variable t)
        (unless (local-variable-p 'loophole--map-alist)
          (setq loophole--map-alist loophole--map-alist))
        (run-hook-with-args 'loophole-enable-functions map-variable))
    (user-error "Specified map-variable %s is not registered" map-variable)))

(defun loophole-disable (map-variable)
  "Disable the keymap stored in MAP-VARIABLE."
  (interactive
   (list (loophole-read-map-variable
          "Disable keymap temporarily: "
          (lambda (map-variable)
            (symbol-value (get map-variable :loophole-state-variable))))))
  (if (loophole-registered-p map-variable)
      (let ((state-variable (get map-variable :loophole-state-variable)))
        (set state-variable nil)
        (unless (local-variable-p 'loophole--map-alist)
          (setq loophole--map-alist loophole--map-alist))
        (run-hook-with-args 'loophole-disable-functions map-variable))
    (user-error "Specified map-variable %s is not registered" map-variable)))

(defun loophole-disable-latest ()
  "Disable the lastly added or prioritized active keymap."
  (interactive)
  (let* ((state-variable
          (seq-find #'symbol-value (loophole-state-variable-list)))
         (map-variable (get state-variable :loophole-map-variable)))
    (if map-variable
        (loophole-disable map-variable)
      (user-error "There are no enabled loophole maps"))))

(defun loophole-disable-all ()
  "Disable the all keymaps."
  (interactive)
  (dolist (map-variable (loophole-map-variable-list))
    (loophole-disable map-variable)))

(defun loophole-name (map-variable map-name &optional tag)
  "Name Loophole map MAP-VARIABLE as MAP-NAME.
If optional argument TAG is non-nil, tag string for the map
is set as TAG; otherwise, initial character of MAP-NAME is
used as tag string.
Old MAP-VARIABLE and corresponding state-variable are
initialized; their documentation is set as nil, they are
unbound, their properties for Loophole are set as nil, they
are removed from `loophole--map-alist'.

If loophole-MAP-NAME-map or loophole-MAP-NAME-map-state have
been already bound, this function signals `user-error'.
Only unbound name can be used for MAP-NAME.

When interactive call, this function asks MAP-VARIABLE and
MAP-NAME.  If prefix-argument is non-nil, TAG is also asked.
If MAP-NAME contains `loophole-tag-sign', use a following
string as TAG regardless of the value of prefix-argument."
  (interactive
   (let* ((arg-map-variable (loophole-read-map-variable "Name keymap: "))
          (arg-map-name (read-string (format "New name[%stag] for keymap %s: "
                                             loophole-tag-sign
                                             arg-map-variable)))
          (arg-tag (let ((match (string-match loophole-tag-sign arg-map-name)))
                     (cond (match
                            (prog1
                                (substring arg-map-name
                                           (1+ match)
                                           (length arg-map-name))
                              (setq arg-map-name
                                    (substring arg-map-name 0 match))))
                           (current-prefix-arg
                            (read-string (format "New tag for keymap %s: "
                                                 arg-map-variable)))))))
     (list arg-map-variable arg-map-name arg-tag)))
  (unless (loophole-registered-p map-variable)
    (user-error "Specified map-variable %s is not registered" map-variable))
  (let* ((state-variable (get map-variable :loophole-state-variable))
         (map-variable-name (format "loophole-%s-map" map-name))
         (state-variable-name (concat map-variable-name "-state"))
         (tag (or tag (substring map-name 0 1))))
    (let ((named-map-variable (intern map-variable-name))
          (named-state-variable (intern state-variable-name)))
      (let ((bound (seq-find #'boundp
                             (list named-map-variable named-state-variable))))
        (if bound
            (user-error "Specified name %s has already been bound" bound)))
      (put named-map-variable 'variable-documentation
           (format "Keymap for temporary use.
Introduced by `loophole-name' for renaming %s
which had been already unbound." map-variable))
      (put named-state-variable 'variable-documentation
           (format "State of `%s'.
Introduced by `loophole-name' for renaming %s
which had been already unbound." named-map-variable state-variable))
      (set named-map-variable (symbol-value map-variable))
      (make-variable-buffer-local named-state-variable)
      (set-default named-state-variable
                   (if (get map-variable :loophole-global)
                       (symbol-value state-variable)
                     nil))
      (mapc
       (lambda (buffer)
         (with-current-buffer buffer
           (if (local-variable-p state-variable)
               (set named-state-variable (symbol-value state-variable)))))
       (if (listp loophole--buffer-list)
           loophole--buffer-list
         (buffer-list)))
      (add-variable-watcher named-state-variable
                            #'loophole--follow-adding-local-variable)
      (if (get map-variable :loophole-global)
          (add-variable-watcher named-state-variable
                                #'loophole--follow-global-state))
      (put named-map-variable :loophole-state-variable named-state-variable)
      (put named-state-variable :loophole-map-variable named-map-variable)
      (put named-map-variable :loophole-tag tag)
      (put named-map-variable :loophole-global
           (get map-variable :loophole-global))
      (let ((cell (assq state-variable (default-value 'loophole--map-alist))))
        (setcar cell named-state-variable))
      (mapc (lambda (buffer)
              (with-current-buffer buffer
                (if (local-variable-p 'loophole--map-alist)
                    (let ((cell (assq state-variable loophole--map-alist)))
                      (if (consp cell)
                          (setcar cell named-state-variable))))))
            (if (listp loophole--buffer-list)
                loophole--buffer-list
              (buffer-list)))
      (mapc (lambda (buffer)
              (with-current-buffer buffer
                (if (and (local-variable-p 'loophole--editing)
                         (eq map-variable loophole--editing))
                    (setq loophole--editing named-map-variable))))
            (if (listp loophole--buffer-list)
                loophole--buffer-list
              (buffer-list)))
      (remove-variable-watcher state-variable
                               #'loophole--follow-adding-local-variable)
      (if (get map-variable :loophole-global)
          (remove-variable-watcher state-variable
                                   #'loophole--follow-global-state))
      (put state-variable 'variable-documentation nil)
      (put map-variable 'variable-documentation nil)
      (makunbound state-variable)
      (makunbound map-variable)
      (put map-variable :loophole-tag nil)
      (put state-variable :loophole-map-variable nil)
      (put map-variable :loophole-state-variable nil)
      (run-hook-with-args 'loophole-name-functions named-map-variable))))

(defun loophole-start-editing (map-variable)
  "Start keymap edit session with MAP-VARIABLE."
  (interactive (list (loophole-read-map-variable "Start editing keymap: ")))
  (setq loophole--editing map-variable)
  (run-hook-with-args 'loophole-start-editing-functions map-variable))

(defun loophole-stop-editing ()
  "Stop keymap edit session."
  (interactive)
  (let ((map-variable loophole--editing))
    (setq loophole--editing nil)
    (run-hook-with-args 'loophole-stop-editing-functions map-variable)))

(defun loophole-describe (map-variable)
  "Display all key bindings in MAP-VARIABLE."
  (interactive (list (loophole-read-map-variable "Describe keymap: ")))
  (help-setup-xref `(loophole-describe ,map-variable)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (princ (format "`%s' %sLoophole Map Bindings:\n"
                   map-variable (if (get map-variable :loophole-global)
                                    "Globalized ")))
    (princ (substitute-command-keys
            (format "\\{%s}" map-variable)))
    (with-current-buffer standard-output
      (save-excursion
        (re-search-backward "`\\([^`']+\\)'" nil t)
        (help-xref-button 1 'help-variable map-variable)))))

(define-derived-mode loophole-write-lisp-mode emacs-lisp-mode
  "Loophole Write Lisp"
  "Auxiliary major mode for writing Lisp form in loophole.
Calling this major mode in Lisp program offers
`emacs-lisp-mode' like environment with few key bindings and
recursive edit.
After finish or abort writing, caller can get written
forms by `buffer-string'.
If writing is finished, forms following calling this
function will be evaluated.
In order to do something even for abort case, call this
function in body form of `unwind-protect' and write follow
up forms in unwind forms."
  :group 'loophole
  (setq header-line-format
        `(""
          mode-line-front-space
          ,(substitute-command-keys
            (concat "\\<loophole-write-lisp-mode-map>"
                    "Writing lisp form.  "
                    "finish `\\[loophole-finish-writing-lisp]', "
                    "Abort `\\[loophole-abort-writing-lisp]'."))))
  (recursive-edit))

(defun loophole-finish-writing-lisp ()
  "Finish writing Lisp form in `loophole-write-lisp-mode' buffer."
  (interactive)
  (unless (zerop (recursion-depth))
    (if (eq major-mode 'loophole-write-lisp-mode)
        (emacs-lisp-mode))
    (exit-recursive-edit)))

(defun loophole-abort-writing-lisp ()
  "Abort writing Lisp form in `loophole-write-lisp-mode' buffer."
  (interactive)
  (unless (zerop (recursion-depth))
    (if (eq major-mode 'loophole-write-lisp-mode)
        (emacs-lisp-mode))
    (abort-recursive-edit)))

(defun loophole-start-kmacro ()
  "Start defining keyboard macro.
Definition can be finished by calling `loophole-end-kmacro'."
  (interactive)
  (kmacro-start-macro nil)
  (let* ((end (where-is-internal 'loophole-end-kmacro nil t))
         (abort (where-is-internal 'loophole-abort-kmacro nil t))
         (body (if (and end abort)
                   (format "[End: %s, Abort: %s]"
                           (key-description end)
                           (key-description abort))
                 "[loophole-end/abort-kmacro should be bound to key]")))
    (message "Defining keyboard macro... %s" body))
  (unless (called-interactively-p 'any) (recursive-edit)))

(defun loophole-end-kmacro ()
  "End defining keyboard macro."
  (interactive)
  (unwind-protect
      (kmacro-end-macro nil)
    (unless (zerop (recursion-depth)) (exit-recursive-edit))))

(defun loophole-abort-kmacro ()
  "Abort defining keyboard macro."
  (interactive)
  (unless (zerop (recursion-depth)) (abort-recursive-edit))
  (keyboard-quit))

(defun loophole-obtain-key-and-object ()
  "Return set of key and any Lisp object.
Object is obtained as return value of `eval-minibuffer'."
  (let ((key (loophole-read-key "Set key temporarily: ")))
    (list key (eval-minibuffer (format "Set key %s to entry: "
                                       (key-description key))))))

(defun loophole-obtain-key-and-command-by-read-command ()
  "Return set of key and command obtained by reading command symbol."
  (let ((key (loophole-read-key "Set key temporarily: ")))
    (list key (read-command (format "Set key %s to command: "
                                    (key-description key))))))

(defun loophole-obtain-key-and-command-by-key-sequence ()
  "Return set of key and command obtained by key sequence lookup."
  (let ((key (loophole-read-key "Set key temporarily: ")))
    (list key (let ((binding
                     (key-binding (loophole-read-key
                                   (format
                                    "Set key %s to command bound for: "
                                    (key-description key))))))
                (message "%s" binding)
                binding))))

(defun loophole-obtain-key-and-command-by-lambda-form ()
  "Return set of key and command obtained by writing lambda form.
This function provides work space for writing lambda form as
a temporary buffer.
Actually, any Lisp forms can be written in a temporary
buffer, and if obtained object is valid command, this
function return it.
If multiple Lisp forms are written, they are evaluate
sequentially, and return a value of the last form."
  (let ((key (loophole-read-key "Set key temporarily: ")))
    (let ((name "*Loophole*")
          (buffer (current-buffer))
          (window (selected-window))
          (frame (selected-frame))
          (configuration-list (mapcar #'current-window-configuration
                                      (frame-list))))
      (unwind-protect
          (progn
            (switch-to-buffer-other-window (get-buffer-create name) t)
            (erase-buffer)
            (insert ";; For obtaining lambda form.\n\n")
            (insert loophole-command-by-lambda-form-format)
            (let ((found (search-backward "(#)" nil t)))
              (if found (delete-region (point) (+ (point) 3))))
            (loophole-write-lisp-mode)
            (let ((lambda-form
                   (eval (read (concat
                                "(progn "
                                (with-current-buffer name (buffer-string))
                                ")")))))
              (if (commandp lambda-form)
                  (list key lambda-form)
                (user-error
                 "Obtained Lisp object is not valid command: %s" lambda-form))))
        (let ((configuration
               (seq-find (lambda (c)
                           (eq (selected-frame) (window-configuration-frame c)))
                         configuration-list)))
          (if configuration
              (set-window-configuration configuration)
            (delete-frame)))
        (if (frame-live-p frame) (select-frame-set-input-focus frame t))
        (if (window-live-p window) (select-window window t))
        (if (buffer-live-p buffer) (switch-to-buffer buffer t t))))))

(defun loophole-obtain-key-and-kmacro-by-read-key ()
  "Return set of key and kmacro obtained by reading key.
This function `read-key' recursively.
When you finish keyboard macro,
type `loophole-kmacro-by-read-key-finish-key'.
By default, `loophole-kmacro-by-read-key-finish-key' is \\[keyboard-quit]
the key bound to `keyboard-quit'.  In this situation, you
cannot use \\[keyboard-quit] for quitting.
Once `loophole-kmacro-by-read-key-finish-key' is changed,
you can finish definition of kmacro by new finish key, and
\\[keyboard-quit] takes effect as quit."
  (let ((finish (vconcat loophole-kmacro-by-read-key-finish-key))
        (quit (vconcat (where-is-internal 'keyboard-quit nil t))))
    (or (vectorp finish)
        (stringp finish)
        (vectorp quit)
        (stringp quit)
        (user-error "Neither finishing key nor quitting key is invalid"))
    (let* ((menu-prompting nil)
           (key (loophole-read-key "Set key temporarily: ")))
      (letrec
          ((read-arbitrary-key-sequence
            (lambda (v)
              (let* ((k (vector
                         (read-key
                          (format "Set key %s to kmacro: (%s to finish) [%s]"
                                  (key-description key)
                                  (key-description finish)
                                  (mapconcat (lambda (e)
                                               (key-description (vector e)))
                                             (reverse v)
                                             " ")))))
                     (v (vconcat k v)))
                (cond ((loophole-key-equal
                        (seq-take v (length finish))
                        finish)
                       (reverse (seq-drop v (length finish))))
                      ((loophole-key-equal
                        (seq-take v (length quit))
                        quit)
                       (keyboard-quit))
                      (t (funcall read-arbitrary-key-sequence v)))))))
        (let ((macro (funcall read-arbitrary-key-sequence nil)))
          (kmacro-start-macro nil)
          (end-kbd-macro nil #'kmacro-loop-setup-function)
          (setq last-kbd-macro macro)
          (list key (kmacro-lambda-form (kmacro-ring-head))))))))

(defun loophole-obtain-key-and-kmacro-by-recursive-edit ()
  "Return set of key and kmacro obtained by recursive edit.
\\<loophole-mode-map>
This function starts recursive edit in order to offer
keyboard macro defining work space.  Definition can be
finished by calling `loophole-end-kmacro' which is bound to
\\[loophole-end-kmacro].
Besides, Definition can be aborted by calling
`loophole-abort-kmacro' which is bound to \\[loophole-abort-kmacro]."
  (list (loophole-read-key "Set key temporarily: ")
        (progn
          (loophole-register 'loophole-kmacro-by-recursive-edit-map
                             'loophole-use-kmacro-by-recursive-edit-map
                             loophole-kmacro-by-recursive-edit-map-tag
                             t)
          (unwind-protect (loophole-start-kmacro)
            (loophole-unregister 'loophole-kmacro-by-recursive-edit-map))
          (kmacro-lambda-form (kmacro-ring-head)))))

(defun loophole-obtain-key-and-kmacro-by-recall-record ()
  "Return set of key and kmacro obtained by recalling record."
  (let ((key (loophole-read-key "Set key temporarily: ")))
    (letrec
        ((make-label-kmacro-alist
          (lambda (ring counter)
            (let* ((raw-label (key-description (car (car ring))))
                   (entry (assoc raw-label counter))
                   (label (if entry
                              (format "%s <%d>" raw-label (1+ (cdr entry)))
                            raw-label)))
              (cond ((null ring) ring)
                    (t (cons
                        `(,label . ,(car ring))
                        (funcall make-label-kmacro-alist
                                 (cdr ring)
                                 (cons (if entry
                                           `(,raw-label . ,(1+ (cdr entry)))
                                         `(,raw-label . 1))
                                       counter)))))))))
      (let* ((head (kmacro-ring-head))
             (alist (funcall make-label-kmacro-alist
                             (if head
                                 (cons head kmacro-ring)
                               kmacro-ring)
                             nil))
             (read (completing-read (format "Set key %s to kmacro: "
                                            (key-description key))
                                    alist nil t))
             (kmacro (kmacro-lambda-form (cdr (assoc read alist)))))
        (list key kmacro)))))

(defun loophole-obtain-key-and-array-by-read-key ()
  "Return set of key and array obtained by reading key.
This function `read-key' recursively.  When you finish
inputting key sequence,
type `loophole-array-by-read-key-finish-key'.
By default, `loophole-array-by-read-key-finish-key' is \\[keyboard-quit]
the key bound to `keyboard-quit'.  In this situation, you
cannot use \\[keyboard-quit] for quitting.
Once `loophole-array-by-read-key-finish-key' is changed, you
can finish definition of kmacro by new finish key, and \\[keyboard-quit]
takes effect as quit."
  (let ((finish (vconcat loophole-array-by-read-key-finish-key))
        (quit (vconcat (where-is-internal 'keyboard-quit nil t))))
    (or (vectorp finish)
        (stringp finish)
        (vectorp quit)
        (stringp quit)
        (user-error "Neither finishing key nor quitting key is invalid"))
    (let* ((menu-prompting nil)
           (key (loophole-read-key "Set key temporarily: ")))
      (letrec
          ((read-arbitrary-key-sequence
            (lambda (v)
              (let* ((k (vector
                         (read-key
                          (format "Set key %s to array: (%s to finish) [%s]"
                                  (key-description key)
                                  (key-description finish)
                                  (mapconcat (lambda (e)
                                               (key-description (vector e)))
                                             (reverse v)
                                             " ")))))
                     (v (vconcat k v)))
                (cond ((loophole-key-equal
                        (seq-take v (length finish))
                        finish)
                       (reverse (seq-drop v (length finish))))
                      ((loophole-key-equal
                        (seq-take v (length quit))
                        quit)
                       (keyboard-quit))
                      (t (funcall read-arbitrary-key-sequence v)))))))
        (let ((array (funcall read-arbitrary-key-sequence nil)))
          (list key array))))))

(defun loophole-obtain-key-and-array-by-read-string ()
  "Return set of key and array obtained by `read-string'."
  (let ((key (loophole-read-key "Set key temporarily: ")))
    (list key (read-string (format "Set key %s to array: "
                                   (key-description key))))))

(defun loophole-obtain-key-and-keymap-by-read-keymap-variable ()
  "Return set of key and keymap obtained by reading keymap variable."
  (let ((key (loophole-read-key "Set key temporarily: ")))
    (list key (symbol-value
               (intern
                (completing-read
                 (format "Set key %s to keymap bound to symbol: "
                         (key-description key))
                 obarray
                 (lambda (s)
                   (and (boundp s) (not (keywordp s))
                        (keymapp (symbol-value s))))))))))

(defun loophole-obtain-key-and-keymap-by-read-keymap-function ()
  "Return set of key and keymap obtained by reading keymap function.
Keymap function is a symbol whose function cell is a keymap
or a symbol whose function cell is ultimately a keymap."
  (let ((key (loophole-read-key "Set key temporarily: ")))
    (letrec ((symbol-function-recursively
              (lambda (s)
                (let ((f (symbol-function s)))
                  (cond ((eq f s) f)
                        ((not (symbolp f)) f)
                        (t (funcall symbol-function-recursively f)))))))
      (list key (funcall symbol-function-recursively
                         (intern
                          (completing-read
                           (format "Set key %s to keymap fbound to symbol: "
                                   (key-description key))
                           obarray
                           (lambda (s)
                             (let ((f (funcall symbol-function-recursively s)))
                               (keymapp f))))))))))

(defun loophole-obtain-key-and-symbol-by-read-keymap-function ()
  "Return set of key and symbol obtained by reading keymap function.
Keymap function is a symbol whose function cell is a keymap
or a symbol whose function cell is ultimately a keymap."
  (let ((key (loophole-read-key "Set key temporarily: ")))
    (letrec ((symbol-function-recursively
              (lambda (s)
                (let ((f (symbol-function s)))
                  (cond ((eq f s) f)
                        ((not (symbolp f)) f)
                        (t (funcall symbol-function-recursively f)))))))
      (list key (intern
                 (completing-read
                  (format "Set key %s to symbol whose function cell is keymap: "
                                   (key-description key))
                  obarray
                  (lambda (s)
                    (let ((f (funcall symbol-function-recursively s)))
                      (keymapp f)))))))))

(defun loophole-obtain-key-and-symbol-by-read-command ()
  "Return set of key and symbol obtained by reading command symbol."
  (let ((key (loophole-read-key "Set key temporarily: ")))
    (list key (read-command
               (format "Set key %s to symbol whose function cell is command: "
                       (key-description key))))))

(defun loophole-prefix-rank-value (arg)
  "Return rank value for raw prefix argument ARG.
In the context of this function, rank of prefix argument is
defined as follows.
The rank of no prefix argument is 0.
The rank of prefix argument specified by \\[universal-argument] and C-1 is 1,
The rank of \\[universal-argument] \\[universal-argument] and C-2 is 2,
Likewise, rank n means \\[universal-argument] * n or C-[n]."
  (cond ((null arg) 0)
        ((listp arg) (truncate (log (prefix-numeric-value arg) 4)))
        ((natnump arg) arg)
        (t 0)))

(defun loophole-bind-entry (key entry &optional keymap)
  "Bind KEY to ENTRY temporarily.
Any Lisp object is acceptable for ENTRY, but only few types
make sense.  Meaningful types of ENTRY is completely same as
general keymap entry.

By default, KEY is bound in the currently editing keymap or
generated new one.  If optional argument KEYMAP is non-nil,
and it is registered to loophole, KEYMAP is used instead."
  (interactive (loophole-obtain-key-and-object))
  (define-key
    (if keymap
        (let ((map-variable (loophole-map-variable-for-keymap keymap)))
          (if (and map-variable
                   (loophole-registered-p map-variable))
              keymap
            (error "Invalid keymap: %s" keymap)))
      (loophole-ready-map))
    key
    entry)
  (run-hooks 'loophole-bind-hook))

(defun loophole-bind-command (key command &optional keymap)
  "Bind KEY to COMMAND temporarily.
COMMAND must be a command.

This function finally calls `loophole-bind-entry', so that
the keymap used for binding and the meaning of optional
arguments KEYMAP are same as `loophole-bind-entry'.
See docstring of `loophole-bind-entry'for more details.

When called interactively, this function determines
obtaining method for KEY and COMMAND according to
`loophole-bind-command-order'.
When this function called without prefix argument,
the first element of `loophole-bind-command-order' is
employed as obtaining method.
\\[universal-argument] and C-1 invokes the second element,
\\[universal-argument] \\[universal-argument] and C-2 invokes the third one.
Likewise \\[universal-argument] * n and C-[n] invoke the (n+1)th element."
  (interactive
   (let* ((n (loophole-prefix-rank-value current-prefix-arg))
          (obtaining-method (elt loophole-bind-command-order n)))
     (if (null obtaining-method)
         (user-error "Undefined prefix argument"))
     (funcall obtaining-method)))
  (if (commandp command)
      (loophole-bind-entry key command keymap)
    (error "Invalid command: %s" command)))

(defun loophole-bind-kmacro (key kmacro &optional keymap)
  "Bind KEY to KMACRO temporarily.
KMACRO must be a `kmacro' object.

This function finally calls `loophole-bind-entry', so that
the keymap used for binding and the meaning of optional
arguments KEYMAP are same as `loophole-bind-entry'.
See docstring of `loophole-bind-entry' for more details.

When called interactively, this function determines
obtaining method for KEY and KMACRO according to
`loophole-bind-kmacro-order'.
When this function is called without prefix argument,
the first element of `loophole-bind-kmacro-order' is
employed as obtaining method.
\\[universal-argument] and C-1 invokes the second element,
\\[universal-argument] \\[universal-argument] and C-2 invokes the third one.
Likewise \\[universal-argument] * n and C-[n] invoke the (n+1)th element."
  (interactive
   (let* ((n (loophole-prefix-rank-value current-prefix-arg))
          (obtaining-method (elt loophole-bind-kmacro-order n)))
     (if (null obtaining-method)
         (user-error "Undefined prefix argument"))
     (funcall obtaining-method)))
  (if (kmacro-p kmacro)
      (loophole-bind-entry key kmacro keymap)
    (error "Invalid kmacro: %s" kmacro)))

(defun loophole-bind-last-kmacro (key)
  "Bind KEY to the lastly accessed keyboard macro.
Currently editing keymap or generated new one is used for
binding."
  (interactive (list (loophole-read-key "Set key temporarily: ")))
  (loophole-bind-kmacro key (kmacro-lambda-form (kmacro-ring-head))))

(defun loophole-bind-array (key array &optional keymap)
  "Bind KEY to ARRAY temporarily.
ARRAY must be either a string or vector, which work as
a basic keyboard macro.

This function finally calls `loophole-bind-entry', so that
the keymap used for binding and the meaning of optional
arguments KEYMAP are same as `loophole-bind-entry'.
See docstring of `loophole-bind-entry'for more details.

When called interactively, this function determines
obtaining method for KEY and ARRAY according to
`loophole-bind-array-order'.
When this function called without prefix argument,
the first element of `loophole-bind-array-order' is
employed as obtaining method.
\\[universal-argument] and C-1 invokes the second element,
\\[universal-argument] \\[universal-argument] and C-2 invokes the third one.
Likewise \\[universal-argument] * n and C-[n] invoke the (n+1)th element."
  (interactive
   (let* ((n (loophole-prefix-rank-value current-prefix-arg))
          (obtaining-method (elt loophole-bind-array-order n)))
     (if (null obtaining-method)
         (user-error "Undefined prefix argument"))
     (funcall obtaining-method)))
  (if (arrayp array)
      (loophole-bind-entry key array keymap)
    (error "Invalid array : %s" array)))

(defun loophole-bind-keymap (key another-keymap &optional keymap)
  "Bind KEY to ANOTHER-KEYMAP temporarily.
ANOTHER-KEYMAP must be a keymap object, and KEY will be
a prefix key for ANOTHER-KEYMAP.

This function finally calls `loophole-bind-entry', so that
the keymap used for binding and the meaning of optional
arguments KEYMAP are same as `loophole-bind-entry'.
See docstring of `loophole-bind-entry'for more details.

When called interactively, this function determines
obtaining method for KEY and ANOTHER-KEYMAP according to
`loophole-bind-keymap-order'.
When this function called without prefix argument,
the first element of `loophole-bind-keymap-order' is
employed as obtaining method.
\\[universal-argument] and C-1 invokes the second element,
\\[universal-argument] \\[universal-argument] and C-2 invokes the third one.
Likewise \\[universal-argument] * n and C-[n] invoke the (n+1)th element."
  (interactive
   (let* ((n (loophole-prefix-rank-value current-prefix-arg))
          (obtaining-method (elt loophole-bind-keymap-order n)))
     (if (null obtaining-method)
         (user-error "Undefined prefix argument"))
     (funcall obtaining-method)))
  (if (keymapp another-keymap)
      (loophole-bind-entry key another-keymap keymap)
    (error "Invalid keymap : %s" another-keymap)))

(defun loophole-bind-symbol (key symbol &optional keymap)
  "Bind KEY to SYMBOL temporarily.
SYMBOL must be a symbol whose function cell is a keymap,
a command , a keyboard macro or a symbol whose function
cell is ultimately either of above when scanned recursively.

This function finally calls `loophole-bind-entry', so that
the keymap used for binding and the meaning of optional
arguments KEYMAP are same as `loophole-bind-entry'.
See docstring of `loophole-bind-entry'for more details.

When called interactively, this function determines
obtaining method for KEY and SYMBOL according to
`loophole-bind-symbol-order'.
When this function called without prefix argument,
the first element of `loophole-bind-symbol-order' is
employed as obtaining method.
\\[universal-argument] and C-1 invokes the second element,
\\[universal-argument] \\[universal-argument] and C-2 invokes the third one.
Likewise \\[universal-argument] * n and C-[n] invoke the (n+1)th element."
  (interactive
   (let* ((n (loophole-prefix-rank-value current-prefix-arg))
          (obtaining-method (elt loophole-bind-symbol-order n)))
     (if (null obtaining-method)
         (user-error "Undefined prefix argument"))
     (funcall obtaining-method)))
  (letrec ((inspect-function-cell
            (lambda (symbol)
              (let ((function-cell (symbol-function symbol)))
                (or (keymapp function-cell)
                    (commandp function-cell)
                    (arrayp function-cell)
                    (if (and function-cell (symbolp function-cell))
                        (funcall inspect-function-cell function-cell)))))))
    (if (and (symbolp symbol)
             (funcall inspect-function-cell symbol))
        (loophole-bind-entry key symbol keymap)
      (error "Invalid symbol : %s" symbol))))

(defun loophole-set-key (key entry)
  "Set the temporary binding for KEY and ENTRY.
This function finally calls `loophole-bind-entry', so that
the keymap used for binding is same as
`loophole-bind-entry', i.e. currently editing keymap or
generated new one is used.  ENTRY is also same as
`loophole-bind-entry'.  Any Lisp object is acceptable for
ENTRY, although only few types make sense.  Meaningful types
of ENTRY is completely same as general keymap entry.

When called interactively, this function determines
obtaining method for KEY and ENTRY according to
`loophole-set-key-order'.
When this function is called without prefix argument,
the first element of `loophole-set-key-order' is
employed as obtaining method.
\\[universal-argument] and C-1 invokes the second element,
\\[universal-argument] \\[universal-argument] and C-2 invokes the third one.
Likewise \\[universal-argument] * n and C-[n] invoke the (n+1)th element."
  (interactive
   (let* ((n (loophole-prefix-rank-value current-prefix-arg))
          (obtaining-method (elt loophole-set-key-order n)))
     (if (null obtaining-method)
         (user-error "Undefined prefix argument"))
     (funcall obtaining-method)))
  (loophole-bind-entry key entry))

(defun loophole-unset-key (key)
  "Unset the temporary biding of KEY."
  (interactive (list (loophole-read-key "Unset temporarily set key: ")))
  (if loophole--editing
      (let ((map (symbol-value loophole--editing)))
        (if (lookup-key map key)
            (loophole-bind-entry key nil map)
          (message "No entry found in editing keymap: %s" loophole--editing)))))

(defun loophole-modify-lambda-form (key &optional map-variable)
  "Modify lambda form bound to KEY in MAP-VARIABLE.
If KEYMAP is nil, lookup all active loophole maps ."
  (interactive (list (loophole-read-key "Modify lambda form for key: ")
                     (if current-prefix-arg
                         (loophole-read-map-variable "Lookup: "))))
  (unless map-variable
    (setq map-variable (loophole-map-variable-for-key-binding key))
    (unless map-variable
      (user-error "No entry found in loophole maps for key: %s"
                  (key-description key))))
  (let ((name "*Loophole*")
        (buffer (current-buffer))
        (window (selected-window))
        (frame (selected-frame))
        (configuration-list (mapcar #'current-window-configuration
                                    (frame-list)))
        (entry (lookup-key (symbol-value map-variable) key)))
    (unless (and (commandp entry)
                 (not (symbolp entry))
                 (not (arrayp entry)))
      (user-error "Bound entry is not lambda form: %s" entry))
    (unwind-protect
        (let ((loophole-buffer (get-buffer-create name)))
          (switch-to-buffer-other-window loophole-buffer t)
          (erase-buffer)
          (insert ";; For modifying lambda form.\n\n")
          (pp entry loophole-buffer)
          (loophole-write-lisp-mode)
          (goto-char 1)
          (let ((lambda-form (read loophole-buffer)))
            (if (commandp lambda-form)
                (loophole-bind-entry key lambda-form
                                     (symbol-value map-variable))
              (user-error
               "Modified Lisp object is not valid command: %s" lambda-form))))
      (let ((configuration
             (seq-find (lambda (c)
                         (eq (selected-frame) (window-configuration-frame c)))
                       configuration-list)))
        (if configuration
            (set-window-configuration configuration)
          (delete-frame)))
      (if (frame-live-p frame) (select-frame-set-input-focus frame t))
      (if (window-live-p window) (select-window window t))
      (if (buffer-live-p buffer) (switch-to-buffer buffer t t)))))

(defun loophole-modify-kmacro (key &optional map-variable)
  "Modify kmacro bound to KEY in MAP-VARIABLE.
If KEYMAP is nil, lookup all active loophole maps ."
  (interactive (list (loophole-read-key "Modify kmacro for key: ")
                     (if current-prefix-arg
                         (loophole-read-map-variable "Lookup: "))))
  (unless map-variable
    (setq map-variable (loophole-map-variable-for-key-binding key))
    (unless map-variable
      (user-error "No entry found in loophole maps for key: %s"
                  (key-description key))))
  (let ((name "*Loophole*")
        (buffer (current-buffer))
        (window (selected-window))
        (frame (selected-frame))
        (configuration-list (mapcar #'current-window-configuration
                                    (frame-list)))
        (entry (lookup-key (symbol-value map-variable) key)))
    (unless (kmacro-p entry)
      (user-error "Bound entry is not kmacro: %s" entry))
    (unwind-protect
        (let ((loophole-buffer (get-buffer-create name)))
          (switch-to-buffer-other-window loophole-buffer t)
          (erase-buffer)
          (insert ";; For modifying kmacro.\n\n")
          (pp entry loophole-buffer)
          (loophole-write-lisp-mode)
          (goto-char 1)
          (let ((kmacro (read loophole-buffer)))
            (if (kmacro-p kmacro)
                (loophole-bind-entry key kmacro (symbol-value map-variable))
              (user-error
               "Modified Lisp object is not kmacro: %s" kmacro))))
      (let ((configuration
             (seq-find (lambda (c)
                         (eq (selected-frame) (window-configuration-frame c)))
                       configuration-list)))
        (if configuration
            (set-window-configuration configuration)
          (delete-frame)))
      (if (frame-live-p frame) (select-frame-set-input-focus frame t))
      (if (window-live-p window) (select-window window t))
      (if (buffer-live-p buffer) (switch-to-buffer buffer t t)))))

(defun loophole-modify-array (key &optional map-variable)
  "Modify array bound to KEY in MAP-VARIABLE.
If KEYMAP is nil, lookup all active loophole maps ."
  (interactive (list (loophole-read-key "Modify array for key: ")
                     (if current-prefix-arg
                         (loophole-read-map-variable "Lookup: "))))
  (unless map-variable
    (setq map-variable (loophole-map-variable-for-key-binding key))
    (unless map-variable
      (user-error "No entry found in loophole maps for key: %s"
                  (key-description key))))
  (let ((name "*Loophole*")
        (buffer (current-buffer))
        (window (selected-window))
        (frame (selected-frame))
        (configuration-list (mapcar #'current-window-configuration
                                    (frame-list)))
        (entry (lookup-key (symbol-value map-variable) key)))
    (unless (arrayp entry)
      (user-error "Bound entry is not array: %s" entry))
    (unwind-protect
        (let ((loophole-buffer (get-buffer-create name)))
          (switch-to-buffer-other-window loophole-buffer t)
          (erase-buffer)
          (insert ";; For modifying array.\n\n")
          (pp entry loophole-buffer)
          (loophole-write-lisp-mode)
          (goto-char 1)
          (let ((array (read loophole-buffer)))
            (if (arrayp array)
                (loophole-bind-entry key array (symbol-value map-variable))
              (user-error
               "Modified Lisp object is not array: %s" array))))
      (let ((configuration
             (seq-find (lambda (c)
                         (eq (selected-frame) (window-configuration-frame c)))
                       configuration-list)))
        (if configuration
            (set-window-configuration configuration)
          (delete-frame)))
      (if (frame-live-p frame) (select-frame-set-input-focus frame t))
      (if (window-live-p window) (select-window window t))
      (if (buffer-live-p buffer) (switch-to-buffer buffer t t)))))

(defun loophole-suspend ()
  "Suspend Loophole.
To suspend Loophole, this function delete
`loophole--map-alist' from `emulation-mode-map-alists'."
  (interactive)
  (setq emulation-mode-map-alists
        (delq 'loophole--map-alist emulation-mode-map-alists))
  (setq loophole--suspended t))

(defun loophole-resume ()
  "Resume Loophole.
To resume Loophole, this functions push
`loophole--map-alist' to `emulation-mode-map-alists'
unless `loophole--map-alist' is a member of
`emulation-mode-map-alists'."
  (interactive)
  (add-to-list 'emulation-mode-map-alists 'loophole--map-alist nil #'eq)
  (setq loophole--suspended nil))

(defun loophole-quit ()
  "Quit loophole completely.
Disable the all keymaps, stop editing, and turn off
`loophole-mode'."
  (interactive)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (loophole-disable-all)
            (loophole-stop-editing)))
        (if (listp loophole--buffer-list)
            loophole--buffer-list
          (buffer-list)))
  (loophole-mode 0)
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode loophole-mode
  "Toggle Loophole mode.

When Loophole mode is enabled, active loophole maps take
effect; i.e., key bindings bound in keymaps which are
registered to Loophole and whose state is non-nil take
effect.

Loophole mode also offers the bindings for the
temporary key bindings management command.

\\{loophole-mode-map}"
  :group 'loophole
  :global t
  :lighter (""
            loophole-mode-lighter-base
            (loophole--editing loophole-mode-lighter-editing-sign)
            (:eval (let ((n (length
                             (delq nil
                                   (mapcar
                                    (lambda (e) (symbol-value (car e)))
                                    loophole--map-alist)))))
                     (if (zerop n)
                         ""
                       (format ":%d" n)))))
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c [") #'loophole-set-key)
            (define-key map (kbd "C-c \\") #'loophole-disable-latest)
            (define-key map (kbd "C-c ] q") #'loophole-quit)
            (define-key map (kbd "C-c ] ,") #'loophole-suspend)
            (define-key map (kbd "C-c ] .") #'loophole-resume)
            (define-key map (kbd "C-c ] ^") #'loophole-prioritize)
            (define-key map (kbd "C-c ] [") #'loophole-start-editing)
            (define-key map (kbd "C-c ] ]") #'loophole-stop-editing)
            (define-key map (kbd "C-c ] {") #'loophole-register)
            (define-key map (kbd "C-c ] }") #'loophole-unregister)
            (define-key map (kbd "C-c ] n") #'loophole-name)
            (define-key map (kbd "C-c ] /") #'loophole-describe)
            (define-key map (kbd "C-c ] s") #'loophole-set-key)
            (define-key map (kbd "C-c ] u") #'loophole-unset-key)
            (define-key map (kbd "C-c ] e") #'loophole-enable)
            (define-key map (kbd "C-c ] d") #'loophole-disable)
            (define-key map (kbd "C-c ] D") #'loophole-disable-all)
            (define-key map (kbd "C-c ] \\") #'loophole-disable-latest)
            (define-key map (kbd "C-c ] (") #'loophole-start-kmacro)
            (define-key map (kbd "C-c ] )") #'loophole-end-kmacro)
            (define-key map (kbd "C-c ] k s") #'loophole-start-kmacro)
            (define-key map (kbd "C-c ] k e") #'loophole-end-kmacro)
            (define-key map (kbd "C-c ] k a") #'loophole-abort-kmacro)
            (define-key map (kbd "C-c ] k b") #'loophole-bind-last-kmacro)
            (define-key map (kbd "C-c ] b e") #'loophole-bind-entry)
            (define-key map (kbd "C-c ] b c") #'loophole-bind-command)
            (define-key map (kbd "C-c ] b k") #'loophole-bind-kmacro)
            (define-key map (kbd "C-c ] b K") #'loophole-bind-last-kmacro)
            (define-key map (kbd "C-c ] b a") #'loophole-bind-array)
            (define-key map (kbd "C-c ] b m") #'loophole-bind-keymap)
            (define-key map (kbd "C-c ] b s") #'loophole-bind-symbol)
            (define-key map (kbd "C-c ] m l") #'loophole-modify-lambda-form)
            (define-key map (kbd "C-c ] m k") #'loophole-modify-kmacro)
            (define-key map (kbd "C-c ] m a") #'loophole-modify-array)
            map)
  (if loophole-mode
      (progn
        (setq loophole--buffer-list nil)
        (mapc (lambda (buffer)
                (with-current-buffer buffer
                  (if (seq-some #'local-variable-p
                                (loophole-local-variable-if-set-list))
                      (push buffer loophole--buffer-list))
                  (dolist (state-variable (loophole-state-variable-list))
                    (if (and (get (get state-variable
                                       :loophole-map-variable) :loophole-global)
                             (local-variable-p state-variable))
                        (set state-variable (default-value state-variable))))))
              (buffer-list))
        (dolist (variable (loophole-local-variable-if-set-list))
          (add-variable-watcher variable
                                #'loophole--follow-adding-local-variable))
        (dolist (state-variable (loophole-state-variable-list))
          (if (get (get state-variable :loophole-map-variable) :loophole-global)
              (add-variable-watcher state-variable
                                    #'loophole--follow-global-state)))
        (mapc (lambda (buffer)
                (with-current-buffer buffer
                  (add-hook 'change-major-mode-hook
                            #'loophole--follow-killing-local-variable nil t)
                  (add-hook 'kill-buffer-hook
                            #'loophole--follow-killing-local-variable nil t)))
              loophole--buffer-list)
        (unless loophole--suspended (loophole-resume)))
    (let ((flag loophole--suspended))
      (loophole-suspend)
      (setq loophole--suspended flag))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (remove-hook 'change-major-mode-hook
                           #'loophole--follow-killing-local-variable t)
              (remove-hook 'kill-buffer-hook
                           #'loophole--follow-killing-local-variable t)))
          loophole--buffer-list)
    (dolist (state-variable (loophole-state-variable-list))
      (if (get (get state-variable :loophole-map-variable) :loophole-global)
          (remove-variable-watcher state-variable
                                   #'loophole--follow-global-state)))
    (dolist (variable (loophole-local-variable-if-set-list))
      (remove-variable-watcher variable
                               #'loophole--follow-adding-local-variable))
    (setq loophole--buffer-list t)))

(defun loophole-mode-set-lighter-format (style &optional format)
  "Set lighter format for loophole mode.
STYLE is a symbol to specify style of format.
STYLE can be 'number', 'tag', 'simple', 'static', 'custom',
and any other Lisp object.  Each means as follows.
number: display lighter-base suffixed with editing status,
        and number of enabled keymaps.  If no keymaps are
        enabled, numeric suffix is omitted.
tag:    display lighter-base suffixed with editing status,
        and concatenated tag strings of keymaps.  If no
        keymaps are enabled, tag suffix is omitted.
simple: display lighter-base suffixed with editing status
static: display lighter-base with no suffix.
custom: use FORMAT.
If STYLE is other than above, lighter is omitted."
  (let ((form (cond
               ((eq style 'number)
                '(""
                  loophole-mode-lighter-base
                  (:eval (if (and (loophole-suspending-p)
                                  (not loophole-mode-lighter-use-face))
                             loophole-mode-lighter-suspending-sign))
                  (loophole--editing loophole-mode-lighter-editing-sign)
                  (:eval (let ((n (length
                                   (delq nil
                                         (mapcar
                                          (lambda (e) (symbol-value (car e)))
                                          loophole--map-alist)))))
                           (if (zerop n)
                               ""
                             (concat ":"
                                     (let ((s (int-to-string n)))
                                       (if loophole-mode-lighter-use-face
                                           (propertize
                                            s 'face
                                            (if (loophole-suspending-p)
                                                'loophole-suspending
                                              'loophole-using))
                                         s))))))))
               ((eq style 'tag)
                '(""
                  loophole-mode-lighter-base
                  (:eval (if (and (loophole-suspending-p)
                                  (not loophole-mode-lighter-use-face))
                             loophole-mode-lighter-suspending-sign))
                  (loophole--editing
                   (""
                    loophole-mode-lighter-editing-sign
                    (:eval (let ((tag (get loophole--editing :loophole-tag)))
                             (if (and loophole-mode-lighter-use-face
                                      (stringp tag))
                                 (propertize tag 'face nil)
                               tag)))))
                  (:eval
                   (let ((l (delq
                             nil
                             (mapcar
                              (lambda (a)
                                (if (symbol-value (car a))
                                    (let ((e (substitute-command-keys
                                              (get (get (car a)
                                                        :loophole-map-variable)
                                                   :loophole-tag))))
                                      (if (and loophole-mode-lighter-use-face
                                               (stringp e))
                                          (propertize
                                           e 'face
                                           (if (loophole-suspending-p)
                                               'loophole-suspending
                                             'loophole-using))
                                        e))))
                              loophole--map-alist))))
                     (if (zerop (length l))
                         ""
                       (concat loophole-tag-sign
                               (mapconcat #'identity l ",")))))))
               ((eq style 'simple)
                '(""
                  loophole-mode-lighter-base
                  (:eval (if (loophole-suspending-p)
                             loophole-mode-lighter-suspending-sign))
                  (loophole--editing loophole-mode-lighter-editing-sign)))
               ((eq style 'static) loophole-mode-lighter-base)
               ((eq style 'custom) format)
               (t "")))
        (cell (assq 'loophole-mode minor-mode-alist)))
    (if cell (setcdr cell (list form)))))

(defun loophole-turn-on-auto-prioritize ()
  "Turn on auto prioritize as user customization.
Add hooks to call `loophole-prioritize' for
`loophole-enable', `loophole-name' and
`loophole-start-editing'.

All of these hooks are optional.
User can choose some of them and add to init file
individually, instead of calling this function."
  (add-hook 'loophole-enable-functions #'loophole-prioritize)
  (add-hook 'loophole-start-editing-functions #'loophole-prioritize)
  (add-hook 'loophole-name-functions #'loophole-prioritize))

(defun loophole-turn-off-auto-prioritize ()
  "Turn off auto prioritize as user customization.
Remove hooks added by `loophole-turn-on-auto-prioritize'."
  (remove-hook 'loophole-enable-functions #'loophole-prioritize)
  (remove-hook 'loophole-start-editing-functions #'loophole-prioritize)
  (remove-hook 'loophole-name-functions #'loophole-prioritize))

(defun loophole-turn-on-auto-stop-editing ()
  "Turn on auto stop-editing as user customization.
Add hooks to call `loophole-stop-editing' for
`loophole-prioritize', `loophole-enable',
`loophole-disable' and `loophole-name'.
 `loophole-disable-latest' and `loophole-disable-all' are
also affected by the hook for `loophole-disable'.

All of these hooks are optional.
User can choose some of them and add to init file
individually, instead of calling this function."
  (add-hook 'loophole-prioritize-functions
            (lambda (map-variable)
              (unless (eq map-variable loophole--editing)
                (loophole-stop-editing))))
  (add-hook 'loophole-enable-functions
            (lambda (map-variable)
              (unless (eq map-variable loophole--editing)
                (loophole-stop-editing))))
  (add-hook 'loophole-disable-functions
            (lambda (map-variable)
              (if (eq map-variable loophole--editing)
                  (loophole-stop-editing))))
  (add-hook 'loophole-name-functions
            (lambda (map-variable)
              (unless (eq map-variable loophole--editing)
                (loophole-stop-editing)))))

(defun loophole-turn-off-auto-stop-editing ()
  "Turn off auto stop-editing as user customization.
Remove hooks added by `loophole-turn-on-auto-stop-editing'."
  (remove-hook 'loophole-prioritize-functions
            (lambda (map-variable)
              (unless (eq map-variable loophole--editing)
                (loophole-stop-editing))))
  (remove-hook 'loophole-enable-functions
            (lambda (map-variable)
              (unless (eq map-variable loophole--editing)
                (loophole-stop-editing))))
  (remove-hook 'loophole-disable-functions
            (lambda (map-variable)
              (if (eq map-variable loophole--editing)
                  (loophole-stop-editing))))
  (remove-hook 'loophole-name-functions
              (lambda (map-variable)
                (unless (eq map-variable loophole--editing)
                  (loophole-stop-editing)))))

(defun loophole-turn-on-auto-resume ()
  "Turn on auto resume as user customization.
Add hooks to call `loophole-resume' for
`loophole-prioritize', `loophole-enable',
`loophole-name', `loophole-start-editing' and
`loophole-bind-entry'.
Binding commands including `loophole-set-key' and
`loophole-unset-key' are also affected by the hook of
`loophole-bind-entry'.

All of these hooks are optional.
User can choose some of them and add to init file
individually, instead of calling this function."
  (add-hook 'loophole-prioritize-functions (lambda (_) (loophole-resume)))
  (add-hook 'loophole-enable-functions (lambda (_) (loophole-resume)))
  (add-hook 'loophole-name-functions (lambda (_) (loophole-resume)))
  (add-hook 'loophole-start-editing-functions (lambda (_) (loophole-resume)))
  (add-hook 'loophole-bind-hook #'loophole-resume))

(defun loophole-turn-off-auto-resume ()
  "Turn off auto resume as user customization.
Remove hooks added by `loophole-turn-on-auto-resume'."
  (remove-hook 'loophole-prioritize-functions (lambda (_) (loophole-resume)))
  (remove-hook 'loophole-enable-functions (lambda (_) (loophole-resume)))
  (remove-hook 'loophole-name-functions (lambda (_) (loophole-resume)))
  (remove-hook 'loophole-start-editing-functions (lambda (_) (loophole-resume)))
  (remove-hook 'loophole-bind-hook #'loophole-resume))

(defun loophole-turn-on-timer ()
  "Turn on timer for activating state as user customization.
Add hooks and advices to take care of timers.

In contrast with `loophole-turn-on-auto-prioritize',
`loophole-turn-on-auto-stop-editing' and
`loophole-turn-on-auto-resume', the hook for
`loophole-unregister-functions', `loophole-mode-hook' and
the advises for `loophole-local-variable-if-set-list' and
`loophole-name' are mandatory.  Others are optional.

When user choose optional hooks and advises, hooks and
advises described above must be added together to init file."
  (add-hook 'loophole-unregister-functions #'loophole--remove-all-timers)
  (advice-add 'loophole-local-variable-if-set-list
               :filter-return
               (lambda (return)
                 "Add `loophole--timer-alist'
  to return value."
                 (cons 'loophole--timer-alist return)))
  (advice-add 'loophole-name
               :after #'loophole--change-map-variable-name-of-timer)
  (add-hook 'loophole-mode-hook
             (lambda ()
               (if loophole-mode
                   (loophole--revive-or-start-all-timers)
                 (loophole--stop-all-timers))))
  (advice-add 'loophole-ready-map
               :filter-return
               (lambda (return)
                 "Call `loophole-start-timer'
  for map-variable of returned keymap, and return original."
                 (if loophole-mode
                     (loophole-start-timer
                      (loophole-map-variable-for-keymap return)))
                 return))
  (add-hook 'loophole-enable-functions
             (lambda (map-variable)
               (if loophole-mode (loophole-start-timer map-variable))))
  (add-hook 'loophole-disable-functions
             (lambda (map-variable)
               (if loophole-mode (loophole-stop-timer map-variable)))))

(defun loophole-turn-off-timer ()
  "Turn off timer for activating state as user customization.
Remove hooks and advises added by `loophole-turn-on-timer'."
  (remove-hook 'loophole-unregister-functions #'loophole--remove-all-timers)
  (advice-remove 'loophole-local-variable-if-set-list
                  (lambda (return)
                    "Add `loophole--timer-alist'
  to return value."
                    (cons 'loophole--timer-alist return)))
  (advice-remove 'loophole-name
                  #'loophole--change-map-variable-name-of-timer)
  (remove-hook 'loophole-mode-hook
                (lambda ()
                  (if loophole-mode
                      (loophole--revive-or-start-all-timers)
                    (loophole--stop-all-timers))))
  (advice-remove 'loophole-ready-map
                  (lambda (return)
                    "Call `loophole-start-timer'
  for map-variable of returned keymap, and return original."
                    (if loophole-mode
                        (loophole-start-timer
                         (loophole-map-variable-for-keymap return)))
                    return))
  (remove-hook 'loophole-enable-functions
                (lambda (map-variable)
                  (if loophole-mode (loophole-start-timer map-variable))))
  (remove-hook 'loophole-disable-functions
                (lambda (map-variable)
                  (if loophole-mode (loophole-stop-timer map-variable)))))

(defun loophole-turn-on-editing-timer ()
  "Turn on timer for editing state as user customization.
Add hooks and advices to take care of timers .

In contrast with `loophole-turn-on-auto-prioritize',
`loophole-turn-on-auto-stop-editing' and
`loophole-turn-on-auto-resume', the advise for
`loophole-local-variable-if-set-list' and the  hook for
`loophole-mode-hook' are mandatory.  Others are optional.

When user choose optional hooks, the advise described above
must be added together to init file."
  (advice-add 'loophole-local-variable-if-set-list
               :filter-return
               (lambda (return)
                 "Add `loophole--editing-timer'
  to return value."
                 (cons 'loophole--editing-timer return)))
  (add-hook 'loophole-mode-hook
             (lambda ()
               (if loophole-mode
                   (loophole--revive-or-start-all-editing-timers)
                 (loophole--stop-all-editing-timers))))
  (add-hook 'loophole-start-editing-functions
             (lambda (_) (if loophole-mode (loophole-start-editing-timer))))
  (add-hook 'loophole-stop-editing-functions
             (lambda (_) (if loophole-mode (loophole-stop-editing-timer)))))

(defun loophole-turn-off-editing-timer ()
  "Turn off timer for editing state as user customization.
Remove hooks and advises added by `loophole-turn-on-editing-timer'."
  (advice-remove 'loophole-local-variable-if-set-list
                 (lambda (return)
                   "Add `loophole--editing-timer'
  to return value."
                   (cons 'loophole--editing-timer return)))
  (remove-hook 'loophole-mode-hook
                (lambda ()
                  (if loophole-mode
                      (loophole--revive-or-start-all-editing-timers)
                    (loophole--stop-all-editing-timers))))
  (remove-hook 'loophole-start-editing-functions
                (lambda (_) (if loophole-mode (loophole-start-editing-timer))))
  (remove-hook 'loophole-stop-editing-functions
                (lambda (_) (if loophole-mode (loophole-stop-editing-timer)))))

(defcustom loophole-use-auto-prioritize t
  "Flag if prioritize loophole map automatically.

Because this option uses :set property, `setq' does not work
for this variable.  Use `custom-set-variables' or call
`loophole-turn-on-auto-prioritize' or
`loophole-turn-off-auto-prioritize' manually.
They setup some hooks.

For more detailed customization, see documentation string of
`loophole-turn-on-auto-prioritize'."
  :group 'loophole
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (loophole-turn-on-auto-prioritize)
           (loophole-turn-off-auto-prioritize))))

(defcustom loophole-use-auto-stop-editing t
  "Flag if stop editing keymap automatically.

Because this option uses :set property, `setq' does not work
for this variable.  Use `custom-set-variables' or call
`loophole-turn-on-auto-stop-editing' or
`loophole-turn-off-auto-stop-editing' manually.
They setup some hooks.

For more detailed customization, see documentation string of
`loophole-turn-on-auto-stop-editing'."
  :group 'loophole
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (loophole-turn-on-auto-stop-editing)
           (loophole-turn-off-auto-stop-editing))))

(defcustom loophole-use-auto-resume t
  "Flag if resume Loophole automatically.

Because this option uses :set property, `setq' does not work
for this variable.  Use `custom-set-variables' or call
`loophole-turn-on-auto-resume' or
`loophole-turn-off-auto-resume' manually.
They setup some hooks.

For more detailed customization, see documentation string of
`loophole-turn-on-auto-resume'."
  :group 'loophole
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (loophole-turn-on-auto-resume)
           (loophole-turn-off-auto-resume))))

(defcustom loophole-use-timer nil
  "Flag if use timer for activating state.

Because this option uses :set property, `setq' does not work
for this variable.  Use `custom-set-variables' or call
`loophole-turn-on-timer' or `loophole-turn-off-timer'
manually.  They setup some hooks and advises.

For more detailed customization, see documentation string of
`loophole-turn-on-timer'."
  :group 'loophole
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (loophole-turn-on-timer)
           (loophole-turn-off-timer))))

(defcustom loophole-use-editing-timer nil
  "Flag if use timer for editing state.

Because this option uses :set property, `setq' does not work
for this variable.  Use `custom-set-variables' or call
`loophole-turn-on-editing-timer' or `loophole-turn-off-editing-timer'
manually.  They setup some hooks and advises.

For more detailed customization, see documentation string of
`loophole-turn-on-editing-timer'."
  :group 'loophole
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (loophole-turn-on-editing-timer)
           (loophole-turn-off-editing-timer))))

(defalias 'loophole-dig 'loophole-set-key)
(defalias 'loophole-bury 'loophole-unset-key)
(defalias 'loophole-cover 'loophole-disable)
(defalias 'loophole-cover-latest 'loophole-disable-latest)
(defalias 'loophole-cover-all 'loophole-disable-all)
(defalias 'loophole-reveal 'loophole-enable)
(defalias 'loophole-edit 'loophole-start-editing)
(defalias 'loophole-break 'loophole-stop-editing)

(provide 'loophole)

;;; loophole.el ends here
