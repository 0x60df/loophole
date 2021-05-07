;;; loophole.el --- Manage temporary key bindings in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2020, 2021 0x60DF

;; Author: 0x60DF <0x60df@gmail.com>
;; Created: 30 Aug 2020
;; Version: 0.3.2
;; Keywords: convenience
;; URL: https://github.com/0x60df/loophole

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

(require 'seq)
(require 'kmacro)
(require 'nadvice)

(defgroup loophole nil
  "Manage temporary key bindings."
  :group 'convenience)

(defvar-local loophole--map-alist nil
  "Alist of keymaps for loophole.
Syntax is same as `minor-mode-map-alist', i.e. each element
looks like (STATE-VARIABLE . KEYMAP).  STATE-VARIABLE is a
symbol whose boolean value represents if the KEYMAP is
active or not.  KEYMAP is a keymap object.")

(defvar loophole--state-alist nil
  "Alist of states for loophole.
Each element looks like (MAP-VARIABLE . (BUFFERS...)).
MAP-VARIABLE is a symbol which holds keymap.
BUFFERS is a list of buffers on which the state of
MAP-VARIABLE is active.")

(defvar loophole--buffer-list nil
  "List of buffers which have local loophole map.")

(defvar-local loophole--editing-map-variable nil
  "Variable of keymap which is currently being edited.
Loophole binds keys in this keymap.  When this value is nil,
Loophole may generate new keymap and bind keys in it.")

(defvar loophole--suspended nil
  "Non-nil if Loophole is suspended manually.
Once Loophole is resumed, this comes back to nil.
To see true state of suspension, use
`loophole-suspending-p' instead of this variable.")

(defvar loophole-base-map (make-sparse-keymap)
  "Base keymap for all Loophole maps.
This keymap will be inherited to all Loophole maps,
except for the case user explicitly decline inheritance.")

(defcustom loophole-temporary-map-max 8
  "Maximum number of temporary keymaps.
When the number of bound temporary keymaps is
`loophole-temporary-map-max' or higher, generating new map
overwrites the earliest used one."
  :group 'loophole
  :type 'integer)

(defcustom loophole-allow-keyboard-quit t
  "If non-nil, binding commands can be quit even while reading keys."
  :group 'loophole
  :type 'boolean)

(defcustom loophole-kmacro-completing-key (where-is-internal
                                           'keyboard-quit nil t)
  "Key sequence to complete definition of keyboard macro."
  :group 'loophole
  :type 'key-sequence)

(defcustom loophole-bind-command-order
  '(loophole-obtain-key-and-command-by-symbol
    loophole-obtain-key-and-command-by-key-sequence
    loophole-obtain-key-and-object)
  "The priority list of methods to obtain key and command for binding.
`loophole-bind-command' refers this variable to select
obtaining method.
First element gets first priority.
Each element should return a list looks like (key command)."
  :group 'loophole
  :type '(repeat symbol))

(defcustom loophole-bind-kmacro-order
  '(loophole-obtain-key-and-kmacro-by-recursive-edit
    loophole-obtain-key-and-kmacro-by-read-key
    loophole-obtain-key-and-kmacro-by-recall-record
    loophole-obtain-key-and-object)
  "The priority list of methods to obtain key and kmacro for binding.
`loophole-bind-kmacro' refers this variable to select
obtaining method.
First element gets first priority.
Each element should return a list looks like (key kmacro)."
  :group 'loophole
  :type '(repeat symbol))

(defcustom loophole-bind-array-order
  '(loophole-obtain-key-and-object)
  "The priority list of methods to obtain key and array for binding.
`loophole-bind-array' refers this variable to select
obtaining method.
First element gets first priority.
Each element should return a list looks like (key array)."
  :group 'loophole
  :type '(repeat symbol))

(defcustom loophole-bind-keymap-order
  '(loophole-obtain-key-and-object)
  "The priority list of methods to obtain key and keymap for binding.
`loophole-bind-keymap' refers this variable to select
obtaining method.
First element gets first priority.
Each element should return a list looks like (key keymap)."
  :group 'loophole
  :type '(repeat symbol))

(defcustom loophole-set-key-order
  '(loophole-obtain-key-and-command-by-symbol
    loophole-obtain-key-and-kmacro-by-recursive-edit
    loophole-obtain-key-and-command-by-key-sequence
    loophole-obtain-key-and-kmacro-by-read-key
    loophole-obtain-key-and-kmacro-by-recall-record
    loophole-obtain-key-and-object)
  "The priority list of methods to obtain key and object for binding.
`loophole-set-key' refers this to select obtaining method.
First element gets first priority.
Each element should return a list looks like (key object)."
  :group 'loophole
  :type '(repeat symbol))

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

(defun loophole-suspending-p ()
  "Non-nil during suspending Loophole.
During suspension, `loophole--map-alist' is removed from
`emulation-mode-map-alists'.
Consequently, all loophole maps lose effect while its state
is preserved."
  (not (memq 'loophole--map-alist emulation-mode-map-alists)))

(defun loophole--follow-buffer-local-condition ()
  "Update variables for buffer local information.
This function is intended to be added to
`change-major-mode-hook' and `kill-buffer-hook'.
`loophole--state-alist' and `loophole--buffer-list' will be
modified."
  (when (memq (current-buffer) loophole--buffer-list)
    (mapc (lambda (cell)
            (setcdr cell (delq (current-buffer) (cdr cell))))
          loophole--state-alist)
    (setq loophole--buffer-list (delq (current-buffer) loophole--buffer-list))))

(defun loophole-register (map-variable state-variable &optional tag
                                       without-base-map)
  "Register the set of MAP-VARIABLE and STATE-VARIABLE to loophole.
Optional argument TAG is a tag string which may be shown in
mode line.  TAG should not contain `loophole-tag-sign',
because tag will be prefixed by `loophole-tag-sign' on the
mode-line.
Unless WITHOUT-BASE-MAP is non-nil, `loophole-base-map' is
set as parent keymap for MAP-VARIABLE."
  (put map-variable :loophole-state-variable state-variable)
  (put state-variable :loophole-map-variable map-variable)
  (put map-variable :loophole-tag tag)
  (unless (local-variable-if-set-p state-variable)
    (make-variable-buffer-local state-variable))
  (unless without-base-map
    (set-keymap-parent (symbol-value map-variable) loophole-base-map))
  (setq-default loophole--map-alist
                (cons `(,state-variable . ,(symbol-value map-variable))
                      (default-value 'loophole--map-alist)))
  (mapc (lambda (buffer)
          (with-current-buffer buffer
                  (push `(,state-variable . ,(symbol-value map-variable))
                        loophole--map-alist)))
        loophole--buffer-list)
  (push `(,map-variable . ()) loophole--state-alist))

(defun loophole-registered-p (map-variable &optional state-variable)
  "Return non-nil if MAP-VARIABLE is registered to loophole.
If optional argument STATE-VARIABLE is not nil,
Return non-nil if both MAP-VARIABLE and STATE-VARIABLE are
registered, and they are associated."
  (and (if state-variable
           (eq state-variable (get map-variable :loophole-state-variable))
         (setq state-variable (get map-variable :loophole-state-variable)))
       (eq map-variable (get state-variable :loophole-map-variable))
       (assq state-variable (default-value 'loophole--map-alist))
       (assq map-variable loophole--state-alist)))

(defun loophole-prioritize (map-variable)
  "Give first priority to MAP-VARIABLE.
This is done by move the entry in `loophole--map-alist' to
the front."
  (interactive
   (let ((map-variable-list (loophole-map-variable-list)))
     (list (cond (map-variable-list
                  (intern (completing-read "Name keymap: " map-variable-list)))
                 (t (user-error "There are no loophole maps"))))))
  (let ((state-variable (get map-variable :loophole-state-variable)))
    (when state-variable
      (unless (eq (assq state-variable loophole--map-alist)
                  (car loophole--map-alist))
        (unless (local-variable-p 'loophole--map-alist)
          (add-to-list 'loophole--buffer-list (current-buffer) nil #'eq)
          (add-hook 'change-major-mode-hook
                    #'loophole--follow-buffer-local-condition nil t)
          (add-hook 'kill-buffer-hook
                    #'loophole--follow-buffer-local-condition nil t))
        (setq loophole--map-alist
              (seq-filter (lambda (cell)
                            (not (eq (car cell) state-variable)))
                          loophole--map-alist))
        (push `(,state-variable . ,(symbol-value map-variable))
              loophole--map-alist)
        (setq-default
         loophole--map-alist
         (cons `(,state-variable . ,(symbol-value map-variable))
               (seq-filter (lambda (cell)
                             (not (eq (car cell) state-variable)))
                           (default-value 'loophole--map-alist))))))))

(defun loophole-generate ()
  "Return Loophole map variable which holds newly generated keymap.

Name of map variable is loophole-n-map.
If the number of temporary keymap is
`loophole-temporary-map-max' or higher, earliest used one
will be overwritten by new sparse keymap.

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
                                 (funcall find-nonbound-temporary-map-variable
                                          (+ i 1)))
                                (t s)))))
                     (find-earliest-used-disabled-temporary-map-variable
                      (lambda ()
                        (seq-find (lambda (map-var)
                                    (and (null (assq map-var
                                                     loophole--state-alist))
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
                                 (funcall find-orphan-temporary-map-variable
                                          (+ i 1)))
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
         (state-variable (intern (concat (symbol-name map-variable) "-state")))
         (tag (replace-regexp-in-string
               "loophole-\\([0-9]+\\)-map" "\\1"
               (symbol-name map-variable))))
    (set map-variable (make-sparse-keymap))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (loophole-disable map-variable)
              (force-mode-line-update)))
          (cdr (assq map-variable loophole--state-alist)))
    (set state-variable nil)
    (unless (local-variable-if-set-p state-variable)
      (make-variable-buffer-local state-variable))
    (unless (loophole-registered-p map-variable state-variable)
      (loophole-register map-variable state-variable tag))
    map-variable))

(defun loophole-ready-map ()
  "Return available temporary keymap.
If currently editing keymap exists, return it; otherwise
generate new one and return it."
  (cond (loophole--editing-map-variable
         (symbol-value loophole--editing-map-variable))
        (t (let ((map-variable (loophole-generate)))
             (loophole-prioritize map-variable)
             (loophole-start-editing map-variable)
             (set (get map-variable :loophole-state-variable) t)
             (let ((cell (assq map-variable loophole--state-alist)))
               (unless (memq (current-buffer) (cdr cell))
                 (push (current-buffer) (cdr cell))))
             (unless (local-variable-p 'loophole--map-alist)
               (add-to-list 'loophole--buffer-list (current-buffer) nil #'eq)
               (add-hook 'change-major-mode-hook
                         #'loophole--follow-buffer-local-condition nil t)
               (add-hook 'kill-buffer-hook
                         #'loophole--follow-buffer-local-condition nil t)
               (make-local-variable 'loophole--map-alist))
             (symbol-value map-variable)))))

(defun loophole-enable (map-variable)
  "Enable the keymap stored in MAP-VARIABLE."
  (interactive
   (let ((disabled-map-variable-list
          (seq-filter (lambda (map-variable)
                        (not (symbol-value (get map-variable
                                                :loophole-state-variable))))
                      (loophole-map-variable-list))))
     (list
      (cond (disabled-map-variable-list
             (intern (completing-read "Enable keymap temporarily: "
                                      disabled-map-variable-list)))
            (t (message "There are no disabled loophole maps.")
               nil)))))
  (if (loophole-registered-p map-variable)
      (let ((state-variable (get map-variable :loophole-state-variable)))
        (when state-variable
          (set state-variable t)
          (let ((cell (assq map-variable loophole--state-alist)))
            (unless (memq (current-buffer) (cdr cell))
              (push (current-buffer) (cdr cell))))
          (unless (local-variable-p 'loophole--map-alist)
            (add-to-list 'loophole--buffer-list (current-buffer) nil #'eq)
            (add-hook 'change-major-mode-hook
                      #'loophole--follow-buffer-local-condition nil t)
            (add-hook 'kill-buffer-hook
                      #'loophole--follow-buffer-local-condition nil t)
            (make-local-variable 'loophole--map-alist))))
    (user-error "Specified map-variable %s is not registered" map-variable)))

(defun loophole-disable (map-variable)
  "Disable the keymap stored in MAP-VARIABLE."
  (interactive
   (let ((enabled-map-variable-list
          (seq-filter (lambda (map-variable)
                        (symbol-value (get map-variable
                                           :loophole-state-variable)))
                      (loophole-map-variable-list))))
     (list
      (cond (enabled-map-variable-list
             (intern (completing-read "Disable keymap temporarily: "
                                      enabled-map-variable-list)))
            (t (message "There are no enabled loophole maps.")
               nil)))))
  (if (loophole-registered-p map-variable)
      (let ((state-variable (get map-variable :loophole-state-variable)))
        (when state-variable
          (set state-variable nil)
          (let ((cell (assq map-variable loophole--state-alist)))
            (setcdr cell (delq (current-buffer) (cdr cell))))))
    (user-error "Specified map-variable %s is not registered" map-variable)))

(defun loophole-disable-latest ()
  "Disable the lastly added or prioritized active keymap."
  (interactive)
  (let* ((state-variable
          (seq-find #'symbol-value (loophole-state-variable-list)))
         (map-variable (get state-variable :loophole-map-variable)))
    (if map-variable (loophole-disable map-variable))))

(defun loophole-disable-all ()
  "Disable the all keymaps."
  (interactive)
  (mapc (lambda (map-variable)
          (loophole-disable map-variable))
        (loophole-map-variable-list)))

(defun loophole-name (map-variable map-name &optional tag)
  "Name Loophole map MAP-VARIABLE as MAP-NAME.
If optional argument TAG is non-nil, tag string for the map
is set as TAG; otherwise, initial character of MAP-NAME is
used as tag string.
Old MAP-VARIABLE and corresponding state-variable are
initialized: they are unbound, their plists are set as nil,
they are removed from `loophole--map-alist'.

When interactive call, this function asks MAP-VARIABLE and
MAP-NAME.  If prefix-argument is non-nil, TAG is also asked.
If MAP-NAME contains `loophole-tag-sign', use a following
string as TAG regardless of the value of prefix-argument."
  (interactive
   (let* ((map-variable-list (loophole-map-variable-list))
          (arg-map-variable
           (cond (map-variable-list
                  (intern (completing-read "Name keymap: " map-variable-list)))
                 (t (user-error "There are no loophole maps"))))
          (arg-map-name (read-string (format "New name[%stag] for keymap %s: "
                                             loophole-tag-sign
                                             arg-map-variable)))
          (arg-tag (let ((match (string-match loophole-tag-sign arg-map-name)))
                     (cond (match
                            (prog1
                                (substring arg-map-name
                                           (+ match 1)
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
    (let ((bound (seq-find (lambda (name)
                             (boundp (intern name)))
                           (list map-variable-name state-variable-name ))))
      (if bound (user-error "Specified name %s has already been bound" bound)))
    (let ((named-map-variable (intern map-variable-name))
          (named-state-variable (intern state-variable-name)))
      (set named-map-variable (symbol-value map-variable))
      (make-variable-buffer-local named-state-variable)
      (set-default named-state-variable nil)
      (mapc
       (lambda (buffer)
         (with-current-buffer buffer
           (set named-state-variable (symbol-value state-variable))))
       loophole--buffer-list)
      (put named-map-variable :loophole-state-variable named-state-variable)
      (put named-state-variable :loophole-map-variable named-map-variable)
      (put named-map-variable :loophole-tag tag)
      (let ((cell (assq state-variable (default-value 'loophole--map-alist))))
        (setcar cell named-state-variable))
      (mapc (lambda (buffer)
              (with-current-buffer buffer
                (let ((cell (assq state-variable loophole--map-alist)))
                  (if (consp cell)
                      (setcar cell named-state-variable)))))
            loophole--buffer-list)
      (setcar (assq map-variable loophole--state-alist) named-map-variable)
      (mapc (lambda (buffer)
              (with-current-buffer buffer
                (if (eq map-variable loophole--editing-map-variable)
                  (setq loophole--editing-map-variable named-map-variable))))
            loophole--buffer-list))
    (set map-variable nil)
    (set state-variable nil)
    (makunbound map-variable)
    (makunbound state-variable)
    (setplist map-variable nil)
    (setplist state-variable nil)))

(defun loophole-start-editing (map-variable)
  "Start keymap edit session with MAP-VARIABLE."
  (interactive
   (let ((map-variable-list (loophole-map-variable-list)))
     (list (cond (map-variable-list
                  (intern (completing-read
                           "Name keymap: " map-variable-list)))
                 (t (user-error "There are no loophole maps"))))))
  (setq loophole--editing-map-variable map-variable))

(defun loophole-stop-editing ()
  "Stop keymap edit session."
  (interactive)
  (setq loophole--editing-map-variable nil))

;;;###autoload
(defun loophole-start-kmacro ()
  "Start defining keyboard macro.
Definition can be finished by calling `loophole-end-kmacro'."
  (interactive)
  (kmacro-start-macro nil)
  (let* ((complete (where-is-internal 'loophole-end-kmacro nil t))
         (abort (where-is-internal 'loophole-abort-kmacro nil t))
         (body (if (and complete abort)
                   (format "[Complete: %s, Abort: %s]"
                           (key-description complete)
                           (key-description abort))
                 "[loophole-end/abort-kmacro should be bound to key]")))
    (message "Defining keyboard macro... %s" body))
  (if (not (called-interactively-p 'any))
      (recursive-edit)))

(defun loophole-end-kmacro ()
  "End defining keyboard macro."
  (interactive)
  (unwind-protect
      (kmacro-end-macro nil)
    (if (not (zerop (recursion-depth)))
        (exit-recursive-edit))))

(defun loophole-abort-kmacro ()
  "Abort defining keyboard macro."
  (interactive)
  (if (not (zerop (recursion-depth)))
      (abort-recursive-edit))
  (keyboard-quit))

(defun loophole-obtain-key-and-object ()
  "Return set of key and any Lisp object.
Object is obtained as return value of `eval-minibuffer'."
  (let* ((menu-prompting nil)
         (key (loophole-read-key "Set key temporarily: ")))
    (list key (eval-minibuffer (format "Set key %s to entry: "
                                       (key-description key))))))

(defun loophole-obtain-key-and-command-by-symbol ()
  "Return set of key and command obtained by reading minibuffer."
  (let* ((menu-prompting nil)
          (key (loophole-read-key "Set key temporarily: ")))
    (list key (read-command (format "Set key %s to command: "
                                    (key-description key))))))

(defun loophole-obtain-key-and-command-by-key-sequence ()
  "Return set of key and command obtained by key sequence lookup."
  (let* ((menu-prompting nil)
         (key (loophole-read-key "Set key temporarily: ")))
    (list key (let ((binding
                     (key-binding (loophole-read-key
                                   (format
                                    "Set key %s to command bound for: "
                                    (key-description key))))))
                (message "%s" binding)
                binding))))

(defun loophole-obtain-key-and-kmacro-by-read-key ()
  "Return set of key and kmacro obtained by reading key.
This function `read-key' recursively.  When you finish
keyboard macro, type `loophole-kmacro-completing-key'.
By default, `loophole-kmacro-completing-key' is \\[keyboard-quit]
the key bound to `keyboard-quit'.  In this situation, you
cannot use \\[keyboard-quit] for quitting.
Once `loophole-kmacro-completing-key' is changed, you can
complete definition of kmacro by new completing key, and
\\[keyboard-quit] takes effect as quit."
  (let ((complete (vconcat loophole-kmacro-completing-key))
        (quit (vconcat (where-is-internal 'keyboard-quit nil t))))
    (or (vectorp complete)
        (stringp complete)
        (vectorp quit)
        (stringp quit)
        (user-error "Neither completing key nor quitting key is invalid"))
    (let* ((menu-prompting nil)
           (key (loophole-read-key "Set key temporarily: ")))
      (letrec
          ((read-arbitrary-key-sequence
            (lambda (v)
              (let* ((k (vector
                         (read-key
                          (format "Set key %s to kmacro: (%s to complete) [%s]"
                                  (key-description key)
                                  (key-description complete)
                                  (mapconcat (lambda (e)
                                               (key-description (vector e)))
                                             (reverse v)
                                             " ")))))
                     (v (vconcat k v)))
                (cond ((loophole-key-equal
                        (seq-take v (length complete))
                        complete)
                       (reverse (seq-drop v (length complete))))
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
  (let* ((menu-prompting nil)
          (key (loophole-read-key "Set key temporarily: ")))
    (list key (progn (loophole-start-kmacro)
                     (kmacro-lambda-form (kmacro-ring-head))))))

(defun loophole-obtain-key-and-kmacro-by-recall-record ()
  "Return set of key and kmacro obtained by recalling record."
  (let* ((menu-prompting nil)
         (key (loophole-read-key "Set key temporarily: ")))
    (letrec
        ((make-label-kmacro-alist
          (lambda (ring counter)
            (let* ((raw-label (key-description (car (car ring))))
                   (entry (assoc raw-label counter))
                   (label (if entry
                              (format "%s <%d>" raw-label (+ (cdr entry) 1))
                            raw-label)))
              (cond ((null ring) ring)
                    (t (cons
                        `(,label . ,(car ring))
                        (funcall make-label-kmacro-alist
                                 (cdr ring)
                                 (cons (if entry
                                           `(,raw-label . ,(+ (cdr entry) 1))
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

(defun loophole-prefix-rank-value (arg)
  "Return rank value for raw prefix argument ARG.
In the context of this function, rank of prefix argument is
defined as follows.
The rank of no prefix argument is 0.
The rank of prefix argument specified by C-u and C-1 is 1,
The rank of C-u C-u and C-2 is 2,
Likewise, rank n means C-u * n or C-n."
  (cond ((null arg) 0)
        ((listp arg) (truncate (log (prefix-numeric-value arg) 4)))
        ((natnump arg) arg)
        (t 0)))

;;;###autoload
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
        (let* ((state-variable (car (rassq keymap loophole--map-alist)))
               (map-variable (get state-variable :loophole-map-variable)))
          (if (and keymap
                   map-variable
                   (loophole-registered-p map-variable)
                   (eq (symbol-value map-variable) keymap))
              keymap
            (error "Invalid keymap: %s" keymap)))
      (loophole-ready-map))
    key
    entry))

;;;###autoload
(defun loophole-bind-command (key command &optional keymap)
  "Bind KEY to COMMAND temporarily.
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
C-u and C-1 invokes the second element,
C-u C-u and C-2 invokes the third one.
Likewise C-u * n and C-n invoke the (n+1)th element."
  (interactive
   (let* ((n (loophole-prefix-rank-value current-prefix-arg))
          (obtaining-method (elt loophole-bind-command-order n)))
     (if (null obtaining-method)
         (user-error "Undefined prefix argument"))
     (funcall obtaining-method)))
  (if (commandp command)
      (loophole-bind-entry key command keymap)
    (error "Invalid command: %s" command)))

;;;###autoload
(defun loophole-bind-kmacro (key kmacro &optional keymap)
  "Bind KEY to KMACRO temporarily.
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
C-u and C-1 invokes the second element,
C-u C-u and C-2 invokes the third one.
Likewise C-u * n and C-n invoke the (n+1)th element."
  (interactive
   (let* ((n (loophole-prefix-rank-value current-prefix-arg))
          (obtaining-method (elt loophole-bind-kmacro-order n)))
     (if (null obtaining-method)
         (user-error "Undefined prefix argument"))
     (funcall obtaining-method)))
  (if (or (vectorp kmacro)
          (stringp kmacro)
          (kmacro-p kmacro))
      (loophole-bind-entry key kmacro keymap)
    (error "Invalid kmacro: %s" kmacro)))

;;;###autoload
(defun loophole-bind-last-kmacro (key)
  "Bind KEY to the lastly accessed keyboard macro.
Currently editing keymap or generated new one is used for
binding."
  (interactive
   (let* ((menu-prompting nil)
          (key (loophole-read-key "Set key temporarily: ")))
     (list key)))
  (loophole-bind-kmacro key (kmacro-lambda-form (kmacro-ring-head))))

;;;###autoload
(defun loophole-bind-array (key array &optional keymap)
  "Bind KEY to ARRAY temporarily.
ARRAY is either a string or vector.
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
C-u and C-1 invokes the second element,
C-u C-u and C-2 invokes the third one.
Likewise C-u * n and C-n invoke the (n+1)th element."
  (interactive
   (let* ((n (loophole-prefix-rank-value current-prefix-arg))
          (obtaining-method (elt loophole-bind-array-order n)))
     (if (null obtaining-method)
         (user-error "Undefined prefix argument"))
     (funcall obtaining-method)))
  (if (arrayp array)
      (loophole-bind-entry key array keymap)
    (error "Invalid array : %s" array)))

;;;###autoload
(defun loophole-bind-keymap (key another-keymap &optional keymap)
  "Bind KEY to ANOTHER-KEYMAP temporarily.
ANOTHER-KEYMAP is a keymap object, and KEY will be a prefix
key for ANOTHER-KEYMAP.
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
C-u and C-1 invokes the second element,
C-u C-u and C-2 invokes the third one.
Likewise C-u * n and C-n invoke the (n+1)th element."
  (interactive
   (let* ((n (loophole-prefix-rank-value current-prefix-arg))
          (obtaining-method (elt loophole-bind-keymap-order n)))
     (if (null obtaining-method)
         (user-error "Undefined prefix argument"))
     (funcall obtaining-method)))
  (if (keymapp another-keymap)
      (loophole-bind-entry key another-keymap keymap)
    (error "Invalid keymap : %s" another-keymap)))

;;;###autoload
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
C-u and C-1 invokes the second element,
C-u C-u and C-2 invokes the third one.
Likewise C-u * n and C-n invoke the (n+1)th element."
  (interactive
   (let* ((n (loophole-prefix-rank-value current-prefix-arg))
          (obtaining-method (elt loophole-set-key-order n)))
     (if (null obtaining-method)
         (user-error "Undefined prefix argument"))
     (funcall obtaining-method)))
  (loophole-bind-entry key entry))

(defun loophole-unset-key (key)
  "Unset the temporary biding of KEY."
  (interactive "kUnset key temporarily: ")
  (if loophole--editing-map-variable
      (define-key (cdar loophole--map-alist) key nil)))

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
  (unless (memq 'loophole--map-alist emulation-mode-map-alists)
    (push 'loophole--map-alist emulation-mode-map-alists))
  (setq loophole--suspended nil))

(defun loophole-quit ()
  "Quit loophole completely.
Disable the all keymaps, and turn off `loophole-mode'."
  (interactive)
  (loophole-disable-all)
  (loophole-mode 0))

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
            (loophole--editing-map-variable loophole-mode-lighter-editing-sign)
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
            (define-key map (kbd "C-c ] n") #'loophole-name)
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
            map)
  (if loophole-mode
      (unless loophole--suspended
        (loophole-resume))
    (let ((flag loophole--suspended))
      (loophole-suspend)
      (setq loophole--suspended flag))))

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
                  (loophole--editing-map-variable
                   loophole-mode-lighter-editing-sign)
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
                  (loophole--editing-map-variable
                   (""
                    loophole-mode-lighter-editing-sign
                    (:eval (let ((tag (get loophole--editing-map-variable
                                         :loophole-tag)))
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
                                    (let ((e (get (get (car a)
                                                       :loophole-map-variable)
                                                  :loophole-tag)))
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
                               (mapconcat 'identity l ",")))))))
               ((eq style 'simple)
                '(""
                  loophole-mode-lighter-base
                  (:eval (if (loophole-suspending-p)
                           loophole-mode-lighter-suspending-sign))
                  (loophole--editing-map-variable
                   loophole-mode-lighter-editing-sign)))
               ((eq style 'static) loophole-mode-lighter-base)
               ((eq style 'custom) format)
               (t "")))
        (cell (assq 'loophole-mode minor-mode-alist)))
    (if cell (setcdr cell (list form)))))

(defun loophole-turn-on-auto-prioritize ()
  "Turn on auto prioritize.
Add advices to call `loophole-prioritize' for
`loophole-enable', `loophole-name' and
`loophole-start-editing'."
  (advice-add 'loophole-enable
              :after (lambda (map-variable) (loophole-prioritize map-variable)))
  (advice-add 'loophole-start-editing
              :after (lambda (map-variable) (loophole-prioritize map-variable)))
  (advice-add 'loophole-name
              :after (lambda (_map-variable map-name _tag)
                       (loophole-prioritize
                        (intern
                         (format "loophole-%s-map" map-name))))))

(defun loophole-turn-off-auto-prioritize ()
  "Turn off auto prioritize.
Remove advices added by `loophole-turn-on-auto-prioritize'."
  (advice-remove 'loophole-enable
                 (lambda (map-variable) (loophole-prioritize map-variable)))
  (advice-remove 'loophole-start-editing
                 (lambda (map-variable) (loophole-prioritize map-variable)))
  (advice-remove 'loophole-name (lambda (_map-variable map-name _tag)
                                  (loophole-prioritize
                                   (intern
                                    (format "loophole-%s-map" map-name))))))

(defun loophole-turn-on-auto-stop-editing ()
  "Turn on auto stop-editing.
Add advices to call `loophole-stop-editing' for
`loophole-prioritize', `loophole-enable',
`loophole-disable', `loophole-disable-all' and
`loophole-name'.
 `loophole-disable-latest' is also affected by the
advice for `loophole-disable'."
  (advice-add 'loophole-prioritize
              :after (lambda (map-variable)
                       (unless (eq map-variable loophole--editing-map-variable)
                         (loophole-stop-editing))))
  (advice-add 'loophole-enable
              :after (lambda (map-variable)
                       (unless (eq map-variable loophole--editing-map-variable)
                         (loophole-stop-editing))))
  (advice-add 'loophole-disable
              :after (lambda (map-variable)
                       (if (eq map-variable loophole--editing-map-variable)
                           (loophole-stop-editing))))
  (advice-add 'loophole-disable-all :after #'loophole-stop-editing)
  (advice-add 'loophole-name
              :after (lambda (_map-variable map-name _tag)
                       (let ((map-var (intern
                                       (format "loophole-%s-map" map-name))))
                         (unless (eq map-var loophole--editing-map-variable)
                           (loophole-stop-editing))))))

(defun loophole-turn-off-auto-stop-editing ()
  "Turn off auto stop-editing.
Remove advices added by `loophole-turn-on-auto-stop-editing'."
  (advice-remove 'loophole-prioritize
                 (lambda (map-variable)
                   (unless (eq map-variable loophole--editing-map-variable)
                     (loophole-stop-editing))))
  (advice-remove 'loophole-enable
                 (lambda (map-variable)
                   (unless (eq map-variable loophole--editing-map-variable)
                     (loophole-stop-editing))))
  (advice-remove 'loophole-disable
                 (lambda (map-variable)
                   (if (eq map-variable loophole--editing-map-variable)
                       (loophole-stop-editing))))
  (advice-remove 'loophole-disable-all  #'loophole-stop-editing)
  (advice-remove 'loophole-name
                 (lambda (_map-variable map-name _tag)
                   (let ((map-var (intern (format "loophole-%s-map" map-name))))
                     (unless (eq map-var loophole--editing-map-variable)
                       (loophole-stop-editing))))))

(defun loophole-turn-on-auto-resume ()
  "Turn on auto resume .
Add advices to call `loophole-resume' for
`loophole-prioritize', `loophole-enable',
`loophole-name', `loophole-start-editing' and
`loophole-bind-entry'.
Binding commands including `loophole-set-key' also affected
by the advice of `loophole-bind-entry'."
  (advice-add 'loophole-prioritize
              :after (lambda (&rest _) (loophole-resume)))
  (advice-add 'loophole-enable
              :after (lambda (&rest _) (loophole-resume)))
  (advice-add 'loophole-name
              :after (lambda (&rest _) (loophole-resume)))
  (advice-add 'loophole-start-editing
              :after (lambda (&rest _) (loophole-resume)))
  (advice-add 'loophole-bind-entry
              :after (lambda (&rest _) (loophole-resume))))

(defun loophole-turn-off-auto-resume ()
  "Turn off auto prioritize.
Remove advices added by `loophole-turn-on-auto-resume'."
  (advice-remove 'loophole-prioritize (lambda (&rest _) (loophole-resume)))
  (advice-remove 'loophole-enable (lambda (&rest _) (loophole-resume)))
  (advice-remove 'loophole-name (lambda (&rest _) (loophole-resume)))
  (advice-remove 'loophole-start-editing (lambda (&rest _) (loophole-resume)))
  (advice-remove 'loophole-bind-entry (lambda (&rest _) (loophole-resume))))

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
