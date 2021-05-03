;;; loophole.el --- Manage temporary key bindings in Emacs

;; Copyright (C) 2020, 2021 0x60DF

;; Author: 0x60DF <0x60df@gmail.com>
;; Created: 30 Aug 2020
;; Version: 0.2.0
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

(defgroup loophole nil
  "Manage temporary key bindings."
  :group 'convenience)

(defvar loophole--map-alist nil
  "Alist of keymaps for loophole.
Syntax is same as `minor-mode-map-alist', i.e. each element
looks like (STATE-VARIABLE . KEYMAP).  STATE-VARIABLE is a
symbol whose boolean value represents if the KEYMAP is
active or not.  KEYMAP is a keymap object.")

(defvar loophole--editing nil
  "When non-nil, Loophole binds keys in the existing keymap.
Specifically, the first entry of `loophole--map-alist' is
used for the binding.")

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
    loophole-obtain-key-and-command-by-key-sequence)
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
    loophole-obtain-key-and-kmacro-by-recall-record)
  "The priority list of methods to obtain key and kmacro for binding.
`loophole-bind-kmacro' refers this variable to select
obtaining method.
First element gets first priority.
Each element should return a list looks like (key kmacro)."
  :group 'loophole
  :type '(repeat symbol))

(defcustom loophole-set-key-order
  '(loophole-obtain-key-and-command-by-symbol
    loophole-obtain-key-and-kmacro-by-recursive-edit
    loophole-obtain-key-and-command-by-key-sequence
    loophole-obtain-key-and-kmacro-by-read-key
    loophole-obtain-key-and-kmacro-by-recall-record)
  "The priority list of methods to obtain key and object for binding.
`loophole-set-key' refers this to select obtaining method.
First element gets first priority.
Each element should return a list looks like (key object)."
  :group 'loophole
  :type '(repeat symbol))

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

(defun loophole-start-editing ()
  "Start keymap edit session."
  (interactive)
  (setq loophole--editing t))

(defun loophole-stop-editing ()
  "Stop keymap edit session."
  (interactive)
  (setq loophole--editing nil))

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

(defun loophole-register (map-variable state-variable &optional tag
                                       without-base-map)
  "Register the set of MAP-VARIABLE and STATE-VARIABLE to loophole.
Optional argument TAG is tag string which may be shown in
mode line.
Unless WITHOUT-BASE-MAP is non-nil, `loophole-base-map' is
set as parent keymap for MAP-VARIABLE."
  (put map-variable :loophole-state-variable state-variable)
  (put state-variable :loophole-map-variable map-variable)
  (put map-variable :loophole-tag tag)
  (unless without-base-map
    (set-keymap-parent (symbol-value map-variable) loophole-base-map))
  (push `(,state-variable . ,(symbol-value map-variable)) loophole--map-alist))

(defun loophole-registered-p (map-variable &optional state-variable)
  "Return non-nil if MAP-VARIABLE is registered to loophole.
If optional argument STATE-VARIABLE is not nil,
Return non-nil if both MAP-VARIABLE and STATE-VARIABLE are
registered, and they are associated."
  (and (if state-variable
           (eq state-variable (get map-variable :loophole-state-variable))
         (setq state-variable (get map-variable :loophole-state-variable)))
       (eq map-variable (get state-variable :loophole-map-variable))
       (assq state-variable loophole--map-alist)))

(defun loophole-prioritize (map-variable)
  "Give first priority to MAP-VARIABLE.
This is done by move the entry in `loophole--map-alist' to
the front.  If precedence is changed, quit current editing
session."
  (interactive
   (let ((map-variable-list (loophole-map-variable-list)))
     (list (cond (map-variable-list
                  (intern (completing-read "Name keymap: " map-variable-list)))
                 (t (user-error "There are no loophole maps"))))))
  (let ((state-variable (get map-variable :loophole-state-variable)))
    (when state-variable
      (unless (eq (assq state-variable loophole--map-alist)
                  (car loophole--map-alist))
        (setq loophole--map-alist
              (assq-delete-all state-variable loophole--map-alist))
        (push `(,state-variable . ,(symbol-value map-variable))
              loophole--map-alist)
        (loophole-stop-editing)))))

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
                 (reverse (loophole-map-variable-list))))
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
                                    (and (not (symbol-value
                                               (get map-var
                                                    :loophole-state-variable)))
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
    (set state-variable nil)
    (unless (loophole-registered-p map-variable state-variable)
      (loophole-register map-variable state-variable tag))
    map-variable))

(defun loophole-ready-map ()
  "Return available temporary keymap.
If currently editing keymap exists, return it; otherwise
generate new one and return it."
  (cond (loophole--editing
         (set (caar loophole--map-alist) t)
         (cdar loophole--map-alist))
        (t (let* ((map-variable (loophole-generate))
                  (state-variable (get map-variable :loophole-state-variable)))
             (loophole-prioritize map-variable)
             (loophole-start-editing)
             (set state-variable t)
             (symbol-value map-variable)))))

(defun loophole-enable-map (map-variable)
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
  (if map-variable
      (let ((state-variable (get map-variable :loophole-state-variable)))
        (when state-variable
          (set state-variable t)))))

(defun loophole-disable-map (map-variable)
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
  (if map-variable
      (let ((state-variable (get map-variable :loophole-state-variable)))
        (when state-variable
          (set state-variable nil)))))

(defun loophole-disable-last-map ()
  "Disable the lastly enabled keymap."
  (interactive)
  (let* ((state-variable
          (seq-find #'symbol-value (loophole-state-variable-list)))
         (map-variable (get state-variable :loophole-map-variable)))
    (if map-variable (loophole-disable-map map-variable))))

(defun loophole-disable-all-maps ()
  "Disable the all keymaps."
  (interactive)
  (mapc (lambda (map-variable)
          (loophole-disable-map map-variable))
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
MAP-NAME.  If prefix-argument is non-nil, TAG is also asked."
  (interactive
   (let* ((map-variable-list (loophole-map-variable-list))
          (arg-map-variable
           (cond (map-variable-list
                  (intern (completing-read "Name keymap: " map-variable-list)))
                 (t (user-error "There are no loophole maps"))))
          (arg-map-name (read-string (format "New name for keymap %s: "
                                             arg-map-variable)))
          (arg-tag (if current-prefix-arg
                       (read-string (format "New tag for keymap %s: "
                                            arg-map-variable)))))
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
    (let ((map-var (intern map-variable-name))
          (state-var (intern state-variable-name)))
      (set map-var (symbol-value map-variable))
      (set state-var (symbol-value state-variable))
      (loophole-register map-var state-var tag))
    (setq loophole--map-alist
          (assq-delete-all state-variable loophole--map-alist))
    (set map-variable nil)
    (set state-variable nil)
    (makunbound map-variable)
    (makunbound state-variable)
    (setplist map-variable nil)
    (setplist state-variable nil)))

(defun loophole-edit (map-variable)
  "Edit MAP-VARIABLE.
`loophole-prioritize' it and `loophole-start-editing'."
  (interactive
   (let ((map-variable-list (loophole-map-variable-list)))
     (list (cond (map-variable-list
                  (intern (completing-read "Name keymap: " map-variable-list)))
                 (t (user-error "There are no loophole maps"))))))
  (loophole-prioritize map-variable)
  (loophole-start-editing))

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
      (list
       key
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
         (funcall read-arbitrary-key-sequence nil))))))

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
                     last-kbd-macro))))

(defun loophole-obtain-key-and-kmacro-by-recall-record ()
  "Return set of key and kmacro obtained by recalling record."
  (let* ((menu-prompting nil)
          (key (loophole-read-key "Set key temporarily: ")))
    (list key (completing-read (format "Set key %s to kmacro: "
                                       (key-description (kbd "C-a")))
                               (mapcar #'car (remq nil (cons (kmacro-ring-head)
                                                             kmacro-ring)))
                               nil t))))

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
  (if loophole--editing
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
  (loophole-disable-all-maps)
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
            (define-key map (kbd "C-c \\") #'loophole-disable-last-map)
            (define-key map (kbd "C-c ] q") #'loophole-quit)
            (define-key map (kbd "C-c ] ,") #'loophole-suspend)
            (define-key map (kbd "C-c ] .") #'loophole-resume)
            (define-key map (kbd "C-c ] ^") #'loophole-prioritize)
            (define-key map (kbd "C-c ] [") #'loophole-start-editing)
            (define-key map (kbd "C-c ] ]") #'loophole-stop-editing)
            (define-key map (kbd "C-c ] n") #'loophole-name)
            (define-key map (kbd "C-c ] +") #'loophole-edit)
            (define-key map (kbd "C-c ] s") #'loophole-set-key)
            (define-key map (kbd "C-c ] u") #'loophole-unset-key)
            (define-key map (kbd "C-c ] e") #'loophole-enable-map)
            (define-key map (kbd "C-c ] d") #'loophole-disable-map)
            (define-key map (kbd "C-c ] D") #'loophole-disable-all-maps)
            (define-key map (kbd "C-c ] \\") #'loophole-disable-last-map)
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
                  (:eval (if (loophole-suspending-p)
                           loophole-mode-lighter-suspending-sign))
                  (loophole--editing loophole-mode-lighter-editing-sign)
                  (:eval (let ((n (length
                                   (delq nil
                                         (mapcar
                                          (lambda (e) (symbol-value (car e)))
                                          loophole--map-alist)))))
                           (if (zerop n)
                               ""
                             (format ":%d" n))))))
               ((eq style 'tag)
                '(""
                  loophole-mode-lighter-base
                  (:eval (if (loophole-suspending-p)
                           loophole-mode-lighter-suspending-sign))
                  (loophole--editing
                   (""
                    loophole-mode-lighter-editing-sign
                    (:eval (let ((s (caar loophole--map-alist)))
                             (or (symbol-value s)
                                 (get (get s :loophole-map-variable)
                                      :loophole-tag))))))
                  (:eval (let ((l (delq nil
                                        (mapcar
                                         (lambda (e)
                                           (if (symbol-value (car e))
                                               (get (get (car e)
                                                         :loophole-map-variable)
                                                    :loophole-tag)))
                                         loophole--map-alist))))
                           (if (zerop (length l))
                               ""
                             (concat "#" (mapconcat 'identity l ",")))))))
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

(provide 'loophole)

;;; loophole.el ends here
