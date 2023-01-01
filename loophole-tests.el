;;; loophole-tests.el --- Tests for Loophole -*- lexical-binding: t -*-

;; Copyright (C) 2022 0x60DF

;; Author: 0x60DF <0x60df@gmail.com>
;; URL: https://github.com/0x60df/loophole
;; Package-Requires: ((emacs "27.1") (loophole "0.8.3"))

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

;; Loophole-tests provides the tests for Loophole.
;; Load this library and call `ert' to run the tests.

;;; Code:

(require 'loophole)
(require 'ert)
(require 'seq)

;; Test tools

(defvar loophole--test-barriered-symbol-list nil
  "List of symbols which is barriered in testing environment.
In `loophole--test-with-pseudo-environment', `intern' and
`unintern' applied to the symbols listed in this variable
or the symbols prefixed by loophole are deflected to
temporary `obarray'.
Note that, if obarray is specified explicitly for
 `intern' and `unintern', deflection does not performed.")

(defconst loophole--test-wait-time 0.5
  "Time to signal `loophole-test-error' for tests that reads events.")

(define-error 'loophole-test-error "Loophole test error" 'error)

(defmacro loophole--test-with-pseudo-environment (&rest body)
  "Evaluate BODY with pseudo environment for Loophole.

In this macro, temporary buffer is set as current by
`with-temp-buffer'.
All the global Loophole variables and
`emulation-mode-map-alists' are let-bound to isolate them
from ordinary environment and prevent side effect.
Local Loophole variables are saved and killed.  Saved values
are restored by `unwind-protect'.
`intern' and `unintern' are advised to use temporary obarray
instead of standard `obarray' if specified symbol has prefix
loophole, or is a member of
`loophole--test-barriered-symbol-list'.
Advice are removed after evaluating BODY by `unwind-protet'.
The symbol interned to temporary obarray is isolated from
oridinary environment, and barriered from side effect.
Variable watcher and hook functions added by `loophole-mode'
are temporarily removed to prevenet side effect.

Symbols which are manipulated by the form being tested
should be referred via `intern'.

Most variables are set as initial value.  User options for
automation whose default value is t are set as nil.
`emulation-mode-map-alists' is set as a list containing
only `loophole--map-alist'.
Default value of local variables are set as initial value."
  (declare (debug t) (indent 0))
  (let ((temp-obarray (make-symbol "temp-obarray"))
        (mode-state (make-symbol "mode-state"))
        (local-variable-if-set-list (make-symbol "local-variable-if-set-list"))
        (local-variable-alist (make-symbol "local-variable-alist")))
    `(let ((loophole--buffer-list t)
           (loophole--suspended nil)
           (loophole-base-map (make-sparse-keymap))
           (loophole-temporary-map-max 8)
           (loophole-allow-keyboard-quit t)
           (loophole-decide-obtaining-method-after-read-key
            'negative-argument)
           (loophole-protect-keymap-entry t)
           (loophole-read-key-limit-time 1.0)
           (loophole-read-key-termination-key (where-is-internal
                                               'keyboard-quit nil t))
           (loophole-read-buffer-inhibit-recursive-edit nil)
           (loophole-read-buffer-finish-key [?\C-c ?\C-c])
           (loophole-read-buffer-abort-key [?\C-c ?\C-k])
           (loophole-force-overwrite-bound-variable t)
           (loophole-force-make-variable-buffer-local t)
           (loophole-force-unintern nil)
           (loophole-make-register-always-read-tag 'infer)
           (loophole-make-describe-use-builtin nil)
           (loophole-timer-default-time (* 60 60))
           (loophole-editing-timer-default-time (* 60 5))
           (loophole-command-by-lambda-form-format
            (concat "(lambda (&optional arg)\n"
                    "  \"Temporary command on `loophole'.\"\n"
                    "  (interactive \"P\")\n"
                    "  (#))"))
           (loophole-defining-kmacro-map
            (let ((map (make-sparse-keymap)))
              (define-key map "\C-c[" #'loophole-end-kmacro)
              (define-key map "\C-c\\" #'loophole-abort-kmacro)
              map))
           (loophole-defining-kmacro-map-flag t)
           (loophole-defining-kmacro-map-tag
            (concat "kmacro[End: \\[loophole-end-kmacro], "
                    "Abort: \\[loophole-abort-kmacro]]"))
           (loophole-alternative-read-key-function
            #'loophole-read-key-with-time-limit)
           (loophole-bind-entry-order
            '(loophole-obtain-object))
           (loophole-bind-command-order
            '(loophole-obtain-command-by-read-command
              loophole-obtain-command-by-key-sequence
              loophole-obtain-command-by-lambda-form
              loophole-obtain-object))
           (loophole-bind-kmacro-order
            '(loophole-obtain-kmacro-by-recursive-edit
              loophole-obtain-kmacro-by-read-key
              loophole-obtain-kmacro-by-recall-record
              loophole-obtain-object))
           (loophole-bind-array-order
            '(loophole-obtain-array-by-read-key
              loophole-obtain-array-by-read-string
              loophole-obtain-object))
           (loophole-bind-keymap-order
            '(loophole-obtain-keymap-by-read-keymap-variable
              loophole-obtain-keymap-by-read-keymap-function
              loophole-obtain-object))
           (loophole-bind-symbol-order
            '(loophole-obtain-symbol-by-read-keymap-function
              loophole-obtain-symbol-by-read-command
              loophole-obtain-symbol-by-read-array-function
              loophole-obtain-object))
           (loophole-set-key-order
            '(loophole-obtain-command-by-read-command
              loophole-obtain-kmacro-by-recursive-edit
              loophole-obtain-command-by-key-sequence
              loophole-obtain-kmacro-by-read-key
              loophole-obtain-command-by-lambda-form
              loophole-obtain-kmacro-by-recall-record
              loophole-obtain-object))
           (loophole-after-register-functions nil)
           (loophole-after-unregister-functions nil)
           (loophole-after-prioritize-functions nil)
           (loophole-after-globalize-functions nil)
           (loophole-after-localize-functions nil)
           (loophole-after-enable-functions nil)
           (loophole-after-disable-functions nil)
           (loophole-after-start-editing-functions nil)
           (loophole-after-stop-editing-functions nil)
           (loophole-after-globalize-editing-functions nil)
           (loophole-after-localize-editing-functions nil)
           (loophole-after-name-functions nil)
           (loophole-after-merge-functions nil)
           (loophole-after-duplicate-functions nil)
           (loophole-read-buffer-set-up-hook nil)
           (loophole-bind-hook nil)
           (loophole-default-storage-file
            (concat user-emacs-directory "loophole-maps"))
           (loophole-make-load-overwrite-map 'temporary)
           (loophole-print-event-in-char-read-syntax t)
           (loophole-tag-sign "#")
           (loophole-mode-lighter-base " L")
           (loophole-mode-lighter-editing-sign "+")
           (loophole-mode-lighter-suspending-sign "-")
           (loophole-mode-lighter-preset-alist
            '((tag . (""
                      loophole-mode-lighter-base
                      (:eval (if (and (loophole-suspending-p)
                                      (not loophole-mode-lighter-use-face))
                                 loophole-mode-lighter-suspending-sign))
                      (:eval (if (loophole-editing)
                                 (concat
                                  loophole-mode-lighter-editing-sign
                                  (let ((tag (get (loophole-editing)
                                                  :loophole-tag)))
                                    (if (and loophole-mode-lighter-use-face
                                             (stringp tag))
                                        (propertize (replace-regexp-in-string
                                                     "%" "%%"
                                                     (substitute-command-keys
                                                      tag))
                                                    'face 'loophole-editing)
                                      tag)))))
                      (:eval
                       (let ((l (seq-filter #'symbol-value
                                            (loophole-state-variable-list))))
                         (if (zerop (length l))
                             ""
                           (concat
                            loophole-tag-sign
                            (mapconcat
                             (lambda (state-variable)
                               (let ((e (replace-regexp-in-string
                                         "%" "%%"
                                         (substitute-command-keys
                                          (let ((tag (get
                                                      (get
                                                       state-variable
                                                       :loophole-map-variable)
                                                      :loophole-tag)))
                                            (if (stringp tag)
                                                tag
                                              ""))))))
                                 (if (and loophole-mode-lighter-use-face
                                          (stringp e))
                                     (propertize e
                                                 'face
                                                 (if (loophole-suspending-p)
                                                     'loophole-suspending
                                                   'loophole-using))
                                   e)))
                             l ",")))))))
              (number . (""
                         loophole-mode-lighter-base
                         (:eval (if (and (loophole-suspending-p)
                                         (not loophole-mode-lighter-use-face))
                                    loophole-mode-lighter-suspending-sign))
                         (:eval (if (loophole-editing)
                                    loophole-mode-lighter-editing-sign))
                         (:eval (let ((n (length
                                          (seq-filter
                                           #'symbol-value
                                           (loophole-state-variable-list)))))
                                  (if (zerop n)
                                      ""
                                    (concat
                                     ":"
                                     (let ((s (int-to-string n)))
                                       (if loophole-mode-lighter-use-face
                                           (propertize
                                            s 'face
                                            (if (loophole-suspending-p)
                                                'loophole-suspending
                                              'loophole-using))
                                         s))))))))
              (simple . (""
                         loophole-mode-lighter-base
                         (:eval (if (loophole-suspending-p)
                                    loophole-mode-lighter-suspending-sign))
                         (:eval (if (loophole-editing)
                                    loophole-mode-lighter-editing-sign))))
              (static . loophole-mode-lighter-base)))
           (loophole-mode-lighter-use-face nil)
           (loophole-auto-start-editing-for-existing-binding-advice nil)
           (loophole-idle-prioritize-timer nil)
           (loophole-idle-save-timer nil)
           (loophole-use-auto-prioritize nil)
           (loophole-use-auto-stop-editing nil)
           (loophole-use-auto-resume nil)
           (loophole-use-auto-timer nil)
           (loophole-use-auto-editing-timer nil)
           (loophole-use-auto-start-editing-for-existing-binding nil)
           (loophole-use-idle-prioritize nil)
           (loophole-use-idle-save nil)
           (loophole--define-map-font-lock-regexp
            (concat "(\\(loophole-define-map\\)\\_>"
                    "[ 	]*\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)?"))
           (loophole-font-lock 'multiline))
       (let ((loophole-mode-lighter
              (cdr (assq 'tag loophole-mode-lighter-preset-alist)))
             (emulation-mode-map-alists '(loophole--map-alist))
             (,temp-obarray (obarray-make))
             (,mode-state loophole-mode)
             (,local-variable-if-set-list
              `((loophole--map-alist . nil)
                (loophole--editing . t)
                (loophole--timer-alist . nil)
                (loophole--editing-timer . nil)
                ,@(seq-filter #'local-variable-if-set-p
                              (mapcar #'car loophole--map-alist))))
             ,local-variable-alist)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (let (list)
               (dolist (cons-or-variable ,local-variable-if-set-list)
                 (let ((variable (if (consp cons-or-variable)
                                     (car cons-or-variable)
                                   cons-or-variable)))
                   (if (local-variable-p variable)
                       (push `(,variable . ,(symbol-value variable)) list))))
               (if list
                   (push `(,buffer . ,list) ,local-variable-alist)))))
         (push `(nil . ,(mapcar (lambda (cons-or-variable)
                                  (let ((variable (if (consp cons-or-variable)
                                                      (car cons-or-variable)
                                                    cons-or-variable)))
                                    `(,variable . ,(default-value variable))))
                                ,local-variable-if-set-list))
               ,local-variable-alist)
         (with-temp-buffer
           (let ((deflect-to-temp-obarray
                   (lambda (args)
                     (if (cadr args)
                         args
                       (let ((name (car args)))
                         (if (or (member
                                  name
                                  (mapcar
                                   #'symbol-name
                                   loophole--test-barriered-symbol-list))
                                 (string-prefix-p "loophole" name))
                             (list name ,temp-obarray)
                           args))))))
             (unwind-protect
                 (progn
                   (dolist (cons-or-variable ,local-variable-if-set-list)
                     (let ((variable (if (consp cons-or-variable)
                                         (car cons-or-variable)
                                       cons-or-variable)))
                       (remove-variable-watcher
                        variable #'loophole--follow-adding-local-variable)))
                   (dolist (buffer-binding-list (cdr ,local-variable-alist))
                     (with-current-buffer (car buffer-binding-list)
                       (remove-hook 'kill-buffer-hook
                                    #'loophole--follow-killing-local-variable
                                    t)
                       (remove-hook 'change-major-mode-hook
                                    #'loophole--follow-killing-local-variable
                                    t)
                       (dolist (binding (cdr buffer-binding-list))
                         (kill-local-variable (car binding)))))
                   (dolist (cons-or-variable ,local-variable-if-set-list)
                     (if (consp cons-or-variable)
                         (set-default (car cons-or-variable)
                                      (cdr cons-or-variable))))
                   (advice-add 'intern :filter-args deflect-to-temp-obarray)
                   (advice-add 'unintern :filter-args deflect-to-temp-obarray)
                   ,@body)
               (advice-remove 'unintern deflect-to-temp-obarray)
               (advice-remove 'intern deflect-to-temp-obarray)
               (dolist (buffer (buffer-list))
                 (with-current-buffer buffer
                   (dolist (cons-or-variable ,local-variable-if-set-list)
                     (let ((local-variable-if-set
                            (if (consp cons-or-variable)
                                (car cons-or-variable)
                              cons-or-variable)))
                       (if (local-variable-p local-variable-if-set)
                           (kill-local-variable local-variable-if-set))))))
               (dolist (binding (cdar ,local-variable-alist))
                 (set-default (car binding) (cdr binding)))
               (dolist (buffer-binding-list (cdr ,local-variable-alist))
                 (with-current-buffer (car buffer-binding-list)
                   (dolist (binding (cdr buffer-binding-list))
                     (set (car binding) (cdr binding)))
                   (when ,mode-state
                     (add-hook 'change-major-mode-hook
                               #'loophole--follow-killing-local-variable
                               nil t)
                     (add-hook 'kill-buffer-hook
                               #'loophole--follow-killing-local-variable
                               nil t))))
               (if ,mode-state
                   (dolist (cons-or-variable ,local-variable-if-set-list)
                     (let ((variable (if (consp cons-or-variable)
                                         (car cons-or-variable)
                                       cons-or-variable)))
                       (add-variable-watcher
                        variable
                        #'loophole--follow-adding-local-variable)))))))))))

(defmacro loophole--test-with-keyboard-events (keyboard-events &rest body)
  "Evaluate BODY followed by KEYBOARD-EVENTS.
KEYBOARD-EVENTS should be a string, vector representing
keyboard events, or a list of them .

In this macro,  KEYBOARD-EVENTS are bound to
`overriding-terminal-local-map' transiently.  Consequently,
KEYBOARD-EVENTS are ensured to be complete key sequence.
Bound entry is key binding currently valid or undefined
if specified events are not bound to valid command.
However, if any form in BODY invokes minibuffer, transient
key bindings are disable.  After the form exit minibuffer,
transient binding are re-enabled.
If keyboard-events is a list, its elements are bound to
`overriding-terminal-local-map' individually.

If KEYBOARD-EVENTS is something invalid and test is not
finished even after `loophole--test-wait-time' is spent,
`loophole-test-error' is signaled.

When Emacs runs noninteractively, i.e. in batch mode,
`read-from-minibuffer' usually reads standard input for
Emacs.  In this macro, when run noninteractively,
`read-from-minibuffer' is adviced to return a string that is
a portion of KEYBOARD-EVENTS delimited by newline.
By this advice, standard input is emulated with
KEYBOARD-EVENTS, and even while batch mode, tests can be run
 with emulated KEYBOARD-EVENTS.
Note that, adviced `read-from-minibuffer' returns a portion
of KEYBOARD-EVENTS as it is, test should not rely of the
completing features of reading minibuffer functions like
`completing-read'."
  (declare (debug t) (indent 1))
  (let ((exit-function (make-symbol "exit-function"))
        (enter-transient-map (make-symbol "enter-transient-map"))
        (exit-transient-map (make-symbol "exit-transient-map"))
        (return-events (make-symbol "return-events"))
        (yes-or-no-p (make-symbol "yes-or-no-p"))
        (y-or-n-p (make-symbol "y-or-n-p"))
        (flatten-events (make-symbol "flatten-events"))
        (pseudo-standard-input (make-symbol "flatten-events"))
        (timer (make-symbol "timer")))
    `(let* ((,flatten-events (if (listp ,keyboard-events)
                                 (apply #'vconcat ,keyboard-events)
                               ,keyboard-events))
            (unread-command-events (listify-key-sequence ,flatten-events))
            (,pseudo-standard-input ,flatten-events)
            (,exit-function #'ignore))
       (let  ((,enter-transient-map
               (lambda ()
                 (setq ,exit-function
                       (set-transient-map
                        (let ((map (make-sparse-keymap)))
                          (if (listp ,keyboard-events)
                              (dolist (key ,keyboard-events)
                                (define-key map key
                                  (let ((entry (key-binding key)))
                                    (if (commandp entry)
                                        entry
                                      #'undefined))))
                            (define-key map ,keyboard-events
                              (let ((entry (key-binding ,keyboard-events)))
                                (if (commandp entry)
                                    entry
                                  #'undefined))))
                          map)
                        t))))
              (,exit-transient-map
               (lambda ()
                 (funcall ,exit-function)
                 (setq ,exit-function #'ignore)))
              (,return-events
               (lambda (&rest _)
                 (let* ((newline-position (seq-position
                                            ,pseudo-standard-input ?\r))
                        (head (seq-take
                               ,pseudo-standard-input newline-position))
                        (tail (seq-drop
                               ,pseudo-standard-input (1+ newline-position))))
                   (unless (seq-every-p #'characterp head)
                     (signal 'loophole-test-error
                             (list (concat "Non-character events for minibuffer"
                                           " in batch mode is not allowed"))))
                   (setq ,pseudo-standard-input tail)
                   (concat head))))
              (,yes-or-no-p
               (lambda (&rest _)
                 (cond ((equal (seq-take (vconcat ,pseudo-standard-input) 4)
                               (vector ?y ?e ?s ?\r))
                        (setq ,pseudo-standard-input
                              (seq-drop ,pseudo-standard-input 4))
                        t)
                       ((equal (seq-take (vconcat ,pseudo-standard-input) 3)
                               (vector ?n ?o ?\r))
                        (setq ,pseudo-standard-input
                              (seq-drop ,pseudo-standard-input 3))
                        nil)
                       (t (signal 'loophole-test-error
                                  (list (concat "Only yes or no are allowed"
                                                " for yes-or-no-p"
                                                " in batch mode")))))))
              (,y-or-n-p
               (lambda (&rest _)
                 (cond ((equal (seq-take (vconcat ,pseudo-standard-input) 1)
                               (vector ?y))
                        (setq ,pseudo-standard-input
                              (seq-drop ,pseudo-standard-input 1))
                        t)
                       ((equal (seq-take (vconcat ,pseudo-standard-input) 1)
                               (vector ?n))
                        (setq ,pseudo-standard-input
                              (seq-drop ,pseudo-standard-input 1))
                        nil)
                       (t (signal 'loophole-test-error
                                  (list (concat "Only y or n are allowed"
                                                " for y-or-n-p"
                                                " in batch mode")))))))
              (,timer nil))
         (unwind-protect
             (progn
               (funcall ,enter-transient-map)
               (add-hook 'minibuffer-setup-hook ,exit-transient-map)
               (add-hook 'minibuffer-exit-hook ,enter-transient-map)
               (when noninteractive
                 (advice-add 'read-from-minibuffer :override ,return-events)
                 (advice-add 'read-string :override ,return-events)
                 (advice-add 'yes-or-no-p :override ,yes-or-no-p)
                 (advice-add 'y-or-n-p :override ,y-or-n-p))
               (setq ,timer
                     (run-with-timer
                      loophole--test-wait-time nil
                      (lambda ()
                        (signal
                         'loophole-test-error
                         (list "Test with keyboard events is timed out")))))
               ,@body)
           (if (timerp ,timer) (cancel-timer ,timer))
           (when noninteractive
             (advice-remove 'y-or-n-p ,y-or-n-p)
             (advice-remove 'yes-or-no-p ,yes-or-no-p)
             (advice-remove 'read-string ,return-events)
             (advice-remove 'read-from-minibuffer ,return-events))
           (funcall ,exit-function)
           (remove-hook 'minibuffer-exit-hook ,enter-transient-map)
           (remove-hook 'minibuffer-setup-hook ,exit-transient-map))))))

(defun loophole--test-set-pseudo-map-alist ()
  "Set pseudo `loophole--map-alist' value.
This function must be used in
`loophole--test-with-pseudo-environment'."
  (setq loophole--map-alist nil)
  (setq-default loophole--map-alist nil)
  (dolist (name-tag '(("loophole-1-map" . "1")
                      ("loophole-2-map" . "2")
                      ("loophole-test-a-map" . "a")
                      ("loophole-test-b-map" . "b")
                      ("loophole" . "loop")))
    (let* ((name (car name-tag))
           (tag (cdr name-tag))
           (map-variable (intern name))
           (state-variable (intern (concat name "-state"))))
      (set map-variable (make-sparse-keymap))
      (set state-variable t)
      (put map-variable :loophole-state-variable state-variable)
      (put state-variable :loophole-map-variable map-variable)
      (put map-variable :loophole-tag tag)
      (setq loophole--map-alist
            (cons `(,state-variable . ,(symbol-value map-variable))
                  loophole--map-alist))
      (setq-default loophole--map-alist
                    (cons `(,state-variable . ,(symbol-value map-variable))
                          (default-value 'loophole--map-alist)))))
  (make-variable-buffer-local (intern "loophole-2-map-state"))
  (make-variable-buffer-local (intern "loophole-test-b-map-state"))
  (make-variable-buffer-local (intern "loophole-state"))
  (setq loophole--map-alist (reverse loophole--map-alist))
  (setq-default loophole--map-alist (reverse
                                     (default-value 'loophole--map-alist))))

;; Auxiliary functions

(ert-deftest loophole-test-map-variable ()
  "Test for `loophole-map-variable'."
  (loophole--test-with-pseudo-environment
    (loophole--test-set-pseudo-map-alist)
    (should (eq (loophole-map-variable (intern "loophole-1-map-state"))
                (intern "loophole-1-map")))
    (should (eq (loophole-map-variable (intern "loophole-test-a-map-state"))
                (intern "loophole-test-a-map")))
    (should (eq (loophole-map-variable (intern "loophole-state"))
                (intern "loophole")))
    (should-error (loophole-map-variable (intern "loophole-2-map"))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole-map-variable (intern "loophole-test-b-map"))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole-map-variable (intern "loophole"))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole-map-variable nil) :type 'error :exclude-subtypes t)
    (should-error (loophole-map-variable 0) :type 'wrong-type-argument)
    (should-error (loophole-map-variable 1.0) :type 'wrong-type-argument)
    (should-error (loophole-map-variable ?c) :type 'wrong-type-argument)
    (should-error (loophole-map-variable (intern "s"))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole-map-variable (cons 0 0))
                  :type 'wrong-type-argument)
    (should-error (loophole-map-variable (string ?s))
                  :type 'wrong-type-argument)
    (should-error (loophole-map-variable (vector ?v))
                  :type 'wrong-type-argument)))

(ert-deftest loophole-test-state-variable ()
  "Test for `loophole-state-variable'."
  (loophole--test-with-pseudo-environment
    (loophole--test-set-pseudo-map-alist)
    (should (eq (loophole-state-variable (intern "loophole-1-map"))
                (intern "loophole-1-map-state")))
    (should (eq (loophole-state-variable (intern "loophole-test-a-map"))
                (intern "loophole-test-a-map-state")))
    (should (eq (loophole-state-variable (intern "loophole"))
                (intern "loophole-state")))
    (should-error (loophole-state-variable (intern "loophole-2-map-state"))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole-state-variable (intern "loophole-test-b-map-state"))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole-state-variable (intern "loophole-state"))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole-state-variable nil)
                  :type 'error :exclude-subtypes t)
    (should-error (loophole-state-variable 0) :type 'wrong-type-argument)
    (should-error (loophole-state-variable 1.0) :type 'wrong-type-argument)
    (should-error (loophole-state-variable ?c) :type 'wrong-type-argument)
    (should-error (loophole-state-variable (intern "s"))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole-state-variable (cons 0 0))
                  :type 'wrong-type-argument)
    (should-error (loophole-state-variable (string ?s))
                  :type 'wrong-type-argument)
    (should-error (loophole-state-variable (vector ?v))
                  :type 'wrong-type-argument)))

(ert-deftest loophole-test-map-variable-list ()
  "Test for `loophole-map-variable-list'."
  (let ((meet-requirement
         (lambda ()
           (let ((return (loophole-map-variable-list)))
             (or (null return)
                 (and (consp return)
                      (seq-every-p #'loophole-registered-p return)
                      (equal (mapcar #'loophole-state-variable return)
                             (mapcar #'car loophole--map-alist))))))))
    (loophole--test-with-pseudo-environment
      (should (funcall meet-requirement))
      (loophole--test-set-pseudo-map-alist)
      (should (funcall meet-requirement)))))

(ert-deftest loophole-test-state-variable-list ()
  "Test for `loophole-state-variable-list'."
  (let ((meet-requirement
         (lambda ()
           (let ((return (loophole-state-variable-list)))
             (or (null return)
                 (and (consp return)
                      (seq-every-p (lambda (state-variable)
                                     (loophole-registered-p
                                      (loophole-map-variable state-variable)
                                      state-variable))
                                   return)
                      (equal return
                             (mapcar #'car loophole--map-alist))))))))
    (loophole--test-with-pseudo-environment
      (should (funcall meet-requirement))
      (loophole--test-set-pseudo-map-alist)
      (should (funcall meet-requirement)))))

(ert-deftest loophole-test-key-equal ()
  "Test for `loophole-key-equal'."
  (should (loophole-key-equal (string ?a) (vector ?a)))
  (should-not (loophole-key-equal (string ?A) (vector ?\S-a)))
  (should (loophole-key-equal (string ?\C-a) (vector ?\C-a)))
  (should (loophole-key-equal (string ?\e ?a) (vector ?\e ?a)))
  (should (loophole-key-equal (string ?\e ?a) (string (+ ?a (ash 2 6)))))
  (should (loophole-key-equal (string ?\e ?a) (vector ?\M-a)))
  (should (loophole-key-equal (string ?\e ?/) (vector ?\e ?/)))
  (should (loophole-key-equal (string ?\e ?/) (vector ?\M-/)))
  (should (loophole-key-equal (kbd "DEL") (vector ?\^?)))
  (should (loophole-key-equal (string ?\e ?\C-a) (vector ?\e ?\C-a)))
  (should (loophole-key-equal (string ?\e ?\C-a) (vector ?\C-\M-a)))
  (should-error (loophole-key-equal 0 0) :type 'wrong-type-argument)
  (should-error (loophole-key-equal 1.0 1.0) :type 'wrong-type-argument)
  (should-error (loophole-key-equal ?c ?c) :type 'wrong-type-argument)
  (should-error (loophole-key-equal (intern "s") (intern "s"))
                :type 'wrong-type-argument)
  (should-error (loophole-key-equal (cons 0 0) (cons 0 0))
                :type 'wrong-type-argument))

(ert-deftest loophole-test-local-variable-if-set-list ()
  "Test for `loophole-local-variable-if-set-list'."
  (loophole--test-with-pseudo-environment
    (should (equal (loophole-local-variable-if-set-list)
                   (list 'loophole--map-alist
                         'loophole--editing
                         'loophole--timer-alist
                         'loophole--editing-timer)))
    (loophole--test-set-pseudo-map-alist)
    (should (equal (loophole-local-variable-if-set-list)
                   (list 'loophole--map-alist
                         'loophole--editing
                         'loophole--timer-alist
                         'loophole--editing-timer
                         (intern "loophole-2-map-state")
                         (intern "loophole-test-b-map-state")
                         (intern "loophole-state"))))))

(ert-deftest loophole-test-read-key ()
  "Test for `loophole-read-key'."
  (loophole--test-with-pseudo-environment
    (loophole--test-with-keyboard-events (vector ?a)
      (should (loophole-key-equal (loophole-read-key "") (vector ?a))))
    (loophole--test-with-keyboard-events (vector ?A)
      (should (loophole-key-equal (loophole-read-key "") (vector ?A))))
    (loophole--test-with-keyboard-events (vector ?\C-a)
      (should (loophole-key-equal (loophole-read-key "") (vector ?\C-a))))
    (loophole--test-with-keyboard-events (vector ?\M-a)
      (should (loophole-key-equal (loophole-read-key "") (vector ?\M-a))))
    (loophole--test-with-keyboard-events (vector ?\C-x ?\C-f)
      (should (loophole-key-equal (loophole-read-key "") (vector ?\C-x ?\C-f))))
    (loophole--test-with-keyboard-events (vector ?\C-\M-f)
      (should (loophole-key-equal (loophole-read-key "") (vector ?\C-\M-f))))
    (loophole--test-with-keyboard-events (vector ?\C-\S-f)
      (should (loophole-key-equal (loophole-read-key "") (vector ?\C-\S-f))))
    (loophole--test-with-keyboard-events (vector ?\M-\S-f)
      (should (loophole-key-equal (loophole-read-key "") (vector ?\M-\S-f))))
    (loophole--test-with-keyboard-events (vector ?\H-a)
      (should (loophole-key-equal (loophole-read-key "") (vector ?\H-a))))
    (loophole--test-with-keyboard-events (vector ?\s-a)
      (should (loophole-key-equal (loophole-read-key "") (vector ?\s-a))))
    (loophole--test-with-keyboard-events (vector ?\S-a)
      (should (loophole-key-equal (loophole-read-key "") (vector ?\S-a))))
    (loophole--test-with-keyboard-events (vector ?\A-a)
      (should (loophole-key-equal (loophole-read-key "") (vector ?\A-a))))
    (loophole--test-with-keyboard-events (vector ?\C-\M-\S-\H-\s-\A-a)
      (should (loophole-key-equal (loophole-read-key "")
                                  (vector ?\C-\M-\S-\H-\s-\A-a))))
    (let* ((quit-binding-key (where-is-internal 'keyboard-quit nil t))
           (quit-key (or quit-binding-key [?\C-g])))
      (loophole--test-with-keyboard-events quit-key
        (unless quit-binding-key
          (define-key overriding-terminal-local-map quit-key #'keyboard-quit))
        (should (condition-case nil
                    (progn (loophole-read-key "") nil)
                  (quit t))))
      (loophole--test-with-keyboard-events quit-key
        (unless quit-binding-key
          (define-key overriding-terminal-local-map quit-key #'keyboard-quit))
        (should (loophole-key-equal (let ((loophole-allow-keyboard-quit nil))
                                      (loophole-read-key ""))
                                    quit-key))))))

(ert-deftest loophole-test-read-key-with-time-limit ()
  "Test for `loophole-read-key-with-time-limit'."
  (loophole--test-with-pseudo-environment
    (let* ((loophole-read-key-limit-time (* 0.2 loophole--test-wait-time)))
      (should (catch 'loophole-test-read-key-with-time-limit
                (run-with-idle-timer
                 (* loophole-read-key-limit-time 2.0)
                 nil
                 (lambda () (throw 'loophole-test-read-key-with-time-limit t)))
                (loophole-read-key-with-time-limit "")
                nil))
      (loophole--test-with-keyboard-events (vector ?a)
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?a))))
      (loophole--test-with-keyboard-events (vector ?A)
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?A))))
      (loophole--test-with-keyboard-events (vector ?\C-a)
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?\C-a))))
      (loophole--test-with-keyboard-events (vector ?\M-a)
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?\M-a))))
      (loophole--test-with-keyboard-events (vector ?\M-g ?\M-g)
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?\M-g ?\M-g))))
      ;; Cannot use C-x or ESC for this test because they are defined in
      ;; key-translate-table, function-key-map, and input-decode-map.
      ;; See comment in `read-key' source for details.
      (loophole--test-with-keyboard-events (vector ?\C-\M-f)
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?\C-\M-f))))
      (loophole--test-with-keyboard-events (vector ?\C-\S-f)
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?\C-\S-f))))
      (loophole--test-with-keyboard-events (vector ?\M-\S-f)
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?\M-\S-f))))
      (loophole--test-with-keyboard-events (vector ?\H-a)
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?\H-a))))
      (loophole--test-with-keyboard-events (vector ?\s-a)
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?\s-a))))
      (loophole--test-with-keyboard-events (vector ?\S-a)
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?\S-a))))
      (loophole--test-with-keyboard-events (vector ?\A-a)
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?\A-a))))
      (loophole--test-with-keyboard-events (vector ?\C-\M-\S-\H-\s-\A-a)
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?\C-\M-\S-\H-\s-\A-a))))
      (loophole--test-with-keyboard-events (list (vector ?a)
                                                 (vector ?b)
                                                 (vector ?c))
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?a ?b ?c))))
      (loophole--test-with-keyboard-events (list (vector ?\C-a)
                                                 (vector ?\C-b)
                                                 (vector ?\C-c))
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?\C-a ?\C-b ?\C-c))))
      (loophole--test-with-keyboard-events (list (vector ?\M-a)
                                                 (vector ?\M-b)
                                                 (vector ?\M-c))
        (should (loophole-key-equal (loophole-read-key-with-time-limit "")
                                    (vector ?\M-a ?\M-b ?\M-c))))
      (let* ((quit-binding-key (where-is-internal 'keyboard-quit nil t))
             (quit-key (or quit-binding-key [?\C-g])))
        (loophole--test-with-keyboard-events quit-key
          (unless quit-binding-key
            (define-key overriding-terminal-local-map quit-key #'keyboard-quit))
          (should (condition-case nil
                      (progn (loophole-read-key-with-time-limit "") nil)
                    (quit t))))
        (loophole--test-with-keyboard-events quit-key
          (unless quit-binding-key
            (define-key overriding-terminal-local-map quit-key #'keyboard-quit))
          (should (loophole-key-equal (let ((loophole-allow-keyboard-quit nil))
                                        (loophole-read-key-with-time-limit ""))
                                      quit-key)))))))

(ert-deftest loophole-test-read-key-until-termination-key ()
  "Test for `loophole-read-key-until-termination-key'."
  (loophole--test-with-pseudo-environment
    (loophole--test-with-keyboard-events loophole-read-key-termination-key
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector))))
    (loophole--test-with-keyboard-events
        (list (vector ?a) loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?a))))
    (loophole--test-with-keyboard-events
        (list (vector ?A) loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?A))))
    (loophole--test-with-keyboard-events
        (list (vector ?\C-a) loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?\C-a))))
    (loophole--test-with-keyboard-events
        (list (vector ?\M-a) loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?\M-a))))
    ;; Cannot use C-x or ESC for this test because they are defined in
    ;; key-translate-table, function-key-map, and input-decode-map.
    ;; See comment in `read-key' source for details.
    (loophole--test-with-keyboard-events
        (list (vector ?\M-g ?\M-g) loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?\M-g ?\M-g))))
    (loophole--test-with-keyboard-events
        (list (vector ?\C-\M-f) loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?\C-\M-f))))
    (loophole--test-with-keyboard-events
        (list (vector ?\C-\S-f) loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?\C-\S-f))))
    (loophole--test-with-keyboard-events
        (list (vector ?\M-\S-f) loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?\M-\S-f))))
    (loophole--test-with-keyboard-events
        (list (vector ?\H-a) loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?\H-a))))
    (loophole--test-with-keyboard-events
        (list (vector ?\s-a) loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?\s-a))))
    (loophole--test-with-keyboard-events
        (list (vector ?\S-a) loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?\S-a))))
    ;; Cannot use A-a for this test because it is defined in key-translation-map
    ;; when batch mode, more specifically when no window system.
    ;; Same situation with C-x and ESC
    (loophole--test-with-keyboard-events
        (list (vector ?\A-b) loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?\A-b))))
    (loophole--test-with-keyboard-events
        (list (vector ?\C-\M-\S-\H-\s-\A-a) loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?\C-\M-\S-\H-\s-\A-a))))
    (loophole--test-with-keyboard-events
        (list (vector ?a)
              (vector ?b)
              (vector ?c)
              loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?a ?b ?c))))
    (loophole--test-with-keyboard-events
        (list (vector ?\C-a)
              (vector ?\C-b)
              (vector ?\C-c)
              loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?\C-a ?\C-b ?\C-c))))
    (loophole--test-with-keyboard-events
        (list (vector ?\M-a)
              (vector ?\M-b)
              (vector ?\M-c)
              loophole-read-key-termination-key)
      (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                  (vector ?\M-a ?\M-b ?\M-c))))
    (loophole--test-with-keyboard-events nil
      (let ((loophole-read-key-termination-key (vector))
            (loophole-allow-keyboard-quit nil))
        (should-error (loophole-read-key-until-termination-key "")
                      :type 'user-error)))
    (let* ((quit-binding-key (where-is-internal 'keyboard-quit nil t))
           (quit-key (or quit-binding-key [?\C-g])))
      (loophole--test-with-keyboard-events quit-key
        (unless quit-binding-key
          (define-key overriding-terminal-local-map quit-key #'keyboard-quit))
        (should (loophole-key-equal (loophole-read-key-until-termination-key "")
                                    (vector))))
      (loophole--test-with-keyboard-events
          (list quit-key loophole-read-key-termination-key)
        (unless quit-binding-key
          (define-key overriding-terminal-local-map quit-key #'keyboard-quit))
        (should (loophole-key-equal
                 (let ((loophole-allow-keyboard-quit nil))
                   (loophole-read-key-until-termination-key ""))
                 (vector))))
      (let ((loophole-read-key-termination-key (vector ?\C-\])))
        (loophole--test-with-keyboard-events quit-key
          (unless quit-binding-key
            (define-key overriding-terminal-local-map quit-key #'keyboard-quit))
          (should (condition-case nil
                      (progn (loophole-read-key-until-termination-key "") nil)
                    (quit t))))
        (loophole--test-with-keyboard-events
            (list quit-key loophole-read-key-termination-key)
          (unless quit-binding-key
            (define-key overriding-terminal-local-map quit-key #'keyboard-quit))
          (should (loophole-key-equal
                   (let ((loophole-allow-keyboard-quit nil))
                     (loophole-read-key-until-termination-key ""))
                   quit-key)))))))

(ert-deftest loophole-test-read-map-variable ()
  "Test for `loophole-read-map-variable'.
This test has assertions for interactive behaviors of
`loophole-read-map-variable'.  When this test is run in
batch-mode, these assertions are skipped."
  :tags '(interactive)
  (loophole--test-with-pseudo-environment
    (should-error (loophole-read-map-variable "")
                  :type 'user-error)
    (loophole--test-set-pseudo-map-alist)
    (loophole--test-with-keyboard-events "loophole-1-map"
      (should (eq (loophole-read-map-variable "")
                  (intern "loophole-1-map"))))
    (should-error (loophole-read-map-variable "" (lambda (_) nil))
                  :type 'user-error)
    (unless noninteractive
      (let* ((monitored-args nil)
             (monitor-args (lambda (&rest args) (setq monitored-args args))))
        (unwind-protect
            (let ((help-char-works
                   (lambda (help-char-count &optional filter-for-map-variable)
                     (loophole--test-with-keyboard-events
                         (vconcat (make-vector help-char-count help-char)
                                  (vector ?\C-m))
                       (loophole-read-map-variable "" filter-for-map-variable))
                     (let ((map-variable-list
                            (if filter-for-map-variable
                                (seq-filter filter-for-map-variable
                                            (loophole-map-variable-list))
                              (loophole-map-variable-list))))
                       (prog1 (and monitored-args
                                   (eq (car monitored-args)
                                       (nth (% (1- help-char-count)
                                               (length map-variable-list))
                                            map-variable-list)))
                         (setq monitored-args nil))))))
              (advice-add 'loophole-describe :override monitor-args)
              (let ((size (length (loophole-map-variable-list))))
                (should (funcall help-char-works 1))
                (should (funcall help-char-works (/ size 2)))
                (should (funcall help-char-works (1+ size)))
                (should (funcall help-char-works (* size 2))))
              (let* ((filter (lambda (symbol)
                               (string-match "[0-9]" (symbol-name symbol))))
                     (size (length
                            (seq-filter filter (loophole-map-variable-list)))))
                (should (funcall help-char-works 1 filter))
                (should (funcall help-char-works (/ size 2) filter))
                (should (funcall help-char-works (1+ size) filter))
                (should (funcall help-char-works (* size 2) filter))))
          (advice-remove 'loophole-describe monitor-args))))))

(ert-deftest loophole-test-read-buffer ()
  "Test for `loophole-read-buffer'.
This test has assertions for interactive behaviors of
`loophole-read-buffer'.  When this test is run in
batch-mode, these assertions are skipped."
  :tags '(interactive)
  (loophole--test-with-pseudo-environment
    (with-temp-buffer
      (let ((temp-buffer (current-buffer)))
        (with-temp-buffer
          ;; Test return value
          (with-current-buffer temp-buffer (insert ?0))
          (should (= (loophole--test-with-keyboard-events
                         loophole-read-buffer-finish-key
                       (define-key overriding-terminal-local-map
                         loophole-read-buffer-finish-key nil)
                       (loophole-read-buffer #'ignore temp-buffer))
                     0))
          (with-current-buffer temp-buffer (erase-buffer))
          (with-current-buffer temp-buffer (insert (format "%d %d %d" 1 2 3)))
          (should (= (loophole--test-with-keyboard-events
                         loophole-read-buffer-finish-key
                       (define-key overriding-terminal-local-map
                         loophole-read-buffer-finish-key nil)
                       (loophole-read-buffer #'ignore temp-buffer))
                     1))
          (with-current-buffer temp-buffer (erase-buffer))
          (should (= (loophole--test-with-keyboard-events
                         (list (vector ?4) loophole-read-buffer-finish-key)
                       (define-key overriding-terminal-local-map
                         loophole-read-buffer-finish-key nil)
                       (loophole-read-buffer #'ignore temp-buffer))
                     4))
          (with-current-buffer temp-buffer (erase-buffer))
          (should (condition-case nil
                      (progn (loophole--test-with-keyboard-events
                                 loophole-read-buffer-abort-key
                               (define-key overriding-terminal-local-map
                                 loophole-read-buffer-abort-key nil)
                               (loophole-read-buffer #'ignore temp-buffer))
                             nil)
                    (quit t)))
          ;; Test CALLBACK
          (let ((loophole-read-buffer-inhibit-recursive-edit t))
            (with-current-buffer temp-buffer (insert ?0))
            (should (condition-case nil
                        (progn
                          (loophole-read-buffer
                           (lambda (read)
                             (throw 'loophole-test-read-buffer (+ read 5)))
                           temp-buffer)
                          nil)
                      (quit t)))
            (should (= (catch 'loophole-test-read-buffer
                         (loophole--test-with-keyboard-events
                             loophole-read-buffer-finish-key
                           (define-key overriding-terminal-local-map
                             loophole-read-buffer-finish-key nil)
                           (with-current-buffer temp-buffer
                             (recursive-edit))))
                       5))
            (with-current-buffer temp-buffer (erase-buffer))
            (should (condition-case nil
                        (progn (loophole-read-buffer
                                (lambda ()
                                  (interactive)
                                  (throw 'loophole-test-read-buffer nil))
                                temp-buffer)
                               nil)
                      (quit t)))
            (should (let ((quit (vector ?q)))
                      (loophole--test-with-keyboard-events
                          (list loophole-read-buffer-abort-key quit)
                        (define-key overriding-terminal-local-map
                          loophole-read-buffer-abort-key
                          (lambda ()
                            (interactive)
                            (call-interactively
                             (lookup-key (current-local-map)
                                         loophole-read-buffer-abort-key))
                            (set-transient-map
                             (let ((map (make-sparse-keymap)))
                               (define-key map quit #'exit-recursive-edit)
                               map))))
                        (with-current-buffer temp-buffer
                          (recursive-edit)))
                      t)))
          ;; Test the case BUFFER-OR-NAME is a buffer name string
          (with-current-buffer temp-buffer (insert ?6))
          (should (= (loophole--test-with-keyboard-events
                         loophole-read-buffer-finish-key
                       (define-key overriding-terminal-local-map
                         loophole-read-buffer-finish-key nil)
                       (loophole-read-buffer #'ignore
                                             (buffer-name temp-buffer)))
                     6))
          (with-current-buffer temp-buffer (erase-buffer))
          ;; Test the case BUFFER-OR-NAME is a dead buffer
          (with-temp-buffer
            (insert ?7)
            (let ((dead-buffer))
              (with-temp-buffer (setq dead-buffer (current-buffer)))
              (should (= (loophole--test-with-keyboard-events
                             loophole-read-buffer-finish-key
                           (define-key overriding-terminal-local-map
                             loophole-read-buffer-finish-key nil)
                           (loophole-read-buffer #'ignore dead-buffer))
                         7))))
          ;; Test window setup and restoration
          (let* ((frame (selected-frame))
                 (window-configuration (current-window-configuration frame)))
            (let ((display-buffer-alist nil))
              ;; Test if window is set up and restored
              (with-current-buffer temp-buffer (insert ?8))
              (let ((throw-key (vector ?t))
                    (throw-context
                     (lambda ()
                       (interactive)
                       (throw 'loophole-test-read-buffer
                              (list (current-buffer)
                                    (selected-frame)
                                    (current-window-configuration))))))
                (let ((context-during-writing
                       (catch 'loophole-test-read-buffer
                         (loophole--test-with-keyboard-events throw-key
                           (define-key overriding-terminal-local-map
                             throw-key throw-context)
                           (loophole-read-buffer #'ignore temp-buffer)))))
                  (should (eq temp-buffer (car context-during-writing)))
                  (should (eq frame (cadr context-during-writing)))
                  (should-not (compare-window-configurations
                               window-configuration
                               (caddr context-during-writing))))
                (should (eq frame (selected-frame)))
                (should (compare-window-configurations
                         window-configuration (current-window-configuration))))
              (with-current-buffer temp-buffer (erase-buffer))
              ;; Test if the function works even when frame is changed
              (with-current-buffer temp-buffer (insert ?9))
              (unless noninteractive
                (let ((exit-key (vector ?q))
                      (change-key (vector ?f))
                      (change-frame
                       (lambda ()
                         (interactive)
                         (select-frame
                          (seq-find (lambda (f)
                                      (and (not (eq f frame))
                                           (not (eq f (selected-frame)))))
                                    (frame-list))
                          t))))
                  (should (= (loophole--test-with-keyboard-events
                                 (list change-key exit-key)
                               (define-key overriding-terminal-local-map
                                 change-key change-frame)
                               (define-key overriding-terminal-local-map
                                 exit-key #'exit-recursive-edit)
                               (loophole-read-buffer #'ignore temp-buffer))
                             9))
                  (should (eq frame (selected-frame)))
                  (should (compare-window-configurations
                           window-configuration
                           (current-window-configuration)))))
              (with-current-buffer temp-buffer (erase-buffer))
              ;; Test if the function works even when window is changed
              (with-current-buffer temp-buffer (insert ?1 ?0))
              (let ((exit-key (vector ?q))
                    (change-key (vector ?w))
                    (change-window
                     (lambda ()
                       (interactive)
                       (select-window
                        (seq-find (lambda (w)
                                    (not (eq w (selected-window))))
                                  (window-list))
                        t))))
                (should (= (loophole--test-with-keyboard-events
                               (list change-key exit-key)
                             (define-key overriding-terminal-local-map
                               change-key change-window)
                             (define-key overriding-terminal-local-map
                               exit-key #'exit-recursive-edit)
                             (loophole-read-buffer #'ignore temp-buffer))
                           10))
                (should (eq frame (selected-frame)))
                (should (compare-window-configurations
                         window-configuration
                         (current-window-configuration))))
              (with-current-buffer temp-buffer (erase-buffer))
              ;; Test if the function works even when window is deleted
              (with-current-buffer temp-buffer (insert ?1 ?1))
              (let ((exit-key (vector ?q))
                    (delete-key (vector ?w)))
                (should (= (loophole--test-with-keyboard-events
                               (list delete-key exit-key)
                             (define-key overriding-terminal-local-map
                               delete-key #'delete-window)
                             (define-key overriding-terminal-local-map
                               exit-key #'exit-recursive-edit)
                             (loophole-read-buffer #'ignore temp-buffer))
                           11))
                (should (eq frame (selected-frame)))
                (should (compare-window-configurations
                         window-configuration
                         (current-window-configuration))))
              (with-current-buffer temp-buffer (erase-buffer))
              ;; Test if window is set up and restored when
              ;; `loophole-read-buffer-inhibit-recursive-edit' is non-nil
              (with-current-buffer temp-buffer (insert ?1 ?2))
              (let ((loophole-read-buffer-inhibit-recursive-edit t))
                (condition-case nil
                  (loophole-read-buffer
                   (lambda (_)
                     (throw 'loophole-test-read-buffer
                            (list (current-buffer)
                                  (selected-frame)
                                  (current-window-configuration))))
                   temp-buffer)
                  (quit))
                (let ((context-during-writing
                     (catch 'loophole-test-read-buffer
                       (loophole--test-with-keyboard-events
                           loophole-read-buffer-finish-key
                         (define-key overriding-terminal-local-map
                           loophole-read-buffer-finish-key nil)
                         (with-current-buffer temp-buffer
                           (recursive-edit))))))
                (should (eq temp-buffer (car context-during-writing)))
                (should (eq frame (cadr context-during-writing)))
                (should-not (compare-window-configurations
                             window-configuration
                             (caddr context-during-writing))))
                (should (eq frame (selected-frame)))
                (should (compare-window-configurations
                         window-configuration (current-window-configuration))))
              (with-current-buffer temp-buffer (erase-buffer)))
            (unless noninteractive
              ;; Test if the function works when
              ;; `switch-to-buffer-other-window' makes frame
              (let ((display-buffer-alist '(("*" display-buffer-pop-up-frame))))
                ;; Test if the function works even when frame is changed
                (with-current-buffer temp-buffer (insert ?1 ?3))
                (let ((exit-key (vector ?q))
                      (change-key (vector ?f))
                      (change-frame
                       (lambda ()
                         (interactive)
                         (select-frame
                          (seq-find (lambda (f)
                                      (and (not (eq f frame))
                                           (not (eq f (selected-frame)))))
                                    (frame-list))
                          t))))
                  (should (= (loophole--test-with-keyboard-events
                              (list change-key exit-key)
                              (define-key overriding-terminal-local-map
                                change-key change-frame)
                              (define-key overriding-terminal-local-map
                                exit-key #'exit-recursive-edit)
                              (loophole-read-buffer #'ignore temp-buffer))
                             13))
                  (should (eq frame (selected-frame)))
                  (should (compare-window-configurations
                           window-configuration
                           (current-window-configuration))))
                (with-current-buffer temp-buffer (erase-buffer))
                ;; Test if the function works even when window is deleted
                (with-current-buffer temp-buffer (insert ?1 ?4))
                (let ((exit-key (vector ?q))
                      (delete-key (vector ?w)))
                  (should (= (loophole--test-with-keyboard-events
                                 (list delete-key exit-key)
                               (define-key overriding-terminal-local-map
                                 delete-key #'delete-frame)
                               (define-key overriding-terminal-local-map
                                 exit-key #'exit-recursive-edit)
                               (loophole-read-buffer #'ignore temp-buffer))
                             14))
                  (should (eq frame (selected-frame)))
                  (should (compare-window-configurations
                           window-configuration
                           (current-window-configuration))))
                (with-current-buffer temp-buffer (erase-buffer))
                ;; Test if window is set up and restored when
                ;; `loophole-read-buffer-inhibit-recursive-edit' is non-nil
                (with-current-buffer temp-buffer (insert ?1 ?5))
                (let ((loophole-read-buffer-inhibit-recursive-edit t))
                  (condition-case nil
                      (loophole-read-buffer
                       (lambda (_)
                         (throw 'loophole-test-read-buffer
                                (list (current-buffer)
                                      (selected-frame)
                                      (current-window-configuration))))
                       temp-buffer)
                    (quit))
                  (let ((context-during-writing
                         (catch 'loophole-test-read-buffer
                           (loophole--test-with-keyboard-events
                               loophole-read-buffer-finish-key
                             (define-key overriding-terminal-local-map
                               loophole-read-buffer-finish-key nil)
                             (with-current-buffer temp-buffer
                               (recursive-edit))))))
                    (let ((made-frame (cadr context-during-writing)))
                      (should (eq temp-buffer (car context-during-writing)))
                      (should-not (eq frame made-frame))
                      (should-not (compare-window-configurations
                                   window-configuration
                                   (caddr context-during-writing)))
                      (should (and made-frame
                                   (not (frame-live-p made-frame))))))
                  (should (eq frame (selected-frame)))
                  (should (compare-window-configurations
                           window-configuration
                           (current-window-configuration))))))))))))

(ert-deftest loophole-test-map-variable-for-keymap ()
  "Test for `loophole-map-variable-for-keymap'."
  (loophole--test-with-pseudo-environment
    (loophole--test-set-pseudo-map-alist)
    (should (eq (loophole-map-variable-for-keymap
                 (symbol-value (intern "loophole-1-map")))
                (intern "loophole-1-map")))
    (should (eq (loophole-map-variable-for-keymap
                 (symbol-value (intern "loophole-test-a-map")))
                (intern "loophole-test-a-map")))
    (should (eq (loophole-map-variable-for-keymap
                 (symbol-value (intern "loophole")))
                (intern "loophole")))
    (should (null (loophole-map-variable-for-keymap (make-sparse-keymap))))
    (should-error (loophole-map-variable-for-keymap 0)
                  :type 'wrong-type-argument)
    (should-error (loophole-map-variable-for-keymap 1.0)
                  :type 'wrong-type-argument)
    (should-error (loophole-map-variable-for-keymap ?c)
                  :type 'wrong-type-argument)
    (should-error (loophole-map-variable-for-keymap (intern "s"))
                  :type 'wrong-type-argument)
    (should-error (loophole-map-variable-for-keymap (cons 0 0))
                  :type 'wrong-type-argument)
    (should-error (loophole-map-variable-for-keymap (string ?s))
                  :type 'wrong-type-argument)
    (should-error (loophole-map-variable-for-keymap (vector ?v))
                  :type 'wrong-type-argument)))

(ert-deftest loophole-test-map-variable-for-key-binding ()
  "Test for `loophole-map-variable-for-key-binding'."
  (loophole--test-with-pseudo-environment
    (loophole--test-set-pseudo-map-alist)
    (define-key (symbol-value (intern "loophole-1-map")) [?\C-a] #'ignore)
    (define-key (symbol-value (intern "loophole")) [?\C-a] #'ignore)
    (define-key (symbol-value (intern "loophole-2-map")) [?\C-b] #'ignore)
    (define-key (symbol-value (intern "loophole-test-b-map")) [?\C-c]
      #'ignore)
    (set (intern "loophole-2-map-state") nil)
    (should (eq (loophole-map-variable-for-key-binding [?\C-a])
                (intern "loophole-1-map")))
    (should (null (loophole-map-variable-for-key-binding [?\C-b])))
    (should (eq (loophole-map-variable-for-key-binding [?\C-c])
                (intern "loophole-test-b-map")))
    (should-error (loophole-map-variable-for-key-binding 0)
                  :type 'wrong-type-argument)
    (should-error (loophole-map-variable-for-key-binding 1.0)
                  :type 'wrong-type-argument)
    (should-error (loophole-map-variable-for-key-binding ?c)
                  :type 'wrong-type-argument)
    (should-error (loophole-map-variable-for-key-binding (intern "s"))
                  :type 'wrong-type-argument)
    (should-error (loophole-map-variable-for-key-binding (cons 0 0))
                  :type 'wrong-type-argument)))

(ert-deftest loophole-test-registered-p ()
  "Test for `loophole-registered-p'."
  (loophole--test-with-pseudo-environment
    (loophole--test-set-pseudo-map-alist)
    (should (loophole-registered-p (intern "loophole-1-map")))
    (should (loophole-registered-p (intern "loophole-1-map")
                                   (intern "loophole-1-map-state")))
    (should (loophole-registered-p (intern "loophole-test-a-map")))
    (should (loophole-registered-p (intern "loophole-test-a-map")
                                   (intern "loophole-test-a-map-state")))
    (should (loophole-registered-p (intern "loophole")))
    (should (loophole-registered-p (intern "loophole")
                                   (intern "loophole-state")))
    (should-not (loophole-registered-p (intern "loophole-1-map")
                                       (intern "loophole-2-map-state")))
    (should-not (loophole-registered-p (intern "loophole-test-a-map")
                                       (intern "loophole-test-b-map-state")))
    (should-not (loophole-registered-p (intern "loophole")
                                       (intern "loophole-1-map-state")))
    (should-not (loophole-registered-p (intern "loophole-3-map")))
    (should-not (loophole-registered-p nil))
    (should-error (loophole-registered-p 0) :type 'wrong-type-argument)
    (should-error (loophole-registered-p 1.0) :type 'wrong-type-argument)
    (should-error (loophole-registered-p ?c) :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "s")))
    (should-error (loophole-registered-p (cons 0 0))
                  :type 'wrong-type-argument)
    (should-error (loophole-registered-p (string ?s))
                  :type 'wrong-type-argument)
    (should-error (loophole-registered-p (vector ?v))
                  :type 'wrong-type-argument)
    (should-error (loophole-registered-p nil 0) :type 'wrong-type-argument)
    (should-error (loophole-registered-p nil 1.0) :type 'wrong-type-argument)
    (should-error (loophole-registered-p nil ?c) :type 'wrong-type-argument)
    (should-not (loophole-registered-p nil (intern "s")))
    (should-error (loophole-registered-p nil (cons 0 0))
                  :type 'wrong-type-argument)
    (should-error (loophole-registered-p nil (string ?s))
                  :type 'wrong-type-argument)
    (should-error (loophole-registered-p nil (vector ?v))
                  :type 'wrong-type-argument)))

(ert-deftest loophole-test-priority-is-local-p ()
  "Test for `loophole-priority-is-local-p'."
  (loophole--test-with-pseudo-environment
    (should-not (loophole-priority-is-local-p))
    (loophole--test-set-pseudo-map-alist)
    (should (loophole-priority-is-local-p))
    (with-temp-buffer (should-not (loophole-priority-is-local-p)))))

(ert-deftest loophole-test-global-p ()
  "Test for `loophole-global-p'."
  (loophole--test-with-pseudo-environment
    (loophole--test-set-pseudo-map-alist)
    (should (loophole-global-p (intern "loophole-1-map")))
    (should-not (loophole-global-p (intern "loophole-2-map")))
    (should-not (loophole-global-p (intern "loophole-3-map")))
    (should-error (loophole-global-p 0) :type 'wrong-type-argument)
    (should-error (loophole-global-p 1.0) :type 'wrong-type-argument)
    (should-error (loophole-global-p ?c) :type 'wrong-type-argument)
    (should-not (loophole-global-p (intern "s")))
    (should-error (loophole-global-p (cons 0 0)) :type 'wrong-type-argument)
    (should-error (loophole-global-p (string ?s)) :type 'wrong-type-argument)
    (should-error (loophole-global-p (vector ?v)) :type 'wrong-type-argument)
    (should-not (loophole-global-p nil))))

(ert-deftest loophole-test-global-editing-p ()
  "Test for `loophole-global-editing-p'."
  (loophole--test-with-pseudo-environment
    (should-not (loophole-global-editing-p))
    (loophole--test-set-pseudo-map-alist)
    (setq-default loophole--editing (intern "loophole-1-map"))
    (should (loophole-global-editing-p))
    (setq-default loophole--editing t)
    (should-not (loophole-global-editing-p))))

(ert-deftest loophole-test-suspending-p ()
  "Test for `loophole-suspending-p'."
  (loophole--test-with-pseudo-environment
    (should-not (loophole-suspending-p))
    (setq emulation-mode-map-alists nil)
    (should (loophole-suspending-p))
    (setq emulation-mode-map-alists '(loophole--map-alist))
    (should-not (loophole-suspending-p))))

(ert-deftest loophole-test-editing ()
  "Test for `loophole-editing'."
  (loophole--test-with-pseudo-environment
    (should (null (loophole-editing)))
    (loophole--test-set-pseudo-map-alist)
    (setq-default loophole--editing (intern "loophole-1-map"))
    (should (eq (loophole-editing)
                (intern "loophole-1-map")))
    (setq loophole--editing (intern "loophole-1-map"))
    (should (eq (loophole-editing)
                (intern "loophole-1-map")))))

(ert-deftest loophole-test-tag-string ()
  "Test for `loophole-tag-string'."
  (loophole--test-with-pseudo-environment
    (loophole--test-set-pseudo-map-alist)
    (should (equal (loophole-tag-string (intern "loophole-1-map"))
                   (string ?1)))
    (should (equal (loophole-tag-string (intern "loophole-test-a-map"))
                   (string ?a)))
    (should (equal (loophole-tag-string (intern "loophole"))
                   (string ?l ?o ?o ?p)))
    (should-error (loophole-tag-string (intern "loophole-3-map"))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole-tag-string nil) :type 'error :exclude-subtypes t)
    (should-error (loophole-tag-string 0) :type 'wrong-type-argument)
    (should-error (loophole-tag-string 1.0) :type 'wrong-type-argument)
    (should-error (loophole-tag-string ?c) :type 'wrong-type-argument)
    (should-error (loophole-tag-string (intern "s"))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole-tag-string (cons 0 0)) :type 'wrong-type-argument)
    (should-error (loophole-tag-string (string ?s)) :type 'wrong-type-argument)
    (should-error (loophole-tag-string (vector ?v))
                  :type 'wrong-type-argument)))

(ert-deftest loophole-test-char-read-syntax ()
  "Test for `loophole--char-read-syntax'."
  (should (equal (loophole--char-read-syntax ?a) (string ?? ?a)))
  (should (equal (loophole--char-read-syntax ?\C-a) (string ?? ?\\ ?C ?- ?a)))
  (should (equal (loophole--char-read-syntax ?\M-a) (string ?? ?\\ ?M ?- ?a)))
  (should (equal (loophole--char-read-syntax ?\S-a) (string ?? ?\\ ?S ?- ?a)))
  (should (equal (loophole--char-read-syntax ?\H-a) (string ?? ?\\ ?H ?- ?a)))
  (should (equal (loophole--char-read-syntax ?\s-a) (string ?? ?\\ ?s ?- ?a)))
  (should (equal (loophole--char-read-syntax ?\A-a) (string ?? ?\\ ?A ?- ?a)))
  (should (equal (loophole--char-read-syntax ?\C-\M-\S-\H-\s-\A-a)
                 (string ??
                         ?\\ ?A ?-
                         ?\\ ?s ?-
                         ?\\ ?H ?-
                         ?\\ ?S ?-
                         ?\\ ?C ?-
                         ?\\ ?M ?-
                         ?a)))
  (should (equal (loophole--char-read-syntax (intern "return")) "return"))
  (should (equal (loophole--char-read-syntax ?a) (string ?? ?a)))
  (should (equal (loophole--char-read-syntax ?a) (string ?? ?a)))
  (should-error (loophole-global-p 1.0) :type 'wrong-type-argument)
  (should-error (loophole-global-p (cons 0 0)) :type 'wrong-type-argument)
  (should-error (loophole-global-p (string ?s)) :type 'wrong-type-argument)
  (should-error (loophole-global-p (vector ?v)) :type 'wrong-type-argument))

(ert-deftest loophole-test-symbol-function-recursively ()
  "Test for `loophole--symbol-function-recursively'."
  (let ((f1 (make-symbol "f1"))
        (f2 (make-symbol "f2"))
        (f3 (make-symbol "f3"))
        (f4 (make-symbol "f4"))
        (f5 (make-symbol "f5"))
        (f6 (make-symbol "f6"))
        (f7 (make-symbol "f7"))
        (f8 (make-symbol "f8"))
        (l1 (lambda () 1))
        (l2 (lambda () 2))
        (l3 (lambda () 3)))
    (fset f1 l1)
    (fset f2 f3)
    (fset f3 l2)
    (fset f4 f5)
    (fset f5 f6)
    (fset f6 l3)
    (fset f7 f8)
    (fset f8 f7)
    (should (eq (loophole--symbol-function-recursively f1) l1))
    (should (eq (loophole--symbol-function-recursively f3) l2))
    (should (eq (loophole--symbol-function-recursively f6) l3))
    (should (eq (loophole--symbol-function-recursively f7) f7))
    (should (eq (loophole--symbol-function-recursively f8) f8))))

(ert-deftest loophole-test-trace-key-to-find-non-keymap-entry ()
  "Test for `loophole--trace-key-to-find-non-keymap-entry'."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector ?\C-c ?a ?b) #'ignore)
    (define-key map (vector ?\C-c ?c) (intern "loophole-prefix"))
    (fset (intern "loophole-prefix")
          (let ((inner-map (make-sparse-keymap)))
            (define-key inner-map (vector ?x) #'ignore)
            (define-key inner-map (vector ?y ?z) #'ignore)
            inner-map))
    (should (equal (loophole--trace-key-to-find-non-keymap-entry
                    (vector ?\C-c ?a ?b) map)
                   (vector ?\C-c ?a ?b)))
    (should (equal (loophole--trace-key-to-find-non-keymap-entry
                    (vector ?\C-c ?c ?x) map)
                   (vector ?\C-c ?c ?x)))
    (should (equal (loophole--trace-key-to-find-non-keymap-entry
                    (vector ?\C-c ?c ?y ?z) map)
                   (vector ?\C-c ?c ?y ?z)))
    (should-not (loophole--trace-key-to-find-non-keymap-entry
                 (vector ?\C-x) map))
    (should-not (loophole--trace-key-to-find-non-keymap-entry
                 (vector ?\C-c ?i) map))
    (should-not (loophole--trace-key-to-find-non-keymap-entry
                 (vector ?\C-c ?a) map))
    (should (equal (loophole--trace-key-to-find-non-keymap-entry
                    (vector ?\C-c ?a ?b ?i) map)
                   (vector ?\C-c ?a ?b)))
    (should (equal (loophole--trace-key-to-find-non-keymap-entry
                    (vector ?\C-c ?c ?x ?i) map)
                   (vector ?\C-c ?c ?x)))
    (should-not (loophole--trace-key-to-find-non-keymap-entry
                 (vector ?\C-c ?y) map))
    (should (equal (loophole--trace-key-to-find-non-keymap-entry
                    (vector ?\C-c ?c ?y ?z ?i) map)
                   (vector ?\C-c ?c ?y ?z)))
    (should-error (loophole--trace-key-to-find-non-keymap-entry 0 map)
                  :type 'wrong-type-argument)
    (should-error (loophole--trace-key-to-find-non-keymap-entry 1.0 map)
                  :type 'wrong-type-argument)
    (should-error (loophole--trace-key-to-find-non-keymap-entry ?c map)
                  :type 'wrong-type-argument)
    (should-error
     (loophole--trace-key-to-find-non-keymap-entry (intern "s") map)
     :type 'wrong-type-argument)
    (should-error (loophole--trace-key-to-find-non-keymap-entry (cons 0 0) map)
                  :type 'wrong-type-argument)
    (should-error (loophole--trace-key-to-find-non-keymap-entry (vector ?a) 0)
                  :type 'wrong-type-argument)
    (should-error (loophole--trace-key-to-find-non-keymap-entry (vector ?a) 1.0)
                  :type 'wrong-type-argument)
    (should-error (loophole--trace-key-to-find-non-keymap-entry (vector ?a) ?c)
                  :type 'wrong-type-argument)
    (should-error
     (loophole--trace-key-to-find-non-keymap-entry (vector ?a) (intern "s"))
     :type 'wrong-type-argument)
    (should-error
     (loophole--trace-key-to-find-non-keymap-entry (vector ?a) (cons 0 0))
     :type 'wrong-type-argument)
    (should-error
     (loophole--trace-key-to-find-non-keymap-entry (vector ?a) (string ?s))
     :type 'wrong-type-argument)
    (should-error
     (loophole--trace-key-to-find-non-keymap-entry (vector ?a) (vector ?v))
     :type 'wrong-type-argument)))

(ert-deftest loophole-test-protected-keymap-prefix-key ()
  "Test for `loophole--protected-keymap-prefix-key'."
  (let ((make-void-protected-element
         (lambda (key)
           (let* ((body-map (make-sparse-keymap))
                  (wall-map (make-sparse-keymap))
                  (protected-element (list body-map wall-map)))
             (define-key body-map key (make-sparse-keymap))
             (define-key wall-map key 'undefined)
             protected-element))))
    (let ((key (vector ?q)))
      (should (equal key
                     (loophole--protected-keymap-prefix-key
                      (funcall make-void-protected-element key)))))
    (let ((key (vector ?\C-c ?a)))
      (should (equal key
                     (loophole--protected-keymap-prefix-key
                      (funcall make-void-protected-element key)))))
    (let ((key (vector ?\C-c ?a ?b ?c)))
      (should (equal key
                     (loophole--protected-keymap-prefix-key
                      (funcall make-void-protected-element key)))))
    (let ((key (vector 'left ?a ?b ?c)))
      (should (equal key
                     (loophole--protected-keymap-prefix-key
                      (funcall make-void-protected-element key)))))
    (let ((element (funcall make-void-protected-element (vector ?\C-c ?a))))
      (define-key (car element) (vector ?\C-c ?b) (make-sparse-keymap))
      (define-key (cadr element) (vector ?\C-c ?b) 'undefined)
      (should-error (loophole--protected-keymap-prefix-key element)
                    :type 'error :exclude-subtypes t))
    (let* ((key (vector ?\C-c ?a))
           (element (funcall make-void-protected-element key)))
      (define-key (cadr element) key #'ignore)
      (should-error (loophole--protected-keymap-prefix-key element)
                    :type 'error :exclude-subtypes t))
    (let* ((key (vector ?\C-c ?a))
           (element (funcall make-void-protected-element key)))
      (define-key (car element) key nil)
      (should-error (loophole--protected-keymap-prefix-key element)
                    :type 'error :exclude-subtypes t)))
  (should-error (loophole--protected-keymap-prefix-key 0)
                :type 'wrong-type-argument)
  (should-error (loophole--protected-keymap-prefix-key 1.0)
                :type 'wrong-type-argument)
  (should-error (loophole--protected-keymap-prefix-key ?c)
                :type 'wrong-type-argument)
  (should-error (loophole--protected-keymap-prefix-key (intern "s"))
                :type 'wrong-type-argument)
  (should-error (loophole--protected-keymap-prefix-key (cons 0 0))
                :type 'error :exclude-subtypes t)
  (should-error (loophole--protected-keymap-prefix-key (string ?s))
                :type 'wrong-type-argument)
  (should-error (loophole--protected-keymap-prefix-key (vector ?v))
                :type 'wrong-type-argument)
  (should-error (loophole--protected-keymap-prefix-key
                 (list (make-sparse-keymap) (make-sparse-keymap)))
                :type 'error :exclude-subtypes t))

(ert-deftest loophole-test-protected-keymap-entry-list ()
  "Test for `loophole--protected-keymap-entry-list'."
  (let ((protected-keymap (make-sparse-keymap)))
    (setcdr protected-keymap
            (let ((body-map (make-sparse-keymap))
                  (wall-map (make-sparse-keymap)))
              (define-key body-map (vector 4)
                (intern "loophole-protected-map"))
              (define-key wall-map (vector 4) #'undefined)
              (fset (intern "loophole-protected-map") (make-sparse-keymap))
              (cons body-map (cons wall-map (cdr protected-keymap)))))
    (dolist (key '(3 2 1))
      (setcdr protected-keymap
              (let ((body-map (make-sparse-keymap))
                    (wall-map (make-sparse-keymap)))
                (define-key body-map (vector key) (make-sparse-keymap))
                (define-key wall-map (vector key) #'undefined)
                (cons body-map (cons wall-map (cdr protected-keymap))))))
    (should (equal (loophole--protected-keymap-entry-list protected-keymap)
                   `(((keymap (1 keymap)) (keymap (1 . undefined)))
                     ((keymap (2 keymap)) (keymap (2 . undefined)))
                     ((keymap (3 keymap)) (keymap (3 . undefined)))
                     ((keymap (4 . ,(intern "loophole-protected-map")))
                      (keymap (4 . undefined))))))
    (should-error (loophole--protected-keymap-entry-list 0)
                  :type 'wrong-type-argument)
    (should-error (loophole--protected-keymap-entry-list 1.0)
                  :type 'wrong-type-argument)
    (should-error (loophole--protected-keymap-entry-list ?c)
                  :type 'wrong-type-argument)
    (should-error (loophole--protected-keymap-entry-list (intern "s"))
                  :type 'wrong-type-argument)
    (should-error (loophole--protected-keymap-entry-list (cons 0 0))
                  :type 'wrong-type-argument)
    (should-error (loophole--protected-keymap-entry-list (string ?s))
                  :type 'wrong-type-argument)
    (should-error (loophole--protected-keymap-entry-list (vector ?v))
                  :type 'wrong-type-argument)
    (let ((copy (copy-sequence protected-keymap)))
      (set-keymap-parent copy (make-sparse-keymap))
      (should-error (loophole--protected-keymap-entry-list copy)
                    :type 'error :exclude-subtypes t))
    (should-error (loophole--protected-keymap-entry-list
                   (butlast protected-keymap 1))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole--protected-keymap-entry-list
                   (let ((copy (copy-sequence protected-keymap)))
                     (setcar (cdr copy) 1)
                     copy))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole--protected-keymap-entry-list
                   (let ((copy (copy-sequence protected-keymap)))
                     (define-key (cadr copy) (vector 1) #'ignore)
                     copy))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole--protected-keymap-entry-list
                   (let ((copy (copy-sequence protected-keymap)))
                     (define-key (caddr copy) (vector 1) #'ignore)
                     copy))
                  :type 'error :exclude-subtypes t)
    (let ((copy (copy-sequence protected-keymap)))
      (setcdr copy
              (let ((body-map (make-sparse-keymap))
                    (wall-map (make-sparse-keymap)))
                (define-key body-map (vector 5) (make-sparse-keymap))
                (define-key wall-map (vector 6) #'undefined)
                (cons body-map (cons wall-map (cdr copy)))))
      (should-error (loophole--protected-keymap-entry-list copy)
                    :type 'error :exclude-subtypes t))))

(ert-deftest loophole-test-add-protected-keymap-entry ()
  "Test for `loophole--add-protected-keymap-entry'."
  (loophole--test-with-pseudo-environment
    (loophole--test-set-pseudo-map-alist)
    (loophole--add-protected-keymap-entry (intern "loophole-1-map")
                                          (vector ?a)
                                          (make-sparse-keymap))
    (let ((protected-keymap
           (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should (eq protected-keymap
                  (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal
               (loophole--protected-keymap-prefix-key
                (car (loophole--protected-keymap-entry-list protected-keymap)))
               (vector ?a))))
    (set-keymap-parent (symbol-value (intern "loophole-2-map"))
                       loophole-base-map)
    (loophole--add-protected-keymap-entry (intern "loophole-2-map")
                                          (vector ?b)
                                          (make-sparse-keymap))
    (let ((protected-keymap
           (get (intern "loophole-2-map") :loophole-protected-keymap)))
      (should (memq protected-keymap
                    (keymap-parent (symbol-value (intern "loophole-2-map"))))))
    (loophole--add-protected-keymap-entry (intern "loophole-2-map")
                                          (vector ?c)
                                          (make-sparse-keymap))
    (let ((protected-keymap
           (get (intern "loophole-2-map") :loophole-protected-keymap)))
      (should (memq protected-keymap
                    (keymap-parent (symbol-value (intern "loophole-2-map"))))))
    (set-keymap-parent (symbol-value (intern "loophole-test-a-map"))
                       (make-composed-keymap (list loophole-base-map
                                                   (make-sparse-keymap))))
    (loophole--add-protected-keymap-entry (intern "loophole-test-a-map")
                                          (vector ?d)
                                          (make-sparse-keymap))
    (let ((protected-keymap
           (get (intern "loophole-test-a-map") :loophole-protected-keymap)))
      (should (memq protected-keymap
                    (keymap-parent
                     (symbol-value (intern "loophole-test-a-map"))))))
    (set-keymap-parent (symbol-value (intern "loophole-test-b-map"))
                       (make-sparse-keymap))
    (loophole--add-protected-keymap-entry (intern "loophole-test-b-map")
                                          (vector ?e)
                                          (make-sparse-keymap))
    (let ((protected-keymap
           (get (intern "loophole-test-b-map") :loophole-protected-keymap)))
      (should (memq protected-keymap
                    (keymap-parent
                     (symbol-value (intern "loophole-test-b-map"))))))
    (let ((map (symbol-value (intern "loophole")))
          (key (vector ?f)))
      (define-key map key #'ignore)
      (loophole--add-protected-keymap-entry (intern "loophole")
                                            key
                                            (make-sparse-keymap))
      (let ((parent (keymap-parent map)))
        (unwind-protect
            (progn
              (set-keymap-parent map nil)
              (let ((lookup (lookup-key map key))
                    (trace (loophole--trace-key-to-find-non-keymap-entry
                            key map)))
                (should (or (null lookup)
                            (and (numberp lookup)
                                 (null trace))))))
          (set-keymap-parent map parent))))
    (define-key (symbol-value (intern "loophole")) (vector ?g) #'ignore)
    (let ((parent (keymap-parent (symbol-value (intern "loophole"))))
          (protected-keymap
           (get (intern "loophole") :loophole-protected-keymap)))
      (should-error
       (loophole--add-protected-keymap-entry (intern "loophole")
                                             (vector ?g ?a)
                                             (make-sparse-keymap))
       :type 'error :exclude-subtypes t)
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole")))))
      (should (equal protected-keymap
                     (get (intern "loophole") :loophole-protected-keymap))))
    (should-error (loophole--add-protected-keymap-entry
                   nil (vector ?h) (make-sparse-keymap))
                  :type 'error :exclude-subtypes t)
    (should-error (loophole--add-protected-keymap-entry
                   0 (vector ?h) (make-sparse-keymap))
                  :type 'wrong-type-argument)
    (should-error (loophole--add-protected-keymap-entry
                   1.0 (vector ?h) (make-sparse-keymap))
                  :type 'wrong-type-argument)
    (should-error (loophole--add-protected-keymap-entry
                   ?c (vector ?h) (make-sparse-keymap))
                  :type 'wrong-type-argument)
    (let ((protected-keymap (get (intern "s") :loophole-protected-keymap)))
      (should-error (loophole--add-protected-keymap-entry
                     (intern "s") (vector ?h) (make-sparse-keymap))
                    :type 'error :exclude-subtypes t)
      (should (equal protected-keymap
                     (get (intern "s") :loophole-protected-keymap))))
    (should-error (loophole--add-protected-keymap-entry
                   (cons 0 0) (vector ?h) (make-sparse-keymap))
                  :type 'wrong-type-argument)
    (should-error (loophole--add-protected-keymap-entry
                   (string ?s) (vector ?h) (make-sparse-keymap))
                  :type 'wrong-type-argument)
    (should-error (loophole--add-protected-keymap-entry
                   (vector ?v) (vector ?h) (make-sparse-keymap))
                  :type 'wrong-type-argument)
    (let ((parent (keymap-parent (symbol-value (intern "loophole-1-map"))))
          (protected-keymap
           (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--add-protected-keymap-entry
                     (intern "loophole-1-map") 0 (make-sparse-keymap))
                    :type 'wrong-type-argument)
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let ((parent (keymap-parent (symbol-value (intern "loophole-1-map"))))
          (protected-keymap
           (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--add-protected-keymap-entry
                     (intern "loophole-1-map") 1.0 (make-sparse-keymap))
                    :type 'wrong-type-argument)
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let ((parent (keymap-parent (symbol-value (intern "loophole-1-map"))))
          (protected-keymap
           (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--add-protected-keymap-entry
                     (intern "loophole-1-map") ?c (make-sparse-keymap))
                    :type 'wrong-type-argument)
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let ((parent (keymap-parent (symbol-value (intern "loophole-1-map"))))
          (protected-keymap
           (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--add-protected-keymap-entry
                     (intern "loophole-1-map") (intern "s")
                     (make-sparse-keymap))
                    :type 'wrong-type-argument)
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let ((parent (keymap-parent (symbol-value (intern "loophole-1-map"))))
          (protected-keymap
           (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--add-protected-keymap-entry
                     (intern "loophole-1-map") (cons 0 0) (make-sparse-keymap))
                    :type 'wrong-type-argument)
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let ((parent (keymap-parent (symbol-value (intern "loophole-1-map"))))
          (protected-keymap
           (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--add-protected-keymap-entry
                     (intern "loophole-1-map") (vector ?h) 0)
                    :type 'wrong-type-argument)
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let ((parent (keymap-parent (symbol-value (intern "loophole-1-map"))))
          (protected-keymap
           (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--add-protected-keymap-entry
                     (intern "loophole-1-map") (vector ?h) 1.0)
                    :type 'wrong-type-argument)
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let ((parent (keymap-parent (symbol-value (intern "loophole-1-map"))))
          (protected-keymap
           (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--add-protected-keymap-entry
                     (intern "loophole-1-map") (vector ?h) ?c)
                    :type 'wrong-type-argument)
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let ((parent (keymap-parent (symbol-value (intern "loophole-1-map"))))
          (protected-keymap
           (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--add-protected-keymap-entry
                     (intern "loophole-1-map") (vector ?h) (intern "s"))
                    :type 'wrong-type-argument)
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let ((parent (keymap-parent (symbol-value (intern "loophole-1-map"))))
          (protected-keymap
           (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--add-protected-keymap-entry
                     (intern "loophole-1-map") (vector ?h) (cons 0 0))
                    :type 'wrong-type-argument)
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let ((parent (keymap-parent (symbol-value (intern "loophole-1-map"))))
          (protected-keymap
           (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--add-protected-keymap-entry
                     (intern "loophole-1-map") (vector ?h) (string ?s))
                    :type 'wrong-type-argument)
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let ((parent (keymap-parent (symbol-value (intern "loophole-1-map"))))
          (protected-keymap
           (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--add-protected-keymap-entry
                     (intern "loophole-1-map") (vector ?h) (vector ?v))
                    :type 'wrong-type-argument)
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))))

(ert-deftest loophole-test-set-ordinary-entry ()
  "Test for `loophole--set-ordinary-entry'."
  (loophole--test-with-pseudo-environment
    (loophole--test-set-pseudo-map-alist)
    (let ((key (vector ?a))
          (var (intern "loophole-1-map")))
      (loophole--set-ordinary-entry var key #'ignore)
      (should (eq (lookup-key (symbol-value var) key) #'ignore)))
    (let* ((key (vector ?b))
           (var (intern "loophole-2-map"))
           (map (symbol-value var)))
      (loophole--add-protected-keymap-entry var key (make-sparse-keymap))
      (loophole--set-ordinary-entry var key #'ignore)
      (should (eq (lookup-key map key) #'ignore))
      (should-not (lookup-key (get var :loophole-protected-keymap) key))
      (should-not (keymap-parent map)))
    (let* ((key (vector ?c))
           (var (intern "loophole-2-map"))
           (map (symbol-value var)))
      (loophole--add-protected-keymap-entry var
                                            (vconcat key (vector ?d))
                                            (make-sparse-keymap))
      (loophole--set-ordinary-entry var key #'ignore)
      (should (eq (lookup-key map key) #'ignore))
      (should-not (lookup-key (get var :loophole-protected-keymap) key))
      (should-not (keymap-parent map)))
    (let* ((key (vector ?e))
           (var (intern "loophole-test-a-map"))
           (map (symbol-value var)))
      (set-keymap-parent map loophole-base-map)
      (loophole--add-protected-keymap-entry var key (make-sparse-keymap))
      (loophole--set-ordinary-entry var key #'ignore)
      (should (eq (keymap-parent map) loophole-base-map)))
    (let* ((key (vector ?f))
           (var (intern "loophole-test-b-map"))
           (map (symbol-value var))
           (key2 (vector ?g)))
      (set-keymap-parent map loophole-base-map)
      (loophole--add-protected-keymap-entry var key (make-sparse-keymap))
      (loophole--add-protected-keymap-entry var key2 (make-sparse-keymap))
      (loophole--set-ordinary-entry var key #'ignore)
      (should (keymapp (keymap-parent map)))
      (should (memq loophole-base-map (keymap-parent map)))
      (should (memq (get var :loophole-protected-keymap) (keymap-parent map)))
      (should-not (lookup-key (get var :loophole-protected-keymap) key))
      (should (lookup-key (get var :loophole-protected-keymap) key2)))
    (let* ((key (vector ?h))
           (var (intern "loophole"))
           (map (symbol-value var))
           (key2 (vector ?i))
           (original-parent (make-sparse-keymap))
           (parent (make-composed-keymap (list loophole-base-map
                                               original-parent))))
      (set-keymap-parent map parent)
      (loophole--add-protected-keymap-entry var key (make-sparse-keymap))
      (loophole--add-protected-keymap-entry var key2 (make-sparse-keymap))
      (loophole--set-ordinary-entry var key #'ignore)
      (should (keymapp (keymap-parent map)))
      (should (memq loophole-base-map (keymap-parent map)))
      (should (memq original-parent (keymap-parent map)))
      (should (memq (get var :loophole-protected-keymap) (keymap-parent map)))
      (should-not (lookup-key (get var :loophole-protected-keymap) key))
      (should (lookup-key (get var :loophole-protected-keymap) key2)))
    (should-error (loophole--set-ordinary-entry nil (vector ?j) nil)
                  :type 'error :exclude-subtypes t)
    (should-error (loophole--set-ordinary-entry 0 (vector ?j) nil)
                  :type 'wrong-type-argument)
    (should-error (loophole--set-ordinary-entry 1.0 (vector ?j) nil)
                  :type 'wrong-type-argument)
    (should-error (loophole--set-ordinary-entry ?c (vector ?j) nil)
                  :type 'wrong-type-argument)
    (let ((protected-keymap (get (intern "s") :loophole-protected-keymap) ))
      (should-error (loophole--set-ordinary-entry (intern "s") (vector ?j) nil)
                    :type 'error :exclude-subtypes t)
      (should (equal protected-keymap
                     (get (intern "s") :loophole-protected-keymap))))
    (should-error (loophole--set-ordinary-entry (cons 0 0) (vector ?j) nil)
                  :type 'wrong-type-argument)
    (should-error (loophole--set-ordinary-entry (string ?s) (vector ?j) nil)
                  :type 'wrong-type-argument)
    (should-error (loophole--set-ordinary-entry (vector ?v) (vector ?j) nil)
                  :type 'wrong-type-argument)
    (let* ((map (symbol-value (intern "loophole-1-map")))
           (parent (keymap-parent map))
           (protected-keymap
            (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--set-ordinary-entry
                   (intern "loophole-1-map") 0 nil)
                    :type 'wrong-type-argument)
      (should (equal map (symbol-value (intern "loophole-1-map"))))
      (should (equal parent (keymap-parent
                             (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let* ((map (symbol-value (intern "loophole-1-map")))
           (parent (keymap-parent map))
           (protected-keymap
            (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--set-ordinary-entry
                   (intern "loophole-1-map") 1.0 nil)
                    :type 'wrong-type-argument)
      (should (equal map (symbol-value (intern "loophole-1-map"))))
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let* ((map (symbol-value (intern "loophole-1-map")))
           (parent (keymap-parent map))
           (protected-keymap
            (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--set-ordinary-entry
                   (intern "loophole-1-map") ?c nil)
                    :type 'wrong-type-argument)
      (should (equal map (symbol-value (intern "loophole-1-map"))))
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let* ((map (symbol-value (intern "loophole-1-map")))
           (parent (keymap-parent map))
           (protected-keymap
            (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--set-ordinary-entry
                   (intern "loophole-1-map") (intern "loophole-1-map") nil)
                    :type 'wrong-type-argument)
      (should (equal map (symbol-value (intern "loophole-1-map"))))
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))
    (let* ((map (symbol-value (intern "loophole-1-map")))
           (parent (keymap-parent map))
           (protected-keymap
            (get (intern "loophole-1-map") :loophole-protected-keymap)))
      (should-error (loophole--set-ordinary-entry
                   (intern "loophole-1-map") (cons 0 0) nil)
                    :type 'wrong-type-argument)
      (should (equal map (symbol-value (intern "loophole-1-map"))))
      (should (equal parent
                     (keymap-parent (symbol-value (intern "loophole-1-map")))))
      (should (equal protected-keymap
                     (get (intern "loophole-1-map")
                          :loophole-protected-keymap))))))

(ert-deftest loophole-test-toss-binding-form ()
  "Test for `loophole-toss-binding-form'."
  (loophole--test-with-pseudo-environment
    (loophole--test-set-pseudo-map-alist)
    (let ((key (vector ?a))
          (form 'ctl-x-4-map))
      (loophole-toss-binding-form key form)
      (loophole-bind-entry key (eval form)
                           (symbol-value (intern "loophole-1-map")))
      (should (member (cons key form)
                      (get (intern "loophole-1-map") :loophole-form-storage)))
      (let (advice-is-remained)
        (advice-mapc (lambda (advice _)
                       (if (and (symbolp advice)
                                (string-equal (symbol-name advice)
                                              "loophole-one-time-advice"))
                           (setq advice-is-remained t)))
                     'loophole-bind-entry)
        (should-not advice-is-remained))
      (loophole-toss-binding-form key form)
      (loophole-bind-entry key #'ignore
                           (symbol-value (intern "loophole-2-map")))
      (should-not
       (member (cons key form)
               (get (intern "loophole-2-map") :loophole-form-storage)))
      (let (advice-is-remained)
        (advice-mapc (lambda (advice _)
                       (if (and (symbolp advice)
                                (string-equal (symbol-name advice)
                                              "loophole-one-time-advice"))
                           (setq advice-is-remained t)))
                     'loophole-bind-entry)
        (should-not advice-is-remained)))))

(ert-deftest loophole-test-valid-form ()
  "Test for `loophole--valid-form'."
  (loophole--test-with-pseudo-environment
    (loophole--test-set-pseudo-map-alist)
    (let ((key (vector ?a))
          (form 'ctl-x-4-map))
      (loophole-toss-binding-form key form)
      (loophole-bind-entry key (eval form)
                           (symbol-value (intern "loophole-1-map")))
      (should (equal (loophole--valid-form (intern "loophole-1-map"))
                     (list (cons key form))))
      (loophole-toss-binding-form key form)
      (loophole-bind-entry key (eval form)
                           (symbol-value (intern "loophole-1-map")))
      (loophole--valid-form (intern "loophole-1-map"))
      (should (equal (get (intern "loophole-1-map") :loophole-form-storage)
                     (list (cons key form))))
      (loophole-bind-entry key nil (symbol-value (intern "loophole-1-map")))
      (loophole--valid-form (intern "loophole-1-map"))
      (should-not (get (intern "loophole-1-map") :loophole-form-storage)))))

(ert-deftest loophole-test-follow-killing-local-variable ()
  "Test for `loophole--follow-killing-local-variable'."
  (loophole--test-with-pseudo-environment
    (with-temp-buffer
      (setq loophole--buffer-list (list (current-buffer)))
      (add-hook 'change-major-mode-hook
                #'loophole--follow-killing-local-variable nil t)
      (add-hook 'kill-buffer-hook
                #'loophole--follow-killing-local-variable nil t)
      (text-mode)
      (should-not (memq (current-buffer) loophole--buffer-list))
      (should-not (memq #'loophole--follow-killing-local-variable
                        change-major-mode-hook))
      (should-not (memq #'loophole--follow-killing-local-variable
                        kill-buffer-hook))
      (setq loophole--buffer-list (list (current-buffer)))
      (add-hook 'change-major-mode-hook
                #'loophole--follow-killing-local-variable nil t)
      (add-hook 'kill-buffer-hook
                #'loophole--follow-killing-local-variable nil t))
    (should-not (memq (current-buffer) loophole--buffer-list))))

(ert-deftest loophole-test-follow-adding-local-variable ()
  "Test for `loophole--follow-adding-local-variable'."
  (loophole--test-with-pseudo-environment
    (setq loophole--buffer-list nil)
    (with-temp-buffer
      (make-variable-buffer-local (intern "loophole-1"))
      (add-variable-watcher (intern "loophole-1")
                            #'loophole--follow-adding-local-variable)
      (set (intern "loophole-1") t)
      (should (memq (current-buffer) loophole--buffer-list))
      (should (memq #'loophole--follow-killing-local-variable
                    change-major-mode-hook))
      (should (memq #'loophole--follow-killing-local-variable
                    kill-buffer-hook))
      (set (intern "loophole-1") t)
      (should (= (length (seq-uniq loophole--buffer-list))
                 (length loophole--buffer-list))))
    (with-temp-buffer
      (unwind-protect
          (progn
            (add-variable-watcher 'loophole--map-alist
                                  #'loophole--follow-adding-local-variable)
            (let ((loophole--map-alist))
              (should-not (memq (current-buffer) loophole--buffer-list))
              (should-not (memq #'loophole--follow-killing-local-variable
                                change-major-mode-hook))
              (should-not (memq #'loophole--follow-killing-local-variable
                                kill-buffer-hook)))
            (should-not (memq (current-buffer) loophole--buffer-list))
            (should-not (memq #'loophole--follow-killing-local-variable
                              change-major-mode-hook))
            (should-not (memq #'loophole--follow-killing-local-variable
                              kill-buffer-hook)))
        (remove-variable-watcher 'loophole--map-alist
                                 #'loophole--follow-adding-local-variable))
      (make-variable-buffer-local (intern "loophole-2"))
      (set (intern "loophole-2") t)
      (add-variable-watcher (intern "loophole-2")
                            #'loophole--follow-adding-local-variable)
      (makunbound (intern "loophole-2"))
      (should-not (memq (current-buffer) loophole--buffer-list))
      (should-not (memq #'loophole--follow-killing-local-variable
                        change-major-mode-hook))
      (should-not (memq #'loophole--follow-killing-local-variable
                        kill-buffer-hook))
      (make-variable-buffer-local (intern "loophole-3"))
      (add-variable-watcher (intern "loophole-3")
                            #'loophole--follow-adding-local-variable)
      (defvaralias (intern "loophole-4") (intern "loophole-3"))
      (should-not (memq (current-buffer) loophole--buffer-list))
      (should-not (memq #'loophole--follow-killing-local-variable
                        change-major-mode-hook))
      (should-not (memq #'loophole--follow-killing-local-variable
                        kill-buffer-hook))
      (add-variable-watcher (intern "loophole-5")
                            #'loophole--follow-adding-local-variable)
      (set (intern "loophole-5") t)
      (should-not (memq (current-buffer) loophole--buffer-list))
      (should-not (memq #'loophole--follow-killing-local-variable
                    change-major-mode-hook))
      (should-not (memq #'loophole--follow-killing-local-variable
                    kill-buffer-hook)))
    (setq loophole--buffer-list t)
    (with-temp-buffer
      (make-variable-buffer-local (intern "loophole-6"))
      (add-variable-watcher (intern "loophole-6")
                            #'loophole--follow-adding-local-variable)
      (set (intern "loophole-6") t)
      (should-not (memq #'loophole--follow-adding-local-variable
                        (get-variable-watchers (intern "loophole-6"))))
      (should-not (memq #'loophole--follow-killing-local-variable
                        change-major-mode-hook))
      (should-not (memq #'loophole--follow-killing-local-variable
                        kill-buffer-hook)))))

(ert-deftest loophole-test-buffer-list ()
  "Test for `loophole-buffer-list'."
  (loophole--test-with-pseudo-environment
    (let (buffer1 buffer2)
      (with-temp-buffer
        (loophole--test-set-pseudo-map-alist)
        (setq buffer1 (current-buffer))
        (with-temp-buffer
          (setq buffer2 (current-buffer))
          (set (intern "loophole-2-map-state") t)
          (set (intern "loophole-test-b-map-state") t)
          (set (intern "loophole-state") t)
          (should-not (seq-difference (loophole-buffer-list)
                                      (list buffer2 buffer1)))
          (setq loophole--buffer-list (list buffer2 buffer1))
          (should-not (seq-difference (loophole-buffer-list)
                                      (list buffer2 buffer1)))
          (with-temp-buffer
            (setq loophole--buffer-list (list buffer2 buffer1 (current-buffer)))
            (should-not (seq-difference (loophole-buffer-list)
                                        (list buffer2 buffer1)))
            (should-not (seq-difference loophole--buffer-list
                                        (list buffer2 buffer1))))
          (with-temp-buffer
            (add-hook 'change-major-mode-hook
                      #'loophole--follow-killing-local-variable nil t)
            (add-hook 'kill-buffer-hook
                      #'loophole--follow-killing-local-variable nil t)
            (setq loophole--buffer-list (list buffer2 buffer1 (current-buffer)))
            (loophole-buffer-list)
            (should-not (memq #'loophole--follow-killing-local-variable
                              change-major-mode-hook))
            (should-not (memq #'loophole--follow-killing-local-variable
                              kill-buffer-hook)))
          (with-temp-buffer
            (add-hook 'change-major-mode-hook
                      #'loophole--follow-killing-local-variable nil t)
            (add-hook 'kill-buffer-hook
                      #'loophole--follow-killing-local-variable nil t)
            (setq loophole--buffer-list t)
            (loophole-buffer-list)
            (should-not (memq #'loophole--follow-killing-local-variable
                              change-major-mode-hook))
            (should-not (memq #'loophole--follow-killing-local-variable
                              kill-buffer-hook))))))))

;;; Main functions

(ert-deftest loophole-test-register ()
  "Test for `loophole-register'.
This test has assertions for interactive behaviors of
`loophole-register'.  When this test is run in
batch-mode, these assertions are skipped."
  :tags '(interactive)
  (loophole--test-with-pseudo-environment
    ;; Basic behavior
    (set (intern "loophole-1-map") (make-sparse-keymap))
    (set (intern "loophole-1-map-state") nil)
    (make-variable-buffer-local (intern "loophole-1-map-state"))
    (loophole-register (intern "loophole-1-map")
                       (intern "loophole-1-map-state")
                       (string ?1))
    (should (loophole-registered-p (intern "loophole-1-map")
                                   (intern "loophole-1-map-state")))
    (should (string-equal (loophole-tag-string (intern "loophole-1-map"))
                          (string ?1)))
    (should-not (loophole-global-p (intern "loophole-1-map")))
    (should (equal (keymap-parent (symbol-value (intern "loophole-1-map")))
                   loophole-base-map))
    (set (intern "loophole-2-map") (make-sparse-keymap))
    (set (intern "loophole-2-map-state") nil)
    (make-variable-buffer-local (intern "loophole-2-map-state"))
    (let ((parent (make-sparse-keymap)))
      (set-keymap-parent (symbol-value (intern "loophole-2-map")) parent)
      (loophole-register (intern "loophole-2-map")
                         (intern "loophole-2-map-state"))
      (should (memq loophole-base-map
                    (keymap-parent (symbol-value (intern "loophole-2-map")))))
      (should (memq parent
                    (keymap-parent (symbol-value (intern "loophole-2-map"))))))
    (set (intern "loophole-3-map") (make-sparse-keymap))
    (set (intern "loophole-3-map-state") nil)
    (make-variable-buffer-local (intern "loophole-3-map-state"))
    (loophole-register (intern "loophole-3-map")
                       (intern "loophole-3-map-state"))
    (should-not (loophole-tag-string (intern "loophole-3-map")))
    (set (intern "loophole-4-map") (make-sparse-keymap))
    (set (intern "loophole-4-map-state") nil)
    (loophole-register (intern "loophole-4-map")
                       (intern "loophole-4-map-state")
                       nil t)
    (should (loophole-global-p (intern "loophole-4-map")))
    (set (intern "loophole-5-map") (make-sparse-keymap))
    (set (intern "loophole-5-map-state") nil)
    (make-variable-buffer-local (intern "loophole-5-map-state"))
    (loophole-register (intern "loophole-5-map")
                       (intern "loophole-5-map-state")
                       nil nil t)
    (should-not (keymap-parent (symbol-value (intern "loophole-5-map"))))
    ;; Handling parent
    (set (intern "loophole-6-map") (make-sparse-keymap))
    (set (intern "loophole-6-map-state") nil)
    (make-variable-buffer-local (intern "loophole-6-map-state"))
    (let ((parent (make-sparse-keymap)))
      (set-keymap-parent (symbol-value (intern "loophole-6-map")) parent)
      (loophole-register (intern "loophole-6-map")
                         (intern "loophole-6-map-state")
                         nil nil t)
      (should (eq parent
                  (keymap-parent (symbol-value (intern "loophole-6-map"))))))
    (set (intern "loophole-7-map") (make-sparse-keymap))
    (set (intern "loophole-7-map-state") nil)
    (make-variable-buffer-local (intern "loophole-7-map-state"))
    (set-keymap-parent (symbol-value (intern "loophole-7-map"))
                       loophole-base-map)
    (loophole-register (intern "loophole-7-map")
                       (intern "loophole-7-map-state"))
    (should (eq (keymap-parent (symbol-value (intern "loophole-7-map")))
                loophole-base-map))
    (set (intern "loophole-8-map") (make-sparse-keymap))
    (set (intern "loophole-8-map-state") nil)
    (make-variable-buffer-local (intern "loophole-8-map-state"))
    (let ((parent (make-composed-keymap (list (make-sparse-keymap)
                                              loophole-base-map))))
      (set-keymap-parent (symbol-value (intern "loophole-8-map")) parent)
      (loophole-register (intern "loophole-8-map")
                         (intern "loophole-8-map-state"))
      (should (equal (keymap-parent (symbol-value (intern "loophole-8-map")))
                     parent)))
    ;; Form storage
    (set (intern "loophole-9-map") (make-sparse-keymap))
    (set (intern "loophole-9-map-state") nil)
    (make-variable-buffer-local (intern "loophole-9-map-state"))
    (loophole-register (intern "loophole-9-map")
                       (intern "loophole-9-map-state"))
    (let ((key (vector ?a))
          (form 'ctl-x-4-map))
      (loophole-toss-binding-form key form)
      (loophole-bind-entry key (eval form)
                           (symbol-value (intern "loophole-9-map"))))
    (loophole-unregister (intern "loophole-9-map"))
    (loophole-register (intern "loophole-9-map")
                       (intern "loophole-9-map-state"))
    (let ((form-storage
           (get (intern "loophole-9-map") :loophole-form-storage)))
      (should (loophole--valid-form (intern "loophole-9-map")))
      (should (equal form-storage
                     (loophole--valid-form (intern "loophole-9-map")))))
    ;; Protected keymap
    (should (get (intern "loophole-9-map") :loophole-protected-keymap))
    (should (memq (get (intern "loophole-9-map") :loophole-protected-keymap)
                  (keymap-parent (symbol-value (intern "loophole-9-map")))))
    (should (memq loophole-base-map
                  (keymap-parent (symbol-value (intern "loophole-9-map")))))
    (set (intern "loophole-10-map") (make-sparse-keymap))
    (set (intern "loophole-10-map-state") nil)
    (make-variable-buffer-local (intern "loophole-10-map-state"))
    (loophole-register (intern "loophole-10-map")
                       (intern "loophole-10-map-state")
                       nil nil t)
    (loophole--add-protected-keymap-entry (intern "loophole-10-map")
                                          (vector ?a) (make-sparse-keymap))
    (loophole-unregister (intern "loophole-10-map"))
    (loophole-register (intern "loophole-10-map")
                       (intern "loophole-10-map-state"))
    (should (get (intern "loophole-10-map") :loophole-protected-keymap))
    (should (memq (get (intern "loophole-10-map") :loophole-protected-keymap)
                  (keymap-parent (symbol-value (intern "loophole-10-map")))))
    (should (memq loophole-base-map
                  (keymap-parent (symbol-value (intern "loophole-10-map")))))
    (set (intern "loophole-11-map") (make-sparse-keymap))
    (set (intern "loophole-11-map-state") nil)
    (make-variable-buffer-local (intern "loophole-11-map-state"))
    (let ((parent (make-sparse-keymap)))
      (set-keymap-parent (symbol-value (intern "loophole-11-map")) parent)
      (loophole-register (intern "loophole-11-map")
                         (intern "loophole-11-map-state"))
      (loophole--add-protected-keymap-entry (intern "loophole-11-map")
                                            (vector ?a) (make-sparse-keymap))
      (loophole-unregister (intern "loophole-11-map"))
      (loophole-register (intern "loophole-11-map")
                         (intern "loophole-11-map-state"))
      (should (get (intern "loophole-11-map") :loophole-protected-keymap))
      (should (memq (get (intern "loophole-11-map") :loophole-protected-keymap)
                    (keymap-parent (symbol-value (intern "loophole-11-map")))))
      (should (memq parent
                    (keymap-parent (symbol-value (intern "loophole-11-map")))))
      (should (memq loophole-base-map
                    (keymap-parent (symbol-value (intern "loophole-11-map"))))))
    (set (intern "loophole-12-map") (make-sparse-keymap))
    (set (intern "loophole-12-map-state") nil)
    (make-variable-buffer-local (intern "loophole-12-map-state"))
    (loophole-register (intern "loophole-12-map")
                       (intern "loophole-12-map-state"))
    (loophole--add-protected-keymap-entry (intern "loophole-12-map")
                                          (vector ?a) (make-sparse-keymap))
    (loophole-unregister (intern "loophole-12-map"))
    (set-keymap-parent
     (symbol-value (intern "loophole-12-map"))
     (make-composed-keymap
      (list (get (intern "loophole-12-map") :loophole-protected-keymap)
            loophole-base-map)))
    (let ((parent (keymap-parent (symbol-value (intern "loophole-12-map")))))
      (loophole-register (intern "loophole-12-map")
                         (intern "loophole-12-map-state"))
      (should (equal (keymap-parent (symbol-value (intern "loophole-12-map")))
                     parent)))
    (set (intern "loophole-13-map") (make-sparse-keymap))
    (set (intern "loophole-13-map-state") nil)
    (make-variable-buffer-local (intern "loophole-13-map-state"))
    (loophole-register (intern "loophole-13-map")
                       (intern "loophole-13-map-state"))
    (loophole--add-protected-keymap-entry (intern "loophole-13-map")
                                          (vector ?a) (make-sparse-keymap))
    (loophole-unregister (intern "loophole-13-map"))
    (put (intern "loophole-13-map") :loophole-protected-keymap nil)
    (loophole-register (intern "loophole-13-map")
                       (intern "loophole-13-map-state"))
    (should-not (get (intern "loophole-13-map") :loophole-protected-keymap))
    (should-not
     (memq (get (intern "loophole-13-map") :loophole-protected-keymap)
           (keymap-parent (symbol-value (intern "loophole-13-map")))))
    (should (memq loophole-base-map
                  (keymap-parent (symbol-value (intern "loophole-13-map")))))
    (set (intern "loophole-14-map") (make-sparse-keymap))
    (set (intern "loophole-14-map-state") nil)
    (make-variable-buffer-local (intern "loophole-14-map-state"))
    (loophole-register (intern "loophole-14-map")
                       (intern "loophole-14-map-state"))
    (loophole--add-protected-keymap-entry (intern "loophole-14-map")
                                          (vector ?a) (make-sparse-keymap))
    (loophole-unregister (intern "loophole-14-map"))
    (set-keymap-parent (symbol-value (intern "loophole-14-map"))
                       nil)
    (loophole-register (intern "loophole-14-map")
                       (intern "loophole-14-map-state"))
    (should-not (get (intern "loophole-14-map") :loophole-protected-keymap))
    (should (eq loophole-base-map
                (keymap-parent (symbol-value (intern "loophole-14-map")))))
    ;; Hook
    (set (intern "loophole-15-map") (make-sparse-keymap))
    (set (intern "loophole-15-map-state") nil)
    (make-variable-buffer-local (intern "loophole-15-map-state"))
    (let* ((registered nil)
           (equal-to-base-map nil)
           (map-variable nil)
           (loophole-after-register-functions
            (lambda (arg)
              (setq registered (loophole-registered-p arg))
              (setq equal-to-base-map (equal (keymap-parent (symbol-value arg))
                                             loophole-base-map))
              (setq map-variable arg))))
      (loophole-register (intern "loophole-15-map")
                         (intern "loophole-15-map-state"))
      (should registered)
      (should equal-to-base-map)
      (should (eq map-variable (intern "loophole-15-map"))))
    ;; Buffer local
    (set (intern "loophole-16-map") (make-sparse-keymap))
    (set (intern "loophole-16-map-state") nil)
    (make-variable-buffer-local (intern "loophole-16-map-state"))
    (with-temp-buffer
      (setq loophole--map-alist loophole--map-alist)
      (with-temp-buffer
        (setq loophole--map-alist loophole--map-alist)
        (loophole-register (intern "loophole-16-map")
                           (intern "loophole-16-map-state"))
        (should (equal (cons (intern "loophole-16-map-state")
                             (symbol-value (intern "loophole-16-map")))
                       (car (default-value 'loophole--map-alist))))
        (should (equal (cons (intern "loophole-16-map-state")
                             (symbol-value (intern "loophole-16-map")))
                       (car loophole--map-alist))))
      (should (equal (cons (intern "loophole-16-map-state")
                           (symbol-value (intern "loophole-16-map")))
                     (car loophole--map-alist))))
    (should (equal (cons (intern "loophole-16-map-state")
                         (symbol-value (intern "loophole-16-map")))
                   (car loophole--map-alist)))
    (set (intern "loophole-17-map") (make-sparse-keymap))
    (set (intern "loophole-17-map-state") nil)
    (make-variable-buffer-local (intern "loophole-17-map-state"))
    (with-temp-buffer
      (let ((buffer1 (current-buffer)))
        (set (intern "loophole-17-map-state") nil)
        (with-temp-buffer
          (let ((buffer2 (current-buffer)))
            (set (intern "loophole-17-map-state") nil)
            (setq-default loophole--buffer-list nil)
            (loophole-register (intern "loophole-17-map")
                               (intern "loophole-17-map-state"))
            (should (consp loophole--buffer-list))
            (should-not (seq-difference
                         loophole--buffer-list
                         (list buffer1 buffer2)
                         #'eq))
            (should (memq #'loophole--follow-adding-local-variable
                          (get-variable-watchers
                           (intern "loophole-17-map-state"))))))))
    (set (intern "loophole-18-map") (make-sparse-keymap))
    (set (intern "loophole-18-map-state") nil)
    (with-temp-buffer
      (set (intern "loophole-18-map-state") nil)
      (with-temp-buffer
        (set (intern "loophole-18-map-state") nil)
        (setq-default loophole--buffer-list nil)
        (loophole-register (intern "loophole-18-map")
                           (intern "loophole-18-map-state")
                           nil t)
        (should-not loophole--buffer-list)
        (should-not (memq #'loophole--follow-adding-local-variable
                          (get-variable-watchers
                           (intern "loophole-18-map-state"))))))
    ;; Prompt and forcing
    (set (intern "loophole-19-map") (make-sparse-keymap))
    (set (intern "loophole-19-map-state") nil)
    (loophole-register (intern "loophole-19-map")
                       (intern "loophole-19-map-state"))
    (should (local-variable-if-set-p (intern "loophole-19-map-state")))
    (set (intern "loophole-20-map") (make-sparse-keymap))
    (set (intern "loophole-20-map-state") nil)
    (let ((loophole-force-make-variable-buffer-local nil))
      (loophole--test-with-keyboard-events (string ?y ?e ?s ?\r)
        (loophole-register (intern "loophole-20-map")
                           (intern "loophole-20-map-state")))
      (should (local-variable-if-set-p (intern "loophole-20-map-state"))))
    (set (intern "loophole-21-map") (make-sparse-keymap))
    (set (intern "loophole-21-map-state") nil)
    (let ((loophole-force-make-variable-buffer-local nil))
      (loophole--test-with-keyboard-events (string ?n ?o ?\r)
        (should-error (loophole-register (intern "loophole-21-map")
                                         (intern "loophole-21-map-state")
                                         (string ?2 ?1))
                      :type 'user-error))
      (should-not (loophole-registered-p (intern "loophole-21-map")
                                         (intern "loophole-21-map-state")))
      (should-not (symbol-plist (intern "loophole-21-map")))
      (should-not (keymap-parent (symbol-value (intern "loophole-21-map"))))
      (should-not (local-variable-if-set-p (intern "loophole-21-map-state"))))
    (set (intern "loophole-22-map") (make-sparse-keymap))
    (set (intern "loophole-22-map-state") 1)
    (fset (intern "loophole-22-map-state") #'(lambda () 2))
    (put (intern "loophole-22-map-state") :loophole-test 3)
    (make-variable-buffer-local (intern "loophole-22-map-state"))
    (loophole--test-with-keyboard-events (string ?y ?e ?s ?\r)
      (loophole-register (intern "loophole-22-map")
                         (intern "loophole-22-map-state")
                         nil t))
    (should-not (local-variable-if-set-p (intern "loophole-22-map-state")))
    (should (eql (symbol-value (intern "loophole-22-map-state")) 1))
    (should (equal (symbol-function (intern "loophole-22-map-state"))
                   #'(lambda () 2)))
    (should (eql (get (intern "loophole-22-map-state") :loophole-test) 3))
    (set (intern "loophole-23-map") (make-sparse-keymap))
    (set (intern "loophole-23-map-state") 1)
    (fset (intern "loophole-23-map-state") #'(lambda () 2))
    (put (intern "loophole-23-map-state") :loophole-test 3)
    (make-variable-buffer-local (intern "loophole-23-map-state"))
    (loophole--test-with-keyboard-events (string ?n ?o ?\r)
      (should-error (loophole-register (intern "loophole-23-map")
                                       (intern "loophole-23-map-state")
                                       (string ?2 ?3) t)
                    :type 'user-error))
    (should-not (loophole-registered-p (intern "loophole-23-map")
                                       (intern "loophole-23-map-state")))
    (should-not (symbol-plist (intern "loophole-23-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-23-map"))))
    (should (local-variable-if-set-p (intern "loophole-23-map-state")))
    (should (eql (symbol-value (intern "loophole-23-map-state")) 1))
    (should (equal (symbol-function (intern "loophole-23-map-state"))
                   #'(lambda () 2)))
    (should (equal (symbol-plist (intern "loophole-23-map-state"))
                   '(:loophole-test 3)))
    (set (intern "loophole-24-map") (make-sparse-keymap))
    (set (intern "loophole-24-map-state") 1)
    (fset (intern "loophole-24-map-state") #'(lambda () 2))
    (put (intern "loophole-24-map-state") :loophole-test 3)
    (make-variable-buffer-local (intern "loophole-24-map-state"))
    (let ((loophole-force-unintern t))
      (loophole-register (intern "loophole-24-map")
                         (intern "loophole-24-map-state")
                         nil t))
    (should-not (local-variable-if-set-p (intern "loophole-24-map-state")))
    (should (eql (symbol-value (intern "loophole-24-map-state")) 1))
    (should (equal (symbol-function (intern "loophole-24-map-state"))
                   #'(lambda () 2)))
    (should (eql (get (intern "loophole-24-map-state") :loophole-test) 3))
    ;; Error
    (set (intern "loophole-25-map") (make-sparse-keymap))
    (set (intern "loophole-25-map-state") nil)
    (make-variable-buffer-local (intern "loophole-25-map-state"))
    (should-error (loophole-register
                   0 (intern "loophole-25-map-state"))
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   1.0 (intern "loophole-25-map-state"))
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   ?c (intern "loophole-25-map-state"))
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (cons 0 0) (intern "loophole-25-map-state"))
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (string ?s) (intern "loophole-25-map-state"))
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (vector ?v) (intern "loophole-25-map-state"))
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (intern "loophole-25-map") 0)
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (intern "loophole-25-map") 1.0)
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (intern "loophole-25-map") ?c)
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (intern "loophole-25-map") (cons 0 0))
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (intern "loophole-25-map") (string ?s))
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (intern "loophole-25-map") (vector ?v))
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (intern "loophole-25-map") (intern "loophole-25-map-state")
                   0)
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (intern "loophole-25-map") (intern "loophole-25-map-state")
                   1.0)
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (intern "loophole-25-map") (intern "loophole-25-map-state")
                   ?c)
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (intern "loophole-25-map") (intern "loophole-25-map-state")
                   (intern "s"))
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (intern "loophole-25-map") (intern "loophole-25-map-state")
                   (cons 0 0))
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (should-error (loophole-register
                   (intern "loophole-25-map") (intern "loophole-25-map-state")
                   (vector ?v))
                  :type 'wrong-type-argument)
    (should-not (loophole-registered-p (intern "loophole-25-map")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-25-map"))))
    (set (intern "loophole-25-map-dummy") 0)
    (should-error (loophole-register
                   (intern "loophole-25-map-dummy")
                   (intern "loophole-25-map-state"))
                  :type 'error  :exclude-subtypes t)
    (should-not (loophole-registered-p (intern "loophole-25-map-dummy")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map-dummy")))
    (set (intern "loophole-25-map-dummy") 1.0)
    (should-error (loophole-register
                   (intern "loophole-25-map-dummy")
                   (intern "loophole-25-map-state"))
                  :type 'error  :exclude-subtypes t)
    (should-not (loophole-registered-p (intern "loophole-25-map-dummy")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map-dummy")))
    (set (intern "loophole-25-map-dummy") ?c)(should-error (loophole-register
                   (intern "loophole-25-map-dummy")
                   (intern "loophole-25-map-state"))
                  :type 'error  :exclude-subtypes t)
    (should-not (loophole-registered-p (intern "loophole-25-map-dummy")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map-dummy")))
    (set (intern "loophole-25-map-dummy") (intern "s"))
    (should-error (loophole-register
                   (intern "loophole-25-map-dummy")
                   (intern "loophole-25-map-state"))
                  :type 'error  :exclude-subtypes t)
    (should-not (loophole-registered-p (intern "loophole-25-map-dummy")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map-dummy")))
    (set (intern "loophole-25-map-dummy") (cons 0 0))
    (should-error (loophole-register
                   (intern "loophole-25-map-dummy")
                   (intern "loophole-25-map-state"))
                  :type 'error  :exclude-subtypes t)
    (should-not (loophole-registered-p (intern "loophole-25-map-dummy")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map-dummy")))
    (set (intern "loophole-25-map-dummy") (string ?s))
    (should-error (loophole-register
                   (intern "loophole-25-map-dummy")
                   (intern "loophole-25-map-state"))
                  :type 'error  :exclude-subtypes t)
    (should-not (loophole-registered-p (intern "loophole-25-map-dummy")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map-dummy")))
    (set (intern "loophole-25-map-dummy") (vector ?v))
    (should-error (loophole-register
                   (intern "loophole-25-map-dummy")
                   (intern "loophole-25-map-state"))
                  :type 'error  :exclude-subtypes t)
    (should-not (loophole-registered-p (intern "loophole-25-map-dummy")
                                       (intern "loophole-25-map-state")))
    (should-not (symbol-plist (intern "loophole-25-map-dummy")))
    (set (intern "loophole-26-map") (make-sparse-keymap))
    (set (intern "loophole-26-map-state") nil)
    (make-variable-buffer-local (intern "loophole-26-map-state"))
    (loophole-register
     (intern "loophole-26-map") (intern "loophole-26-map-state"))
    (set (intern "loophole-27-map") (make-sparse-keymap))
    (set (intern "loophole-27-map-state") nil)
    (make-variable-buffer-local (intern "loophole-27-map-state"))
    (should-error (loophole-register
                   (intern "loophole-26-map") (intern "loophole-26-map-state"))
                  :type 'user-error)
    (let ((plist (symbol-plist (intern "loophole-26-map")))
          (parent (keymap-parent (symbol-value (intern "loophole-26-map")))))
      (should-error (loophole-register (intern "loophole-26-map")
                                       (intern "loophole-27-map-state"))
                    :type 'user-error)
      (should-not (loophole-registered-p (intern "loophole-26-map")
                                         (intern "loophole-27-map-state")))
      (should (equal (symbol-plist (intern "loophole-26-map"))
                     plist))
      (should (equal (keymap-parent (symbol-value (intern "loophole-26-map")))
                     parent)))
    (should-error (loophole-register
                   (intern "loophole-27-map") (intern "loophole-26-map-state"))
                  :type 'user-error)
    (should-not (loophole-registered-p (intern "loophole-27-map")
                                       (intern "loophole-26-map-state")))
    (should-not (symbol-plist (intern "loophole-27-map")))
    (should-not (keymap-parent (symbol-value (intern "loophole-27-map"))))
    (set (intern "loophole-28-map") (symbol-value (intern "loophole-26-map")))
    (set (intern "loophole-28-map-state") nil)
    (make-variable-buffer-local (intern "loophole-28-map-state"))
    (let ((parent (keymap-parent (symbol-value (intern "loophole-28-map")))))
      (should-error (loophole-register (intern "loophole-28-map")
                                       (intern "loophole-28-map-state"))
                    :type 'user-error)
      (should-not (loophole-registered-p (intern "loophole-28-map")
                                         (intern "loophole-28-map-state")))
      (should-not (symbol-plist (intern "loophole-28-map")))
      (should (equal (keymap-parent (symbol-value (intern "loophole-28-map")))
                     parent)))
    ;; Interactive spec
    (let* ((oba (obarray-make))
           (return-args (lambda (&rest args) args))
           (replace-obarray-for-completing-read
            (lambda (args)
              (cons (car args)
                    (cons oba (nthcdr 2 args)))))
           (replace-obarray-for-intern
            (lambda (args)
              (if (cadr args)
                  args
                (let ((name (car args)))
                  (if (or (member
                           name
                           (mapcar
                            #'symbol-name
                            loophole--test-barriered-symbol-list))
                          (string-prefix-p "loophole" name))
                      (list name oba)
                    args))))))
      (unwind-protect
          (progn
            (advice-add 'loophole-register :override return-args)
            (advice-add 'completing-read
                        :filter-args replace-obarray-for-completing-read)
            (advice-add 'intern :filter-args replace-obarray-for-intern)
            (set (intern "loophole-29-map" oba) (make-sparse-keymap))
            (set (intern "loophole-29-map-state" oba) nil)
            (loophole--test-with-keyboard-events
                "loophole-29-maploophole-29-map-state"
              (let ((args (call-interactively #'loophole-register)))
                (should (eq (nth 0 args) (intern "loophole-29-map" oba)))
                (should (eq (nth 1 args) (intern "loophole-29-map-state" oba)))
                (should (equal (nth 2 args) (string ?2 ?9)))
                (should-not (nth 3 args))
                (should-not (nth 4 args))))
            (unless noninteractive
              (should (catch 'loophole-test-register
                      (run-with-timer
                       (* 0.2 loophole--test-wait-time)
                       nil
                       (lambda () (throw 'loophole-test-register t)))
                      (loophole--test-with-keyboard-events
                          "loophole-30-maploophole-30-map-state"
                        (call-interactively #'loophole-register))
                      nil)))
            (let ((loophole-make-register-always-read-tag t))
              (loophole--test-with-keyboard-events
                  "loophole-29-maploophole-29-map-state30"
                (let ((args (call-interactively #'loophole-register)))
                  (should (eq (nth 0 args) (intern "loophole-29-map" oba)))
                  (should (eq (nth 1 args)
                              (intern "loophole-29-map-state" oba)))
                  (should (equal (nth 2 args) (string ?3 ?0)))
                  (should-not (nth 3 args))
                  (should-not (nth 4 args)))))
            (let ((current-prefix-arg '(4)))
              (loophole--test-with-keyboard-events
                  "loophole-29-maploophole-29-map-state30yy"
                (let ((args (call-interactively #'loophole-register)))
                  (should (eq (nth 0 args) (intern "loophole-29-map" oba)))
                  (should (eq (nth 1 args)
                              (intern "loophole-29-map-state" oba)))
                  (should (equal (nth 2 args) (string ?3 ?0)))
                  (should (nth 3 args))
                  (should (nth 4 args)))))
            (let ((current-prefix-arg 1))
              (loophole--test-with-keyboard-events
                  "loophole-29-maploophole-29-map-state30ny"
                (let ((args (call-interactively #'loophole-register)))
                  (should (eq (nth 0 args) (intern "loophole-29-map" oba)))
                  (should (eq (nth 1 args)
                              (intern "loophole-29-map-state" oba)))
                  (should (equal (nth 2 args) (string ?3 ?0)))
                  (should-not (nth 3 args))
                  (should (nth 4 args)))))
            (let ((current-prefix-arg '-))
              (loophole--test-with-keyboard-events
                  "loophole-29-maploophole-29-map-state30yn"
                (let ((args (call-interactively #'loophole-register)))
                  (should (eq (nth 0 args) (intern "loophole-29-map" oba)))
                  (should (eq (nth 1 args)
                              (intern "loophole-29-map-state" oba)))
                  (should (equal (nth 2 args) (string ?3 ?0)))
                  (should (nth 3 args))
                  (should-not (nth 4 args))))))
        (advice-remove 'intern replace-obarray-for-intern)
        (advice-remove 'completing-read replace-obarray-for-completing-read)
        (advice-remove 'loophole-register return-args)))))

(ert-deftest loophole-test-unregister ()
  "Test for `loophole-unregister'."
  (loophole--test-with-pseudo-environment
    ;; Basic behavior
    (set (intern "loophole-1-map") (make-sparse-keymap))
    (set (intern "loophole-1-map-state") nil)
    (make-variable-buffer-local (intern "loophole-1-map-state"))
    (loophole-register (intern "loophole-1-map")
                       (intern "loophole-1-map-state")
                       (string ?1))
    (loophole-unregister (intern "loophole-1-map"))
    (should-not (loophole-registered-p (intern "loophole-1-map")
                                       (intern "loophole-1-map-state")))
    (should-not (get (intern "loophole-1-map") :loophole-tag))
    (should-not (keymap-parent (symbol-value (intern "loophole-1-map"))))
    (let ((key (vector ?a))
          (form 'ctl-x-4-map))
      (set (intern "loophole-2-map") (make-sparse-keymap))
      (set (intern "loophole-2-map-state") nil)
      (make-variable-buffer-local (intern "loophole-2-map-state"))
      (loophole-register (intern "loophole-2-map")
                         (intern "loophole-2-map-state"))
      (loophole-toss-binding-form key form)
      (loophole-bind-entry key (eval form)
                           (symbol-value (intern "loophole-2-map")))
      (loophole-unregister (intern "loophole-2-map"))
      (should (member (cons key form)
                      (get (intern "loophole-2-map") :loophole-form-storage)))
      (should (eq (lookup-key
                   (get (intern "loophole-2-map") :loophole-protected-keymap)
                   key)
                  (eval form)))
      (should (eq (get (intern "loophole-2-map") :loophole-protected-keymap)
                  (keymap-parent (symbol-value (intern "loophole-2-map")))))
      (set (intern "loophole-3-map") (make-sparse-keymap))
      (set (intern "loophole-3-map-state") nil)
      (make-variable-buffer-local (intern "loophole-3-map-state"))
      (loophole-register (intern "loophole-3-map")
                         (intern "loophole-3-map-state"))
      (loophole-toss-binding-form key form)
      (loophole-bind-entry key (eval form)
                           (symbol-value (intern "loophole-3-map")))
      (loophole-unregister (intern "loophole-3-map") t)
      (should-not (get (intern "loophole-3-map") :loophole-form-storage))
      (should-not (get (intern "loophole-3-map") :loophole-protected-keymap))
      (should (eq (lookup-key
                   (keymap-parent (symbol-value (intern "loophole-3-map"))) key)
                  (eval form))))
    ;; Handling parent
    (let ((parent (make-sparse-keymap)))
      (set (intern "loophole-4-map") (make-sparse-keymap))
      (set (intern "loophole-4-map-state") nil)
      (make-variable-buffer-local (intern "loophole-4-map-state"))
      (set-keymap-parent (symbol-value (intern "loophole-4-map")) parent)
      (loophole-register (intern "loophole-4-map")
                         (intern "loophole-4-map-state"))
      (loophole-unregister (intern "loophole-4-map"))
      (should (eq (keymap-parent (symbol-value (intern "loophole-4-map")))
                  parent)))
    (let ((parent (make-sparse-keymap)))
      (set (intern "loophole-5-map") (make-sparse-keymap))
      (set (intern "loophole-5-map-state") nil)
      (make-variable-buffer-local (intern "loophole-5-map-state"))
      (set-keymap-parent (symbol-value (intern "loophole-5-map")) parent)
      (loophole-register (intern "loophole-5-map")
                         (intern "loophole-5-map-state")
                         nil nil t)
      (loophole-unregister (intern "loophole-5-map"))
      (should (eq (keymap-parent (symbol-value (intern "loophole-5-map")))
                  parent)))
    (let ((parent (make-sparse-keymap))
          (key (vector ?a))
          (form 'ctl-x-4-map))
      (set (intern "loophole-6-map") (make-sparse-keymap))
      (set (intern "loophole-6-map-state") nil)
      (make-variable-buffer-local (intern "loophole-6-map-state"))
      (set-keymap-parent (symbol-value (intern "loophole-6-map")) parent)
      (loophole-register (intern "loophole-6-map")
                         (intern "loophole-6-map-state"))
      (loophole-toss-binding-form key form)
      (loophole-bind-entry key (eval form)
                           (symbol-value (intern "loophole-6-map")))
      (loophole-unregister (intern "loophole-6-map"))
      (should (keymap-parent (symbol-value (intern "loophole-6-map"))))
      (should-not (seq-difference
                   (cdr
                    (keymap-parent (symbol-value (intern "loophole-6-map"))))
                   (list parent
                         (get (intern "loophole-6-map")
                              :loophole-protected-keymap))
                   #'eq)))
    (with-temp-buffer
      (setq loophole--map-alist loophole--map-alist)
      (with-temp-buffer
        (setq loophole--map-alist loophole--map-alist)
        (set (intern "loophole-7-map") (make-sparse-keymap))
        (set (intern "loophole-7-map-state") nil)
        (make-variable-buffer-local (intern "loophole-7-map-state"))
        (loophole-register (intern "loophole-7-map")
                           (intern "loophole-7-map-state"))
        (loophole-unregister (intern "loophole-7-map"))
        (should-not loophole--map-alist))
      (should-not loophole--map-alist))
    (should-not (default-value 'loophole--map-alist))
    ;; Stopping editing
    (set (intern "loophole-8-map") (make-sparse-keymap))
    (set (intern "loophole-8-map-state") nil)
    (make-variable-buffer-local (intern "loophole-8-map-state"))
    (loophole-register (intern "loophole-8-map")
                       (intern "loophole-8-map-state"))
    (set (intern "loophole-9-map") (make-sparse-keymap))
    (set (intern "loophole-9-map-state") nil)
    (make-variable-buffer-local (intern "loophole-9-map-state"))
    (loophole-register (intern "loophole-9-map")
                       (intern "loophole-9-map-state"))
    (with-temp-buffer
      (loophole-start-editing (intern "loophole-8-map"))
      (with-temp-buffer
        (loophole-start-editing (intern "loophole-9-map"))
        (with-temp-buffer
          (loophole-start-editing (intern "loophole-8-map"))
          (loophole-unregister (intern "loophole-8-map"))
          (should-not (loophole-editing)))
        (should (eq (loophole-editing) (intern "loophole-9-map"))))
      (should-not (loophole-editing)))
    (should-not (loophole-editing))
    (loophole-globalize-editing)
    (loophole-start-editing (intern "loophole-9-map"))
    (loophole-unregister (intern "loophole-9-map"))
    (should-not (loophole-editing))
    ;; Hook
    (set (intern "loophole-10-map") (make-sparse-keymap))
    (set (intern "loophole-10-map-state") nil)
    (make-variable-buffer-local (intern "loophole-10-map-state"))
    (loophole-register (intern "loophole-10-map")
                       (intern "loophole-10-map-state"))
    (let* ((registered t)
           (parent t)
           (map-variable nil)
           (loophole-after-unregister-functions
            (lambda (arg)
              (setq registered (loophole-registered-p arg))
              (setq parent (keymap-parent (symbol-value arg)))
              (setq map-variable arg))))
      (loophole-unregister (intern "loophole-10-map"))
      (should-not registered)
      (should-not parent)
      (should (eq map-variable (intern "loophole-10-map"))))
    ;; Variable-watcher and hook for loophole-mode
    (setq-default loophole--buffer-list nil)
    (set (intern "loophole-16-map") (make-sparse-keymap))
    (set (intern "loophole-16-map-state") nil)
    (make-variable-buffer-local (intern "loophole-16-map-state"))
    (loophole-register (intern "loophole-16-map")
                       (intern "loophole-16-map-state"))
    (with-temp-buffer
      (set (intern "loophole-16-map-state") t)
      (with-temp-buffer
        (set (intern "loophole-16-map-state") t)
        (loophole-unregister (intern "loophole-16-map"))
        (should-not (memq #'loophole--follow-killing-local-variable
                          change-major-mode-hook))
        (should-not (memq #'loophole--follow-killing-local-variable
                          kill-buffer-hook)))
      (should-not (memq #'loophole--follow-killing-local-variable
                        change-major-mode-hook))
      (should-not (memq #'loophole--follow-killing-local-variable
                        kill-buffer-hook)))
    (should-not (memq #'loophole--follow-adding-local-variable
                      (get-variable-watchers (intern "loophole-16-map-state"))))
    ;; Error
    (set (intern "loophole-17-map") (make-sparse-keymap))
    (set (intern "loophole-17-map-state") nil)
    (make-variable-buffer-local (intern "loophole-17-map-state"))
    (loophole-register (intern "loophole-17-map")
                       (intern "loophole-17-map-state"))
    (loophole-start-editing (intern "loophole-17-map"))
    (let ((plist (symbol-plist (intern "loophole-17-map")))
          (parent (keymap-parent (symbol-value (intern "loophole-17-map")))))
      (should-error (loophole-unregister 0) :type 'wrong-type-argument)
      (should (loophole-registered-p (intern "loophole-17-map")
                                     (intern "loophole-17-map-state")))
      (should (eq (symbol-plist (intern "loophole-17-map"))
                  plist))
      (should (eq (keymap-parent (symbol-value (intern "loophole-17-map")))
                  parent))
      (should (eq (loophole-editing) (intern "loophole-17-map")))
      (should-error (loophole-unregister 1.0) :type 'wrong-type-argument)
      (should (loophole-registered-p (intern "loophole-17-map")
                                     (intern "loophole-17-map-state")))
      (should (eq (symbol-plist (intern "loophole-17-map"))
                  plist))
      (should (eq (keymap-parent (symbol-value (intern "loophole-17-map")))
                  parent))
      (should (eq (loophole-editing) (intern "loophole-17-map")))
      (should-error (loophole-unregister ?c) :type 'wrong-type-argument)
      (should (loophole-registered-p (intern "loophole-17-map")
                                     (intern "loophole-17-map-state")))
      (should (eq (symbol-plist (intern "loophole-17-map"))
                  plist))
      (should (eq (keymap-parent (symbol-value (intern "loophole-17-map")))
                  parent))
      (should (eq (loophole-editing) (intern "loophole-17-map")))
      (should-error (loophole-unregister (cons 0 0)) :type 'wrong-type-argument)
      (should (loophole-registered-p (intern "loophole-17-map")
                                     (intern "loophole-17-map-state")))
      (should (eq (symbol-plist (intern "loophole-17-map"))
                  plist))
      (should (eq (keymap-parent (symbol-value (intern "loophole-17-map")))
                  parent))
      (should (eq (loophole-editing) (intern "loophole-17-map")))
      (should-error (loophole-unregister (string ?s))
                    :type 'wrong-type-argument)
      (should (loophole-registered-p (intern "loophole-17-map")
                                     (intern "loophole-17-map-state")))
      (should (eq (symbol-plist (intern "loophole-17-map"))
                  plist))
      (should (eq (keymap-parent (symbol-value (intern "loophole-17-map")))
                  parent))
      (should (eq (loophole-editing) (intern "loophole-17-map")))
      (should-error (loophole-unregister (vector ?v))
                    :type 'wrong-type-argument)
      (should (loophole-registered-p (intern "loophole-17-map")
                                     (intern "loophole-17-map-state")))
      (should (eq (symbol-plist (intern "loophole-17-map"))
                  plist))
      (should (eq (keymap-parent (symbol-value (intern "loophole-17-map")))
                  parent))
      (should (eq (loophole-editing) (intern "loophole-17-map")))
      (set (intern "loophole-18-map") (make-sparse-keymap))
      (set (intern "loophole-18-map-state") nil)
      (make-variable-buffer-local (intern "loophole-18-map-state"))
      (should-error (loophole-unregister (intern "loophole-18-map"))
                    :type 'user-error)
      (should (eq (symbol-plist (intern "loophole-17-map"))
                  plist))
      (should (eq (keymap-parent (symbol-value (intern "loophole-17-map")))
                  parent))
      (should (eq (loophole-editing) (intern "loophole-17-map"))))
    ;; Interactive spec
    (set (intern "loophole-19-map") (make-sparse-keymap))
    (set (intern "loophole-19-map-state") nil)
    (make-variable-buffer-local (intern "loophole-19-map-state"))
    (loophole-register (intern "loophole-19-map")
                       (intern "loophole-19-map-state"))
    (let ((return-args (lambda (&rest args) args)))
      (unwind-protect
          (progn
            (advice-add 'loophole-unregister :override return-args)
            (loophole--test-with-keyboard-events "loophole-19-map"
              (let ((args (call-interactively #'loophole-unregister)))
                (should (eq (nth 0 args) (intern "loophole-19-map")))
                (should-not (nth 1 args))))
            (unless noninteractive
              (should (catch 'loophole-test-unregister
                        (run-with-timer
                         (* 0.2 loophole--test-wait-time)
                         nil
                         (lambda () (throw 'loophole-test-unregister t)))
                        (loophole--test-with-keyboard-events
                            "loophole-20-map"
                          (call-interactively #'loophole-unregister))
                        nil)))
            (let ((current-prefix-arg '(4)))
              (loophole--test-with-keyboard-events "loophole-19-map"
                (let ((args (call-interactively #'loophole-unregister)))
                  (should (eq (nth 0 args) (intern "loophole-19-map")))
                  (should (nth 1 args)))))
            (let ((current-prefix-arg 1))
              (loophole--test-with-keyboard-events "loophole-19-map"
                (let ((args (call-interactively #'loophole-unregister)))
                  (should (eq (nth 0 args) (intern "loophole-19-map")))
                  (should (nth 1 args)))))
            (let ((current-prefix-arg '-))
              (loophole--test-with-keyboard-events "loophole-19-map"
                (let ((args (call-interactively #'loophole-unregister)))
                  (should (eq (nth 0 args) (intern "loophole-19-map")))
                  (should (nth 1 args))))))
        (advice-remove 'loophole-unregister return-args)))))

(ert-deftest loophole-test-prioritize ()
  "Test for `loophole-prioritize'."
  (loophole--test-with-pseudo-environment
    (loophole--test-set-pseudo-map-alist)
    (with-temp-buffer
      (loophole-prioritize (intern "loophole-test-a-map"))
      (should (equal (mapcar #'car loophole--map-alist)
                     (list (intern "loophole-test-a-map-state")
                           (intern "loophole-1-map-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-b-map-state")
                           (intern "loophole-state"))))
      (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                     (list (intern "loophole-test-a-map-state")
                           (intern "loophole-1-map-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-b-map-state")
                           (intern "loophole-state"))))
      (with-temp-buffer
        (loophole-prioritize (intern "loophole-2-map"))
        (should (equal (mapcar #'car loophole--map-alist)
                       (list (intern "loophole-2-map-state")
                             (intern "loophole-test-a-map-state")
                             (intern "loophole-1-map-state")
                             (intern "loophole-test-b-map-state")
                             (intern "loophole-state"))))
        (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                       (list (intern "loophole-2-map-state")
                             (intern "loophole-test-a-map-state")
                             (intern "loophole-1-map-state")
                             (intern "loophole-test-b-map-state")
                             (intern "loophole-state")))))
      (should (equal (mapcar #'car loophole--map-alist)
                     (list (intern "loophole-test-a-map-state")
                           (intern "loophole-1-map-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-b-map-state")
                           (intern "loophole-state"))))
      (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                     (list (intern "loophole-2-map-state")
                           (intern "loophole-test-a-map-state")
                           (intern "loophole-1-map-state")
                           (intern "loophole-test-b-map-state")
                           (intern "loophole-state")))))
    (should (equal (mapcar #'car loophole--map-alist)
                   (list (intern "loophole-1-map-state")
                         (intern "loophole-2-map-state")
                         (intern "loophole-test-a-map-state")
                         (intern "loophole-test-b-map-state")
                         (intern "loophole-state"))))
    (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                   (list (intern "loophole-2-map-state")
                         (intern "loophole-test-a-map-state")
                         (intern "loophole-1-map-state")
                         (intern "loophole-test-b-map-state")
                         (intern "loophole-state"))))
    (with-temp-buffer
      (loophole-prioritize (intern "loophole") 'local)
      (should (equal (mapcar #'car loophole--map-alist)
                     (list (intern "loophole-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-a-map-state")
                           (intern "loophole-1-map-state")
                           (intern "loophole-test-b-map-state"))))
      (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                     (list (intern "loophole-2-map-state")
                           (intern "loophole-test-a-map-state")
                           (intern "loophole-1-map-state")
                           (intern "loophole-test-b-map-state")
                           (intern "loophole-state"))))
      (loophole-prioritize (intern "loophole-test-b-map") 'default)
      (should (equal (mapcar #'car loophole--map-alist)
                     (list (intern "loophole-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-a-map-state")
                           (intern "loophole-1-map-state")
                           (intern "loophole-test-b-map-state"))))
      (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                     (list (intern "loophole-test-b-map-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-a-map-state")
                           (intern "loophole-1-map-state")
                           (intern "loophole-state")))))
    (with-temp-buffer
      (let* ((local-is-prioritized t)
             (default-is-prioritized t)
             (map-variable nil)
             (loophole-after-prioritize-functions
              (lambda (arg)
                (setq local-is-prioritized
                      (equal (mapcar #'car loophole--map-alist)
                             (list (intern "loophole-1-map-state")
                                   (intern "loophole-test-b-map-state")
                                   (intern "loophole-2-map-state")
                                   (intern "loophole-test-a-map-state")
                                   (intern "loophole-state"))))
                (setq default-is-prioritized
                      (equal (mapcar #'car (default-value 'loophole--map-alist))
                             (list (intern "loophole-1-map-state")
                                   (intern "loophole-test-b-map-state")
                                   (intern "loophole-2-map-state")
                                   (intern "loophole-test-a-map-state")
                                   (intern "loophole-state"))))
                (setq map-variable arg))))
        (loophole-prioritize (intern "loophole-1-map"))
        (should local-is-prioritized)
        (should default-is-prioritized)
        (should (eq map-variable (intern "loophole-1-map")))))
    (with-temp-buffer
      (should-error (loophole-prioritize (intern "loophole-3-map"))
                    :type 'user-error)
      (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                     (list (intern "loophole-1-map-state")
                           (intern "loophole-test-b-map-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-a-map-state")
                           (intern "loophole-state"))))
      (should-not (local-variable-p 'loophole--map-alist)))
    (with-temp-buffer
      (should-error (loophole-prioritize nil) :type 'user-error)
      (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                     (list (intern "loophole-1-map-state")
                           (intern "loophole-test-b-map-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-a-map-state")
                           (intern "loophole-state"))))
      (should-not (local-variable-p 'loophole--map-alist)))
    (with-temp-buffer
      (should-error (loophole-prioritize 0) :type 'wrong-type-argument)
      (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                     (list (intern "loophole-1-map-state")
                           (intern "loophole-test-b-map-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-a-map-state")
                           (intern "loophole-state"))))
      (should-not (local-variable-p 'loophole--map-alist)))
    (with-temp-buffer
      (should-error (loophole-prioritize 1.0) :type 'wrong-type-argument)
      (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                     (list (intern "loophole-1-map-state")
                           (intern "loophole-test-b-map-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-a-map-state")
                           (intern "loophole-state"))))
      (should-not (local-variable-p 'loophole--map-alist)))
    (with-temp-buffer
      (should-error (loophole-prioritize ?c) :type 'wrong-type-argument)
      (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                     (list (intern "loophole-1-map-state")
                           (intern "loophole-test-b-map-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-a-map-state")
                           (intern "loophole-state"))))
      (should-not (local-variable-p 'loophole--map-alist)))
    (with-temp-buffer
      (should-error (loophole-prioritize (cons 0 0)) :type 'wrong-type-argument)
      (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                     (list (intern "loophole-1-map-state")
                           (intern "loophole-test-b-map-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-a-map-state")
                           (intern "loophole-state"))))
      (should-not (local-variable-p 'loophole--map-alist)))
    (with-temp-buffer
      (should-error (loophole-prioritize (string ?s))
                    :type 'wrong-type-argument)
      (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                     (list (intern "loophole-1-map-state")
                           (intern "loophole-test-b-map-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-a-map-state")
                           (intern "loophole-state"))))
      (should-not (local-variable-p 'loophole--map-alist)))
    (with-temp-buffer
      (should-error (loophole-prioritize (vector ?v))
                    :type 'wrong-type-argument)
      (should (equal (mapcar #'car (default-value 'loophole--map-alist))
                     (list (intern "loophole-1-map-state")
                           (intern "loophole-test-b-map-state")
                           (intern "loophole-2-map-state")
                           (intern "loophole-test-a-map-state")
                           (intern "loophole-state"))))
      (should-not (local-variable-p 'loophole--map-alist)))
    (let ((return-args (lambda (&rest args) args)))
      (unwind-protect
          (progn
            (advice-add 'loophole-prioritize :override return-args)
            (loophole--test-with-keyboard-events "loophole-2-map"
              (let ((args (call-interactively #'loophole-prioritize)))
                (should (eq (nth 0 args) (intern "loophole-2-map")))
                (should-not (nth 1 args))))
            (unless noninteractive
              (should (catch 'loophole-test-unregister
                        (run-with-timer
                         (* 0.2 loophole--test-wait-time)
                         nil
                         (lambda () (throw 'loophole-test-unregister t)))
                        (loophole--test-with-keyboard-events
                            "loophole-4-map"
                          (call-interactively #'loophole-prioritize))
                        nil))))
        (advice-remove 'loophole-prioritize return-args)))))

(provide 'loophole-tests)

;;; loophole-tests.el ends here
