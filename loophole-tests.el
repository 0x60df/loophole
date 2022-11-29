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

(define-error 'loohpole-test-error "Loophole test error" 'error)

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
Bound entry is key-binding currently valid or undefined
if specified events are not bound to valid command.
However, if any form in BODY invokes minibuffer, transient
key bindings are disable.  After the form exit minibuffer,
transient binding are re-enabled.
If keyboard-events is a list, its elements are bound to
`overriding-terminal-local-map' individually.

If KEYBOARD-EVENTS is something invalid and test is not
finished even after `loophole--test-wait-time' is spent,
`loophole-test-error' is signaled."
  (declare (debug t) (indent 1))
  (let ((exit-function (make-symbol "exit-function"))
        (enter-transient-map (make-symbol "enter-transient-map"))
        (exit-transient-map (make-symbol "exit-transient-map"))
        (timer (make-symbol "timer")))
    `(let ((unread-command-events (if (listp ,keyboard-events)
                                      (apply #'append
                                               (mapcar #'listify-key-sequence
                                                         ,keyboard-events))
                                    (listify-key-sequence ,keyboard-events)))
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
              (,timer nil))
         (unwind-protect
             (progn
               (funcall ,enter-transient-map)
               (add-hook 'minibuffer-setup-hook ,exit-transient-map)
               (add-hook 'minibuffer-exit-hook ,enter-transient-map)
               (setq ,timer
                     (run-with-timer
                      loophole--test-wait-time nil
                      (lambda ()
                        (signal
                         'loophole-test-error
                         (list "Test with keyboard events is timed out")))))
               ,@body)
           (if (timerp ,timer) (cancel-timer ,timer))
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
  (loophole--test-with-pseudo-environment
    (should (loophole-key-equal (string ?a) (vector ?a)))
    (should-not (loophole-key-equal (string ?A) (vector ?\S-a)))
    (should (loophole-key-equal (string ?\C-a) (vector ?\C-a)))
    (should (loophole-key-equal (string ?\e ?a) (vector ?\e ?a)))
    (should (loophole-key-equal (string ?\e ?a) (string (+ ?a (lsh 2 6)))))
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
                  :type 'wrong-type-argument)))

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
      ;; Cannot use C-x or ESC for this test becase they are defined in
      ;; key-translate-table, function-key-map, and input-decode-map.
      ;; See comment in `read-key' srouce for details.
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

(provide 'loophole-tests)

;;; loophole-tests.el ends here

