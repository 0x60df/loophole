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

;; Test tools

(defvar loophole--test-barriered-symbol-list nil
  "List of symbols which is barriered in testing environment.
In `loophole--test-with-pseudo-environment', `intern' and
`unintern' applied to the symbols listed in this variable
or the symbols prefixed by loophole are deflected to
temporary `obarray'.
Note that, if obarray is specified explicitly for
 `intern' and `unintern', deflection does not performed.")

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
              `(loophole--map-alist
                loophole--editing
                loophole--timer-alist
                loophole--editing-timer
                ,@(seq-filter #'local-variable-if-set-p
                              (mapcar #'car loophole--map-alist))))
             ,local-variable-alist)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (let (list)
               (dolist (variable ,local-variable-if-set-list)
                 (if (local-variable-p variable)
                     (push `(,variable . ,(symbol-value variable)) list)))
               (if list
                   (push `(,buffer . ,list) ,local-variable-alist)))))
         (push `(nil . ,(mapcar (lambda (variable)
                                  `(,variable . ,(default-value variable)))
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
                   (dolist (variable ,local-variable-if-set-list)
                     (remove-variable-watcher
                      variable #'loophole--follow-adding-local-variable))
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
                   (setq-default loophole--map-alist nil)
                   (setq-default loophole--editing t)
                   (setq-default loophole--timer-alist nil)
                   (setq-default loophole--editing-timer nil)
                   (advice-add 'intern :filter-args deflect-to-temp-obarray)
                   (advice-add 'unintern :filter-args deflect-to-temp-obarray)
                   ,@body)
               (advice-remove 'unintern deflect-to-temp-obarray)
               (advice-remove 'intern deflect-to-temp-obarray)
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
                   (dolist (variable ,local-variable-if-set-list)
                     (add-variable-watcher
                      variable
                      #'loophole--follow-adding-local-variable))))))))))

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
    (should-error (loophole-map-variable (make-string 0 ?s))
                  :type 'wrong-type-argument)
    (should-error (loophole-map-variable (make-vector 0 0))
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
    (should-error (loophole-state-variable (make-string 0 ?s))
                  :type 'wrong-type-argument)
    (should-error (loophole-state-variable (make-vector 0 0))
                  :type 'wrong-type-argument)))

(provide 'loophole-tests)

;;; loophole-tests.el ends here

