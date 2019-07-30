;;; counsel-test.el --- Browse and execute tests with ivy -*- lexical-binding: t -*-

;; Copyright (c) 2019 Konstantin Sorokin (GNU/GPL Licence)

;; Authors: Konstantin Sorokin <sorokin.kc@gmail.com>
;; URL: http://github.com/xmagpie/counsel-test
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (ivy "0.11.0") (s "1.12.0"))
;; Keywords: tools, ivy, counsel, testing, ctest, pytest

;; This file is NOT part of GNU Emacs.

;; counsel-test is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; counsel-test is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with counsel-test.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; This package is a small framework for integrating counsel with
;;; different testing frameworks.  By using it one can browse, select and
;;; execute tests from concrete frameowork with the power of counsel and ivy.

;;; Code:
(require 's)
(require 'seq)
(require 'subr-x)
(require 'ivy)

(defvar counsel-test-dir nil
  "Directory to run ctest in.

It is recommended to set this variable via dir-locals.el.")

(defun counsel-test--candidates-cmd-result (candidates-cmd)
  "Execute the given CANDIDATES-CMD command and handle the output.

Return a list of strings representing the trimmed command output."
  (seq-map 's-trim (s-lines (shell-command-to-string candidates-cmd))))

(defun counsel-test--get-dir (&optional force-read-dir)
  "Determine the directory to run the tests in.

When FORCE-READ-DIR is not nil prompt for ctest directory even if it was
already set."
  (let ((test-dir (if (or force-read-dir (not counsel-test-dir))
                      (read-directory-name "Select test dir: ")
                    counsel-test-dir)))
    (s-append "/" (s-chop-suffix "/" test-dir))))

;; Ctest section
(defvar counsel-test-ctest-cmd "ctest"
  "Command used to invoke ctest.")

(defvar counsel-test-ctest-env "CLICOLOR_FORCE=1 CTEST_OUTPUT_ON_FAILURE=1"
  "Envirionment variables for tests.

It is recommended to set this variable via dir-locals.el.")

(defun counsel-test-ctest--discover (&optional force-read-dir)
  "Run ctest to get the available test candidates.

When FORCE-READ-DIR is not nil prompt for ctest directory even if it was
already set."
  (let* ((candidates-cmd (concat counsel-test-ctest-cmd " -N"))
         (test-re "^Test[[:space:]]*#")
         (default-directory (counsel-test--get-dir force-read-dir)))
    (seq-filter (lambda(s)
                  (s-match test-re s))
                (counsel-test--candidates-cmd-result candidates-cmd))))

(defun counsel-test-ctest--num-from-str (s)
  "Extract number from the string representing test.

S is a single string representing test from the output of ctest
-N, e.g Test #2: MyTest"
  (string-to-number (cadr (s-match "#\\([[:digit:]]+\\)" s))))

(defun counsel-test-ctest--nums-from-strs (strs)
  "Extract numbers from strings representing tests.

STRS is a list of test strings from the output of ctest -N"
  (seq-map 'counsel-test-ctest--num-from-str strs))

(defun counsel-test-ctest--create-cmd (test-nums)
  "Create ctest command to run selected candidates.

TEST-NUMS is a list of numbers representing the tests to run"
  (format "%s%s -I %s"
	  (if (string-empty-p counsel-test-ctest-env)
              ""
            (format "env %s " counsel-test-ctest-env))
          counsel-test-ctest-cmd
	  (s-join "," (seq-map (lambda(n)
                                 (format "%d,%d" n n))
                               test-nums))))


(defun counsel-test-ctest--action (selections)
  "The action to run on the selected candidates.

SELECTIONS is a list of candidate tests to execute."
  (let* ((test-nums (counsel-test-ctest--nums-from-strs selections))
         (default-directory (counsel-test--get-dir))
         (compile-command (counsel-test-ctest--create-cmd test-nums)))
    (compile compile-command)))

;;;###autoload
(defun counsel-test-ctest (arg)
  "Browse and execute ctest tests.

If the value of `counsel-test-dir' is not set (e.g. nil) prompt user for the
ctest directory.

With a prefix argument ARG also force prompt user for this directory."
  (interactive "P")

  (unless (executable-find counsel-test-ctest-cmd)
    (error "Command '%s' not found in path" counsel-test-ctest-cmd))

  (ivy-read "Select tests: " (counsel-test-ctest--discover arg)
	    :require-match t
	    :sort t
	    :action (lambda (x) (counsel-test-ctest--action (list x)))
            :multi-action 'counsel-test-ctest--action
	    :caller 'counsel-test-ctest))

(provide 'counsel-test)
;;; counsel-test.el ends here
