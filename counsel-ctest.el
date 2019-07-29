;;; counsel-ctest.el --- Browse and execute ctest with ivy -*- lexical-binding: t -*-

;; Copyright (c) 2019 Konstantin Sorokin (GNU/GPL Licence)

;; Authors: Konstantin Sorokin <sorokin.kc@gmail.com>
;; URL: http://github.com/xmagpie/counsel-ctest
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (ivy "0.11.0") (s "1.12.0"))
;; Keywords: tools, ivy, counsel, ctest

;; This file is NOT part of GNU Emacs.

;; counsel-ctest is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; counsel-ctest is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with counsel-ctest.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; This package is an integration of counsel and ctest.  By using it one can
;;; browse, select and execute ctest tests with the power of counsel and ivy.

;;; Code:
(require 's)
(require 'ivy)

(defvar counsel-ctest-cmd "ctest"
  "Command used to invoke ctest.")

(defvar counsel-ctest-dir nil
  "Directory to run ctest in.

It is recommended to set this variable via dir-locals.el.")

(defvar counsel-ctest-env "CLICOLOR_FORCE=1 CTEST_OUTPUT_ON_FAILURE=1"
  "Envirionment variables for tests.

It is recommended to set this variable via dir-locals.el.")

(defun counsel-ctest--get-build-dir (&optional force-read-dir)
  "Determine the directory to run the tests in.

When FORCE-READ-DIR is not nil prompt for ctest directory even if it was
already set."
  (when (or force-read-dir (not counsel-ctest-dir))
    (setq counsel-ctest-dir
	  (read-directory-name "CTest Build Dir: ")))
  (s-append "/" (s-chop-suffix "/" counsel-ctest-dir)))

(defun counsel-ctest--get-candidates (&optional force-read-dir)
  "Run ctest to get the available test candidates.

When FORCE-READ-DIR is not nil prompt for ctest directory even if it was
already set."
  (let* ((candidates-cmd (concat counsel-ctest-cmd " -N"))
	 (test-re "^Test[[:space:]]*#")
	 (default-directory (counsel-ctest--get-build-dir force-read-dir)))
    (seq-filter (lambda(s)
                  (s-match test-re s))
                (seq-map 's-trim
                         (s-lines (shell-command-to-string
                                   candidates-cmd))))))

(defun counsel-ctest--num-from-str (s)
  "Extract number from the string representing test.

S is a single string representing test from the output of ctest
-N, e.g Test #2: MyTest"
  (string-to-number
	   (cadr (s-match "#\\([[:digit:]]+\\)" s))))

(defun counsel-ctest--nums-from-strs (strs)
  "Extract numbers from strings representing tests.

STRS is a list of test strings from the output of ctest -N"
  (seq-map 'counsel-ctest--num-from-str strs))

(defun counsel-ctest--create-cmd (test-nums)
  "Create ctest command to run selected candidates.

TEST-NUMS is a list of numbers representing the tests to run"
  (format "env %s %s -I %s"
	  counsel-ctest-env
          counsel-ctest-cmd
	  (s-join "," (seq-map (lambda(n)
                                 (format "%d,%d" n n))
                               test-nums))))

(defun counsel-ctest--action (selections)
   "The action to run on the selected candidates.

SELECTIONS is a list of candidate tests to execute."
   (let* ((test-nums (counsel-ctest--nums-from-strs selections))
          (default-directory (counsel-ctest--get-build-dir))
	  (compile-command (counsel-ctest--create-cmd test-nums)))
     (compile compile-command)))

;;;###autoload
(defun counsel-ctest (arg)
  "Browse and execute ctest tests.

If the value of `counsel-ctest-dir' is not set (e.g. nil) prompt user for the
ctest directory.

With a prefix argument ARG also force prompt user for this directory."
  (interactive "P")

  (unless (executable-find counsel-ctest-cmd)
    (error "Command '%s' not found in path" counsel-ctest-cmd))

  (ivy-read "Select tests: " (counsel-ctest--get-candidates arg)
	    :require-match t
	    :sort t
	    :action (lambda (x) (counsel-ctest--action (list x)))
            :multi-action 'counsel-ctest--action
	    :caller 'counsel-ctest))

(provide 'counsel-ctest)
;;; counsel-ctest.el ends here
