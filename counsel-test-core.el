;;; counsel-test-core.el --- counsel-test: Core definitions -*- lexical-binding: t -*-

;; Copyright (c) 2019 Konstantin Sorokin (GNU/GPL Licence)

;; Authors: Konstantin Sorokin <sorokin.kc@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 's)
(require 'seq)
(require 'ivy)

(defvar counsel-test-dir nil
  "Directory to run tests in.

It is recommended to set this variable via dir-locals.el.")

(defun counsel-test--candidates-cmd-result (candidates-cmd)
  "Execute the given CANDIDATES-CMD command and handle the output.

Return a list of strings representing the trimmed command output."
  (seq-map 's-trim (s-lines (shell-command-to-string candidates-cmd))))

(defun counsel-test--get-dir (&optional force-read-dir)
  "Determine the directory to run the tests in.

OPTIONAL FORCE-READ-DIR whether to force prompt user for the test directory"
  (let ((test-dir (if (or force-read-dir (not counsel-test-dir))
                      (read-directory-name "Select test dir: ")
                    counsel-test-dir)))
    (s-append "/" (s-chop-suffix "/" test-dir))))

(defun counsel-test (discover-f create-cmd-f caller &optional prefix-arg)
  "This function is a generic entry-point for external testing frameworks.

One should specify two functions:

DISCOVER-F is a function that extracts a list of tests (possibly by running
external executable) and gives them as a list of candidates for `ivy-read'.

CREATE-CMD-F is a function that accepts the list of strings (selections from
`ivy-read' based on DISCOVER-F) and creates an external command to run tests in
compile.

CALLER is caller argument for `ivy-read'.

OPTIONAL PREFIX-ARG is forwarded to `counsel-test--get-dir' to force directory
read."
  (let* ((default-directory (counsel-test--get-dir prefix-arg))
         (multi-action (lambda (x) (compile (funcall create-cmd-f x))))
         (single-action (lambda (x) (funcall multi-action (list x)))))
    (ivy-read "Select tests: " (funcall discover-f)
              :require-match t
              :sort t
              :action single-action
              :multi-action multi-action
              :caller caller)))


(provide 'counsel-test-core)
;;; counsel-test-core.el ends here


