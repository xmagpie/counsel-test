;;; counsel-ctest-test.el --- counsel-ctest: tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Konstantin Sorokin

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;;; Code:

(require 'ert)
(require 'el-mock)

(require 'counsel-ctest)

(ert-deftest counsel-ctest:check-build-dir ()
  (with-mock
    (stub read-directory-name => "/test/path")
    (should (string-equal (counsel-ctest--get-build-dir t) "/test/path/"))

    (let* ((default-dir "/default/test/path/")
           (counsel-ctest-dir default-dir))
      (should (equal (counsel-ctest--get-build-dir) default-dir)))

    (should (equal (counsel-ctest--get-build-dir) "/test/path/"))))

(ert-deftest counsel-ctest:check-get-candidates ()
  (with-mock
    (let ((counsel-ctest-dir "/test/path")
          (expected-result '("Test  #1: test-one"
                             "Test  #2: test-two"
                             "Test  #3: test-three")))
      (stub shell-command-to-string =>
            "Test project /path/to/projects
  Test  #1: test-one
  Test  #2: test-two
  Test  #3: test-three

Total Tests: 3")

      (should (equal (counsel-ctest--get-candidates) expected-result)))))

(ert-deftest counsel-ctest:check-num-from-str ()
  (should (equal (counsel-ctest--num-from-str "Test  #1: test-one") 1))
  (should (equal (counsel-ctest--num-from-str "Test  #302: test") 302)))

(ert-deftest counsel-ctest:create-cmd ()
  (let ((counsel-ctest-env "ENV_VAR=value")
        (counsel-ctest-cmd "ctest"))
    (should (equal (counsel-ctest--create-cmd '(1)) "env ENV_VAR=value ctest -I 1,1"))
    (should (equal (counsel-ctest--create-cmd
                    '(1 23 145))
                   "env ENV_VAR=value ctest -I 1,1,23,23,145,145")))

  (let ((counsel-ctest-env "")
        (counsel-ctest-cmd "ctest -v"))
    (should (equal (counsel-ctest--create-cmd '(1)) "ctest -v -I 1,1"))
    (should (equal (counsel-ctest--create-cmd
                    '(1 23 145))
                   "ctest -v -I 1,1,23,23,145,145"))))
;;; counsel-ctest-test.el ends here
