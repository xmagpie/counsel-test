;;; counsel-test-test.el --- counsel-test: tests  -*- lexical-binding: t; -*-

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

(require 'counsel-test)

(ert-deftest counsel-test-:check-build-dir ()
  (with-mock
    (stub read-directory-name => "/test/path")
    (should (string-equal (counsel-test--get-dir t) "/test/path/"))

    (let* ((default-dir "/default/test/path/")
           (counsel-test-dir default-dir))
      (should (equal (counsel-test--get-dir) default-dir)))

    (should (equal (counsel-test--get-dir) "/test/path/"))))

(ert-deftest counsel-test-ctest:check-discover ()
  (with-mock
    (let ((counsel-test-dir "/test/path")
          (expected-result '("Test  #1: test-one"
                             "Test  #2: test-two"
                             "Test  #3: test-three")))
      (stub shell-command-to-string =>
            "Test project /path/to/projects
  Test  #1: test-one
  Test  #2: test-two
  Test  #3: test-three

Total Tests: 3")

      (should (equal (counsel-test-ctest--discover) expected-result)))))

(ert-deftest counsel-test-ctest:check-num-from-str ()
  (should (equal (counsel-test-ctest--num-from-str "Test  #1: test-one") 1))
  (should (equal (counsel-test-ctest--num-from-str "Test  #302: test") 302)))

(ert-deftest counsel-test-ctest:create-cmd ()
  (let ((single-selection '("Test  #1: test-one"))
        (multiple-selections '("Test  #1: test-one" "Test  #23: extra"
                               "Test  #145: description")))
    (let ((counsel-test-ctest-env "ENV_VAR=value")
          (counsel-test-ctest-cmd "ctest"))
      (should (equal (counsel-test-ctest--create-cmd single-selection)
                     "env ENV_VAR=value ctest -I 1,1"))
      (should (equal (counsel-test-ctest--create-cmd multiple-selections)
                     "env ENV_VAR=value ctest -I 1,1,23,23,145,145")))

    (let ((counsel-test-ctest-env "")
          (counsel-test-ctest-cmd "ctest -v"))
      (should (equal (counsel-test-ctest--create-cmd single-selection)
                     "ctest -v -I 1,1"))
      (should (equal (counsel-test-ctest--create-cmd multiple-selections)
                     "ctest -v -I 1,1,23,23,145,145")))))
;;; counsel-test-test.el ends here
