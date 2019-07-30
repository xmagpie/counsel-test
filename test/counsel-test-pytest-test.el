;;; counsel-test-pytest-test.el --- counsel-test: pytest tests  -*- lexical-binding: t; -*-

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

(require 'counsel-test-pytest)

(ert-deftest counsel-test-pytest:check-cut-params ()
  ;; No param cases
  (should (equal (counsel-test-pytest--cut-params "path/test.py::fun")
                 "path/test.py::fun"))
  (should (equal (counsel-test-pytest--cut-params "path/test::Class::fun")
                 "path/test::Class::fun"))
  ;; Param cases
  (should (equal (counsel-test-pytest--cut-params "path/test.py::fun[val]")
                 "path/test.py::fun"))
  (should (equal (counsel-test-pytest--cut-params
                  "path/test.py::Class::fun[001]")
                 "path/test.py::Class::fun"))
  (should (equal (counsel-test-pytest--cut-params "path/test.py::fun[[[a]]]")
                 "path/test.py::fun"))
  (should (equal (counsel-test-pytest--cut-params "path/test.py::fun[]")
                 "path/test.py::fun")))

(ert-deftest counsel-test-pytest:check-discover ()
  :expected-result :failed
  (with-mock
    (let ((counsel-test-dir "/test/path")
          (expected-result '("tests/unit/test_module1.py::test_one"
                             "tests/unit/test_module2.py::Class::test"
                             "tests/integration/test_module3.py::test")))
      (stub shell-command-to-string =>
            "tests/unit/test_module1.py::test_one
tests/unit/test_module2.py::test_one
tests/unit/test_module2.py::Class::test
tests/integration/test_module3.py::test[param1]
tests/integration/test_module3.py::test[param2]

-- Docs: https://docs.pytest.org/en/latest/warnings.html
5 warnings in 0.50 seconds")

      (should (equal (counsel-test-pytest--discover) expected-result)))))

(ert-deftest counsel-test-pytest:create-cmd ()
  (let ((single-selection '("tests/unit/test_module1.py::test_one"))
        (multiple-selections '("tests/unit/test_module1.py::test_one"
                               "tests/unit/test_module2.py::test_one"))
        (counsel-test-pytest-cmd "pytest"))
      (should (equal (counsel-test-pytest--create-cmd single-selection)
                     "pytest tests/unit/test_module1.py::test_one"))
      (should (equal (counsel-test-pytest--create-cmd multiple-selections)
                     "pytest tests/unit/test_module1.py::test_one tests/unit/test_module2.py::test_one"))))

;;; counsel-test-pytest-test.el ends here
