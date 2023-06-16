;;; onlyonce-test.el --- Test for onlyonce

;; Copyright (C) 2023  Kyure_A

;; Author: Kyure_A <k@kyre.moe>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Test for onlyonce

;;; Code:

(require 'ert)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'onlyonce)

(ert-deftest test-onlyonce--convert-command-from-list ()
  (should (equal (onlyonce--convert-command-from-list '(you-want-to-execute-onlyonce-function 'hogehoge)) '(you-want-to-execute-onlyonce-function hogehoge)))
  (should (equal (onlyonce--convert-command-from-list '(setq hoge "onlyonce")) '(setq hoge "onlyonce"))))

(ert-deftest test-onlyonce--convert-command-from-string ()
  (should (equal (onlyonce--convert-command-from-string "you-want-to-execute-onlyonce-function 'hogehoge") '(you-want-to-execute-onlyonce-function hogehoge)))
  (should (equal (onlyonce--convert-command-from-string "package-install onlyonce") '(package-install onlyonce)))
  ;; (should (equal (onlyonce--convert-command-from-string "setq hoge \"onlyonce\"") '(setq hoge "onlyonce")))
  )

(ert-deftest test-onlyonce--convert-command-from-symbol ()
  (should (equel (onlyonce--convert-command-from-symbol 'you-want-to-execute-onlyonce-function) '(you-want-to-execute-onlyonce-function))))

(ert-deftest test-onlyonce-startup ())

(provide 'onlyonce-test)
;;; onlyonce-test.el ends here
