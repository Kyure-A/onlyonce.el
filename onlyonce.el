;;; onlyonce.el --- Short description of your project  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Kyure_A

;; Author: Kyure_A <k@kyre.moe>
;; Keywords: tools

;; Version: 0.0.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/Kyure-A/onlyonce.el

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

;; Short description of your project

;;; Code:

(require 'cl-lib)
(require 'f)
(require 's)

(defgroup onlyonce ()
  "A tool to run functions that you want to run only once during the installation of dotfiles in init.el."
  :group 'tools
  :prefix "onlyonce"
  :link '(url-link "https://github.com/Kyure-A/onlyonce.el"))

(defcustom onlyonce-custom-file custom-file
  :group 'onlyonce
  :version ""
  :type 'string)

(defcustom onlyonce-executable-list '()
  :group 'onlyonce
  :version ""
  :type '(repeat function))

(defun onlyonce-add (func)
  "Add function that you want to be loaded automatically in init.el but executed *only once* during dotfiles installation."
  (add-to-list 'onlyonce-executable-list 'func))

(defun onlyonce-executed-p ()
  "Return a boolean (t or nil) indicating whether or not it has already been executed."
  (interactive)
  (if (boundp 'onlyonce-executed) t nil))

(defun onlyonce-startup ()
  "Execute a set of functions added by onlyonce-add that you want executed only once."
  (interactive)
  (unless (onlyonce-executed-p)
    (custom-set-variables 'onlyonce-executed t)
    (cl-loop as i
	     from 0 to (length 'onlyonce-executable-list)
	     do (funcall (nth i 'onlyonce-executable-list)))))

(provide 'onlyonce)

;;; onlyonce.el ends here
