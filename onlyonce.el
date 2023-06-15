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

(defgroup onlyonce ()
  "A tool to run functions that you want to run only once during the installation of dotfiles in init.el."
  :group 'tools
  :prefix "onlyonce-"
  :link '(url-link "https://github.com/Kyure-A/onlyonce.el"))

(defcustom onlyonce-custom-file custom-file
  "This variable is used to set the file.
It records whether or not the command added by onlyonce-add has been executed."
  :group 'onlyonce
  :version ""
  :type 'string)

(defcustom onlyonce--executable-list '()
  "List of commands to execute with onlyonce.el."
  :group 'onlyonce
  :version ""
  :type '(repeat function))

(defcustom onlyonce--executed-p nil
  "Indicates whether onlyonce.el has been executed.
This variable is referenced by onlyonce-executed-p."
  :group 'onlyonce
  :version ""
  :type 'boolean)

(defmacro onlyonce-add (command)
  "Add COMMAND that you want to be loaded automatically.
and executed *only once* during dotfiles installation."
  `(add-to-list 'onlyonce--executable-list ,command))

(defun onlyonce-startup ()
  "Execute a set of functions added that you want executed only once."
  (when (eq nil onlyonce--executed-p)
    (progn (custom-set-variables '(onlyonce--executed-p t))
	   (dolist (command onlyonce--executable-list t)
	     (progn (funcall command) ;; 現在は引数を取るコマンドを実行できない（例: (push 'a 1)）のでどうにかする
		    (message "%s is executed by onlyonce.el." command))))))

(provide 'onlyonce)

;;; onlyonce.el ends here
