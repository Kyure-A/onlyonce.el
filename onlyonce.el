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

(require 's)

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

(when (file-exists-p onlyonce-custom-file)
  (load onlyonce-custom-file))

(defcustom onlyonce--executable-list '()
  "List of commands to execute with onlyonce.el."
  :group 'onlyonce
  :version ""
  :type '(repeat string))

(defcustom onlyonce--executed-p nil
  "Indicates whether onlyonce.el has been executed or not."
  :group 'onlyonce
  :version ""
  :type 'boolean)

(defun onlyonce-add (command)
  "Add COMMAND (string) that you want to be loaded automatically.and executed *only once* during dotfiles installation."
  (add-to-list 'onlyonce--executable-list command))

(defun onlyonce--convert-command-from-string (command)
  "Interpret COMMANDs (and their arguments) and convert them into a usable form with onlyonce-startup."
  (let* ((converted '())
	 (commands (s-split " " (s-replace "'" "" command))))
    (dolist (arg commands t)
      (push (intern arg) converted))
    (reverse converted)))

(defun onlyonce--convert-command-from-list (command)
  "Interpret COMMANDs (and their arguments) and convert them into a usable form with onlyonce-startup."
  (let* ((converted '())
	 (commands '()))
    (dolist (arg command t)
      (if (symbolp arg)
	  (push (symbol-name arg) commands)
	(while (consp arg)
	  (setq arg (eval arg)))
	(push (symbol-name arg) commands)))
    (dolist (arg commands t)
      (push (s-replace "'" "" arg) converted))
    converted))

(defun onlyonce--convert-command (command)
  ;; not work
  (when (listp command)
    (onlyonce--convert-command-from-list command))
  (when (stringp command)
    (onlyonce--convert-command-from-string command)))

(defun onlyonce-startup ()
  "Execute a set of functions added that you want executed only once."
  (unless (eval onlyonce--executed-p)
    (progn (dolist (command-args onlyonce--executable-list t)
	     (let* ((command (car (onlyonce--convert-command-from-string command-args)))
		    (args (cdr (onlyonce--convert-command-from-string command-args))))
	       (progn (apply command args)
		      (message "%s is executed by onlyonce.el." command))))
	   (custom-set-variables '(onlyonce--executed-p t)))))

(provide 'onlyonce)

;;; onlyonce.el ends here
