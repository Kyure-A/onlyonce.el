;;; onlyonce.el --- Package that executes a function that you want to execute *onlyonce* the first time  -*- lexical-binding: t; -*-

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

;; A tool to run functions that you want to run only once during the installation of dotfiles in init.el.

;;; Code:

(require 'cl-lib)
(require 's)

(defgroup onlyonce ()
  "A tool to run functions that you want to run only once during the installation of dotfiles in init.el."
  :group 'tools
  :prefix "onlyonce-"
  :link '(url-link "https://github.com/Kyure-A/onlyonce.el"))

(defcustom onlyonce--executable-list '()
  "List of commands to execute with onlyonce.el.  Commands in this list are normalized by onlyonce-convert-command-*."
  :group 'onlyonce
  :type 'list
  :version "")

(defcustom onlyonce-executed-p nil
  "Indicates whether onlyonce.el has been executed or not.  This variable is initialized to t after 'onlyonce-startup' is executed and added to custom.el."
  :group 'onlyonce
  :version ""
  :type 'boolean)

(when (file-exists-p custom-file)
  (load custom-file))

(defun onlyonce--normalize-command-from-string (str)
  "STR to normalize the command to a form that can be executed with 'onlyonce-startup'."
  (cl-check-type str string)
  (let* ((normalized '())
	 (commands (s-split " " (s-replace "'" "" str))))
    (dolist (arg commands t)
      (push (intern arg) normalized))
    (reverse normalized)))

(defun onlyonce--normalize-command-from-list (lis)
  "LIS to normalize the command to a form that can be executed with 'onlyonce-startup'."
  (cl-check-type lis list)
  (let* ((normalized '())
	 (commands '()))
    (dolist (arg lis t)
      (if (stringp arg)
	  (push arg commands)
	(if (symbolp arg)
	    (push arg commands)
	  (while (consp arg)
	    (setf arg (eval arg)))
	  (push arg commands))))
    (dolist (arg commands t)
      (push arg normalized))
    normalized))

(defun onlyonce--normalize-command-from-symbol (sym)
  "SYM to normalize the command to a form that can be executed with 'onlyonce-startup'."
  (cl-check-type sym symbol)
  (list sym))

(defun onlyonce--normalize-command (command)
  "Interpret COMMANDs (and their arguments) and normalize them into a usable form with 'onlyonce-startup'."
  (let* ((ret nil))
    (when (symbolp command)
      (setf ret (onlyonce--normalize-command-from-symbol command)))
    (when (listp command)
      (setf ret (onlyonce--normalize-command-from-list command)))
    (when (stringp command)
      (setf ret (onlyonce--normalize-command-from-string command)))
    ret))

(defun onlyonce-add (command)
  "Add COMMAND (string) that you want to be loaded automatically and executed *only once* during dotfiles installation."
  (add-to-list 'onlyonce--executable-list (onlyonce--normalize-command command)))

(defun onlyonce-startup ()
  "Execute a set of functions added that you want executed only once."
  (interactive)
  (unless (eval onlyonce-executed-p)
    (progn (dolist (command-args onlyonce--executable-list t)
	     (let* ((command (car command-args))
		    (args (cdr command-args)))
	       (progn (apply command args)
		      (message "%s is executed by onlyonce.el." command))))
	   (custom-set-variables '(onlyonce-executed-p t))
	   (custom-save-all)
	   onlyonce-executed-p)))

(provide 'onlyonce)

;;; onlyonce.el ends here
