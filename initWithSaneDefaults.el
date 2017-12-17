;;; initWithSaneDefaults.el --- Initialise clean Emacs and set sane defaults  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Amol Gawai

;; Author: Amol Gawai;;; initWithSaneDefaults.el --- Initialise emacs and set sane defaults  -*- lexical-binding: t; -*- <amol@doesnot.exist>
;; Keywords: lisp

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

;; Initialise Emacs with clean looks and set sane defaults expeced from a modern text editor.

;;; Code:

(setq user-full-name "Amol Gawai"
      user-mail-address "amol@doesnot.exist")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
;; Keep all backup and auto-save files in one directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))


(setq inhibit-splash-screen t
      inhibit-startup-screen t
      visible-bell nil
      ring-bell-function 'ignore
      echo-keystrokes 0.1
      use-dialog-box nil
      apropos-do-all t
      initial-scratch-message nil)

(blink-cursor-mode -1)

;; mac specifi key setup
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))

;; delete the previous selection when overrides it with a new insertion.
(delete-selection-mode t)

(defalias 'list-buffers 'ibuffer)

(toggle-indicate-empty-lines)

;; save the desktop
(desktop-save-mode 1)
(show-paren-mode 1)
(column-number-mode t)
(global-visual-line-mode t)
(global-linum-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;(diminish 'visual-line-mode)

(if (display-graphic-p)
    (scroll-bar-mode -1))
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;; split the window vertically on right - useful for wide screen
(setq split-width-threshold nil)
(setq split-width-threshold 0)

;; Enable filesets to group a set of files
(filesets-init)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; These functions are useful. Activate them.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Emacs for OS X crashes a lot, let see if increasing garbage collector solves the issue
;;The threshold is in bytes
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(provide 'initWithSaneDefaults)
;;; initWithSaneDefaults.el ends here
