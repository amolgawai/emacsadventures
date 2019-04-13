;;; bootstrapPackaging.el --- Add package repositories and initialise use-package  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Amol Gawai

;; Author: Amol Gawai <amol@doesnot.exist>
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

;; Add package repositories and enable package loading with use-package

;;; Code:

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("quelpa" . "https://github.com/quelpa/quelpa")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
;; avoid problems with files newer than their byte-compiled counterparts
;; it's better a lower startup than load an outdated and maybe bugged package
(setq load-prefer-newer t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;;(require 'diminish)
(use-package diminish
  :ensure t
  :demand t
  :diminish (visual-line-mode . "?")
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-fill-function
  :diminish subword-mode)
(use-package delight
  :ensure t)

(use-package bind-key
  :ensure t)

;; enhance emacs list - https://github.com/rolandwalker/list-utils
(use-package list-utils
  :ensure t)
;; If you want to install multiple packages at once, creat a list
;; use-package is much better alternative to reduce startup delay
; list the packages you want
;;(setq package-list '(package1 package2))

; install the missing packages
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

;; Additional information about packages in the mode-line
(use-package paradox
  :defer 1
  :custom
  (paradox-column-width-package 27)
  (paradox-column-width-version 13)
  (paradox-execute-asynchronously t)
  (paradox-hide-wiki-packages t)
  :config
  (paradox-enable)
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

(provide 'bootstrapPackaging)
;;; bootstrapPackaging.el ends here
