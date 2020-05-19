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
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
;; avoid problems with files newer than their byte-compiled counterparts
;; it's better a lower startup than load an outdated and maybe bugged package
(setq load-prefer-newer t)

                                        ; activate all the packages (in particular autoloads)
(package-initialize)

                                        ; fetch the list of packages available
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; bootstrap quelpa
;; don't auto-update on windows as it results in various problems
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(setq quelpa-update-melpa-p nil)

;; use quelpa with use-package
(unless (package-installed-p 'quelpa-use-package)
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git")))
(setq quelpa-use-package-inhibit-loading-quelpa t)
(require 'quelpa-use-package)

(setq use-package-ensure-function 'quelpa)

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t))

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'quelpa-use-package)

;; (eval-when-compile
;;   (require 'use-package))

;; bootstrap quelpa for use-package
;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher git
;;    :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
;; (require 'quelpa-use-package)

;;(require 'diminish)
(use-package diminish
  :defer 0.1
  :diminish (visual-line-mode . "?")
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-fill-function
  :diminish subword-mode)
(use-package delight
  :quelpa (delight :fetcher github :repo "emacsmirror/delight")
  :defer t)
(use-package try
  :defer t)

(use-package bind-key
  :defer t)

;; enhance emacs list - https://github.com/rolandwalker/list-utils
(use-package list-utils
  :defer t)
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
