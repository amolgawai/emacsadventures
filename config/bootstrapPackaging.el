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

;; Bootstrap straight.el
;; TODO - Replace quelpa with straight
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(setq straight-recipes-emacsmirror-use-mirror t)
;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default t)

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  (auto-package-update-delete-old-versions t))

;; for benchmarking init time, uncomment when needed
;; (use-package benchmark-init
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; for ensuring system utilities are installed
;; (use-package use-package-ensure-system-package
;;   :ensure t)

(use-package diminish
  :straight t
  ;; :defer 0.1
  :diminish (visual-line-mode . "?")
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-fill-function
  :diminish subword-mode)
(use-package delight
  :straight (delight :type git :host github :repo "emacsmirror/delight")
  :defer t)

(use-package try
  :defer t)

(use-package bind-key
  :defer t)

;; enhance emacs list - https://github.com/rolandwalker/list-utils
(use-package list-utils
  :defer t)

;; Additional information about packages in the mode-line
(use-package paradox
  :defer t
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
