;;; fileNav.el --- Utilities for quick navigation in the file  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Amol Gawai

;; Author: Amol Gawai <amol@doesnot.exist>
;; Keywords: lisp, abbrev, abbrev

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

;; various utilities to navigate file

;;; Code:

(use-package imenu-list
  :ensure t
  :defer t
  :bind ([f9] . imenu-list-smart-toggle)
  :config
  (setq imenu-list-auto-resize t)            ;; resize automatically
  (setq imenu-list-focus-after-activation t))

(use-package avy
  :ensure t
  :diminish
  :bind (("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2))
  :config
  (avy-setup-default))


(provide 'fileNav)
;;; fileNav.el ends here
