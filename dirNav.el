;;; dirNav.el --- directory navigation utilities like neotree etc.  -*- lexical-binding: t; -*-

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

;; Directory navigation setup like dired customization, neotree like elements etc.
;; neotree setup refs
;; - https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-neotree.el
;; - https://github.com/sakshamsharma/max-emacs/blob/master/elisp/neotree-init.el

;;; Code:

(use-package neotree
  :ensure t
  :config
  (progn
    (setq-default neo-smart-open t)    ;  every time when the neotree window is
                                        ;  opened, it will try to find current
                                        ;  file and jump to node.
    (setq-default neo-dont-be-alone t) ; Don't allow neotree to be the only open
                                        ; window
    (setq neo-theme 'icons ) ;; make sure all-the-icons
    ;; is installed
    ;;    (setq neo-theme 'nerd) ; 'classic, 'nerd, 'ascii, 'arrow

    (global-set-key [f8] 'neotree-toggle)

    (bind-keys
     :map neotree-mode-map
     ("<C-return>" . neotree-change-root)
     ("C"          . neotree-change-root)
     ("c"          . neotree-create-node)
     ("+"          . neotree-create-node)
     ("d"          . neotree-delete-node)
     ("r"          . neotree-rename-node))))

;; http://emacsrocks.com/e16.html
(setq dired-dwim-target t)

;; http://pragmaticemacs.com/emacs/tree-style-directory-views-in-dired-with-dired-subtree/
(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
	      ("i" . dired-subtree-insert)
	      ("I" . dired-subtree-remove)))

(use-package dired-collapse
  :ensure t
  :commands (dired-collapse dired-collapse-mode)
  :init
  (add-hook 'dired-mode-hook 'dired-collapse-mode))

(provide 'dirNav)
;;; dirNav.el ends here
