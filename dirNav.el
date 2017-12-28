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

    (setq neo-window-fixed-size nil)
    (global-set-key [f8] 'neotree-toggle)

    (add-hook 'neo-change-root-hook
              (lambda () (neo-buffer--with-resizable-window
                          (let ((fit-window-to-buffer-horizontally t))
                            (fit-window-to-buffer)))))

    (bind-keys
     :map neotree-mode-map
     ("<C-return>" . neotree-change-root)
     ("C"          . neotree-change-root)
     ("c"          . neotree-create-node)
     ("+"          . neotree-create-node)
     ("d"          . neotree-delete-node)
     ("r"          . neotree-rename-node))))


;; dired config
;; ref - https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-dired.el

(use-package dired
  :commands (dired-toggle-read-only) ; to toggle read-only state of any buffer
  :config
  (progn
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies  'always)
    ;; Set this variable to non-nil, Dired will try to guess a default
    ;; target directory. This means: if there is a dired buffer
    ;; displayed in the next window, use its current subdir, instead
    ;; of the current subdir of this dired buffer. The target is used
    ;; in the prompt for file copy, rename etc.
    (setq dired-dwim-target t)

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -G : Do not print group names like 'users'
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables,
    ;;       '/' to directories, etc.
    (setq dired-listing-switches "-alGhvF --group-directories-first") ; default: "-al"

    (defun my/dired-rename-buffer-name ()
      "Rename the dired buffer name to distinguish it from file buffers.
It added extra strings at the front and back of the default dired buffer name."
      (let ((name (buffer-name)))
        (if (not (string-match "/$" name))
            (rename-buffer (concat "*Dired* " name "/") t))))

    (defun my/dired-truncate-lines ()
      (toggle-truncate-lines 1))

    (add-hook 'dired-mode-hook #'my/dired-rename-buffer-name)
    (add-hook 'dired-mode-hook #'my/dired-truncate-lines)

    (use-package dired-x
      :config
      (progn
        (setq dired-omit-verbose nil)
        ;; hide backup, autosave, *.*~ files
        ;; omit mode can be toggled using `M-o' in dired buffer
        (add-hook 'dired-mode-hook #'dired-omit-mode)))
    (use-package dired+
      :ensure t
      :config
      (progn
        (require 'dired+)
        (setq diredp-hide-details-initially-flag nil))
      (diredp-toggle-find-file-reuse-dir 1))

    ;; http://pragmaticemacs.com/emacs/tree-style-directory-views-in-dired-with-dired-subtree/
    (use-package dired-subtree
      :ensure t
      :bind (:map dired-mode-map
                  ("i" . dired-subtree-insert)
                  ("I" . dired-subtree-remove)))
    ;; https://github.com/Fuco1/dired-hacks/blob/master/dired-collapse.el
    (use-package dired-collapse
      :ensure t
      :commands (dired-collapse dired-collapse-mode)
      :config
      (add-hook 'dired-mode-hook 'dired-collapse-mode))))

(provide 'dirNav)
;;; dirNav.el ends here
