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
  :defer t
  :custom
  (neo-smart-open t)
  (neo-dont-be-alone t)
  (neo-theme 'icons)
  :bind (("<C-f8>" . neotree-toggle)
         (:map neotree-mode-map
               ("<C-return>" . neotree-change-root)
               ("C"          . neotree-change-root)
               ("c"          . neotree-create-node)
               ("+"          . neotree-create-node)
               ("d"          . neotree-delete-node)
               ("r"          . neotree-rename-node)))
  :config
  (defun neotree-resize-window (&rest _args)
    "Resize neotree window.
https://github.com/jaypei/emacs-neotree/pull/110"
    (interactive)
    (neo-buffer--with-resizable-window
     (let ((fit-window-to-buffer-horizontally t))
       (fit-window-to-buffer))))

  (add-hook 'neo-change-root-hook #'neotree-resize-window)
  (add-hook 'neo-enter-hook #'neotree-resize-window)
  )

;; Treemacs - replaces the Neotree
;; Ref - https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (use-package treemacs-evil
    :requires (treemacs evil))


  (use-package treemacs-all-the-icons
    :requires treemacs
    :config
    (treemacs-load-theme "all-the-icons"))

  ;; (use-package treemacs-icons-dired
  ;;   :defer t
  ;;   :after (treemacs dired)
  ;;   :config (treemacs-icons-dired-mode))

  (use-package treemacs-magit
    :requires (treemacs magit))

  (use-package treemacs-persp
    :requires (treemacs persp-mode)
    :config (treemacs-set-scope-type 'Perspectives))

  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
		  treemacs-display-current-project-exclusively t
          treemacs-project-follow-mode           t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("<f8>"        . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :hook (projectile-after-switch-project-hook . treemacs-display-current-project-exclusively))

(use-package ranger
  :defer t
  :bind* ("C-x C-d" . ranger))

;; dired config
;; ref - https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-dired.el

(use-package dired
  :straight (:type built-in)
  ;; :defer t
  :commands (dired-toggle-read-only) ; to toggle read-only state of any buffer
  :hook ((dired-mode . auto-revert-mode) ; Auto-refresh dired on file change
         (dired-mode . my/dired-rename-buffer-name)
         (dired-mode . my/dired-truncate-lines))
  :config
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
    (toggle-truncate-lines 1)))


;; (use-package dired-x
;;   :straight nil
;;   :defer t
;;   :config
;;   (progn
;;     (setq dired-omit-verbose nil)
;;     ;; hide backup, autosave, *.*~ files
;;     ;; omit mode can be toggled using `M-o' in dired buffer
;;     (add-hook 'dired-mode-hook #'dired-omit-mode)))
(use-package dired+
  :straight (dired+ :type git :host github :repo "emacsmirror/dired-plus")
  :defer t
  :after (dired)
  :config
  (progn
    (setq diredp-hide-details-initially-flag nil)
    (diredp-toggle-find-file-reuse-dir 1)))

;; http://pragmaticemacs.com/emacs/tree-style-directory-views-in-dired-with-dired-subtree/
(use-package dired-subtree
  :straight dired-hacks
  :defer t
  :after (dired)
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              ("I" . dired-subtree-remove)))
;; https://github.com/Fuco1/dired-hacks/blob/master/dired-collapse.el
(use-package dired-collapse
  :defer t
  :straight dired-hacks
  :after (dired)
  :commands (dired-collapse dired-collapse-mode)
  :config
  (add-hook 'dired-mode-hook 'dired-collapse-mode))
;; filter dired buffer. Ref - https://writequit.org/denver-emacs/presentations/2016-05-24-elpy-and-dired.html#orgheadline13
(use-package dired-narrow
  :defer t
  :straight dired-hacks
  :after (dired)
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))
;; quicj preview certain files. Ref - https://writequit.org/denver-emacs/presentations/2016-05-24-elpy-and-dired.html#orgheadline13
(use-package quick-preview
  :defer t
  :after (dired)
  :straight dired-hacks
  :bind (("C-c q" . quick-preview-at-point)
         :map dired-mode-map
         ("Q" . quick-preview-at-point)))

(use-package dired-sidebar
  :defer t
  :straight dired-hacks
  :after (dired)
  ;; :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(defun emcsadvntr/sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

(provide 'dirNav)
;;; dirNav.el ends here
