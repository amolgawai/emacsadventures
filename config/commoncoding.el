;;; commoncoding.el --- configuration which is common to all programming languages  -*- lexical-binding: t; -*-

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

;;

;;; Code:

;; code folding using hs-minor-mode
(add-hook 'prog-mode-hook
          (lambda()
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>")  'hs-hide-block)
            (local-set-key (kbd "C-c <up>")    'hs-hide-all)
            (local-set-key (kbd "C-c <down>")  'hs-show-all)
            (hs-minor-mode t)))

;; auto indentation
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'prog-mode-hook 'set-newline-and-indent)

;; show what function we're in
(use-package which-func
  :ensure t
  :init
  (which-function-mode 1))

;; compile command improvements
;; ref - http://endlessparentheses.com/better-compile-command.html
;; https://github.com/krgn/emacs.d/blob/824b4f7b0c1f19ac15942f404c94e5e98c3d8820/config/setup-compile.el
(use-package compile
  :commands (emcsadvntr/compile-please)
  :bind (:map prog-mode-map
              ([f5] . emcsadvntr/compile-please)
              ([C-f5] . compile))
  :init
  (progn
    (use-package ansi-color)

    ;; colorize that buffer plz
    (defun colorize-compilation-buffer ()
      (toggle-read-only)
      (ansi-color-apply-on-region (point-min) (point-max))
      (toggle-read-only))

    ;; close if compilation was successful
    (defun compile-autoclose (buffer string)
      "Bury a compilation buffer if succeeded without warnings "
      (if (and
           (string-match "compilation" (buffer-name buffer))
           (string-match "finished" string)
           (not
            (with-current-buffer buffer
              (search-forward "warning" nil t))))
          (run-with-timer 1 nil
                          (lambda (buf)
                            (delete-window (get-buffer-window buf)))
                          buffer)))
    (setq compilation-window-height 10)
    (defun emcsadvntr/compile-please (comint)
      "Compile without confirmation.
With a prefix argument, use comint-mode."
      (interactive "P")
      ;; Do the command without a prompt.
      ;;      (save-window-excursion
      (compile (eval compile-command) (and comint t)))
    (defun my-compilation-hook ()
      (when (not (get-buffer-window "*compilation*"))
        (save-selected-window
          (save-excursion
            (let* ((w (split-window-vertically))
                   (h (window-height w)))
              (select-window w)
              (switch-to-buffer "*compilation*")
              (shrink-window (- h compilation-window-height)))))))
    (add-hook 'compilation-mode-hook 'my-compilation-hook)

    ;; jump to first error
    (setq compilation-auto-jump-to-first-error t)

    ;; and add the hook
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
    ;; scroll output
    (setq compilation-scroll-output t)
    ;; don't hang on warnings, only errors
    (setq compilation-skip-threshold 2)

    (setq compilation-finish-functions 'compile-autoclose)))

;; quickly run/compilr any language
;; ref - https://github.com/emacsorphanage/quickrun
(use-package quickrun
  :bind
  (("<f5>" . quickrun)
   ("M-<f5>" . quickrun-shell)))

;; Magit for git interactions
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))
;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))
;; Ask password for pushing to remote
(setenv "SSH_ASKPASS" "git-gui--askpass")

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

(use-package git-timemachine
  :ensure t
  )

;; snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config

  (use-package yasnippet-snippets
    :ensure t)

  (yas-global-mode 1)
  )
;; (use-package yasnippet                  ; Snippets
;;   :ensure t
;;   :diminish
;;   :config
;;   (validate-setq
;;    yas-verbosity 1                      ; No need to be so verbose
;;    yas-wrap-around-region t)

;;   (with-eval-after-load 'yasnippet
;;     (validate-setq yas-snippet-dirs '(yasnippet-snippets-dir)))

;;   (yas-reload-all)
;;   (yas-global-mode))

;; (use-package yasnippet-snippets         ; Collection of snippets
;;    :ensure t)

;; syntax checking with flycheck
(use-package flycheck
  :ensure t
  :diminish
  :init (global-flycheck-mode))

;; Formatting for many languages according to specific formatters
;; rf - https://github.com/lassik/emacs-format-all-the-code
(use-package format-all
  :ensure t
  :bind ("C-c C-f" . format-all-buffer))

;; Company -- complete anything
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-backends (remove 'company-ropemacs company-backends)
        company-tooltip-limit 20
        company-tooltip-align-annotations t)
  (add-to-list 'company-backends 'company-jedi)
  (global-company-mode 1))

;; Highlight todos, fixmes etc.
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

;; dash documentation
(use-package counsel-dash
  :commands (counsel-dash
             counsel-dash-set-local-docsets
             counsel-dash-activate-local-docset
             counsel-dash-activate-docset
             counsel-dash-deactivate-docset
             counsel-dash-install-docset)
  :load-path "vendor/counsel-dash"
  :init
  (setq counsel-dash-docsets-path "~/.docset")

  (setq counsel-dash-browser-func 'eww)
  (setq counsel-dash-common-docsets '("Emacs Lisp" "Swift" "iOS" "Go" "Rust" "C++" "Docker" "Python 3"))
  ;; (define-key evil-normal-state-map (kbd "C-f") 'counsel-dash)
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
  (add-hook 'rust-mode-hook (lambda () (setq-local counsel-dash-docsets '("Rust"))))
  (add-hook 'dockerfile-mode-hook (lambda () (setq-local counsel-dash-docsets '("Docker"))))
  (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python 3"))))
  (add-hook 'c++-mode-hook (lambda () (setq-local counsel-dash-docsets '("C" "C++"))))
  (add-hook 'go-mode-hook (lambda () (setq-local counsel-dash-docsets '("Go"))))
  ;; (add-hook 'js2-minor-mode-hook (lambda () (setq-local counsel-dash-docsets '("Javascript" "NodeJS"))))
  ;; (add-hook 'web-mode-hook (lambda () (setq-local counsel-dash-docsets '("Javascript" "HTML""CSS"))))
  ;; (add-hook 'scss-mode-hook (lambda () (setq-local counsel-dash-docsets '("CSS"))))
  (add-hook 'swift-mode-hook (lambda () (setq-local counsel-dash-docsets '("iOS" "Swift")))))
;; ref - https://github.com/tuhdo/emacs-proglang/blob/master/custom/setup-helm.el
;; (use-package helm-dash
;;   :ensure t
;;   :init
;;   (global-set-key (kbd "C-c d") 'helm-dash-at-point)
;;   (defun c-doc ()
;;     (setq helm-dash-docsets '("C")))
;;   (defun c++-doc ()
;;     (setq helm-dash-docsets '("C" "C++")))
;;   (add-hook 'c-mode-hook 'c-doc)
;;   (add-hook 'c++-mode-hook 'c++-doc))

;; LSP - Language Server Protocol Support for Emacs
;; Ref - https://github.com/emacs-lsp/lsp-mode
;; Configure the hooks for each language in the respective language config
;; Install LSPs for specific language. See - https://github.com/emacs-lsp/lsp-mode#supported-languages
(use-package lsp-mode
  :hook (rust-mode .  lsp-deferred)
  :commands lsp)
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company-lsp :ensure t :commands company-lsp)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package helm-lsp :ensure t :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package lsp-mode
;;   :hook (rust-mode . lsp)
;;   :commands (lsp lsp-deferred))
;; (use-package lsp-ui
;;   :requires lsp-mode flycheck
;;   :config
;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-doc-use-childframe t
;;         lsp-ui-doc-position 'top
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-sideline-enable nil
;;         lsp-ui-flycheck-enable t
;;         lsp-ui-flycheck-list-position 'right
;;         lsp-ui-flycheck-live-reporting t
;;         lsp-ui-peek-enable t
;;         lsp-ui-peek-list-width 60
;;         lsp-ui-peek-peek-height 25)

;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))
;; (use-package company-lsp :commands company-lsp)
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; ;; optionally if you want to use debugger
;; (use-package dap-mode
;;   :ensure t
;;   :commands dap-mode
;;   :config
;;   (dap-mode 1)
;;   (require 'dap-ui)
;;   (dap-ui-mode 1)
;;   (require 'dap-lldb))
;; (use-package dap-LANGUAGE) to load the dap adapter for your language


(provide 'commoncoding)
;;; commoncoding.el ends here
