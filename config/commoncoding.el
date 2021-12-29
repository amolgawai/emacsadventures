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
  :defer t
  :init
  (which-function-mode 1))

;; compile command improvements
;; ref - http://endlessparentheses.com/better-compile-command.html
;; https://github.com/krgn/emacs.d/blob/824b4f7b0c1f19ac15942f404c94e5e98c3d8820/config/setup-compile.el
(use-package compile
  :defer t
  :commands (emcsadvntr/compile-please)
  :bind (:map prog-mode-map
              ([f5] . emcsadvntr/compile-please)
              ([C-f5] . compile))
  :init
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

  (setq compilation-finish-functions 'compile-autoclose))

(use-package ansi-color
  :defer t)

;; quickly run/compilr any language
;; ref - https://github.com/emacsorphanage/quickrun
(use-package quickrun
  :defer t
  :bind
  (("<f5>" . quickrun)
   ("M-<f5>" . quickrun-shell)))

;; bazel build system
(use-package bazel-mode
  :straight (emacs-bazel-mode :type git :host github :repo "bazelbuild/emacs-bazel-mode")
  :mode ("BUILD\\|WORKSPACE\\|CROSSTOOL\\|\\.bazel|\\.bzl\\'")
  :config
  (add-hook 'bazel-mode-hook 'bazel-install-reformat))

;; cmake build system
(use-package cmake-mode
  :hook (cmake-mode . lsp-deferred)
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

;; cmake-ide
(use-package cmake-ide
  :after projectile
  :init (cmake-ide-setup)
  :hook (c++-mode . my/cmake-ide-find-project)
  :preface
  (defun my/cmake-ide-find-project ()
    "Find the directory of the project for cmake-ide."
    (with-eval-after-load 'projectile
      (setq cmake-ide-project-dir (projectile-project-root))
      (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
    (setq cmake-ide-compile-command
          (concat "cd " cmake-ide-build-dir " && cmake .. && make"))
    (cmake-ide-load-db))

  (defun my/switch-to-compilation-window ()
    "Switch to the *compilation* buffer after compilation."
    (other-window 1))
  :bind ([remap comment-region] . cmake-ide-compile)
  :config (advice-add 'cmake-ide-compile :after #'my/switch-to-compilation-window))

;; Magit for git interactions
(use-package magit
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

(use-package evil-magit
  :defer t
  :requires magit evil
  :after magit)

;; github with magit
(use-package forge
  :defer t
  :after magit)
;; :config
;; (evil-bind-key '(normal visual) magit-mode-map "," 'forge-dispatch)
;; (evil-bind-key 'normal magit-commit-section-map (kbd "gb") 'forge-browse-dwim)
;; (evil-bind-key 'normal magit-remote-section-map (kbd "gb") 'forge-browse-remote)
;; (evil-bind-key 'normal magit-branch-section-map (kbd "gb") 'forge-browse-branch)
;; (evil-bind-key 'normal forge-topic-mode-map (kbd "C-c C-c") 'forge-create-post))

;; (use-package forge-list
;;   :defer t)
;; :config
;; (evil-bind-key 'normal forge-topic-list-mode-map (kbd "q") 'quit-window)
;; (evil-bind-key 'normal forge-topic-list-mode-map (kbd "o") 'forge-browse-topic))

(use-package magit-todos
  :defer t
  :after magit
  :config
  (magit-todos-mode))

(use-package magit-delta
  :defer t
  :requires magit)

;; Ask password for pushing to remote
(setenv "SSH_ASKPASS" "git-gui--askpass")

(use-package git-timemachine)
(use-package git-gutter-fringe+
  :config
  (global-git-gutter+-mode)
  (git-gutter-fr+-minimal))

;; snippets
(use-package yasnippet
  ;; :defer t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1))

;; default snippets
(use-package yasnippet-snippets
  :after yasnippet)
;; (use-package yasnippet                  ; Snippets
;;   :diminish
;;   :config
;;   (validate-setq
;;    yas-verbosity 1                      ; No need to be so verbose
;;    yas-wrap-around-region t)

;;   (with-eval-after-load 'yasnippet
;;     (validate-setq yas-snippet-dirs '(yasnippet-snippets-dir)))

;;   (yas-reload-all)
;;   (yas-global-mode))


;; syntax checking with flycheck
(use-package flycheck
  :defer t
  :diminish
  :init (global-flycheck-mode))

;; Formatting for many languages according to specific formatters
;; rf - https://github.com/lassik/emacs-format-all-the-code
(use-package format-all
  :defer t
  :bind ("C-c C-f" . format-all-buffer))

;; Company -- complete anything
(use-package company
  :defer 0.1
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode inferior-python-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  ;; tooltip limit
  (company-tooltip-limit 20)
  ;; selection wrap-around
  (company-selection-wrap-around t)
  ;; cotion sorting weight by frequency
  (company-transformers '(company-sort-by-occurrence))
  :config
  ;; (unless clangd-p (delete 'company-clang company-backends))
  (setq company-backends (remove 'company-ropemacs company-backends))
  :init
  (global-company-mode 1))

;; company sorting
(use-package company-statistics
  :after company
  :config
  (add-hook 'after-init-hook 'company-statistics-mode))

;; icons for company mode
;; ref - https://github.com/TheBB/dotemacs/blob/master/init.el
(use-package company-box
  ;; :disabled t
  :defer t
  :requires all-the-icons
  :diminish company-box-mode
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setf (alist-get 'min-height company-box-frame-parameters) 6)
  ;; (setq company-box-doc-enable nil)
  (setq company-box-icons-unknown 'fa_question_circle)
  (setq company-box-icons-elisp
        '((fa_tag :face font-lock-function-name-face) ;; Function
          (fa_cog :face font-lock-variable-name-face) ;; Variable
          (fa_cube :face font-lock-constant-face) ;; Feature
          (md_color_lens :face font-lock-doc-face))) ;; Face
  (setq company-box-icons-yasnippet 'fa_bookmark)
  (setq company-box-icons-lsp
        `(( 1  . ,(all-the-icons-faicon "file-text-o" :v-adjust -0.0575))     ; Text
          ( 2  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575))            ; Method
          ( 3  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575))            ; Function
          ( 4  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575))            ; Constructor
          ( 5  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Field
          ( 6  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Variable
          ( 7  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575))             ; Class
          ( 8  . ,(all-the-icons-faicon "cogs" :v-adjust -0.0575))            ; Interface
          ( 9  . ,(all-the-icons-alltheicon "less"))                          ; Module
          (10  . ,(all-the-icons-faicon "wrench" :v-adjust -0.0575))          ; Property
          (11  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Unit
          (12  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Value
          (13  . ,(all-the-icons-material "content_copy" :v-adjust -0.2))     ; Enum
          (14  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Keyword
          (15  . ,(all-the-icons-material "content_paste" :v-adjust -0.2))    ; Snippet
          (16  . ,(all-the-icons-material "palette" :v-adjust -0.2))          ; Color
          (17  . ,(all-the-icons-faicon "file" :v-adjust -0.0575))            ; File
          (18  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Reference
          (19  . ,(all-the-icons-faicon "folder" :v-adjust -0.0575))          ; Folder
          (20  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; EnumMember
          (21  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Constant
          (22  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575))             ; Struct
          (23  . ,(all-the-icons-faicon "bolt" :v-adjust -0.0575))            ; Event
          (24  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Operator
          (25  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575))             ; TypeParameter
          ))
  ;; :init
  ;; (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  ;; :config
  ;; (setf (alist-get 'min-height company-box-frame-parameters) 6)
  ;; (setq company-box-icons-alist 'company-box-icons-all-the-icons
  ;;       company-box-backends-colors nil

  ;;       ;; These are the Doom Emacs defaults
  ;;       company-box-icons-all-the-icons
  ;;       `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
  ;;         (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
  ;;         (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
  ;;         (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
  ;;         (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
  ;;         (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
  ;;         (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
  ;;         (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
  ;;         (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
  ;;         (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
  ;;         (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
  ;;         (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
  ;;         (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
  ;;         (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
  ;;         (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
  ;;         (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
  ;;         (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
  ;;         (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
  ;;         (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
  ;;         (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
  ;;         (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
  ;;         (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
  ;;         (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
  ;;         (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
  ;;         (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
  ;;         (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
  ;;         (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green)))))

  ;; Add a space after the icon
  (dolist (elt company-box-icons-all-the-icons)
    (setcdr elt (concat (cdr elt) " "))))


;; Highlight todos, fixmes etc.
(use-package hl-todo
  :defer t
  :config
  (global-hl-todo-mode))

;; dash documentation
(use-package counsel-dash
  :defer t
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
  :defer t
  :hook ((lsp-mode . lsp-ui-mode)
         ((c++-mode rust-mode) .  lsp-deferred))
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  ;; `-background-index' requires clangd v8+!
  ;; (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error")))
  )
(use-package company-lsp
  :defer t
  :after (company lsp-mode)
  :config
  (push 'company-lsp company-backends))
(use-package lsp-ivy :defer t :commands lsp-ivy-workspace-symbol)
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :defer t :commands lsp-treemacs-errors-list)
(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :after (lsp-mode)
  :requires lsp-mode flycheck
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-header nil
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-alignment 'at-point
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-border (face-foreground 'default)
        lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer
        lsp-ui-peek-show-directory t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-imenu-enable t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25))


;; optionally if you want to use debugger
(use-package dap-mode
  :defer t
  :straight t
  :commands dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1))
;; (use-package dap-lldb
;;   :straight t
;;   :defer t))
;; (use-package dap-LANGUAGE) ;; to load the dap adapter for your language

;; show color for the values
(use-package rainbow-mode
  ;; :defer t
  :hook prog-mode
  :config (setq-default rainbow-x-colors-major-mode-list '()))

(provide 'commoncoding)
;;; commoncoding.el ends here
