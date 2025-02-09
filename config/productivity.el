;;; productivity.el --- emacs productivity enhancing configurations  -*- lexical-binding: t; -*-

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

;; guide-key is helful for displaying available key bindings
;; (use-package guide-key
;;   :config
;;   (progn
;;     (setq guide-key/guide-key-sequence t)
;;     (setq guide-key/popup-window-position 'bottom)
;;     (guide-key-mode 1)))

;; which-key is similar to guide-key but little more powerful
;; which-key
(use-package which-key
  :init
  (setq which-key-separator " → "
        which-key-prefix-prefix "+"
        which-key-max-display-columns 5
        ;; which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL")
        which-key-show-remaining-keys t
        which-key-show-remaining-keys t)
  :diminish which-key-mode
  :config
  ;; (which-key-setup-side-window-right-bottom)
  (which-key-mode))

;; display which key in a frame rather than minibuffer
;; ref - https://github.com/waymondo/hemacs/blob/master/init.el
;; (use-package which-key-posframe
;;   :after which-key
;;   :config
;;   (which-key-posframe-mode t)
;;   :custom
;;   (which-key-posframe-poshandler 'posframe-poshandler-frame-center))

;; advanced meta -x
(use-package smex
                                        ;  :init (smex-initialize)
  :bind ("M-x" . smex))

;; reformat buffer
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

;; search multiple buffers - ref https://github.com/jwiegley/use-package
(use-package color-moccur
  :defer t
  :commands (isearch-moccur isearch-all)
  :bind (("M-s O" . moccur)
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all))
  :init
  (setq isearch-lazy-highlight t)
  :config
  (use-package moccur-edit :straight t))

;; (use-package auto-complete
;;   :diminish
;;   :init
;;   (progn
;;     (auto-complete-mode t))
;;   :bind (("C-n" . ac-next)
;;          ("C-p" . ac-previous))
;;   :config
;;   (progn
;;     (use-package auto-complete-config)

;;     (ac-set-trigger-key "TAB")
;;     (ac-config-default)

;;     (setq ac-delay 0.02)
;;     (setq ac-use-menu-map t)
;;     (setq ac-menu-height 50)
;;     (setq ac-use-quick-help nil)
;;     (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
;;     (setq ac-ignore-case nil)
;;     (setq ac-dwim  t)
;;     (setq ac-fuzzy-enable t)

;;     (use-package ac-dabbrev
;;       :straight (ac-dabbrev :type git :host github :repo "emacsmirror/ac-dabbrev")
;;       :config
;;       (progn
;;         (add-to-list 'ac-sources 'ac-source-dabbrev)))

;;     (setq ac-modes '(js3-mode
;;                      emacs-lisp-mode
;;                      lisp-mode
;;                      lisp-interaction-mode
;;                      slime-repl-mode
;;                      c-mode
;;                      cc-mode
;;                      c++-mode
;;                      go-mode
;;                      java-mode
;;                      eclim-mode
;;                      malabar-mode
;;                      clojure-mode
;;                      clojurescript-mode
;;                      scala-mode
;;                      scheme-mode
;;                      ocaml-mode
;;                      tuareg-mode
;;                      coq-mode
;;                      haskell-mode
;;                      agda-mode
;;                      agda2-mode
;;                      perl-mode
;;                      cperl-mode
;;                      python-mode
;;                      ruby-mode
;;                      enh-ruby-mode
;;                      lua-mode
;;                      ecmascript-mode
;;                      javascript-mode
;;                      js-mode
;;                      js2-mode
;;                      php-mode
;;                      css-mode
;;                      makefile-mode
;;                      sh-mode
;;                      fortran-mode
;;                      f90-mode
;;                      ada-mode
;;                      xml-mode
;;                      sgml-mode
;;                      ts-mode
;;                      sclang-mode
;;                      verilog-mode))))

(use-package restart-emacs
  :bind* (("C-x M-c" . restart-emacs)))

(use-package smartparens
  :init (smartparens-global-mode 1)
  :diminish smartparens-mode)

;; workspaces
(use-package perspective
  :defer t
  :commands persp-mode
  :bind (("s-<up>" . persp-next)
         ("s-<down>" . persp-prev))
  :config
  (setq persp-suppress-no-prefix-key-warning t)
  ;; create directory for perspectives if doesn't exist
  (let ((perspective-dir (expand-file-name "perspectives" user-emacs-directory)))
	(make-directory perspective-dir :parents)))

;;   ;; :defer t
;;   :config
;;   (persp-mode t))

;; projectile
(use-package projectile
  :config
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))
;; (setq projectile-completion-system 'helm))

(use-package counsel-projectile
  :after (projectile counsel)
  :config
  (counsel-projectile-mode))

(use-package persp-projectile
  :after projectile
  :config
  (setq wg-morph-on nil ;; switch off animation
        persp-autokill-buffer-on-remove 'kill-weak
        persp-auto-save-opt 0
        persp-auto-resume-time -1
        persp-nil-hidden t
        persp-add-buffer-on-find-file t
        persp-add-buffer-on-after-change-major-mode t
        persp-hook-up-emacs-buffer-completion t
        ;; persp-state-default-file (locate-user-emacs-file "perspectives/default.persp"))
        persp-state-default-file (expand-file-name "perspectives/default.persp" user-emacs-directory))
  ;; persp-state-default-file (expand-file-name ".persp-last" user-emacs-directory))
  (add-hook 'kill-emacs-hook #'persp-state-save)

  ;; Make ivy play nice
  ;; (with-eval-after-load "ivy"
  ;;   (add-hook 'ivy-ignore-buffers
  ;;             #'(lambda (b)
  ;;                 (when persp-mode
  ;;                   (let ((persp (get-current-persp)))
  ;;                     (if persp
  ;;                         (not (persp-contain-buffer-p b persp))
  ;;                       nil)))))
  ;;   (setq ivy-sort-functions-alist
  ;;         (append ivy-sort-functions-alist
  ;;                 '((persp-kill-buffer   . nil)
  ;;                   (persp-remove-buffer . nil)
  ;;                   (persp-add-buffer    . nil)
  ;;                   (persp-switch        . nil)
  ;;                   (persp-window-switch . nil)
  ;;                   (persp-frame-switch . nil)))))
  (persp-mode t))

;; projectile ripgrep
(use-package projectile-ripgrep
  :after projectile
  :config
  (add-hook 'ripgrep-search-mode-hook 'hl-line-mode))

;; (use-package helm-projectile
;;   :config
;;   (helm-projectile-on))
                                        ; flashes the cursor's line when you scroll
(use-package ibuffer-projectile
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-sidebar
  ;; :load-path "~/.emacs.d/fork/ibuffer-sidebar"
  :defer t
  :commands (ibuffer-sidebar-toggle-sidebar)
  ;; :config
  ;; (setq ibuffer-sidebar-use-custom-font t)
  ;; (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140))
  )

(use-package beacon
  :config
  (beacon-mode 1)
                                        ; (setq beacon-color "#666600")
  )

                                        ; deletes all the whitespace when you hit backspace or delete
;; (use-package hungry-delete
;;   :config
;;   (global-hungry-delete-mode))
(use-package hungry-delete
   :diminish
   :hook (after-init . global-hungry-delete-mode)
   :init (setq hungry-delete-except-modes
               '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))
                                        ; expand the marked region in semantic increments (negative prefix to reduce region)
(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; move lines/regions
(use-package move-text
  :config
  (move-text-default-bindings))

;; wiki-summary for searching wikipedia
(use-package wiki-summary
  :defer 1
  :bind ("C-c W" . wiki-summary)
  :preface
  (defun my/format-summary-in-buffer (summary)
    "Given a summary, stick it in the *wiki-summary* buffer and display the buffer"
    (let ((buf (generate-new-buffer "*wiki-summary*")))
      (with-current-buffer buf
        (princ summary buf)
        (fill-paragraph)
        (goto-char (point-min))
        (text-mode)
        (view-mode))
      (pop-to-buffer buf))))
(advice-add 'wiki-summary/format-summary-in-buffer :override #'my/format-summary-in-buffer)

;; smart C-a
;; Ref - http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  :straight (eshell-toggle :type git :host github :repo "4DA/eshell-toggle")
  :bind
  ("s-`" . eshell-toggle))

;; Aweshell, shell extension base on eshell with better features.
(use-package aweshell
  :defer t
  :straight (aweshell
             :type git
             :host github
             :repo "manateelazycat/aweshell")
  ;; :straight (aweshell :type git :host github :repo "manateelazycat/aweshell")
  :commands (aweshell-new aweshell-dedicated-open)
  :bind
  (("s-#" . aweshell-dedicated-toggle)))

;; Shell Here
;; Shell Here, a tool that opens a shell buffer in (or relative to) default-directory.
(use-package shell-here
  :defer t
  :bind ("s-~" . shell-here))

(use-package vterm
  :straight t
  :defer t
  :config
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no"))

;; manage multiple vterm
(use-package multi-vterm
  :defer t
  :after vterm
  :config
  (add-hook 'vterm-mode-hook
			(lambda ()
			  (setq-local evil-insert-state-cursor 'box)
			  (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

;; ** edit browser text area in Emacs (sync both ways)
;; ref - https://superuser.com/questions/488348/edit-any-text-input-shown-by-a-browser-mostly-chrome-with-emacs?noredirect=1
;; source - https://github.com/alpha22jp/atomic-chrome
(use-package atomic-chrome
  :defer t
  ;; dependency Atomic Chrome extension (in Chrome)
  :init
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-extension-type-list '(atomic-chrome))
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)))
  :config
  (atomic-chrome-start-server))

;; replace the standard kill, mark with easy kill
;; ref - https://github.com/leoliu/easy-kill
(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

;; homebrew on macos
(use-package homebrew-mode
  :if (memq window-system '(mac ns))
  :straight (homebrew-mode :type git :host github :repo "dunn/homebrew-mode")
  :defer t
  :config
  (global-homebrew-mode))

;; fuzzy search using fzf - make sure fzf is installed
(use-package fzf
  :defer t)
  ;; :init
  ;; (setenv "FZF_DEFAULT_COMMAND" "rg --hidden --ignore .git -g \"\""))

;; open new frame with empy buffer
(defun emcsadvntr/new-buffer-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

;; Spell-check
(use-package flyspell
  :defer t
  :diminish
  :if (or (executable-find "hunspell")(executable-find "aspell"))
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  ;; (flyspell-mode . (lambda ()
  ;;                    (dolist (key '("C-;" "C-," "C-."))
  ;;                      (unbind-key key flyspell-mode-map)))))
  :init
  (setq flyspell-issue-message-flag nil)
  ;; Set $DICPATH to "$HOME/Library/Spelling" for hunspell.
  (cond
   ;; try hunspell at first
   ;; if hunspell does NOT exist, use aspell
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
	(setq ispell-really-hunspell t)
    (setenv
     "DICPATH"
     (concat (getenv "HOME") "/Library/Spelling"))
    (setenv "DICTIONARY" "en_GB")
    (setq ispell-local-dictionary "en_GB")
    (setq ispell-local-dictionary-alist
          ;; Please note the list `("-d" "en_GB")` contains ACTUAL parameters passed to hunspell
          ;; You could use `("-d" "en_GB,en_US-med")` to check with multiple dictionaries
          '(("UK_English" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))

   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB")))))

(use-package flyspell-lazy
  :defer t
  :config
  (flyspell-lazy-mode 1))

(use-package flyspell-correct
  :defer t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :defer t
  :after flyspell-correct)

;; profiling the startup time
;; ref - https://github.com/jschaf/esup
(use-package esup
  ;; To use MELPA Stable use ":pin mepla-stable",
  ;; :pin melpa
  :commands (esup))

(provide 'productivity)
;;; productivity.el ends here
