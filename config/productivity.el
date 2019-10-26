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
;;   :ensure t
;;   :config
;;   (progn
;;     (setq guide-key/guide-key-sequence t)
;;     (setq guide-key/popup-window-position 'bottom)
;;     (guide-key-mode 1)))

;; which-key is similar to guide-key but little more powerful
;; which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

;; advanced meta -x
(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex))

;; reformat buffer
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

;; search multiple buffers - ref https://github.com/jwiegley/use-package
(use-package color-moccur
  :ensure t
  :commands (isearch-moccur isearch-all)
  :bind (("M-s O" . moccur)
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all))
  :init
  (setq isearch-lazy-highlight t)
  :config
  (use-package moccur-edit :quelpa t))

;; (use-package auto-complete
;;   :ensure t
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
;;       :quelpa (ac-dabbrev :fetcher github :repo "emacsmirror/ac-dabbrev")
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
  :ensure t
  :bind* (("C-x M-c" . restart-emacs)))

(use-package smartparens
  :ensure t
  :init (smartparens-global-mode 1)
  :diminish smartparens-mode)
;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package ibuffer-projectile
  :ensure t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

                                        ; flashes the cursor's line when you scroll
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
                                        ; (setq beacon-color "#666600")
  )

                                        ; deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

                                        ; expand the marked region in semantic increments (negative prefix to reduce region)
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; move lines/regions
(use-package move-text
  :ensure t
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
  :quelpa (eshell-toggle :fetcher github :repo "4DA/eshell-toggle" :version original)
  :bind
  ("s-`" . eshell-toggle))

;; ** edit browser text area in Emacs (sync both ways)
;; ref - https://superuser.com/questions/488348/edit-any-text-input-shown-by-a-browser-mostly-chrome-with-emacs?noredirect=1
;; source - https://github.com/alpha22jp/atomic-chrome
(use-package atomic-chrome
  ;; dependency Atomic Chrome extension (in Chrome)
  :ensure t
  :init
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-extension-type-list '(atomic-chrome))
  (setq atomic-chrome-url-major-mode-alist
		'(("github\\.com" . gfm-mode)))
  :config
  (atomic-chrome-start-server))

(provide 'productivity)
;;; productivity.el ends here
