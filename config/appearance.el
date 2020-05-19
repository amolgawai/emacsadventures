;;; appearance.el --- Set the appearance of emacs as per my test  -*- lexical-binding: t; -*-

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

;; Setting appearance of Emacs with font, themes, powerline, rainbow brackets etc.

;;; Code:

;; set the font for all the frames
;; (add-to-list 'default-frame-alist
;;              '(font . "Source Code Pro-16"))
;; (custom-set-faces
;;  '(fixed-pitch ((t (:family "Source Code Pro"))))
;;  '(variable-pitch ((t (:family "Source Sans Pro" :height )))))
(set-face-attribute 'default nil :family "Source Code Pro" :height 190)
(set-face-attribute 'fixed-pitch nil :family "Source Code Pro" :height 190)
(set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 210)

;; FIXME - find a place for these settings
;; Use variable pitch font for info buffers
;; ref - https://yoo2080.wordpress.com/2013/05/30/monospace-font-in-tables-and-source-code-blocks-in-org-mode-proportional-font-in-other-parts/
;;; display Info mode buffers in proportional font
(add-hook 'Info-mode-hook 'variable-pitch-mode)

;;; but code examples in monospace font
(defvar emcsadvntr/rx-info-code (rx bol "     " (* not-newline) eol))
(add-hook 'Info-mode-hook 'emcsadvntr/Info-font-lock)
(defun emcsadvntr/Info-font-lock ()
  "Fonts for code blocks in info."
  (interactive)
  (require 'org)
  (font-lock-add-keywords
   nil
   `((,emcsadvntr/rx-info-code
      .
      ;; let's just use org-block
      (quote org-block)
      ))))

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

;; themes
;; sanityinc-tomorrow-night
(use-package color-theme-sanityinc-tomorrow
  :defer t)

;;  material-theme
(use-package material-theme
  :defer t)

;;  :config
;;  (load-theme 'material t))

;; dracula theme
(use-package dracula-theme
  :defer t)
  ;;  :init (load-theme 'dracula t)

;; spacemacs
(use-package spacemacs-theme
  :defer t
  :init
  (setq spacemacs-use-variable-pitch nil))

;; solarised
(use-package solarized-theme
  :defer t
  :init
  (setq solarized-use-variable-pitch nil))

;; monokai
(use-package monokai-theme
  :if (window-system)
  :defer t
  :init
  (setq monokai-use-variable-pitch nil))

;; doom themes - https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  ;;  :defer t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; function for changing themes
(defun switch-theme (theme)
  "Disables any currently active themes and loads THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun disable-active-themes ()
  "Disables any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(bind-key "s-<f12>" 'switch-theme)
(bind-key "s-<f11>" 'disable-active-themes)


;; rainboaw delimiters
(use-package rainbow-delimiters
  :diminish
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; icons
(use-package all-the-icons
  :diminish) ; call M-x all-the-icons-install-fonts

(use-package all-the-icons-dired
  :defer t
  :diminish
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))


;; mode line

;; Powerline
;; (use-package powerline
;;   :init
;;   :config
;;   (powerline-center-theme)
;;   (setq powerline-default-separator 'arrow)
;;   :ensure t)

;; Spaceline - a modified powerline from spacemacs
;; ref - https://github.com/TheBB/spaceline
;; config ref - https://writequit.org/eos/eos-appearance.html
;; (use-package spaceline
;;   :ensure t
;;   :init
;;   (setq powerline-default-separator 'arrow
;;         spaceline-minor-modes-separator " ")
;;   (require 'spaceline-config)
;;   (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
;;   (spaceline-emacs-theme)
;;   ;;  (spaceline-helm-mode)
;;   (use-package info+
;;     :quelpa (info+ :fetcher github :repo "emacsmirror/info-plus")
;;     :ensure t
;;     :init
;;     (spaceline-info-mode))
;;   (use-package fancy-battery
;;     :ensure t
;;     :init
;;     (add-hook 'after-init-hook #'fancy-battery-mode)
;;     (display-battery-mode -1)))

;; (use-package spaceline-all-the-icons
;;   :after spaceline
;;   :ensure t
;;   :config
;;   (setq spaceline-all-the-icons-separator-type 'arrow)
;;   (spaceline-all-the-icons-theme)
;;   (spaceline-all-the-icons--setup-neotree))

;; doom-modeline, clean and beautiful modeline
;; ref - https://github.com/seagle0128/doom-modeline#install
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count nil
  doom-modeline-major-mode-color-icon t)
  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (setq doom-modeline-lsp t)
  ;; Whether display the minor modes in the mode-line.
  (setq doom-modeline-minor-modes nil))

;; posframe -  a popup frame (used for ivy and which-key)
;; ref - https://github.com/waymondo/hemacs/blob/master/init.el
(use-package posframe
  :defer t
  :custom
  (posframe-arghandler #'emcsadvntr-posframe-arghandler)
  :config
  (defun emcsadvntr-posframe-arghandler (posframe-buffer arg-name value)
    (let ((info '(:internal-border-width 12 :min-width 80 :background-color "#282a36")))
      (or (plist-get info arg-name) value))))


(provide 'appearance)
;;; appearance.el ends here
