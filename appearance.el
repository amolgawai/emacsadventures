;;; appearance.el --- Adjust appearance of emacs by customizing themes, fonts, mode line etc.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  

;; Author:  Amol Gawai
;; Keywords: lisp, convenience,
;; dependency - use-package

;; set the font for all the frames
(add-to-list 'default-frame-alist
             '(font . "Source Code Pro-14"))

;; themes

;; default theme - sanityinc-tomorrow-night
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night t))

;;  material-theme
(use-package material-theme
  :defer 10
  :init
  (setq material-use-variable-pitch nil)
  :ensure t)

;; spacemacs
(use-package spacemacs-theme
  :defer 10
  :init
  (setq spacemacs-use-variable-pitch nil)
  :ensure t)

;; solarised
(use-package solarized-theme
  :defer 10
  :init
  (setq solarized-use-variable-pitch nil)
  :ensure t)

;; monokai
(use-package monokai-theme
  :if (window-system)
  :ensure t
  :init
  (setq monokai-use-variable-pitch nil))

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


;; Powerline
(use-package powerline
  :init
  :config
  (powerline-center-theme)
  (setq powerline-default-separator 'arrow)
  :ensure t)

