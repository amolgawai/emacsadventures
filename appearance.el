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
(add-to-list 'default-frame-alist
             '(font . "Source Code Pro-14"))

;; themes

;; default theme - sanityinc-tomorrow-night
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

;;  material-theme
(use-package material-theme
;;  :defer 10
  :ensure t
  :config
  (load-theme 'material t))

;; spacemacs
(use-package spacemacs-theme
  :defer t
  :init
  (setq spacemacs-use-variable-pitch nil)
  :ensure t)

;; solarised
(use-package solarized-theme
  :defer t
  :init
  (setq solarized-use-variable-pitch nil)
  :ensure t)

;; monokai
(use-package monokai-theme
  :if (window-system)
  :defer t
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

;; rainboaw delimiters
(use-package rainbow-delimiters
  :ensure t
  :diminish
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(provide 'appearance)
;;; appearance.el ends here
