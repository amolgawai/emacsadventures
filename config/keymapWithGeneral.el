;; keymapWithGeneral.el --- custom keymapping with general.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Amol Gawai

;; Author: Amol Gawai <amol@doesnot.exist>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; general.el provides a great way to add keybindings.  It provides many ways
;; to add keymaps consistently in the same way use-package helps package management.
;; Additionally, general.el helps quite a lot with implementing Vim like keymaps
;; Refs -
;; https://github.com/noctuid/general.el
;; https://dev.to/huytd/emacs-from-scratch-1cg6

;;; Code:

;; FIXME - Make this general and remove package specific things to respective use-package
(use-package general
  :ensure t
  :commands (general-define-key general-override-mode general-evil-setup general--simulate-keys)
  :config
  (progn
	(general-evil-setup)
	(general-define-key
	 :states '(normal visual insert emacs)
	 :prefix "SPC"
	 :non-normal-prefix "M-SPC"
	 ;; general
	 "/"   '(projectile-ripgrep :which-key "ripgrep")
	 "."   '(counsel-find-file :which-key "open file")
	 "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
	 "SPC" '(counsel-M-x :which-key "M-x")
	 "m"   '(magit-status :which-key "Magit")
	 ;; Buffers
	 "b"    '(:ignore t :which-key "buffer")
	 "b."   '(counsel-projectile-switch-to-buffer :which-key "project buffer list")
	 "bb"   '(ivy-switch-buffer :which-key "switch")
	 "bl"   '(list-buffers :which-key "list")"bs"   '(save-buffer :which-key "save")
	 ;; projectile
	 "p"    '(:ignore t :which-key "projectile")
	 "pc"   '(:keymap projectile-command-map :which-key "commands")
	 "pf"  '(counsel-projectile-find-file :which-key "find files")
	 "pp"  '(projectile-persp-switch-project :which-key "switch project")
	 "pb"  '(counsel-projectile-switch-to-buffer :which-key "switch buffer")
	 ;; search
	 "s"   '(:ignore t :which-key "search")
	 "sf"  '(counsel-imenu :which-key "functions")
	 ;; quit
	 "q"   '(:ignore t :which-key "quit")
	 "qz"  '(delete-frame :which-key "delete frame")
	 "qq"  '(kill-emacs :which-key "quit")
	 "qr"  '(restart-emacs :which-key "restart")
	 ;; toggles
	 "t"   '(:ignore t :which-key "toggle")
	 "tt"  '(treemacs :which-key "treemacs")
	 "tn"  '(neotree-toggle :which-key "neotree")
	 "tf"  '(imenu-list-smart-toggle :which-key "function sidebar")
	 "ts"  '(eshell-toggle :which-key "shell")
	 ;; Window
	 "w"   '(:ignore t :which-key "window")
	 "wl"  '(windmove-right :which-key "move right")
	 "wh"  '(windmove-left :which-key "move left")
	 "wk"  '(windmove-up :which-key "move up")
	 "wj"  '(windmove-down :which-key "move bottom")
	 "w/"  '(split-window-right :which-key "split right")
	 "w-"  '(split-window-below :which-key "split bottom")
	 "wd"  '(delete-window :which-key "delete window")
	 "wx"  '(delete-other-windows :which-key "delete other windows")
	 ))
  (general-nmap "SPC h" (general-simulate-key "C-h" :which-key "help")))
  ;; (general-nmap "SPC m" (general-simulate-key "," :which-key "major mode")))


(provide 'keymapWithGeneral)
;;; keymapWithGeneral.el ends here
