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
  :defer 0.2
  :commands (general-define-key general-override-mode general-evil-setup general--simulate-keys)
  :config
  (progn
	(general-evil-setup)
	(general-define-key
	 :states '(normal visual insert emacs)
	 :prefix "SPC"
	 :non-normal-prefix "C-SPC"
	 ;; general
	 "/"         '(projectile-ripgrep :which-key "ripgrep")
	 "."         '(counsel-find-file :which-key "open file")
	 ","         '(fzf-projectile :which-key "fuzzy open file in project")
	 ;; ":"         '(indent-buffer :which-key "format buffer")
	 ":"         '(format-all-buffer :which-key "format buffer")
	 ;; "TAB"       '(switch-to-prev-buffer :which-key "previous buffer")
	 "SPC"       '(counsel-M-x :which-key "M-x")
	 "m"         '(magit-status :which-key "Magit")
     ;; perspectives
     "TAB"       '(:ignore t :which-key "perspectives")
     "TAB TAB"   '(persp-switch :which-key "switch to/create new")
     "TAB l"     '(persp-switch-last :which-key "last")
     "TAB d"     '(persp-kill :which-key "delete")
     "TAB c"     '(:keymap perspective-map :which-key "commands")
     "TAB p"     '(:ignore t :which-key "Load/Save")
     "TAB p l"   '(persp-state-load :which-key "Load")
     "TAB p s"   '(persp-state-save :which-key "Save")
	 ;; Buffers
	 "b"         '(:ignore t :which-key "buffer")
	 "bb"        '(counsel-projectile-switch-to-buffer :which-key "project buffer list")
	 "b."        '(ivy-switch-buffer :which-key "switch")
	 "bl"        '(ibuffer :which-key "buffer list")
	 "bd"        '(kill-buffer :which-key "kill")
	 "bs"        '(save-buffer :which-key "save")
	 "b <left>"  '(centaur-tabs-backward :which-key "previous")
	 "b <right>" '(centaur-tabs-forward :which-key "next")
	 ;; "b <left>"  '(projectile-previous-project-buffer :which-key "previous")
	 ;; "b <right>" '(projectile-next-project-buffer :which-key "next")
	 ;; eval
	 "e"         '(:ignore t :which-key "evaluate")
	 "ee"        '(eval-last-sexp :which-key "last expression")
	 "eb"        '(eval-buffer :which-key "buffer")
     "er"        '(eval-region :which-key "region")
	 ;; Frames
	 "f"         '(:ignore t :which-key "frames")
     "fn"        '(emcsadvntr/new-buffer-frame :which-key "new frame")
	 "fo"        '(find-file-other-frame :which-key "open file -> new frame")
	 "ff"        '(other-frame :which-key "other frame")
	 "fd"        '(delete-frame :which-key "delete")
     ;; help
     "h"         '(:ignore t :which-key "help")
     ;; "hh"        '((general-simulate-key "C-h") :which-key "help")
	 "hy"        '(yas-describe-tables :which-key "snippet tables")
	 ;; notes
	 "n"         '(:ignore t :which-key "notes")
     "na"        '(org-agenda :which-key "agenda")
     "nc"        '(org-capture :which-key "capture")
	 "nd"        '(emcsadvntr/deft-dwim :which-key "deft")
	 "nj"        '(org-journal-new-entry :which-key "journal entry")
     "no"        '(ivy-omni-org :which-key "open org file")
     "nr"        '(org-refile :which-key "refile note")
	 ;; open
	 "o"         '(:ignore t :which-key "open")
	 "of"        '(counsel-fzf :which-key "file")
	 ;; projectile
	 "p"         '(:ignore t :which-key "projectile")
	 "pc"        '(:keymap projectile-command-map :which-key "commands")
	 "pf"        '(counsel-projectile-find-file :which-key "find files")
	 "pp"        '(projectile-persp-switch-project :which-key "switch project")
	 "pb"        '(counsel-projectile-switch-to-buffer :which-key "switch buffer")
     ;; run/compile
     "r"         '(:ignore t :which-key "run/compile")
     "rr"        '(quickrun :which-key "quickrun/compile")
     "rc"        '(emcsadvntr/compile-please :which-key "compile please")
	 ;; search
	 "s"         '(:ignore t :which-key "search")
	 "sf"        '(counsel-imenu :which-key "functions")
	 "sp"        '(projectile-ripgrep :which-key "in project")
     "so"        '(counsel-org-agenda-headlines :which-key "in org notes")
	 ;; quit
	 "q"         '(:ignore t :which-key "quit")
	 "ql"        '(persp-state-restore :which-key "reload last session")
	 "qz"        '(delete-frame :which-key "delete frame")
	 "qq"        '(kill-emacs :which-key "quit")
	 "qr"        '(restart-emacs :which-key "restart")
	 ;; toggles
	 "t"         '(:ignore t :which-key "toggle")
	 "tb"        '(ibuffer-sidebar-toggle-sidebar :which-key "ibuffer sidebar")
	 "tc"        '(emcsadvntr/sidebar-toggle :which-key "dired/ibuffer sidebar")
	 "td"        '(dired-sidebar-toggle-sidebar :which-key "dired sidebar")
	 "tt"        '(treemacs :which-key "treemacs")
	 "tn"        '(neotree-toggle :which-key "neotree")
	 "tf"        '(imenu-list-smart-toggle :which-key "function sidebar")
	 "ts"        '(eshell-toggle :which-key "shell")
     "to"        '(org-sidebar-toggle :which-key "org-sidebar")
     "tp"        '(elpy-shell-toggle-dedicated-shell :which-key "python shell")
     "tr"        '(ranger :which-key "ranger file manager")
	 ;; Window
	 "w"         '(:ignore t :which-key "window")
	 "wl"        '(windmove-right :which-key "move right")
	 "wh"        '(windmove-left :which-key "move left")
	 "wk"        '(windmove-up :which-key "move up")
	 "wj"        '(windmove-down :which-key "move bottom")
	 "w/"        '(split-window-right :which-key "split right")
	 "w-"        '(split-window-below :which-key "split bottom")
	 "wd"        '(delete-window :which-key "delete window")
	 "wx"        '(delete-other-windows :which-key "delete other windows")
     "w="        '(balance-windows :which-key "balance")
     "ws"        '(ace-swap-window :which-key "swap")
	 ))
  (general-nmap "SPC c" (general-simulate-key "C-c" :which-key "C-c"))
  (general-nmap "SPC x" (general-simulate-key "C-x" :which-key "C-x"))
  (general-nmap "SPC h h" (general-simulate-key "C-h" :which-key "help")))
;; (general-nmap "SPC m" (general-simulate-key "," :which-key "major mode")))


(provide 'keymapWithGeneral)
;;; keymapWithGeneral.el ends here
