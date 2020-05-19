;;; evilConfig.el --- config evil mode               -*- lexical-binding: t; -*-

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

;; Evil provides excellent way to emulate Vim modes.  This is to cobnfigure
;; evil and other associated packages.

;;; Code:

(use-package evil
  :defer .1 ;; don't block emacs when starting, load evil immediately after startup
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; evil-collection for useful extra keymaps
(use-package evil-collection
  :after evil
  :defer t
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

;; other evil helpers
(use-package evil-surround
  :defer t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :defer t
  :diminish
  :config (evil-commentary-mode +1))

;; highlight the editing
(use-package evil-goggles
  :after evil
  :defer t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

;; align operators - adds gl and gL
(use-package evil-lion
  :after evil
  :defer t
  :config
  (evil-lion-mode))

(provide 'evilConfig)
;;; evilConfig.el ends here
