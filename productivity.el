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
  :diminish which-key-mode
  :config (which-key-mode))

;; reformat buffer
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

;; search multiple buffers - ref https://github.com/jwiegley/use-package
(use-package color-moccur
  :commands (isearch-moccur isearch-all)
  :bind (("M-s O" . moccur)
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all))
  :init
  (setq isearch-lazy-highlight t)
  :config
  (use-package moccur-edit))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (auto-complete-mode t))
  :bind (("C-n" . ac-next)
         ("C-p" . ac-previous))
  :config
  (progn
    (use-package auto-complete-config)

    (ac-set-trigger-key "TAB")
    (ac-config-default)

    (setq ac-delay 0.02)
    (setq ac-use-menu-map t)
    (setq ac-menu-height 50)
    (setq ac-use-quick-help nil)
    (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
    (setq ac-ignore-case nil)
    (setq ac-dwim  t)
    (setq ac-fuzzy-enable t)

    (use-package ac-dabbrev
      :config
      (progn
        (add-to-list 'ac-sources 'ac-source-dabbrev)))

    (setq ac-modes '(js3-mode
                     emacs-lisp-mode
                     lisp-mode
                     lisp-interaction-mode
                     slime-repl-mode
                     c-mode
                     cc-mode
                     c++-mode
                     go-mode
                     java-mode
                     eclim-mode
                     malabar-mode
                     clojure-mode
                     clojurescript-mode
                     scala-mode
                     scheme-mode
                     ocaml-mode
                     tuareg-mode
                     coq-mode
                     haskell-mode
                     agda-mode
                     agda2-mode
                     perl-mode
                     cperl-mode
                     python-mode
                     ruby-mode
                     enh-ruby-mode
                     lua-mode
                     ecmascript-mode
                     javascript-mode
                     js-mode
                     js2-mode
                     php-mode
                     css-mode
                     makefile-mode
                     sh-mode
                     fortran-mode
                     f90-mode
                     ada-mode
                     xml-mode
                     sgml-mode
                     ts-mode
                     sclang-mode
                     verilog-mode))))

(use-package restart-emacs
  :ensure t
  :bind* (("C-x M-c" . restart-emacs)))

(provide 'productivity)
;;; productivity.el ends here
