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
  :ensure t
  :init
  (which-function-mode 1))

;; compile command improvements
;; ref - http://endlessparentheses.com/better-compile-command.html
;; https://github.com/krgn/emacs.d/blob/824b4f7b0c1f19ac15942f404c94e5e98c3d8820/config/setup-compile.el
(use-package compile
  :commands (emcsadvntr/compile-please)
  :bind (:map prog-mode-map
              ([f5] . emcsadvntr/compile-please)
              ([C-f5] . compile))
  :init
  (progn
    (use-package ansi-color)

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
    (defcustom emcsadvntr/compile-window-size 105
      "Width given to the non-compilation window."
      :type 'integer
      :group 'my)
    (defun emcsadvntr/compile-please (comint)
      "Compile without confirmation.
With a prefix argument, use comint-mode."
      (interactive "P")
      ;; Do the command without a prompt.
      (save-window-excursion
        (compile (eval compile-command) (and comint t)))
      ;; Create a compile window of the desired width.
      (pop-to-buffer (get-buffer "*compilation*"))
      (enlarge-window
       (- (frame-width)
          emcsadvntr/compile-window-size
          (window-width))
       'vertical))

    ;; jump to first error
    (setq compilation-auto-jump-to-first-error t)

    ;; and add the hook
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
    ;; scroll output
    (setq compilation-scroll-output t)
    ;; don't hang on warnings, only errors
    (setq compilation-skip-threshold 2)

    (setq compilation-finish-functions 'compile-autoclose)))

;; Magit for git interactions
(use-package magit
  :ensure t
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

;; Ask password for pushing to remote
(setenv "SSH_ASKPASS" "git-gui--askpass")

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1)
  (git-gutter:linum-setup))

(use-package git-timemachine
  :ensure t
  )

;; snippets
(use-package yasnippet
  :ensure t
  :diminish
  :config
  (progn
    (yas-global-mode)
    (add-to-list 'yas-snippet-dirs
                 "~/.emacs.d/snippets")))

;; syntax checking with flycheck
(use-package flycheck
  :ensure t
  :diminish
  :init (global-flycheck-mode))

;; Company -- complete anything
;; (use-package company
;;   :ensure t
;;   :diminish company-mode
;;   :config
;;   (setq company-backends (remove 'company-ropemacs company-backends)
;;         company-tooltip-limit 20
;;         company-tooltip-align-annotations t)
;;   (global-company-mode 1))

(provide 'commoncoding)
;;; commoncoding.el ends here
