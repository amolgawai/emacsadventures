;;; markdownAndOrg.el --- Configure markdown and org modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Amol Gawai

;; Author: Amol Gawai <amolgawai@Amols-MBP>
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

;; Setup markdown and Org modes

;;; Code:

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1))))

;; org mode

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c b" . org-iswitchb)))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-ac
  :ensure t
  :init (progn
          (require 'org-ac)
          (org-ac/config-default)))

;; deft for managing notes
(use-package deft
  :ensure t
  :bind ("<f7>" . emcsadvntr/deft-dwim)
  :config
  (progn
    (setq deft-directory "~/MyOrganiser")
    (setq deft-recursive t)
    (setq deft-use-filename-as-title nil)
    (setq deft-use-filter-string-for-filename t)
    (setq deft-file-naming-rules '((noslash . "-")
                                   (nospace . "-")
                                   (case-fn . downcase)))
    ;; better deft invoke and quite
    ;; ref - https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-deft.el
    (defvar emcsadvntr/pre-deft-window-configuration nil
      "Variable to store the window configuration before `deft' was called.")

    ;; Advise deft to save window config
    (defun emcsadvntr/deft-dwim-save-windows (orig-fun &rest args)
      (setq emcsadvntr/pre-deft-window-configuration (current-window-configuration))
      (apply orig-fun args))
    (advice-add 'deft :around #'emcsadvntr/deft-dwim-save-windows)

    (defun emcsadvntr/deft-quit ()
      "Save buffer, kill both the deft-opened file buffer and the *Deft* buffer,
and restore the window config to the way it was before deft was invoked."
      (interactive)
      (let ((buf (buffer-name)))
        (save-buffer)
        (kill-buffer buf)
        (delq buf deft-auto-save-buffers) ; Remove the buffer from `deft-auto-save-buffers'
        (kill-buffer "*Deft*")
        (when (window-configuration-p emcsadvntr/pre-deft-window-configuration)
          (set-window-configuration emcsadvntr/pre-deft-window-configuration)
          ;; Reset `emcsadvntr/pre-deft-window-configuration' back to `nil' because
          ;; that value is one of the criteria to check if the user is currently
          ;; editing a deft-opened file
          (setq emcsadvntr/pre-deft-window-configuration nil))))

    (defun emcsadvntr/deft-dwim (option)
      "Launch deft or quit a deft opened file based on context.
If OPTION is \\='(4), call `deft-find-file'.
Else if OPTION is \\='(16), call `deft'.
Else if major-mode is `deft-mode', bury the buffer.
Else if in a deft-opened file buffer, call `emcsadvntr/deft-quit'.
Else call `deft'."
      (interactive "P")
      (cond
       ((equal '(4) option) ; when using C-u
        (call-interactively #'deft-find-file))
       ((equal '(16) option) ; when using C-u C-u
        (call-interactively #'deft))
       ((derived-mode-p 'deft-mode)
        (bury-buffer))
       ;; If the user is in a file buffer opened by deft,
       ;; - `emcsadvntr/pre-deft-window-configuration' will be non-nil, AND
       ;; - the buffer name would have been added to `deft-auto-save-buffers'
       ;;   by the `deft-open-file' function (whether the user has chosen to
       ;;   auto save the deft files or not).
       ((and emcsadvntr/pre-deft-window-configuration
             (member (get-buffer (buffer-name)) deft-auto-save-buffers))
        (emcsadvntr/deft-quit))
       (t
        (call-interactively #'deft))))))

(provide 'markdownAndOrg)
;;; markdownAndOrg.el ends here
