;;; pythonWithElpy.el --- setup python with elpy     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Amol Gawai

;; Author: Amol Gawai <amol@doesnot.exist>
;; Keywords: abbrev, bib

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

;; elpy provides a complete IDE for python
;; Find more at - https://github.com/jorgenschaefer/elpy

;;; Code:

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;; pyenv settings
;; ref - https://github.com/howardabrams/dot-files/blob/master/emacs-python.org
(use-package pyenv-mode
  :ensure t
  :config
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))

  ;; ref - http://rakan.me/emacs/python-dev-with-emacs-and-pyenv/
  (defvar pyenv-current-version nil nil)

  (defun pyenv-init()
    "Initialize pyenv's current version to the global one."
    (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
      (message (concat "Setting pyenv version to " global-pyenv))
      (pyenv-mode-set global-pyenv)
      (setq pyenv-current-version global-pyenv)))

  (add-hook 'after-init-hook 'pyenv-init)
  (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
  (add-hook 'python-mode-hook 'pyenv-mode))

(use-package pyenv-mode-auto
  :ensure t)

;; package managment for python
(use-package poetry
  :ensure t)

;; code formatting
(use-package blacken
  :ensure t
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

;; Convert from python 2 to 3
(defun python-2to3-current-file ()
  "Convert current buffer from python 2 to python 3.
This command calls python3's script 「2to3」.
URL `http://ergoemacs.org/emacs/elisp_python_2to3.html'
Version 2016-02-16"
  (interactive)
  (let* (
         (fName (buffer-file-name))
         (fSuffix (file-name-extension fName)))
    (when (buffer-modified-p)
      (save-buffer))
    (if (or (string-equal fSuffix "py") (string-equal fSuffix "py3"))
        (progn
          (shell-command (format "2to3 -w %s" fName))
          (revert-buffer  "IGNORE-AUTO" "NOCONFIRM" "PRESERVE-MODES"))
      (error "File 「%s」 doesn't end in “.py” or “.py3”" fName))))


(provide 'pythonWithElpy)
;;; pythonWithElpy.el ends here
