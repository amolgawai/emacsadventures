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

;; source - https://github.com/howardabrams/dot-files/blob/master/emacs-python.org

;; basic python settings
(use-package python
  :mode ("\\.py\\'" . python-mode)
  ;; ("\\.wsgi$" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (setq-default indent-tabs-mode nil)
  :hook (python-mode-hook . smartparens-mode)
  :config
  (setq python-indent-offset 4)
  ;; ipython support, also remove weird character on ipython prompt
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter-args "-i --simple-prompt --pprint")))
;; (add-hook 'python-mode-hook 'color-identifiers-mode))

;; emacs ipython notebook (jupyter in emacs)
(use-package ein
  :ensure t)

;; docstring helper
(use-package python-docstring
  :ensure t
  :config
  (python-docstring-install)
  :diminish python-docstring-mode)


(use-package jedi
  :ensure t
  :init
  (add-to-list 'company-backends 'company-jedi)
  :config
  (use-package company-jedi
    :ensure t
	:hook (python-mode-hook . (lambda () (add-to-list 'company-backends 'company-jedi)))
    :init
    (setq company-jedi-python-bin "python")))

;; the python IDE
(use-package elpy
  :ensure t
  :defer t
  :hook (elpy-mode-hook . (lambda () (elpy-shell-toggle-dedicated-shell 1)))
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (defalias 'workon 'pyvenv-workon)
  ;; (setq elpy-rpc-backend "jedi")
  ;; use flycheck instead of flymake
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (setq elpy-modules
        '(elpy-module-company
          elpy-module-eldoc
          elpy-module-pyvenv
          elpy-module-sane-defaults)
        elpy-shell-echo-input nil
        elpy-shell-starting-directory 'current-directory
        elpy-shell-echo-output nil
        elpy-rpc-virtualenv-path 'current))

;; so that elpy plays well with virtual environments
(use-package pyenv-mode-auto
  :ensure t
  :config
  (let ((workon-home (expand-file-name "~/.pyenv/versions")))
    (setenv "WORKON_HOME" workon-home)
    (setenv "VIRTUALENVWRAPPER_HOOK_DIR" workon-home)))

;; package managment for python
(use-package poetry
  :ensure t
  :diminish t)

;; code formatting
(use-package blacken
  :ensure t
  :diminish t
  :hook (python-mode-hook . blacken-mode))

;; Convert from python 2 to 3
;; works only in python 3 and when 2to3 is installed via pip
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
