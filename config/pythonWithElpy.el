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
  :custom ((indent-tabs-mode nil)
           (python-indent-offset 4)
           (tab-width 4))
  :init
  (setenv "PYTHONIOENCODING" "utf-8")
  (let ((workon-home (expand-file-name "~/.pyenv/versions")))
    (setenv "WORKON_HOME" workon-home)
    (setenv "VIRTUALENVWRAPPER_HOOK_DIR" workon-home))
  :hook ((python-mode . smartparens-mode)
         (python-mode . (lambda ()
                          ((setq indent-tabs-mode nil
                                 python-indent 4
                                 tab-width 4)
                           (untabify (point-min) (point-max)))))
         (inferior-python-mode . (lambda() (setq company-mode -1))))
  ;; (global-company-mode -1
  ;;                      company-box-mode -1
  ;;                      company-statistics-mode -1
  ;; company-mode -1))))
  :config
  ;; ipython support, also remove weird character on ipython prompt
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args ""
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")))

  ;; emacs ipython notebook (jupyter in emacs)
  (use-package ein
    :defer t)

  ;; docstring helper
  (use-package python-docstring
    :defer t
    :config
    (python-docstring-install)
    :diminish python-docstring-mode)

  ;; suggest imports automatically
  ;; make sure to add following in the respective environments
  ;; pip install importmagic epc
  (use-package importmagic
    :hook (python-mode . importmagic-mode))

  ;; auto generate docstring
  (use-package sphinx-doc
    :defer t
    :hook ((python-mode . sphinx-doc-mode)))

  (use-package jedi
    :defer t)

  (use-package company-jedi
    :defer t
    :hook (python-mode-hook . (lambda () (add-to-list 'company-backends 'company-jedi)))
    :init
    (setq company-jedi-python-bin "python"))

  ;; the python IDE
  (use-package elpy
    :defer t
    :hook ((elpy-mode . (lambda () (elpy-shell-toggle-dedicated-shell 1)))
           ;; (pyenv-mode . elpy-rpc-restart)
           (elpy-mode . (lambda ()
                          (add-hook 'before-save-hook
                                    'elpy-black-fix-code nil t))))
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

  ;; pyenv for emacs
  ;; (use-package pyenv-mode
  ;;   :init
  ;;   (let ((workon-home (expand-file-name "~/.pyenv/versions")))
  ;;     (setenv "WORKON_HOME" workon-home)
  ;;     (setenv "VIRTUALENVWRAPPER_HOOK_DIR" workon-home))
  ;;   :config
  ;;   (defun projectile-pyenv-mode-set ()
  ;;     "Set pyenv version matching project name."
  ;;     (let ((project (projectile-project-name)))
  ;;       (if (member project (pyenv-mode-versions))
  ;;           (pyenv-mode-set project)
  ;;         (pyenv-mode-unset))))

  ;;   (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
  ;;   (add-hook 'python-mode-hook 'pyenv-mode))

  ;; so that elpy plays well with virtual environments
  ;; (use-package pyenv-mode-auto)
  ;; :config
  ;; (let ((workon-home (expand-file-name "~/.pyenv/versions")))
  ;;   (setenv "WORKON_HOME" workon-home)
  ;;   (setenv "VIRTUALENVWRAPPER_HOOK_DIR" workon-home)))

  ;; (use-package auto-virtualenv
  ;;   :hook (python-mode-hook . 'auto-virtualenv-set-virtualenv))

  ;; package managment for python
  (use-package poetry
    :defer t
    :diminish t)

  ;; code formatting
  (use-package blacken
    :defer t
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
