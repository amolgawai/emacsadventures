;;; packages.el --- tempalte for managing  packages from various repositories  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  
;; Ref - https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
;; Author:  <AGI5@LT4-FHM1-CEM>
;; Keywords: lisp, convenience,

; list the packages you want
(setq package-list '(package1 package2))

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("quelpa" . "https://github.com/quelpa/quelpa")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
