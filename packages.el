;;; packages.el --- tempalte for managing  packages from various repositories  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  
;; Ref - https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
;; Author:  Amol Gawai
;; Keywords: lisp, convenience,


; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("quelpa" . "https://github.com/quelpa/quelpa")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
;; avoid problems with files newer than their byte-compiled counterparts
;; it's better a lower startup than load an outdated and maybe bugged package
(setq load-prefer-newer t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; If you want to install multiple packages at once, creat a list
;; use-package is much better alternative to reduce startup delay
; list the packages you want
;;(setq package-list '(package1 package2))

; install the missing packages
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))
