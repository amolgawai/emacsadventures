;;; loadMyConfig.el --- Load all my configurations   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Amol Gawai

;; Author: Amol Gawai;; loads all files in my configuration. <amol@doesnot.exist>
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

;; This file loads all my Eacs configurations.  oad this file in the init.el
;; Load this file from init.el - (load-file "~/.emacs.d/emacsadventures/loadMyConfig.el")

;;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
;; (setq gc-cons-threshold (* 50 1000 1000))

(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;; measure startup time
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; add the directory to load path
(add-to-list 'load-path (locate-user-emacs-file "emacsadventures/config"))

;;Load configuration files
(require 'bootstrapPackaging)
(require 'appearance)
(require 'initWithSaneDefaults)
(require 'evilConfig)
(require 'productivity)
(require 'shackleSetup)
(require 'ivycounselswiper)
(require 'commoncoding)
;; (require 'helmLikeiDo)
(require 'markdownAndOrg)
(require 'fileNav)
(require 'dirNav)
(require 'windowNav)
;;(require 'pythonWithAnaconda)
(require 'cpp)
(require 'pythonWithElpy)
(require 'golang)
(require 'rust)
(require 'webdev)
(require 'keymapWithGeneral)

;; Load private config
(defun my-load-all-in-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))
(my-load-all-in-directory (locate-user-emacs-file "emacsadventures/private"))

;; load keybindings in a buffer
(condition-case err
    (let ((buffer (get-buffer-create "*emacsadventures-keyref*")))
      (with-current-buffer buffer
        (insert-file-contents (locate-user-emacs-file "emacsadventures/keybindings.md"))
        (markdown-mode))
      (switch-to-buffer "*emacsadventures-keyref*"))
  (error (message "%s" error-message-string err)))


;; Make gc pauses faster by decreasing the threshold.
;; (setq gc-cons-threshold (* 2 1000 1000))

;; after startup, it is important you reset this to some reasonable default. A large
;; gc-cons-threshold will cause freezing and stuttering during long-term
;; interactive use. I find these are nice defaults:
(add-hook 'emacs-startup-hook
          (lambda()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1
                  file-name-handler-alist last-file-name-handler-alist)))

(provide 'loadMyConfig)
;;; loadMyConfig.el ends here
