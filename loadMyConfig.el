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

;; add the directory to load path
(add-to-list 'load-path (locate-user-emacs-file "emacsadventures/config"))
                                        ;Load configuration files
(require 'bootstrapPackaging)
(require 'appearance)
(require 'initWithSaneDefaults)
(require 'evilConfig)
(require 'keymapWithGeneral)
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
(require 'pythonWithElpy)
(require 'golang)
(require 'rust)

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
        (markdown-mode)))
  (error (message "%s" error-message-string err)))

(provide 'loadMyConfig)
;;; loadMyConfig.el ends here
