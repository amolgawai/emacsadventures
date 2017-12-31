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
(require 'productivity)
(require 'ivycounselswiper)
(require 'commoncoding)
;; (require 'helmLikeiDo)
(require 'markdownAndOrg)
(require 'fileNav)
(require 'dirNav)
(require 'pythonWithAnaconda)
(require 'golang)

(require 'load-directory)
(load-directory (locate-user-emacs-file "emacsadventures/private"))

(provide 'loadMyConfig)
;;; loadMyConfig.el ends here
