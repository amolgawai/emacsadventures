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

;;; Code:

;; Load this file from init.el

;Load configuration files
(load-file "~/.emacs.d/emacsadventures/initWithSaneDefaults.el")
(load-file "~/.emacs.d/emacsadventures/packages.el")
(load-file "~/.emacs.d/emacsadventures/appearance.el")
(load-file "~/.emacs.d/emacsadventures/commoncoding.el")
(load-file "~/.emacs.d/emacsadventures/productivity.el")
(load-file "~/.emacs.d/emacsadventures/ivycounselswiper.el")

(provide 'loadMyConfig)
;;; loadMyConfig.el ends here
