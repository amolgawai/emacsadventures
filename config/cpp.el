;;; cpp.el --- configuration for c++                 -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Amol Gawai

;; Author: Amol Gawai <amol@doesnot.exist>
;; Keywords:

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

;; configuration for c and cpp coding

;;; Code:

(c-set-offset 'substatement-open 0)
(c-set-offset 'innamespace 0)
(c-set-offset 'brace-list-open 0)
(setq c-basic-offset 4)

(setq lsp-clients-clangd-args
	  '("-j=8"
		"--header-insertion=never"
		"--all-scopes-completion"
		"--background-index"
		"--clang-tidy"
		"--compile-commands-dir=build"
		"--cross-file-rename"
        "-log=error"
		"--suggest-missing-includes"))

(use-package clang-format+
  :straight (clang-format+ :type git :host github
						   :repo "SavchenkoValeriy/emacs-clang-format-plus"))

(use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode))

(use-package cmake-mode)

(provide 'cpp)
;;; cpp.el ends here
