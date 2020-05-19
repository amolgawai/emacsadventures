;;; rust.el --- configuration for rust programming language  -*- lexical-binding: t; -*-
;; Copyright (C) 2019  Amol Gawai
;; Author: Amol Gawai <amol@doesnot.exist>
;; Keywords: languages
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
;; ref - https://www.reddit.com/r/rust/comments/a3da5g/my_entire_emacs_config_for_rust_in_fewer_than_20/
;;

;;; Code:


(use-package toml-mode
  :defer t
  :quelpa (toml-mode :fetcher github :repo "dryman/toml-mode"))

(use-package rustic
  :defer t)
  ;; :hook (rust-mode . (lambda ()
  ;;                       (lsp)
  ;;                       (lsp-ui-doc-mode)
  ;;                       (lsp-ui-sideline-mode)
  ;;                       (eldoc-mode)
  ;;                       (smart-dash-mode)))
  ;; :init
  ;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic)))

;; Add keybindings for interacting with Cargo
;; (use-package cargo
;;   :after rust-mode
;;   :hook (rust-mode . cargo-minor-mode))
;; add following at commandline  for cargo mode to work properly

;; In order to run cargo-process-fmt you need to have the rustfmt package installed.
;; rustup component add rustfmt-preview

;; In order to run cargo-process-check you need to have the cargo-check package installed.
;; cargo install cargo-check

;; In order to run cargo-process-clippy you need to have the clippy package installed.
;; cargo install clippy
;; or
;; rustup component add clippy-preview

;; In order to run cargo-process-{add,rm,upgrade} you need to have the cargo-edit package installed.
;; cargo install cargo-edit

(use-package flycheck-rust
  :defer t
  :quelpa (flycheck-rust :fetcher github :repo "flycheck/flycheck-rust")
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'rust)
;;; rust.el ends here
