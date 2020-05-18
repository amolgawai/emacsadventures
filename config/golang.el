;;; golang.el --- setup golang development environment  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Amol Gawai

;; Author: Amol Gawai <amol@doesnot.exist>
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

;; setup development environment for golang
;; ref - https://sourcegraph.com/github.com/Schnouki/dotfiles/-/blob/emacs/init-40-go.el#L5

;;; Code:

(use-package go-mode
  :ensure t
  :defer t
  :mode "\\.go\\'"
  :commands (godoc gofmt gofmt-before-save)
  :bind (:map go-mode-map
              ("C-c C-k" . godoc)
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c C-n" . go-rename)
              ("C-c C-t" . go-add-tags)
              )
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  :config
  (progn
    (add-to-list 'go-guess-gopath-functions #'schnouki/go-hellogopher-gopath)
    (defadvice go-root-and-paths (around schnouki/go-root-and-paths)
      (let* ((root-and-paths ad-do-it)
             (root (car root-and-paths))
             (env-paths (cdr root-and-paths))
             (guessed-paths (split-string (go-guess-gopath) path-separator)))
        (setq ad-return-value (cons root (append guessed-paths env-paths)))))
    (ad-activate 'go-root-and-paths)
    (add-hook 'go-mode-hook #'go-set-project)
    (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
    (add-hook 'go-mode-hook ; set compile command default
              (lambda()
                (if (not (string-match "go" compile-command))
                    (set (make-local-variable 'compile-command)
                         "go build -v && go test -v && go vet"))))
    (setq tab-width 4)
    (setq indent-tabs-mode 1)))

(defun schnouki/go-hellogopher-gopath ()
  (let ((d (locate-dominating-file buffer-file-name ".GOPATH")))
    (if d
        (list (concat d
                      (file-name-as-directory ".GOPATH"))))))

;; (use-package company-go
;;   :ensure t
;;   :commands company-go
;;   :init (add-to-list 'company-backends 'company-go))

(use-package go-autocomplete
  :defer t)

(use-package go-eldoc
  :defer t
  :commands go-eldoc-setup
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-guru
  :defer t
  :hook (go-mode-hook . go-guru-hl-identifier-mode)
  :commands go-guru-hl-identifier-mode)

(use-package go-rename
  :defer t
  :commands go-rename)

(use-package go-add-tags
  :defer t
  :commands go-add-tags)

(use-package godoctor
  :defer t
  :bind (:map go-mode-map
              ("C-c d d" . godoctor-godoc)
              ("C-c d e" . godoctor-extract)
              ("C-c d r" . godoctor-rename)
              ("C-c d t" . godoctor-toggle)))

;; Patched versions of go-packages-native and go-packages-go-list that strip
;; "vendor" dirs (https://github.com/dominikh/go-mode.el/issues/135)
(defun schnouki/go-packages-strip-vendor (packages)
  (mapcar (lambda (pkg)
            (if (string-match "/vendor/\\(.*\\)" pkg)
                (match-string 1 pkg)
              pkg))
          packages))
(defun schnouki/go-packages-native-without-vendor ()
  "Return a list of all installed Go packages, stripping vendor directories."
  (schnouki/go-packages-strip-vendor (go-packages-native)))
(defun schnouki/go-packages-go-list-without-vendor ()
  "Return a list of all Go packages, using `go list', stripping vendor directories."
  (schnouki/go-packages-strip-vendor (go-packages-go-list)))
(setq go-packages-function 'schnouki/go-packages-go-list-without-vendor)


(provide 'golang)
;;; golang.el ends here
