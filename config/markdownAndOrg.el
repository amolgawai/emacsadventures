;;; markdownAndOrg.el --- Configure markdown and org modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Amol Gawai

;; Author: Amol Gawai <amolgawai@Amols-MBP>
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

;; Setup markdown and Org modes

;;; Code:

;; variable pitch for all the text modes
(add-hook 'text-mode-hook 'variable-pitch-mode)

;; adjust specific font faces for a perticular mode
;; ref - https://yoo2080.wordpress.com/2013/05/30/monospace-font-in-tables-and-source-code-blocks-in-org-mode-proportional-font-in-other-parts/
(defun emcsadvntr/adjoin-to-list-or-symbol (element list-or-symbol)
  "Use this helper function for adjusting font faces for specific blocks.

Used mainly for markdown and org mode to adjust fixed width for some faces
ELEMENT - pass the font attribute
LIST-OR-SYMBOL - pass the list of faces"
  (let ((list (if (not (listp list-or-symbol))
                  (list list-or-symbol)
                list-or-symbol)))
    (require 'cl-lib)
    (cl-adjoin element list)))

;; markdown settings
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (progn
    (setq markdown-make-gfm-checkboxes-buttons t)
    (setq markdown-command "multimarkdown"))
  :config
  (progn
    (add-hook 'markdown-mode-hook
              '(lambda ()
                 (mapc
                  (lambda (face)
                    (set-face-attribute
                     face nil
                     :inherit
                     (emcsadvntr/adjoin-to-list-or-symbol
                      'fixed-pitch
                      (face-attribute face :inherit))))
                  (list 'markdown-pre-face 'markdown-inline-code-face))))))

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1))))

;; org mode

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture))
  :config
  (progn
	;; basic
    (setq org-directory (expand-file-name "~/MyOrganiser")
		  org-default-notes-file (concat org-directory "/Inbox.org")
		  org-log-done t
		  org-fast-tag-selection-single-key t
          org-hide-emphasis-markers t
		  org-use-fast-todo-selection t)
                                        ;    (setq org-startup-truncated nil)
    (setq org-agenda-files (apply 'append
                                  (mapcar
                                   (lambda (directory)
                                     (directory-files-recursively
                                      directory org-agenda-file-regexp))
                                   '("~/MyOrganiser" "~/code/technotes"))))

    ;; beautification
    (setq org-startup-indented t
		  ;; org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
		  org-ellipsis "  " ;; folding symbol
		  org-pretty-entities t
		  org-hide-emphasis-markers t
		  ;; show actually italicized text instead of /italicized text/
		  org-fontify-whole-heading-line t
		  org-fontify-done-headline t
		  org-fontify-quote-and-verse-blocks t)
    ;; (custom-theme-set-faces
    ;;  'user
    ;;  '(variable-pitch ((t (:family "Source Sans Pro" :height 225 )))))
    ;;  '(fixed-pitch ((t ( :family "Source Code Pro" :slant normal :weight normal :height 1.0 :width normal)))))

	(add-hook 'org-mode-hook 'emcsadvntr/org-mode-hook)
    (defun emcsadvntr/org-mode-hook ()
      "Org-mode specific settings"
      (setq line-spacing 0.2) ;; Add more line padding for readability
      ;; header-line-format " ")
      (setq left-margin-width 1)
      (setq right-margin-width 2)
      (set-window-buffer nil (current-buffer))
      (variable-pitch-mode 1) ;; All fonts with variable pitch.
      (mapc
       (lambda (face)
         (set-face-attribute
          face nil
          :inherit
          (emcsadvntr/adjoin-to-list-or-symbol
           'fixed-pitch
           (face-attribute face :inherit))))
       (list 'org-code 'org-block 'org-table
             'org-block-background 'org-verbatim
             'org-meta-line
             'org-document-info-keyword)))
    ;; (lambda (face) ;; Other fonts with fixed-pitch.
    ;;   (set-face-attribute face nil :inherit 'fixed-pitch))
    ;; (list 'org-code
    ;;   	;; 'org-link
    ;;   	'org-block
    ;;   	'org-table
    ;;   	'org-verbatim
    ;;   	'org-block-begin-line
    ;;   	'org-block-end-line
    ;;   	'org-meta-line
    ;;   	'org-document-info-keyword))))

	;; GTD setup
    (setq org-todo-keywords
          '(
            (sequence "⚡ IDEA(i)" "☛ TODO(t)" "⚙ STARTED(s)" "☚ NEXT(n)" "⚑ WAITING(w)" "|" "✔ DONE(d)")
            (sequence "|" "✘ CANCELED(c)" "☟ DELEGATED(l)" "♺ SOMEDAY(f)")
            ))

    (setq org-todo-keyword-faces
          '(("⚡ IDEA" . (:foreground "Magenta"))
            ("☛ TODO" . org-todo)
            ("☚ NEXT" . org-warning)
            ("⚙ STARTED" . "yellow")
            ("⚑ WAITING" . (:foreground "Pink"))
            ("♺ SOMEDAY"  . (:foreground "#FFEF9F"))
            ("☟ DELEGATED" . (:inherit outline-2))
            ("✘ CANCELED" . (:inherit org-done :strike-through t))
            ("✔ DONE"     . (:inherit org-done :strike-through t))))

    (setq org-tag-persistent-alist '((:startgrouptag)
                                     ("GTD")
                                     (:grouptags)
                                     ("What") ("Where") ("Who") ("When")
                                     (:endgrouptag)
                                     (:startgrouptag)
                                     ("What")
                                     (:grouptags)
                                     ("Vision" . ?V) ("Goal" . ?G) ("AreaOfLife" . ?A) ("Project" . ?P)
                                     ("Profile") ("Checklist") ("Reference")
                                     (:endgrouptag)
                                     (:startgrouptag)
                                     ("Where")
                                     (:grouptags)
                                     ("@home") ("@work") ("@desk") ("@www") ("@book") ("@email") ("@call")
                                     (:endgrouptag)
                                     (:startgrouptag)
                                     ("Who")
                                     (:grouptags)
                                     ("self")
                                     (:endgrouptag)
                                     (:startgrouptag)
                                     ("When")
                                     (:grouptags)
                                     ("1_Now") ("2_Next") ("3_Later") ("4_Someday")
                                     (:endgrouptag)))

    (setq org-tags-exclude-from-inheritance (quote ("Goal" "Project")))
    ;; Agenda settings
    (setq org-agenda-ndays 7
          org-agenda-show-all-dates t
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-done t
          org-agenda-start-on-weekday nil
          org-agenda-restore-windows-after-quit t
          org-agenda-window-setup 'only-window
          org-agenda-tags-column -100
          org-deadline-warning-days 14
		  ;; org-agenda-block-separator nil
          ;; org-agenda-compact-blocks t
          org-agenda-start-with-log-mode t)

    (setq org-agenda-custom-commands
          '(("g" "Goals" tags "Goal"
             ((org-agenda-remove-tags t)
              (org-agenda-overriding-header "Goals")))
            ("p" "Projects"
             ((tags "+Project+TODO=\"⚙ STARTED\""
                    ((org-agenda-overriding-header "❖----------------Active Projects----------------------❖")))
              (tags "+Project-TODO=\"⚙ STARTED\""
                    ((org-agenda-overriding-header "❖----------------All other Projects----------------------❖"))))
			 ((org-agenda-remove-tags t)
			  (org-agenda-overriding-header "Project List")))
            ("na" "Current and Next Actions"
             ((tags "-Project+TODO=\"⚙ STARTED\""
                    ((org-agenda-overriding-header "❖----------------Current Actions----------------------❖")))
              (tags "-Project+TODO=\"☚ NEXT\""
                    ((org-agenda-overriding-header "❖----------------Next Actions----------------------❖"))))
			 ((org-agenda-overriding-header "Action List")))
            ("nn" "Next Actions" todo "☚ NEXT"
			 ((org-agenda-overriding-header "Next Actions")))
            ("d" "Agenda + Next Actions + All" ((agenda) (todo "☚ NEXT") (alltodo "")))))

    ;; Refiling - Ref -> https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
    (setq org-refile-targets '((org-agenda-files :maxlevel . 3))
          org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes 'confirm)


    ;; capture
    (setq org-reverse-note-order t)

    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline (concat org-directory "/Inbox.org") "Tasks")
             "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
            ("i" "Idea" entry (file+headline (concat org-directory "/Inbox.org") "Ideas")
             "* IDEA %?\nAdded: %U\n" :prepend t :kill-buffer t)))
    (setq org-use-speed-commands t
          org-src-fontify-natively t
          org-src-tab-acts-natively t)))

;;  Supercharge your Org daily/weekly agenda by grouping items
;; https://github.com/alphapapa/org-super-agenda
(use-package org-super-agenda
  :init
  (setq org-super-agenda-groups
        '((:name "Goals"
                 :tag "Goal")
          (:name "Active Projects"
                 :and (:todo "STARTED" :tag "Project"))))
  :config
  (org-super-agenda-mode))
;; '((:name "Next Items"
;;          :time-grid t
;;          :tag ("NEXT" "outbox"))
;;   (:name "Important"
;;          :priority "A")
;;   (:name "Quick Picks"
;;          :effort< "0:30")
;;   (:priority<= "B"
;;                :scheduled future
;;                :order 1))))

;; Org Cliplink: insert the link in the clipboard as an org link. Adds the
;; title of the page as the description
;; https://github.com/rexim/org-cliplink
(use-package org-cliplink
  :bind (:map org-mode-map
              ;; "C-c C-l" is bound to `org-insert-link' by default
              ;; "C-c C-L" is bound to `org-cliplink'
              ("C-c C-S-l" . org-cliplink)))

;; easily open org files from ivy interface
;; ref - https://github.com/akirak/ivy-omni-org
(use-package ivy-omni-org
  :custom
  (ivy-omni-org-file-sources '(org-agenda-files)))

;; supercharge the agenda with org-super-agenda
;; (use-package org-super-agenda
;;   :ensure t
;;   :after org-agenda
;;   :init
;;   (setq org-super-agenda-groups
;;       '((:name "Next Items"
;;                :time-grid t
;; 			   :todo "NEXT")
;;         (:name "Important"
;;                :priority "A")
;;         (:name "Quick Picks"
;;                :effort< "0:30")
;;         (:priority<= "B"
;;                      :scheduled future
;;                      :order 1)))
;;   :config
;;   (org-super-agenda-mode))

;; beautiful bullets for org mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-ac
  :ensure t
  :init (progn
          (require 'org-ac)
          (org-ac/config-default)))

(use-package org-journal
  :after org
  :custom
  (org-journal-dir (concat (file-name-as-directory org-directory) "journal"))
  (org-journal-file-format "%Y/%m/%Y%m%d")
  (org-journal-date-format "%A, %Y-%m-%d")
  ;; (org-journal-encrypt-journal t)
  ;; (org-journal-enable-encryption nil)
  (org-journal-enable-agenda-integration t)
  :bind
  ("C-x j" . org-journal-new-entry))

(use-package org-sidebar
  :ensure t)

;; deft for managing notes
(use-package deft
  :ensure t
  :after org
  :bind ("<f7>" . emcsadvntr/deft-dwim)
  :custom
  (deft-directory org-directory)
  (deft-recursive t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase)))
  (deft-org-mode-title-prefix t)
  (deft-extensions '("org" "txt" "text" "md" "markdown" "org.gpg"))
  (deft-default-extension "org")
  :config
  (progn
    ;; better deft invoke and quite
    ;; ref - https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-deft.el
    (defvar emcsadvntr/pre-deft-window-configuration nil
      "Variable to store the window configuration before `deft' was called.")

    ;; Advise deft to save window config
    (defun emcsadvntr/deft-dwim-save-windows (orig-fun &rest args)
      (setq emcsadvntr/pre-deft-window-configuration (current-window-configuration))
      (apply orig-fun args))
    (advice-add 'deft :around #'emcsadvntr/deft-dwim-save-windows)
    (advice-add 'deft-parse-title :around #'emcsadvntr/parse-title-with-directory-prepended)

    ;; better names in deft listing including directory name
    ;; ref - https://jingsi.space/post/2017/04/05/organizing-a-complex-directory-for-emacs-org-mode-and-deft/
    (defun emcsadvntr/strip-quotes (str)
      (cond ((string-match "\"\\(.+\\)\"" str) (match-string 1 str))
            ((string-match "'\\(.+\\)'" str) (match-string 1 str))
            (t str)))

    (defun emcsadvntr/parse-title-from-front-matter-data (str)
      (if (string-match "^title: \\(.+\\)" str)
          (let* ((title-text (emcsadvntr/strip-quotes (match-string 1 str)))
                 (is-draft (string-match "^draft: true" str)))
            (concat (if is-draft "[DRAFT] " "") title-text))))

    (defun emcsadvntr/deft-file-relative-directory (filename)
      (file-name-directory (file-relative-name filename deft-directory)))

    (defun emcsadvntr/title-prefix-from-file-name (filename)
      (let ((reldir (emcsadvntr/deft-file-relative-directory filename)))
        (if reldir
            (concat (directory-file-name reldir) " > "))))

    (defun emcsadvntr/parse-title-with-directory-prepended (orig &rest args)
      (let ((str (nth 1 args))
            (filename (car args)))
        (concat
         (emcsadvntr/title-prefix-from-file-name filename)
         (let ((nondir (file-name-nondirectory filename)))
           (if (or (string-prefix-p "README" nondir)
                   (string-suffix-p ".txt" filename))
               nondir
             (if (string-prefix-p "---\n" str)
                 (emcsadvntr/parse-title-from-front-matter-data
                  (car (split-string (substring str 4) "\n---\n")))
               (apply orig args)))))))

    (defun emcsadvntr/deft-quit ()
      "Save buffer, kill both the deft-opened file buffer and the *Deft* buffer,
and restore the window config to the way it was before deft was invoked."
      (interactive)
      (let ((buf (buffer-name)))
        (save-buffer)
        (kill-buffer buf)
        (delq buf deft-auto-save-buffers) ; Remove the buffer from `deft-auto-save-buffers'
        (kill-buffer "*Deft*")
        (when (window-configuration-p emcsadvntr/pre-deft-window-configuration)
          (set-window-configuration emcsadvntr/pre-deft-window-configuration)
          ;; Reset `emcsadvntr/pre-deft-window-configuration' back to `nil' because
          ;; that value is one of the criteria to check if the user is currently
          ;; editing a deft-opened file
          (setq emcsadvntr/pre-deft-window-configuration nil))))

    (defun emcsadvntr/deft-dwim (option)
      "Launch deft or quit a deft opened file based on context.
If OPTION is \\='(4), call `deft-find-file'.
Else if OPTION is \\='(16), call `deft'.
Else if major-mode is `deft-mode', bury the buffer.
Else if in a deft-opened file buffer, call `emcsadvntr/deft-quit'.
Else call `deft'."
      (interactive "P")
      (cond
       ((equal '(4) option) ; when using C-u
        (call-interactively #'deft-find-file))
       ((equal '(16) option) ; when using C-u C-u
        (call-interactively #'deft))
       ((derived-mode-p 'deft-mode)
        (bury-buffer))
       ;; If the user is in a file buffer opened by deft,
       ;; - `emcsadvntr/pre-deft-window-configuration' will be non-nil, AND
       ;; - the buffer name would have been added to `deft-auto-save-buffers'
       ;;   by the `deft-open-file' function (whether the user has chosen to
       ;;   auto save the deft files or not).
       ((and emcsadvntr/pre-deft-window-configuration
             (member (get-buffer (buffer-name)) deft-auto-save-buffers))
        (emcsadvntr/deft-quit))
       (t
        (call-interactively #'deft))))))

;; taskpaper mode to support taskpaper
(use-package taskpaper-mode
  :ensure t
  :mode ("\\.todo\\'" . taskpaper-mode))

(provide 'markdownAndOrg)
;;; markdownAndOrg.el ends here
