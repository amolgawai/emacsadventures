;; load theme - make sure the theme is installed
(add-hook 'after-init-hook (lambda () (load-theme 'spacemacs-dark)))

;; Tab Completion of local names - Bind hippie-expand
(global-set-key [(meta f1)] (make-hippie-expand-function
                               '(try-expand-dabbrev-visible
                                 try-expand-dabbrev
                                 try-expand-dabbrev-all-buffers) t))
;; auto indentation
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'c-mode 'set-newline-and-indent)

;; code folding using hs-minor-mode
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

;; function list    
(setq imenu-list-auto-resize t)
;; save the desktop
(desktop-save-mode 1)
;; line numbers
(global-linum-mode t)
;; split the window vertically on right - useful for wide screen
(setq split-width-threshold nil)
(setq split-width-threshold 0)
;; ECB with multiple framews
(defun ecb-activated-in-selected-frame ()
  "A hack to use ECB in multiple frames.
It first deactivates ECB, then activate it in current frame."
  (interactive)
  (let ((current-frame (selected-frame)))
                                        ; The frame foucs change when activating or deactivating ECB is weird, so
                                        ; activate current selected frame explicitly.
    (if (and (boundp 'ecb-minor-mode) (ecb-minor-mode))
        (ecb-deactivate)
      )
    (select-frame current-frame)
    (ecb-activate)
    )
  )