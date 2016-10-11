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