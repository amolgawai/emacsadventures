; load theme - make sure the theme is installed
(add-hook 'after-init-hook (lambda () (load-theme 'spacemacs-dark)))

;; set the frame size in win32
(add-hook 'after-init-hook '(lambda () (w32-send-sys-command #xf030)))
;; set the font for all the frames
(add-to-list 'default-frame-alist
             '(font . "Source Code Pro-12"))
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
    ))
  ;; Convert from python 2 to 3
  (defun python-2to3-current-file ()
    "Convert current buffer from python 2 to python 3.
This command calls python3's script 「2to3」.
URL `http://ergoemacs.org/emacs/elisp_python_2to3.html'
Version 2016-02-16"
    (interactive)
    (let* (
           (fName (buffer-file-name))
           (fSuffix (file-name-extension fName)))
      (when (buffer-modified-p)
        (save-buffer))
      (if (or (string-equal fSuffix "py") (string-equal fSuffix "py3"))
          (progn
            (shell-command (format "2to3 -w %s" fName))
            (revert-buffer  "IGNORE-AUTO" "NOCONFIRM" "PRESERVE-MODES"))
        (error "file 「%s」 doesn't end in “.py” or “.py3”." fName))))
  )