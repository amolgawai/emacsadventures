; load theme - make sure the theme is installed
(add-hook 'after-init-hook (lambda () (load-theme 'spacemacs-dark)))
;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)
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

;; function list with imenu-list
(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
(setq imenu-list-auto-resize t)
;; save the desktop
(desktop-save-mode 1)
;; line numbers
(global-linum-mode t)
;; split the window vertically on right - useful for wide screen
(setq split-width-threshold nil)
(setq split-width-threshold 0)

;; golang
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
            (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
            (if (not (string-match "go" compile-command))   ; set compile command default
                (set (make-local-variable 'compile-command)
                     "go build -v && go test -v && go vet"))
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

;; set the initial window size - adjust width and height accordingly
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (when (display-graphic-p)
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))
(set-frame-size-according-to-resolution)

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
;; Helm customization
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z