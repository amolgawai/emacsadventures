
;; set the frame size in win32
(add-hook 'after-init-hook '(lambda () (w32-send-sys-command #xf030)))
;; set the font for all the frames

;; Tab Completion of local names - Bind hippie-expand
(global-set-key [(meta f1)] (make-hippie-expand-function
                             '(try-expand-dabbrev-visible
                               try-expand-dabbrev
                               try-expand-dabbrev-all-buffers) t))
;; auto indentation
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'c-mode 'set-newline-and-indent)

;; reset winner mode keys so that hide shows can bind them
(global-set-key [f7] 'winner-undo)
(global-set-key [C-f7] 'winner-redo)
(eval-after-load "winner"
  '(define-key winner-mode-map (kbd "C-c <right>") nil))
(eval-after-load "winner"
  '(define-key winner-mode-map (kbd "C-c <left>") nil))


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
