;;; shackleSetup.el --- shackle helps in managing variety of buffers w.r.t size and reuse etc.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Amol Gawai

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

;; ref - https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-shackle.el

;;; Code:





;; Shackle
;; https://github.com/wasamasa/shackle

(use-package shackle
  :ensure t
;;  :if (not (bound-and-true-p disable-pkg-shackle))
  :config
  (progn
    (setq shackle-lighter "")
    (setq shackle-select-reused-windows nil) ; default nil
    (setq shackle-default-alignment 'below) ; default below
    (setq shackle-default-size 0.4) ; default 0.5

    (setq shackle-rules
          ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
          '((compilation-mode              :select nil                                               )
            ("*undo-tree*"                                                    :size 0.25 :align right)
            ("*eshell*"                    :select t                          :other t               )
            ("*Shell Command Output*"      :select nil                                               )
            ("\\*Async Shell.*\\*" :regexp t :ignore t                                                 )
            (occur-mode                    :select nil                                   :align t    )
            ("*Help*"                      :select t   :inhibit-window-quit t :other t               )
            ("*Completions*"                                                  :size 0.3  :align t    )
            ("*Messages*"                  :select nil :inhibit-window-quit t :other t               )
            ("\\*[Wo]*Man.*\\*"    :regexp t :select t   :inhibit-window-quit t :other t               )
            ("\\*poporg.*\\*"      :regexp t :select t                          :other t               )
            ("\\`\\*helm.*?\\*\\'"   :regexp t                                    :size 0.3  :align t    )
            ("*Calendar*"                  :select t                          :size 0.3  :align below)
            ("*info*"                      :select t   :inhibit-window-quit t                         :same t)
            (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
            (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
            ))

    (shackle-mode 1)))

;; Elements of the `shackle-rules' alist:
;;
;; |-----------+------------------------+--------------------------------------------------|
;; | CONDITION | symbol                 | Major mode of the buffer to match                |
;; |           | string                 | Name of the buffer                               |
;; |           |                        | - which can be turned into regexp matching       |
;; |           |                        | by using the :regexp key with a value of t       |
;; |           |                        | in the key-value part                            |
;; |           | list of either         | a list groups either symbols or strings          |
;; |           | symbol or string       | (as described earlier) while requiring at        |
;; |           |                        | least one element to match                       |
;; |           | t                      | t as the fallback rule to follow when no         |
;; |           |                        | other match succeeds.                            |
;; |           |                        | If you set up a fallback rule, make sure         |
;; |           |                        | it's the last rule in shackle-rules,             |
;; |           |                        | otherwise it will always be used.                |
;; |-----------+------------------------+--------------------------------------------------|
;; | KEY-VALUE | :select t              | Select the popped up window. The                 |
;; |           |                        | `shackle-select-reused-windows' option makes     |
;; |           |                        | this the default for windows already             |
;; |           |                        | displaying the buffer.                           |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :inhibit-window-quit t | Special buffers usually have `q' bound to        |
;; |           |                        | `quit-window' which commonly buries the buffer   |
;; |           |                        | and deletes the window. This option inhibits the |
;; |           |                        | latter which is especially useful in combination |
;; |           |                        | with :same, but can also be used with other keys |
;; |           |                        | like :other as well.                             |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :ignore t              | Skip handling the display of the buffer in       |
;; |           |                        | question. Keep in mind that while this avoids    |
;; |           |                        | switching buffers, popping up windows and        |
;; |           |                        | displaying frames, it does not inhibit what may  |
;; |           |                        | have preceded this command, such as the          |
;; |           |                        | creation/update of the buffer to be displayed.   |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :same t                | Display buffer in the current window.            |
;; |           | :popup t               | Pop up a new window instead of displaying        |
;; |           | *mutually exclusive*   | the buffer in the current one.                   |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :other t               | Reuse the window `other-window' would select if  |
;; |           | *must not be used      | there's more than one window open, otherwise pop |
;; |           | with :align, :size*    | up a new window. When used in combination with   |
;; |           |                        | the :frame key, do the equivalent to             |
;; |           |                        | other-frame or a new frame                       |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :align                 | Align a new window at the respective side of     |
;; |           | 'above, 'below,        | the current frame or with the default alignment  |
;; |           | 'left, 'right,         | (customizable with `shackle-default-alignment')  |
;; |           | or t (default)         | by deleting every other window than the          |
;; |           |                        | currently selected one, then wait for the window |
;; |           |                        | to be "dealt" with. This can either happen by    |
;; |           |                        | burying its buffer with q or by deleting its     |
;; |           |                        | window with C-x 0.                               |
;; |           | :size                  | Aligned window use a default ratio of 0.5 to     |
;; |           | a floating point       | split up the original window in half             |
;; |           | value between 0 and 1  | (customizable with `shackle-default-size'), the  |
;; |           | is interpreted as a    | size can be changed on a per-case basis by       |
;; |           | ratio. An integer >=1  | providing a different floating point value like  |
;; |           | is interpreted as a    | 0.33 to make it occupy a third of the original   |
;; |           | number of lines.       | window's size.                                   |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :frame t               | Pop buffer to a frame instead of a window.       |
;; |-----------+------------------------+--------------------------------------------------|
;;
;; http://emacs.stackexchange.com/a/13687/115
;; Don't show Async Shell Command buffers

(provide 'shackleSetup)
;;; shackleSetup.el ends here
