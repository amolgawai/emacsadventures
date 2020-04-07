# Keybindings quickref

EVIL quickstart
---------------

    SPC               <leader>
	C-SPC             non-normal <leader>
    SPC q l           load last session
    C-z               toggle evil mode
	gc                comment/uncomment
	zm                fold-all
	zr                open-all
	zc                close current fold
	zo                open current fold

Generic
--------
    C-x M-c           `restart-emacs'
    <f7>              `emcsadvntr/deft-dwim'                   Start/quit deft
    C-<f8>             Invoke "Neotree"
    <f8>               Invoke treemacs
    <f9>              `imenu-list-smart-toggle'                show function list
    C-c i              counsel-imenu                            navigate function list
    <f12>              Format the current buffer
    C-=                Expand selected region
    C-'               `avy-goto-char-2'
    C-:               `avy-goto-char'
    M-g l             `avy-goto-line'
    M-g w             `avy-goto-word-1'
    C-c W             `wiki-summary'
    C-c a             `org-agenda'
    C-c b             `org-iswitchb'
    C-c c             `org-capture'
    C-c l             `org-store-link'
    C-c W              get summary from wikipedia
    C-x g             `magit-status'
    C-x t t           `treemacs'
    M-0               `treemacs-select-window'                was `digit-argument'
    M-x               `smex'                                  [now: `counsel-M-x']
    M-s O             `moccur'
    s-<f11>           `disable-active-themes'
    s-<f12>           `switch-theme'
    s-`               `eshell-toggle'                         was `other-frame'

Folding
--------
    C-c <up arrow>    Fold All
    C-c <down arrow>  UnFold All
    C-c <left arrow>  Fold Current
    C-c <right arrow> UnFold Current

Buffer/Window Navigation
--------
    C-M-<Left arrow>  Move to left frame
    C-M-<Right arrow> Move to right frame
    C-M-<Up arrow>    Move to top frame
    C-M-<Down arrow>> Move to bottom frame
    C-x o             Move to any frame                      Ace-window
    C-<next>          Move to next buffer/project            Centaur-tabs - next is PageUp
    C-<prior>         Move to previous buffer/project        Centaur-tabs - prior is PageDown

prog-mode-map: <f5>
--------
    <f5>              `emcsadvntr/compile-please'
    <C-f5>            `compile'

dired-mode-map
--------
    /                 `dired-narrow'
    I                 `dired-subtree-remove'                  was `dired-info'
    i                 `dired-subtree-insert'                  was `dired-maybe-insert-subdir'


isearch-mode-map
--------
    M-O               `isearch-moccur-all'
    M-o               `isearch-moccur'


neotree-mode-map
--------
    +                 `neotree-create-node'
    C                 `neotree-change-root'
    c                 `neotree-create-node'
    d                 `neotree-delete-node'                   was `#<byte-compiled lambda>'
    r                 `neotree-rename-node'
    <C-return>        `neotree-change-root'
