emacsadventures
===============

This is my adventure of setting up emacs for everyday use. I was inspired by [Spacemacs](https://github.com/syl20bnr/spacemacs) and [super-emacs](https://github.com/myTerminal/super-emacs).
My goals for the configuration are
* Asthetic - an eye pleasing emacs
* Fast and productive
* emacsy - keep emacs behavior and add only when necessary

Screenshots
-----------

Let the pictures do talking...
![Config In Action](/screenshots/InAction.png)
![With Spaceline](/screenshots/WithSpaceLine.png)

How To Install
--------------
If you want to use my configuration, following are the steps
* Install [Source Code Pro Fonts](https://github.com/adobe-fonts/source-code-pro/)
* Clone this repository
* Link the resulting directory from your .emacs.d
* Add following line to your init.el
`(load "~/.emacs.d/emacsadventures/loadMyConfig.el")`
* Start emacs
* look for any errors as some packages may fail to install
* Install the failed packages manually by running following
`M-x packages-install-package`
* Install all-the-icons by running
`M-x all-the-icons-install-fonts`
* On windows, you might need to install them from a resulting directory
* Restart emacs and begin your adventure

How To Customize
----------------
You can customize the configuration the usual emacs way.
* Start in the loadMyConfig.el
* comment out sections which you do not want
* Add yout personal configuration in the 'private' directory
* The personal configuration is loaded as a last step

Quick Start
-----
Here are some hints about the features
* --Invoke the directory explorer "Neotree" - `F8`--
* Invoke [Treemacs - a tree layout file explorer for Emacs](https://github.com/Alexander-Miller/treemacs) `C-x t t`
* Invoke [Deft - nvAlt like note system](https://jblevins.org/projects/deft/) `F7`
* Function List (imenu-list) `F9`
* Format the current buffer - `F12`
* Folding keys

`Fold all - C-c Up Arrow`

`UnFold all - C-c Down Arrow`

`Fold current - C-c Left Arrow`

`UnFold current - C-c Righ Arrow`
* Navigate buffers easily `C-x Right/Left arrows`
* Move to windows with [Ace Window](https://github.com/abo-abo/ace-window) `C-x O`
* Magit - `M-x g`
* Exapnd selected region -> `C-=`
* Fast movement with [Avy](https://github.com/abo-abo/avy)

`Go to Char -> C-:`

`Go to Char by 2 char -> C-'`

`Go to Line -> M-g f`

`Go to Word -> M-g w`

* Quickly get summary from wikipedia `C-c W`

User Guide [Not started]
==========
Please find the user guide at [User Guide](/userguide.md)

Cheatsheets
-----------
All the important cheatsheets are located in the [quickref](/quickref) directory
* [Markdown mode User Guide](https://jblevins.org/projects/markdown-mode/)
* [Taskpaper mode User Guide](https://github.com/saf-dmitry/taskpaper-mode)

Feedback
--------
I would be very pleased if you give me feedback to improve this configuration. I will try to improve as I go along using it.
Thank You.

Further Research
-----------------
Furhter research for improvements
* [Replace Neotree with Treemacs](https://github.com/Alexander-Miller/treemacs)
* [emacs dashboard](https://github.com/rakanalh/emacs-dashboard)
* [page-break-lines](https://github.com/purcell/page-break-lines)
* [Fill-Column-Indicator](https://github.com/alpaker/Fill-Column-Indicator)
* [taskpaper-mode](https://github.com/saf-dmitry/taskpaper-mode)
* [buffer tabs - nerdtab](https://github.com/casouri/nerdtab)
