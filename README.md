# emacsadventures
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

Feedback
--------
I would be very pleased if you give me feedback to improve this configuration. I will try to improve as I go along using it.
Thank You.
