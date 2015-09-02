# emacs.d
This is for the .emacs.d directory.

Custom configuration is generally organized in the `lisp/defuns.el` page. Each nullary function in there should be thought of as a single 'configuration option' that is relatively independent of the other ones.

I have gone down from the insane number of old config files to just three, a simple-modal mode, an evil-mode, and a god-mode. The evil-mode config is currently receiving the most attention since vim is already nearly ideal for most editing tasks. Functionality in that config will be organized around the evil-leader "," which is universal, and the local-leader " " which is different depending on the mode.

long term goals going forward are: patching the kmacro.el built-in library so that keychords or localleader stuff can be replaced with literal commands. This would avoid the stupid hacks in keychord and friends needed to decide when to activate a key chord. I do not currently use any key chords largely for this reason.

figure out how to create emacs workers so that `gnus` or other similarly blocking things don't lock up the entire UI. Alternatively, figure out how to run mutt under emacs so the scheduler can worry about concurrency instead of me.
