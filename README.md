# emacs.d
This is for the .emacs.d directory.

It uses god-mode pervasively.
It's an open question what to do with all the shift-whatever "Chords" that are being wasted in god-mode at the moment.

Using kmacros in god-mode is actually really annoying since the macro will do different things depending on what mode you are in, so I adviced some of the kmacro-functions so it'll always drop you into non-godmode when the macro starts and restore your previous perhaps-godmode when the macro exits or when the macro definition exits. It seems to work rather well so far.

Certain modes like syntax highlighting-related things are delayed when emacs is run in the terminal to keep the load time down. (Currently at 0.2 seconds, 1.7~2.7 seconds to load everything). This isn't ideal behavior. When emacs isn't running in the terminal or as a daemon, it should either load supplemental modules in the background, load them as needed when a file is opened, or do some crazy asynch stuff.
