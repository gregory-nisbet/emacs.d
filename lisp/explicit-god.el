(evil-define-state explicit-god
  "Explicit God"
  :tag "<&>"
  :message "explicit god"
  :entry-hook evil-explicit-god-entry-hook
  :exit-hook evil-explicit-god-exit-hook
  :suppress-keymap t)

(setq explicit-god-emacs-key-list
      '(("a" move-beginning-of-line)
	("b" backward-char)
	("d" delete-char)
	("e" move-end-of-line)
	("f" forward-char)
	("g" ESC-prefix)
	("k" kill-line)
	("l" recenter-top-bottom)
	("n" next-line)
	("o" open-line)
	("p" previous-line)
	("q" quoted-insert)
	("r" isearch-backward)
	("s" isearch-forward)
	("t" transpose-chars)
	("u" universal-argument)
	("v" scroll-up-command)
	("w" kill-region)
	("y" yank)
	("x" ctl-x-prefix)))

(setq explicit-god-emacs-meta-key-alist
      '(("a" backward-sentence)
	("b" backward-word)
	("c" capitalize-word)
	("d" kill-word)
	("e" forward-sentence)
	("f" forward-word)
	("h" mark-paragraph)
	("i" tab-to-tab-stop)
	("j" indent-new-comment-line)
	("k" kill-sentence)
	("l" downcase-word)
	("m" back-to-indentation)
	("q" fill-paragraph)
	("r" move-to-window-line-top-bottom)
	("t" transpose-words)
	("u" upcase-word)
	("v" scroll-up-command)
	("w" kill-ring-save)
	("x" execute-extended-command)
	("y" yank-pop)
	("z" zap-to-char)))




(mapcar (lambda (x)
	  (let ((key (elt x 0)) (command (elt x 1)))
	    (cl-assert (stringp key))
	    (cl-assert (symbolp command))
	    (define-key evil-explicit-god-state-map (kbd key) command))) explicit-god-emacs-key-list)

(provide 'explicit-god)
