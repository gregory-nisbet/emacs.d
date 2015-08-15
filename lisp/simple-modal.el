(defun simple-modal-key (str command)
  (cl-assert (stringp str))
  (cl-assert (commandp command))
  (define-key simple-modal-mode-map (kbd str) command))

(defvar simple-modal-mode-map (make-sparse-keymap))

(cl-labels 
  ((key (str command) (simple-modal-key str command)))
  (key "j" 'backward-char)
  (key "k" 'next-line)
  (key "l" 'forward-char)
  (key "i" 'previous-line)
  (key "u" 'backward-word)
  (key "o" 'forward-word)
  (key "m" 'scroll-up-command)
  (key "," 'scroll-down-command)
  (key "s" 'isearch-forward-regexp)
  (key "r" 'isearch-backward-regexp)
  (key "d" 'backward-delete-char-untabify)
  (key "f" 'delete-forward-char)
  (key "y" 'yank)
  (key "w" 'backward-kill-word)
  (key "gw" 'save-buffer)
  (key "gf" 'find-file)
  (key "gg" 'beginning-of-buffer)
  (key "G" 'end-of-buffer)
  )

(define-minor-mode 
  simple-modal-mode
  "ergonomic keybindings for your alphanumeric keys. Vaguely VI inspired"
  :lighter " simple-modal"
  :keymap simple-modal-mode-map)

(provide 'simple-modal)
