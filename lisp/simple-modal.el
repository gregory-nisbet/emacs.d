(define-prefix-command 'simple-modal)

(defun simple-modal-key (str command)
  (cl-assert (stringp str))
  (cl-assert (commandp command))
  (define-key 'simple-modal (kbd str) command))

(cl-labels 
  ((key (str command) (simple-modal-key str command)))
  (key "j" 'backward-char)
  (key "k" 'next-line)
  (key "l" 'forward-char)
  (key "i" 'previous-line)
  (key "u" 'backward-word)
  (key "o" 'forward-word)
  (key "," 'scroll-up-command)
  (key "." 'scroll-down-command)
  (key "s" 'isearch-forward-regexp)
  (key "r" 'isearch-backward-regexp))
