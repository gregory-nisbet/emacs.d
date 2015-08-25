(defun simple-modal-key (str command)
  "assign key in simple modal map"
  (cl-assert (stringp str))
  (cl-assert (commandp command))
  (define-key simple-modal-mode-map (kbd str) command))

(defun simple-modal--enable ()
  "enable simple modal and force modeline update"
  (interactive)
  (setf simple-modal-mode t)
  (force-mode-line-update))

(defun simple-modal--disable ()
  "disable simple modal and force modeline update"
  (interactive)
  (setf simple-modal-mode nil)
  (force-mode-line-update))

(defvar simple-modal-mode-map (make-sparse-keymap))

(cl-labels 
    ((key (str command) (simple-modal-key str command)))
  (key "i" 'simple-modal--disable)
  (key "h" 'backward-char)
  (key "j" 'next-line)
  (key "l" 'forward-char)
  (key "k" 'previous-line)
  (key "u" 'backward-word)
  (key "o" 'forward-word)
  (key "m" 'scroll-up-command)
  (key "," 'scroll-down-command)
  ;; repeating isearch repeats or requests is absent
  (key "s" 'isearch-repeat-forward)
  (key "r" 'isearch-repeat-backward)
  (key "d" 'backward-delete-char-untabify)
  (key "f" 'delete-forward-char)
  (key "y" 'yank)
  (key "w" 'backward-kill-word)
  (key "gw" 'save-buffer)
  (key "gf" 'find-file)
  (key "gb" (kbd "C-x C-b"))
  (key "gg" 'beginning-of-buffer)
  (key "G" 'end-of-buffer)
  (key "e" 'forward-paragraph)
  (key "a" 'backward-paragraph)
  (key "0" 'digit-argument)
  (key "1" 'digit-argument)
  (key "2" 'digit-argument)
  (key "4" 'digit-argument)
  (key "5" 'digit-argument)
  (key "6" 'digit-argument)
  (key "7" 'digit-argument)
  (key "8" 'digit-argument)
  (key "9" 'digit-argument)
  (key "t" 'pop-to-mark-command)
  (key "T" 'pop-global-mark)
  (key "." 'repeat)
  (key "v" 'kill-line)
  )

(define-minor-mode 
  simple-modal-mode
  "ergonomic keybindings for your alphanumeric keys. Vaguely VI inspired"
  :lighter " simple-modal"
  :keymap simple-modal-mode-map)

(provide 'simple-modal)
