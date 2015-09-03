(require 'cl-lib)
(define-prefix-command 'evil-leader-map)

(defun evil-leader-map-add-key (str cmd)
  "add a key to the evil leader map"
  (cl-assert (stringp str))
  (cl-assert (commandp str))
  (define-key evil-leader-map (kbd str) cmd))

;; populate the evil-leader map
(cl-flet
  ((e (str cmd) (evil-leader-map-add-key str cmd)))
  (e "," 'evil-repeat-find-char-reverse))

(provide 'evil-leader)
