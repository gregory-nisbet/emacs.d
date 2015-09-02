(require 'cl-lib)
(define-prefix-command 'evil-leader-map)

(defun evil-leader-map-add-key (str cmd)
  "add a key to the evil leader map"
  (cl-assert (stringp str))
  (define-key (kbd str) cmd))

;; populate the evil-leader map
(cl-flet
  ((e str cmd (evil-leader-map-add-key str cmd)))
  )

(provide 'evil-leader)
