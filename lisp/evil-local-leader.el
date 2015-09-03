;; make a feature similar to localleader in vim. 
;; this is a dedicated key for mode-specific stuff.
(require 'cl-lib)

(defvar evil-local-leader-normal-prefix (kbd "SPC")
  "default local-leader key")

(define-prefix-command 'evil-local-leader-default-map)

(defun evil-local-leader-map-add-key (str cmd)
  "add a key to the evil leader map"
  (cl-assert (stringp str))
  (define-key (kbd str) cmd))

(defun evil-local-leader-impose-keymap (kmp)
  "impose a certain prefix command."
  (define-key 'evil-normal-state-local-map evil-local-leader-normal-prefix kmp))

(provide 'evil-local-leader)
