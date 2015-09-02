;; make a feature similar to localleader in vim. 
;; this is a dedicated key for mode-specific stuff.
(require 'cl-lib)

(defvar evil-local-leader-default-map 
  (make-sparse-keymap)
  "default map for evil local")

(defvar evil-local-leader-map
  evil-local-leader-default-map
  "map for local leader currently in effect")

(defun evil-local-leader-map-add-key (str cmd)
  "add a key to the evil leader map"
  (cl-assert (stringp str))
  (define-key (kbd str) cmd))

(defun evil-local-leader-impose-keymap (kmp)
  "impose a certain keymap."
  (cl-assert (keymapp kmp))
  (setq evil-local-leader-map kmp))

(provide 'evil-local-leader)
