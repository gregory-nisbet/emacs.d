(require 'cl-lib)
(define-prefix-command 'evil-overlord-map)

(defun evil-overlord-map-add-key (str cmd)
  "add a key to the evil overlord map"
  (cl-assert (stringp str))
  (cl-assert (commandp str))
  (define-key evil-overlord-map (kbd str) cmd))

;; populate the evil-overlord map
(cl-flet
  ((e (str cmd) (evil-overlord-map-add-key str cmd)))
  (e "," 'evil-repeat-find-char-reverse))

(defmacro evil-overlord-create-map (name parent args)
  (cl-assert (symbolp name))
  `(progn
     (define-prefix-command ,name)
     (set-keymap-parent ,name ,parent)
     (mapcar (lambda (x)
               (let
                   ((key (nth 0 x))
                    (value (nth 1 x)))
                 (evil-overlord-extend-map (eval ,name) key value))))))  

(defun evil-overlord-extend-map (map key fun)
  (cl-assert (keymapp map))
  (cl-assert (stringp key))
  (define-key map (kbd key) fun))
      
(provide 'evil-overlord)
