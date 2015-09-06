;; the goal here is eventually to make a transient hippie expansion command
;; that enables a user to attempt to complete
;; hit space to see more completions
;; hit u to undo them
;; hit r to redo a previously undone thing.

(define-minor-mode hippie-mode
  "expansion and global undo in hippie mode"
  :lighter " hippie"
  :keymap (let ((map make-sparse-keymap))
            (define-key map (kbd " ") 'hippie-expand)
            (define-key map (kbd "u") 'undo-tree-undo)
            (define-key map (kbd "r") 'undo-tree-redo)
            map))

(provide 'hippie-mode)
