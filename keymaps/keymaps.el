(defun add-to-map (key cmd)
  "add a command to a keymap"
  (cl-assert (stringp key))
  (cl-assert (commandp cmd))
  (define-key current-keymap (kbd key) cmd))
(defun submap (key cmd)
  "add a submap to a keymap"
  (cl-assert (stringp key)) 
  (define-key current-keymap (kbd key) cmd))
(defun window-number-goto (number)
  "generate command for going to a particular number"
  (eval `(lambda ()
    (interactive)
    (window-number-select ,number))))

(define-prefix-command 'leader-map)
(let
  ((current-keymap 'leader-map))
    (submap "f" 'file-map)
    (submap "w" 'window-map)
    (add-to-map "l" 'recenter-top-bottom)
    (add-to-map "j" 'other-window)
    (add-to-map "h" 'ace-jump-mode)
    (add-to-map "o" 'occur)
    (add-to-map "i" 'apropos)
    (add-to-map "b" 'switch-to-buffer)
    (add-to-map "c" 'comment-dwim)
;;    (add-to-map "u" 'evil-scroll-up)
    (add-to-map "d" 'execute-extended-command)
    (add-to-map "a" 'ace-jump-mode)
    (add-to-map " " 'set-mark-command)
	;; keys 0-9 for keys
    (mapcar
     (lambda (i-num)
       (let ((ii (number-to-string i-num)))
         (add-to-map ii (window-number-goto i-num))))
     (list 0 1 2 3 4 5 6 7 8 9)))

(define-prefix-command 'file-map)
(let
    ((current-keymap 'file-map))
    (add-to-map "f" 'find-file)
    (add-to-map "w" 'write-file)
    (add-to-map "r" 'recentf-open-files)
    (add-to-map "o" 'find-file-other-window)
    ;; save file and immediately enter insert mode
    (add-to-map "i" (lambda ()
                      (interactive)
                      (save-buffer)
                      (evil-append 0))))

(define-prefix-command 'window-map)
(let
    ((current-keymap 'window-map))
  (add-to-map "u" 'scroll-other-window-down)
  (add-to-map "d" 'scroll-other-window)
  (add-to-map "h" 'windmove-left)
  (add-to-map "j" 'windmove-down)
  (add-to-map "k" 'windmove-up)
  (add-to-map "l" 'windmove-right)
  (add-to-map "s" 'split-window-right)
  (add-to-map "v" 'split-window-below)
  (add-to-map "w" 'window-configuration-to-register)
  (add-to-map "j" 'jump-to-register))

(provide 'keymaps)
