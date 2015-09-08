;; cumbersome mode has six modifier keys
;; d f j k space comma
;; eventually all of them will be bound

(defun cumbersome-mode-key (str command)
  "assign key in simple modal map"
  (cl-assert (stringp str))
  (cl-assert (commandp command))
  (define-key simple-modal-mode-map (kbd str) command))

(defun cumbersome-mode-key--enable ()
  "enable simple modal and force modeline update"
  (interactive)
  (setf simple-modal-mode t)
  (force-mode-line-update))

(defun cumbersome-mode--disable ()
  "disable simple modal and force modeline update"
  (interactive)
  (setf simple-modal-mode nil)
  (force-mode-line-update))

(defvar cumbersome-mode-map (make-keymap)
  "main map")
(defvar cumbersome-mode-f-map (make-keymap)
  "submap from f key")
(defvar cumbersome-mode-d-map (make-keymap)
  "submap from d key")
(defvar cumbersome-mode-j-map (make-keymap)
  "submap from j key")
(defvar cumbersome-mode-k-map (make-keymap)
  "submap from k key")
(defvar cumbersome-mode-space-map (make-keymap)
  "submap from space key")
(defvar cumbersome-mode-comma-map (make-keymap)
  "submap from comma key")

(define-prefix-command 'cumbersome-mode-base cumbersome-mode-map)
(define-prefix-command 'cumbersome-mode-f cumbersome-mode-f-map)
(define-prefix-command 'cumbersome-mode-d cumbersome-mode-d-map)
(define-prefix-command 'cumbersome-mode-j cumbersome-mode-j-map)
(define-prefix-command 'cumbersome-mode-k cumbersome-mode-k-map)
(define-prefix-command 'cumbersome-mode-space cumbersome-mode-space-map)
(define-prefix-command 'cumbersome-mode-comma cumbersome-mode-comma-map)

(defalias $yank icicle-yank-maybe-completing)
(defalias $undo undo-tree-undo)
(defalias $redo undo-tree-redo)

(defvar cumbersome-mode-bindable-keys
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defun populate-keymap-from-list (kmp list)
  ;; list has even length
  (cl-assert (keymapp kmp))
  (cl-assert (= (mod (length list) 2) 0))
  (let ((counter 0))
    (while )
  )

(setf cumbersome-mode-base-raw
      '("f" cumbersome-mode-f
        "d" cumbersome-mode-d
        "SPC" cumbersome-mode-space
        "j" cumbersome-mode-j
        "k" cumbersome-mode-k
        "m" next-line
        "v" previous-line
        "g" backward-char
        "h" forward-char
        "n" scroll-up-command
        "b" scroll-down-command
        "c" repeat
        "t" kill-line
        "w" kill-region
        "o" other-window
        "x" delete-char
        "e" delete-forward-char
        "s" isearch-repeat-forward
        "r" isearch-repeat-backward
        "u" universal-argument
        "i" cumbersome-mode--disable
        "y" $yank
        "U" $undo
        "R" $redo
        "0" digit-argument
        "1" digit-argument
        "2" digit-argument
        "3" digit-argument
        "4" digit-argument
        "5" digit-argument
        "6" digit-argument
        "7" digit-argument
        "8" digit-argument
        "9" digit-argument
        "0" digit-argument
        "a" set-mark
        ))

(define-minor-mode 
  simple-modal-mode
  cumbersome-mode
  "ergonomic keybindings for your alphanumeric keys. Vaguely VI inspired"
  :lighter " simple-modal"
  :keymap cumbersome-mode-map-base)

(provide 'cumbersome-mode)
