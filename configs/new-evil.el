(require 'cl-lib)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
; do not forget this step.
(package-initialize)
(require 'defuns)
(set-archives)
(package-initialize)

(setq evil-want-C-u-scroll t)

(defun get-package (item)
  "fetch package over network and get"
  (require-package item)
  (require item))

(mapcar #'get-package 
        '(evil evil-leader evil-org evil-nerd-commenter))

(global-set-key (kbd "C-l") #'evil-normal-state)
(global-set-key (kbd "C-c l") #'recenter-top-bottom)

(define-key evil-insert-state-map (kbd "M-SPC") #'hippie-expand)
(define-key evil-insert-state-map (kbd "M-/") #'hippie-expand)
(define-key evil-normal-state-map (kbd "C-f") #'universal-argument)
(define-key evil-normal-state-map (kbd "C-c") #'evil-normal-state)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
;; default hotkeys for evil-nerd-commenter
(evilnc-default-hotkeys)
(evil-mode +1)
