(require 'cl-lib)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
; do not forget this step.
(package-initialize)
(require 'defuns)
(set-archives)


(package-initialize)
(setq-config)
(sensible-aliases)
(sensible-defaults)
(load-modes)
(load-evil)

;; the emacs keys are not really that great
;;(evil-purge-ctrl-keys)

(load-optional-modes)
(recentf-mode +1)
;;(global-set-key (kbd "C-t") 'previous-line)
;;(global-set-key (kbd "C-p") 'transpose-chars)
;;(global-set-key (kbd "C-h") 'evil-normal-state)
(global-set-key (kbd "C-c h") (lookup-key global-map (kbd "<f1>")))
(global-set-key (kbd "C-c t") 'previous-buffer)
(global-set-key (kbd "C-c n") 'next-buffer)

;; TODO space to repeat hippie expansion and easy undoing of all hippie-ing
(define-key evil-normal-state-map (kbd "M-SPC") 'hippie-expand)
(define-key evil-insert-state-map (kbd "M-SPC") 'hippie-expand)
(define-key evil-insert-state-map (kbd "C-l") 'evil-normal-state)

(global-set-key (kbd "C-.") 'repeat)
;; ^R now goes back a line
;; C-c r for isearch reverse has weird unexpected behavior
;; like not repeating the search when you'd expect it to
;; maybe think of better things than C-v and M-v for
;; scrolling up and down pages.
;; save ^T for ratpoison
(ratpoison-compat)

(expression-navigation)
(god-local-mapify)
(sensible-modes)

(evil-mode)
;; el-screen
(require-package 'elscreen)
(require 'elscreen)
;; (elscreen-set-prefix-key (kbd "C-a"))

(require 'evil-overlord)

(setq evil-regexp-search t)

(define-key evil-normal-state-map (kbd ",") #'evil-overlord-map)
;;(define-key evil-normal-state-local-map (kbd "SPC") #'evil-local-leader-default-map)

(define-key evil-insert-state-map (kbd "C-a") #'hippie-expand)

(define-key evil-normal-state-map (kbd "g r") #'recentf-open-files)
(define-key evil-normal-state-map (kbd "g s") #'save-buffer)
(define-key evil-normal-state-map (kbd "g c") #'evil-search-forward)
(define-key evil-normal-state-map (kbd "g T") #'previous-buffer)
(define-key evil-normal-state-map (kbd "g t") #'next-buffer)
(define-key evil-normal-state-map (kbd "g H") #'evil-window-top)
(define-key evil-normal-state-map (kbd "g L") #'evil-window-bottom)
  
;; leaderkey stuff, available in every mode.
(define-key evil-overlord-map (kbd "o") #'list-buffers)
(define-key evil-overlord-map (kbd "h") #'windmove-left)
(define-key evil-overlord-map (kbd "j") #'windmove-down)
(define-key evil-overlord-map (kbd "k") #'windmove-up)
(define-key evil-overlord-map (kbd "l") #'windmove-right)
(define-key evil-overlord-map (kbd "s") #'split-window-right)
(define-key evil-overlord-map (kbd "v") #'split-window-below)
(define-key evil-overlord-map (kbd "w") #'save-buffer)
(define-key evil-overlord-map (kbd "d") #'anything)

(require-package 'anything)
(require 'anything)
;; todo: better mode-specific bindings right now there are not any.
;; I am thinking for anything, move some of the bindings to
;; local-leader and then make "  " a literal space because let's be real here how often do you actually
;; want a space in your filepath or pattern.
                            
(require-package 'icicles)
(require 'icicles)
(icy-mode +1)
;; icicles rebinds C-h? maybe
;; (define-key icicle-mode-map (kbd "C-h") nil)

