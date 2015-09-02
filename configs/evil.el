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
(global-set-key (kbd "C-h") 'evil-normal-state)
(global-set-key (kbd "C-c h") (lookup-key global-map (kbd "<f1>")))
(global-set-key (kbd "C-c t") 'previous-buffer)
(global-set-key (kbd "C-c n") 'next-buffer)

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
