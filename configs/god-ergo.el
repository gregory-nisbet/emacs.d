(require 'cl-lib)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
; do not forget this step.
(package-initialize)
(require 'defuns)
(require 'god-kmacro)
(set-archives)


(package-initialize)
(setq-config)
(sensible-aliases)
(sensible-defaults)
(load-modes)
(load-optional-modes)
(recentf-mode +1)
(global-set-key (kbd "C-t") 'previous-line)
(global-set-key (kbd "C-p") 'transpose-chars)
(global-set-key (kbd "C-h") (god-extension-set-mode "enable god mode" t))
(global-set-key (kbd "C-c h") (lookup-key global-map (kbd "<f1>")))
(define-key god-local-mode (kbd " ") 'repeat)

(global-set-key (kbd "C-.") 'repeat)
(ratpoison-compat)

(expression-navigation)
(god-local-mapify)
(sensible-modes)
