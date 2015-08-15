;; modal emacs config
;; uses god-mode pervasively
;; remapping ctl-p to ctl-t is just not practical


(defmacro global-window-shortcut (key-string which-window)
  "create global shortcut C-c # for jumping to window #"
  `(global-set-key
    (kbd ,key-string)
    (lambda ()
      (interactive)
      (window-number-select ,which-window))))

(require 'cl-lib)
(add-to-list 'load-path "~/.emacs.d/lisp")
(when (file-exists-p "~/.emacs.d/private.el")
  (require 'private))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/") 
			 ("org" . "http://orgmode.org/elpa/") 
			 ("marmalade" . "http://marmalade-repo.org/packages/") 
			 ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

(package-initialize)
(setq visible-bell t)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq disabled-command-function nil)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

; http://ergoemacs.org/emacs/emacs_alias.html
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)
(defalias 'perl-mode 'cperl-mode)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(recentf-mode +1)
(setq recentf-max-menu-items 25)

(defun require-package (package)
  "install package from source"
  (setq-default highlight-tabs t)
  "Install given PACKAGE." 
  (unless (package-installed-p package) 
    (unless (assoc package package-archive-contents) 
      (package-refresh-contents)) 
    (package-install package)))

(require-package 'ace-jump-mode)
(require-package 'paredit)
;;(require-package 'key-chord)

;; (require-package 'circe)                
  
;; (add-to-list 'load-path "~/.emacs.d/god-mode/")
;; (add-to-list 'load-path "~/.emacs.d/god-kmacro/")
;; (add-to-list 'load-path "~/.emacs.d/window-number/")
;; (add-to-list 'load-path "~/.emacs.d/keymaps/")
;; (add-to-list 'load-path "~/.emacs.d/tagbody/")
;; (add-to-list 'load-path "~/.emacs.d/coroutine/")

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'simple-modal)
(require 'ace-jump-mode)
(require 'paredit)
(require 'window-number)
(require 'recentf)
; tagbody is a dependency of coroutine
; (require 'coroutine)

(defalias 'lom 'load-optional-modes)
(defun load-optional-modes ()
    "load non mandatory modes, keep start time down"
  (interactive)

  (require-package 'haskell-mode)
  (require-package 'eww)
  (require-package 'web-mode)
  (require-package 'dockerfile-mode)
  (require-package 'markdown-mode)
  (require-package 'tuareg)
  (require-package 'php-mode)
  (require-package 'magit)
    
  (require 'magit)
  ;; (require 'circe)
  (require 'haskell)
  (require 'eww)
  (require 'web-mode)
  (require 'php-mode)
  (require 'dockerfile-mode) 
  (require 'markdown-mode)
  (require 'tuareg)

  ;; use this symbol to quickly check if optional modes actually loaded
  (setf DEBUG_LOADED_OPTIONAL t))

;; this option is used to load optional stuff. included in the systemd service definition
(setf load-optional (or (daemonp) window-system))
(if t (load-optional-modes) (run-with-idle-timer 10 nil #'load-optional-modes))

;; replace some functions with more useful ones
;; some of these are taken from
;; https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
;; (global-set-key (kbd "C-t") 'previous-line)
;;(global-set-key (kbd "C-p") 'other-window)
(global-set-key (kbd "C-l") 'hippie-expand)
(global-set-key (kbd "C-c l") 'recenter-top-bottom)
(global-set-key (kbd "C-c a") 'ace-jump-mode)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-a") 'backward-paragraph)
(global-set-key (kbd "M-e") 'forward-paragraph)
(global-set-key (kbd "C-h") 'simple-modal-mode)
;; (key-chord-define-global "df" 'god-mode)
;; (global-set-key (kbd "C-;") 'other-window)
;; (key-chord-define-global "df" (god-extension-set-mode "enable god mode" t))
;; (key-chord-define-global "jk" 'other-window)
;; (key-chord-define-global "jj" (god-extension-set-mode "disable god mode" nil))
;; (global-set-key (kbd "C-;") 'other-window)
(global-window-shortcut "C-c 1" 1)
(global-window-shortcut "C-c 2" 2)
(global-window-shortcut "C-c 3" 3)
(global-window-shortcut "C-c 4" 4)
(global-window-shortcut "C-c 5" 5)
(global-window-shortcut "C-c 6" 6)
(global-window-shortcut "C-c 7" 7)
(global-window-shortcut "C-c 8" 8)
(global-window-shortcut "C-c 9" 9)
(global-window-shortcut "C-c 0" 10)
;; (global-set-key (kbd "C-c t") 'transpose-chars)
(global-set-key (kbd "C-c f") 'frameset-to-register)
(global-set-key (kbd "C-c j") 'jump-to-register)
(global-set-key (kbd "C-c h") (lookup-key global-map (kbd "<f1>")))
;; (define-key dired-mode-map (kbd "t") 'dired-previous-line)
(define-key dired-mode-map (kbd "C-c C-t") 'dired-previous-line)
(define-key dired-mode-map (kbd "C-c C-d") 'dired-toggle-marks)
(define-key dired-mode-map (kbd "C-c C-c") 'image-dired-map)
(define-key dired-mode-map (kbd "b") 'dired-up-directory)
  
;;(key-chord-mode +1)
(show-paren-mode +1)
