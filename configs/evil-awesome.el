;; evil-mode and god-mode
;; use ctrl-H to enter god-mode
;; use C-c n to enter normal-mode.

(defun require-package (package)
  "install package from source"
  (setq-default highlight-tabs t)
  "Install given PACKAGE." 
  (unless (package-installed-p package) 
    (unless (assoc package package-archive-contents) 
      (package-refresh-contents)) 
    (package-install package)))

(defmacro global-window-shortcut (key-string which-window)
  "create global shortcut C-c # for jumping to window #"
  `(global-set-key
    (kbd ,key-string)
    (lambda ()
      (interactive)
      (window-number-select ,which-window))))

(defun add-to-map (key cmd)
  "add a command to a keymap"
  (cl-assert (stringp key))
  (cl-assert (commandp cmd))
  (define-key current-keymap (kbd key) cmd))
(defun add-to-map-and-ctl (key cmd)
  "add a command to a keymap, and the ctl version too"
  (cl-assert (stringp key))
  (cl-assert (commandp cmd))
  (define-key current-keymap (kbd key) cmd)
  (define-key current-keymap (kbd (format "C-%s" key)) cmd))
(defun submap (key cmd)
  "add a submap to a keymap"
  (cl-assert (stringp key)) 
  (define-key current-keymap (kbd key) cmd))

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

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'ace-jump-mode)
(require 'paredit)
(require 'window-number)
(require 'recentf)
(require 'evil)
; tagbody is a dependency of coroutine
; (require 'coroutine)


(require-package 'haskell-mode)
(require-package 'eww)
(require-package 'web-mode)
(require-package 'dockerfile-mode)
(require-package 'markdown-mode)
(require-package 'tuareg)
(require-package 'php-mode)
(require-package 'magit)
(require-package 'projectile)
  
(require 'magit)
;; (require 'circe)
(require 'haskell)
(require 'eww)
(require 'web-mode)
(require 'php-mode)
(require 'dockerfile-mode) 
(require 'markdown-mode)
(require 'tuareg)
(require 'projectile)

;; use this symbol to quickly check if optional modes actually loaded
(setf DEBUG_LOADED_OPTIONAL t)


;; replace some functions with more useful ones
;; some of these are taken from
;; https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
;; (global-set-key (kbd "C-t") 'previous-line)
;;(global-set-key (kbd "C-p") 'other-window)
(global-set-key (kbd "C-l") 'evil-normal-state)
(global-set-key (kbd "C-c l") 'recenter-top-bottom)
(global-set-key (kbd "C-c a") 'ace-jump-mode)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
;; (global-set-key (kbd "C-z") 'god-mode)
(global-set-key (kbd "M-a") 'backward-paragraph)
(global-set-key (kbd "M-e") 'forward-paragraph)

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
;; (define-key dired-mode-map (kbd "t") 'dired-previous-line)
(define-key dired-mode-map (kbd "C-c C-t") 'dired-previous-line)
(define-key dired-mode-map (kbd "C-c C-d") 'dired-toggle-marks)
(define-key dired-mode-map (kbd "C-c C-c") 'image-dired-map)
(define-key dired-mode-map (kbd "b") 'dired-up-directory)

(define-key evil-normal-state-map "," 'leader-map)

;; remove ctrl-r from evil-mode map to make it reverse isearch regexp
;; because that is super useful
;; think about another key to elevate to paste from register.
(define-key evil-normal-state-map (kbd "C-r") nil)
(define-key evil-insert-state-map (kbd "C-r") nil)

;; C-a is an abomination before the lord.
(define-key evil-normal-state-map (kbd "C-a") 'undo-tree-redo)
(define-key evil-insert-state-map (kbd "C-a") 'evil-paste-from-register)

(evil-set-initial-state 'neotree-mode 'emacs) 


;; leader keys
(define-prefix-command 'leader-map)
(let 
  ((current-keymap 'leader-map))
  (add-to-map "o" 'ibuffer)
  (add-to-map "qa" 'save-buffers-kill-terminal)
  (add-to-map "w" 'save-buffer)
  (add-to-map "n" 'neotree)
  (add-to-map "s" 'isearch-repeat-forward)
  (add-to-map "r" 'isearch-repeat-backward)
  (add-to-map "a" 'move-beginning-of-line)
  (add-to-map "e" 'move-end-of-line))

(evil-mode +1)


