;; -*- lexical-binding: t -*-
;; more expansive evil mode designed to mimic the current vim setup.

;; evil mode with no mandatory key chords. ^K is normal mode too.
;; long-term most of this file will be devoted to the leader keymap.
;; it'll be similar to spacemacs, but hopefully easier to keep track of and w/o the branding.

(require 'cl-lib)

;; various package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/") 
			 ("org" . "http://orgmode.org/elpa/") 
			 ("marmalade" . "http://marmalade-repo.org/packages/") 
			 ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

(package-initialize)
;; get rid of annoying audible bell
(setq visible-bell t)
(setq magit-last-seen-setup-instructions "1.4.0")
;; default indentation tab 4 spaces.
(setq tab-width 4)
;;  indent with spaces not tabs by default
(setq-default indent-tabs-mode nil)
;; better color scheme for terminal
(set-background-color "honeydew")


(defun require-package (package)
  "install package from source"
  (setq-default highlight-tabs t)
  "Install given PACKAGE." 
  (unless (package-installed-p package) 
    (unless (assoc package package-archive-contents) 
      (package-refresh-contents)) 
    (package-install package)))

(require-package 'evil)
(require-package 'php-mode)
(require-package 'ace-jump-mode)
(require-package 'magit)
(require-package 'circe)
(require-package 'haskell-mode)

(add-to-list 'load-path "~/.emacs.d/lisp/") 

(require 'god-mode)
(require 'evil)
(require 'key-chord)
(require 'window-number)
(require 'php-mode)
(require 'recentf)
(require 'ace-jump-mode)
(require 'magit)
(require 'circe)
(require 'haskell)
(require 'keymaps)
;; (require 'evil-config)

;;indent haskell more sanely
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(recentf-mode +1)
(setq recentf-max-menu-items 25)

(evil-mode +1)

;; evil mode add keys and stuff
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

(define-key evil-insert-state-map (kbd " ") isearch-forward-regexp)
(define-key evil-insert-state-map (kbd "C-SPC") isearch-backward-regexp)
