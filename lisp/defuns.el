(defmacro god-extension-set-mode (docstring state)
  "create function to set god-mode to specific state"
  `(lambda ()
     ,docstring
     (interactive)
     (setq god-global-mode ,state)
     (if god-global-mode
       (god-local-mode +1)
       (god-local-mode -1))))

(defmacro global-window-shortcut (key-string which-window)
  "create global shortcut C-c # for jumping to window #"
  `(global-set-key
     (kbd ,key-string)
     (lambda ()
       (interactive)
       (window-number-select ,which-window))))

(defmacro god-extension-set-mode (docstring state)
  "toggle god mode"
  `(lambda ()
     ,docstring
    (interactive)
    (setq god-global-mode ,state)
    (if god-global-mode
        (god-local-mode +1)
      (god-local-mode -1))))

(defun set-archives ()
  "set the archives"
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/") 
       ("org" . "http://orgmode.org/elpa/") 
       ("marmalade" . "http://marmalade-repo.org/packages/") 
       ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
       ("melpa" . "http://melpa.milkbox.net/packages/"))))

(defun require-package (package)
  "install package from source"
  (setq-default highlight-tabs t)
  "Install given PACKAGE." 
  (unless (package-installed-p package) 
    (unless (assoc package package-archive-contents) 
      (package-refresh-contents)) 
    (package-install package)))

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

(defun load-modes ()
  "require generally used modes"


  (require-package 'ace-jump-mode)
  (require-package 'paredit)
  (require-package 'evil)
  (require-package 'php-mode)

  (require 'god-mode)
  (require 'ace-jump-mode)
  (require 'paredit)
  (require 'window-number)
  (require 'recentf)
  (require 'god-kmacro)
  (require 'keymaps)
  )

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
  (require-package 'ensime)
  (require-package 'jdee)
   
  (require 'magit)
  ;; (require 'circe)
  (require 'haskell)
  (require 'eww)
  (require 'web-mode)
  (require 'php-mode)
  (require 'dockerfile-mode) 
  (require 'markdown-mode)
  (require 'tuareg)
  (require 'ensime)
  (require 'jdee)

  ;; use this symbol to quickly check if optional modes actually loaded
  (setf DEBUG_LOADED_OPTIONAL t))

(defun sensible-defaults ()
  "overwrite some of the default keybindings"
  ;; replace some functions with more useful ones
  ;; some of these are taken from
  ;; https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
  ;; (global-set-key (kbd "C-t") 'previous-line)
  ;; (global-set-key (kbd "C-p") 'other-window)

  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR.")

  (global-set-key (kbd "C-l") 'hippie-expand)
  (global-set-key (kbd "C-c l") 'recenter-top-bottom)
  (global-set-key (kbd "C-c f") 'frameset-to-register)
  (global-set-key (kbd "C-c j") 'jump-to-register)
  (global-set-key (kbd "C-c a") 'ace-jump-mode)
  (global-set-key (kbd "M-z") 'zap-up-to-char)
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)
  (global-set-key (kbd "C-z") 'god-mode)
  (global-set-key (kbd "M-a") 'backward-paragraph)
  (global-set-key (kbd "M-e") 'forward-paragraph))

(defun window-shortcuts ()
  "set global window shortcuts in C-c map"
  (global-window-shortcut "C-c 1" 1)
  (global-window-shortcut "C-c 2" 2)
  (global-window-shortcut "C-c 3" 3)
  (global-window-shortcut "C-c 4" 4)
  (global-window-shortcut "C-c 5" 5)
  (global-window-shortcut "C-c 6" 6)
  (global-window-shortcut "C-c 7" 7)
  (global-window-shortcut "C-c 8" 8)
  (global-window-shortcut "C-c 9" 9)
  (global-window-shortcut "C-c 0" 10))

(defun dired-extensions ()
  (define-key dired-mode-map (kbd "C-c C-t") 'dired-previous-line)
  (define-key dired-mode-map (kbd "C-c C-d") 'dired-toggle-marks)
  (define-key dired-mode-map (kbd "C-c C-c") 'image-dired-map)
  (define-key dired-mode-map (kbd "b") 'dired-up-directory))

(defun load-private ()
  (when (file-exists-p "~/.emacs.d/private.el")
  (require 'private)))

(defun setq-config ()
  "miscellaneous setq configuration"
  (setq visible-bell t)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq disabled-command-function nil)
  (setq recentf-max-menu-items 25)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(defun sensible-aliases ()
  "aliases that are sensible"
  ; http://ergoemacs.org/emacs/emacs_alias.html
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'list-buffers 'ibuffer)
  (defalias 'perl-mode 'cperl-mode))

(defun sensible-modes ()
  "enable sensible modes"
  (recentf-mode +1)
  (show-paren-mode +1))

(defun delayed-load-optional-modes ()
  "load optional modes if we've been idle for a while"
  (setf load-optional (or (daemonp) window-system))
  (if load-optional (load-optional-modes) (run-with-idle-timer 10 nil #'load-optional-modes)))

(defun dired-changes ()
  "small changes to dired mode keybindings"
  (define-key dired-mode-map (kbd "C-c C-t") 'dired-previous-line)
  (define-key dired-mode-map (kbd "C-c C-d") 'dired-toggle-marks)
  (define-key dired-mode-map (kbd "C-c C-c") 'image-dired-map)
  (define-key dired-mode-map (kbd "b") 'dired-up-directory))

(provide 'defuns)
