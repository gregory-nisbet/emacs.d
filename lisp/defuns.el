
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

(defun sensible-defaults ()
  "overwrite some of the default keybindings"
  ;; replace some functions with more useful ones
  ;; some of these are taken from
  ;; https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
  ;; (global-set-key (kbd "C-t") 'previous-line)
  ;; (global-set-key (kbd "C-p") 'other-window)
  ;; (global-set-key (kbd "C-l") 'hippie-expand)
  ;; (global-set-key (kbd "C-c l") 'recenter-top-bottom)
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
  "set global windows"
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

(provide 'defuns)
