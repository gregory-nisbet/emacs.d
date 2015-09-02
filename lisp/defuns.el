(require 'cl-lib)

(defun log-exception (exception func-name)
  "log errors to message buffer"
  (message "logging: EXCEPTION [%s] in %s" exception func-name))

;; not sure if unwind-protect is necessary here.
;; NOTE docstring is mandatory.
(defmacro defun* (func args docstring &rest body)
  "create function that robustly handles failure. docstring required."
  (cl-assert (stringp docstring))
  `(defun ,func ,args
     ,docstring
     (progn
       (unwind-protect
           (condition-case ex
               (progn ,@body)
             ('error
              (log-exception ex ,(symbol-name func))))))))


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

(defun* set-archives ()
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

(defun* load-modes ()
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
  (require 'org)
  )

(defun* load-evil ()
  "require evil mode"
  (require-package 'evil)
  (require 'evil))

(defun* load-optional-modes ()
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
  ;; not god-mode
  (require-package 'go-mode)
  (require-package 'wanderlust)
  (require-package 'w3m)
  
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
  ;; not god-mode
  (require 'go-mode)
  (require 'w3m)

  ;; recommended from emacswiki
  ;; http://emacswiki.org/emacs/WanderLust
  ;; do we need a require here?
  (autoload 'wl "wl" "Wanderlust" t)

  ;; use this symbol to quickly check if optional modes actually loaded
  (setf DEBUG_LOADED_OPTIONAL t))

(defun* sensible-defaults ()
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

(defun* window-shortcuts ()
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

(defun* dired-extensions ()
  "dired local map extensions"
  (define-key dired-mode-map (kbd "C-c C-t") 'dired-previous-line)
  (define-key dired-mode-map (kbd "C-c C-d") 'dired-toggle-marks)
  (define-key dired-mode-map (kbd "C-c C-c") 'image-dired-map)
  (define-key dired-mode-map (kbd "b") 'dired-up-directory))

(defun* load-private ()
  "load definitions in private elisp file"
  (when (file-exists-p "~/.emacs.d/private.el")
    (require 'private)))

(defun* setq-config ()
  "miscellaneous setq configuration"
  (setq visible-bell t)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq disabled-command-function nil)
  (setq recentf-max-menu-items 25)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(defun* sensible-aliases ()
  "aliases that are sensible"
                                        ; http://ergoemacs.org/emacs/emacs_alias.html
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'list-buffers 'ibuffer)
  (defalias 'perl-mode 'cperl-mode))

(defun* sensible-modes ()
  "enable sensible modes"
  (recentf-mode +1)
  (show-paren-mode +1))

(defun* delayed-load-optional-modes ()
  "load optional modes if we've been idle for a while"
  (setf load-optional (or (daemonp) window-system))
  (if load-optional (load-optional-modes) (run-with-idle-timer 10 nil #'load-optional-modes)))

(defun* dired-changes ()
  "small changes to dired mode keybindings"
  (define-key dired-mode-map (kbd "C-c C-t") 'dired-previous-line)
  (define-key dired-mode-map (kbd "C-c C-d") 'dired-toggle-marks)
  (define-key dired-mode-map (kbd "C-c C-c") 'image-dired-map)
  (define-key dired-mode-map (kbd "b") 'dired-up-directory))

(defun* expression-navigation ()
  "commands for navigating balanced expressions"
  ;; expression navigation
  (global-set-key (kbd "C-c c n") 'forward-list)
  (global-set-key (kbd "C-c c t") 'backward-list)
  (global-set-key (kbd "C-c c f") 'forward-sexp)
  (global-set-key (kbd "C-c c b") 'backward-sexp)
  (global-set-key (kbd "C-c c k") 'kill-sexp)

  ;; not enabled by default because in ratpoison compat
  ;;(global-set-key (kbd "C-c c r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-c c space") 'mark-sexp))

(defun* god-local-mapify ()
  "define stuff like capital letters in god-local mode"
  (define-key god-local-mode-map (kbd "i") (god-extension-set-mode "disable god mode" nil))
  (define-key god-local-mode-map (kbd "S") 'save-buffer)
  (define-key god-local-mode-map (kbd "F") 'find-file)
  (define-key god-local-mode-map (kbd "V") 'scroll-down-command)
  (define-key god-local-mode-map (kbd "R") 'recentf-open-files))

(defun* ratpoison-compat ()
  "remap C-r to previous line so as not to conflict with ratpoison"
  (global-set-key (kbd "C-r") 'previous-line)
  (global-set-key (kbd "C-c r") 'isearch-backward-regexp)
  ;; enable repetition of C-c r to repeat search
  (define-key isearch-mode-map (kbd "C-c r") 'isearch-repeat-backward))


;; the only one of these I actually like is paste-from-register
;; but I can't use C-r for that because that is back-line
;; These are the commands mapped in insert mode in evil-mode
;; most of them are pretty low frequency commands so they will be remapped
;; to C-c g <whatever>                    
;; C-d             evil-shift-left-line
;; C-e             evil-copy-from-below
;; C-k             evil-insert-digraph
;; C-n             evil-complete-next
;; C-o             evil-execute-in-normal-state
;; C-p             evil-complete-previous
;; C-r             evil-paste-from-register
;; C-t             evil-shift-right-line
;; C-v             quoted-insert
;; C-w             evil-delete-backward-word
;; C-x             Prefix Command
;; C-y             evil-copy-from-above
;; C-z             evil-emacs-state
;; <delete>        delete-char
;; <escape>        evil-normal-state
;; <remap>         Prefix Command
;; <remap> <delete-backward-char>  evil-delete-backward-char-and-join
;; C-x C-n         evil-complete-next-line
;; C-x C-p         evil-complete-previous-line
(defun* evil-purge-ctrl-keys ()
  "eliminate ctrl keys from insert state (making it essentially emacs state, I guess"
  (define-key evil-insert-state-map (kbd "C-d") nil)
  (define-key evil-insert-state-map (kbd "C-e") nil)
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-o") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-r") nil)
  (define-key evil-insert-state-map (kbd "C-t") nil)
  (define-key evil-insert-state-map (kbd "C-v") nil)
  (define-key evil-insert-state-map (kbd "C-w") nil)
  (define-key evil-insert-state-map (kbd "C-x") nil)
  (define-key evil-insert-state-map (kbd "C-y") nil)
  (define-key evil-insert-state-map (kbd "C-z") nil)

  (global-set-key (kbd "C-c g d") 'evil-shift-left-line)
  (global-set-key (kbd "C-c g e") 'evil-copy-from-below)
  (global-set-key (kbd "C-c g k") 'evil-insert-digraph)
  (global-set-key (kbd "C-c g n") 'evil-complete-next)
  (global-set-key (kbd "C-c g o") 'evil-execute-in-normal-state)
  (global-set-key (kbd "C-c g p") 'evil-complete-previous)
  (global-set-key (kbd "C-c g r") 'evil-paste-from-register)
  (global-set-key (kbd "C-c g t") 'evil-shift-right-line)
  (global-set-key (kbd "C-c g v") 'quoted-insert)
  (global-set-key (kbd "C-c g w") 'evil-delete-backward-word)
  (global-set-key (kbd "C-c g x n") 'evil-complete-next-line)
  (global-set-key (kbd "C-c g x p") 'evil-complete-previous-line)
  (global-set-key (kbd "C-c g y") 'evil-copy-from-above)
  (global-set-key (kbd "C-c g z") 'evil-emacs-state)
  
  
  
  )

(defun* wanderlust-sensible-defaults ()
  "wanderlust is hard to set up, so we are using these as defaults"

  ;; taken from https://github.com/wanderlust/wanderlust/issues/98
  
  (setq wl-default-folder "%INBOX")
  
  )

(defun* wanderlust-setup ()
  "setting up wanderlust to actually work"
  ;; w3m might be somewhere else
  ;; todo make more bulletproof
  (setq exec-path (cons "/usr/local/bin/" exec-path)))


(provide 'defuns)
