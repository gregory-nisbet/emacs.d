;; minimal modal emacs config. cl-lib not actually needed. ^P and ^T swapped ^P is so much more common

(require 'cl-lib)

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

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(defmacro global-window-shortcut (key-string which-window)
  "goto window"
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

(require-package 'php-mode)
(require-package 'ace-jump-mode)
(require-package 'magit)
(require-package 'circe)
(require-package 'haskell-mode)
(require-package 'key-chord)
(require-package 'eww)
(require-package 'web-mode)
(require-package 'paredit)
(require-package 'dockerfile-mode)

(add-to-list 'load-path "~/.emacs.d/god-mode/")
(add-to-list 'load-path "~/.emacs.d/god-kmacro/")
(add-to-list 'load-path "~/.emacs.d/window-number/")

(require 'god-mode)
(require 'ace-jump-mode)
(require 'php-mode)
(require 'recentf)
(require 'magit)
(require 'circe)
(require 'haskell)
(require 'eww)
(require 'web-mode)
(require 'paredit)
(require 'window-number)
(require 'dockerfile-mode) 


;; replace some functions with more useful ones
;; some of these are taken from
;; https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
(global-set-key (kbd "C-t") 'previous-line)
(global-set-key (kbd "C-p") 'transpose-chars) 
(global-set-key (kbd "C-l") 'hippie-expand)
(global-set-key (kbd "C-c l") 'recenter-top-bottom)
(global-set-key (kbd "C-c a") 'ace-jump-mode)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-z") 'god-mode)
(global-set-key (kbd "C-c i") 'apropos)
(key-chord-define-global "df" 'god-mode)
(global-set-key (kbd "C-;") 'other-window)
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




(key-chord-mode +1)
(show-paren-mode +1)

;; make god mode distinguishable without reading the
;; modeline
(add-hook 'god-local-mode-hook
          (lambda ()
            (if god-local-mode
                (setq cursor-type 'hollow)
              (setq cursor-type 'box))))

;; create buffer local variable for storing whether we exited
;; god-mode for kmacro (this is independent of other pause and
;; resume stuff.
(defvar god-local-mode-paused-kmacro)
(make-variable-buffer-local 'god-local-mode-paused-kmacro)
(setf god-local-mode-paused-kmacro nil)
;; advise key macros to start you in non-godmode regardless
;; remember state before macro was executed
(defvar god-local-mode-paused-kmacro-call)
(make-variable-buffer-local 'god-local-mode-paused-kmacro-call)
(setf god-local-mode-paused-kmacro-call nil)

(defadvice kmacro-start-macro (before kmacro-start-no-godmode activate)
  "disable godmode after kmacro begin"
  ;; recursive kmacro god-mode status tracking is not implemented yet
  ;; if the god-mode is not nil, then we're already in a macro
  ;; or didn't cleanly reset the buffer local variable.
  (cl-assert (not god-local-mode-paused-kmacro))
  (setf god-local-mode-paused-kmacro (if god-local-mode +1 0))
  (god-local-mode 0))

(defadvice kmacro-end-macro (around kmacro-end-restore-godmode activate)
  (unwind-protect
      (condition-case ex
          ad-do-it
        ('error (message (format "Caught exception [%s]" ex))))
    (progn
      (cl-assert god-local-mode-paused-kmacro)
      (god-local-mode god-local-mode-paused-kmacro)
      (setf god-local-mode-paused-kmacro nil))))
        

;; disable god mode and store previous god-mode state
(defadvice kmacro-call-macro (before kmacro-call-no-godmode activate)
  "disable god-mode before executing macro"
  (cl-assert (not god-local-mode-paused-kmacro-call))
  (setf god-local-mode-paused-kmacro-call (if god-local-mode 1 0))
  (god-local-mode 0))

;; reenable god-mode after executing keyboard macro
;; wrap errors safely
(defadvice kmacro-call-macro (around kmacro-call-restore-godmode activate)
  "re-enable god-mode after executing macro"
  (unwind-protect
      (condition-case ex
          ad-do-it
        ('error (message (format "Caught exception [%s]" ex))))
    (progn
      (cl-assert god-local-mode-paused-kmacro-call)
      (god-local-mode god-local-mode-paused-kmacro-call)
      (setf god-local-mode-paused-kmacro-call nil))))


;; https://curiousprogrammer.wordpress.com/2009/06/08/error-handling-in-emacs-lisp/
(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex))))))
     ,@clean-up))
