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
;; (set-background-color "honeydew")


(defun require-package (package)
  "install package from source"
  (setq-default highlight-tabs t)
  "Install given PACKAGE." 
  (unless (package-installed-p package) 
    (unless (assoc package package-archive-contents) 
      (package-refresh-contents)) 
    (package-install package)))

(define-key key-translation-map (kbd "C-SPC") (kbd "ESC"))
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-l") 'set-mark-command)
(global-set-key (kbd "C-c l") 'recenter-top-bottom)

(require-package 'helm)
;; (require-package 'ac-helm)
(require-package 'auto-complete)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/Workspace/foreign/ac-helm")
(require 'autocomplete-buffer)

;; enable autocomplete mode everywhere!
;; (auto-complete-mode +1)
;; (global-auto-complete-mode t)
;; I have literally no idea what this does. I think it runs autocompletion
;; asynchronously with helm all the time. perhaps not
(global-set-key (kbd "C-c c") 'autocomplete-buffer-with-helm)


