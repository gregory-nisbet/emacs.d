(require 'cl-lib)
(require 'cl)
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'god-mode)
(global-set-key (kbd "C-z") 'god-local-mode)
