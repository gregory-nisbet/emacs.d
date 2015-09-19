(package-initialize)
(setq evil-want-C-u-scroll t)
(require 'evil)
(define-key evil-normal-state-map (kbd "C-f") #'universal-argument)
(evil-mode +1)

