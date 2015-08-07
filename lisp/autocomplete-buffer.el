;; http://stackoverflow.com/questions/21021313/how-to-show-all-possible-completions-hippie-expand-command-creates-in-emacs 
(require 'ac-helm)
(require 'auto-complete-config)
(ac-config-default)
(defun autocomplete-buffer-with-helm-auto ()
    "select autocomplete candidates by helm. Useful to narrow candidates"
  (interactive)
  (let ((c (ac-candidates)))
    (if (= (length c) 1)
	; if we have a single expansion just do it
	(ac-expand)
      (when ac-completing
	(with-helm-show-completion
	 ac-point ac-last-point
	 (helm :sources 'helm-source-auto-complete-candidates
	       :buffer "*helm auto-complete"))))))

(defun autocomplete-buffer-with-helm ()
  (interactive)
  (ignore-errors
    (call-interactively 'auto-complete)
    (call-interactively 'autocomplete-buffer-with-helm-auto)))

(provide 'autocomplete-buffer)
