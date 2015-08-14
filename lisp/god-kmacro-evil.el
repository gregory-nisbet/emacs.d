;; god-kmacro-evil
;; allow god mode for fun and profit



;; create buffer local variable for storing whether we exited
;; god-mode for kmacro (this is independent of other pause and
;; resume stuff.
(defvar god-kmacro-paused)
(make-variable-buffer-local 'god-kmacro-paused)
(setf god-kmacro-paused nil)
;; advise key macros to start you in non-godmode regardless
;; remember state before macro was executed
(defvar god-mode-kmacro-call-paused)
(make-variable-buffer-local 'god-kmacro-call-paused)
(setf god-kmacro-call-paused nil)

(defvar god-kmacro-evil-paused)
(make-variable-buffer-local 'god-kmacro-evil-paused)
(setf god-kmacro-evil-paused nil)

(defvar god-kmacro-evil-call-paused)
(make-variable-buffer-local 'god-kmacro-evil-call-paused)
(setf god-kmacro-evil-call-paused)

(defadvice kmacro-start-macro (before god-kmacro-start-macro activate)
  "disable godmode after kmacro begin"
  ;; recursive kmacro god-mode status tracking is not implemented yet
  ;; if the god-mode is not nil, then we're already in a macro
  ;; or didn't cleanly reset the buffer local variable.
  (cl-assert (not god-kmacro-paused))
  (setf god-kmacro-paused (if god-local-mode +1 0))
  ;; evil-mode
  (cl-assert (not god-kmacro-evil-paused))
  (setf god-kmacro-evil-paused evil-state)
  ;; exit god-mode and evil whatever
  (god-local-mode 0)
  (evil-emacs-state))

(defadvice kmacro-end-macro (around god-kmacro-end-macro activate)
  (unwind-protect
      (condition-case ex
          ad-do-it
        ('error (message (format "Caught exception [%s]" ex))))
    (progn
      (cl-assert god-kmacro-paused)
      (god-local-mode god-kmacro-paused)
      (setf god-kmacro-paused nil)
      
      (cl-assert god-kmacro-evil-paused)
      (evil-change-state god-kmacro-evil-paused)
      (setf god-kmacro-evil-paused nil))))
        

;; disable god mode and store previous god-mode state
(defadvice kmacro-call-macro (before god-kmacro-call-macro-start activate)
  "disable god-mode before executing macro"
  (cl-assert (not god-kmacro-call-paused))
  (setf god-kmacro-call-paused (if god-local-mode 1 0))

  (cl-assert (not god-kmacro-evil-call-paused))
  (setf god-kmacro-evil-call-paused evil-state)
  
  (god-local-mode 0)
  (evil-emacs-state))

;; reenable god-mode after executing keyboard macro
;; wrap errors safely
(defadvice kmacro-call-macro (around god-kmacro-call-macro-end activate)
  "re-enable god-mode after executing macro"
  (unwind-protect
      (condition-case ex
          ad-do-it
        ('error (message (format "Caught exception [%s]" ex))))
    (progn
      (cl-assert god-kmacro-call-paused)
      (god-local-mode god-kmacro-call-paused)
      (setf god-kmacro-call-paused nil)

      (cl-assert god-kmacro-evil-call-paused)
      (evil-change-state god-kmacro-evil-call-paused)
      (setf god-kmacro-evil-call-paused nil))))

(provide 'god-kmacro-evil)
