(defun sequences ()
  "get only sequences from keymap"
  (cl-remove-if-not consp global-map))

