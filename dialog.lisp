(defun run-dialog (args)
  (uiop:run-program (cons "dialog" args)
                    :output :interactive
                    :input :interactive
                    :error-output '(:string :stripped T)
                    :ignore-error-status T))

(defun checkbox-enum (l)
  (loop for item in l appending
        (list item (write-to-string 0))))

(defun checklist (prompt l)
  (multiple-value-bind (out tag ret)
      (run-dialog (concatenate 'list (list "--no-items" "--checklist" prompt "0" "0" "0") (checkbox-enum l)))
    (str:words tag)))

(defun menu-dispatch (title options)
  (multiple-value-bind (out tag ret)
      (run-dialog (concatenate
                    'list
                    (list "--menu" title "0" "0" "0")
                    (mapcan (lambda (op) (list (first op) (second op))) options)))
    (if (zerop ret)
        (funcall (third (find-if (lambda (x) (string-equal tag (first x))) options))))))

(defun dialog-clear ()
  (uiop:run-program "clear"
		    :output :interactive
		    :input :interactive))
