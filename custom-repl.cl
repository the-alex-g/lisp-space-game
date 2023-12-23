(defparameter *legal-commands* '(fly scan))

(defun custom-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((add-quotes (to) (list 'quote to)))
      (cons (car cmd) (mapcar #'add-quotes (cdr cmd))))))

(defun custom-eval (cmd)
  (if (member (car cmd) *legal-commands*)
      (eval cmd)
      '(invalid command)))

(defun format-text (list caps lit)
  (when list
    (let ((item (car list))
    	  (rest (cdr list)))
      (cond ((eql item #\space) (cons item (format-text rest caps lit)))
      	    ((member item '(#\! #\? #\.)) (cons item (format-text rest t lit)))
	    ((eql item #\") (format-text rest caps (not lit)))
	    (lit (cons item (format-text rest nil lit)))
	    (caps (cons (char-upcase item) (format-text rest nil lit)))
	    (t (cons (char-downcase item) (format-text rest nil nil)))))))

(defun get-trimmed-string (list)
  (remove #\( (remove #\) (prin1-to-string list))))

(defun custom-print (output)
  (princ (coerce (format-text (coerce (get-trimmed-string output) 'list) t nil) 'string))
  (fresh-line))

(defun custom-repl ()
  (let ((cmd (custom-read)))
    (unless (eq (car cmd) 'quit)
      (custom-print (custom-eval cmd))
      (custom-repl))))