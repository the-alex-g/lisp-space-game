(defparameter *planet-names* '(archimedes pythagoras pliny plato socrates))
(defparameter *name-uses* (make-hash-table))

(defun rand-nth (list)
  (if (> (length list) 1)
      (nth (random (length list)) list)
      (car list)))

(defun initiate-name-uses ()
  (mapcan (lambda (planet-name)
                  (setf (gethash planet-name *name-uses*) (random 5)))
	  *planet-names*))

(defun get-planet-name ()
  (let ((planet-name (rand-nth *planet-names*)))
    (incf (gethash planet-name *name-uses*))
    (cons planet-name (cons (gethash planet-name *name-uses*) nil))))

(defun start-game ()
  (initiate-name-uses))