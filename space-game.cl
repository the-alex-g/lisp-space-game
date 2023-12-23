(defparameter *planet-names* '(archimedes pythagoras pliny plato socrates))
(defparameter *name-uses* (make-hash-table))
(defparameter *gates* (make-hash-table :test 'equal))
(defparameter *planets* ())

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

(defun link-planets (planet-a planet-b)
  (when (and planet-a planet-b)
    (unless (member planet-a (gethash planet-b *gates*))
      (setf (gethash planet-b *gates*) (cons planet-a (gethash planet-b *gates*))))
    (unless (member planet-b (gethash planet-a *gates*))
      (setf (gethash planet-a *gates*) (cons planet-b (gethash planet-a *gates*))))))

(defun set-planet-gates (new-planet)
  (setf (gethash new-planet *gates*) ())
  (loop for i below (min (+ (random 3) 2) (length *planets*))
  	do (link-planets new-planet (rand-nth *planets*))))

(defun get-planet ()
  (let ((planet-name (get-planet-name)))
    (set-planet-gates planet-name)
    (setf *planets* (cons planet-name *planets*))))

(defun start-game ()
  (initiate-name-uses))