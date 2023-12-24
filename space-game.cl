(load "custom-repl.cl")

(defparameter *planet-names* '(archimedes pythagoras pliny plato socrates))
(defparameter *name-uses* (make-hash-table))
(defparameter *gates* (make-hash-table))
(defparameter *planets* ())
(defparameter *current-planet* nil)
(defparameter *max-galaxy-size* 40)
(defparameter *systems* '(engines sensors))
(defparameter *loaded-systems* '(engines sensors))

(defstruct planet name scanned)

(defmacro systemfunc (system name args &body body)
  `(setf (symbol-function (quote ,name))
         (lambda ,args
	   (if (member (quote ,system) *loaded-systems*)
	       ,@body
	       '(the required system is not loaded)))))

(mapcan (lambda (name) (setf (gethash name *name-uses*) (random 5))) *planet-names*)

(defun rand-nth (list)
  (if (> (length list) 1)
      (nth (random (length list)) list)
      (car list)))

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
  (loop for i below (min (+ (random 3) 2) (length *planets*))
  	do (link-planets new-planet (rand-nth *planets*))))

(defun get-planet ()
  (let ((planet (make-planet :name (get-planet-name)
       			     :scanned nil)))
    (set-planet-gates planet)
    (setf *planets* (cons planet *planets*))
    planet))
    
(defun start-game ()
  (setf *current-planet* (get-planet))
  (custom-repl))

(defun scan ()
  (when (and (not (planet-scanned *current-planet*)) (< (length *planets*) *max-galaxy-size*))
      (setf (planet-scanned *current-planet*) t)
      (loop for i below (+ (random 3) 1)
      	    do (let ((planet (get-planet)))
	         (link-planets planet *current-planet*))))
  (cons `(you are on ,(planet-name *current-planet*) and there are gates to )
  	 (loop for p in (gethash *current-planet* *gates*)
  	       collect (planet-name p))))

(defun get-planet-with-name (planet-id list)
  (if (car list)
      (if (equal (planet-name (car list)) planet-id)
      	  (car list)
	  (get-planet-with-name planet-id (cdr list)))
      nil))

(defun fly (name number)
  (let* ((planet-id (cons name (cons number nil)))
        (linked-planets (gethash *current-planet* *gates*))
	(planet (get-planet-with-name planet-id *planets*)))
    (if (and planet (member planet linked-planets))
    	(progn (setf *current-planet* planet)
	       `(you have flown to ,planet-id))
	'(you cannot fly there))))
	