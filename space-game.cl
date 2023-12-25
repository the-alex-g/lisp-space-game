(load "custom-repl.cl")

(defparameter *planet-names* '(archimedes pythagoras pliny plato socrates))
(defparameter *name-uses* (make-hash-table))
(defparameter *gates* (make-hash-table))
(defparameter *planets* ())
(defparameter *current-planet* nil)
(defparameter *max-galaxy-size* 40)
(defparameter *systems* '(engines sensors))
(defparameter *loaded-systems* '(engines sensors))
(defparameter *max-systems-loaded* 3)
(defparameter *cannot-define* '(scan fly unload upload))

(defstruct planet name scanned)

(mapcan (lambda (name) (setf (gethash name *name-uses*) (random 5))) *planet-names*)

(defmacro systemfunc (system name args &body body)
  `(setf (symbol-function (quote ,name))
         (lambda ,args
	   (cond ((member (quote ,system) *loaded-systems*) ,@body)
	      	 (t '(the required system is not loaded))))))

(defmacro break-off-line (list &body body)
	   `(let ((line ()) (rest ()))
	     (labels ((get-line (l started)
			(if l
			    (if (eq (car l) '\[)
				(if started
				    (progn (setf rest l)
					   nil)
				    (get-line (cdr l) t))
				(cons (car l) (get-line (cdr l) started)))
			    nil)))
	       (setf line (get-line ,list nil))
	       ,@body)))

(defun break-into-lines (list)
  (labels ((parse (l)
  	      (break-off-line l
	        (if rest
		    (cons line (parse rest))
		    (list line)))))
    (parse list)))

(defun quote-lines (lines exclude)
  (mapcar (lambda (line)
  	    (cons (car line) (mapcar (lambda (item)
	    	       	     	       (if (member item exclude)
				       	   item
					   `(quote ,item)))
			 	     (cdr line))))
	  lines))

(defun defcmd (name args &rest body)
  (if (member name *cannot-define*)
    '(invalid command name)
    (let ((code (quote-lines (break-into-lines body) args)))
      (setf *legal-commands* (cons name *legal-commands*))
      (eval `(setf (symbol-function (quote ,name))
       	  	   (lambda ,args ,@code)))
      `(defined new command ,name))))

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

(defun get-planet ()
  (let ((planet (make-planet :name (get-planet-name)
       			     :scanned nil)))
    (setf *planets* (cons planet *planets*))
    planet))
    
(defun start-game ()
  (setf *current-planet* (get-planet))
  (custom-repl))

(systemfunc sensors scan ()
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

(systemfunc engines fly (name number)
  (let* ((planet-id (cons name (cons number nil)))
        (linked-planets (gethash *current-planet* *gates*))
	(planet (get-planet-with-name planet-id *planets*)))
    (if (and planet (member planet linked-planets))
    	(progn (setf *current-planet* planet)
	       `(you have flown to ,planet-id))
	'(you cannot fly there))))

(defun upload (system)
  (if (member system *systems*)
      (if (member system *loaded-systems*)
      	  `(,system are already loaded)
          (if (< (length *loaded-systems*) *max-systems-loaded*)
       	      (progn (setf *loaded-systems* (cons system *loaded-systems*))
	  	     `(you have ,*loaded-systems* loaded))
	      '(you already have max systems loaded!)))
      '(that system does not exist)))

(defun unload (system)
  (if (eq system 'all)
      (labels ((un ()
      	         (if *loaded-systems*
		     (progn (unload (car *loaded-systems*))
		     	    (un))
	 	     '(you have unloaded all systems))))
	(un))
      (if (member system *loaded-systems*)
      	  (labels ((u (list)
	  	      (if list
		      	  (if (eq (car list) system)
			      (cdr list)
			      (cons (car list) (u (cdr list)))))))
	    (progn (setf *loaded-systems* (u *loaded-systems*))
	    	   `(you have ,*loaded-systems* loaded)))
	  `(,system is not loaded))))