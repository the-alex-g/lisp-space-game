(defparameter *planet-names* '(archimedes pythagoras pliny plato socrates))
(defparameter *name-uses* (make-hash-table))
(defparameter *gates* (make-hash-table))
(defparameter *planets* ())
(defparameter *current-planet* nil)
(defparameter *max-galaxy-size* 40)
(defparameter *systems* '(engines sensors shields weapons))
(defparameter *loaded-systems* '(engines sensors))
(defparameter *max-systems-loaded* 3)
(defparameter *forbidden-commands* '())
(defparameter *commands* '(fly scan upload unload defcmd systems fire charge discharge))
(defparameter *custom-commands* '())
(defparameter *current-encounter* nil)
(defparameter *player-health* 15)
(defparameter *charged-systems* '())
(defparameter *charges* 3)
(defparameter *common-encounter-constructors* ())
(defparameter *uncommon-encounter-constructors* ())
(defparameter *rare-encounter-constructors* ())

(defstruct planet name scanned)
(defstruct encounter on-finish)

(defmacro defencounter (name intro-text &rest slots)
  `(progn (defstruct (,name (:include encounter)) ,@slots)
  	  (defmethod get-intro-text ((encounter ,name))
	    ,intro-text)))

(defencounter pirate '(a ruthless pirate attacks!) (health (+ 1 (random 4)))
	   	   	     		 	   (shields (eq 0 (random 3)))
					 	   (engines (random 25)))
(push #'make-pirate *common-encounter-constructors*)

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

(defun range (min max)
  (+ min (random (- (+ 1 max) min))))

(defun quit-game ()
  (mapcan 'fmakunbound *custom-commands*)
  '(process terminated))

(defun get-legal-commands ()
  (labels ((eat (list)
  	     (if list
	     	 (if (member (car list) *forbidden-commands*)
		     (eat (cdr list))
		     (cons (car list) (eat (cdr list))))
		 nil)))
    (eat *commands*)))

(defun custom-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((add-quotes (to) (list 'quote to)))
      (cons (car cmd) (mapcar #'add-quotes (cdr cmd))))))

(defun custom-eval (cmd)
  (if (member (car cmd) (get-legal-commands))
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
    (if (eq (car cmd) 'quit)
    	(quit-game)
      	(flet ((run-repl ()
	         (custom-print (custom-eval cmd))
		 (when *current-encounter*
		       (encounter-process *current-encounter*))
      	         (custom-repl)))
	  (run-repl)))))

(defun get-system-strength (system default charged)
	   (if (member system *loaded-systems*)
	       (cond ((member system *charged-systems*)
		      (setf *charged-systems* (remove system *charged-systems*))
		      charged)
		     (t
		      default))
	       0))

(defun attack-player (power)
  (if (< (random 100) (get-system-strength 'engines 15 50))
      (custom-print '(you dodged the attack))
      (let ((damage (- (range 1 (+ power 1)) (get-system-strength 'shields 1 2))))
	(if (< damage 1)
	    (custom-print '(your shields blocked the attack))
	    (progn (decf *player-health* damage)
	    	   (custom-print `(you have taken ,damage damage and have ,*player-health* health left)))))))
     
(defmethod encounter-process (encounter)
  (eval (encounter-on-finish encounter)))

(defmethod encounter-process ((encounter pirate))
  (cond ((< (pirate-health encounter) 1)
  	 (custom-print '(you defeated the pirate!))
	 (setf *current-encounter* nil)
	 (custom-print (eval (encounter-on-finish encounter))))
	(t
	 (attack-player 1)
	 (custom-print `(the pirate has ,(pirate-health encounter) health remaining)))))

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
  (if (or (member name *custom-commands*) (not (fboundp name)))
    (let ((code (quote-lines (break-into-lines body) args)) (is-old (fboundp name)))
      (setf *commands* (cons name *commands*))
      (eval `(setf (symbol-function (quote ,name))
       	  	   (lambda ,args ,@code)))
      (setf *custom-commands* (cons name *custom-commands*))
      (if is-old
      	  `(redefined command ,name)
	  `(defined new command ,name)))
    '(invalid command name)))

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
      (loop for i below (range 1 3)
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

(defun start-encounter (finish)
  (flet ((get-encounter (index)
  	   (flet ((make-encounter (builder)
	   	    (funcall builder :on-finish finish)))
	     (cond ((< index 3)
	     	    (make-encounter (rand-nth *common-encounter-constructors*)))))))
		   ;((< index 5)
		    ;(make-encounter (rand-nth *uncommon-encounter-constructors*)))
		   ;(t
		    ;(make-encounter (rand-nth *rare-encounter-constructors*)))
    (setf *current-encounter* (get-encounter (random 3)))
    (get-intro-text *current-encounter*)))

(systemfunc engines fly (name number)
  (let* ((planet-id (cons name (cons number nil)))
         (linked-planets (gethash *current-planet* *gates*))
 	 (planet (get-planet-with-name planet-id *planets*)))
    (if (and planet (member planet linked-planets))
        (let ((action `(progn (setf *current-planet* ,planet)
	     	      	      '(you have flown to ,planet-id))))
	  (if (eq (random 2) 0)
	      (start-encounter action)
	      (eval action)))
 	'(you cannot fly there))))

(defmethod attack (encounter)
  '(you have fired on the encounter... i guess))

(defmethod attack ((encounter pirate))
    (if (< (random 100) (pirate-engines encounter))
    	'(the pirate dodges your attack)
  	 (let ((damage (get-system-strength 'weapons (range 1 2) (range 2 3))))
    	   (when (pirate-shields encounter)
    	   	 (decf damage))
           (decf (pirate-health encounter) damage)
	   (if (eq 0 damage)
	       '(the pirates shields blocked your attack)
	       `(you dealt ,damage damage to the pirate)))))

(systemfunc weapons fire ()
  (attack *current-encounter*))

(defun charge (system)
  (cond ((not (member system *loaded-systems*))
   	 `(,system are not loaded))
        ((eq *charges* 0)
  	 '(your power core is exhausted))
	((member system *charged-systems*)
	 '(that system is already charged))
	(t
	 (decf *charges*)
	 (setf *charged-systems* (cons system *charged-systems*))
	 `(you have ,*charged-systems* charged and ,*charges* charges remaining))))

(defun discharge (system)
  (cond ((not (member system *charged-systems*))
  	 `(your ,system are not charged))
	(t
	 (unless (eq *charges* 3)
	 	 (incf *charges*))
	 (setf *charged-systems* (remove system *charged-systems*))
	 `(you have discharged ,system and have ,*charged-systems* charged with ,*charges* charges remaining))))

(defun systems ()
  `(you have ,*loaded-systems* loaded))

(defun upload (system)
  (if (member system *systems*)
      (if (member system *loaded-systems*)
      	  `(,system are already loaded)
          (if (< (length *loaded-systems*) *max-systems-loaded*)
       	      (progn (setf *loaded-systems* (cons system *loaded-systems*))
	  	     (systems))
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
      (cond ((member system *loaded-systems*)
	     (setf *loaded-systems* (remove system *loaded-systems*))
	     (discharge system)
	     (systems))
	    (t
             `(,system is not loaded)))))