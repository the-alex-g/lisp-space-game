(defparameter *planet-names* '(archimedes pythagoras pliny plato socrates))
(defparameter *name-uses* (make-hash-table))
(defparameter *gates* (make-hash-table))
(defparameter *planets* ())
(defparameter *current-planet* nil)
(defparameter *max-galaxy-size* 40)
(defparameter *locked-systems* '(drones))
(defparameter *systems* '(engines sensors shields weapons))
(defparameter *loaded-systems* '(engines sensors weapons))
(defparameter *max-systems-loaded* 3)
(defparameter *always-allowed-commands* '(upload unload defcmd systems charge discharge commands))
(defparameter *default-allowed-commands* '(fly scan))
(defparameter *allowed-commands* *default-allowed-commands*)
(defparameter *commands* '())
(defparameter *custom-commands* '())
(defparameter *current-encounter* nil)
(defparameter *max-player-health* 15)
(defparameter *player-health* *max-player-health*)
(defparameter *resources* 10)
(defparameter *money* 10)
(defparameter *charged-systems* '())
(defparameter *charges* 3)
(defparameter *common-encounter-constructors* ())
(defparameter *uncommon-encounter-constructors* ())
(defparameter *rare-encounter-constructors* ())
(defparameter *alignment* 5)
(defparameter *quest* nil)

(defstruct quest destination cost reward)

(defun range (min max)
  (+ min (random (- (+ 1 max) min))))

(defun rand-nth (list)
  (if (> (length list) 1)
      (nth (random (length list)) list)
      (car list)))

(defmacro multiprogn (&body body)
  (labels ((add-progn (lines)
  	     (if (> (length lines) 1)
	     	 `(progn ,(car lines) ,(add-progn (cdr lines)))
		 (car lines))))
    (add-progn body)))

(defstruct planet name scanned (alignment (+ (range 0 5) (range 0 5))))
(defstruct encounter on-finish)

(defmacro defencounter (rarity name intro-text allowed-commands include &rest slots)
  `(multiprogn (defstruct (,name (:include ,include)) ,@slots)
  	       (defmethod encounter-intro-text ((encounter ,name))
	         ,intro-text)
	       (defmethod encounter-allowed-commands ((encounter ,name))
	         ,allowed-commands)
	       ,(when rarity
  	     	   `(push ,(read-from-string (concatenate 'string "#'make-" (symbol-name name)))
  	     	    	  	,(read-from-string (concatenate 'string
		    		       		    	  	"*"
			  		     	    	  	(symbol-name rarity)
							  	"-encounter-constructors*"))))))

(defencounter common pirate '(a ruthless pirate attacks!)
	      	     '(fire flee)
		     encounter
	      	     (health (range 1 4))
	   	     (shields (eq 0 (random 3)))
		     (engines (random 26)))

(defencounter uncommon thieves '(thieves steal from your ship!)
	      	       nil
		       encounter)

(defencounter uncommon police '(a law enforcement vessel appears)
	      	       nil
		       encounter)

(defencounter uncommon derilect '(you found an abandoned spaceship)
	      	       '(fire salvage leave)
		       encounter
		       (type (rand-nth '(empty full full full full
		       	     	         danger danger danger danger pirate))))

(defencounter common quest-giver '(A passing spaceship asks you for a favor)
	      	     nil
		     encounter)

(defencounter common merchant '(you found a wandering space merchant)
	      	     '(fire buy sell repair leave)
		     encounter
		     (exchange-rate (range 2 5))
		     (sell-rate 0)
		     (repair-cost (range 3 10)))

(defencounter uncommon system-merchant '(you find a wandering space merchant)
	      	       '(fire buy sell repair leave)
		       merchant
		       (system-cost (* (/ (range 1 8) 2) 10))
		       (system (rand-nth *locked-systems*)))

(defencounter nil hostile-merchant nil
	      	  '(fire flee)
		  encounter
		  (health (range 1 5))
		  (shields (eq 0 (random 2)))
		  (engines (random 16)))

(defencounter nil hostile-thieves nil
	      	  '(fire flee)
		  pirate
		  (money 0)
		  (resources 0))

(defencounter nil hostile-police nil
	      	  '(fire flee)
		  encounter
		  (health (range 2 5))
		  (shields (< 1 (random 3)))
		  (engines (random 31)))

(mapcan (lambda (name) (setf (gethash name *name-uses*) (random 5))) *planet-names*)
(defparameter *test-encounter* #'make-quest-giver)

(defun allowed-commands ()
  (concatenate 'list *allowed-commands* *always-allowed-commands*))

(defmacro command (name args &body body)
  `(progn (defun ,name (,@args &key always-process &allow-other-keys)
    	    (cond ((or (member (quote ,name) (allowed-commands))
	       	       always-process)
	           ,@body)
	  	  (t
	   	   '(invalid command))))
          (push (quote ,name) *commands*)))

(defmacro no-override-cmd (name args &body body)
  `(progn (defun ,name (,@args)
    	    (cond ((or (member (quote ,name) (allowed-commands)))
	           ,@body)
	  	  (t
	   	   '(invalid command))))
          (push (quote ,name) *commands*)))

(defmacro systemfunc (system name args &body body)
  `(command ,name ,args
    (cond ((member (quote ,system) *loaded-systems*)
    	   ,@body)
	  (t
	   '(the required system is not loaded)))))

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

(defun quit-game ()
  (mapcan 'fmakunbound *custom-commands*)
  (setf *custom-commands* nil)
  '(process terminated))

(defun custom-read ()
  (princ '>>>)
  (princ #\space)
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((add-quotes (to) (list 'quote to)))
      (cons (car cmd) (mapcar #'add-quotes (cdr cmd))))))

(defun custom-eval (cmd)
  (if (member (car cmd) *commands*)
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

(defun show-player ()
  (princ #\newline)
  (unless *current-encounter*
  	  (custom-print `(you are on ,(planet-name *current-planet*)
  		  	  and there are gates to ,(mapcar (lambda (planet)
		      	    	      	 	  	  (planet-name planet))
						  (gethash *current-planet* *gates*))))
	  (when *quest*
	  	(custom-print `(you have been tasked with delivering ,(quest-cost *quest*) to ,(planet-name (quest-destination *quest*))))))
  (princ "Health ")
  (loop for i below *player-health*
  	do (princ "|"))
  (fresh-line)
  (custom-print `(you have ,*loaded-systems* loaded))
  (if *charged-systems*
      (custom-print `(you have ,*charged-systems* charged and ,*charges* charges remaining))
      (custom-print `(you have ,*charges* charges remaining)))
  (custom-print `(you have ,*resources* resources and ,*money* money)))

(defun custom-repl ()
  (let ((cmd (custom-read)))
    (if (eq (car cmd) 'quit)
    	(quit-game)
      	(multiprogn (custom-print (custom-eval cmd))
		    (when *current-encounter*
		       	  (encounter-process *current-encounter*))
		    (show-player)
		    (custom-repl)))))

(defun get-system-strength (system default charged)
	   (if (member system *loaded-systems*)
	       (cond ((member system *charged-systems*)
		      (setf *charged-systems* (remove system *charged-systems*))
		      charged)
		     (t
		      default))
	       0))

(command leave ()
  (eval (encounter-on-finish *current-encounter*)))

(defun attack-player (power)
  (if (< (random 100) (get-system-strength 'engines 15 50))
      (custom-print '(you dodged the attack))
      (let ((damage (- (range 1 (+ power 1)) (get-system-strength 'shields 1 2))))
	(if (< damage 1)
	    (custom-print '(your shields blocked the attack))
	    (progn (decf *player-health* damage)
	    	   (custom-print `(you have taken ,damage damage)))))))

(defun gain-money (amount)
  (incf *money* amount)
  (when (> amount 0)
  	(custom-print `(you gained ,amount money))))

(defun gain-resources (amount)
  (incf *resources* amount)
  (when (> amount 0)
  	(custom-print `(you gained ,amount resources))))

(defun choose (choices actions)
  (custom-print `(enter one of the following options ,choices))
  (let ((cmd (car (custom-read))))
    (if (eq cmd 'quit)
    	(quit-game)
      	(if (member cmd choices)
	    (eval (cadr (assoc cmd actions)))
	    (choose choices actions)))))

(defun show-merchant-wares (merchant)
  (when (eq (merchant-sell-rate merchant) 0)
  	(setf (merchant-sell-rate merchant) (max 1 (- (merchant-exchange-rate merchant) 1 (* (range 0 1) (range 0 1))))))
  (princ #\newline)
  (custom-print `(you can buy resources for ,(merchant-exchange-rate merchant) money))
  (custom-print `(you can sell resources for ,(merchant-sell-rate merchant) money))
  (custom-print `(you can buy repairs for ,(merchant-repair-cost merchant) money)))

(defmacro change-encounter (new-encounter-builder message &rest flags)
  `(multiprogn (setf *current-encounter*
  	       	     (funcall ,new-encounter-builder
  	       	     	      ,@flags
  	       	     	      :on-finish (encounter-on-finish *current-encounter*)))
  	       (setf *allowed-commands* (encounter-allowed-commands *current-encounter*))
	       (custom-print ,message)))

(defun process-hostile-encounter (encounter-name encounter-health
       				  attack-power defeated-options rewards)
  (cond ((< encounter-health 1)
  	 (custom-print `(you defeated the ,encounter-name))
	 (choose defeated-options rewards)
	 (custom-print (leave :always-process t)))
	(t
	 (attack-player attack-power)
	 (custom-print `(the ,encounter-name has ,encounter-health health left)))))

(defmethod encounter-process (encounter)
  (leave :always-process t))

(defmethod encounter-process ((encounter quest-giver))
  (princ #\newline)
  (let* ((destination (rand-nth *planets*))
         (cost (range 2 10))
	 (deliver-item (rand-nth '(resources money)))
	 (aquire-item (if (eq deliver-item 'resources) 'money 'resources))
	 (reward (+ cost (range 1 5))))
    (custom-print `(a passing ship asks you to deliver ,cost ,deliver-item to ,(planet-name destination) in exchange for ,reward ,aquire-item))
    (when *quest*
    	  (custom-print '(note that accepting this quest will replace your current one)))
    (choose '(accept decline)
    	    `((accept (setf *quest* (make-quest :destination ,destination
	    	      	    	    		:cost '(,cost ,deliver-item)
	    	      	    	    		:reward '(multiprogn (custom-print '(you have completed the delivery))
								     (if (eq (quote ,aquire-item) 'resources)
								     	 (gain-resources ,reward)
									 (gain-money ,reward))))))
              (decline ()))))
  (leave :always-process t))

(defmethod encounter-process ((encounter derilect))
  (princ #\newline)
  (custom-print '(you may attempt to salvage the wreck)))

(defmethod encounter-process ((encounter police))
  (cond ((< *alignment* 3)
	 (change-encounter #'make-hostile-police
	 		   '(the ship turns and opens fire!)))
	(t
	 (custom-print '(after a short while the ship leaves))
	 (custom-print (leave :always-process t)))))
      
(defmethod encounter-process ((encounter pirate))
  (process-hostile-encounter 'pirate (pirate-health encounter) 1
  			     '(loot release destroy)
	 	       	     `((loot (progn (gain-money ,(range 0 10))
		       	      	     	    (gain-resources ,(range 5 15))))
			       (release (progn (decf *alignment*)
					       (if (eq 0 ,(random 2))
			 	      	       	   (gain-money ,(range 0 10))
				      	    	   (gain-resources ,(range 0 10)))))
			       (destroy (progn (gain-money ,(range 5 20))
			       		       (incf *alignment*))))))
  
(defmethod encounter-process ((encounter hostile-merchant))
  (process-hostile-encounter 'merchant (hostile-merchant-health encounter) 1
  			     '(loot release destroy)
	 	       	     `((loot (multiprogn (gain-money ,(range 5 15))
		       	      	     	    	 (gain-resources ,(range 10 20))
						 (decf *alignment*)))
			       (release (if (eq 0 ,(random 3))
			 	      	    (gain-money ,(range 5 10))
				      	    (gain-resources ,(range 5 15))))
			       (destroy (progn (custom-print '(you gain nothing for
			       		       		       destroying the merchant))
			       		       (decf *alignment*))))))

(defmethod encounter-process ((encounter hostile-police))
  (process-hostile-encounter 'police (hostile-police-health encounter) 1
  			     '(loot release destroy)
			     `((loot (if (eq 0 ,(random 2))
			     	     	 (custom-print '(there is nothing valuable on board))
					 (if (eq 0 ,(random 2))
					     (gain-money ,(range 0 10))
					     (gain-resources ,(range 0 5)))))
			       (release (custom-print '(the ship warps away)))
			       (destroy (progn (gain-resources ,(range 1 5))
			       		       (decf *alignment* 2))))))

(defmethod encounter-process ((encounter hostile-thieves))
  (process-hostile-encounter 'thieves (hostile-thieves-health encounter) 1
  			     '(loot release destroy)
	 	       	     `((loot (progn (gain-money ,(+ (range 5 15)
		       	       	      	    		 (hostile-thieves-money encounter)))
		       	      	            (gain-resources ,(+ (range 0 10)
				     		      	    (hostile-thieves-resources encounter)))))
			       (release (progn (if (eq 0 ,(random 2))
			 	      	       	   (gain-money ,(range 0 10))
				      	    	   (gain-resources ,(range 0 10)))
					       (decf *alignment*)))
			       (destroy (progn (gain-money ,(range 10 20))
			       		       (incf *alignment*))))))

(defmethod encounter-process ((encounter merchant))
  (show-merchant-wares encounter))

(defmethod encounter-process ((encounter system-merchant))
  (show-merchant-wares encounter)
  (when (system-merchant-system encounter)
  	(custom-print `(you can buy the ,(system-merchant-system encounter) system for
		       ,(system-merchant-system-cost encounter) money))))

(defmethod encounter-process ((encounter thieves))
  (let ((money-lost (min *money* (range 5 10)))
        (resources-lost (min *resources* (range 5 10))))
    (when (> money-lost 0)
    	  (decf *money* money-lost)
	  (custom-print `(they took ,money-lost money)))
    (when (> resources-lost 0)
    	  (decf *resources* resources-lost)
	  (custom-print `(they took ,resources-lost resources)))
    (if (member 'sensors *loaded-systems*)
    	(choose '(chase leave)
		`((chase (if (< (random 100) (get-system-strength 'engines 15 50))
			   (change-encounter #'make-hostile-thieves
			   		     '(you chase down the thieves)
					     :resources ,resources-lost
					     :money ,money-lost)
	                   (progn (custom-print '(you are unable to track down the thieves))
	     	    	   	  (leave :always-process t))))
		  (leave (leave :always-process t))))
	(leave :always-process t))))
  
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

(no-override-cmd defcmd (name args &rest body)
  (if (or (member name *custom-commands*) (not (fboundp name)))
    (let ((code (quote-lines (break-into-lines body) args)) (is-old (fboundp name)))
      (push name *commands*)
      (eval `(setf (symbol-function (quote ,name))
       	  	   (lambda ,args ,@code)))
      (push name *custom-commands*)
      (if is-old
      	  `(redefined command ,name)
	  `(defined new command ,name)))
    '(invalid command name)))

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
	   	    (when *test-encounter* (setf builder *test-encounter*))
	   	    (funcall builder :on-finish `(multiprogn (setf *allowed-commands* *default-allowed-commands*)
		    	     	     			     (setf *current-encounter* nil)
		    	     	     			     ,finish))))
	     (cond ((< index 3)
	     	    (make-encounter (rand-nth *common-encounter-constructors*)))
		   ((< index 5)
		    (make-encounter (rand-nth *uncommon-encounter-constructors*)))))))
		   ;(t
		    ;(make-encounter (rand-nth *rare-encounter-constructors*)))
    (setf *current-encounter* (get-encounter (random 5)))
    (setf *allowed-commands* (encounter-allowed-commands *current-encounter*))
    (encounter-intro-text *current-encounter*)))

(systemfunc engines fly (name number)
  (let* ((planet-id (cons name (cons number nil)))
         (linked-planets (gethash *current-planet* *gates*))
 	 (planet (get-planet-with-name planet-id *planets*)))
    (if (and planet (member planet linked-planets))
        (let ((action `(change-planet ,planet (quote ,planet-id))))
	  (if (eq (random 2) 0)
	      (start-encounter action)
	      (eval action)))
 	'(you cannot fly there))))

(defun complete-quest ()
  (let ((variable (if (eq (cdr (quest-cost *quest*)) 'resources) '*resources* '*money*))
  	(cost (car (quest-cost *quest*))))
    (if (>= (eval variable) cost)
    	(multiprogn (eval `(decf ,variable ,cost))
		    (eval (quest-reward *quest*))
	       	    (setf *quest* nil))
	(custom-print `(you do not have enough ,(cdr (quest-cost *quest*)) to complete the delivery at this time)))))

(defun change-planet (new-planet planet-name)
  (setf *current-planet* new-planet)
  (when (> (abs (- *alignment* (planet-alignment *current-planet*))) 5)
      	 (custom-print `(As you approach ,planet-name the planetary defense systems open fire on your ship))
	 (attack-player (range 2 4)))
  (when *quest*
  	(when (equal (planet-name (quest-destination *quest*)) planet-name)
	      (complete-quest)))
  `(you have arrived at ,planet-name))

(defun attack-encounter (encounter-name engines shields &key name-prefix)
  (if (< (random 100) engines)
      `(the ,encounter-name dodges your attack)
      (let ((damage (get-system-strength 'weapons (range 1 2) (range 2 3))))
        (when shields
	      (decf damage))
	(eval (if name-prefix
	      	  `(decf (,(read-from-string (concatenate 'string (symbol-name name-prefix)
		   	  		      		   	  "-"
								  (symbol-name encounter-name)
							      	  "-health")) *current-encounter*)
		         ,damage)
	      	  `(decf (,(read-from-string (concatenate 'string (symbol-name encounter-name)
							      	  "-health")) *current-encounter*)
		         ,damage)))
	(if (eq 0 damage)
	    `(the ,encounter-name shields deflected your attack)
	    `(you dealt ,damage damage to the ,encounter-name)))))

(defmethod attack (encounter)
  '(you have fired on the encounter... i guess))

(defmethod attack ((encounter pirate))
  (attack-encounter 'pirate (pirate-engines encounter) (pirate-shields encounter)))

(defmethod attack ((encounter hostile-merchant))
  (attack-encounter 'merchant (hostile-merchant-engines encounter) (hostile-merchant-shields encounter) :name-prefix 'hostile))

(defmethod attack ((encounter hostile-thieves))
  (attack-encounter 'thieves (hostile-thieves-engines encounter) (hostile-thieves-shields encounter) :name-prefix 'hostile))

(defmethod attack ((encounter hostile-police))
  (attack-encounter 'police (hostile-police-engines encounter)
  		    	    (hostile-police-shields encounter)
			    :name-prefix 'hostile))

(defmethod attack ((encounter merchant))
  (decf *alignment*)
  (cond ((eq (random 2) 0)
      	 (change-encounter #'make-hostile-merchant
	 		   '(the merchant prepares to retaliate!))
	 (attack *current-encounter*))
	(t
	 (custom-print '(the merchant flees into interplanetary space))
	 (leave))))

(defmethod attack ((encounter derilect))
  (cond ((eq (derilect-type encounter) 'pirate)
  	 (change-encounter #'make-pirate '(the wreck powers on and prepares to attack!))
	 (attack *current-encounter*))
	((eq (derilect-type encounter) 'empty)
	 (custom-print '(the wreck disintegrates))
	 (leave))
	(t
	 (custom-print '(the wreck disintegrates leaving behind a few resources))
	 (gain-resources (range 1 3))
	 (leave))))

(systemfunc weapons fire ()
  (attack *current-encounter*))

(command charge (system)
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

(command discharge (system)
  (cond ((not (member system *charged-systems*))
  	 `(your ,system are not charged))
	(t
	 (unless (eq *charges* 3)
	 	 (incf *charges*))
	 (setf *charged-systems* (remove system *charged-systems*))
	 `(you have discharged ,system and have ,*charged-systems* charged with ,*charges* charges remaining))))

(command systems ()
  `(you have ,*loaded-systems* loaded))

(command upload (system)
  (if (member system *systems*)
      (if (member system *loaded-systems*)
      	  `(,system are already loaded)
          (if (< (length *loaded-systems*) *max-systems-loaded*)
       	      (progn (setf *loaded-systems* (cons system *loaded-systems*))
	  	     (systems))
	      '(you already have max systems loaded!)))
      '(that system does not exist)))

(command unload (system)
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

(command sell (amount)
  (if (eq amount 'all)
      (sell *resources*)
      (cond ((>= *resources* amount)
      	     (decf *resources* amount)
	     (let ((money-gained (* (merchant-sell-rate *current-encounter*) amount)))
	       (incf *money* money-gained)
	       `(you sold ,amount resources for ,money-gained money)))
	    (t
	     '(you do not have enough resources)))))

(command buy (amount)
  (let ((cost-per-resource (merchant-exchange-rate *current-encounter*)))
    (if (eq amount 'all)
    	(buy (floor (/ *money* cost-per-resource)))
	(if (numberp amount)
	    (let ((cost (* cost-per-resource amount)))
	      (cond ((>= *money* cost)
	  	    (decf *money* cost)
		    (incf *resources* amount)
		    `(you bought ,amount resources for ,cost money))
		   (t
		    '(you do not have enough money))))
	    (if (eq (type-of *current-encounter*) 'system-merchant)
		(let ((system (system-merchant-system *current-encounter*))
		      (system-cost (system-merchant-system-cost *current-encounter*)))
		   (cond (system
		   	  (if (>= *money* system-cost)
		   	      (multiprogn (decf *money* system-cost)
		   	      		  (push system *systems*)
		   			  (remove system *locked-systems*)
		   			  (setf (system-merchant-system *current-encounter*) nil)
		   			  `(you bought the ,system system for ,system-cost money))
			      '(you do not have enough money)))
			 (t
			  '(you cannot buy that))))
		'(you cannot buy that))))))
		
(command repair (amount)
  (if (< *player-health* *max-player-health*)
      (let ((cost-per-repair (merchant-repair-cost *current-encounter*)))
        (if (eq amount 'all)
    	    (repair (min (- *max-player-health* *player-health*) (floor (/ *money* cost-per-repair))))
	    (let ((cost (* cost-per-repair amount)))
	      (cond ((>= *money* cost)
	  	     (decf *money* cost)
		     (incf *player-health* amount)
		     `(you repaired ,amount damage for ,cost money))
		    (t
		     '(you do not have enough money))))))
      '(you are already at max health)))

(command commands ()
  (if *custom-commands*
      (progn (custom-print `(built-in commands ,(allowed-commands)))
      	     `(custom commands ,*custom-commands*))
      (allowed-commands)))

(command salvage ()
  (cond ((eq (type-of *current-encounter*) 'derilect)
  	 (let ((type (derilect-type *current-encounter*)))
	   (cond ((eq type 'empty)
	   	  (custom-print '(you find nothing useful aboard the wreck))
		  (leave))
		 ((eq type 'full)
		  (gain-resources (range 0 5))
		  (gain-money (range 0 10))
		  (leave))
		 ((eq type 'danger)
		  (gain-resources (range 0 5))
		  (gain-money (range 0 10))
		  (let ((damage (max 0 (- (range 1 3)
		       		       	  (get-system-strength 'shields 1 2)))))
		    (decf *player-health* damage)
		    (if (eq damage 0)
		    	(custom-print '(the wreck collapses but your shields
				      	protect you from the debris))
			(custom-print `(the wreck collapses and you take ,damage damage)))
		    (leave)))
		 ((eq type 'pirate)
		  (change-encounter #'make-pirate '(the wreck powers on and prepares to attack!))
		  '(the wreck powers on and prepares to attack!)))))))

(command flee ()
  (let ((encounter-name (type-of *current-encounter*)))
    (cond ((> (+ (random 100) (random (get-system-strength 'engines 15 50)))
    	      (eval `(,(read-from-string (concatenate 'string (symbol-name encounter-name)
	      	    			 	      	      "-engines"))
	      	      *current-encounter*)))
	   (custom-print '(you escape from your attackers))
	   (leave :always-process t))
	  (t
	   '(your attackers are faster than you and you cannot escape)))))
	   