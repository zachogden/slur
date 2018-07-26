;;;; slur
;;;; inventory 
;;;; --> dynamic inventory of all ingredients on hand
;;;; --> hash table for now

(defparameter *inventory* (make-hash-table))

(defun setup-inventory ()
	(clrhash *inventory*)
	(setf (gethash 'FERMENTABLES *inventory*) (make-hash-table))
	(setf (gethash 'HOPS         *inventory*) (make-hash-table))
	(setf (gethash 'YEAST        *inventory*) (make-hash-table))
	(setf (gethash 'ADJUNCTS     *inventory*) (make-hash-table)))

(defun inventory (type)
  (gethash type *inventory*))

(defun get-from-inventory (i type)
  (gethash i (inventory type)))

(defun load-hop-inventory ()
  (setf (gethash 'lemon-drop (inventory 'HOPS)) 
	  (make-instance 'hop :name "lemon drop" :form "pellet" :alpha-acid 5.4)) ;confirmed
  (setf (gethash 'cascade (inventory 'HOPS)) 
	  (make-instance 'hop :name "cascade" :form "pellet" :alpha-acid 6.4)) ;confirmed
  (setf (gethash 'amarillo (inventory 'HOPS))
    (make-instance 'hop :name "amarillo"   :form "pellet" :alpha-acid  8.5))
  (setf (gethash 'simcoe (inventory 'HOPS))
    (make-instance 'hop :name "simcoe"     :form "pellet" :alpha-acid 13.0))
  (setf (gethash 'willamette (inventory 'HOPS))
    (make-instance 'hop :name "willamette" :form "pellet" :alpha-acid  5.4)) ;confirmed
  (setf (gethash 'EKG (inventory 'HOPS))
    (make-instance 'hop :name "East Kent Golding" :form "pellet" :alpha-acid  4.8)))

(defun load-fermentable-inventory ()
  (setf (gethash 'CRYSTAL-10  (inventory 'FERMENTABLES)) (make-grain 'CRYSTAL-10   9.5))
  (setf (gethash 'FLAKED-OATS (inventory 'FERMENTABLES)) (make-grain 'FLAKED-OATS  9.0)))

(defun load-inventory ()
	(load-hop-inventory)
	(load-fermentable-inventory))