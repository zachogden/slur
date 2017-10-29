;;; Section 4: Hops

; Class definition
(defclass hop (ingredient)
	 ((alpha-acid
	    :initarg :alpha-acid
	    :initform 0
	    :reader alpha-acid
	    :documentation "Alpha acid percentage")))

; Initialization error checking
; Confirms initialization variables are of correct type.
(defmethod initialize-instance :after ((h hop) &key)
	(if (not (numberp (slot-value h 'alpha-acid))) (setf (slot-value h 'alpha-acid) 0)))

(defclass hop-addition ()
	((hop-type
		:initarg :hop-type
		:initform nil
		:reader hop-type
		:documentation "Hop type of addition (from hop shop)")
	(weight
		:initarg :weight
		:initform 0
		:accessor weight
		:documentation "Weight (oz)")
	(at-time
		:initarg :at-time
		:initform 0
		:accessor at-time
		:documentation "Addition time (min)")
	(step-tag
		:initarg :step-tag
		:initform 'BOIL
		:accessor step-tag
		:documentation "Addition step tag (i.e. 'BOIL, 'WHIRLPOOL)")))

(defun make-hop-addition (type weight at-time &optional step-tag) 
	(make-instance 'hop-addition :hop-type type :weight weight :at-time at-time :step-tag step-tag))

; hop-utilization
; 1. Calculate hop utilization
; 2. boil-sg : sg of wort measured pre-boil
;    boil-time    : duration of boil in minutes
; 3. Hop utilization based on empirically derived data from G. Tinseth
(defmacro hop-utilization (boil-sg boil-time)
  `(* (* 1.65 (expt 0.000125 (- ,boil-sg 1))) (/ (- 1 (exp (* -0.04 ,boil-time))) 4.15)))

; IBU
; 1. Calculate IBU contribution of hop addition
; 2. aau           : aau of hop addition
;    boil-sg       : sg of wort measured pre-boil
;    boil-time     : duration of boil in minutes
;    recipe-volume : desired final volume of fermented beer
; 3. IBU = aau * U * 75 / V
(defmacro IBU (aau boil-sg boil-time recipe-volume)
  `(* ,aau (hop-utilization ,boil-sg ,boil-time) (/ 75 ,recipe-volume)))

; recipe-IBU
; 1. Calculate cumulative IBU of all hop additions
; 2. hop-schedule  : list of hop additions of the form (weight | AA % | time), where time is boil length
;    boil-sg  : sg of wort measured pre-boil
;    recipe-volume : desired final volume of fermented beer
(defun recipe-IBU (kettle boil-sg recipe-volume)
  (loop for k in kettle if (typep (first k) 'hop) sum
    (IBU (aau (second k) (alpha-acid (first k))) boil-sg (abs (third k)) recipe-volume)))

