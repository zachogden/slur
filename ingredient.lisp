;;;; slur
;;;; ingredient superclass

(defclass ingredient ()
	((name
	    :initarg :name
	    :initform (error "Ingredient name required")
		:reader name
		:documentation "Ingredient variety/strain/common name")
	 (form
	    :initarg :form
	 	:reader form
	 	:documentation "Ingredient form (e.g. pellet for hops, liquid for yeast etc")
	 (weight
		:initarg :weight
    	:initform 0
		:accessor weight
		:documentation "Weight/amount of ingredient")))

(defgeneric add-ingredient (recipe i &rest r))