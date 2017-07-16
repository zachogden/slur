;;;; slur
;;;; Recipe data

;;;; Conventions:
;;;; -> functions documented in the form
;;;; -> function-name
;;;; -> 1. basic description
;;;; -> 2. argument(s)
;;;; -> 3. notes/assumptions, if any
;;;; -> macros should be self explanatory enough to not require this 
;;;;
;;;; Specific gravity shortened to "sg," gravity points to "gp"
;;;; All measurements of volume should be prefixed w/ boil-, ferm-, etc
;;;; sg gp are of the form XX, specific sg 1.0XX
;;;; Yeast cell counts assumed to be given as billions of cells

;;; Section 0: Data structures

; A "recipe" is a hash table w/ 8 keys :
; 1. HEADER    : (name | volume | water/grist ratio)
; 2. WATER     : RESERVED
; 3. GRAIN     : fermentables (as defined below) used in recipe
; 4. HOP       : hops (as defined below) used in recipe
; 5. MASH      : mash steps (all brewing steps up to kettle)
; 6. KETTLE    : kettle steps (all brewing steps from kettle to FV) 
; 7. FERMENT   : fermentation steps
; 8. PACKAGING : RESERVED
;
; The hash table should encapsulate all information needed to make a given beer

;;; Section 1: Recipe file I/O 

;; RESERVED

;;; Section 2: Recipe building

;; Whole recipe tools

; recipe-eff-adjust
; 1. Scales grain bill to target OG based on given mash efficiency
; 2. bill : mash grain bill
;    vol  : recipe volume
;    OG   : target OG
;    eff  : mash efficiency
(defun recipe-eff-adjust (bill vol OG eff)
  (let ((scale-factor (/ (sg-to-gp OG) (* (max-mash-gp bill vol) eff))))
    (loop for grain in bill collect (list (first grain) (* scale-factor (second grain))))))

; add-to-recipe
; 1. Adds ingredient to recipe hash table
; 2. type       : See list of seven hash table keys above
;    ingredient : List of one of 7 types. Each defined below
; 3. Ingredient, though a list, must be enclosed in another list to add to recipe table
(defun add-ingredient (recipe type ingredient &aux (i (list ingredient)))
  (if (not (gethash type recipe)) (setf (gethash type recipe) i)
    (setf (gethash type recipe) (append (gethash type recipe) i))))

; dump-recipe
; 1. Dumps raw contents of recipe hash table
; 2. (None)
; 3. For debug use
(defun dump-recipe (recipe)
  (loop for v being the hash-values in recipe using (hash-key k) do
    (format t "***~a***~%~a~%~%" k v)))

; clear-recipe
; 1. Clear recipe hash table
; 2. (None)
(defmacro clear-recipe () `(clrhash ,*recipe*))

;; Ingredients tools 
;; --> Many macros are extremely simple. Used for clarity in later code.

; grain
; 1. Creates a single grain
; 2. name      : slur tag of grain (i.e. '2-ROW)
;    weight    : weight in pounds of grain
;    step      : indicates where grain/extract is added (MASH, STEEP, BOIL, FV) 
; 3. For readability, any fermentable/extract is called a "grain"
;    This is mostly so I don't have to type fermentable hundreds of times 
(defmacro grain (name weight step) `(list ,name ,weight ,step))

; hops
; 1. Creates a single hop addition
; 2. name  : slur tag of hop (i.e. 'CITRA)
;    oz    : Weight in oz of addition
;    aa    : Alpha acid content of hop (XX%)
;    type  : Usage tag (FWH, BOIL, WHIRLPOOL, DRY-HOP)
;    time  : How long hop is in wort (boil time, dry hop time)          
; 3. Minutes for BOIL, WHIRLPOOL. Days for DRY-HOP. nil for FWH.
;    Dry hop days are counted from N=0, day wort is transferred to ferm vessel
;    This time scheme follows current recipe conventions, may prevent errors in recipes
(defmacro hop (name oz aa type time) `(list ,name ,oz ,aa ,type ,time))

; mash-step
; 1. Creates a single mash step
; 2. name    : name of mash step (e.g. DOUGH-IN, DECOCTION, SPARGE)
;    temp    : temperature (F) at which step is performed, or target temp if infusion/decoction
;    time    : duration of step
;    percent : percent of total mash vol on which step is performed
; 3. Names currently nonstandard. No error checking if portion of mash is "dropped" by user
(defmacro mash-step (name temp time percent) `(list ,name ,temp ,time ,percent))

; kettle-step
; 1. Creates a single kettle step
; 2. name  : name of kettle step (e.g. KETTLE-SOUR, BOIL, IRISH-MOSS)
;    temp  : temperature at which step is performed
;    time  : duration (minutes) of step
;    tag   : step-specific tag (e.g. WLP-603 for kettle sour strain, 0.5 for half tablet of whirlfloc)
; 3. Tag is nil if not used
;    Addition of hops, extract are in respective lists w/ BOIL tag
(defmacro kettle-step (name temp time &optional tag) `(list ,name ,temp ,time ,tag))

; make-kettle
; 1. Create kettle structure (everything between mashtun and primary fermentation vessel)
; 2. grains : grain additions (if any) to kettle
;    hops   : hop schedule
; 3. TODO: Slaps all hops into kettle. Need to screen.
(defun make-kettle (grains hops additions)
  (sort (append 
    (if grains (loop for grain in grains collect 
      (list (fourth grain) (first grain) (second grain))))

    (loop for hop in hops collect (list (fifth hop) (first hop) (* (second hop) (third hop))))
   
    (loop for addition in additions collect (list (third addition) (first addition)))) 
  #'> :key #'car))

; ferm-step
; 1. Creates a single fermentation step
; 2. name   : name of ferm step (e.g. PRIMARY-FERMENTATION, COLD-CRASH)
;    temp   : temperature at which step is performed
;    time   : duration (days) of step
;    strain : yeast/whatever strain used for step (nil if none)
;    tag    : step-specific tag (i.e. ferm sour once SG of 1.01 reached, tag = 1.01)
; 3. Only standard ferm step tag currently PRIMARY-FERMENTATION
(defmacro ferm-step (name temp time strain &optional tag) `(list ,name ,temp ,time ,strain ,tag))








