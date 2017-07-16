;;;; slur
;;;; Mash utility functions

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

;;; Section 1: Utility functions
;;; -> water volume in quarts
;;; -> grist mass in pounds
;;; -> temperature in degrees F

; grist-mass
; 1. Calculates total mass of given grain bill
; 2. grain-bill : selected grain bill
; 3. (None)
(defun grist-mass (grain-bill) (loop for grain in grain-bill sum (second grain)))


;;; Section 2: All-grain brewing

;; General AG utility functions

; max-mash-gp
; 1. Calculates the therotical maximum sugar extraction from a given grain bill
; 2. bill   : list of lists of the form ((grain_1 pounds_1)...(grain_n pounds_n))
;    volume : pre-boil wort volume
; 3. Returned value of units gallons*points/pound (pt/lbs/gal)
(defun max-mash-gp (bill volume)
  (/ (loop for grain in bill sum (* (max-grain-points (first grain)) (second grain))) volume))


;; Brewhouse utility functions

; house-mash-gp
; 1. Estimates gravity of wort leaving mash and entering boil (i.e. BG)
; 2. bill     : grain bill
;    boil-vol : boil volume / volume of wort drawn from mash
;    eff      : mash efficiency
; 3. Does not take into account dead space, differences in sparge technique, etc
(defmacro house-mash-gp (bill boil-vol eff) `(* ,eff (max-mash-gp ,bill ,boil-vol)))

; mash-efficiency
; 1. Calculates mash efficiency
; 2. bill     : grain bill
;    boil-sg  : boil gravity in SG points
;    boil-vol : boil volume
; 3. (None)
(defmacro mash-efficiency (bill boil-sg boil-vol) `(/ (sg-to-gp ,boil-sg) (max-mash-gp ,bill ,boil-vol)))


;; Strike & sparge helper macros

; strike-water-vol
; 1. Calculates volume of strike water (gallons) required for mash
; 2. grist-mass  : mass of grain bill
;    ratio (opt) : water/grist ratio
; 3. (None)
(defmacro strike-water-vol (grist-mass &optional (ratio 1.5)) `(/ (* ,grist-mass ,ratio) 4))

; strike-water-temp
; 1. Calculates required temperature of strike water
; 2. ratio     : water/grist ratio (qt/lbs)
;    ambient-t : ambient temperature
;    target-t  : target mash temp
; 3. Assumes prewarmed mashtun, grain at ambient temp
(defmacro strike-water-temp (ratio ambient-t target-t) `(+ (* (/ 0.2 ,ratio) (- ,target-t ,ambient-t)) ,target-t))

; returns in gallons
; assumes absorption 0.5 qt/lbs
(defun grain-water-absorbed (grist-mass) (/ (* 0.5 grist-mass) 4))

; infusion-volume
; 1. Calculates volume of mash infusion (i.e. raising mash temp via infusion)
; 2. grist-mass  : mass of grain bill in mhas
;    current-t   : current mash temp
;    infusion-t  : temperature of infused water/liquid
;    mash-vol    : current volume of mash
; 3. (None)
(defmacro infusion-volume (grist-mass current-t target-t infusion-t mash-water-vol) 
  `(/ (* (- ,target-t ,current-t) (+ (* 0.2 ,grist-mass) ,mash-water-vol)) (- ,infusion-t ,target-t)))



  
