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
  (/ (loop for grain in bill sum (* (first (gethash (first grain) *fermentables*)) (second grain))) volume))


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

(defun get-mash-temp (r)
  (loop for mash-step in (mash r) if (and (typep mash-step 'list) (eql (first mash-step) 'MASH))
    return (second mash-step)))

(defun get-mash-grains (r) 
  (loop for g in (mash r) if (typep g 'fermentable) collect (list (name g) (weight g) (srm g))))

(defun sort-mash-grains (r)
  (sort (get-mash-grains r) #'> :key #'second))


; scale-grain-bill-to-efficiency
; 1. Scales grain bill according to (assumed) recipe and house efficiencies
; 2. grain-bill        : see (1)
;    recipe-efficiency : brewhouse efficiency assumed by recipe (typically 75%)
;    house-efficiency  : actual brewhouse efficiency
(defun scale-to-efficiency (grain-bill recipe-efficiency house-efficiency)
  (scale-grain-bill grain-bill (/ recipe-efficiency house-efficiency)))

(defun target-bg (r b) (gp-to-sg (house-mash-gp (get-mash-grains r) (boil-vol r b) (mash-eff b))))

; add-mash-step
; 1. Adds mash step to recipe
; 2. r         : recipe (of same class)
;    b         : brewery (of same class)
;    mash-step : mash step to be added
; 3. Recalculates BG after addition
(defun add-mash-step (r mash-step)
  (if (not (mash r)) (setf (mash r) (list mash-step))
    (setf (mash r) (cons mash-step (mash r)))))

(defun spit-mash (r b &aux (mash-grains (get-mash-grains r)))
  (let* (
    (mash-loss   (grain-water-absorbed (grist-mass mash-grains)))
    (strike-vol  (float-round (strike-water-vol (grist-mass mash-grains) (wgr b)) 2))
    (strike-temp (strike-water-temp (wgr b) (ambient-temp b) (get-mash-temp r)))
    (sparge-vol  (float-round (+ mash-loss (- (boil-vol r b) strike-vol)) 2)))
  
      (format t "Strike ~a gallons of water to ~aF~%~%" strike-vol strike-temp)

      (format t "Dough in the following:~%")
  
      (loop for grain in (sort-mash-grains r) do (format t "~a lbs ~a~%" (second grain) (first grain)))

      (format t "Sparge ~a gal~%" sparge-vol)))


