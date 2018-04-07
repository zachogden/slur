;;;; slur
;;;; utilities
;;;; --> Utility functions for common calculations (e.g. IBU, pitch rates)

(load (merge-pathnames "unit-conversions.lisp" *load-truename*))

;;; Section 0: Data structures

(defclass recipe ()
  ((name
    :initarg :name
    :initform "Unnamed Recipe"
    :accessor name
    :documentation "Recipe name")
  (volume
    :initarg :volume
    :initform 5
    :accessor volume
    :documentation "Recipe volume (gal)")
  (water
    :initarg :water
    :initform nil
    :accessor water
    :documentation "Water profile [RESERVED]")
  (mash
    :initarg :mash
    :initform nil
    :accessor mash
    :documentation "Mash steps")
  (kettle
    :initarg :kettle
    :initform nil
    :accessor kettle
    :documentation "Kettle steps")
  (ferm
    :initarg :ferm
    :initform nil
    :accessor ferm
    :documentation "Fermentation steps")

  ; recipe descriptor values
  (BG
    :initarg :BG
    :initform nil
    :accessor BG
    :documentation "Recipe BG")
  (OG
    :initarg :OG
    :initform nil
    :accessor OG
    :documentation "Recipe OG")
  (FG
    :initarg :FG
    :initform nil
    :accessor FG
    :documentation "Recipe FG")
  (SRM
    :initarg :SRM
    :initform nil
    :accessor SRM
    :documentation "Recipe SRM")))


;;; Section 4: General Recipe Utilities

; grain-bill-percentage
; 1. Returns grain bill of the form (GRAIN-NAME PERCENTAGE-OF-BILL)
; 2. grain-bill : grain bill to be considered
; 3. (None)
(defun grain-bill-percentage (grain-bill)
  (let ((grains (loop for grain in grain-bill collect (second grain))))
    (loop for grain in grains collect (* 0.01 (round (/ grain (apply '+ grains)) 0.01)))))

; scale-grain-bill
; 1. Scales grain bill by scale-factor
; 2. grain-bill   : see (1)
;    scale-factor : see (1)
; 3. (None)
(defun scale-grain-bill (grain-bill scale-factor)
  (loop for grain in grain-bill collect (list (first grain) (* scale-factor (second grain)))))


; grist-mass : Calculates total mass of given grain bill
(defun grist-mass (grain-bill) (loop for grain in grain-bill sum (second grain)))


;;; Section 2: All-grain brewing

;; General AG utility functions

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
(defmacro mash-efficiency (bill boil-sg boil-vol) `(/ (sg-to-gp ,boil-sg) (max-mash-gp ,bill ,boil-vol)))


;; Strike & sparge helper macros

; strike-water-vol
; 1. Calculates volume of strike water (gallons) required for mash
; 2. grist-mass  : mass of grain bill
;    ratio (opt) : water/grist ratio
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
(defmacro infusion-volume (grist-mass current-t target-t infusion-t mash-water-vol) 
  `(/ (* (- ,target-t ,current-t) (+ (* 0.2 ,grist-mass) ,mash-water-vol)) (- ,infusion-t ,target-t)))

;;; Section 5: Fermentation Processes

; apparent-attenuation
; 1. Calculates the apparent attenuation of beer
; 2. OG : original gravity of wort
;    FG : final gravity of beer
(defmacro apparent-attenuation (OG FG) `(/ (- ,OG ,FG) (- ,OG 1)))

(defun abv (og fg)
  (* (* 76.08 (/ (- og fg) (- 1.775 og))) (/ fg 0.794)))

; ale-pitch & lager-pitch
; 1. Calculates pitch size (in billions of yeast cells) for ale/lager
; 2. OG  : OG of wort
;    vol : gallons of wort
; 3. Interpolated from data in Palmer's "How to Brew," 3rd ed., page 68
(defun ale-pitch (OG vol)
  (if (< OG 1.05) 
    (* 20 vol)
    (* (+ (* 12 (/ (mod (sg-to-gp OG) 50) 10)) 16) vol)))

(defmacro lager-pitch (OG vol)
  `(ale-pitch (+ ,OG 0.01) ,vol))

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

;;; Section 6: Packaging

;; Kegging

; (Phead+1.013)*(2.71828182845904^(-10.73797+(2617.25/(Tbeer+273.15))))*10
(defun carbonation (head-p beer-t)
  (* 10 (* (+ head-p 1.103) (expt 2.71828182845904 (+ -10.73797 (/ 2617.25 (+ beer-t 273.15)))))))
  

(defun carb-psi-f (head-p beer-t)
  (/ (carbonation (/ head-p 14.5038) (/ (- beer-t 32) 1.8)) 1.96))


;; Bottling

; priming-sugar-oz
; 1. Calculate ounces of priming sugar for bottling
; 2. beer-vol   : gallons of beer being bottled
;    sugar-type : either corn (4oz/5gal) or table (3.8oz/5gal)
; 3. ounces of priming sugar
(defun priming-sugar-oz (beer-vol &optional (sugar-type 'corn))
  (if (eql sugar-type 'corn) 
    (* 0.8 beer-vol)
    (* 0.76 beer-vol)))