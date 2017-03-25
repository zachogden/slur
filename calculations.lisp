;;;; slur
;;;; Utility macros and functions

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



;;; Section 1: Basic unit conversions

; Maximum ppg (gp/pound/gallon) of various malted grains
; Data from Palmer's "How to Brew" 3rd ed., Table 27
(defparameter *malt-data* (list
  (list '2-ROW-LAGER    37)
  (list '6-ROW          35)
  (list '2-ROW-PALE-ALE 38)
  (list 'BISCUIT        35)
  (list 'VICTORY        35)
  (list 'VIENNA         35)
  (list 'MUNICH         35)
  (list 'BROWN          32)
  (list 'CARAPILS       32)
  (list 'CRYSTAL-10-15  35)
  (list 'CRYSTAL-25-40  34)
  (list 'CRYSTAL-60-75  34)
  (list 'CRYSTAL-120    33)
  (list 'SPECIAL-B      31)
  (list 'CHOCOLATE      28)
  (list 'ROSTMALZ       32)
  (list 'ROASTED-BARLEY 25)
  (list 'BLACK-PATENT   25)
  (list 'WHEAT          37)
  (list 'RYE            29)
  (list 'FLAKED-OATS    32)
  (list 'FLAKED-CORN    39)
  (list 'FLAKED-BARLEY  32)
  (list 'FLAKED-WHEAT   36)
  (list 'FLAKED-RICE    38)
  (list 'MALTODEXTRIN   40)
  (list 'CORN-SUGAR     42)
  (list 'CANE-SUGAR     46)))

(defparameter *malt-table* (make-hash-table))


(defmacro gp-to-sg (gp) `(+ 1 (/ ,gp 1000)))

(defmacro sg-to-gp (sg) `(* 1000 (- ,sg 1)))

(defmacro oz-to-tbsp (oz) `(* 2 ,oz))

(defmacro oz-to-cup (oz) `(/ ,oz 8))

(defmacro aau (weight AA-percent) `(* ,weight ,AA-percent))

(defmacro sg-round (sg) `(/ (round (* ,sg 1000)) 1000.0))



;;; Section 2: Cleaners/Sanitizers
;;; -> "gal" assumed to be gallons of solution desired
;;; -> returned units in U.S. fl. oz. unless otherwise specified      

; PBW: Recommended mixture is 1-2 oz/gal, returns midrange (1.5 oz/gal)
(defmacro pbw (gal) `(* 1.5 ,gal))

; StarSan: Recommendated mixture is 1 oz / 5 gallons
(defun starsan (gal) `(* 0.2 ,gal))



;;; Section 3: Mash & Extract

;; Mash
;; -> water volume in quarts, grist mass in pounds, temperature in degrees F

; setup-malt-table
; 1. Initializes hash table containing malt data
; 2. (None)
(defun setup-malt-table ()
  (clrhash *malt-table*)
  (loop for malt in *malt-data* do (setf (gethash (first malt) *malt-table*) (second malt))))

(defmacro malt-yield (malt) `(gethash ,malt *malt-table*))

; max-mash-gp
; 1. Calculates the therotical maximum sugar extraction from a given grain bill
; 2. bill   : list of lists of the form ((grain_1 pounds_1)...(grain_n pounds_n))
;    volume : pre-boil wort volume
; 3. Returned value of units gallons*points/pound (pt/lbs/gal)
(defun max-mash-gp (bill volume)
  (/ (loop for grain in bill sum (* (malt-yield (first grain)) (second grain))) volume))

(defmacro max-mash-sg (bill volume) `(gp-to-sg (max-mash-gp ,bill ,volume)))

; Brewhouse helper macros

(defmacro house-mash-gp (bill boil-vol eff) `(* ,eff (max-mash-gp ,bill ,boil-vol)))

(defmacro house-mash-sg (bill boil-vol eff) `(gp-to-sg (brewhouse-mash-gp ,bill ,boil-vol ,eff)))

(defmacro house-efficiency (bill boil-sg boil-vol) `(/ (sg-to-gp ,boil-sg) (max-mash-gp ,bill ,boil-vol)))

; Strike & sparge helper macros

(defmacro strike-water-vol (grist-mass &optional (ratio 1.5)) `(* ,grist-mass ,ratio))

(defmacro strike-water-temp (ratio ambient-t target-t) `(+ (* (/ 0.2 ,ratio) (- ,target-t ,ambient-t)) ,target-t))

(defmacro infusion-volume (grist-mass current-t target-t infusion-t mash-water-vol) 
  `(/ (* (- ,target-t ,current-t) (+ (* 0.2 ,grist-mass) ,mash-water-vol)) (- ,infusion-t ,target-t)))


;; Extract
;; -> "gal" assumed to be boil/wort volume in gallons
;; -> sg (yield) is specific sg of extract in gp/lb/gal

(defmacro extract-get-gp (sg pounds gal) `(/ (* ,sg ,pounds) ,gal))

(defmacro extract-get-pounds (gp sg gal) `(* ,gal (/ ,gp ,sg)))


;;; Section 4: Boil & Hops

;; Boil

(defmacro bg-to-og (bg boil-vol ferm-vol) `(gp-to-sg (/ (* (sg-to-gp ,bg) ,boil-vol) ,ferm-vol)))


;; Hops

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



;;; Section 5: Yeast

(defmacro apparent-attenuation (OG FG) `(/ (- ,OG ,FG) (- ,OG 1)))

; pitch-rate-cells
; 1. Calculates pitch rate (in billions of yeast cells) for a given wort
; 2. wort-sg : OG of wort
;    gallons      : gallons of wort
; 3. Interpolated from data in Palmer's "How to Brew," 3rd ed., page 68
(defun ale-pitch-rate (wort-sg gallons)
  (if (< wort-sg 1.05) 
    (* 20 gallons)
    (* (+ (* 12 (/ (mod (sg-to-gp wort-sg) 50) 10)) 16) gallons)))

; Interpolated from data in Palmer, 3rd ed.
(defmacro lager-pitch-rate (wort-sg gallons)
  `(ale-pitch-rate (+ ,wort-sg 0.01) ,gallons))


; starter-growth-rate
; Calculate the growth rate () given an inoculation in billions of cells/liter
; 2. inoculation : initial inoculation of starter
; 3. based on empirical data from Chris White and interpretation by Brewer's Friend
(defun starter-growth-rate (inoculation)
  (- (* 12.54793776 (expt inoculation -0.4594858324)) 0.9994994906))



;;; Section 6: Fermentation



;;; Section 7: Kegging & Bottling

;; Kegging


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



;;; Section 8: Recipe Building
;;; -> recipes of the form ((grain bill) (hop schedule) volume)

(defun recipe-eff-adjustment (bill volume target-OG eff)
  (let ((scale-factor (/ (sg-to-gp target-OG) (* (max-mash-gp bill volume) eff))))
    (loop for grain in bill collect (list (first grain) (* scale-factor (second grain))))))

; recipe-IBU
; 1. Calculate cumulative IBU of all hop additions
; 2. hop-schedule  : list of hop additions of the form (weight | AA % | time), where time is boil length
;    boil-sg  : sg of wort measured pre-boil
;    recipe-volume : desired final volume of fermented beer
(defun recipe-IBU (hop-schedule boil-sg recipe-volume)
  (loop for hop-addition in hop-schedule sum 
    (IBU (aau (first hop-addition) (second hop-addition)) boil-sg (third hop-addition) recipe-volume)))

; make-recipe
; 1. Display standard measurements (OG, IBU, etc) given recipe
; 2. bill   : list of lists of the form ((grain_1 pounds_1)...(grain_n pounds_n))
;    hop-schedule : list of lists of the form ((ounces_1 aa%_1 boil-time_1)...) where AA% expressed as integer
;    boil-vol     : desired volume of boil (i.e. total amount sparged from mash)
;    ferm-vol     : desired post-volume volume (i.e. volume of wort going into fermenter)
;    efficiency   : brewhouse efficiency, defaulted to typical 75%, expressed as float
(defun make-recipe (bill hop-schedule boil-vol ferm-vol &optional (efficiency 0.75))
  (setup-malt-table)
  (let ((bg (brewhouse-mash-sg bill boil-vol efficiency)))
    (let ((og (bg-to-og bg boil-vol ferm-vol)))
      (loop for grain in bill do (format t "~a lbs. ~a~%" (second grain) (first grain)))
      (format t "~%")
      (loop for hop in hop-schedule do (format t "~a oz of ~a% hops for ~a min.~%" (first hop) (second hop) (third hop)))
      (format t "~%")
      (format t "BG:    ~a~%" (sg-round bg))
      (format t "OG:    ~a~%" (sg-round og))
      (format t "IBU:   ~a~%" (round (recipe-IBU hop-schedule bg ferm-vol)))
      (format t "Pitch: ~a~%" (round (ale-pitch-rate og ferm-vol))))))


