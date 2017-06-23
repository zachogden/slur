;;;; slur

;;;; v. 0.1.5 / 22 Jun 2017

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

(load (merge-pathnames "grains.lisp" *load-truename*))
(load (merge-pathnames "unit-conversions.lisp" *load-truename*))
(load (merge-pathnames "mash.lisp" *load-truename*))

; Malt recipe data
; Of the form (name | pounds)

(defparameter saison-malt (list
  (list 'PILSNER 13)
  (list 'WHEAT   1)
  (list 'MUNICH  1)))

; Hops recipe data
; Of the form (ounces | % AA | boil time | [optional] post-boil tag)
; Non-boil tags include FWH, WHIRLPOOL, DRY-HOP-N (where N = number of days)
; Dry hop days are counted from N=0, day wort is transferred to ferm vessel

(defparameter saison-hops (list
  (list 'SAAZ 1.7 4 60)
  (list 'SAAZ 0.75 4 0)))

; Yeast data from respective lab websites
; of the form (attenuation % range | [optional] flocculation | [opt] alc. tolerance range | [opt] temp range)

(defparameter saison-yeast (list 'WLP-028 (list 70 75) 'MEDIUM (list 8 12) (list 65 70)))

; Mash schedule recipe data
; Of the form (step | temperature | time | % of mash)
;
; Examples:
; (SINGLE-INFUSION 152 60 100)
; (SPARGE          160 0  100) 
; for a simple single infusion mash for 60 min at 152F
; 
; (DOUGH-IN        112 15 100)
; (ACID-REST       131 15 100)
; (AMYLASE-REST    149 30 67)
; (DECOCTION       158 20 33)
; (DECOCTION-BOIL  212 10 33)
; (RECOMBINE-DECOC 158 10 100)
; (MASH-OUT        170 0  100)
; (SPARGE          160 0  100)
; For a complex Hefeweizen mash schedule, where the decoction is seperated, rested,
; and then boiled while the remaining mash is independently heated and rested.

; Step times that are "as needed" or not applicable (i.e. mash out) should be listed as 0

(defparameter saison-mash (list
  (list 'SINGLE-INFUSION 147 90 100)
  (list 'MASH-OUT        170 0  100)
  (list 'SPARGE          170 0  100)))

; Fermentation schedule recipe data
; Of the form (step | temperature | [optional] days)
; 
; Example:
; ('PRIMARY-FERMENTATION 62)
; ('DIACETYL-REST        72 2)
; ('COLD-CRASH           32 14)

(defparameter saison-fermentation (list
  (list 'PRIMARY-FERMENTATION 75 14)
  (list 'COLD-CONDITION       40 7)))

; Recipe Data
; Of the form (recipe-volume | grain-bill | hop-schedule | yeast | mash-schedule | fermentation-schedule)
; Where recipe-volume is the desired final volume of beer in gallons

(defparameter *grain-table* (make-hash-table))

(defparameter *brewery* nil)

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

; (Phead+1.013)*(2.71828182845904^(-10.73797+(2617.25/(Tbeer+273.15))))*10
(defun carbonation (head-p beer-t)
  (* 10 (* (+ head-p 1.103) (expt 2.71828182845904 (+ -10.73797 (/ 2617.25 (+ beer-t 273.15)))))))
  

(defun carb-psi-f (head-p beer-t)
  (/ (carbonation (/ head-p 14.5038) (/ (- beer-t 32) 1.8)) 1.96))

(defun abv (og fg)
  (* (* 76.08 (/ (- og fg) (- 1.775 og))) (/ fg 0.794)))

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
(defun make-recipe1 (bill hop-schedule boil-vol ferm-vol &optional (efficiency 0.75))
  (setup-grain-table)
  (let ((bg (house-mash-sg bill boil-vol efficiency)))
    (let ((og (bg-to-og bg boil-vol ferm-vol)))
      (loop for grain in bill do (format t "~a lbs. ~a~%" (second grain) (first grain)))
      (format t "~%")
      (loop for hop in hop-schedule do (format t "~a oz of ~a% hops for ~a min.~%" (first hop) (second hop) (third hop)))
      (format t "~%")
      (format t "BG:    ~a~%" (float-round bg 2))
      (format t "OG:    ~a~%" (float-round og 2))
      (format t "FG:    ~a~%" (float-round (gp-to-sg (* .25 (sg-to-gp og))) 2))
      (format t "ABV:   ~a%~%"  (abv og (gp-to-sg (* .25 (sg-to-gp og)))))
      (format t "IBU:   ~a~%" (round (recipe-IBU hop-schedule bg ferm-vol)))
      (format t "Pitch: ~a~%" (round (ale-pitch-rate og ferm-vol))))))


; build-brewery
; 1. Sets up slur environment
; 2. mash-efficiency : Calculated/observed mash efficiency
;    boil-loss       : wort volume loss during boil (gal/hr)
; 3. Required prior to use of recipe functions
(defun build-brewery (mash-efficiency boil-loss)
  (setf *brewery* (list mash-efficiency boil-loss))
  (setup-grain-table))

(defmacro build-recipe (recipe-name recipe-volume grain-bill hop-schedule yeast mash-schedule fermentation-schedule)
  `(list ,recipe-name ,recipe-volume ,grain-bill ,hop-schedule ,yeast ,mash-schedule ,fermentation-schedule))

(defun spit-recipe (recipe)
  (format t "~a (~a gallons)~%~%" (first recipe) (second recipe))
  (format t "Grain bill:~%")
  (loop for grain in (third recipe) do (format t "~albs ~a~%" (second grain) (first grain)))
  (format t "~%~%Mash schedule:~%")
  (loop for mash-step in (sixth recipe) do 
    (format t "~a ~a% of mash for ~a minutes at ~aF~%" 
      (first mash-step) (fourth mash-step) (third mash-step) (second mash-step)))
  (format t "~%Hop schedule:~%")
  (loop for hop in (fourth recipe) do 
    (format t "~aoz of ~a AAU ~a hops at ~a minutes~%" (second hop) (third hop) (first hop) (fourth hop)))
  (format t "~%Ferment with ~a yeast as follows:~%" (first (fifth recipe)))
  (loop for ferm-step in (seventh recipe) do
    (format t "~a at ~aF for ~a days~%" (first ferm-step) (second ferm-step) (third ferm-step))))

