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
;;;; Gravity points are of the form XX, specific gravity 1.0XX
;;;; Yeast cell counts assumed to be given as billions of cells



;;; Section 1: Basic unit conversions


(defmacro points-to-gravity (points) `(+ 1 (/ ,points 1000)))

(defmacro gravity-to-points (gravity) `(* 1000 (- ,gravity 1)))

(defmacro oz-to-tbsp (oz) `(* 2 ,oz))

(defmacro oz-to-cup (oz) `(/ ,oz 8))

(defmacro AAU (weight AA-percent) `(* ,weight ,AA-percent))



;;; Section 2: Cleaners/Sanitizers
;;; -> "gal" assumed to be gallons of solution desired
;;; -> returned units in U.S. fl. oz. unless otherwise specified      

; PBW: Recommended mixture is 1-2 oz/gal, returns midrange (1.5 oz/gal)
(defmacro PBW (gal) `(* 1.5 ,gal))

; StarSan: Recommendated mixture is 1 oz / 5 gallons
(defun starsan (gal) `(* 0.2 ,gal))



;;; Section 3: Mash & Extract

;; Mash


;; Extract
;; -> "gal" assumed to be boil/wort volume in gallons
;; -> SG (yield) is specific gravity of extract in points/lb/gal

(defmacro extract-get-points (SG pounds gal) `(/ (* ,SG ,pounds) ,gal))

(defmacro extract-get-pounds (points SG gal) `(* ,gal (/ ,points ,SG)))


;;; Section 4: Boil & Hops

;; Boil


;; Hops

; hop-utilization
; 1. Calculate hop utilization
; 2. boil-gravity : SG of wort measured pre-boil
;    boil-time    : duration of boil in minutes
; 3. Hop utilization based on empirically derived data from G. Tinseth
(defun hop-utilization (boil-gravity boil-time)
  (*
    (* 1.65 (expt 0.000125 (- boil-gravity 1)))
    (/ (- 1 (exp (* -0.04 boil-time)))4.15)))

; IBU
; 1. Calculate IBU of wort
; 2. AAU           : cumulative AAU of hop additions
;    boil-gravity  : SG of wort measured pre-boil
;    boil-time     : duration of boil in minutes
;    recipe-volume : desired final volume of fermented beer
; 3. IBU = AAU * U * 75 / V
(defun IBU (AAU boil-gravity boil-time recipe-volume)
  (* AAU (hop-utilization boil-gravity boil-time) (/ 75 recipe-volume)))

; recipe-IBU
; 1. Calculate cumulative IBU of all hop additions
; 2. hop-schedule  : list of hop additions of the form (weight | AA % | time), where time is boil length
;    boil-gravity  : SG of wort measured pre-boil
;    recipe-volume : desired final volume of fermented beer
(defun recipe-IBU (hop-schedule boil-gravity recipe-volume)
  (loop for hop-addition in hop-schedule sum 
    (IBU (AAU (first hop-addition) (second hop-addition)) boil-gravity (third hop-addition) recipe-volume)))



;;; Section 5: Yeast

(defun apparent-attenuation (OG FG)
  (/ (- OG FG) (- OG 1)))

; pitch-rate-cells
; 1. Calculates pitch rate (in billions of yeast cells) for a given wort
; 2. wort-gravity : OG of wort
;    gallons      : gallons of wort
; 3. Interpolated from data in Palmer's "How to Brew," 3rd ed., page 68
(defun ale-pitch-rate (wort-gravity gallons)
  (* (+ (* 12 (/ (mod (gravity-to-points wort-gravity) 50) 10)) 16) gallons))

(defun lager-pitch-rate (wort-gravity gallons)
  (ale-pitch-rate (+ wort-gravity 0.01) gallons))

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


