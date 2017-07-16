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


;; Section 1: Gravity measurements

; plato-to-sg
(defun plato-to-sg (plato) (+ 1 (/ plato (- 258.6 (* 227.1 (/ plato 258.2))))))

; sg-to-plato
(defun sg-to-plato (sg) (+ (* -1 616.868) (* 1111.14 sg) (* -630.272 (expt sg 2)) (* 135.997 (expt sg 3))))

; gp-to-sg
; 1. Converts gp to sg
; 2. gp : gravity points to be converted
; 3. XX.XX --> 1.XXX
(defmacro gp-to-sg (gp) `(+ 1 (/ ,gp 1000)))

; sg-to-gp
; 1. Converts sg to gp
; 2. sg : specific gravity to be converted
; 3. 1.XXX --> XX.XX
(defmacro sg-to-gp (sg) `(* 1000 (- ,sg 1)))

; bg-to-og
; 1. Converts boil gravity (bg) to original gravity (og)
; 2. bg       : boil gravity
;    boil-vol : boil volume (gal)
;    ferm-vol : fermentor (post-boil) volume
; 3. Does not consider trub/deadspace loss
(defmacro bg-to-og (bg boil-vol ferm-vol) `(gp-to-sg (/ (* (sg-to-gp ,bg) ,boil-vol) ,ferm-vol)))

;; Mass/volume conversions

; oz-to-lbs
; 1. Converts ounces to pounds
; 2. oz : ounces to be converted
; 3. Reciprocal function (lbs-to-oz) defined similarly
(defmacro oz-to-lbs (oz)  `(/ ,oz  16))
(defmacro lbs-to-oz (lbs) `(* ,lbs 16))

; oz-to-tbsp
; 1. Converts ounces to tablespoons
; 2. oz : ounces to be converted
; 3. Reciprocal function (tbsp-to-oz) defined similarly
(defmacro oz-to-tbsp (oz)   `(* ,oz   2))
(defmacro tbsp-to-oz (tbsp) `(/ ,tbsp 2))

; oz-to-cups
; 1. Converts ounces to cups
; 2. oz : ounces to be converted
; 3. Reciprocal function (cups-to-oz) defined similarly
(defmacro oz-to-cups (oz)   `(/ ,oz   8))
(defmacro cups-to-oz (cups) `(* ,cups 8))


;; Section 2: Numeric utility functions

; float-round
; 1. Rounds a given floating point number to "precision" digits past decimal
; 2. n         : number to be rounded
;    precision : amount of digits past decimal to be retained
; 3. Does not truncate, extracts value past decimal (i.e. X.YY-->YY), rounds that value, then concatenates w/ X
(defmacro float-round (n precision) `(/ (round (* ,n (expt 10 ,precision))) (coerce (expt 10 , precision) 'float)))

; aau
; 1. Calculates alpha acid units of given hops
; 2. weight      : weight of hops (in oz) to be considered
; 3. alpha-acids : alpha acid content (in percent) of given hops
(defmacro aau (weight alpha-acids) `(* ,weight ,alpha-acids))


;;; Section 3: Cleaner/Sanitizer utility functions
;;; -> "gal" assumed to be gallons of solution desired
;;; -> returned units in U.S. fl. oz. unless otherwise specified      

; pbw
; 1. Calculates amount of PBW req'd in solution for cleaning
; 2. gal : volume of cleaning solution
; 3. Recommended mixture is 1-2 oz/gal, returns midrange (1.5 oz/gal)
(defmacro pbw (gal) `(* 1.5 ,gal))

; starsan
; 1. Calculates amount of StarSan req'd in solution for sanitizing
; 2. gal : volume of sanitizing solution
; 3. Recommendated mixture is 1 oz / 5 gallons
(defun starsan (gal) `(* 0.2 ,gal))




