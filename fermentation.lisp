;;;; slur
;;;; Fermentation and associated classes/functions

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

(load (merge-pathnames "yeast.lisp" *load-truename*))

;;; Seciton 1: Utility functions

; apparent-attenuation
; 1. Calculates the apparent attenuation of beer
; 2. OG : original gravity of wort
;    FG : final gravity of beer
(defmacro apparent-attenuation (OG FG) `(/ (- ,OG ,FG) (- ,OG 1)))

; ale-pitch
; 1. Calculates pitch size (in billions of yeast cells) for an ale
; 2. OG  : OG of wort
;    vol : gallons of wort
; 3. Interpolated from data in Palmer's "How to Brew," 3rd ed., page 68
(defun ale-pitch (OG vol)
  (if (< OG 1.05) 
    (* 20 vol)
    (* (+ (* 12 (/ (mod (sg-to-gp OG) 50) 10)) 16) vol)))

; lager-pitch
; 1. Calculates pitch size (in billions of yeast cells) for a lager
; 2. OG  : original gravity of wort
;    vol : volume of wort (gal)
; 3. Interpolated from data in Palmer, 3rd ed.
(defmacro lager-pitch (OG vol)
  `(ale-pitch (+ ,OG 0.01) ,vol))

;;; [RESERVED] Section 2: The Fermentation Vessel (FV) 
;;; [RESERVED] Functionality for partigyle brewing, cooling, temp control

;;; Section 3: Fermentation steps

; spit-ferm
; 1. Displays fermentation steps in user-friendly format
; 2. r : recipe
(defun spit-ferm (r)
	(let ((y (first (ferm r))))
		(format t "Ferment in ~a with ~a for ~a days @ ~a F~%" (first y) (second y) (third y) (fourth y))
		(yeast-check (second y) (fourth y) (abv (OG r) (FG r)))))

; add-ferm-step
; 1. Adds fermentation step to recipe
; 2. r : recipe
;      : step to be added
(defun add-ferm-step (r ferm-step)
	(if (not (ferm r)) (setf (ferm r) (list ferm-step))
		(setf (ferm r) (cons ferm-step (ferm r)))))

; yeast-check
; 1. Confirms environment & wort within published yeast bounds
; 2. strain : yeast strain (see yeast-bank)
;    temp   : fermentation temp
;    abv    : estimated ABV of beer
(defun yeast-check (strain temp abv)
  (loop for yeast in yeast-bank do
    (when (eql (first yeast) strain)
      (let ((strain-temp-range (sixth yeast))
            (strain-abv-limit  (second (fifth yeast))))
        (progn
          (if (or (< temp (first strain-temp-range)) (> temp (second strain-temp-range)))
            (format t "***Warning: Fermentation temperature outside of yeast range (~a-~a)***~%"
              (first strain-temp-range) (second strain-temp-range)))
          (if (> abv strain-abv-limit)
            (format t "***Warning: ABV exceeds yeast tolerance (~a)***~%" strain-abv-limit)))))))

