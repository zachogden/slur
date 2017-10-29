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
    :accessor water
    :documentation "Water profile [RESERVED]")
  (mash
    :initform nil
    :accessor mash
    :documentation "Mash steps")
  (kettle
    :initform nil
    :accessor kettle
    :documentation "Kettle steps")
  (ferm
    :accessor ferm
    :documentation "Fermentation steps")
  (packaging
    :accessor pack
    :documentation "Packaging (kegging/bottling) [RESERVED]")))

;;; Section 1: Recipe file I/O 

;; RESERVED

;;; Section 2: Recipe building

; spit-recipe
; 1. Outputs recipe data in human-readable format
; 2. b : brewery
;    r : recipe
; 3. N/A
(defun spit-recipe (r b &aux (bg (target-bg r b)) (og (target-og r b)) )
  (format t "~a (~a), ~a gal~%~%" (name r) (name b) (volume r))

  (spit-mash r b)
  (format t "~%For a target boil vol of ~a gal and BG ~a~%" (boil-vol r b) (float-round bg 3))

  (spit-kettle r)

  (format t "~%For a target OG of ~a~%~%" (float-round og 3)) 

  (format t "~%Recipe Overview:~%OG:  ~a~%FG:  ~a~%ABV: ~a%~%IBU: ~a~%"
    (float-round og 3)
    (float-round (gp-to-sg (* 0.25 (sg-to-gp og))) 3) 
    (float-round (abv (target-og r b) (gp-to-sg (* 0.25 (sg-to-gp (target-og r b))))) 1) 
    (round (recipe-IBU (kettle r) (target-bg r b) (volume r)))))

