;;;; slur
;;;; Fermentation and associated classes/functions

(load (merge-pathnames "yeast.lisp" *load-truename*))

;;; Seciton 1: Utility functions

;;; [RESERVED] Section 2: The Fermentation Vessel
;;; [RESERVED] Functionality for partigyle brewing, temp control

;;; Section 3: Fermentation steps

; add-ferm-step
; 1. Adds fermentation step to recipe
; 2. r : recipe
;      : step to be added
(defun add-ferm-step (r ferm-step)
  (if (not (ferm r)) (setf (ferm r) (list ferm-step))
    (setf (ferm r) (cons ferm-step (ferm r)))))

; spit-ferm
; 1. Displays fermentation steps in user-friendly format
; 2. r : recipe
(defun spit-ferm (r)
	(let ((y (first (ferm r))))
		(format t "Ferment in ~a with ~a for ~a days @ ~a F~%" (first y) (second y) (third y) (fourth y))
		(yeast-check (second y) (fourth y) (abv (OG r) (FG r)))))


