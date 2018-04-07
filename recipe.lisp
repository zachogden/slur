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



;;; Section 2: Recipe building
(defun add-step (r p s)
  (cond
    ((eql p 'MASH)   (add-mash-step   r s))
    ((eql p 'KETTLE) (add-kettle-step r s))
    ((eql p 'FERM)   (add-ferm-step   r s))
    (t (format t "Not a valid process~%"))))

(defun spit-mash (r b &aux (mash-grains (get-mash-grains r)))
  (let* (
    (mash-loss   (grain-water-absorbed (grist-mass mash-grains)))
    (strike-temp (strike-water-temp (wgr b) (ambient-temp b) (get-mash-temp r)))

    (strike-vol  (if (eql (mash-type (mash r)) 'NO-SPARGE)
                  (no-sparge-check r b) 
                  (float-round (strike-water-vol (grist-mass mash-grains) (wgr b)) 2)))
    (sparge-vol  (float-round (+ mash-loss (- (boil-vol r b) strike-vol)) 2)))
  
      (format t "Strike ~a gallons of water to ~aF~%~%" strike-vol strike-temp)

      (format t "Dough in the following:~%")
  
      (loop for grain in (sort-mash-grains r) do (format t "~a lbs ~a~%" (second grain) (first grain)))

      (format t "Sparge ~a gal~%" sparge-vol)))

(defun spit-kettle (r)
  (loop for k in (kettle r)
      collecting (third k) into time-table do
      (if (not (eql (car (reverse time-table)) (cadr (reverse time-table))))
        (if (minusp (third k)) 
          (format t "~%***WHIRLPOOL (~a MIN)***~%" (abs (third k)))
          (format t "~%***~a MINUTES***~%" (third k))))

      (if (typep (first k) 'hop) 
        (format t "~a oz ~a~%" (second k) (name (first k)))
        (format t "~a~%" (first k)))))

; build
; 1. Calculates descriptors for recipe
; 2. r : recipe
;    b : brewery
(defun build (r b)
  (setf (BG r) (target-bg r b))
  (setf (OG r) (target-og r b))
  (setf (FG r) (gp-to-sg (* 0.25 (sg-to-gp (OG r)))))

  ; Morey SRM
  (setf (SRM r) 
    (* 1.4922 (expt (/ (loop for g in (get-mash-grains r) sum (* (second g) (third g))) (volume r)) 0.6859))))

; spit-recipe
; 1. Outputs recipe data in human-readable format
; 2. b : brewery
;    r : recipe
; 3. N/A
(defun spit-recipe (r b)
  (build r b)

  (format t "~a (~a), ~a gal~%~%" (name r) (name b) (volume r))

  (spit-mash r b)
  (format t "~%For a target boil vol of ~a gal and BG ~a~%" (boil-vol r b) (float-round (BG r) 3))

  (spit-kettle r)

  (format t "~%For a target OG of ~a~%~%" (float-round (OG r) 3))

  (spit-ferm r) 

  (format t "~%Recipe Overview:~%OG:  ~a~%FG:  ~a~%ABV: ~a%~%IBU: ~a~%SRM: ~a~%"
    (float-round (OG r) 3)
    (float-round (FG r) 3) 
    (float-round (abv (OG r) (FG r)) 1) 
    (round (recipe-IBU (kettle r) (BG r) (volume r)))
    (float-round (SRM r) 1)))

