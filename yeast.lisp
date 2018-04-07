;;;; slur
;;;; Yeast and associated classes/functions

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

(defparameter *yeast-db* (make-hash-table))

; Yeast data from respective lab websites
; of the form (description | attenuation % range | flocculation | alc. tolerance range | temp range)
(defparameter *yeast-data* (list
  (list 'WLP-001 'CALIFORNIA-ALE         (list 73 80) 'MEDIUM (list 10 15) (list 68 73))
  (list 'WLP-013 'LONDON-ALE             (list 67 75) 'MEDIUM (list  5 10) (list 66 71))
  (list 'WLP-028 'EDINBURGH-SCOTTISH-ALE (list 70 75) 'MEDIUM (list  8 12) (list 65 70))
  (list 'WLP-029 'GERMAN-ALE-KOLSCH      (list 72 78) 'MEDIUM (list  5 10) (list 65 69))
  (list 'WLP-565 'BELGIAN-SAISON-I       (list 65 75) 'MEDIUM (list  5 10) (list 68 75))))

; setup-yeast-table
; 1. Initializes yeast lookup table
; 2. N/A
(defun setup-yeast-table ()
  (clrhash *yeast-db*)
  (loop for y in *yeast-data* do 
    (setf (gethash (first y) *yeast-db*) (rest y))))

; yeast-check
; 1. Confirms environment & wort within published yeast bounds
; 2. strain : yeast strain (see yeast-bank)
;    temp   : fermentation temp
;    abv    : estimated ABV of beer
(defun yeast-check (strain temp abv)
  (let ((yeast (gethash strain *yeast-db*)))
    (if yeast
      (let ((temp-range (fifth yeast)) (abv-limit  (second (fourth yeast))))
        (if (or (< temp (first temp-range)) (> temp (second temp-range)))
          (format t "***Warning: Fermentation temperature outside of yeast range (~a-~a)***~%"
            (first temp-range) (second temp-range)))
        (if (> abv abv-limit)
          (format t "***Warning: ABV exceeds yeast tolerance (~a)***~%" abv-limit)))
    (format t "Yeast strain not known."))))





