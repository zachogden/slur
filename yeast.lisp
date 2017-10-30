;;; Section 5: Yeast

; Yeast data from respective lab websites
; of the form (description | attenuation % range | flocculation | alc. tolerance range | temp range)
(defparameter yeast-bank (list
  (list 'WLP-001 'CALIFORNIA-ALE         (list 73 80) 'MEDIUM (list 10 15) (list 68 73))
  (list 'WLP-028 'EDINBURGH-SCOTTISH-ALE (list 70 75) 'MEDIUM (list 8 12)  (list 65 70))
  (list 'WLP-029 'GERMAN-ALE-KOLSCH      (list 72 78) 'MEDIUM (list 5 10)  (list 65 69))
  (list 'WLP-565 'BELGIAN-SAISON-I       (list 65 75) 'MEDIUM (list 5 10)  (list 68 75))))

; The yeast datatype is to be interpreted as an addition (pitch) of a given yeast
; from the yeast bank. This allows for copitching, seperate pitching for kettle souring, etc
(defclass yeast (ingredient)
  ((strain
    :initarg :strain
    :accessor strain
    :documentation "strain from yeast bank")
  (cells
    :initarg :cells
    :initform 0
    :accessor cells
    :documentation "pitch rate (billions of cells)")))


