;;; Section 4: Hops

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

; recipe-IBU
; 1. Calculate cumulative IBU of all hop additions
; 2. hop-schedule  : list of hop additions of the form (weight | AA % | time), where time is boil length
;    boil-sg  : sg of wort measured pre-boil
;    recipe-volume : desired final volume of fermented beer
(defun recipe-IBU (hop-schedule boil-sg recipe-volume)
  (loop for hop-addition in hop-schedule sum 
    (IBU (aau (second hop-addition) (third hop-addition)) boil-sg (fifth hop-addition) recipe-volume)))

