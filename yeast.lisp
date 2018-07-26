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

(defun yeast-check (strain temp abv)
  (loop for yeast in (load-slur-data "/home/zach/code/slur/yeast-data.slur" 'YEAST) do
    (when (eql (first yeast) strain)
      (let ((strain-temp-range (sixth yeast))
            (strain-abv-limit  (second (fifth yeast))))
        (progn
          (if (or (< temp (first strain-temp-range)) (> temp (second strain-temp-range)))
            (format t "***Warning: Fermentation temperature outside of yeast range (~a-~a)***~%"
              (first strain-temp-range) (second strain-temp-range)))
          (if (> abv strain-abv-limit)
            (format t "***Warning: ABV exceeds yeast tolerance (~a)***~%" strain-abv-limit)))))))




