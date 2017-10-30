;;;; slur
;;;; Kettle utility functions

(defun boil-time (r)
  (loop for k in (kettle r) if (and (typep k 'list) (eql (first k) 'BOIL)) return (third k)))

(defun boil-vol (r b) (+ (volume r) (* (boil-loss b) (/ (boil-time r) 60))))

(defun add-kettle-step (r kettle-step &aux s)
  (setf s 
  	(if (typep kettle-step 'hop-addition) 
  		(progn 
  			(if (eql 'WHIRLPOOL (step-tag kettle-step))
  				(setf (at-time kettle-step) (* -1 (at-time kettle-step))) t) 
    		(list (hop-type kettle-step) (weight kettle-step) (at-time kettle-step) (step-tag kettle-step)))
        kettle-step))

  (if (not (kettle r)) (setf (kettle r) (list s))
    (setf (kettle r) (cons s (kettle r))))

  (sort (kettle r) #'> :key #'third))

(defun target-og (r b) (bg-to-og (target-bg r b) (boil-vol r b) (volume r)))

; recipe-eff-adjust
; 1. Scales grain bill to target OG based on given mash efficiency
; 2. bill : mash grain bill
;    vol  : recipe volume
;    OG   : target OG
;    eff  : mash efficiency
(defun scale-to-OG (bill vol OG eff)
  (let ((scale-factor (/ (sg-to-gp OG) (* (max-mash-gp bill vol) eff))))
    (loop for grain in bill collect (list (first grain) (* scale-factor (second grain))))))

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