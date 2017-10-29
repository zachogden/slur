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
    		(list (hop-type kettle-step) (weight kettle-step) (at-time kettle-step)))
        kettle-step))

  (if (not (kettle r)) (setf (kettle r) (list s))
    (setf (kettle r) (cons s (kettle r))))

  (sort (kettle r) #'> :key #'third))

(defun target-bg (r b) (gp-to-sg (house-mash-gp (get-mash-grains r) (boil-vol r b) (mash-eff b))))

(defun target-og (r b) (bg-to-og (target-bg r b) (boil-vol r b) (volume r)))

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