;;;; slur
;;;; Mash and boil utility functions

;;; Section 1: Utility functions
;;; -> water volume in quarts
;;; -> grist mass in pounds
;;; -> temperature in degrees F

; max-mash-gp
; 1. Calculates the therotical maximum sugar extraction from a given grain bill
; 2. bill   : list of lists of the form ((grain_1 pounds_1)...(grain_n pounds_n))
;    volume : pre-boil wort volume
; 3. Returned value of units gallons*points/pound (pt/lbs/gal)
(defun max-mash-gp (bill volume)
  (/ (loop for grain in bill sum (* (first (gethash (first grain) *fermentables*)) (second grain))) volume))


(defun get-mash-temp (r)
  (loop for mash-step in (mash r) if (and (typep mash-step 'list) (eql (first mash-step) 'MASH))
    return (second mash-step)))

(defun get-mash-grains (r) 
  (loop for g in (mash r) if (typep g 'fermentable) collect (list (name g) (weight g) (srm g))))

(defun sort-mash-grains (r)
  (sort (get-mash-grains r) #'> :key #'second))

(defun mash-type (mash-sched)
  (loop for m in mash-sched if (or
    (eql (first m) 'BATCH-SPARGE)
    (eql (first m) 'FLY-SPARGE)
    (eql (first m) 'NO-SPARGE))
  return (first m)))

;;;; Kettle utility functions

(defun boil-time (r)
  (loop for k in (kettle r) if (and (typep k 'list) (eql (first k) 'BOIL)) return (third k)))

(defun boil-vol (r b) (+ (volume r) (* (boil-loss b) (/ (boil-time r) 60))))

(defun target-bg (r b) (gp-to-sg (house-mash-gp (get-mash-grains r) (boil-vol r b) (mash-eff b))))

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

; add-mash-step
; 1. Adds mash step to recipe
; 2. r         : recipe (of same class)
;    b         : brewery (of same class)
;    mash-step : mash step to be added
; 3. Recalculates BG after addition
(defun add-mash-step (r mash-step)
  (if (not (mash r)) (setf (mash r) (list mash-step))
    (setf (mash r) (cons mash-step (mash r)))))


; scale-grain-bill-to-efficiency
; 1. Scales grain bill according to (assumed) recipe and house efficiencies
; 2. grain-bill        : see (1)
;    recipe-efficiency : brewhouse efficiency assumed by recipe (typically 75%)
;    house-efficiency  : actual brewhouse efficiency
(defun scale-to-efficiency (grain-bill recipe-efficiency house-efficiency)
  (scale-grain-bill grain-bill (/ recipe-efficiency house-efficiency)))


(defun no-sparge-check (r b)
  (let ((vol (+ (* (wgr b) (grist-mass (get-mash-grains r))) (grist-volume (grist-mass (get-mash-grains r))))))
    (if (> vol (mashtun-vol b)) (format t "~%***Mash volume exceeds mashtun capacity***~%"))
    vol))


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

