;;;; slur
;;;; Malt data

;;;; Conventions:
;;;; Specific gravity shortened to "sg," gravity points to "gp"
;;;; All measurements of volume should be prefixed w/ boil-, ferm-, etc
;;;; sg gp are of the form XX, specific sg 1.0XX
;;;; Yeast cell counts assumed to be given as billions of cells

;;; Section 1: Grain and bill utility functions

;;; Maximum ppg (gp/pound/gallon) of various malted grains

(defun get-grain-data ()
  (load-slur-data "/home/zach/code/slur/grain-data.slur" 'GRAIN))

; setup-grain-table
; 1. Initializes hash table containing malt data
; 2. (None)
; 3. (None)
(defun setup-grain-table ()
  (let ((grain-data (get-grain-data)))
    (clrhash *grain-table*)
    (loop for grain in grain-data do 
      (setf (gethash (first grain) *grain-table*) (rest grain)))))

; max-grain-points
; 1. Returns theoretical maximum gravity point contribution for a given malt
; 2. grain : selected grain
; 3. (None)
(defmacro max-grain-points (grain) 
  `(first (gethash ,grain *grain-table*)))

; grain-SRM
; 1. Returns SRM for given grain
; 2. grain : selected grain
; 3. (None)
(defmacro grain-SRM (grain) 
  `(second (gethash ,grain *grain-table*)))

; grain-bill-percentage
; 1. Returns grain bill of the form (GRAIN-NAME PERCENTAGE-OF-BILL)
; 2. grain-bill : grain bill to be considered
; 3. (None)
(defun grain-bill-percentage (grain-bill)
  (let ((grains (loop for grain in grain-bill collect (second grain))))
    (loop for g in (loop for grain in grain-bill collect (second grain)) collect 
      (* 0.01 (round (/ g (apply '+ grains)) 0.01)))))

; scale-grain-bill
; 1. Scales grain bill by scale-factor
; 2. grain-bill   : see (1)
;    scale-factor : see (1)
; 3. (None)
(defun scale-grain-bill (grain-bill scale-factor)
  (loop for grain in grain-bill collect 
    (list (first grain) (* scale-factor (second grain)))))

; scale-grain-bill-to-efficiency
; 1. Scales grain bill according to (assumed) recipe and house efficiencies
; 2. grain-bill        : see (1)
;    recipe-efficiency : brewhouse efficiency assumed by recipe (typically 75%)
;    house-efficiency  : actual brewhouse efficiency
(defun scale-grain-bill-to-efficiency (grain-bill recipe-eff house-eff)
  (scale-grain-bill grain-bill (/ recipe-eff house-eff)))

