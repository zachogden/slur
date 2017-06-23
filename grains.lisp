;;;; slur
;;;; Malt data

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



;;; Section 1: Grain and bill utility functions

; grain-bill-percentage
; 1. Returns grain bill of the form (GRAIN-NAME PERCENTAGE-OF-BILL)
; 2. grain-bill : grain bill to be considered
; 3. (None)
(defun grain-bill-percentage (grain-bill)
  (let ((grains (loop for grain in grain-bill collect (second grain))))
    (loop for grain in grains collect (* 0.01 (round (/ grain (apply '+ grains)) 0.01)))))

; scale-grain-bill
; 1. Scales grain bill by scale-factor
; 2. grain-bill   : see (1)
;    scale-factor : see (1)
; 3. (None)
(defun scale-grain-bill (grain-bill scale-factor)
  (loop for grain in grain-bill collect (list (first grain) (* scale-factor (second grain)))))

; scale-grain-bill-to-efficiency
; 1. Scales grain bill according to (assumed) recipe and house efficiencies
; 2. grain-bill        : see (1)
;    recipe-efficiency : brewhouse efficiency assumed by recipe (typically 75%)
;    house-efficiency  : actual brewhouse efficiency
(defun scale-grain-bill-to-efficiency (grain-bill recipe-efficiency house-efficiency)
  (scale-grain-bill grain-bill (/ recipe-efficiency house-efficiency)))


;;; Maximum ppg (gp/pound/gallon) of various malted grains
;;; Data from Palmer's "How to Brew" 3rd ed., Table 27

;;; All other data found on manufacturer and distributor websites.
;;; Malt data given in the form (MALT-NAME MAX-PPG SRM)
(defparameter *grain-data* (list
  ; Base malts
  (list 'UK-PILSNER      36 1)
  (list 'MALTED-OATS     37 1)
  (list '2-ROW           36 2)
  (list '6-ROW           35 2)
  (list 'BELGIAN-PILSNER 36 2)
  (list 'GERMAN-PILSNER  37 2)
  (list '2-ROW-LAGER     38 2)
  (list 'WHITE-WHEAT     40 2) 
  (list 'BELGIAN-WHEAT   37 2)
  (list 'GERMAN-WHEAT    39 2)
  (list 'CARAPILS        33 2)
  (list 'DEXTRINE-MALT   40 2)
  (list 'ACIDULATED      27 3)
  (list 'PEATED          34 3)
  (list 'MARRIS-OTTER    38 3)
  (list 'ENGLISH-MILD    37 4)
  (list 'VIENNA          35 4)
  (list 'TOASTED         29 5)
  (list 'DARK-WHEAT      39 9)
  (list 'MUNICH          35 9)
  
  ; Speciality malts
  (list 'SMOKED          37 9)
  (list 'CRYSTAL-10      35 10)
  (list 'CRYSTAL-20      34 20)
  (list 'CARARED         35 20)
  (list 'BISCUIT         36 23)
  (list 'VICTORY         34 28)
  (list 'BROWN           32 65)
  (list 'HONEY-MALT      37 25)
  (list 'CRYSTAL-30      34 30)
  (list 'CRYSTAL-40      34 40)
  (list 'CRYSTAL-60      34 60)
  (list 'CRYSTAL-80      34 80)
  (list 'CRYSTAL-90      34 90)
  (list 'CRYSTAL-120     33 120)
  (list 'SPECIAL-B       31 180)
  (list 'ROASTED-BARLEY  25 300)
  (list 'CHOCOLATE       28 350)
  (list 'CARAFA-II       36 412)
  (list 'BLACK-PATEN     25 500)

  ; Grain adjuncts
  (list 'CARAFOAM        0  1)
  (list 'FLAKED-OATS     32 0)
  (list 'FLAKED-CORN     39 0)
  (list 'FLAKED-BARLEY   32 0)
  (list 'FLAKED-WHEAT    36 0)
  (list 'FLAKED-RICE     38 0)

  ; Simple sugar adjuncts
  (list 'MALTODEXTRIN    40 0)
  (list 'CORN-SUGAR      42 0)
  (list 'CANE-SUGAR      46 0)))