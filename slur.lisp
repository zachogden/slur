;;;; slur

;;;; v. 0.1.6 / 16 Jul 2017

;;;; Conventions:
;;;; -> functions documented in the form
;;;; -> function-name
;;;; -> 1. basic description
;;;; -> 2. argument(s)
;;;; -> 3. notes/assumptions, if any
;;;; -> macros should be self explanatory enough to not require this 
;;;;
;;;; Yeast cell counts assumed to be given as billions of cells

;;;; TODO:
;;;; 1. Error checking for ingredient addition (is grain in table?)
;;;; 2. Recipe file I/O
;;;; 3. Water
;;;; 4. Mark recipe as made (w/ actual gravity, etc)?

(defparameter *grain-table* (make-hash-table))

(defparameter *brewery* (make-hash-table))

(load (merge-pathnames "grains.lisp" *load-truename*))
(load (merge-pathnames "unit-conversions.lisp" *load-truename*))
(load (merge-pathnames "mash.lisp" *load-truename*))
(load (merge-pathnames "hops.lisp" *load-truename*))
(load (merge-pathnames "yeast.lisp" *load-truename*))
(load (merge-pathnames "packaging.lisp" *load-truename*))
(load (merge-pathnames "recipe.lisp" *load-truename*))

; build-brewery
; 1. Sets up slur environment
; 2. mash-efficiency : Calculated/observed mash efficiency
;    boil-loss       : wort volume loss during boil (gal/hr)
; 3. Required prior to use of recipe functions
(defun build-brewery (mash-efficiency boil-loss-rate ambient-temp)
  (setf (gethash 'MASH-EFFICIENCY *brewery*) mash-efficiency)
  (setf (gethash 'WATER-GRIST-RATIO *brewery*) 1.25)
  (setf (gethash 'BOIL-LOSS-RATE  *brewery*) boil-loss-rate)
  (setf (gethash 'AMBIENT-TEMP    *brewery*) ambient-temp)
  (setup-grain-table))

(defparameter APA nil)

; Brewing Classic Styles, pg 135
(defun make-APA ()
  (build-brewery 0.62 1 76)
  (setf APA (make-hash-table))
  
  ; 1. Header
  (setf (gethash 'HEADER APA) '('AMERICAN-PALE-ALE 5))

  ; 3. Grains
  (add-ingredient APA 'GRAIN (grain '2-ROW   11.0 'MASH))
  (add-ingredient APA 'GRAIN (grain 'MUNICH  0.75 'MASH))
  (add-ingredient APA 'GRAIN (grain 'VICTORY 0.75 'MASH))
  (add-ingredient APA 'GRAIN (grain 'GERMAN-WHEAT   0.50 'MASH))

  ; 4. Hops
  (add-ingredient APA 'HOP (hop 'CENTENNIAL 1.25 7.5 'BOIL 60))
  (add-ingredient APA 'HOP (hop 'CASCADE    0.5  6.4 'BOIL 10))
  (add-ingredient APA 'HOP (hop 'CENTENNIAL 0.75 7.5 'BOIL 10))
  (add-ingredient APA 'HOP (hop 'CASCADE    0.5  6.4 'BOIL  0))
  (add-ingredient APA 'HOP (hop 'CENTENNAIL 0.75 7.5 'BOIL  0))

  ; 5. Mash
  (add-ingredient APA 'MASH (mash-step 'SINGLE-INFUSION 152 60 100))
  (add-ingredient APA 'MASH (mash-step 'SPARGE          170 60 100))

  ; 6. Kettle
  (add-ingredient APA 'KETTLE (kettle-step 'BOIL      212 60))
  (add-ingredient APA 'KETTLE (kettle-step 'WHIRLFLOC 212 15))

  ; 7. Fermentation
  (add-ingredient APA 'FERMENT (ferm-step 'PRIMARY-FERMENTATION 67 10 'WLP-001)))

(defun spit-recipe (recipe)
  (let* (
    (fermentables (gethash 'GRAIN   recipe))
    (hops         (gethash 'HOP     recipe))
    (fermentation (gethash 'FERMENT recipe))
    (mash         (gethash 'MASH    recipe))
    (kettle-steps (gethash 'KETTLE  recipe))
    (boil-length  60)

    (name (first (gethash 'HEADER recipe)))
    (volume (second (gethash 'HEADER recipe)))
    (water-grist-ratio (gethash 'WATER-GRIST-RATIO *brewery*))

    (mash-grains (loop for grain in fermentables if (eql (third grain) 'MASH) collect grain))
    (mash-loss   (grain-water-absorbed (grist-mass mash-grains)))
    
    (boil-grains    (loop for grain in fermentables if (eql (third grain) 'BOIL) collect grain))
    (boil-vol       (+ volume (* (gethash 'BOIL-LOSS-RATE *brewery*) (/ boil-length 60.0))))
    
    (strike-vol  (float-round (strike-water-vol (grist-mass mash-grains) water-grist-ratio) 2))
    (sparge-vol  (float-round (+ mash-loss (- boil-vol strike-vol)) 2))
    (strike-temp (strike-water-temp water-grist-ratio (gethash 'AMBIENT-TEMP *brewery*) 149))
    
    (kettle (make-kettle boil-grains hops kettle-steps))

    (target-bg  (house-mash-gp mash-grains boil-vol (gethash 'MASH-EFFICIENCY *brewery*)))
    (target-og  (bg-to-og (+ target-bg (max-mash-gp boil-grains boil-vol)) boil-vol volume))
    (target-fg  nil)
    (target-abv nil))

    (setf target-bg (float-round (gp-to-sg target-bg) 3))
    (setf target-og (float-round (gp-to-sg target-og) 3))

    (format t "******~a, ~agal******~%~%" name volume)
    
    (format t "Strike ~a gallons of water to ~aF~%~%" strike-vol strike-temp)
    
    (format t "Dough in the following:~%")
    (loop for grain in mash-grains do (format t "~a lbs ~a~%" (second grain) (first grain)))
    
    (format t "~%Mash as follows:~%")
    (loop for mash-step in mash do 
      (if (eql (first mash-step) 'SPARGE)
        (format t "Sparge ~a gal @ ~aF" (* sparge-vol (/ (fourth mash-step) 100)) (second mash-step))
        (format t "~a @ ~aF x ~a min (~a% mash)~%" 
          (first mash-step) (second mash-step) (third mash-step) (fourth mash-step))))
    
    (format t "~%For a target boil vol of ~a gal and BG ~a~%~%" boil-vol target-bg)

    (format t "Kettle schedule:~%") 
    
    (loop for kettle-step in kettle 
      collecting (car kettle-step) into time-table do
      (if (not (eql (car (reverse time-table)) (cadr (reverse time-table))))
        (format t "~%***~a MINUTES***~%" (car kettle-step))) do
      
      (if (gethash (cadr kettle-step) *grain-table*)
        (format t "~a lbs ~a~%" (caddr kettle-step) (cadr kettle-step))
        (if (loop for hop in hops if (eql (car hop) (cadr kettle-step)) return t)
          (format t "~a AAU ~a hops~%" (caddr kettle-step) (cadr kettle-step))
          (format t "~a~%" (cadr kettle-step)))))

    (format t "~%For a target OG of ~a~%~%" target-og)

    (loop for ferm-step in fermentation do
      (if (eql (car ferm-step) 'PRIMARY-FERMENTATION)
        (progn
          (format t "Ferment with ~a @ ~aF x ~a days~%" (cadddr ferm-step) (cadr ferm-step) (caddr ferm-step))
          (loop for strain in yeast-bank 
            for ferm-strain        = (fourth ferm-step)
            for strain-name        = (first strain) 
            for strain-attenuation = (third strain)
            for ferm-temp          = (second ferm-step) do 
            
            (when (eql strain-name ferm-strain) (progn
              (setf target-fg (float-round (gp-to-sg (* (sg-to-gp target-og) 
                (- 1 (/ (/ (+ (first strain-attenuation) (second strain-attenuation)) 2) 100)))) 3))
              (setf target-abv (abv target-og target-fg)) (return)))))
        
        (format t "~a @ ~aF x ~a days~%" (car ferm-step) (cadr ferm-step) (cadddr ferm-step))))

    (format t "~%Recipe Overview:~%OG:  ~a~%FG:  ~a~%ABV: ~a%~%IBU: ~a~%"
      target-og target-fg (float-round (abv target-og target-fg) 1) 
      (round (recipe-ibu hops target-bg volume)))))


