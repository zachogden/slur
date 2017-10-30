;;;; slur
;;;; Recipe data

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

;;; Section 0: Data structures

(defclass recipe ()
  ((name
    :initarg :name
    :initform "Unnamed Recipe"
    :accessor name
    :documentation "Recipe name")
  (volume
    :initarg :volume
    :initform 5
    :accessor volume
    :documentation "Recipe volume (gal)")
  (water
    :initarg :water
    :initform nil
    :accessor water
    :documentation "Water profile [RESERVED]")
  (mash
    :initarg :mash
    :initform nil
    :accessor mash
    :documentation "Mash steps")
  (kettle
    :initarg :kettle
    :initform nil
    :accessor kettle
    :documentation "Kettle steps")
  (ferm
    :initarg :ferm
    :initform nil
    :accessor ferm
    :documentation "Fermentation steps")
  (packaging
    :initarg :pack
    :initform nil
    :accessor pack
    :documentation "Packaging (kegging/bottling) [RESERVED]")

  ; recipe descriptor values
  (BG
    :initarg :BG
    :initform nil
    :accessor BG
    :documentation "Recipe BG")
  (OG
    :initarg :OG
    :initform nil
    :accessor OG
    :documentation "Recipe OG")
  (FG
    :initarg :FG
    :initform nil
    :accessor FG
    :documentation "Recipe FG")
  (SRM
    :initarg :SRM
    :initform nil
    :accessor SRM
    :documentation "Recipe SRM")))

;;; Section 1: Recipe file I/O 

; read-mash
(defun read-mash (raw)
  (loop for line in raw 
    if (eql 'GRAIN (second line)) 
      collect (make-instance 'fermentable 
        :name (first line) 
        :form (second line) 
        :max-yield (third line)
        :srm (fourth line) 
        :weight (fifth line))
      collect line))

; read-kettle
;THIS IS FUCKED
(defun read-kettle (raw)
  (loop for line in raw
    if (eql (type-of (first line)) 'cons) 
      collect (list 
        (make-instance 'hop
          :name (first (first line))
          :form (second (first line))
          :alpha-acid (third (first line)))
        (second line)
        (third line)
        (fourth line))
    else  
      collect line))

; read-recipe
; 1. reads .slur recipe file, returning recipe object
; 2. filename : name of file to be read
(defun read-recipe (filename)
  (with-open-file (file filename) 
    (make-instance 'recipe 
      :name   (read file) 
      :volume (read file)
      :water  (read file)
      :mash   (read-mash   (read file))
      :kettle (read-kettle (read file))
      :ferm   (read file)
      :pack   (read file)

      :BG (read file)
      :OG (read file)
      :FG (read file))))

(defun write-recipe (r filename)
  (with-open-file (file filename :direction :output :if-exists :supersede) 
    (print (name r)   file)
    (print (volume r) file)
    (print (water r)  file)
    (print (mash r)   file)
    (print (kettle r) file)
    (print (ferm r)   file)
    (print (pack r)   file)

    (print (BG r) file)
    (print (OG r) file)
    (print (FG r) file)))

;;; Section 2: Recipe building

; build
; 1. Calculates descriptors for recipe
; 2. r : recipe
;    b : brewery
(defun build (r b)
  (setf (BG r) (target-bg r b))
  (setf (OG r) (target-og r b))
  (setf (FG r) (gp-to-sg (* 0.25 (sg-to-gp (OG r)))))

  ; Morey SRM
  (setf (SRM r) 
    (* 1.4922 (expt (/ (loop for g in (get-mash-grains r) sum (* (second g) (third g))) (volume r)) 0.6859))))

; spit-recipe
; 1. Outputs recipe data in human-readable format
; 2. b : brewery
;    r : recipe
; 3. N/A
(defun spit-recipe (r b)
  (build r b)

  (format t "~a (~a), ~a gal~%~%" (name r) (name b) (volume r))

  (spit-mash r b)
  (format t "~%For a target boil vol of ~a gal and BG ~a~%" (boil-vol r b) (float-round (BG r) 3))

  (spit-kettle r)

  (format t "~%For a target OG of ~a~%~%" (float-round (OG r) 3))

  (spit-ferm r) 

  (format t "~%Recipe Overview:~%OG:  ~a~%FG:  ~a~%ABV: ~a%~%IBU: ~a~%SRM: ~a~%"
    (float-round (OG r) 3)
    (float-round (FG r) 3) 
    (float-round (abv (OG r) (FG r)) 1) 
    (round (recipe-IBU (kettle r) (BG r) (volume r)))
    (float-round (SRM r) 1)))

