;;;; slur
;;;; io
;;;; --> File I/O, recipe loading/saving functions

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

    (print (BG r) file)
    (print (OG r) file)
    (print (FG r) file)))