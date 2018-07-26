;;;; slur

;;;; v. 0.1.7 / 28 Oct 2017

;;;; To-do
;;;; 1) water chemistry [vvh]
;;;; --> [ion] given common salt additions
;;;; --> i/o water profiles
;;;; --> bounds checking for pH/alk (?)
;;;; 2) Include extract brewing (fermentables in boil) [h]
;;;; 3) Include FV additions (i.e. sugar in primary/secondary a la belgians) [vh]
;;;; 4) Flesh out fermentation functionality [vh]

;;;; 2, 3, 4, 1

;;;; Conventions:
;;;; -> functions documented in the form
;;;; -> function-name
;;;; -> 1. basic description
;;;; -> 2. argument(s)
;;;; -> 3. notes/assumptions, if any
;;;; -> macros should be self explanatory enough to not require this 

(load (merge-pathnames "start.lisp" *load-truename*))

(boot-slur)

(defvar hh nil)

(defun make-red()
  (setf hh (make-instance 'recipe :name "California Red" :volume 5.25))

  (add-step hh 'MASH (make-grain '2-ROW      9.925))
  (add-step hh 'MASH (make-grain 'MUNICH      2.00))
  (add-step hh 'MASH (make-grain 'CRYSTAL-40  1.00))
  (add-step hh 'MASH (make-grain 'CRYSTAL-120 0.50))
  (add-step hh 'MASH (make-grain 'VICTORY     1.00))
  (add-step hh 'MASH (make-grain 'CHOCOLATE (oz-to-lbs 3.0)))

  (add-step hh 'MASH (list 'MASH         152 60 100))
  (add-step hh 'MASH (list 'BATCH-SPARGE 170 60 100))

  (add-step hh 'KETTLE (list 'BOIL      212 60))
  (add-step hh 'KETTLE (list 'WHIRLFLOC 212 15))

  (add-step hh 'KETTLE (make-hop-addition (get-from-inventory 'SIMCOE 'HOPS)     1.25 60 'BOIL))
  (add-step hh 'KETTLE (make-hop-addition (get-from-inventory 'CASCADE 'HOPS)    1.0 10 'BOIL))
  (add-step hh 'KETTLE (make-hop-addition (get-from-inventory 'CENTENNIAL 'HOPS) 1.0 10 'BOIL))
  (add-step hh 'KETTLE (make-hop-addition (get-from-inventory 'CASCADE 'HOPS)    1.0 00 'BOIL))
  (add-step hh 'KETTLE (make-hop-addition (get-from-inventory 'CENTENNIAL 'HOPS) 1.0 00 'BOIL))

  (add-step hh 'FERM (list 'PRIMARY 'WLP-002 10 70)))

(defun asd ()
  (make-red)
  (spit-recipe hh *brewery*))


