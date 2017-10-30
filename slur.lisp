;;;; slur

;;;; v. 0.1.7 / 28 Oct 2017

;;;; To-do
;;;; 1) water chemistry [vvh]
;;;; --> [ion] given common salt additions
;;;; --> i/o water profiles
;;;; --> bounds checking for pH/alk (?)
;;;; 2) pantry/inventory [vh]
;;;; --> hops
;;;; --> fermentables
;;;; 3) post-brew [vh]
;;;; --> tie-in w/ (3)
;;;; --> tie-in w/ (1)
;;;; 4) mash types [h]
;;;; --> no sparge (confirm w/ size of mash tun)
;;;; --> batch sparge
;;;; --> fly sparge

;;;; 5, 2 (barebones/sandbox), 3 (initial functions)

;;;; Conventions:
;;;; -> functions documented in the form
;;;; -> function-name
;;;; -> 1. basic description
;;;; -> 2. argument(s)
;;;; -> 3. notes/assumptions, if any
;;;; -> macros should be self explanatory enough to not require this 

(load (merge-pathnames "unit-conversions.lisp" *load-truename*))

(load (merge-pathnames "brewery.lisp" *load-truename*))
(load (merge-pathnames "ingredient.lisp" *load-truename*))
(load (merge-pathnames "fermentable.lisp" *load-truename*))
(load (merge-pathnames "hops.lisp" *load-truename*))

(load (merge-pathnames "mash.lisp" *load-truename*))
(load (merge-pathnames "kettle.lisp" *load-truename*))

(load (merge-pathnames "fermentation.lisp" *load-truename*))
(load (merge-pathnames "packaging.lisp" *load-truename*))
(load (merge-pathnames "recipe.lisp" *load-truename*))

(setup-fermentables-table)

(defvar my-brewery 
  (make-instance 'brewery :name "Maize & Brew" :mashtun-vol 10.0 :mash-eff 0.75 :ambient-temp 68 :wgr 1.5))

(defvar NEIPA nil)
(defvar blonde nil)

; makeshift hop shop
(defparameter centennial (make-instance 'hop :name "centennial" :form "pellet" :alpha-acid 10.0))
(defparameter amarillo   (make-instance 'hop :name "amarillo"   :form "pellet" :alpha-acid  8.5))
(defparameter simcoe     (make-instance 'hop :name "simcoe"     :form "pellet" :alpha-acid 13.0))
(defparameter willamette (make-instance 'hop :name "willamette" :form "pellet" :alpha-acid  5.4)) ;confirmed

(defun make-blonde ()
  (setf blonde (make-instance 'recipe :name "Blonde Ale" :volume 5))

  (add-mash-step blonde (make-grain '2-ROW 11.5))
  (add-mash-step blonde (make-grain 'CRYSTAL-10 0.5))

  (add-mash-step blonde (list 'MASH   152 60 100))
  (add-mash-step blonde (list 'SPARGE 170 60 100))

  (add-kettle-step blonde (list 'BOIL      212 60))
  (add-kettle-step blonde (list 'WHIRLFLOC 212 15))

  (add-kettle-step blonde (make-hop-addition willamette 1.0 60 'BOIL))

  (add-ferm-step blonde (list 'PRIMARY 'WLP-001 10 70)))

(defun asd ()
  (make-blonde)
  (spit-recipe blonde my-brewery))

(defvar b1 nil)

(defun aaa ()
  (make-blonde)
  (spit-recipe blonde my-brewery)
  (write-recipe blonde "/lisp/slur/blonde.slur")
  (format t "~%**********~%")
  (setf b1 (read-recipe "/lisp/slur/blonde.slur"))
  (spit-recipe b1 my-brewery))
