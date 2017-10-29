;;;; slur

;;;; v. 0.1.7 / 28 Oct 2017

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

(load (merge-pathnames "yeast.lisp" *load-truename*))
(load (merge-pathnames "packaging.lisp" *load-truename*))
(load (merge-pathnames "recipe.lisp" *load-truename*))

(setup-fermentables-table)

(defparameter my-brewery (make-instance 'brewery :name "Maize & Brew" :mash-eff 0.75 :ambient-temp 68 :wgr 1.5))

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

  (add-kettle-step blonde (make-hop-addition willamette 1.0 60)))

(defun make-NEIPA ()
  (setf NEIPA (make-instance 'recipe :name "New England IPA" :volume 5.5))

  (add-mash-step NEIPA (make-grain '2-ROW       11.5))
  (add-mash-step NEIPA (make-grain 'FLAKED-OATS 2.5))

  (add-mash-step NEIPA (list 'MASH   150 60 100))
  (add-mash-step NEIPA (list 'SPARGE 170 60 100))

  (add-kettle-step NEIPA (list 'BOIL 212 60))
  (add-kettle-step NEIPA (list 'WHIRLFLOC 212 15))

  (add-kettle-step NEIPA (make-hop-addition centennial 0.25 60))
  (add-kettle-step NEIPA (make-hop-addition centennial 0.50 5))
  (add-kettle-step NEIPA (make-hop-addition amarillo   0.50 5))
  (add-kettle-step NEIPA (make-hop-addition simcoe     0.50 5))

  (add-kettle-step NEIPA (make-hop-addition centennial 0.75 20 'WHIRLPOOL))
  (add-kettle-step NEIPA (make-hop-addition amarillo   0.75 20 'WHIRLPOOL))
  (add-kettle-step NEIPA (make-hop-addition simcoe     0.75 20 'WHIRLPOOL)))

(defun asd ()
  (make-blonde)
  (spit-recipe blonde my-brewery))

