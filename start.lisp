;;;; slur
;;;; start
;;;; --> initialization/boot routines

(load (merge-pathnames "utilities.lisp" *load-truename*))

(load (merge-pathnames "brewery.lisp" *load-truename*))
(load (merge-pathnames "ingredient.lisp" *load-truename*))
(load (merge-pathnames "fermentable.lisp" *load-truename*))
(load (merge-pathnames "hops.lisp" *load-truename*))

(load (merge-pathnames "inventory.lisp" *load-truename*))

(load (merge-pathnames "mash-boil.lisp" *load-truename*))
(load (merge-pathnames "fermentation.lisp" *load-truename*))
(load (merge-pathnames "recipe.lisp" *load-truename*))

(load (merge-pathnames "io.lisp" *load-truename*))

(defun boot-slur ()
  (setup-fermentables-table)
  (setup-yeast-table)
  (setup-inventory)
  (setup-brewery)

  (load-inventory))