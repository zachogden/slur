;;;; slur calculations

;; points-to-gravity
;; 1. Convert gravity points (XX pts) to specific gravity (1.0XX)
;; 2. points
;; 3. specific gravity
(defun points-to-gravity (points)
  (+ 1 (/ points 1000)))

;; gravity-to-points
;; 1. Convert specific gravity (1.0XX) to gravity points (XX pts)
;; 2. specific gravity
;; 3. points
(defun gravity-to-points (gravity)
  (* 1000 (- gravity 1)))

;; priming-sugar-oz
;; 1. Calculate ounces of priming sugar for bottling
;; 2. beer-vol   : gallons of beer being bottled
;;    sugar-type : either corn (4oz/5gal) or table (3.8oz/5gal)
;; 3. ounces of priming sugar
(defun priming-sugar-oz (beer-vol &optional (sugar-type 'corn))
  (if (eql sugar-type 'corn) 
    (* 0.8 beer-vol)
    (* 0.76 beer-vol)))

;; AAU
;; 1. Calculate alpha acid units of hop addition
;; 2. weight     : weight of hops added
;;    AA-percent : alpha acid percent rating of added hops
;; 3. alpha acid units 
(defun AAU (weight AA-percent)
  (* weight AA-percent))

;; PBW-oz
;; 1. Calculate ounces of PBW powder for cleaning solution
;; 2. gallons : gallons of solution desired
;; 3. Recommended mixture is 1-2 oz/gal, returns midrange (1.5 oz/gal)
(defun PBW-oz (gallons)
  (* 1.5 gallons))

;; oz-to-tbsp
;; 1. Calculate tablespoons of substance given vol in ounces
;; 2. Ounces
;; 3. Tablespoons (2 US tbsp/1 US fl. oz.)
(defun oz-to-tbsp (oz)
  (* 2 oz))

;; oz-to-cup
;; 1. Calculate cups of substance given vol in ounces
;; 2. Ounces
;; 3. Cups (8 US fl. oz/cup)
(defun oz-to-cup (oz)
  (/ oz 8))

;; starsan-oz
;; 1. Calculate ounces of StarSan for sanitizing solution
;; 2. gallons : gallons of solution desired
;; 3. Recommendated mixture is 1 oz / 5 gallons
(defun starsan-oz (gallons)
  (* 0.2 gallons))

;; hop-utilization
;; 1. Calculate hop utilization
;; 2. boil-gravity : SG of wort measured pre-boil
;;    boil-time    : duration of boil in minutes
;; 3. Hop utilization based on empirically derived data from G. Tinseth
(defun hop-utilization (boil-gravity boil-time)
  (*
    (* 1.65 (expt 0.000125 (- boil-gravity 1)))
    (/ (- 1 (exp (* -0.04 boil-time)))4.15)))

;; IBU
;; 1. Calculate IBU of wort
;; 2. AAU           : cumulative AAU of hop additions
;;    boil-gravity  : SG of wort measured pre-boil
;;    boil-time     : duration of boil in minutes
;;    recipe-volume : desired final volume of fermented beer
;; 3. IBU = AAU * U * 75 / V
(defun IBU (AAU boil-gravity boil-time recipe-volume)
  (* AAU (hop-utilization boil-gravity boil-time) (/ 75 recipe-volume)))

;; recipe-IBU
;; 1. Calculate cumulative IBU of all hop additions
;; 2. hop-schedule  : list of hop additions of the form (weight | AA % | time), where time is boil length
;;    boil-gravity  : SG of wort measured pre-boil
;;    recipe-volume : desired final volume of fermented beer
(defun recipe-IBU (hop-schedule boil-gravity recipe-volume)
  (loop for hop-addition in hop-schedule sum 
    (IBU (AAU (first hop-addition) (second hop-addition)) boil-gravity (third hop-addition) recipe-volume)))

(defun extract-points (yield pounds gallons)
  (/ (* yield pounds) gallons))

(defun extract-addition (desired-OG yield gallons)
  (* gallons (/ (gravity-to-points desired-OG) yield)))

(defun apparent-attenuation (OG FG)
  (/ (- OG FG) (- OG 1)))

;; pitch-rate-cells
;; 1. Calculates pitch rate (in billions of yeast cells) for a given wort
;; 2. wort-gravity : OG of wort
;;    gallons      : gallons of wort
;; 3. Interpolated from data in Palmer's "How to Brew," 3rd ed., page 68
(defun ale-pitch-rate (wort-gravity gallons)
  (* (+ (* 12 (/ (mod (gravity-to-points wort-gravity) 50) 10)) 16) gallons))

(defun lager-pitch-rate (wort-gravity gallons)
  (ale-pitch-rate (+ wort-gravity 0.01) gallons))

;; starter-growth-rate
;; Calculate the growth rate () given an inoculation in billions of cells/liter
;; 2. inoculation : initial inoculation of starter
;; 3. based on empirical data from Chris White and interpretation by Brewer's Friend
(defun starter-growth-rate (inoculation)
  (- (* 12.54793776 (expt inoculation -0.4594858324)) 0.9994994906))


