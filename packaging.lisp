;;; Section 7: Kegging & Bottling

;; Kegging

; (Phead+1.013)*(2.71828182845904^(-10.73797+(2617.25/(Tbeer+273.15))))*10
(defun carbonation (head-p beer-t)
  (* 10 (* (+ head-p 1.103) (expt 2.71828182845904 (+ -10.73797 (/ 2617.25 (+ beer-t 273.15)))))))
  

(defun carb-psi-f (head-p beer-t)
  (/ (carbonation (/ head-p 14.5038) (/ (- beer-t 32) 1.8)) 1.96))

(defun abv (og fg)
  (* (* 76.08 (/ (- og fg) (- 1.775 og))) (/ fg 0.794)))

;; Bottling
; priming-sugar-oz
; 1. Calculate ounces of priming sugar for bottling
; 2. beer-vol   : gallons of beer being bottled
;    sugar-type : either corn (4oz/5gal) or table (3.8oz/5gal)
; 3. ounces of priming sugar
(defun priming-sugar-oz (beer-vol &optional (sugar-type 'corn))
  (if (eql sugar-type 'corn) 
    (* 0.8 beer-vol)
    (* 0.76 beer-vol)))




