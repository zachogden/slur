;;;; slur
;;;; brewery superclass
;;;; --> should encapsulate the environment and equipment in brewery,
;;;; --> as well as some brewery standard practices (i.e. water/grist ratio)

(defclass brewery ()
  ((name
    :initarg :name
    :initform "Default Brewery"
    :accessor name
    :documentation "Brewery profile name")
  (wgr
    :initarg :wgr
    :initform 1.5
    :accessor wgr
    :documentation "water:grist ratio")
  (mashtun-vol
    :initarg :mashtun-vol
    :initform 9.5
    :accessor mashtun-vol
    :documentation "Usable mashtun volume (gal)")
  (mash-eff
    :initarg :mash-eff
    :initform 0.75
    :accessor mash-eff
    :documentation "Mash effeciency")
  (boil-loss
    :initarg :boil-loss
    :initform 1.0
    :accessor boil-loss
    :documentation "Boil loss (gal/hr)")
  (ambient-temp
    :initarg :ambient-temp
    :initform 70
    :accessor ambient-temp
    :documentation "Ambient brewhouse temp/dry grain temp")))

(defparameter *brewery* nil)

; setup-brewery
; 1. Initializes brewery
; 2. N/A
; 3. TODO: Include code for settings load from file
(defun setup-brewery ()
  (setf *brewery* 
    (make-instance 'brewery 
      :name "Maize & Brew" 
      :mashtun-vol   9.5 
      :mash-eff      0.61889505 ; most recent brew 
      :ambient-temp 69.00 
      :wgr           1.5)))



