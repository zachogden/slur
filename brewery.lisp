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