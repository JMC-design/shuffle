(in-package :asdf-user)
(defsystem "shuffle"
  :author "Johannes Martinez Calzada"
  :description "Shuffling algorithms"
  :licence "llgpl"
  :depends-on ("cl-randist")
  :components ((:file "shuffle")))
