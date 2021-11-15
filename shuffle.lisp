(defpackage #:shuffle
  (:use :cl)
  (:export #:fisher-yates
	   #:faro
	   #:overhand
	   #:riffle
	   #:pile
	   #:mongean
	   #:cut
	   #:split
	   #:mexican-spiral
	   #:strip
	   #:casino
	   #:st1000))
(in-package :shuffle)

(defun fisher-yates-backup (sequence)
  (let* ((length (length sequence))
	 (array (if (typep sequence 'array) sequence (make-array length :initial-contents sequence))))
    (loop :for i :from (1- length) :downto 0
	  :for random := (random (1+ i))
	  :do (rotatef (aref array random) (aref array i)))
    array))

(defun fisher-yates (sequence)
  (loop :with length := (length sequence) 
	:for i :from (1- length) :downto 0
	:for random := (random (1+ i))
	:do (if (eql 'cons (type-of sequence))
		(rotatef (elt sequence random) (elt sequence i))
		(rotatef (aref sequence random) (aref sequence i))))
  sequence)

;;fixme, split should be more precise than a binomial distribution, standard distribution with a standard-deviation of about 3-4
(defun split (sequence &optional (percent 50) (place (randist:random-binomial (* percent 0.01d0) (length sequence))))
  (list (subseq sequence 0 place) (subseq sequence place)))

(defun cut-50 (sequence &optional (place (round (/ (length sequence) 2))))
  (concatenate (type-of sequence) (subseq sequence place) (subseq sequence 0 place)))

(defun cut (sequence &optional (percent 50) (place (randist:random-binomial (* 0.01d0 percent) (length sequence))))
  (concatenate (type-of sequence) (subseq sequence place) (subseq sequence 0 place)))


(defun faro (sequence &optional (kind 0))
  (destructuring-bind (a b) (split (coerce sequence 'vector) nil (/ (length sequence)2))
    (when (= kind 1) (rotatef a b))
    (coerce (loop :for i :across a
		  :for j :across b
		  :collect i
		  :collect j)
	    (type-of sequence))))

(defun faro-pattern (array pattern)
  (loop :for bit :in pattern
	:for arr := (faro array bit) :then (faro arr bit)
	:finally (return arr)))


(defun strip (sequence &optional (cuts 10)&aux (length (length sequence)) (packet-size (round (/ length (1+ cuts)))))
  "Cut size represents the ideal size of packets.  Actual size will be chosen by random binomial."
  (loop :for i := 0 :then (+ i cards)
	:while (< i length)
	:for cards := (randist:random-binomial 0.5d0 (* 2 packet-size))
	:collect (subseq sequence i (min length (+ i cards))) :into result
	:finally (return (apply #'concatenate (type-of sequence) (nreverse result)))))

;;; Implemented based on Gilbert-Shannon-Reeds model https://en.wikipedia.org/wiki/Gilbert%E2%80%93Shannon%E2%80%93Reeds_model
(defun riffle (sequence &aux (length (length sequence)))
  (destructuring-bind (a b) (split sequence (randist:random-binomial .5d0 length))
    (coerce (loop :for la := (length a) :then (length a)
		  :for lb := (length b) :then (length b)
		  :for pa = (if (zerop la) 0 (/ la (+ la lb)))
		  :for pb = (if (zerop lb) 0 (/ lb (+ la lb)))
		  :for i :below length
		  :for random := (random 1.0)
		  :for card := (if (< random pa) (pop a) (pop b))
		 ; :do (u:fp random pa pb card)
		  :collect card)
	    (type-of sequence))))

(defun mongean (sequence)
  (loop :with result := '()
	:for card :across (coerce sequence 'vector)
	:for i :from 1
	:do (if (oddp i)
		(push card result)
		(nconc result (list card)))
	:finally (return (coerce (nreverse result) (type-of sequence)))))

(defun mexican-spiral (sequence)
  (loop ;:with result := '()
	:for working := (coerce sequence 'list) :then (append working (list discard))
	:for card := (pop working)
	:for discard := (pop working)
	:collect card :into result
	:until (null discard)	
	:finally (return (coerce (nreverse result) (type-of sequence)))))

(defun casino (sequence)
  (riffle (strip (riffle (strip (riffle sequence))))))

(defun st1000 (sequence)
  (riffle (riffle (riffle (riffle (strip (riffle (riffle (strip (riffle sequence))))))))))
