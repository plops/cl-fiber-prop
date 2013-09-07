(in-package :g)

(defun bessel-j-and-deriv (l x)
  (values (jn l x)
	  (cond 
	    ((= l 0) (* .5 (- (jn 2 x) (jn 0 x))))
	    ((= l 1) (* .25 (- (jn 3 x) (* 3 (jn 1 x)))))
	    (t (* .25 (+ (jn (+ l 2) x)
			 (* -2 (jn l x))
			 (jn (- l 2) x)))))))

(defclass cubic-interp ()
  ((table :reader table :type (simple-array double-float 3)) ;; lmax points 2
   (start :reader start :initarg :start :initform 0d0 :type double-float)
   (end :reader end :initarg :end :initform 100d0 :type double-float)))

(defmethod initialize-instance :after ((c cubic-interp) &key (fun #'bessel-j-and-deriv) (lmax 10) (n 100))
  (with-slots (start end table) c
    (setf table (make-array (list lmax n 2) :element-type 'double-float))
    (dotimes (l lmax)
     (dotimes (i n)
       (let ((x (+ start (* (- end start) i (/ 1d0 n)))))
	 (multiple-value-bind (y yy) (funcall fun l x)
	     (setf (aref table l i 0) y
		   (aref table l i 1) yy)))))))
#+nil
(defparameter *bla* (make-instance 'cubic-interp))

#+nil
(array-dimensions (slot-value *bla* 'table))


(defmethod interp ((lut cubic-interp) l x)
    (declare (type fixnum l)
	     (type double-float x)
	     (values double-float &optional))
    (declare (optimize (debug 0) (speed 3) (safety 1)))
    (with-slots (start end table) lut
      (declare (type double-float start end)
	       (type (simple-array double-float 3) table))
      (destructuring-bind (lmax n two) (array-dimensions table)
	(declare (type fixnum lmax n)
		 (ignore two))
	(multiple-value-bind (i xx) (floor (* n (/ (- x start)
						   (- end start))))
	  (declare (type double-float xx)
		   (type fixnum i))
	  (let* ((diff (* (- end start) (/ 1d0 n)))
		  (a (- 1 xx))
		  (b xx)
		  (c (* 1/6 (- (* a a a) a)))
		  (d (* 1/6 (- (* b b b) b))))
	    (declare (type double-float diff a b c d))
	     (+ (* a (aref table l i 0))
		(* b (aref table l (+ i 1) 0))
		(* c (aref table l i 1) diff diff)
		(* d (aref table l (+ i 1) 1) diff diff)))))))

#+nil
(let ((j (make-instance 'cubic-interp)))
  (interp j 3 .3))
