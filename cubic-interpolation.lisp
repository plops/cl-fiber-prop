(in-package :cl-fiber-prop)

(defmacro def-interp (name fun)
;; this macro is supposed to be called with a function
;; that calculates a function and its second derivative
;; (def-interp bessel-j bessel-j-and-deriv)
;; the functions can be used like this:
;; first an array is filled with values of the function and the second derivative
;;  (bessel-j-interp-init :start 0s0 :end 110s0 :n 100)
;; then the *-interp function can be used to interpolate using the array
;;  (time (loop for x from 0s0 upto 98s0 by 1s-5 do (bessel-j-interp 4 x)))
  (flet ((combin (x)
	   (format nil "~a-~a" name x)))
   (macrolet ((s (x)
		`(intern (format nil "*~a*" (combin ',x)))))
     (let ((interp (intern (format nil "~a-INTERP" name)))
	   (interp-init (intern (format nil "~a-INTERP-INIT" name))))
       `(progn
	  (defparameter ,(s n) 100)
	  (defparameter ,(s lmax) 100)
	  (defparameter ,(s start) 0d0)
	  (defparameter ,(s end) 100d0)
	  (defparameter ,(s s) (/ ,(s n) (- ,(s END) ,(s START))))
	  (defparameter ,(s diff2) (expt (/ ,(s s)) 2))
	  (defparameter ,(s table)
	    (MAKE-ARRAY (LIST ,(s lmax) ,(s N) 2)
			:ELEMENT-TYPE 'double-float))

	  (proclaim '(type double-float ,(s START) ,(s end) ,(s s) ,(s diff2)))
	  (proclaim '(TYPE FIXNUM ,(s lmax) ,(s n)))
	  (proclaim '(TYPE (SIMPLE-ARRAY double-float 3) ,(s table)))
	  (DEFUN ,interp-init (&KEY (START 0.0d0) (END 10.0d0) (N 100) (lmax 100))
	    (DECLARE (OPTIMIZE (DEBUG 3) (SPEED 3) (SAFETY 1)))
	    (SETF ,(s START) START
		  ,(s END) END
		  ,(s lmax) lmax
		  ,(s N) N
		  ,(s s) (/ n (- END START))
		  ,(s diff2) (expt (/ ,(s s)) 2)
		  ,(s table) (MAKE-ARRAY (LIST LMAX N 2) :ELEMENT-TYPE 'double-float))
	    (DOTIMES (L LMAX)
	      (DOTIMES (I N)
		(LET ((X (+ START (* (- END START) I (/ 1.0f0 N)))))
		  (DECLARE (TYPE double-float X))
		  (MULTIPLE-VALUE-BIND (Y YY) (,fun L X)
		    (SETF (AREF ,(s table) L I 0) Y
			  (AREF ,(s table) L I 1) YY))))))

	  (proclaim '(INLINE ,interp))

	  (DEFUN ,interp (L X)
	    (DECLARE (TYPE (INTEGER 0 10000) L)
		     (TYPE double-float X)
		     (VALUES double-float &OPTIONAL))
	    (DECLARE (OPTIMIZE (DEBUG 0) (SPEED 3) (SAFETY 0)))
	    (MULTIPLE-VALUE-BIND (I XX) (truncate (* ,(s s) (- x ,(s start))))
	      (DECLARE (TYPE fixnum I)
		       (TYPE double-float XX))
	      (LET* ((A (- 1 XX))
		     (B XX)
		     (C (* 1/6 a (- (* A A) 1)))
		     (D (* 1/6 b (- (* B B) 1))))
		(+ (* A (AREF ,(s table) L I 0))
		   (* B (AREF ,(s table) L (+ I 1) 0))
		   (* C (AREF ,(s table) L I 1) ,(s diff2))
		   (* D (AREF ,(s table) L (+ I 1) 1) ,(s diff2)))))))))))


(declaim (ftype (function (fixnum double-float) (values double-float &optional)) jn))
(cffi:defcfun jn :double (n :int) (x :double))



(defun bessel-j-and-deriv (l x)
  (declare (values double-float double-float &optional)
	   (type fixnum l)
	   (type double-float x))
  (values (jn l x)
	  (cond 
	    ((= l 0) (* .5d0 (- (jn 2 x) (jn 0 x))))
	    ((= l 1) (* .25d0 (- (jn 3 x) (* 3 (jn 1 x)))))
	    (t (* .25d0 (+ (jn (+ l 2) x)
			   (* -2 (jn l x))
			   (jn (- l 2) x)))))))


(defun bessel-k-and-deriv (l x)
  (declare (values double-float )
	   (type fixnum l)
	   (type double-float x))
  (flet ((kn (l x)
	   (declare (type fixnum l)
		    (type double-float x))
	   (gsll:cylindrical-bessel-k (* 1d0 l) (* 1d0 x))
	   ))
    (values (kn l x)
	    (cond 
	      ((= l 0) (* .5d0 (+ (kn 2 x) (kn 0 x))))
	      ((= l 1) (* .25d0 (+ (kn 3 x) (* 3 (kn 1 x)))))
	      (t (* .25d0 (+ (kn (+ l 2) x)
			     (* 2 (kn l x))
			     (kn (- l 2) x))))))))

(defun bessel-k-scaled-and-deriv (l x)
  ;; factor(diff(bessel_k(2,x)*exp(x),x,2));
  (declare (values double-float double-float &optional)
	   (type fixnum l)
	   (type double-float x))
  (flet ((kn (l x)
	   (declare (type fixnum l)
		    (type double-float x))
	   (gsll:cylindrical-bessel-k-scaled (* 1d0 l) (* 1d0 x))
	   ))
    (values (kn l x)
	    (cond 
	      ((= l 0) (* .5d0 (+ (kn 2 x) (* -4 (kn 1 x)) (* 3 (kn 0 x)))))
	      ((= l 1) (* .25d0 (+ (kn 3 x) (* -4 (kn 2 x)) (* 7 (kn 1 x)) (* -4 (kn 0 x)))))
	      (t (* .25d0 (+ (kn (+ l 2) x)
			     (* -4 (kn (+ l 1) x))
			     (* 6 (kn l x))
			     (* -4 (kn (- l 1) x))
			     (kn (- l 2) x))))))))



(declaim (optimize (speed 3) (safety 1) (debug 0)))

(def-interp bessel-j bessel-j-and-deriv)

(declaim (inline bessel-j))
(defun bessel-j (l x)
  (declare (type fixnum l)
	   (type double-float x)
	   (values double-float &optional)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (if (<= 0 l)
      (bessel-j-interp l x)
      (* (if (oddp l) -1 1)
	 (bessel-j-interp (abs l) x))))



(def-interp bessel-k bessel-k-and-deriv)
(def-interp bessel-k-scaled bessel-k-scaled-and-deriv)




#+nil
(progn
  (bessel-j-interp-init :start 0s0 :end 110s0 :n 100)
  (time (loop for x from 0s0 upto 98s0 by 1s-5 do (bessel-j-interp 4 x))))

; (/ 1815165956 (* 99 1e5)) => 183 cycles per call
; (/ 1386921624 (* 99 1e5)) => 140 cycles per call (double-float)
;    896260860   => 92 cycles per call (global variables, no argument in floor)
;    761730486   => 78 cycles per call (global variables, inline, division in floor)
;    428000000   => 45 cycles (truncate instead of floor, simplified calculation)
; (/ 172161426 (* 99 1e5)) => 17.4 cycles per call (global variables, no argument in floor, inline)
;    145631000   => 15 cycles (truncate, no calc in argument)
