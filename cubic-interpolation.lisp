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
	  (defparameter ,(s start) 0s0)
	  (defparameter ,(s end) 100s0)
	  (defparameter ,(s s) (/ ,(s n) (- ,(s END) ,(s START))))
	  (defparameter ,(s diff2) (expt (/ ,(s s)) 2))
	  (defparameter ,(s table)
	    (MAKE-ARRAY (LIST ,(s lmax) ,(s N) 2)
			:ELEMENT-TYPE 'SINGLE-FLOAT))

	  (proclaim '(type SINGLE-FLOAT ,(s START) ,(s end) ,(s s) ,(s diff2)))
	  (proclaim '(TYPE FIXNUM ,(s lmax) ,(s n)))
	  (proclaim '(TYPE (SIMPLE-ARRAY SINGLE-FLOAT 3) ,(s table)))
	  (DEFUN ,interp-init (&KEY (START 0.0f0) (END 10.0f0) (N 100) (lmax 100))
	    (DECLARE (OPTIMIZE (DEBUG 3) (SPEED 3) (SAFETY 1)))
	    (SETF ,(s START) START
		  ,(s END) END
		  ,(s lmax) lmax
		  ,(s N) N
		  ,(s s) (/ n (- END START))
		  ,(s diff2) (expt (/ ,(s s)) 2)
		  ,(s table) (MAKE-ARRAY (LIST LMAX N 2) :ELEMENT-TYPE 'SINGLE-FLOAT))
	    (DOTIMES (L LMAX)
	      (DOTIMES (I N)
		(LET ((X (+ START (* (- END START) I (/ 1.0f0 N)))))
		  (DECLARE (TYPE SINGLE-FLOAT X))
		  (MULTIPLE-VALUE-BIND (Y YY) (,fun L X)
		    (SETF (AREF ,(s table) L I 0) Y
			  (AREF ,(s table) L I 1) YY))))))

	  (proclaim '(INLINE ,interp))

	  (DEFUN ,interp (L X)
	    (DECLARE (TYPE (INTEGER 0 10000) L)
		     (TYPE SINGLE-FLOAT X)
		     (VALUES SINGLE-FLOAT &OPTIONAL))
	    (DECLARE (OPTIMIZE (DEBUG 0) (SPEED 3) (SAFETY 0)))
	    (MULTIPLE-VALUE-BIND (I XX) (truncate (* ,(s s) (- x ,(s start))))
	      (DECLARE (TYPE fixnum I)
		       (TYPE SINGLE-FLOAT XX))
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
(declaim (ftype (function (fixnum single-float) (values single-float &optional)) jnf))
(cffi:defcfun jnf :float (n :int) (x :float))


(defun bessel-j-and-deriv (l x)
  (declare (values single-float single-float &optional)
	   (type fixnum l)
	   (type single-float x))
  (values (jnf l x)
	  (cond 
	    ((= l 0) (* .5s0 (- (jnf 2 x) (jnf 0 x))))
	    ((= l 1) (* .25s0 (- (jnf 3 x) (* 3 (jnf 1 x)))))
	    (t (* .25s0 (+ (jnf (+ l 2) x)
			   (* -2 (jnf l x))
			   (jnf (- l 2) x)))))))


(defun bessel-k-and-deriv (l x)
  (declare (values single-float single-float &optional)
	   (type fixnum l)
	   (type single-float x))
  (flet ((knf (l x)
	   (declare (type fixnum l)
		    (type single-float x))
	   (coerce (gsll:cylindrical-bessel-k (* 1d0 l) (* 1d0 x)) 'single-float)
	   ))
    (values (knf l x)
	    (cond 
	      ((= l 0) (* .5s0 (+ (knf 2 x) (knf 0 x))))
	      ((= l 1) (* .25s0 (+ (knf 3 x) (* 3 (knf 1 x)))))
	      (t (* .25s0 (+ (knf (+ l 2) x)
			     (* 2 (knf l x))
			     (knf (- l 2) x))))))))

(defun bessel-k-scaled-and-deriv (l x)
  ;; factor(diff(bessel_k(2,x)*exp(x),x,2));
  (declare (values single-float single-float &optional)
	   (type fixnum l)
	   (type single-float x))
  (flet ((knf (l x)
	   (declare (type fixnum l)
		    (type single-float x))
	   (coerce (gsll:cylindrical-bessel-k-scaled (* 1d0 l) (* 1d0 x)) 'single-float)
	   ))
    (values (knf l x)
	    (cond 
	      ((= l 0) (* .5s0 (+ (knf 2 x) (* -4 (knf 1 x)) (* 3 (knf 0 x)))))
	      ((= l 1) (* .25s0 (+ (knf 3 x) (* -4 (knf 2 x)) (* 7 (knf 1 x)) (* -4 (knf 0 x)))))
	      (t (* .25s0 (+ (knf (+ l 2) x)
			     (* -4 (knf (+ l 1) x))
			     (* 6 (knf l x))
			     (* -4 (knf (- l 1) x))
			     (knf (- l 2) x))))))))



(declaim (optimize (speed 3) (safety 1) (debug 0)))

(def-interp bessel-j bessel-j-and-deriv)
(def-interp bessel-k bessel-k-and-deriv)
(def-interp bessel-k-scaled bessel-k-scaled-and-deriv)




#+nil
(progn
  (bessel-j-interp-init :start 0s0 :end 110s0 :n 100)
  (time (loop for x from 0s0 upto 98s0 by 1s-5 do (bessel-j-interp 4 x))))

; (/ 1815165956 (* 99 1e5)) => 183 cycles per call
; (/ 1386921624 (* 99 1e5)) => 140 cycles per call (single-float)
;    896260860   => 92 cycles per call (global variables, no argument in floor)
;    761730486   => 78 cycles per call (global variables, inline, division in floor)
;    428000000   => 45 cycles (truncate instead of floor, simplified calculation)
; (/ 172161426 (* 99 1e5)) => 17.4 cycles per call (global variables, no argument in floor, inline)
;    145631000   => 15 cycles (truncate, no calc in argument)
