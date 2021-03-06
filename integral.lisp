(require :gsll)
(require :cl-opengl)
(require :cl-glfw)
(require :cl-glu)

(defpackage :g (:use :cl :gl))
(in-package :g)

(defun compute-integral (r theta l &key (cplx nil) (trig :cosine))
  (declare (type (double-float 0d0) r)
	   (type double-float l)
	   (type (double-float 0d0 #.(* 2 pi)) theta)
	   (optimize (speed 3)))
 (flet ((integrand (x)
	  (declare (type (double-float 0d0 #.(* 2 pi)) x))
	  (let ((arg (* r 
			(+ (* (sin theta) (cos x))
			   (* (cos theta) (sin x))))))
	    (if cplx (sin arg) (cos arg)))))
   (if (= l 0d0)
       (gsll::integration-qag  #'integrand 0d0 (* 2 pi) 6)
       (gsll::integration-qawo #'integrand
			       0d0 (* 1d0 l) (* 2 pi) trig 30))))

#+nil
(compute-integral 5.2d0 .1d0 4d0)

(defun compute-integral-xy (x y l &key (cplx nil) (trig :cosine))
  (declare (type double-float x y l)
	   (optimize (speed 3)))
  (let ((r (sqrt (+ (expt x 2) (expt y 2))))
	(theta (atan y x)))
   (compute-integral r theta l :cplx cplx :trig trig)))

#+nil
(time
 (defparameter *data*
   (let* ((w 4)
	  (h 80)
	  (ln 40)
	  (a (make-array (list ln h w) :element-type 'double-float)))
     (declare (optimize (speed 3))
	      (type (simple-array double-float 3) a))
     (flet ((calc-l (a l bl)
	      (declare (type (simple-array double-float 3) a)
		       (type fixnum l bl))
	      (dotimes (i w)
		(dotimes (j h)
		  (let ((x (* 30 2d0 (/ 1d0 w) (- i (/ w 2))))
			(y (* 30 2d0 (/ 1d0 h) (- j (/ h 2)))))
		    (setf (aref a bl j i)
			  (compute-integral-xy x y (* 1d0 l) 
					       :cplx nil :trig :sine)))))))
       (let* ((procn 1)
	      (chunk (floor ln procn))
	      (arrays (loop for p below procn collect 
			   (make-array (list chunk h w)
				       :element-type 'double-float)))
	      (threads (loop for proc fixnum below 3 #+nil procn collect
			    (sb-thread:make-thread
			     #'(lambda (proc chunk)
				 (loop for l fixnum from (* proc chunk)
				    below (* (+ proc 1) chunk) and bl from 0 do
				      (when (< l ln) (calc-l (nth proc arrays) l bl))))
			     :name (format nil "calc-~d/~d" proc procn) 
			     :arguments (list proc chunk)))))
	 #+nil(loop for bl fixnum below (- ln (* procn chunk)) ;; calculate the rest
	    and l from (* procn chunk) do (calc-l a l l))
	 (declare (type fixnum procn chunk))
	 (dolist (th threads)
	   (sb-thread:join-thread th))
	 (dotimes (p procn)
	   (let ((b (nth p arrays)))
	     (declare (type (simple-array double-float 3) b))
	     (loop for l fixnum from (* p chunk) below (* (+ p 1) chunk)
		and bl fixnum from 0 do
		  (when (< l ln)
		    (dotimes (i w)
		      (dotimes (j h)
			(setf (aref a l j i) (aref b bl j i)))))))))
       a))))
(defparameter *data* nil)
(defparameter *data-cos* nil)

(cffi:defcfun jn :double (n :int) (x :double))
#+nil
(defparameter *data-cos*
   (let* ((w 50)
	  (h 80)
	  (ln 40)
	  (trig :sine)
	  (cplx nil)
	  (a (make-array (list ln h w) :element-type 'double-float)))
     (dotimes (l ln)
       (dotimes (i w)
	 (dotimes (j h)
	   (let* ((rmax 30d0)
		  (x (* rmax 2d0 (/ 1d0 w) (- i (/ w 2))))
		  (y (* rmax 2d0 (/ 1d0 h) (- j (/ h 2))))
		  (r (sqrt (+ (expt x 2) (expt y 2))))
		  (phi (atan y x)))
	     (setf (aref a l j i) 
		   (if cplx
		       (if (eq trig :cosine)
			   (if (= (mod l 2) 0)
			       0d0
			       (* 2 pi (sin (* l phi)) (jn l r)))
			   (if (= (mod l 2) 0)
			       0d0
			       (* 2 pi (cos (* l phi)) (jn l r))))
		       
		       (if (eq trig :cosine)
			   (if (= (mod l 2) 1)
			       0d0
			       (* 2 pi (cos (* l phi)) (jn l r)))
			   (if (= (mod l 2) 1)
			       0d0
			       (* -2 pi (if (= l 0)
					   -1d0
					   (sin (* l phi)))
				  (jn l r))))))))))
     a))



(let ((l 2))
  (defun draw ()
    (declare (optimize (debug 3)))
    (progn
      (clear-color 0 0 0 1)
      (enable :depth-test)
      (clear :depth-buffer-bit))
    (sleep (/ 32))
    (rotate -90 1 0 0)
    (rotate 40 1 0 0)
    (rotate (+ 80 (* 7 (sin (* 2 (glfw:get-time))))) 0 0 1)
					;(rotate (* 5 (glfw:get-time)) 0 0 1)
					;(rotate -25 0 0 1)

    (color 1 1 1)
    (translate 0 0 2.4)
    (let ((off .1) (sc .04) (s .4d0))
     (loop for l below 10  by 2 do
	  (translate 0 0 -.8)
	  (let ((data *data-cos*))
	    (when data
	      (destructuring-bind (ln h w) (array-dimensions data)
		(progn
		  ;		  (incf l 2)
		  (when (<= ln l)
		    (setf l 0)))
		(when (< l ln)
		  (polygon-mode :front-and-back :fill)
		  (color .1 .01 0 1)
		  (dotimes (i w) 
		    (let ((x (* s 2d0 (/ 1d0 w) (- i (/ w 2)))))
		      (with-primitive :triangle-strip
			(dotimes (j h)
			  (let ((y (* s 2d0 (/ 1d0 h) (- j (/ h 2)))))
			    (vertex x y (+ off (* sc (aref data l j i))))
			    (vertex x y 0))))))
		  (dotimes (j h) 
		    (let ((y (* s 2d0 (/ 1d0 h) (- j (/ h 2)))))
		      (with-primitive :triangle-strip
			(dotimes (i w)
			  (let ((x (* s 2d0 (/ 1d0 w) (- i (/ w 2)))))
			    (vertex x y (+ off (* sc (aref data l j i))))
			    (vertex x y 0))))))
		  (color .8 .2 .2)
		  (line-width 1.2)
		  (dotimes (i w)
		    (with-primitive :line-strip
		      (dotimes (j h)
			(let ((x (* s 2d0 (/ 1d0 w) (- i (/ w 2))))
			      (y (* s 2d0 (/ 1d0 h) (- j (/ h 2)))))
			  (vertex x y (+ off .0  (* sc (aref data l j i))))))))))))

	  (when *data*
	    (let ((data *data*))
	      (declare (type (simple-array double-float 3) data))
	      (destructuring-bind (ln h w) (array-dimensions data)
		(color 1 1 1)
		(line-width 1.2)
		(enable :blend :line-smooth)

		(blend-func :src-alpha
			    :one-minus-src-alpha)
		(when (< l ln)
		  (dotimes (i w)
		    (with-primitive :line-strip
		      (dotimes (j h)
			(let ((x (* s 2d0 (/ 1d0 w) (- i (/ w 2))))
			      (y (* s 2d0 (/ 1d0 h) (- j (/ h 2)))))
			  (progn     ;when (< (+ (* x x) (* y y)) 1d0)
			    (vertex x y (+ off .001 (* sc (aref data l j i)))))))))))))))))

#+nil
(defparameter *gl-display-thread* 
  (sb-thread:make-thread #'run-window :name "gl-display-thread"))

(defun keyfun (key action) 
  (cond 
    ((not (eql action glfw:+press+)))
    ((eql key #\Q)
     (glfw:close-window))))

(defun run-window ()
 (flet ((reset-perspective ()
	  (gl:matrix-mode :projection)
	  (gl:load-identity)
	  (destructuring-bind (w h) (glfw:get-window-size)
	    (viewport 0 0 w h)
	    (unwind-protect (glu:perspective 45 (/ w h) 0.1 50)
	      (gl:matrix-mode  :modelview)))))
   (glfw:do-window (:title "A Simple Example" :depthbits 24)
       ((glfw:set-key-callback #'keyfun)
	(reset-perspective))
     (sleep (/ 30s0))
     (reset-perspective)
     (clear :color-buffer-bit)
     (load-identity)
     (translate 0 0 -5)
     (draw))))

#+nil
(time
 (defparameter *int*
   (let (res
	 (rmax 20d0)
	 (ymax (* 2d0 pi))
	 (lmax 8))
     (loop for trig in '(:cosine :sine) do
	  (loop for r from 0d0 upto rmax by (/ rmax 30) do
	       (loop for y from 0 upto ymax by (/ ymax 30) do
		    (loop for l from 0 upto lmax do
			 (flet ((integrand (x)
				  (cos (* r 
					  (+ (* (sin y) (cos x)))
					  (+ (* (cos y) (sin x)))))))
			   (push (append (list trig r y l)
					 (if (= l 0)
					     (multiple-value-list (gsll::integration-qng  #'integrand 0d0 (* 2 pi) 1e-3 1e-3))
					     (multiple-value-list (gsll::integration-qawo #'integrand
											  0d0 (* 1d0 l) (* 2 pi) trig 10 1e-3 1e-3))))
				 res))))))
     (reverse res))))



#+nil
(time
 (defparameter *vals*
   (let* ((rmax 14d0) (rn 60)
	  (ymax (* (/ 8d0) pi)) (yn 7)
	  (ln 20)
	  (res (make-array (list rn yn ln 2 2 2) :element-type 'double-float)))
     (loop for trig in '(:cosine :sine) and tri from 0 do
	  (loop for cplx from 0 upto 1 do
	   (loop for r from 0d0 upto rmax by (/ rmax (1- rn)) and ri from 0 do
		(loop for y from 0 upto ymax by (/ ymax (1- yn)) and yi from 0 do
		     (loop for l from 0 below ln do
			  (flet ((integrand (x)
				   (let ((arg (* r 
						 (+ (* (sin y) (cos x))
						    (* (cos y) (sin x))))))
				     (if (= 0 cplx) (sin arg) (cos arg)))))
			    (multiple-value-bind (val err &optional msg) 
				(if (= l 0)
				    (gsll::integration-qag  #'integrand 0d0 (* 2 pi) 6)
				    (gsll::integration-qawo #'integrand
							    0d0 (* 1d0 l) (* 2 pi) trig 30))
			      (declare (ignore msg))
			      (setf (aref res ri yi l tri cplx 0) val
				    (aref res ri yi l tri cplx 1) err))))))))
     res)))

#+nil
(destructuring-bind (rn yn ln trign valn) (array-dimensions *vals*)
  (loop for r below rn maximize
       (loop for y below yn maximize
	    (loop for l below ln maximize
		 (loop for tri below trign maximize
		      (abs (aref *vals* r y l tri 1)))))))

#+nil
(plot :l 11)

#+nil
(defun plot (&key (l 0))
  (let ((rmax 14d0)
	(cols 0))
    (destructuring-bind (rn yn ln trign valn cplxn) (array-dimensions *vals*)
     (with-open-file (s "p.dat" :direction :output :if-exists :supersede :if-does-not-exist :create)
       (loop for r below rn do
	    (format s "~30,12,3g " (* rmax r (/ 1d0 rn)))
	    (loop for cplx below cplxn do
	     (loop for tri below trign do
		  (loop for y below yn do
		       (format s "~30,12,3g "  (aref *vals* r y l tri cplx 0))
		       (when (= r 0) (incf cols)))))
	    (terpri s)))
     (with-open-file (s "p.gp" :direction :output :if-exists :supersede :if-does-not-exist :create)
       (format s "unset key; set outp \"p.pdf\"; set term pdf; plot")
       (loop for tri below trign do
	    (loop for cplx below cplxn do
	     (loop for y below yn do
		  (let ((col (+ cplx (* cplxn (+ y (* tri yn))))))
		    (format s " \"p.dat\" u 1:~d w l~c"
			    (+ 2 col)
			    (if (= col (1- cols)) #\; #\,))))))))))

#+nil
(plot (loop for (trig r y l val err &optional msg) in *int* collect
     (list r val)))

#+nil
(defun plot (dat)
  (with-open-file (s "p.dat" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format s "~{~{~30g ~}~%~}" dat))
  (with-open-file (s "p.gp" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format s "set outp \"p.pdf\"; set term pdf; plot \"p.dat\" u 1:2;")))


