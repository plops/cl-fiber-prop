(declaim (optimize (debug 3) (speed 0) (safety 3)))

;(require :cffi)
(require :gsll)

(defpackage :g (:use :cl gsll))
(in-package :g)

(declaim (ftype (function (fixnum double-float) (values double-float &optional)) jn yn))
(cffi:defcfun jn :double (n :int) (x :double))
(cffi:defcfun jnf :float (n :int) (x :float))

(defun bess-zeros (&key (a 1) (n 10) (d 1) (e 1e-5))
  (let ((res (make-array n :element-type 'double-float)))
    (loop for i from 1 upto n collect
	(setf (aref res (- i 1)) (gsll:bessel-zero-jnu (* 1d0 a) i)))
   res))

#+nil
(time (bess-zeros :a 0 :n 10))

(define-condition root-not-bracketed () ())

(defun zbrent (fun x1 x2 &optional (tol 1d-6) (itermax 100))
  "Find zero of function fun between x1 and x2. Reference: Numerical Recipes in C"
  (declare (type (function (double-float) (values double-float &optional)) fun)
	   (type double-float x1 x2 tol)
	   (type (integer 0 100000) itermax)
	   (values double-float &optional))
  (let* ((a x1) (b x2) (c x2) (d 0d0) (e 0d0)
	 (fa (funcall fun a)) (fb (funcall fun b)) (fc fb))
    (declare (type double-float a b c d e fa fb fc))
    (when (or (and (< 0 fa) (< 0 fb))
	      (and (< fa 0) (< fb 0)))
      (error 'root-not-bracketed))
    (loop for iter below itermax do
	 (when (or (and (< 0 fc) (< 0 fb))
		   (and (< fc 0) (< fb 0)))
	   (setf c a  fc fa  e (- b a)  d e))
	 (when (< (abs fc) (abs fb))
	   (setf a b b c c a)
	   (setf fa fb fb fc fc fa))
	 (let ((tol1 (+ (* .5 tol)
			(* 2 double-float-epsilon (abs b))))
	       (xm (* .5 (- c b))))      (declare (type double-float tol1 xm))
	   (when (or (<= (abs xm) tol1)
		     (= fb 0))
	     (return-from zbrent b))
	   (if (and (<= tol1 (abs e))
		    (< (abs fb) (abs fa)))
	       (let ((p 0d0) (q 0d0) (r 0d0) (s (/ fb fa))) (declare (type double-float p q r s))
		 (if (= a c)
		     (setf p (* 2 xm s)  q (- 1 s))
		     (setf q (/ fa fc)  r (/ fb fc)  p (* s (- (* 2 xm q (- q r))
							       (* (- b a) (- r 1))))
			   q (* (- q 1) (- r 1) (- s 1))))
		 (when (< 0 p) (setf q (- q)))
		 (setf p (abs p))
		 (if (< (* 2 p) (min (- (* 3 xm q) (abs (* tol1 q))) (abs (* e q))))
		     (setf e d  d (/ p q))
		     (setf d xm  e d)))
	       (setf d xm  e d))
	   (setf a b  fa fb)
	   (incf b (if (< tol1 (abs d)) d   (* (abs tol1) (signum xm))))
	   (setf fb (funcall fun b))))
    (error "maximum number of iterations exceeded")))

#+nil
(zbrent #'sin 1d0 4d0)

#+nil
(char-step-index-fiber 1e-9 89.54 32)

(defparameter *euler-m* 0.57721566490153286060651209008240243104215933593992d0)

(defun char-step-index-fiber (u v l)
  (declare (type (integer 0 1000000) l)
	   (type (double-float 0d0) u v))
  (let ((rad (- (* v v) (* u u))))
    (when (and (<= 0 rad) (< 0 u))
      (let ((posrad rad))
	(declare (type (double-float 0d0) posrad))
	(let ((w (sqrt posrad)))
	 (declare (type (double-float 0d0) w))
	 (return-from char-step-index-fiber 
	   (- (/ (* u (jn (+ l 1) u))
		 (jn l u))
	      (if (< w (* .1 (sqrt (+ l 1)))) 
		  (if (= l 0)
		      (/ -1d0 (+ (log (* w .5)) *euler-m*))
		      (* 2 l))
		  (/ (* w (gsll:cylindrical-bessel-k-scaled (+ l 1) w))
		     (gsll:cylindrical-bessel-k-scaled l w))))))))))

(defun step-fiber-eigenvalues (v)
  (let ((lmax (+ 1 (floor (* 2 V (/ pi)))))
	(mmax (ceiling (- (/ V pi) 1/4))))
    
    (when (< v (aref (bess-zeros :d 1 :a lmax :n 1 :e 1d-6) 0))
      (decf lmax))
    (when (< v (aref (bess-zeros :d 1 :a lmax :n 1 :e 1d-6) 0))
      (decf lmax))
    (setf mmax 
	  (count-if #'(lambda (x) (<= x v))
		    (bess-zeros :d 1 :a 0 :n mmax :e 1d-6)))
    (let* ((poles 
	    ;; there is one mode infront of each pole
	    (loop for l upto lmax collect
		 (let ((poles (loop for e across (bess-zeros :d 1 :a l :n mmax) 
				 while (<= e v)  collect e)))
		   (when poles (append '(1d0) poles (list v))))))
	   (modes (loop for us in poles and l from 0 collect
		       (loop for m from 1 below (length us) collect
			    (progn (format t "checking ~a~%" (list l (1- m) (elt us (1- m)) (elt us m)))
				   (let ((du 1d-9))
				     (handler-case 
					 (zbrent #'(lambda (x) (char-step-index-fiber x v l))
						 (+ (elt us (1- m)) du) (- (elt us m) du))
				       (root-not-bracketed ()))))))))
      ;; occasionally there is no mode in the gap between the last pole and v
      (remove-if #'null (mapcar #'(lambda (y) (remove-if #'(lambda (x) (null x)) y))
				modes)))))

#+nil
(time 
 (let ((count 0))
   (loop for e in (step-fiber-eigenvalues 243d0) do
	(loop for f in e do (incf count)))
   count)) ;; finding 6995 modes takes .924 s



#+nil
(mapcar #'(lambda (ls) (length ls)) 
	 (step-fiber-eigenvalues 20d0))
#+nil
(loop for v from 1d0 upto 40d0 by 4d0 collect
     (mapcar #'(lambda (ls) (length ls)) 
	     (step-fiber-eigenvalues v)))

(defun bigdelta (ncore ncladding)
  "Normally this should be 0.01 .. 0.03 for multimode fibers and
0.001..0.01 for singlemode fibers."
  (declare (type double-float ncore ncladding))
  (/ (- (expt ncore 2) (expt ncladding 2))
     (* 2 (expt ncore 2))))

(defun v (wavelength ncore ncladding core-radius)
  "The core-radius should be 12.5um .. 100um for multimode fibers or
2..5um for single mode fibers. Note that all dimensions are given in
mm."
  (declare (type (double-float 0d0) wavelength ncore ncladding core-radius))
  (* 2 pi (/ wavelength) core-radius (sqrt (- (expt ncore 2) (expt ncladding 2)))))

(defun numerical-aperture (ncore ncladding)
  (declare (type (double-float 0d0) ncore ncladding))
  (sqrt (- (expt ncore 2) (expt ncladding 2))))

#+nil
(numerical-aperture 1.5d0 1.46d0)
#+nil
(v .0005 1.5d0 .146d0 .005d0)
#+nil
(bigdelta 1.5d0 1.46d0)

(defun step-fiber-betas (v bigdelta core-radius u-modes)
  (mapcar #'(lambda (m-list) (mapcar #'(lambda (u) (* (/ core-radius) 
					    (sqrt (- (/ (expt v 2) 
							(* 2 bigdelta))
						     (expt u 2)))))
				m-list))
   u-modes))

(defun step-fiber-betas* (&key (wavelength .0005d0) (ncore 1.5d0) (ncladding 1.46d0) (core-radius .05d0) (debug nil))
  (let ((v (v wavelength ncore ncladding core-radius)))
    (if debug (break "~{~a=~3,3f ~}" (list 'v v 'bigdelta (bigdelta ncore ncladding) 'na (numerical-aperture ncore ncladding))))
    (step-fiber-betas v (bigdelta ncore ncladding) core-radius
		      (step-fiber-eigenvalues v))))

#+nil
(step-fiber-betas* :core-radius .003)

(defun step-fiber-neff (&key (wavelength .0005d0) (ncore 1.5d0) (ncladding 1.46d0) (core-radius .05d0))
  (let ((k0 (* 2 pi (/ wavelength))))
    (mapcar #'(lambda (beta-list) (mapcar #'(lambda (beta) (/ beta k0)) beta-list))
	   (step-fiber-betas* :wavelength wavelength :ncore ncore :ncladding ncladding :core-radius core-radius))))

#+nil
(step-fiber-neff :core-radius .003)
   
(defun step-fiber-field (u v l &key (n 100) (scale 1.3d0) (odd t) (debug nil))
  (declare (type double-float u v scale)
	   (type (integer 0 1000000) l n)
	   (values (simple-array double-float 2) &optional))
  (let* ((a (make-array (list n n) :element-type 'double-float))
	 (w (sqrt (- (expt v 2) (expt u 2))))
	 (nphi (* pi (if (= l 0) 2 1)))
	 (nrad (* (expt v 2) 
		  (/ (* 2 u u (expt (gsll:cylindrical-bessel-k-scaled l w) 2))) 
		  (gsll:cylindrical-bessel-k-scaled (- l 1) w)
		  (gsll:cylindrical-bessel-k-scaled (+ l 1) w)))
	 (norm (expt (* nphi nrad) -.5)))
    (when debug (break "~a" (list 'norm (/ norm) 'u u 'v v 'w w)))
    (dotimes (i n)
      (dotimes (j n)
	(let* ((x (* scale (- i (floor n 2)) (/ 1d0 n)))
	       (y (* scale (- j (floor n 2)) (/ 1d0 n)))
	       (r (sqrt (+ (expt x 2) (expt y 2)))))
	  (setf (aref a j i)
		(* norm
		   (if (= 0 l)
		       1d0
		       (if odd
			   (sin (* l (atan y x)))
			   (cos (* l (atan y x)))))
		   (if (<=  r 1d0)
		       (/ (jn l (* u r))
			  (jn l u))
		       (/ (gsll:cylindrical-bessel-k-scaled l (* w r))
			  (gsll:cylindrical-bessel-k-scaled l w))))))))
    a))

(defun number-of-modes (u-modes)
  (+ (length (car u-modes)) 
     (* 2 (reduce #'+ (mapcar #'length (cdr u-modes))))))

(define-condition mode-index-out-of-range () ())

(defun fiber-lm-to-linear-index (l m u-modes)
  (handler-case
      (unless (elt (elt u-modes (abs l)) m)
     (break "No mode with index l=~a m=~a." l m))
    (sb-kernel:index-too-large-error ()
      (break "No mode with index l=~a m=~a." l m)))
  (let* ((nmodl (mapcar #'length u-modes))
	 (j (cond 
	      ((= l 0) m)
	      ((= l 1) (+ m (elt nmodl 0)))
	      ((= l -1) (+ m (elt nmodl 0) (elt nmodl 1)))
	      ((< 1 l) (+ m (elt nmodl 0)
			  (* 2 (reduce #'+ (subseq nmodl 1 l)))))
	      ((< l -1) (+ m (elt nmodl 0)
			   (* 2 (reduce #'+ (subseq nmodl 1 (abs l))))
			   (elt nmodl (abs l)))))))
    (unless (<= 0 j (1- (number-of-modes u-modes)))
      (error 'mode-index-out-of-range))
    j))

#+nil
(fiber-lm-to-linear-index 0 5 (step-fiber-eigenvalues 12d0))

(defun fiber-linear-to-lm-index (j u-modes)
  (let ((res (make-array (number-of-modes u-modes))))
    (loop for ul in u-modes and l from 0 do
	 (loop for um in ul and m from 0 do
	      (setf (aref res (fiber-lm-to-linear-index l m u-modes))
		    (list l m))
	      (unless (= l 0)
		(setf (aref res (fiber-lm-to-linear-index (- l) m u-modes))
		      (list (- l) m)))))
    (aref res j)))

#+nil
(fiber-linear-to-lm-index 0 *bla-ev*)

(defun step-fiber-eigenvalues-linear (v-or-u-modes)
  (let* ((u-modes (if (numberp v-or-u-modes)
		      (step-fiber-eigenvalues v-or-u-modes) 
		      v-or-u-modes))
	 (nmodes (number-of-modes u-modes))
	 (a (make-array nmodes :element-type 'double-float)))
    (dotimes (i nmodes)
      (destructuring-bind (l m) (fiber-linear-to-lm-index i u-modes)
       (setf (aref a i) (elt (elt u-modes (abs l)) m))))
    a))

#+nil
(step-fiber-eigenvalues-linear 32d0)

(defun step-fiber-betas-linear (u-modes-lin v &key  (nco 1.5d0) (bigdelta (bigdelta nco 1.46)) (lambd .0005d0))
  (declare (type double-float v nco bigdelta lambd)
	   (type (simple-array double-float 1) u-modes-lin)
	   (values (simple-array double-float 1) &optional))
  (let* ((betas (make-array (length u-modes-lin) :element-type 'double-float))
	(na2 (* 2 (expt nco 2) bigdelta))
	(k (* 2 pi (/ lambd)))
	(rho (* v (/ (* k (sqrt na2))))))
    (dotimes (i (length betas))
      (setf (aref betas i) (* (/ rho) (sqrt (- (/ (expt v 2) (* 2 bigdelta))
					     (expt (aref u-modes-lin i) 2))))))
    betas))
#+nil
(let ((v 32d0))
  (sort
   (step-fiber-betas-linear (step-fiber-eigenvalues-linear v) v :bigdelta (bigdelta 1.5 1.46))
   #'<))

(defun find-fastest-mode (u-modes-lin)
  (first (sort (map 'list #'(lambda (u i) (list u i)) u-modes-lin
		    (loop for i below (length u-modes-lin) collect i)) #'> :key #'first)))

#+nil
(find-fastest-mode (step-fiber-eigenvalues-linear 12d0))


(defun step-fiber-minimal-sampling (u-modes v &key (n 128) (scale 1d0))
  ;; largest u will show most oscillations in the field
  " n is number of points between R=0 .. 1, returns the minimal n that
covers -scale*R .. scale*R and still ensures sampling of the signal"
  (destructuring-bind (u lin-index) (find-fastest-mode (step-fiber-eigenvalues-linear u-modes)) 
    (destructuring-bind (l m) (fiber-linear-to-lm-index lin-index u-modes)
      (let* ((field (make-array (list n) :element-type 'double-float)))
	(dotimes (i n)
	  (let ((r (* i (/ 1d0 n))))
	    (setf (aref field i) (/ (jn l (* u r)) (jn l u)))))
	(let* ((ma (loop for i from 1 below (1- n) ;; find local maxima
		   when (and (< (aref field (+ i 1)) (aref field i))
			     (< (aref field (- i 1)) (aref field i)))
		     collect i))
	       (dists (loop for i below (1- (length ma)) collect (* (/ 1d0 n) (- (elt ma (+ 1 i)) (elt ma i))))))
	  (ceiling (* 2 2 2 scale (/ (if dists (reduce #'min dists) .6d0)))) ;; note that you need more sampling for intensity
	  )))))

#+nil
(loop for v from 4d0 upto 40d0 by 5 collect
 (let* (;(v 1d0)
	(us (step-fiber-eigenvalues v)))
   (step-fiber-minimal-sampling us v :n 256 :scale 1.4)))

(declaim (inline sin-fast))
(defun sin-fast (x)
  (declare (type single-float x) (values single-float &optional))
  (declare (optimize (debug 0) (speed 3) (safety 1)))
  (let ((c1 0.9999966f0) (c3 -0.16664815f0) (c5 0.008306204f0) (c7 -1.8360789f-4) 
	(x2 (* x x)))
    #+nil (+ (* x c1)
       (* (expt x 3) c3)
       (* (expt x 5) c5)
       (* (expt x 7) c7))
    (* x (+ c1 (* c3 x2)
	    (* x2 x2 (+ c5 (* c7 x2)))))))

(let* ((n 10000000)
       (u (* .5f0 (coerce pi 'single-float)))
       (du (/ 1f0 n)))
  (declare (type single-float u du))
  (defun sin-test1 ()
    (declare (optimize (debug 0) (speed 3) (safety 1)))
    (loop for x from (- u) upto u by du do
	 (sin x)))
  (defun sin-test2 ()
    (declare (optimize (debug 0) (speed 3) (safety 1)))
    (loop for x from (- u) upto u by du do
	 (sin-fast x)))
  (defun sin-diff ()
    (* (/ 1d0 n) (loop for x from (- u) upto u by du sum
	  (- (sin-fast x) (sin x))))))

#+nil
(let* ((n 10000)
       (u (* .5f0  (coerce pi 'single-float)))
       (du (/ 1f0 n)))
  (with-open-file (s "/run/q/bla.dat" :direction :output :if-exists :supersede :if-does-not-exist :create)
   (loop for x from (- u) upto u by du do
	(format s "~f ~f~%" x (- (sin x) (sin-fast x))))))
#+nil
(time 
 (sin-test2))
;; .195 vs .054 at .1 for n=10000000


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

(defmacro def-interp (name fun)
  `(let* ((j-n 100)
	  (lmax 10)
	  (j-start 0s0)
	  (j-end 100s0)
	  (j (make-array (list lmax j-n 2) :element-type 'single-float
			 :initial-element 0s0)))
     (declare (type single-float j-start j-end)
	      (type fixnum lmax j-n)
	      (type (simple-array single-float 3) j))
     (defun ,(intern (format nil "~a-INTERP-INIT" name))
	 (&key (start 0s0) (end 10s0) (n 100))
       (declare (optimize (debug 3) (speed 3) (safety 1)))
       (setf j-start start
	     j-end end
	     j-n n
	     j (make-array (list lmax j-n 2) :element-type 'single-float
			   :initial-element 0s0))
       (dotimes (l lmax)
	 (dotimes (i n)
	   (let ((x (+ start (* (- end start) i (/ 1s0 n)))))
	     (declare (type single-float x))
	     (multiple-value-bind (y yy) (,fun l x)
	       (setf (aref j l i 0) y
		     (aref j l i 1) yy))))))
     (defun ,(intern (format nil "~a-INTERP" name)) (l x)
       (declare (type (integer 0 10000) l)
		(type single-float x)
		(values single-float &optional))
       (declare (optimize (debug 0) (speed 3) (safety 0)))
       (multiple-value-bind (i xx) (floor (* j-n (/ (- x j-start)
						    (- j-end j-start))))

	 (declare (type (signed-byte 64) i)
		  (type single-float xx))
	 (let* ((diff (* (- j-end j-start)
			 (/ 1s0 j-n)))
		(a (- 1 xx))
		(b xx)
		(c (* 1/6 (- (* a a a) a) ))
		(d (* 1/6 (- (* b b b) b) )))
	   (+ (* a  (aref j l i 0))
	      (* b (aref j l (+ i 1) 0))
	      (* c (aref j l i 1) diff diff)
	      (* d (aref j l (+ i 1) 1) diff diff)))))))

(def-interp bessel-j bessel-j-and-deriv)

#+nil
(let ((j (make-instance 'cubic-interp)))
 (bessel-j-fast-init :start 0s0 :end 100s0 :n 100)
 (with-open-file (s "/run/q/bla.dat" :direction :output :if-exists :supersede :if-does-not-exist :create)
   (loop for x from 0s0 upto 80s0 by .001s0 do
	(format s "~f ~f~%" x (+ (* -1 (jnf 4 x)) #+nil (interp j 4 x) (* 0 (bessel-j-fast 4 x)))))))

#+nil
(progn
  (bessel-j-interp-init :start 0s0 :end 110s0 :n 100)
  (time (loop for x from 0s0 upto 99s0 by 1s-5 do (bessel-j-interp 4 x))))
; (/ 1815165956 (* 99 1e5)) => 183 cycles per call
; (/ 1386921624 (* 99 1e5)) => 140 cycles per call (single-float)

#+nil
(time (loop for x from 0d0 upto 99d0 by 1e-4 do (jn 4 x)))
; (/ 996556092 (* 99 1e4)) => 1006 cycles per call
(defun step-fiber-fields (u-modes v &key (scale 1.3d0) (n (step-fiber-minimal-sampling u-modes v :scale scale)) (debug nil))
  (declare (values (simple-array double-float 3) &optional))
  (let* ((radial-mode-counts (mapcar #'length u-modes))
	 (azimuthal-mode-count (length radial-mode-counts))
	 (fields  (make-array (list (number-of-modes u-modes) n n) :element-type 'double-float))
	 (r-a (make-array (list n n) :element-type 'double-float)) ;; some arrays that store reusable intermediate results
	 (phi-a (make-array (list n n) :element-type 'double-float))
	 (sin-a (make-array (list (- azimuthal-mode-count 1) n n) :element-type 'double-float))
	 (cos-a (make-array (list (- azimuthal-mode-count 1) n n) :element-type 'double-float)))
    (macrolet ((doplane ((j i) &body body) `(dotimes (,j n) (dotimes (,i n) ,@body))))
      (doplane (j i) (let* ((x (* 2 scale (- i (floor n 2)) (/ 1d0 n)))
			    (y (* 2 scale (- j (floor n 2)) (/ 1d0 n)))
			    (r (sqrt (+ (expt x 2) (expt y 2)))))
		       (setf (aref r-a j i) r   (aref phi-a j i) (atan y x))))
      (loop for l from 1 below  azimuthal-mode-count do
	   (if debug (format t "azimuthal ~d/~d~%" l azimuthal-mode-count))
	   (doplane (j i) (setf (aref sin-a (1- l) j i) (sin (* l (aref phi-a j i)))))
	   (doplane (j i) (setf (aref cos-a (1- l) j i) (cos (* l (aref phi-a j i))))))
      (let ((start (get-universal-time)))
       (loop for k below (number-of-modes u-modes) do
	    (when (and debug (= 0 (mod k 10))) (let* ((current (- (get-universal-time) start))
						      (perfield (* (/ 1d0 (if (= 0 k) 1d0 k)) current)))
						 (format t "calculating mode ~d/~d avg-time=~3,3f s per field, finished in ~3,3f s full calculation time ~3,3f ~%" 						
							 k (number-of-modes u-modes)
							 perfield
							 (* perfield (- (number-of-modes u-modes) k))
							 (* perfield (number-of-modes u-modes)))))
	    (destructuring-bind (l m) (fiber-linear-to-lm-index k u-modes)
	      (let* ((u (elt (elt u-modes (abs l)) m))
		     (w (sqrt (- (expt v 2) (expt u 2))))
		     (nphi (* pi (if (= l 0) 2 1)))
		     (nrad (* (expt v 2) 
			      (/ (* 2 u u (expt (gsll:cylindrical-bessel-k-scaled l w) 2))) 
			      (gsll:cylindrical-bessel-k-scaled (- l 1) w)
			      (gsll:cylindrical-bessel-k-scaled (+ l 1) w)))
		     (norm (expt (* nphi nrad) -.5)))
		(doplane (j i) (setf (aref fields k j i)
				     (* norm (cond ((= l 0) 1d0) 
						   ((< l 0) (aref sin-a (- (abs l) 1) j i))
						   (t (aref cos-a (- l 1) j i)))
					(let ((r (aref r-a j i)))
					  (if (<= r 1d0)
					      (/ (jn l (* u r)) (jn l u))
					      (/ (gsll:cylindrical-bessel-k-scaled l (* w r))
						 (gsll:cylindrical-bessel-k-scaled l w))))))))))))
    fields))


#+nil
(time 
 (progn
   (defparameter *bla* nil)
   (defparameter *bla*
     (let ((v 94d0)
	   (start (get-time-of-day)))
       (format t "calculating eigenvalues~%")
       (defparameter *bla-ev* (step-fiber-eigenvalues v)) 
       (format t "ev took ~3d s time~%" (- (get-time-of-day) start))
       (step-fiber-fields *bla-ev* v :debug t)))
   (write-pgm "/run/q/bla.pgm" (convert-ub8  (create-field-mosaic *bla* *bla-ev* :fun #'identity) :scale .9 :offset -.2d0))))



(defun create-field-mosaic (fields u-modes &key (fun #'(lambda (x) (expt x 2))))
  (declare (type (simple-array double-float 3) fields)
	   (values (simple-array double-float 2) &optional))
  (let* ((lmax (length u-modes))
	 (mmax (length (first u-modes)))
	 (n (array-dimension fields 2))
	 (a (make-array (list (* (+ lmax (1- lmax)) n)  (* mmax n)) :element-type 'double-float)))
    (loop for k below (number-of-modes u-modes) do
	 (destructuring-bind (l m) (fiber-linear-to-lm-index k u-modes)
	   (dotimes (j n) (dotimes (i n)
			    (setf (aref a (+ j (* n (+ (- lmax 1) l))) (+ i (* n m)))   (funcall fun (aref fields k j i)))))))
    a))


(defun convert-ub8 (a &key (scale 1d0 scale-p) (offset 0d0) (debug nil))
  (declare (type (simple-array double-float 2) a)
	   (type double-float scale)
	   (values (simple-array (unsigned-byte 8) 2) &optional))
  (let ((b (make-array (array-dimensions a)
		       :element-type '(unsigned-byte 8)))
	(scale2 scale)
	(offset2 offset))
    (unless scale-p
      (let ((ma (reduce #'max (sb-ext:array-storage-vector a)))
	    (mi (reduce #'min (sb-ext:array-storage-vector a))))
	(setf scale2 (if (= ma mi)
			 1d0
			 (/ (- ma mi)))
	      offset2 mi)
	(when debug
	 (break "~a" (list 'scale scale2 'offset offset2 'max ma)))))
    (destructuring-bind (h w) (array-dimensions a)
      (dotimes (i w)
	(dotimes (j h)
	  (setf (aref b j i) (min 255 (max 0 (floor (* 255 scale2 (- (aref a j i) offset2)))))))))
    b))

(defun convert-df (a &key (fun #'abs))
  (declare (type (simple-array (complex double-float) 2) a)
	   (values (simple-array double-float 2) &optional))
  (let ((b (make-array (array-dimensions a)
		       :element-type 'double-float)))
    (destructuring-bind (h w) (array-dimensions a)
      (dotimes (i w)
	(dotimes (j h)
	  (setf (aref b j i) (funcall fun (aref a j i))))))
    b))

(defun write-pgm (filename img)
  (declare (simple-string filename)
           ((array (unsigned-byte 8) 2) img)
           (values null &optional))
  (destructuring-bind (h w)
      (array-dimensions img)
    (declare ((integer 0 65535) w h))
    (with-open-file (s filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%255~%" w h))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :append)
      (let ((data-1d (make-array 
                      (* h w)
                      :element-type '(unsigned-byte 8)
                      :displaced-to img)))
        (write-sequence data-1d s)))
    nil))

;; http://mathoverflow.net/questions/28669/numerical-integration-over-2d-disk
;; Arthur Stroud, Approximate Calculation of Multiple Integrals.
;; http://people.sc.fsu.edu/~jburkardt/f_src/stroud/stroud.html
;; cubatur for the unit circle
;; 1971 lether a generalized product rule for the unit cirlce
;; http://www.holoborodko.com/pavel/numerical-methods/numerical-integration/cubature-formulas-for-the-unit-disk/

(defun calculate-bend-wedge (&key (v 32d0) (n 100) (scale 2d0))
 (let* ((lambd .0005)
	(nco 1.5)
	(ncl 1.46)
	(k (* 2 pi (/ lambd))) 
	;; diameter of the fiber:
	(rho (* v (/ (* k (sqrt (- (expt nco 2) (expt ncl 2)))))))
	;; resolution of the field in mm/px:
	(resol (/ (* 2 scale rho) n))
	(l2 40d0)
	(delx 4)
	(bend-radius (* .5 (+ delx (/ (expt l2 2) delx))))
	(num-elems 100)
	(del-l (/ l2 num-elems))
	(phi (asin (/ del-l bend-radius)))
	(wedge (make-array (list n n) :element-type '(complex double-float))))
   (dotimes (i n)
     (dotimes (j n)
       (setf (aref wedge j i) (exp (complex 0d0 (* nco k phi i resol)))))) ;; is this supposed to be free-space?
   (values wedge resol)))
#+nil
(calculate-bend-wedge)

(defun calculate-couple-coeffs (fields)
  ;; i might have to figure out the proper sampling by calculating a
  ;; high resolution cross section through the highest mode
  (multiple-value-bind (wedge resol) (calculate-bend-wedge)
    (destructuring-bind (nmodes n nx) (array-dimensions fields)
      (declare (ignore nx))
      (let* ((couple-coeffs (make-array (list nmodes nmodes)
					:element-type '(complex double-float))))
	(declare (type (simple-array (complex double-float) 2) couple-coeffs))
	(dotimes (a nmodes)
	 (dotimes (b nmodes)
	   (format t "~a ~%" (list 'couple a b))
	   (setf (aref couple-coeffs b a) 
		 (* (expt resol 2)
		    (loop for j below n sum
			 (loop for i below n sum
			      (* (aref fields a j i)
				 (aref fields b j i)
				 (aref wedge j i))))))))
       couple-coeffs))))

#+nil
(time (defparameter *bla-coef* (calculate-couple-coeffs)))

#+nil
(time  (write-pgm "/run/q/bla-coef.pgm" (convert-ub8  (convert-df *bla-coef*))))
#+nil
(time  (write-pgm "/run/q/bla-coef2.pgm" (convert-ub8  (convert-df *bla-coef*) :scale 1e5)))
#+nil
(time  (write-pgm "/run/q/bla-coef-phase.pgm" (convert-ub8  (convert-df *bla-coef* :fun #'phase))))



;; page 432 mode launching, fields reflected from the end surface are
;; extermely complicated, but there is a simple approximation for
;; weakly guiding fibers
;; incident beam is tilted towards fiber axis: ni (sin thetai) = nco (sin thetaz)
;; fresnel reflection: Et(thetaz) = 2ni/(nco+ni) Ei(thetai)
;; in the rest of the chapter they assume ni=nco (!)

 
;; levin transform, oscillatory integral 5.3.24
;; 13.9


;; shemirani 2009y
;; Due to symmetries enforced by the bends in and directions, it is
;; easiest to find the coupling coefficients in Carte- sian
;; coordinates, using the eigenmodes of the ideal fiber, which are
;; orthonormal Hermite–Gaussian function


(defun solve-couple-into-lp-modes (matrix)
  "Solve the linear equation using SVD with the supplied matrix and
   a right-hand side vector which is the reciprocal of one more than
   the index."
  (let ((dim (dim0 matrix)))
    (multiple-value-bind (u q d)
        (SV-decomposition (copy matrix))
      (SV-solve u q d (gsll::create-rhs-vector dim)))))
