#+nil
(progn
  (setf asdf:*central-registry*
	'(*default-pathname-defaults*
	  #P"/media/sda6/home/martin/quicklisp/quicklisp/"))
  (require :cl-fiber-prop))


(in-package :cl-fiber-prop)

(defun bess-zeros (&key (a 1) (n 10) (d 1) (e 1e-5))
  (let ((res (make-array n :element-type 'double-float)))
    (loop for i from 1 upto n collect
	(setf (aref res (- i 1)) (gsll:bessel-zero-jnu (* 1d0 a) i)))
   res))

#+nil
(time (bess-zeros :a 0 :n 10))

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

(defun step-fiber-fields (u-modes v &key (scale 1.3d0) (n (step-fiber-minimal-sampling u-modes v :scale scale)) (debug nil))
  (declare (values (simple-array double-float 3) &optional))
  (let* ((radial-mode-counts (mapcar #'length u-modes))
	 (azimuthal-mode-count (length radial-mode-counts))
	 (fields  (make-array (list (number-of-modes u-modes) n n) :element-type 'double-float))
	 (r-a (make-array (list n n) :element-type 'double-float)) ;; some arrays that store reusable intermediate results
	 (phi-a (make-array (list n n) :element-type 'double-float))
	 (sin-a (make-array (list (- azimuthal-mode-count 1) n n) :element-type 'double-float))
	 (cos-a (make-array (list (- azimuthal-mode-count 1) n n) :element-type 'double-float))
	 (umax (first (find-fastest-mode (step-fiber-eigenvalues-linear u-modes))))
	 (wmin (sqrt (- (* v v) (* umax umax))))
	 )
    (macrolet ((doplane ((j i) &body body) `(dotimes (,j n) (dotimes (,i n) ,@body))))
      (doplane (j i) (let* ((x (* 2 scale (- i (floor n 2)) (/ 1d0 n)))
			    (y (* 2 scale (- j (floor n 2)) (/ 1d0 n)))
			    (r (sqrt (+ (expt x 2) (expt y 2)))))
		       (setf (aref r-a j i) r   (aref phi-a j i) (atan y x))))
      (loop for l from 1 below  azimuthal-mode-count do
	   (if debug (format t "azimuthal ~d/~d~%" l azimuthal-mode-count))
	   (doplane (j i) (setf (aref sin-a (1- l) j i) (sin (* l (aref phi-a j i)))))
	   (doplane (j i) (setf (aref cos-a (1- l) j i) (cos (* l (aref phi-a j i))))))
      (bessel-j-interp-init :end (* 1.01 v) :n 100 :lmax azimuthal-mode-count)
      (bessel-k-scaled-interp-init :start (* .99 wmin) :end (* 1.01 (sqrt 2) scale v) :n 100 :lmax azimuthal-mode-count)
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
		     (norm (expt (* nphi nrad) -.5))
		     (scale-j (/ (jn l u)))
		     (scale-k (/ (gsll:cylindrical-bessel-k-scaled l w))))
		(doplane (j i) (setf (aref fields k j i)
				     (* norm (cond ((= l 0) 1d0) 
						   ((< l 0) (aref sin-a (- (abs l) 1) j i))
						   (t (aref cos-a (- l 1) j i)))
					(let ((r (aref r-a j i)))
					  (if (<= r 1d0)
					      (* scale-j (bessel-j l (* u r)))
					      (* scale-k (bessel-k-scaled-interp (abs l) (* w r)))))))))))))
    fields))
#+nil
(sb-ext:gc :full t)

#+nil
(time 
 (progn
   (defparameter *bla* nil)
   (defparameter *bla*
     (let ((v 40d0) ;; 32 took 1.95s with direct bessel, takes 1.49s with lookup, 96.7s for v=94 with .017s per field
	   (start (sb-unix::get-time-of-day)))
       (format t "calculating eigenvalues~%")
       (defparameter *bla-ev* (step-fiber-eigenvalues v)) 
       (format t "ev took ~3d s time~%" (- (sb-unix::get-time-of-day) start))
       (step-fiber-fields *bla-ev* v :scale 3d0 :debug t)))
   (write-pgm "/run/q/bla.pgm" (convert-ub8  (create-field-mosaic *bla* *bla-ev* ;:fun #'identity
								  ) :scale .9 :offset -.2d0
					     ))))



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



;; http://mathoverflow.net/questions/28669/numerical-integration-over-2d-disk
;; Arthur Stroud, Approximate Calculation of Multiple Integrals.
;; http://people.sc.fsu.edu/~jburkardt/f_src/stroud/stroud.html
;; cubatur for the unit circle
;; 1971 lether a generalized product rule for the unit cirlce
;; http://www.holoborodko.com/pavel/numerical-methods/numerical-integration/cubature-formulas-for-the-unit-disk/

#+nil
(gsll:make-qawo-table (* 1d0 (- l0 l1))
		      (* 2 pi)
		      :cosine
		      32)

(declaim (optimize (debug 3)))

(defun couple (u-modes j0 j1 v)
 (flet ((mode-norm (l u)
	  (let* ((w (sqrt (- (expt v 2) (expt u 2))))
		 (nphi (* pi (if (= l 0) 2 1)))
		 (nrad (* (expt v 2) 
			  (/ (* 2 u u (expt (gsll:cylindrical-bessel-k-scaled l w) 2))) 
			  (gsll:cylindrical-bessel-k-scaled (- l 1) w)
			  (gsll:cylindrical-bessel-k-scaled (+ l 1) w))))
	    (expt (* nphi nrad) -.5))))
   (destructuring-bind (l0 m0) (fiber-linear-to-lm-index j0 u-modes)
     (destructuring-bind (l1 m1) (fiber-linear-to-lm-index j1 u-modes)
      (let* ((lambd .0005)
	     (nco 1.5)
	     (ncl 1.46)
	     (k (* 2 pi (/ lambd))) 
	     ;; diameter of the fiber:
	     (rho (* v (/ (* k (sqrt (- (expt nco 2) (expt ncl 2)))))))
	     (l2 40d0)
	     (delx 4)
	     (bend-radius (* .5 (+ delx (/ (expt l2 2) delx))))
	     (num-elems 100)
	     (del-l (/ l2 num-elems))
	     (alpha (asin (/ del-l bend-radius)))
	     (u0 (elt (elt u-modes (abs l0)) m0))
	     (u1 (elt (elt u-modes (abs l1)) m1))
	     (w0 (sqrt (- (expt v 2) (expt u0 2))))
	     (w1 (sqrt (- (expt v 2) (expt u1 2))))
	     (scale-norm (* (mode-norm l0 u0) (mode-norm l1 u1)))
	     (scale-in (/ scale-norm (* (jn l0 u0) (jn l1 u1))))
#+nil	     (scale-out (/ scale-norm (* (gsll:cylindrical-bessel-k l0 w0)
					 (gsll:cylindrical-bessel-k l1 w1))))
	     (mo0 (mod (+ l0 l1) 4))
	     (mo1 (abs (mod (- l0 l1) 4))))
	(bessel-j-interp-init :end (* 1.01 v) :n 2000 :lmax (max (+ l0 l1 1) (abs (+ 1 (- l0 l1)))))
#+nil	(bessel-k-scaled-interp-init :start (* .6 (min w0 w1)) :end (* 2.2 (max w0 w1))
				     :n 200 :lmax (max (+ l0 l1) (abs (- l0 l1))))
	(when (and (or (= mo0 0) (= mo0 2))   ;; real result
		   (or (= mo1 0) (= mo1 2)))
	  (+ 
	   (* scale-in
	      (gsl:integration-qag #'(lambda (r) 
				       (* (bessel-j l0 (* u0 r))
					  (bessel-j l1 (* u1 r))
					  r
					  (* 2 (/ pi)
					     (let ((arg (* k alpha rho r)))
					       (+ (* (if (= mo0 0) 1 -1) (bessel-j (+ l0 l1) arg))
						  (* (if (= mo1 0) 1 -1) (bessel-j (- l0 l1) arg)))))))
				  0d0 1d0 6 1e-3 1e-2))	   
	   
	   #+nil(* scale-out
	      (gsl:integration-qagiu 
	       #'(lambda (r) 
		   (* (bessel-k-scaled-interp l0 (* w0 r))
		      (bessel-k-scaled-interp l1 (* w1 r))
		      (exp (* -1 w0 r))
		      (exp (* -1 w1 r))
		      r
		      (* 2 (/ pi)
			 (let ((arg (* k alpha rho r)))
			   (+ (* (if (= mo0 0) 1 -1) (bessel-j-interp (+ l0 l1) arg))
			     (* (if (= mo1 0) 1 -1) (bessel-j (- l0 l1) arg)))))))
	       1d0)))))))))

#+nil
(let ((v 32d0))
 (defparameter *bla* (step-fiber-eigenvalues v)))
#+nil
(defparameter *plot*
  (let* ((v 32d0)
	 (u-modes *bla*))
    (loop for n from 0 below 20 #+nil (number-of-modes u-modes) collect
	 (loop for m from 0 below 20 #+nil (number-of-modes u-modes) collect
	      (let ((x (couple u-modes n m v)))
		(format t "~a~%" (list 'bla n m x))
		x)))))


#+nil
(with-open-file (s "/run/q/bla.dat" :direction :output :if-exists :supersede
		   :if-does-not-exist :create
		   )
  (format s "~{~{~f ~}~%~}" *plot*))

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
