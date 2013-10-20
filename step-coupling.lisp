#+nil
(progn
  (setf asdf:*central-registry*
	(union (list *default-pathname-defaults*)
	       asdf:*central-registry*))
  (require :cl-fiber-prop))

(in-package :cl-fiber-prop)


(defun k-mu-nu (u-modes &key (v 10d0) (wavelength .633d-3) (nco 1.43d0) (rco 12d-3) (bend-radius 1.5d0))
   (let* ((k (/ (* 2 pi) wavelength))
	  (ncl (sqrt (- (expt nco 2)
			(expt (/ V (* k rco)) 2))))
	  (u-lin (step-fiber-eigenvalues-linear u-modes))
	  (b-lin (step-fiber-betas-linear u-lin v :lambd wavelength :nco nco :bigdelta (bigdelta nco ncl)))
	  (n (number-of-modes u-modes))
	  (kbar (make-array (list n n) :element-type 'double-float)))
     (format t "nco= ~f ncl=~f~%" nco ncl)
     (loop for j below n do		; in document nu spurious mode
	  (destructuring-bind (lj mj) (fiber-linear-to-lm-index j u-modes)
	    (loop for i below n do	; in document mu incident mode
		 (destructuring-bind (li mi) (fiber-linear-to-lm-index i u-modes)
		   (let* ((ali (abs li))
			  (alj (abs lj))
			  (e-mu-nu-m 0d0)
			  (val (cond
				 ((and (<= 0 li) (<= 0 lj)) ;; both even
				  (cond ((or (and (= lj 0) (= li 1))
					     (and (= lj 1) (= li 0))) 2d0)
					((or (and (< 0 li) (or (= li (+ lj 1))
							       (= li (- lj 1))))
					     (and (< 0 li) (= li (- 1 lj)))) 1d0)))
				 ((and (<= li 0) (<= lj 0)) ;; both odd
				  (cond ((or (= li 0) (= lj 0)) 0d0)
					((and (< 0 ali) (or (= ali (+ alj 1))
							    (= ali (- alj 1)))) 1d0)
					((and (< 0 ali) (= ali (- 1 alj))) -1d0))))))
		     (when val (let ((ui (aref u-lin i))
				     (uj (aref u-lin j))
				     (e-mu (if (= ali 0) 2d0 1d0))
				     (e-nu (if (= alj 0) 2d0 1d0))
				     (bi (aref b-lin i))
				     (bj (aref b-lin j)))
				 (setf e-mu-nu-m val)
				 (setf (aref kbar j i) #+nil (cond ((= ali (+ 1 alj)) 1d0)
								   ((= ali (- alj 1)) 2d0)
								   (t 0d0)) 
				       (*
					(* (-  (/ (expt (- bj bi) 2)))
					   (/ bend-radius)
					   e-mu-nu-m uj ui
					   (gsl:cylindrical-bessel-j alj uj)
					   (gsl:cylindrical-bessel-j ali ui)
					   (/ (sqrt (* e-mu e-nu)))
					   (/ (* 2 ncl k (expt rco 3)))
					   (/ (sqrt (abs (* (gsl:cylindrical-bessel-j (- alj 1) uj)
							    (gsl:cylindrical-bessel-j (+ alj 1) uj)
							    (gsl:cylindrical-bessel-j (- ali 1) ui)
							    (gsl:cylindrical-bessel-j (+ ali 1) ui))))))))))
		     )))))
     kbar))


(defvar *u-modes* nil)
(defvar *k-mu-nu* (make-array '(1 1) :element-type 'double-float))
(defvar *b-lin* (make-array 1 :element-type 'double-float))

(let ((v 6d0)
      (wavelen .633d-3)) 
  (defparameter *u-modes* (step-fiber-eigenvalues v))
  (defparameter *k-mu-nu* (k-mu-nu *u-modes* :v v :wavelength wavelen :nco 1.43d0 :rco 20d-3 :bend-radius 1.4d0))
  (defparameter *b-lin* (step-fiber-betas-linear (step-fiber-eigenvalues-linear *u-modes*) v :lambd wavelen))
  (declaim (type (simple-array double-float 1) *b-lin*)
	   (type (simple-array double-float 2) *k-mu-nu*) ))

(progn (terpri)
 (destructuring-bind (h w) (array-dimensions *k-mu-nu*)
   (dotimes (j h)
     (dotimes (i w)
       (let ((val (aref *k-mu-nu* j i)))
	 (if (= val 0d0)
	     (format t "  ------")
	     (format t "~8,2f" val))))
     (terpri))))

(defun coupled-mode-equations (z c dcdz)
  (declare (type double-float z)
	   (type grid:vector-double-float c dcdz))
  (let ((n (grid:dim0 c)))
					;(x+iy) (w+iz) = (xw-yz)+i (xz+yw)
					; w = sin... z= -cos...
					; x = c(2nu) y=c(2nu+1)

    (labels ((ev-real (mu) 
	     (loop for nu below (floor n 2) sum 
		  (let ((arg (* z (- (aref *b-lin* mu) (aref *b-lin* nu)))))
		    (* (aref *k-mu-nu* nu mu) (- (* (grid:aref c (* 2 nu)) (sin arg))
					     (* (grid:aref c (+ (* 2 nu) 1)) (- (cos arg))))))))
	   (ev-imag (mu)
	     (loop for nu below (floor n 2) sum 
		  (let ((arg (* z (- (aref *b-lin* mu) (aref *b-lin* nu)))))
		    (* (aref *k-mu-nu* nu mu) (+ (* (grid:aref c (+ (* 2 nu) 0))
						(- (cos arg)))
					     (* (grid:aref c (+ (* 2 nu) 1))
						(sin arg)))))))
	     (ev (mu)
	       (if (evenp mu) 
		   (ev-real (floor mu 2))
		   (ev-imag (floor mu 2)))))
      (dotimes (mu n)
	(setf (grid:aref dcdz mu) (ev mu)))))
  gsll::+success+)

(defun coupled-mode-jacobian (z c dfdc dfdz)
  (declare (type double-float z)
	   (type grid:vector-double-float c dfdz)
	   (type grid:matrix-double-float dfdc))
  (let ((n (grid:dim0 c)))
    (flet ((ev-real (mu) 
	     (loop for nu below (floor n 2) sum 
		  (* (aref *k-mu-nu* nu mu) (grid:aref c (* 2 nu)) -1 (- (aref *b-lin* mu) (aref *b-lin* nu)) (cos (* z (aref *b-lin* nu))))))
	   (ev-imag (mu)
	     (loop for nu below (floor n 2) sum 
		  (* (aref *k-mu-nu* nu mu) (grid:aref c (+ (* 2 nu) 1)) (- (aref *b-lin* mu) (aref *b-lin* nu)) (- (sin (* z (aref *b-lin* nu))))))))
      (dotimes (mu n)
	(setf (grid:aref dfdz mu) (if (evenp mu)
				     (ev-real (floor mu 2))
				     (ev-imag (floor mu 2))))))
    
    (dotimes (mu n)
      (dotimes (nu n)
	(setf (grid:aref dfdc mu nu) 0d0)))
    
    (dotimes (mu (floor n 2))
      (dotimes (nu (floor n 2))
	;; first mu then nu
	(setf (grid:aref dfdc (* 2 mu) (* 2 nu))             (* (aref *k-mu-nu* nu mu) (sin (* z (- (aref *b-lin* mu) (aref *b-lin* nu)))))
	      (grid:aref dfdc (+ 1 (* 2 mu)) (+ 1 (* 2 nu))) (* (aref *k-mu-nu* nu mu) (- (cos (* z (- (aref *b-lin* mu) (aref *b-lin* nu))))))))))
  gsll::+success+)

(grid:dim0 (grid:make-foreign-array 'double-float :dimensions '(3 2)))

(defparameter *c* (gsll:make-standard-control 1d-6 1d-3 1d0 1d0))

#+nil
(defparameter *s*
  (destructuring-bind (h w) (array-dimensions *bla*)
    (gsll:make-ode-stepper gsll:+step-rk2+ (* w 2) #'fun :scalarsp nil)))

#+nil
(defparameter *evo*
 (destructuring-bind (h w) (array-dimensions *bla*)
   (gsll:make-ode-evolution (* 2 w))))

(defmacro def-coupled-mode-equations-optimized ()
  (let* ((nmodes (length *b-lin*))
	 (n (* 2 nmodes))
	 (real-funs (loop for mu below nmodes collect (intern (format nil "EV-REAL-~3,'0d" mu))))
	 (imag-funs (loop for mu below nmodes collect (intern (format nil "EV-IMAG-~3,'0d" mu)))))
       `(defun coupled-mode-equations-optimized (z c dcdz)
	  (declare (type double-float z)
		   (type grid:vector-double-float c dcdz))
	  (flet (,@(loop for mu from 0 below nmodes collect
			`(,(elt real-funs mu) () (declare (values double-float &optional))
			   (+ ,@(remove-if #'null (loop for nu below nmodes collect
				      (let ((k (aref *k-mu-nu* nu mu)))
					(unless (< (abs k) 1d-15)
					  `(* ,k (grid:aref c ,(* 2 nu)) (sin (* ,(- (aref *b-lin* mu) (aref *b-lin* nu)) z))))))))))
		 ,@(loop for mu from 0 below nmodes collect
			`(,(elt imag-funs mu) () (declare (values double-float &optional))
			   (+ ,@(remove-if #'null (loop for nu below nmodes  collect
							(let ((k (aref *k-mu-nu* nu mu)))
							  (unless (< (abs k) 1d-15)
							    `(* ,k (grid:aref c ,(+ 1 (* 2 nu))) -1 (cos (* ,(- (aref *b-lin* mu) (aref *b-lin* nu)) z)))))))))))
	    ,@(loop for mu below n collect 
		   `(setf (grid:aref dcdz ,mu) ,(if (evenp mu) 
						   `(,(elt real-funs (floor mu 2)))
						   `(,(elt imag-funs (floor mu 2)))))))
	  gsll::+success+)))

#+nil
(def-coupled-mode-equations-optimized)

#+nil
(let ((v 8d0)
      (wavelen .633d-3)) 
  (defparameter *u-modes* (step-fiber-eigenvalues v))
  (defparameter *k-mu-nu* (k-mu-nu *u-modes* :v v :wavelength wavelen :nco 1.43d0 :rco 12d-3 :bend-radius 1.7d0))
  (defparameter *b-lin* (step-fiber-betas-linear (step-fiber-eigenvalues-linear *u-modes*) v :lambd wavelen))
  (declaim (type (simple-array double-float 1) *b-lin*)
	   (type (simple-array double-float 2) *k-mu-nu*) ))

#+nil
(time
 (progn
   
   (destructuring-bind (n) (array-dimensions *b-lin*)
     
     (progn ;; create gnuplot file
       (with-open-file (s "bend.gp" :direction :output :if-exists :supersede :if-does-not-exist :create)
	 (format s "plot ")
	 (dotimes (i n)
	   (format s "\"bend6.dat\" u 1:~d w l lw 2 title \"~d\", " (+ 3 i) i))
	 (format s "\"bend6.dat\" u 1:(")
	 (dotimes (i n)
	   (format s "$~d~c" (+ 3 i) (if (= i (- n 1)) #\Space #\+)))
	 (format s ") w l lw 3~%pause -1~%"))
       #+nil (sb-ext:run-program "/usr/bin/gnuplot" '("bend.gp")))


     (let ((y0 (grid:make-foreign-array 'double-float :dimensions (* 2 n)))
	   (time (grid:make-foreign-array 'double-float :dimensions 1))
	   (step-size (grid:make-foreign-array 'double-float :dimensions 1))
	   (ctl (gsll:make-standard-control 1d-12 1d-12 1d0 0d0))
	   (stepper (gsll:make-ode-stepper gsll:+step-rk8pd+ (* n 2) #'coupled-mode-equations nil nil))
	   (evo (gsll:make-ode-evolution (* 2 n)))
	   (max-time 3d0))  
       (loop for i below (grid:dim0 y0) do (setf (grid:aref y0 i) 1d0))
       (setf (grid:aref y0 1) 1d0
	     (grid:aref time 0) 0d0
	     (grid:aref step-size 0) 1d-2)
       (format t "there are ~d modes~%" n)
       (terpri)
       (with-open-file (f "bend6.dat" :direction :output :if-exists :supersede :if-does-not-exist :create)
	 (loop while (< (grid:aref time 0) max-time) do
	      (gsll:apply-evolution evo time y0 step-size ctl stepper max-time)
	      (format f "~20,12f ~8,3g ~{~18,13f ~}~%" 
		      (grid:aref time 0)
		      (grid:aref step-size 0)
		      (loop for i below n collect 
			   (expt (abs (complex (grid:aref y0 (* 2 i)) 
						     (grid:aref y0 (+ 1 (* 2 i))))) 2)
			   #+nil (phase (complex (grid:aref y0 (* 2 i)) 
					   (grid:aref y0 (+ 1 (* 2 i)))))))))
     
       ))))



#+nil
(let ((eps 1d-3)) ;; check that jacobian is calculated correctly
 (flet ((vanderpol (z y f)
	  (let ((y0 (grid:aref y 0))
		(y1 (grid:aref y 1)))
	    (setf (grid:aref f 0) y1
		  (grid:aref f 1) (* (/ eps) 
				     (- (* (+ 1 (- (expt y0 2))) y1)
					y0))))
	  gsll::+success+)
	(vanderpol-jac (z y dfdy dfdt)
	  (let ((y0 (grid:aref y 0))
		(y1 (grid:aref y 1)))
	    (setf (grid:aref dfdt 0) 0d0
		  (grid:aref dfdt 1) 0d0
		  (grid:aref dfdy 0 0) 0d0 ;; df0/dy0
		  (grid:aref dfdy 0 1) 1d0 ;; df0/dy1
		  (grid:aref dfdy 1 0) (* -1 (/ eps) (+ 1 (* 2 y0 y1))) ;; df1/dy0 
		  (grid:aref dfdy 1 1) (* (/ eps) (- 1 (* y0 y0))) ;; df1/dy1
		  ))
	  gsll::+success+))
   (let ((y0  (grid:make-foreign-array 'double-float :dimensions 2))
	 (y1  (grid:make-foreign-array 'double-float :dimensions 2))
	 (f0  (grid:make-foreign-array 'double-float :dimensions 2))
	 (f1  (grid:make-foreign-array 'double-float :dimensions 2))
	 (dfdy (make-array (list 2 2) :element-type 'double-float))
	 (jdfdy (grid:make-foreign-array 'double-float :dimensions '(2 2) :initial-element 0d0))
	 (jdfdt (grid:make-foreign-array 'double-float :dimensions 2))
	 (d 1d-8)
	 (z 1d0))
     (dotimes (i 2)
       (setf (grid:aref y0 i) (random 10d0)))
     (vanderpol-jac z y0 jdfdy jdfdt)
     (loop for j below 2 do
	  (dotimes (i 2)
	    (setf (grid:aref y1 i)  (grid:aref y0 i)))
	  (setf (grid:aref y1 j)  (+ d (grid:aref y0 j))) ;; derivative with respect to y_j
	  (vanderpol z y0 f0)
	  (vanderpol z y1 f1)
	  (dotimes (k 2)
	    (setf (aref dfdy k j) (* (/ d) (- (grid:aref f1 k) (grid:aref f0 k))))))
     (loop for j below 2 maximize (loop for i below 2 maximize (abs (- (aref dfdy j i) (grid:aref jdfdy j i))))))))



#+nil
(let ((eps 1e-3))
 (flet ((vanderpol (z y f)
	  (let ((y0 (grid:aref y 0))
		(y1 (grid:aref y 1)))
	    (setf (grid:aref f 0) y1
		  (grid:aref f 1) (* (/ eps) 
				     (- (* (+ 1 (- (expt y0 2))) y1)
					y0))))
	  gsll::+success+)
	(vanderpol-jac (z y dfdy dfdt)
	  (let ((y0 (grid:aref y 0))
		(y1 (grid:aref y 1)))
	    (setf (grid:aref dfdt 0) 0d0
		  (grid:aref dfdt 1) 0d0
		  (grid:aref dfdy 0 0) 0d0 ;; df0/dy0
		  (grid:aref dfdy 0 1) 1d0 ;; df0/dy1
		  (grid:aref dfdy 1 0) (* -1 (/ eps) (+ 1 (* 2 y0 y1))) ;; df1/dy0 x
		  (grid:aref dfdy 1 1) (* (/ eps) (- 1 (* y0 y0))) ;; df1/dy1
		  ))
	  gsll::+success+))
   (let ((y0 (grid:make-foreign-array 'double-float :dimensions 2))
	 (time (grid:make-foreign-array 'double-float :dimensions 1))
	 (step-size (grid:make-foreign-array 'double-float :dimensions 1))
	 (ctl (gsll:make-standard-control 1d-14 1d-30 1d0 0d0))
	 (stepper (gsll:make-ode-stepper gsll:+step-bsimp+ 2 #'vanderpol #'vanderpol-jac nil))
	 (evo (gsll:make-ode-evolution 2)))  
     (loop for i below (grid:dim0 y0) do (setf (grid:aref y0 i) 0d0))
     (setf (grid:aref y0 0) 2d0
	   (grid:aref time 0) 0d0
	   (grid:aref step-size 0) 1e-9)
     (loop while (< (grid:aref time 0) 2d0) do
	  (gsll:apply-evolution evo time y0 step-size ctl stepper 2d0)
	  (format t "~{~12,8f ~}~%" (list (grid:aref time 0)  (grid:aref y0 0) (grid:aref step-size 0)))))))
