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
   (loop for j below n do		; in document nu 
	(destructuring-bind (lj mj) (fiber-linear-to-lm-index j u-modes)
	  (loop for i below n do	; in document mu receiving mode
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
				   ((and (< 0 ali) (= ali (- 1 alj))) -1d0)))
			    ((and (<= li 0) (<= 0 lj)) ;; receiving odd, decaying even
			     (cond ((= ali 0) 0d0)
				   ((and (= lj 0) (= ali 1)) 2d0)
				   ((= ali (+ 1 alj)) 1d0)
				   ((and (< 0 ali) (= ali (- alj 1))) -1d0)
				   ((and (< 0 ali) (= ali (- 1 alj))) 1d0))))))
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

(defparameter *u-modes* (step-fiber-eigenvalues 10d0))

(defparameter *k-mu-nu* (k-mu-nu *u-modes*))

(let ((v 10d0))
 (defparameter *b-lin* (step-fiber-betas-linear (step-fiber-eigenvalues-linear *u-modes*) v :lambd .633e-3)))

(progn (terpri)
 (destructuring-bind (h w) (array-dimensions *k-mu-nu*)
   (dotimes (j h)
     (dotimes (i w)
       (let ((val (aref *bla* j i)))
	 (if (= val 0d0)
	     (format t " -- ")
	     (format t "~4f" (* 100 val)))))
     (terpri))))

(defun coupled-mode-equations (z c dcdz)
  ;;  (format t "hallo~%")  (format t "~A~%" (list z (grid:aref c 0) (grid:aref cc 0)))
  (let ((n (grid:dim0 c)))
    (labels ((ev-real (mu) 
	     (loop for nu below (floor n 2) sum 
		  (* (aref *k-mu-nu* nu mu) (grid:aref c (* 2 nu)) (sin (* z (- (aref *b-lin* mu) (aref *b-lin* nu)))))))
	   (ev-imag (mu)
	     (loop for nu below (floor n 2) sum 
		  (* (aref *k-mu-nu* nu mu) (grid:aref c (+ (* 2 nu) 1)) (- (cos (* z (- (aref *b-lin* mu) (aref *b-lin* nu))))))))
	     (ev (mu)
	       (if (evenp mu) 
		   (ev-real (floor mu 2))
		   (ev-imag (floor mu 2)))))
      (dotimes (mu n)
	(setf (grid:aref dcdz mu) (ev mu)))))
  gsll::+success+)


(defun coupled-mode-jacobian (z c dfdc dfdz)
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

(defparameter *evo*
 (destructuring-bind (h w) (array-dimensions *bla*)
   (gsll:make-ode-evolution (* 2 w))))

#+nil
(time
 (destructuring-bind (n) (array-dimensions *b-lin*)
   (let ((y0 (grid:make-foreign-array 'double-float :dimensions (* 2 n)))
	 (time (grid:make-foreign-array 'double-float :dimensions 1))
	 (step-size (grid:make-foreign-array 'double-float :dimensions 1))
	 (ctl (gsll:make-standard-control 1d-8 1d-8 1d0 0d0))
	 (stepper (gsll:make-ode-stepper gsll:+step-rk8pd+ (* n 2) #'coupled-mode-equations #'coupled-mode-jacobian nil))
	 (evo (gsll:make-ode-evolution (* 2 n)))
	 (max-time 20d0))  
     (loop for i below (grid:dim0 y0) do (setf (grid:aref y0 i) 0d0))
     (setf (grid:aref y0 0) 1d0
	   (grid:aref time 0) 0d0
	   (grid:aref step-size 0) 1d-2)
     (terpri)
     (with-open-file (f "bend6.dat" :direction :output :if-exists :supersede :if-does-not-exist :create)
      (loop while (and (< (grid:aref time 0) max-time)
		       (< (abs (complex (grid:aref y0 (* 2 1)) 
					(grid:aref y0 (+ 1 (* 2 1))))) 1d0)) do
	   (gsll:apply-evolution evo time y0 step-size ctl stepper max-time)
	   (format f "~20,12f ~8,3g ~{~18,13f ~}~%" 
		   (grid:aref time 0)
		   (grid:aref step-size 0)
		   (loop for i below 25 collect 
			(expt (abs (complex (grid:aref y0 (* 2 i)) 
					    (grid:aref y0 (+ 1 (* 2 i))))) 2))))))))

#+nil
(progn (terpri)
 (dotimes (i 25)
   (format t "\"bend5.dat\" u 1:~d w l, " (+ 3 i))))

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
