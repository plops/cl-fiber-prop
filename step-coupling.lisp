#+nil
(progn
  (setf asdf:*central-registry*
	(union (list *default-pathname-defaults*)
	       asdf:*central-registry*))
  (require :cl-fiber-prop))

(in-package :cl-fiber-prop)

(defun k-mu-nu (u-modes &key (v 10d0) (wavelength .633d-3) (refractive-index 1.5d0) (rco 12d-3) (bend-radius 1d0))
 (let* ((k (/ (* 2 pi) wavelength))
	(u-lin (step-fiber-eigenvalues-linear u-modes))
	(b-lin (step-fiber-betas-linear u-lin v :lambd wavelength))
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
					 (/ (* 2 refractive-index k (expt rco 3)))
					 (/ (sqrt (abs (* (gsl:cylindrical-bessel-j (- alj 1) uj)
							  (gsl:cylindrical-bessel-j (+ alj 1) uj)
							  (gsl:cylindrical-bessel-j (- ali 1) ui)
							  (gsl:cylindrical-bessel-j (+ ali 1) ui))))))))))
		   )))))
   kbar))

(defparameter *u-modes* (step-fiber-eigenvalues 10d0))

(defparameter *bla* (k-mu-nu *u-modes*))

(progn (terpri)
 (destructuring-bind (h w) (array-dimensions *bla*)
   (dotimes (j h)
     (dotimes (i w)
       (let ((val (aref *bla* j i)))
	 (if (= val 0d0)
	     (format t " -- ")
	     (format t "~4f" (* 100 val)))))
     (terpri))))



(defparameter *terms*
  (let* ((res nil)
	 (u-lin (step-fiber-eigenvalues-linear *u-modes*))
	 (b-lin (step-fiber-betas-linear u-lin 10d0 :lambd .633e-3)))
    (destructuring-bind (h w) (array-dimensions *bla*)
      (dotimes (i w)
	(dotimes (j h)
	  (unless (= 0d0 (aref *bla* j i))
	    (push (list j i
			(aref *bla* j i) 
			(- (aref b-lin j) 
			   (aref b-lin i))) res)))))
    (reverse res)))

(defun fun (z c cc)
;  (format t "hallo~%")  (format t "~A~%" (list z (grid:aref c 0) (grid:aref cc 0)))
  (flet ((ev-real (index-mu) 
	   (loop for (j i k delb) in *terms* and index from 0 sum
		(if (= index index-mu) 
		    0d0
		    (* k (grid:aref c (* 2 index)) (sin (* z delb))))))
	 (ev-imag (index-mu) 
	   (loop for (j i k delb) in *terms* and index from 0 sum
		(if (= index index-mu) 
		    0d0
		    (* k (grid:aref c (+ 1 (* 2 index))) (- (cos (* z delb))))))))
    (loop for index-mu below (grid:dim0 cc) do
	 (setf (grid:aref cc index-mu) (if (evenp index-mu) 
					  (ev-real index-mu)
					  (ev-imag index-mu)))))
  gsll::+success+)



(defparameter *c* (gsll:make-standard-control 1d-6 1d-3 1d0 1d0))

(defparameter *s*
  (destructuring-bind (h w) (array-dimensions *bla*)
    (gsll:make-ode-stepper gsll:+step-rk2+ (* w 2) #'fun :scalarsp nil)))

(defparameter *evo*
 (destructuring-bind (h w) (array-dimensions *bla*)
   (gsll:make-ode-evolution (* 2 w))))

#+nil
(destructuring-bind (h w) (array-dimensions *bla*)
  (let ((y0 (grid:make-foreign-array 'double-float :dimensions (* 2 w)))
	(time (grid:make-foreign-array 'double-float :dimensions 1))
	(step-size (grid:make-foreign-array 'double-float :dimensions 1))
	(ctl (gsll:make-standard-control 1d-17 1d-14 1d0 0d0))
	(stepper (gsll:make-ode-stepper gsll:+step-rk2+ (* w 2) #'fun :scalarsp nil))
	(evo (gsll:make-ode-evolution (* 2 w))))  
    (loop for i below (grid:dim0 y0) do (setf (grid:aref y0 i) 0d0))
    (setf (grid:aref y0 0) 1d0
	  (grid:aref time 0) 0d0
	  (grid:aref step-size 0) 1d0)
    (gsll:apply-evolution evo time y0 step-size ctl stepper 100d0)
    (format t "~a~%" (loop for i below 3 collect 
			  (list i (abs (complex (grid:aref y0 (* 2 i)) (grid:aref y0 (+ 1 (* 2 i))))))))))


#+nil
(flet ((vanderpol (z y yy)
	   (let ((eps 1e-3)
		 (y0 (grid:aref y 0))
		 (y1 (grid:aref y 1)))
	    (setf (grid:aref yy 0) y1
		  (grid:aref yy 1) (* (/ eps) 
				      (- (* (+ 1 (- (expt y0 2))) y1)
					 y0))))))
   (let ((y0 (grid:make-foreign-array 'double-float :dimensions 2))
	 (time (grid:make-foreign-array 'double-float :dimensions 1))
	 (step-size (grid:make-foreign-array 'double-float :dimensions 1))
	 (ctl (gsll:make-standard-control 1d-1 1d-2 1d0 0d0))
	 (stepper (gsll:make-ode-stepper gsll:+step-rk8pd+ 2 #'vanderpol :scalarsp nil))
	 (evo (gsll:make-ode-evolution 2)))  
     (loop for i below (grid:dim0 y0) do (setf (grid:aref y0 i) 0d0))
     (setf (grid:aref y0 0) 2d0
	   (grid:aref time 0) 0d0
	   (grid:aref step-size 0) 1e-2)
     (loop while (< (grid:aref time 0) 2d0) do
	  (gsll:apply-evolution evo time y0 step-size ctl stepper 2d0)
	  (format t "~a~%" (list (grid:aref time 0) (grid:aref y0 0))))))
