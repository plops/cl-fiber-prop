#+nil
(progn
  (setf asdf:*central-registry*
	(union (list *default-pathname-defaults*)
	       asdf:*central-registry*))
  (require :cl-fiber-prop))

(in-package :cl-fiber-prop)

(defun k-mu-nu (u-modes &key (v 10d0) (wavelength .633d-3) (refractive-index 1.5d0) (rco 12d-3) (bend-radius 10d0))
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
				   (bi (aref blin i))
				   (bj (aref blin j)))
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

(defparameter *bla* (kbar *u-modes*))

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

(defun fun (z c)
  (flet ((ev (index-mu) 
	   (loop for (j i k delb) in *terms* and index from 0 sum
		(if (= index index-mu) (complex 0d0)
		    (* k (aref c index) (/ (complex 0d0 1d0)) (exp (complex 0d0 (* z delb))))))))
    (macrolet ((frob ()
		 `(values ,@(loop for index-mu below (length *terms*) collect (ev index-mu)))))
      (frob))))



(defparameter *c* (gsll:make-standard-control 1d-6 1d-3 1d0 1d0))

(defparameter *s*
  (destructuring-bind (h w) (array-dimensions *bla*)
   (gsll:make-ode-stepper gsll:+step-rkf45+ (* w 2) #'fun)))

(defparameter *evo*
 (destructuring-bind (h w) (array-dimensions *bla*)
   (gsll:make-ode-evolution (* 2 25))))



(destructuring-bind (h w) (array-dimensions *bla*)
  (let ((y0 (make-array (* 2 w) :element-type 'double-float)))  
    (setf (aref y0 0) 1d0)
    (gsll:apply-evolution *evo* 0d0 y0 .01 *c* *s* .1d0)))
