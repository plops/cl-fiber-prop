#+nil
(progn
  (setf asdf:*central-registry*
	(union (list *default-pathname-defaults*)
	       asdf:*central-registry*))
  (require :cl-fiber-prop))

(in-package :cl-fiber-prop)

(defun kbar (u-modes &key (wavelength .633d0) (refractive-index 1.5d0) (rco 12d0))
 (let* ((k (/ (* 2 pi) wavelength))
	(u-lin (step-fiber-eigenvalues-linear u-modes))
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
		   (when val (let ((ui (aref u-lin ali))
				   (uj (aref u-lin alj))
				   (e-mu (if (= ali 0) 2d0 1d0))
				   (e-nu (if (= alj 0) 2d0 1d0))
				   )
			       (setf e-mu-nu-m val)
			       (setf (aref kbar j i) #+nil (cond ((= ali (+ 1 alj)) 1d0)
								 ((= ali (- alj 1)) 2d0)
								 (t 0d0)) 
				     (* e-mu-nu-m uj ui
					(gsl:cylindrical-bessel-j alj uj)
					(gsl:cylindrical-bessel-j ali ui)
					(/ (sqrt (* e-mu e-nu)))
					(/ (* 2 refractive-index k (expt rco 3)))
					(/ (sqrt (abs (* (gsl:cylindrical-bessel-j (- alj 1) uj)
							 (gsl:cylindrical-bessel-j (+ alj 1) uj)
							 (gsl:cylindrical-bessel-j (- ali 1) ui)
							 (gsl:cylindrical-bessel-j (+ ali 1) ui)))))))))
		   )))))
   kbar))

(defparameter *bla* (kbar (step-fiber-eigenvalues 10d0)))

(progn (terpri)
 (destructuring-bind (h w) (array-dimensions *bla*)
   (dotimes (j h)
     (dotimes (i w)
       (let ((val (aref *bla* j i)))
	 (if (= val 0d0)
	     (format t " -- ")
	     (format t "~4f" (* 100 val)))))
     (terpri))))