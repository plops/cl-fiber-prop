;; implement the modes as described in 1989 hill transmission of
;; linearly polarized infrared light through curved hollow dielectric
;; waveguides


(in-package :cl-fiber-prop)

(defun hollow-fnm (n u a)
    (/ (if (= n 0) 1d0 (sqrt 2))
     (* (sqrt pi) a (jn (+ n 1) u))))

(defun hollow-u (n m)
  "m-th root of the n-th order bessel function"
  (declare (type (integer 0 1000) n)
	   (type (integer 1 1000) m))
  (gsll:bessel-zero-jnu (* 1d0 n) m))

#+nil
(hollow-u 0 1)

(defun hollow-field (n m a)
  "construct lambda that calculates the field for a hollow dielectric
waveguide"
  (declare (type (integer 0 10000) n m)
	   (type double-float a))
  (let* ((u (hollow-u n m))
	 (f (hollow-fnm n u a)))
    (declare (type double-float u f))
    #'(lambda (r phi)
	(declare (type double-float r phi)
		 (values double-float &optional))
	(* f (jn n (* u r (/ a))) (if (= n 0) 1 (cos (* n phi)))))))

#+nil
(funcall (hollow-field 0 1 1d0) 0d0 0d0)

(defun hollow-i (n u nn uu l kappa)
  (declare (type double-float u uu kappa)
	   (type fixnum n nn l)
	   (values double-float &optional))
  (gsll:integration-qng #'(lambda (r) 
			    (* (jn n (* r u))
			       (jn nn (* r uu))
			       (jn l (* r kappa))
			       r))
			0d0 1d0))

(defun hollow-couple-coef (phi n m nn mm &key (lambd 0.0106d0) (a 1d0))
  (declare (type double-float lambd)
	   (values (complex double-float) &optional))
  (let* ((u (hollow-u n m))
	 (uu (hollow-u nn mm))
	 (k (/ (* 2 pi) lambd))
	 (kappa (* k phi a)))
    (* (hollow-fnm n u a)
       (hollow-fnm nn uu a)
       (complex 1d0 0d0)
       (+ (* (expt (complex 0 1) (abs (- n nn))) (hollow-i n u nn uu (abs (- n nn)) kappa))
	  (* (expt (complex 0 1) (+ n nn)) (hollow-i n u nn uu (+ n nn) kappa))))))

#+nil
(with-open-file (s "/run/q/bla.dat" :direction :output :if-exists :supersede :if-does-not-exist :create)
 (loop for phi from 0d-3 below 20d-3 by .1d-3 do
      (format s "~f ~f~%" phi (expt (abs (hollow-couple-coef phi 0 0 0 0)) 2))))
