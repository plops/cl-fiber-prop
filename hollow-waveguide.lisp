;; implement the modes as described in 1989 hill transmission of
;; linearly polarized infrared light through curved hollow dielectric
;; waveguides


(in-package :cl-fiber-prop)

(defun hollow-field (n m a)
  "construct lambda that calculates the field for a hollow dielectric
waveguide"
  (declare (type (integer 0 10000) n m)
	   (type double-float a))
  (let* ((u (gsll:bessel-zero-jnu (* 1d0 n) m)) ;; m-th root of the n-th order bessel function
	 (f (/ (if (= n 0) 1d0 (sqrt 2))
		 (* (sqrt pi) a (jn (+ n 1) u)))))
    (declare (type double-float u f))
    #'(lambda (r phi)
	(declare (type double-float r phi)
		 (values double-float &optional))
	(* f (jn n (* u r (/ a))) (if (= n 0) 1 (cos (* n phi)))))))

#+nil
(funcall (hollow-field 0 1 1d0) 0d0 0d0)
