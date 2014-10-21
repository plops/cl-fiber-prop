

#+nil
(destructuring-bind (nmodes h w) (array-dimensions *bla*)
     (loop for jmode in (mapcar #'first ;; sort modes by u starting with ground mode
				(sort (loop for j across (step-fiber-eigenvalues-linear *bla-ev*) 
					 and i from 0 collect
					   (list i j)) #'< :key #'second)) 
	do
	  (destructuring-bind (l m) (fiber-linear-to-lm-index jmode *bla-ev*)
	    (let* ((v 30d0)
		   (scale 4d0)
		   (lambd .0005)
		   (nco 1.5)
		   (ncl 1.46)
		   (k (* 2 pi (/ lambd))) 
		   ;; diameter of the fiber:
		   (rho (* v (/ (* k (sqrt (- (expt nco 2) (expt ncl 2)))))))
		   ;; resolution of the field in mm/px:
		   (resol (/ (* 2 scale rho) w))
		   (core-simple (* resol resol (loop for j below h sum
				      (loop for i below w sum
					   (let* ((x (* 2 scale (- i (floor w 2)) (/ 1d0 w)))
						  (y (* 2 scale (- j (floor h 2)) (/ 1d0 h)))
						  (r (sqrt (+ (expt x 2) (expt y 2)))))
					     (if (<= r 1d0)
						 (expt (abs (aref *bla* jmode j i)) 2)
						 0d0))))))
		   (clad-simple (* resol resol  (loop for j below h sum
				      (loop for i below w sum
					   (let* ((x (* 2 scale (- i (floor w 2)) (/ 1d0 w)))
						  (y (* 2 scale (- j (floor h 2)) (/ 1d0 h)))
						  (r (sqrt (+ (expt x 2) (expt y 2)))))
					     (if (<= 1d0 r)
						 (expt (abs (aref *bla* jmode j i)) 2)
						 0d0))))))
		   (full-simple (*  (loop for j below h sum
				      (loop for i below w sum
					   (expt (abs (aref *bla* jmode j i)) 2)
					 )))))
	      
	      (format 
	       t "~d ~3d co ~8,2,2e cl ~8,2,2e full ~12,4f ~12,1f ~%"
	       w jmode core-simple clad-simple full-simple (/ (* resol)))))))

;; Integrate[r BesselJ[l, u r]^2, {r, 0, 1} ]
;; 1/2 (BesselJ[-1 + l, u]^2 - (2 l BesselJ[-1 + l, u] BesselJ[l, u])/u +
;;    BesselJ[l, u]^2)

;; page 432 mode launching, fields reflected from the end surface are
;; extermely complicated, but there is a simple approximation for
;; weakly guiding fibers
;; incident beam is tilted towards fiber axis: ni (sin thetai) = nco (sin thetaz)
;; fresnel reflection: Et(thetaz) = 2ni/(nco+ni) Ei(thetai)
;; in the rest of the chapter they assume ni=nco (!)

 
;; levin transform, oscillatory integral 5.3.24
;; 13.9


;; shemirani 2009 Due to symmetries enforced by the bends in it is
;; easiest to find the coupling coefficients in Cartesian coordinates,
;; using the eigenmodes of the ideal fiber, which are orthonormal
;; Hermite–Gaussian function

#+nil
(defun solve-couple-into-lp-modes (matrix)
  "Solve the linear equation using SVD with the supplied matrix and
   a right-hand side vector which is the reciprocal of one more than
   the index."
  (let ((dim (dim0 matrix)))
    (multiple-value-bind (u q d)
        (SV-decomposition (copy matrix))
      (SV-solve u q d (gsll::create-rhs-vector dim)))))
