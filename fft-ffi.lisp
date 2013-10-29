(in-package :fft)

(defconstant +forward+ 1)
(defconstant +backward+ -1)
(defconstant +measure+ 0)
(defconstant +estimate+ (ash 1 6)) ;; array isn't overwritten during planning

(load-shared-object "libfftw3.so.3")
(define-alien-type plan (* int))

;; power of two fastest; 2 3 5 7 11 13 should be the only dividors,
;; then fast algorithms are available; it is benificial if last
;; dimension of r2c/c2r transform should be even

(define-alien-routine fftw_execute
    void
  (plan plan))

(define-alien-routine fftw_destroy_plan
    void
  (plan plan))

;; real input: n0 x n1 x n2 x .. x n_(d-1), d is the rank 
;; output; n0 x n1 x n2 x .. x (n_(d-1)/2+1)
;;                                        ^
;;                                        |
;; non-negative frequencies and one element 
;; array contents are overwritten during planning

;; for in-place transform real input in row-major order must be padded
;; two extra if last dimension is even and one if odd -> 2 (n_(d-1) /
;; 2 + 1) real values but only n_(d-1) values are stored

;; row-major: if you step through memory, the first dimension's index
;; varies most slowly: 

;; pos=i_(d-1) + nd (i_(d-2) + n_(d-2) (... + n1 i0))

;; the position (i,j,k) of a 5x12x27 array would be accessed with
;; array[k+27*(j+12*i)]

;; this is how multidimensional arrays are stored in lisp

(define-alien-routine fftw_plan_dft_r2c
    plan
  (rank int)
  (n (* int))
  (in (* double-float))
  (out (* double-float)) ;; actually complex
  (flags unsigned-int))

(define-alien-routine fftw_plan_dft_c2r
    plan
  (rank int)
  (n (* int))
  (in (* double-float)) ;; actually complex
  (out (* double-float)) 
  (flags unsigned-int))


(define-alien-routine fftw_plan_dft
    plan
  (rank int)
  (n (* int))
  (in (* double-float))	 ;; actually complex
  (out (* double-float))	  ;; actually complex
  (flags unsigned-int))


(load-shared-object "libfftw3_threads.so.3")
  
(define-alien-routine ("fftw_init_threads" init-threads)
    int)

(define-alien-routine ("fftw_plan_with_nthreads" plan-with-nthreads)
    void
  (nthreads int))

#+nil
(progn
 (init-threads)
 (plan-with-nthreads 4))


(defun plan (in &optional out)
  (declare (type (array (complex double-float) *) in))
  (let* ((in-d (array-displacement in))
	 (out-d (array-displacement (if out out in))))
    (if (not (and in-d out-d))
	(error "initially you should allocate data as a 1d array in lisp and then use displacement.")
     (let* ((rank (array-rank in))
	    (dims-in (make-array rank :element-type '(signed-byte 32)
				 :initial-contents (array-dimensions in)))
	    (in-sap (sb-sys:vector-sap
		     (sb-ext:array-storage-vector in-d)))
	    (out-sap (sb-sys:vector-sap
		      (sb-ext:array-storage-vector out-d))))
       (format t "array alignment ~a" (list in-sap out-sap))
       (sb-sys:with-pinned-objects (dims-in in out)
	 (fftw_plan_dft rank (sb-sys:vector-sap dims-in)
			in-sap out-sap +estimate+))))))

(defun ft-inplace (a)
  (declare (type (array (complex double-float) *) a))
  (sb-sys:with-pinned-objects (a)
    (let ((plan (plan a)))
      (fftw_execute plan)))
  a)

(defun ft (in)
  (declare (type (array (complex double-float) *) in))
  (let* ((out1 (make-array (reduce #'* (array-dimensions in))
			   :element-type '(complex double-float)))
	 (out (make-array (array-dimensions in)
			  :element-type '(complex double-float)
			  :displaced-to out1)))
    (sb-sys:with-pinned-objects (in out)
      (let ((plan (plan in out)))
	(fftw_execute plan)))
    out))

#+nil
(time
 (let* ((n0 512)
	(n1 n0)
	(in1 (make-array (* n1 n0)
			 :element-type '(complex double-float)
			 :initial-element (complex 1d0 0d0)))
	(in (make-array (list n1 n0)
			:element-type '(complex double-float)
			:displaced-to in1))
	(out (ft in)))
   (aref out 0 0)))



;; new laptop with 4 processors and with complex double-float 
;; 8192x8192 2.9s


;; 1024x1024x512 transform takes 21s
;; 2048x2048x128 13s on one processor, 8s on two
;; 16192x32384 - takes too long to optimize
;; 8192x8192 2.5s on two, measure 117s
;; 8192x8192x2 6.4s on two, measure 200s (after 8192x8192)
;; 640X480X640 4s on two, measure 214s
;; 128x128x128x128 3.4s on two (really?), measure 89s 

