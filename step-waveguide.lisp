;; problem: the NA of the fiber surpasses the NA of the 10x detection
;; objective idea: fit available data in fourier space and in real
;; space sequentially switching also change the central position in
;; both spaces (4 parameters)

;; i know the transfermatrix using mode patterns for one polarization.
;; using this i can reconstruct the mode pattern that results in the
;; delta function due to the mirror. this way i should be able to
;; verify that the modes are bended due to the refraction at the fiber
;; interface and also estimate the refractive index of the fiber. any
;; further inconsistencies may then be used to recover the distortions
;; of the reference beam.
#+nil
(ql:quickload "cffi")
#+nil
(ql:quickload "gsll")
#+nil
(progn
  (setf asdf:*central-registry*
	(union (list #p"/home/martin/stage/cl-cffi-fftw3/"
		     *default-pathname-defaults*)
	       asdf:*central-registry*))
  (require :cl-fiber-prop)
  (require :fftw))


(defun convert-12p-16 (data)
  (let* ((n (* 1920 1080 12 (/ 8)))
	 (out (make-array (list 1080 1920) :element-type '(unsigned-byte 16)))
	 (out1 (sb-ext:array-storage-vector out)))
   (loop for byte below (length data) by 3
      and short from 0 below n by 2 do
	(let ((ab (aref data byte))
	      (c (ldb (byte 4 0) (aref data (+ 1 byte))))
	      (d (ldb (byte 4 4) (aref data (+ 1 byte))))
	      (ef (aref data (+ 2 byte))))
	  (setf (aref out1 short) (ash (+ (ash ab 4) d) 4)
		(aref out1 (1+ short)) (ash (+ (ash ef 4) c) 4))))
   out))




#+nil
(write-pgm "/dev/shm/o.pgm" (convert-u16-u8 *bla* :scale (/ 255d0 (reduce #'max (sb-ext:array-storage-vector *bla*)))))
;; 614 846

(defun phase-wedge (a kx ky)
  (declare (type (simple-array (complex double-float) 2) a)
	   (type double-float kx ky)
	   (optimize (speed 3))
	   (values (simple-array (complex double-float) 2) &optional))
  (let* ((b (make-array (array-dimensions a) :element-type '(complex double-float))))
   (destructuring-bind (h w) (array-dimensions a)
     (declare (type fixnum h w))
     (let ((s (* 2 pi))
	   (si (/ kx w))
	   (sj (/ ky h)))
      (dotimes (j h)
	(dotimes (i w)
	  (setf (aref b j i) (exp (complex 0d0 (* s (+ (* si i) (* sj j))))))))))
   b))

(defun j1/r (a alpha)
  (declare (type (simple-array * 2) a)
	   (type double-float alpha)
	   (values (simple-array (complex double-float) 2))
	   (optimize (speed 3)))
  (let* ((b (make-array (array-dimensions a)
			:element-type '(complex double-float))))
    (destructuring-bind (h w) (array-dimensions a)
      (declare (type fixnum h w))
      (dotimes (j h)
	(dotimes (i w)
	  (let* ((r (sqrt (+ (expt (/ (- i (floor w 2)) (* 1d0 h)) 2)
			     (expt (/ (- j (floor h 2)) (* 1d0 h)) 2))))
		 (x (* 2 pi alpha r)))
	    (setf (aref b j i) (if (< x 1e-30)
				   (complex 0d0)
				   (let ((j1 (gsll:cylindrical-bessel-j1 x)))
				     (declare (type double-float j1))
				     (complex (* 2d0 (/ j1 x)))))))))
      b)))


(defun tukey-window (nn &key (alpha .9d0))
  "The Tukey window,[8][39] also known as the tapered cosine window,
can be regarded as a cosine lobe of width alpha N/2 that is convolved
with a rectangular window of width (1 - alpha/2)N. For alpha=0
rectangular, for alpha=1 Hann window."
  (declare (type (unsigned-byte 32) nn)
           (type double-float alpha)
	   (optimize (speed 3))
           (values (simple-array double-float 1) &optional))
  (let ((w (make-array nn :element-type 'double-float :initial-element 0d0))
        (n-1 (- nn 1d0)))
    (dotimes (n nn)
      (let ((nd (* 1d0 n)))
       (setf (aref w n)
	     (cond ((<= 0d0 nd (* alpha .5d0 n-1))
		    (* .5d0 (+ 1d0 (cos (* pi (- (/ (* 2d0 n)
						    (* alpha n-1)) 1d0))))))
		   ((<= (* alpha .5d0 n-1) nd (* n-1 (- 1d0 (* .5d0 alpha))))
		    1.0d0)
		   ((<= (* n-1 (- 1 (* .5d0 alpha))) nd n-1)
		    (* .5d0 (+ 1d0 (cos (* pi (+ (/ (* 2d0 n)
						    (* alpha n-1))
						 (/ -2.0d0 alpha)
						 1d0))))))
		   (t 0d0)))))
    w))

(defun tukey-window2 (a &key (alpha-x .2d0) (alpha-y alpha-x))
  (declare (type (simple-array (complex double-float) 2) a)
           (values (simple-array (complex double-float) 2) &optional)
	   (optimize (speed 3)))
  (destructuring-bind (h w) (array-dimensions a)
    (declare (type fixnum h w))
    (let* ((b (make-array (array-dimensions a) :element-type '(complex double-float)))
	   (wh (tukey-window h :alpha alpha-y))
	   (ww (tukey-window w :alpha alpha-x)))
      (declare (type (simple-array double-float 1) wh ww))
      (dotimes (j h)
	(dotimes (i w)
	  (setf (aref b j i) (* (aref a j i)
				(aref wh j)
				(aref ww i)))))
      b)))

#+nil
(prog1 nil
 (tukey-window2 (convert-u16-cdf *bla*)))


(defun .* (a b)
  (declare (type (simple-array * *) a b)
	   (values (simple-array (complex double-float) *) &optional))
  (let* (
	(a1 (make-array (array-total-size a) :element-type (array-element-type a)
			:displaced-to a))
	(b1 (make-array (array-total-size b) :element-type (array-element-type b)
			:displaced-to b))
	(c (make-array (array-dimensions a) :element-type '(complex double-float)))
	(c1 (make-array (array-total-size a) :element-type '(complex double-float)
			:displaced-to c)))
    (dotimes (i (length a1))
      (setf (aref c1 i) (* (aref a1 i) (aref b1 i))))
    c))

(defun .apply (a &optional (fun #'identity))
  (declare (type (simple-array * *) a)
	   (values (simple-array (complex double-float) *) &optional))
  (let* (
	(a1 (make-array (array-total-size a) :element-type (array-element-type a)
			:displaced-to a))
	(c (make-array (array-dimensions a) :element-type '(complex double-float)
		       ))
	(c1 (make-array (array-total-size a) :element-type '(complex double-float)
			:displaced-to c))
	)
    (dotimes (i (length a1))
      (setf (aref c1 i) (funcall fun (aref a1 i)
			 )))
    c))



;; (/ 34133529600 (* 1920 1080 12 (/ 8))) => 10974 = 118x93
;; 118 in fast axis and 93 in slow axis

(defun get-cam-image (cam j i)
  (declare (type (integer 0 2) cam)
	   (type (integer 0 92) j)
	   (type (integer 0 117) i))
  (with-open-file (s (format nil "/media/sdd3/b/cam~d" cam) :element-type '(unsigned-byte 8))
    (let* ((n (* 1920 1080 12 (/ 8)))
	   (a (make-array n :element-type '(unsigned-byte 8))))
      (file-position s (* (+ i (* j 118)) n))
      (read-sequence a s)
      (convert-12p-16 a))))

(defun get-cam-image-laptop (cam j i)
  (declare (type (integer 0 2) cam)
	   (type (integer 0 92) j)
	   (type (integer 0 117) i))
  (unless (and (= 0 (mod i 30))
	       (= 0 (mod j 30)))
    (error "I only stored images with index dividable by 30."))
  (with-open-file (s (format nil "/home/martin/cam_/cam~d_j~d-i~d" cam j i) :element-type '(unsigned-byte 8))
    (let* ((n (* 1920 1080 12 (/ 8)))
	   (a (make-array n :element-type '(unsigned-byte 8))))
      (read-sequence a s)
      (convert-12p-16 a))))

#+nil
(let ((cam 2))
 (loop for i from 0 below 118 by 30 do
      (loop for j from 0 below 93 by 30 do
	   (with-open-file (s (format nil "/media/sdd3/b/cam~d" cam) :element-type '(unsigned-byte 8))
	     (let* ((n (* 1920 1080 12 (/ 8)))
		    (a (make-array n :element-type '(unsigned-byte 8))))
	       (file-position s (* (+ i (* j 118)) n))
	       (read-sequence a s)
	       (with-open-file (s2 (format nil "/media/sdd3/b/cam~d_j~a-i~a" cam j i)
				   :element-type '(unsigned-byte 8)
				   :direction :output)
		 (write-sequence a s2))))
	   )))

#+nil
(require :sb-sprof)

#+nil
(time ;; 4.85s
 (defparameter *window* (.apply (fftw:ft (tukey-window2 (j1/r (make-array (list 1080 1920)
									  :element-type '(complex double-float))
							      180d0)
							:alpha-x .4))
				(lambda (x) (if (< (abs x) 10)
						(complex 0d0)
						(complex (abs x)))))))

#+nil
(time ;; 2.65s, now 0.724s
 (defparameter *windowed-phase-wedge* (tukey-window2 (phase-wedge (make-array (list 1080 1920)
									      :element-type '(complex double-float))
								  614d0 846d0))))


(defun .linear (a)
;  #+sbcl  (declare (values (simple-array * *) &optional))
  #+sbcl
  (let ((d (array-displacement a)))
   (if d
       d
       (sb-ext:array-storage-vector a)))
  #-sbcl
  (make-array (array-total-size a)
              :element-type (array-element-type a)
              :displaced-to a))

(defun .accum (dst b)
  (declare (optimize (speed 3)))
  (declare (type (array double-float 2) b)
           (type (simple-array double-float 2) dst)
           (values (simple-array double-float 2) &optional))
  (let* ((b1 (.linear b))
         (dst1 (.linear dst))
         (n (array-total-size dst)))
    (declare (type (simple-array double-float 1) b1 dst1))
    (dotimes (i n)
      (setf (aref dst1 i) (+ (aref dst1 i) (aref b1 i))))
    dst))
#+nil
(let ((a (make-array (list 1080 1920) :element-type 'double-float)))
  (sb-sprof:with-profiling (:max-samples 1000                                  
					 :report :flat 
					 :loop nil)
    (loop for j below 93 by 30 do
	 (loop for i below 118 by 30 do
	      (format t "~a~%" j i)
	      (let ((im (get-cam-image 0 j i)))
		(.accum a (convert-df
			   (fftw:ft
			    (.* *window*
				(fftw:ft 
				 (.* *windowed-phase-wedge* im)))
			    :sign fftw::+backward+)
			   :fun (lambda (x) (expt (abs x) 2))))))))
  (write-pgm "/dev/shm/ko3.pgm" (convert-ub8 a)))

#+nil
(defparameter *bla*
 (get-cam-image-laptop 0 60 60))

#+nil
(progn
 (myclock::update-img 
  (convert-ub8 
   (convert-df (convert-u16-cdf (get-cam-image-laptop 0 30 30)))))
 nil)

#+nil
(time
 (let* ((im (get-cam-image-laptop 0 30 30))
	(field (fftw:ft
		(.* *window*
		    (fftw:ft 
		     (.* *windowed-phase-wedge* (convert-u16-cdf im))))
		:sign fftw::+backward+)
	       ))
   (defparameter *current-field* field)
   
   (myclock::update-img
    (convert-ub8 (convert-df
		  field					       
		  :fun (lambda (x) (realpart x)))))
   nil
   #+nil
   (write-pgm "/dev/shm/ko3.pgm" (convert-ub8 (convert-df
					       field					       
					       :fun (lambda (x) (realpart x)))))))

;; positions of fiber ends
;; 207x207+1054+148 
;; 625x395
;; 569x637

#+nil
 

#+nil
(reduce #'max
	(mapcar #'(lambda (a) (floor (* 1d-9 (abs a)))) *coef1*))
#+nil
(format t "~{~a~%~}"
 (mapcar #'(lambda (a) (* 1d-9 (abs a))) *coef1*))

(defun create-coefficient-mosaic (coefs u-modes &key (debug nil))
  (declare (type (simple-array (complex double-float) 1) coefs)
	   (optimize (speed 3))
	   (values (simple-array (complex double-float) 2) &optional))
  (let* ((lmax (list-length u-modes))
	 (mmax (list-length (first u-modes)))
	 (nmodes (number-of-modes u-modes))
	 (a (make-array (list (+ lmax (1- lmax))  mmax) :element-type '(complex double-float)))
	 (lut (fiber-linear-to-lm-index-lut u-modes)))
    (declare (type (simple-array (complex double-float) 2) a)
	     (type fixnum nmodes lmax mmax))
    (when debug
      (format t "filling a ~ax~a matrix~%" (array-dimension a 1) (array-dimension a 0)))
    (loop for k below nmodes do
	 (when debug (format t "doing mode ~d/~d.~%" (1+ k) nmodes))
	 (destructuring-bind (l m) (aref lut k)
	   (declare (type fixnum l m))
	   (setf (aref a (+ (- lmax 1) l) m) (aref coefs k))))
    a))

#+nil
(defparameter *c1m* )

#+nil
(write-pgm "/dev/shm/c1m.pgm" (convert-ub8 (convert-df (create-coefficient-mosaic *coef1* *u-modes*) :fun (lambda (x) (log (+ .1 (abs x)))))))


(defmacro with-multiprocessing (processors var task-list
				init body end)
  `(let* ((processes ,processors)
	  (task-groups (schedule-tasks ,task-list :processes processes)))
     (let ,init
       (let ((th (loop for task in task-groups and process from 0 collect
		      (sb-thread:make-thread 
		       #'(lambda ()
			   (loop for ,var in task do
				,body
				))))))
	 (loop for thread in th do (sb-thread:join-thread thread))
	 ,end))))

(defun schedule-tasks (l &key (processes 4))
 (let* ((len (length l))
	(max-tasks-per-process (ceiling len processes))
	(start-proc 0))
   (loop for p below processes collect
	(prog1
	    (subseq l start-proc 
		    (min len 
			 (+ start-proc max-tasks-per-process)))
	  (incf start-proc max-tasks-per-process)))))


(defun combine-mode-coefficients (coefficients mode-fields)
  (declare (type (simple-array double-float 3) mode-fields)
	   (type (simple-array (complex double-float) 1) coefficients)
	   (values (simple-array (complex double-float) 2) &optional)
	   (optimize (speed 3)))
  (destructuring-bind (ncoef n no) (array-dimensions mode-fields) 
    (declare (ignorable ncoef no)
	     (type fixnum n))
    (with-multiprocessing 1 k 
		      (loop for k below (length coefficients) collect k)
		      ((new-field (make-array (list processes n n) 
					      :element-type '(complex double-float)
					      :initial-element (complex 0d0))))
		      (loop for j below n do
			   (loop for i below n do
				(incf (aref new-field process j i)
				      (* (aref coefficients k)
					 (aref mode-fields k j i)))))
		      (let ((out (make-array (list n n) 
					     :element-type '(complex double-float))))
			(loop for p below processes do
			     (loop for j below n do
				  (loop for i below n do
				       (incf (aref out j i) (aref new-field p j i)))))
			out))))

;; measuring the back shifted first order
;; ul 225px horizontal, 1079-934px vertical
;; ol 222 110  | currently 320 180
;; or 1919-1709 107
;; ur 1919-1708 1079-929

(* 222 (/ 1080 1920d0) )

#+nil
(time ;; 118s
 (progn
   (time ;; 4.85s
    (defparameter *window* (.apply (fftw:ft (tukey-window2 (j1/r (make-array (list 1080 1920)
									     :element-type '(complex double-float))
							      124d0)
							   :alpha-x .4))
				   (lambda (x) (if (< (abs x) 10)
						   (complex 0d0)
						   (complex (abs x)))))))
   

   (time ;; 2.65s, now 0.724s
    (defparameter *windowed-phase-wedge* (tukey-window2 (phase-wedge (make-array (list 1080 1920)
									      :element-type '(complex double-float))
								  614d0 846d0))))

   (loop for j from 0 below 93 by 30 do 
	(loop for i from 0 below 118 by 30 do
	     (let* ((im (get-cam-image-laptop 0 j i))
		    (order (.* *window*
			       (fftw:ft 
				(.* *windowed-phase-wedge* (convert-u16-cdf im)))))
		    (field (fftw:ft
			    order
			    :sign fftw::+backward+)))
	       (defparameter *current-field* field)
	       (write-pgm (format nil "/dev/shm/ko1_j~d-i~d.pgm" j i)
			  (convert-ub8 (convert-df
					order				       
					:fun (lambda (x) (abs x)))))
	       (write-pgm (format nil "/dev/shm/ko3_j~d-i~d.pgm" j i)
			  (convert-ub8 (convert-df
					field					       
					:fun (lambda (x) (realpart x))))))
	     (defparameter *coef1*
	       (find-mode-coefficients *current-field* 
				       (floor (+ 1147 1364 -256) 2)
				       (floor (+ 234 441 -256) 2)
				       *fields*))
	     (defparameter *coef1-recon*
	       (combine-mode-coefficients *coef1* *fields*))
	     (write-pgm (format nil "/dev/shm/recon-coef0_j~d-i~d.pgm" j i) (convert-ub8 (convert-df *coef1-recon* :fun #'realpart)))
	     (write-pgm (format nil "/dev/shm/c1m_j~d-i~d.pgm" j i) (convert-ub8 (convert-df (create-coefficient-mosaic *coef1* *u-modes*) :fun (lambda (x) (realpart x)))))))))

#+nil
(write-pgm "/dev/shm/recon-coef2.pgm" (convert-ub8 (convert-df *coef1-recon* :fun #'realpart)))

#+nil
(let* ((n 256)
       (a (make-array (list n n) :element-type '(complex double-float))))
  (dotimes (j n)
    (dotimes (i n)
      (setf (aref a j i) (aref *current-field* 
			       (+ j (floor (+ 234 441 -256 3) 2))
			       (+ i (floor (+ 1147 1364 -256 5) 2))
			       ))))
  (write-pgm "/dev/shm/raw.pgm" (convert-ub8 (convert-df a :fun #'realpart)))
  ;;(defparameter *raw* a)
  (dotimes (j n)
    (dotimes (i n)
      (setf (aref a j i) (complex (+ (* 1 (realpart (aref a j i))) 
				     (*  (realpart (aref *coef1-recon* j i))))))))
  (defparameter *coef1-diff* a)
  (write-pgm "/dev/shm/recon-coef1p-diff0.pgm" (convert-ub8 (convert-df *coef1-diff* :fun #'realpart
								     )))) 
(/ 1d-9 .86d-13)

#+nil
(write-pgm "/dev/shm/cur.pgm" (convert-ub8 (convert-df *current-field* :fun #'realpart
								     )))

;;  mt9p031-2 is in basler aca1920-25gm which should have 2.2um pixel
;;  pitch according to research i did previously.
;; i use a 10x objective with 150mm tubelens
;; 50 um 164.5
;; ftl/fobj = Yim/Yobj = M
;; fobj = ftl/M = 164.5 mm/10 = 16.45 mm
;; Msys = 150/16.45 = 9.118
;; diameter of the 50um fiber on the camera:
;(/ (* 50 (/ 150 16.45)) 2.2) ; => 207.2396px
;; to fill 256 pixels, use scale 1.235 in field calculating function

(/ 256 (/ (* 50 (/ 150 16.45)) 2.2))

(defvar *current-field* nil)
(defvar *fields* nil)
(defun find-mode-coefficients (current-field istart jstart fields &key (debug nil))
  (declare (type fixnum istart jstart)
	   (type (simple-array (complex double-float) 2) current-field)
	   (type (simple-array double-float 3) fields)
	   (optimize (speed 3))
	   (values (simple-array (complex double-float) 1) &optional))
 (let ((n 256)
       (count 0))
   (declare (type fixnum count))
   (loop for j below n do
	(loop for i below n do
	     (let ((r (sqrt (+ (expt (- i (floor n 2)) 2)
			       (expt (- j (floor n 2)) 2)))))
	       (when (< r (* .52 207.2396))
		 (incf count)))))
   (when debug
    (format t "find-mode-coefficient processes count = ~a pixels~%" count))
   (with-multiprocessing 1 k 
		      (loop for k below (array-dimension fields 0) collect k)
		      ((sum (make-array (array-dimension fields 0) 
					:element-type '(complex double-float)
					:initial-element (complex 0d0))))
		      (progn
			(loop for j below n do
			     (loop for i below n do
				  (let ((r (sqrt (+ (expt (- i (floor n 2)) 2)
						    (expt (- j (floor n 2)) 2)))))
				    (when (< r (* .52 207.2396))
				      (incf (aref sum k) (* (aref fields k j i)
							    (aref current-field (+ j jstart) (+ i istart))))))))
			(setf (aref sum k) (/ (aref sum k) (* 1d0 count))))
		      sum)))

#+nil
(time ;; used to be 25s, with types 0.6s
 (defparameter *coef2* (find-mode-coefficients 625 395)))



;; i assume fiber cladding to be quartz: ncl=1.457 
;; na=0.54
;; diameter=50um

; solve(sqrt(nco^2 - 1.457^2)=0.54,nco)
; sqrt (0.54^2+1.457^2) => nco=1.553

#+nil
(sb-ext:gc :full t)
#+nil
(room)
#+nil
(let* ((ncl 1.457)
       (lambd .0006328)
       (na .54)
       (nco (sqrt (+ (expt na 2) (expt ncl 2))))
       (core-radius 25d-3)
       (v (v .0006328 nco ncl core-radius))
					;(betas (step-fiber-betas* :wavelength lambd :ncore nco :ncladding ncl :core-radius core-radius))
       (u-modes (step-fiber-eigenvalues v :tol 1d-22 :itermax 10000))
       (m 3)
       (l 32)
       (u (elt (elt u-modes l) m))
       )
  (* 2 (number-of-modes u-modes)) ;  => 2757 modes (must be multiplied by 2 for the other polarization)
  ;;(field (step-fiber-field u v l :n 207 :scale 2d0))
  (defparameter *u-modes* u-modes)
  (time
   (defparameter *fields* (step-fiber-fields u-modes v
					     :scale (/ 256 (/ (* 50 (/ 150 16.45)) 2.2))  
					     :rco core-radius
					     :nco nco
					     :n 256 
					     :debug t)))
  ;;(write-pgm "/dev/shm/field.pgm" (convert-ub8 field))
  ) 


;; v parameter is 134


;; snyder p. 328 weakly guiding fiber (circular step index) and polarization correction
;; 432 illumination of fiber endface
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


(defun check (fn pos &rest args)
1  "call a gsll function and check that the error lies within a margin i can live with. pos is there to indicate which of many calls was the problematic one"
  (multiple-value-bind (v err) (apply fn args)
    (when (and (< .1 (/ err v))
	       (< .01 v))
	(break "error: function is not precise enough ~a" (list pos 'args args 'result v 'error err)))
    v))

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
	   (if (and (< 80 v) (< 80 u) (< 100 l) (< (* .05 (expt l 1/3)) (- u l)))
	       ;; wkb approximation
	       (let ((chi (+ (sqrt (- (expt u 2) (expt l 2)))
			   (* -1 l (acos (/ l u)))
			   (* pi -.25))))
		 (- (tan chi) (sqrt (/ (+ (expt w 2) (expt l 2))
				     (+ (expt u 2) (expt l 2))))))
	       (- (if (< 130 l)
		      (/ (expt u 2) (* 2 l))
		      (/ (* u (check #'gsll:cylindrical-bessel-j 1 (+ l 1) u))
			 (check #'gsll:cylindrical-bessel-j 2 l u)))
		  (if (and (< w (* .1 (sqrt (+ l 1)))) (< 0 l)) 
		      (* 2 l)
		      (/ (* w (check #'gsll:cylindrical-bessel-k-scaled 3 (+ l 1) w))
			 (check #'gsll:cylindrical-bessel-k-scaled 4 l w)))))))))))

#+nil
(CHAR-STEP-INDEX-FIBER 100d0 200d0 71)

#+nil
(let ((l 140)
      (w 3.4d0))
  (* w (gsll:cylindrical-bessel-k-scaled (+ l 1) w)
     (/ (gsll:cylindrical-bessel-k-scaled l w))))

#+nil
(let ((l 139)
      (u 1d0))
  (list (/ (* u u) (* 2 l))
   (* u (gsll:cylindrical-bessel-j (+ l 1) u)
      (/ (gsll:cylindrical-bessel-j         l u)))))


(defun step-fiber-eigenvalues (v &key debug (tol 1d-9) (itermax 100))
  (declare (type double-float tol))
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
			    (progn (when debug (format t "checking ~a~%" (list l (1- m) (elt us (1- m)) (elt us m))))
				   (let ((du 1d-9))
				     (handler-case 
					 (zbrent #'(lambda (x) (char-step-index-fiber x v l))
						 (+ (elt us (1- m)) du) (- (elt us m) du)
						 tol itermax)
				       (root-not-bracketed ())
				       (max-iterations-exceeded ()))))))))
      ;; occasionally there is no mode in the gap between the last pole and v
      (remove-if #'null (mapcar #'(lambda (y) (remove-if #'(lambda (x) (null x)) y))
				modes)))))


#+nil
(zbrent #'(lambda (x) (char-step-index-fiber x 30d0 10))
	(+ -.000001 28.887375508582966d0) 30d0 1d-15)
#+nil
(char-step-index-fiber 28.887375063530467 30d0 10)


#+nil
(time
 (defparameter *bla*
   (step-fiber-eigenvalues 2000d0)))

#+nil
(number-of-modes *bla*)

#+nil
(time 
 (let ((count 0))
   (loop for e in (step-fiber-eigenvalues 1000d0) do
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
(numerical-aperture 1.553d0 1.457d0)
#+nil
(bigdelta 1.553d0 1.457d0)
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
  (aref (fiber-linear-to-lm-index-lut u-modes) j))

(defun fiber-linear-to-lm-index-lut (u-modes)
  (let ((res (make-array (number-of-modes u-modes))))
    (loop for ul in u-modes and l from 0 do
	 (loop for um in ul and m from 0 do
	      (setf (aref res (fiber-lm-to-linear-index l m u-modes))
		    (list l m))
	      (unless (= l 0)
		(setf (aref res (fiber-lm-to-linear-index (- l) m u-modes))
		      (list (- l) m)))))
    res))

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

#+nil
(time
 (defparameter *bla*
   (loop for v from 3d0 below 60d0 by .1d0 collect
	(let ((lin (step-fiber-eigenvalues-linear v)))
	  (destructuring-bind (umax j) (find-fastest-mode lin)
	    (list (sqrt (- (* v v) (* umax umax))) umax j (length lin) v))))))

#+nil
(with-open-file (s "/run/q/n-min-modes.dat" :direction :output
		   :if-exists :supersede :if-does-not-exist :create)
 (loop for (wmin umax j n) in *bla* and v from 3d0 by .1d0 do
      (format s "~f ~d ~d~%" v j n))) ;; for v=10.2 wmin is very small, for v=30.0 wmin is quite big wmin=6.3


(defun step-fiber-fields (u-modes v &key (scale 1.3d0) rco nco (n (step-fiber-minimal-sampling u-modes v :scale scale)) (debug nil))
  "for proper normalization result must be multiplied with 1/sqrt(r_co^2 n_co sqrt(epsilon_0/mu_0))"
  (declare (values (simple-array double-float 3) &optional))
  (let* ((radial-mode-counts (mapcar #'length u-modes))
	 (azimuthal-mode-count (length radial-mode-counts))
	 (fields  (make-array (list (number-of-modes u-modes) n n) :element-type 'double-float))
	 (r-a (make-array (list n n) :element-type 'double-float)) ;; some arrays that store reusable intermediate results
	 (phi-a (make-array (list n n) :element-type 'double-float))
	 (sin-a (make-array (list (- azimuthal-mode-count 1) n n) :element-type 'double-float))
	 (cos-a (make-array (list (- azimuthal-mode-count 1) n n) :element-type 'double-float))
	 (umax (first (find-fastest-mode (step-fiber-eigenvalues-linear u-modes))))
	 (wmin (sqrt (- (* v v) (* umax umax)))))
    (macrolet ((doplane ((j i) &body body) `(dotimes (,j n) (dotimes (,i n) ,@body))))
      (doplane (j i) (let* ((x (* 2 scale (- i (floor n 2)) (/ 1d0 n)))
			    (y (* 2 scale (- j (floor n 2)) (/ 1d0 n)))
			    (r (sqrt (+ (expt x 2) (expt y 2)))))
		       (setf (aref r-a j i) r   (aref phi-a j i) (atan y x))))
      (loop for l from 1 below  azimuthal-mode-count do
	   (if debug (format t "azimuthal ~d/~d~%" l azimuthal-mode-count))
	   (doplane (j i) (setf (aref sin-a (1- l) j i) (sin (* l (aref phi-a j i)))))
	   (doplane (j i) (setf (aref cos-a (1- l) j i) (cos (* l (aref phi-a j i))))))
      (bessel-j-interp-init :end (* 1.1 umax) :n 2100 :lmax (+ 1 azimuthal-mode-count))
      (bessel-k-scaled-interp-init :start (* .9 wmin) :end (* 1.1 (sqrt 2) scale v) :n 2100 :lmax (+ 1 azimuthal-mode-count))
      (let ((start (get-universal-time)))
	(loop for k in (mapcar #'first ;; sort modes by u starting with ground mode
			       (sort (loop for j across (step-fiber-eigenvalues-linear u-modes) 
					and i from 0 collect
					  (list i j)) #'< :key #'second)) 
	   ;k below (number-of-modes u-modes)
	   do
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
		     (nrad-core (* .5 (- 1 (* (expt (gsll:cylindrical-bessel-j l u) -2) 
					      (gsll:cylindrical-bessel-j (- l 1) u)
					      (gsll:cylindrical-bessel-j (+ l 1) u)))))
		     (nrad-clad (* .5 (- (* (expt
					     (gsll:cylindrical-bessel-k-scaled (abs l) w) -2) 
					    (gsll:cylindrical-bessel-k-scaled (abs (- l 1)) w)
					    (gsll:cylindrical-bessel-k-scaled (abs (+ l 1)) w))
				    1)))
		     (nrad (+ nrad-core nrad-clad))
		     (norm (let ((a (* nphi nrad)))
			     (if (or (< nrad-core 0) (< nrad-clad 0)
				     (< a 0))
				 (progn (break "m ~d l ~d u ~6,3f w ~6,3f co ~8,2,2e cl ~8,2,2,2e" 
					       m l u w nrad-core nrad-clad)
					1d0)
				 (/ (sqrt a)))))
		     (scale-j (/ (jn (abs l) u)))
		     (scale-k (/ (gsl::cylindrical-bessel-k-scaled (abs l) w))))
		(when debug
		 (format t "m ~3d l ~4d u ~6,3f w ~6,3f co ~8,2,2e cl ~8,2,2,2e cl/full ~6,1f%~%" 
			 m l u w  nrad-core nrad-clad (/ (* 100 nrad-clad)  nrad)))
		(doplane (j i) (setf (aref fields k j i)
				     (* norm (cond ((= l 0) 1d0) 
						   ((< l 0) (aref sin-a (- (abs l) 1) j i))
						   (t (aref cos-a (- l 1) j i)))
					(let ((r (aref r-a j i)))
					  (if (<= r 1d0)
					      (* scale-j (bessel-j-interp (abs l) (* u r)))
					      (* scale-k (bessel-k-scaled-interp (abs l) (* w r)))
					      ))))))))))
    fields))

#+Nil
(let*
 ((v 30d0)
  (scale 1.3d0)
  (lambd .0005)
  (nco 1.5)
  (ncl 1.46)
  (k (* 2 pi (/ lambd))) 
  (rho (* v (/ (* k (sqrt (- (expt nco 2) (expt ncl 2)))))))
  (w 66)
  (resol (/ (* 2 scale rho) w))
  (mu0 (* 4d-7 pi))
  (c0 299792458d0)
  )
  (* pi nco (expt rho 2)))

;; integrate(bessel_j(0,r)^2*r,r,0,1)


#+nil
(sb-ext:gc :full t)
#+nil
(room)



#+nil
(time 
 (progn
   (defparameter *bla* nil)
   (defparameter *bla*
     (let* (
	   (start (sb-unix::get-time-of-day))
	   (lambd .0006328)
	   (nco 1.553)
	   (ncl 1.457)
	   (k (* 2 pi (/ lambd))) 
	   ;; diameter of the fiber: ?? is it radius or diameter?
	    (core-radius 25d-3)
	    (v (v lambd nco ncl core-radius))
	   (rho (* 1 core-radius))
	    ;(rho (* v (/ (* k (sqrt (- (expt nco 2) (expt ncl 2)))))))
	   )
       (format t "calculating eigenvalues v=~a~%" v)
       (defparameter *bla-ev* (step-fiber-eigenvalues v)) 
       (format t "ev took ~3d s time~%" (- (sb-unix::get-time-of-day) start))
       (let ((sc 1.4d0))
	(step-fiber-fields *bla-ev* v :scale sc :rco rho :nco nco
			   :n (* 2 4 (step-fiber-minimal-sampling *bla-ev* v :scale sc))
			   :debug t))))
#+nil   (write-pgm "/dev/shm/bla.pgm" (convert-ub8  (create-field-mosaic *bla* *bla-ev* ;:fun #'identity
								  ) :scale .7 ;:offset -.2d0
					     ))))
#+nil
(time
 (write-pgm "/dev/shm/bla.pgm" (convert-ub8  (create-field-mosaic *bla* *bla-ev* :fun #'identity
								) :invert nil :scale .7d0 ;:offset -.2d0
								  )))


(defun create-field-mosaic (fields u-modes &key (fun #'(lambda (x) (expt x 2))))
  (declare (type (simple-array double-float 3) fields)
	   (optimize (speed 3))
	   (values (simple-array double-float 2) &optional))
  (let* ((lmax (length u-modes))
	 (mmax (length (first u-modes)))
	 (n (array-dimension fields 2))
	 (nmodes (number-of-modes u-modes))
	 (a (make-array (list (* (+ lmax (1- lmax)) n)  (* mmax n)) :element-type 'double-float))
	 (lut (fiber-linear-to-lm-index-lut u-modes)))
    (declare (type (simple-array double-float 2) a)
	     (type fixnum n nmodes lmax mmax))
    (loop for k below nmodes do
	 (destructuring-bind (l m) (aref lut k)
	   (declare (type fixnum l m))
	   (dotimes (j n) (dotimes (i n)
			    (setf (aref a (+ j (* n (+ (- lmax 1) l))) (+ i (* n m)))
				  (expt (abs (aref fields k j i)) 2) )))))
    a))



;; http://mathoverflow.net/questions/28669/numerical-integration-over-2d-disk
;; Arthur Stroud, Approximate Calculation of Multiple Integrals.
;; http://people.sc.fsu.edu/~jburkardt/f_src/stroud/stroud.html
;; cubatur for the unit circle
;; 1971 lether a generalized product rule for the unit cirlce
;; http://www.holoborodko.com/pavel/numerical-methods/numerical-integration/cubature-formulas-for-the-unit-disk/

(declaim (optimize (debug 3)))

(defun couple (u-modes j0 j1 v &key scale alpha)
  (destructuring-bind (nl0 m0) (fiber-linear-to-lm-index j0 u-modes)
    (destructuring-bind (nl1 m1) (fiber-linear-to-lm-index j1 u-modes)
     (let* ((lambd .0005)
	    (l0 (abs nl0))
	    (l1 (abs nl1))
	    (nco 1.5)
	    (ncl 1.46)
	    (k (* 2 pi (/ lambd))) 
	    ;; diameter of the fiber:
	    (rco (* v (/ (* k (sqrt (- (expt nco 2) (expt ncl 2)))))))
	    )
       (flet ((mode-norm (l u)
		(let* ((w (sqrt (- (expt v 2) (expt u 2))))
		       (nphi (* pi (if (= l 0) 2 1)))
		       (mu0 (* 4d-7 pi))
		       (c0 299792458d0)
					;(eps0 (/ (* mu0 (expt c0 2))))
		       (nrad-scale (* 1d0 ;(expt rco 2) ;(/ nco (* mu0 c0))
				      ))
		       (nrad-core (* .5 (- 1
					   (* (gsll:cylindrical-bessel-j (- l 1) u)
					      (gsll:cylindrical-bessel-j (+ l 1) u)
					      (expt (gsll:cylindrical-bessel-j l u) -2)))))
		       (nrad-clad (* .5 (- (* (gsll:cylindrical-bessel-k (abs (- l 1)) w)
					      (gsll:cylindrical-bessel-k (abs (+ l 1)) w)
					      (expt 
					       (gsll:cylindrical-bessel-k (abs l) w) -2))
					   1)))
		       (nrad (+ nrad-core nrad-clad))
		       (norm (let ((a (* nphi nrad)))
			       (if (or (< nrad-core 0) (< nrad-clad 0)
				       (< a 0))
				   (progn (break " l ~d u ~6,3f w ~6,3f co ~8,2,2e cl ~8,2,2,2e" 
						 l u w nrad-core nrad-clad)
					  1d0)
				   (/ (sqrt a))))))
		  norm)))
	 (let* ((l2 40d0)
		(delx 4)
		(bend-radius (* .5 (+ delx (/ (expt l2 2) delx))))
		(num-elems 100)
		(del-l (/ l2 num-elems))
		(alpha (if alpha alpha (asin (/ del-l bend-radius))))
		(u0 (elt (elt u-modes (abs l0)) m0))
		(u1 (elt (elt u-modes (abs l1)) m1))
		(w0 (sqrt (- (expt v 2) (expt u0 2))))
		(w1 (sqrt (- (expt v 2) (expt u1 2))))
		(scale-norm (* pi 
			       (mode-norm l0 u0)
			       (mode-norm l1 u1)))
		(scale-in (/ scale-norm 
			     (* (jn (abs l0) u0)
				(jn (abs l1) u1))))
		(scale-out (/ scale-norm 
			      (* (gsll:cylindrical-bessel-k (abs l0) w0)
				 (gsll:cylindrical-bessel-k (abs l1) w1))))
		(ij1 (gsl:integration-qng #'(lambda (r) 
					      (* r (bessel-j-interp l0 (* u0 r))
						 (bessel-j-interp l1 (* u1 r))
						 (bessel-j-interp (+ l0 l1) (* k alpha rco r))))
					  0d0 1d0))
		(ij2 (gsl:integration-qng #'(lambda (r) 
					      (* r (bessel-j-interp l0 (* u0 r))
						 (bessel-j-interp l1 (* u1 r))
						 (bessel-j-interp (abs (- l0 l1)) (* k alpha rco r))))
					  0d0 1d0))
		(ik1 (gsl:integration-qng
		      #'(lambda (r) (* r (bessel-k-scaled-interp l0 (* w0 r))
				  (bessel-k-scaled-interp l1 (* w1 r))
				  (exp (- (+ (* w0 r) (* w1 r)))) 
				  (bessel-j-interp (+ l0 l1) (* k alpha rco r))))
		      1d0 scale))
		(ik2 (gsl:integration-qng
		      #'(lambda (r) (* r (bessel-k-scaled-interp l0 (* w0 r))
				  (bessel-k-scaled-interp l1 (* w1 r))
				  (exp (- (+ (* w0 r) (* w1 r)))) 
				  (bessel-j-interp (abs (- l0 l1)) (* k alpha rco r))))
		      1d0 scale)))
	   
	   (format t "co ~10,5f ~10,5f cl ~12,8f ~12,8f  " 
		   (* scale-in ij1) (* scale-in ij2)
		   (* scale-out ik1) (* scale-out ik2))
	   (expt (abs (+ 
		       (* scale-in
			  (+ (* ij1 (expt (complex 0 1) (+ l0 l1)))
			     (* ij2 (expt (complex 0 1) (abs (- l0 l1))))))	   
		       
		       (* scale-out
			  (+ (* ik1 (expt (complex 0 1) (+ l0 l1)))
			     (* ik2 (expt (complex 0 1) (abs (- l0 l1)))))
			  ))) 2)))))))


#+nil
(let ((v 30d0))
 (defparameter *bla-ev* (step-fiber-eigenvalues v)))

#+nil
(defparameter *plot*
  (let* ((v 30d0)
	 (u-modes *bla-ev*)
	 (lmax (+ 1 (* 2 (length (mapcar #'length u-modes)))))
	 (umax (first (find-fastest-mode (step-fiber-eigenvalues-linear u-modes))))
	 (wmin (sqrt (- (* v v) (* umax umax))))
	 (scale 1.3d0))
    (bessel-j-interp-init :end (* 1.01 v) :n 2100 :lmax lmax)
    (bessel-k-scaled-interp-init :start (* .9 wmin) :end (* 1.1 (sqrt 2) scale v)
				 :n 2100 :lmax lmax)
    (terpri)
    (loop for n from 0 below (number-of-modes u-modes) collect
	 (loop for m from 0 below (number-of-modes u-modes) collect
	      (let ((x  (couple u-modes n m v :scale 2d0 :alpha 10e-3)))
		(format t "i n ~3d m ~3d ~12,5f%~%" n m (* 100 x))
		x)))) )


#+nil
(loop for e in *plot* and f in *bla-coef* and i from 0 do ;; divide both integration methods for comparison
     (loop for ee in e and ff in f and j from 0 collect
	  (let ((val (/ ee ff)))
	    (format t "/ ~3d ~3d ~3,8f~%" i j val)
	    val)))

#+nil
(loop for i from -3 upto 3 collect (list i
				    (expt (complex 0 1) i)))

#+nil
(with-open-file (s "/run/q/bla.dat" :direction :output :if-exists :supersede
		   :if-does-not-exist :create
		   )
  (format s "~{~{~f ~}~%~}" *plot*))

#+nil
(with-open-file (s "/run/q/char-v30-l10.dat" :direction :output :if-exists :supersede
		   :if-does-not-exist :create
		   )
  (let ((l 10)
	(v 30d0))
   (loop for u from 1d0 below v by .001d0 do
	(format s "~f ~f ~f~%" u (char-step-index-fiber u v l) (log (abs (jn l u)))))))

(defun calculate-bend-wedge (&key (v 32d0) (n 100) (scale 2d0) alpha)
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
	(alpha (if alpha alpha (asin (/ del-l bend-radius))))
	(wedge (make-array (list n n) :element-type '(complex double-float))))
   (dotimes (i n)
     (dotimes (j n)
       (setf (aref wedge j i) (exp (complex 0d0 (* k alpha i resol))))))
   (values wedge resol nco rho)))
#+nil
(calculate-bend-wedge :scale 3d0 :n 150 :v 30d0)

(defun calculate-couple-coeffs (fields &key scale v radius alpha)
  ;; i might have to figure out the proper sampling by calculating a
  ;; high resolution cross section through the highest mode
;  (declare (optimize (speed 3)))
  (declare (type (simple-array double-float 3) fields))
  (destructuring-bind (nmodes h w) (array-dimensions fields)
    (multiple-value-bind (wedge resol nco rco) (calculate-bend-wedge :n w :v v :scale scale :alpha alpha)
      
      (declare (type (simple-array (complex double-float) 2) wedge))
      (defparameter *resol* resol)
      (let* ((couple-coeffs (make-array (list nmodes nmodes)
					:element-type 'double-float)))
	(declare (type (simple-array double-float 2) couple-coeffs))
	(loop for a below 7 collect
	  (loop for b below 7 collect
	       (prog1
		   (setf (aref couple-coeffs b a) 
			 (* resol resol
			    (expt
			     (abs
			      (* (/ (* rco rco nco))
			       (loop for j below h sum
				    (loop for i below w sum
					 #+nil
					 (* (aref fields a j i)
					    (aref fields b j i)
					    (aref wedge j i))
					 (let* ((x (* 2 scale (- i (floor w 2)) (/ 1d0 w)))
						(y (* 2 scale (- j (floor h 2)) (/ 1d0 h)))
						(r (sqrt (+ (expt x 2) (expt y 2)))))
					   (if (< r 1d0)
					       (* (aref fields a j i)
						  (aref fields b j i)
						  (aref wedge j i))
					       0d0))))))
			     2)))
		 (format t "s ~3d ~3d ~3,8f ~%" a b (* 100 (aref couple-coeffs b a))))))))))
#+nil
(time (defparameter *bla-coef* (calculate-couple-coeffs *bla* :v 30d0 :scale 2d0 :radius 2d0 :alpha 30d-3)))
#+nil
(time  (write-pgm "/run/q/bla-coef.pgm" (convert-ub8  (convert-df *bla-coef*))))
#+nil
(time  (write-pgm "/run/q/bla-coef2.pgm" (convert-ub8  (convert-df *bla-coef*) :scale 1e5)))
#+nil
(time  (write-pgm "/run/q/bla-coef-phase.pgm" (convert-ub8  (convert-df *bla-coef* :fun #'phase))))



#+nil
(check #'gsll:cylindrical-bessel-k 1 40 3d0)
#+nil
(check #'gsll:cylindrical-bessel-k 2 10 3d0)


#+nil
(defparameter *res* 
 (let ((res ()))
   (destructuring-bind (nmodes h w) (array-dimensions *bla*)
     (loop for jmode in (mapcar #'first ;; sort modes by u starting with ground mode
				(sort (loop for j across (step-fiber-eigenvalues-linear *bla-ev*) 
					 and i from 0 collect
					   (list i j)) #'< :key #'second)) 
	do
	  (destructuring-bind (l m) (fiber-linear-to-lm-index jmode *bla-ev*)
	    (let* ((v 10d0)
		   (scale 1.3d0)
		   (lambd .0005)
		   (nco 1.5)
		   (ncl 1.46)
		   (k (* 2 pi (/ lambd))) 
		   ;; diameter of the fiber:
		   (rho (* v (/ (* k (sqrt (- (expt nco 2) (expt ncl 2)))))))
		   ;; resolution of the field in mm/px:
		   (resol (/ (* 2 scale rho) w))
		   (u (elt (elt *bla-ev* (abs l)) m))
		   (w (sqrt (- (expt v 2) (expt u 2))))
		   (nphi (* pi (if (= l 0) 2 1)))
		   (mu0 (* 4d-7 pi))
		   (c0 299792458d0)
		   ;;(eps0 (/ (* mu0 (expt c0 2))))
		   (nrad (* .5 pi nco (/ (* c0 mu0)) (expt (/ v u) 2)
			    (expt (check #'gsll:cylindrical-bessel-k-scaled 1 (abs l) w) -2) 
			    (check #'gsll:cylindrical-bessel-k-scaled 2 (abs (- l 1)) w)
			    (check #'gsll:cylindrical-bessel-k-scaled 3 (abs (+ l 1)) w)))
		   (core-numerical (/ (gsl:integration-qng 
				       #'(lambda (r) (* r (expt 
						      (check #'gsll:cylindrical-bessel-j 4 l (* u r)) 2)))
				       0d0 1d0)
				      (expt (check #'gsll:cylindrical-bessel-j 5 l u) 2)))
		   (core-analytical (* .5  (- 1 
					      (* (check #'gsll:cylindrical-bessel-j 51 (- l 1) u) 
						 (check #'gsll:cylindrical-bessel-j 52 (+ l 1) u)
						 (expt (check #'gsll:cylindrical-bessel-j 53 l u) -2)))))
		   (clad-numerical (* (check #'gsl:integration-qagiu 
					     61
					     #'(lambda (r) (* r (expt
							    (check #'gsll:cylindrical-bessel-k 6 l 
								   (* w r)) 2)))
					     1d0 1d-15 1d-12 5000)
				      (expt (check #'gsll:cylindrical-bessel-k 7 l w) -2)))
		   ;; this is not stable for the ground mode, and gives a negative result
		   (clad-analytical (* .5 (abs (- 1 (* (check #'gsll:cylindrical-bessel-k 8 (- l 1) w)
						   (check #'gsll:cylindrical-bessel-k 9 (+ l 1) w)
						   (expt (check #'gsll:cylindrical-bessel-k 10 l w) -2))))))
		   (full-analytical (* .5 (expt (/ v u) 2)
				       (check #'gsll:cylindrical-bessel-k 11 (- l 1) w) 
				       (check #'gsll:cylindrical-bessel-k 12 (+ l 1) w) 
				       (expt (check #'gsll:cylindrical-bessel-k 13 l w) -2)))
		   (core-simple (loop for j below h sum
				     (loop for i below w sum
					  (let* ((x (* 2 scale (- i (floor w 2)) (/ 1d0 w)))
						 (y (* 2 scale (- j (floor h 2)) (/ 1d0 h)))
						 (r (sqrt (+ (expt x 2) (expt y 2)))))
					    (if (<= r 1d0)
						(expt (abs (aref *bla* jmode j i)) 2)
						0d0)))))
		   (clad-simple (loop for j below h sum
				     (loop for i below w sum
					  (let* ((x (* 2 scale (- i (floor w 2)) (/ 1d0 w)))
						 (y (* 2 scale (- j (floor h 2)) (/ 1d0 h)))
						 (r (sqrt (+ (expt x 2) (expt y 2)))))
					    (if (<= 1d0 r)
						(expt (abs (aref *bla* jmode j i)) 2)
						0d0))))))
	      
	      (format 
	       t "~3d ~6,3f ~6,3f co ~6,3f ~6,3f ~6,3f  cl ~8,2,2e ~8,2,2e ~8,2,2e cl/full ~9,1,2e full ~6,3f ~6,3f~%"
	       jmode u w core-numerical core-analytical (* resol resol core-simple)
	       clad-numerical clad-analytical (- clad-numerical clad-analytical) (/ clad-numerical (+ core-numerical clad-numerical)) ; (* resol resol clad-simple)
	       (+ core-numerical clad-numerical) full-analytical)
	      (multiple-value-bind (a ae) (gsll:cylindrical-bessel-k (- l 1) w)
		(multiple-value-bind (b be) (gsll:cylindrical-bessel-k l w)
		  (multiple-value-bind (c ce) (gsll:cylindrical-bessel-k (+ l 1) w)
		    (let ((ar (/ ae a))
			  (br (/ be b))
			  (cr (/ ce c)))
		     (push (list jmode ae be ce
				 (* .5 (- 1 (/ (* a c) 
					  (* b b))))
				 (* .5 (+ (* (abs (/ c (* b b))) ae)
				     (* (abs (/ a (* b b))) ce)
				     (* (abs (* 2 a c (expt b -3))) be))))
			   res))))))))
     (reverse res))))




#+nil
(loop for (jmode ae be ce res res-err) in *res* do
     (format t "~3d e ~8,1,2e ~8,1,2e ~8,1,2e res ~10,2,2e err ~10,2,2e ~%"
	     jmode ae be ce res res-err))

;; diff(1-a*c/b^2,a);
					; => |-c/b|*ae
;; diff(1-a*c/b^2,c);
					; => |-a/b|*ce
;; diff(1-a*c*b^(-2),b);
					; => |2*a*c*b^(-3)|*be

;; integrate(bessel_k(0,10*r)^2*r,r,1,inf);
;; 
;; quad_qagi(bessel_k(0,1*r)^2*r/bessel_k(0,.1),r,1,inf);
;; w:9.758; f(r):= bessel_k(0,w*r)^2*r/bessel_k(0,w)^2 ; a:[quad_qag(f(r),r,1,2,6), quad_qag(f(r),r,1,4,6), quad_qag(f(r),r,1,10,6), quad_qagi(f(r),r,1,inf), .5*(1-bessel_k(1,w)^2/bessel_k(0,w)^2)] ;
;; 


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

(/ (* 66 66) (* 2 pi))
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


;; shemirani 2009y
;; Due to symmetries enforced by the bends in and directions, it is
;; easiest to find the coupling coefficients in Carte- sian
;; coordinates, using the eigenmodes of the ideal fiber, which are
;; orthonormal Hermite–Gaussian function

#+nil
(defun solve-couple-into-lp-modes (matrix)
  "Solve the linear equation using SVD with the supplied matrix and
   a right-hand side vector which is the reciprocal of one more than
   the index."
  (let ((dim (dim0 matrix)))
    (multiple-value-bind (u q d)
        (SV-decomposition (copy matrix))
      (SV-solve u q d (gsll::create-rhs-vector dim)))))


