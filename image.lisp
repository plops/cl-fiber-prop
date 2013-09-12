(in-package :cl-fiber-prop)

(defun convert-ub8 (a &key (scale 1d0 scale-p) (offset 0d0) (debug nil))
  (declare (type (simple-array double-float 2) a)
	   (type double-float scale)
	   (optimize (speed 3))
	   (values (simple-array (unsigned-byte 8) 2) &optional))
  (let ((b (make-array (array-dimensions a)
		       :element-type '(unsigned-byte 8)))
	(scale2 scale)
	(offset2 offset))
    (declare (type double-float scale2 offset2)
	     (type (simple-array (unsigned-byte 8) 2) b))
    (unless scale-p
      (let ((ma (reduce #'max (sb-ext:array-storage-vector a)))
	    (mi (reduce #'min (sb-ext:array-storage-vector a))))
	(setf scale2 (if (= ma mi)
			 1d0
			 (/ (- ma mi)))
	      offset2 mi)
	(when debug
	 (break "~a" (list 'scale scale2 'offset offset2 'max ma)))))
    (destructuring-bind (h w) (array-dimensions a)
      (declare (type fixnum h w))
      (dotimes (i w)
	(dotimes (j h)
	  (setf (aref b j i) (min 255 (max 0 (floor (* 255 scale2 (- (aref a j i) offset2)))))))))
    b))

(defun convert-df (a &key (fun #'abs))
  (declare (type (simple-array (complex double-float) 2) a)
	   (values (simple-array double-float 2) &optional))
  (let ((b (make-array (array-dimensions a)
		       :element-type 'double-float)))
    (destructuring-bind (h w) (array-dimensions a)
      (dotimes (i w)
	(dotimes (j h)
	  (setf (aref b j i) (funcall fun (aref a j i))))))
    b))

(defun write-pgm (filename img)
  (declare (simple-string filename)
           ((array (unsigned-byte 8) 2) img)
           (values null &optional))
  (destructuring-bind (h w)
      (array-dimensions img)
    (declare ((integer 0 65535) w h))
    (with-open-file (s filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%255~%" w h))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :append)
      (let ((data-1d (make-array 
                      (* h w)
                      :element-type '(unsigned-byte 8)
                      :displaced-to img)))
        (write-sequence data-1d s)))
    nil))
