(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-cffi-gtk))

(defpackage :myclock 
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :cl))

(in-package :myclock)

(defclass clock-face (gtk-drawing-area)
  ((time :initarg :time
	 :initform (multiple-value-list (get-decoded-time))
	 :accessor clock-face-time))
  (:metaclass gobject-class))

(defun surface-from-lisp-array (w h)
  (let* ((format :argb32)
	 (stride (cairo-format-stride-for-width format w))
	 (a (make-array (list h stride) :element-type '(unsigned-byte 8))))
    (dotimes (j h)
      (dotimes (i w)
	(setf (aref a j (+ 0 (* 4 i))) (min 255 (max 0 i))
	      (aref a j (+ 1 (* 4 i))) (min 255 (max 0 i))
	      (aref a j (+ 2 (* 4 i))) (min 255 (max 0 j))
	      (aref a j (+ 3 (* 4 i))) (min 255 (max 0 j)))))
    (cairo-image-surface-create-for-data 
       (sb-sys:vector-sap (sb-ext:array-storage-vector a))
       format w h stride)))

(defmethod initialize-instance :after ((clock clock-face) &key &allow-other-keys)
  (g-timeout-add 1000 (lambda ()
			(setf (clock-face-time clock)
			      (multiple-value-list (get-decoded-time)))
			(gtk-widget-queue-draw clock)
			+g-source-continue+))
  (g-signal-connect clock "draw"
		    (lambda (widget cr)
		      (let ((cr (pointer cr))
			    (window (gtk-widget-window widget))
			    (surf (surface-from-lisp-array 128 128)))
			(cairo-set-source-rgb cr 1.0 1.0 1.0)
			(cairo-set-source-surface cr surf 0 0)
			(cairo-paint cr)
			(let* ((x (/ (gdk-window-get-width window) 2))
			       (y (/ (gdk-window-get-height window) 2))
			       (radius (- (min x y) 12)))
			  (cairo-arc cr x y radius 0 (* 2 pi))
			  (cairo-set-source-rgb cr 1 1 1)
			  (cairo-fill-preserve cr)
			  (cairo-set-source-rgb cr 0 0 0)
			  (cairo-stroke cr)
			  (let ((angle (* (/ pi 30) (first (clock-face-time clock)))))
			    (cairo-save cr)
			    (cairo-set-source-rgb cr 1 0 0)
			    (cairo-move-to cr x y)
			    (cairo-line-to cr
					   (+ x (* radius (sin angle)))
					   (+ y (* radius (- (cos angle)))))
			    (cairo-stroke cr)
			    (cairo-restore cr)))
			
			(cairo-surface-destroy surf)
			(cairo-destroy cr)
			t))))
(defun run ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
   (within-main-loop
     (let ((window (make-instance 'gtk-window :title "clock"
				  :default-width 128
				  :default-height 128
				  ))
	   (clock (make-instance 'clock-face)))
       (g-signal-connect window "destroy"
			 (lambda (widget) (leave-gtk-main)))
       (gtk-container-add window clock)
       (gtk-widget-show-all window)))))
#+nil
(run)
