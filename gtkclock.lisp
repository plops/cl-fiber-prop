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


#+nil
(let ((a (make-array (list 256 256) :element-type '(unsigned-byte 8))))
  (dotimes (j 256)
    (dotimes (i 256)
      (setf (aref a j i) j)))
  (update-img a)
  nil)

(let ((img nil))
  (defun update-img (a)
    (setf img a))
  (defun surface-from-lisp-array ()
    (when img
      (destructuring-bind (h w) (array-dimensions img)
	(let* ((format :argb32)
	       (stride (cairo-format-stride-for-width format w))
	       (a (make-array (list h stride) :element-type '(unsigned-byte 8))))
	  (dotimes (j h)
	    (dotimes (i w)
	      (let ((v (max 0 (min 255 (aref img j i)))))
	       (setf (aref a j (+ 0 (* 4 i))) v
		     (aref a j (+ 1 (* 4 i))) v
		     (aref a j (+ 2 (* 4 i))) v
		     (aref a j (+ 3 (* 4 i))) 255 ;; alpha
		     ))))
	  (cairo-image-surface-create-for-data 
	  (sb-sys:vector-sap (sb-ext:array-storage-vector a))
	  format w h stride))))))

(progn
 (defun draw-clock-face (widget cr clock)
   (let ((cr (pointer cr))
	 (window (gtk-widget-window widget))
	 (surf (surface-from-lisp-array)))
     (cairo-set-source-rgb cr 1.0 1.0 1.0)
          (cairo-scale cr .5 .5)
     (when surf
       (cairo-set-source-surface cr surf 0 0))

     (cairo-paint cr)
     (let* ((radius (* .5 207))
	    (x (+ 1054 radius))
	    (y (+ 148 radius))
	    )
       (cairo-arc cr x y radius 0 (* 2 pi))
					;(cairo-set-source-rgb cr 1 1 1)
					;(cairo-fill-preserve cr)
       (cairo-set-source-rgb cr 1 1 1)
       (cairo-stroke cr)
       (let ((angle (* (/ pi 30) (first (clock-face-time clock))
		       )))
	 (cairo-save cr)
	 (cairo-set-source-rgb cr 1 0 0)
	 (cairo-move-to cr x y)
	 (cairo-line-to cr
			(+ x (* radius (sin angle)))
			(+ y (* radius (- (cos angle)))))
	 (cairo-stroke cr)
	 (cairo-restore cr)))
    
     (when surf
       (cairo-surface-destroy surf))
     (cairo-destroy cr)
     t))

 (defparameter *draw-clock-face* #'draw-clock-face))

(defmethod initialize-instance :after ((clock clock-face) &key &allow-other-keys)
  (g-timeout-add 1000 (lambda ()
			(setf (clock-face-time clock)
			      (multiple-value-list (get-decoded-time)))
			(gtk-widget-queue-draw clock)
			+g-source-continue+))
  (g-signal-connect clock "draw"
		    (lambda (widget cr)
		      (funcall *draw-clock-face* widget cr clock))))


(defun run ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
   (within-main-loop
     (let ((window (make-instance 'gtk-window :title "clock"
				  :default-width (/ 1920 2)
				  :default-height (/ 1080 2)
				  :border-width 12))
	   (paned (make-instance 'gtk-paned :orientation :horizontal :position 100))
	   (paned-right (make-instance 'gtk-paned :orientation :vertical :position 300))
	   (clock (make-instance 'clock-face))
	   (frame1 (make-instance 'gtk-frame :label "bal"))
	   (scrolled (make-instance 'gtk-scrolled-window
				    :border-width 1
				    :hscrollbar-policy :automatic
				    :vscrollbar-policy :always))
	   (table (make-instance 'gtk-table :n-rows 10
				 :n-columns 10
				 :row-spacing 0
				 :column-spacing 0
				 :homogeneous nil)))
       (defparameter *clock* clock)
       (g-signal-connect window "destroy"
			 (lambda (widget) (leave-gtk-main)))
       (gtk-container-add window paned)
       (gtk-paned-add1 paned clock)
       (gtk-paned-add2 paned paned-right)
       (progn
	 (gtk-scrolled-window-add-with-viewport scrolled table)
	 (dotimes (i 10)
	   (dotimes (j 10)
	     (gtk-table-attach table
			       (let* ((label (make-instance 'gtk-label
							    :use-markup t
							    :label (format nil "<span font='5'>~2,'0d|~2,'0d</span>" i j)))
				      (button (make-instance 'gtk-button)))
				 (gtk-container-add button label)
				 button)
			       i (+ i 1) j (+ j 1)))))
       (gtk-paned-add1 paned-right scrolled)
       (gtk-paned-add2 paned-right frame1)
       (gtk-widget-show-all window)))))
#+nil
(run)
