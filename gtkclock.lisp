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
  (push-pic 10 100 a "j")

  (dotimes (j 256)
    (dotimes (i 256)
      (setf (aref a j i) i)))
  (push-pic 20 300 a "i")
nil)


(defun surface-from-lisp-array (img)
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
       format w h stride))))

(defclass pic ()
  ((surface :accessor surface :initarg :surface)
   (x :accessor pic-x :initarg :pic-x)
   (y :accessor pic-y :initarg :pic-y)
   (name :accessor pic-name :initarg :pic-name)))

(defmethod print-object ((pic pic) stream)
  (format stream "#<pic: ~a ~d,~d>" (pic-name pic) (pic-x pic) (pic-y pic)))

(let ((pics nil))
  (defun clear-pics ()
    (dolist (s pics)
      (cairo-surface-destroy (surface s)))
    (setf pics nil))
  (defun push-pic (x y a name)
    (push (make-instance 'pic :surface (surface-from-lisp-array a)
			 :pic-x x :pic-y y :pic-name name) pics))
  (defun get-pics ()
    pics))
#+nil
(surface (first (get-pics)))
#+nil
(get-pics)
#+nil
(clear-pics)
(defparameter *adjustments* nil)

#+nil
(gtk-container-get-children (cdr (assoc 'rt-sb *adjustments*
				  )))

(defun spin-button-value (widget-name)
  (let ((hbox-children (gtk-container-get-children (cdr (assoc widget-name *adjustments*)))))
    (when hbox-children
     (gtk-adjustment-get-value (gtk-spin-button-get-adjustment (second hbox-children))))))

(defun button-active-p (widget-name)
  (let ((button-widget (cdr (assoc widget-name *adjustments*))))
    (when button-widget
     (gtk-toggle-button-active button-widget))))

(progn
 (defun draw-clock-face (widget cr clock)
   (declare (ignorable widget))
   (let ((cr (pointer cr))
	 ;(window (gtk-widget-window widget))
	 )
     (cairo-set-source-rgb cr 1.0 1.0 1.0)
     (cairo-scale cr 1 1)
     

     (dolist (pic (get-pics))
       (when (button-checked-p (pic-name pic))
	 (cairo-set-source-surface cr (surface pic) (pic-x pic) (pic-y pic))
	 (cairo-paint cr)))
         
     (when *adjustments*
       (let* ((radius (or (spin-button-value 'radius) 0d0))
	      (x (or (spin-button-value 'xpos) 0d0))
	      (y (or (spin-button-value 'ypos) 0d0)))
	(cairo-arc cr x y radius 0 (* 2 pi))
					;(cairo-set-source-rgb cr 1 1 1)
					;(cairo-fill-preserve cr)
	(cairo-set-source-rgb cr 1 0 1)
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
	  (cairo-restore cr))))
    
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


#+nil
(gtk-adjustment-get-value (cdr (assoc 'kradius *adjustments*)))
#+nil
(gtk-widget-destroy (cdr (assoc 'rb-ft *adjustments*)))

(defparameter *frame1* nil)

(defun run ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let ((window (make-instance 'gtk-window :title "holography"
				   :default-width (/ 1920 2)
				   :default-height (/ 1080 2)
				   :border-width 12
				   :type :toplevel))
	    (paned (make-instance 'gtk-paned :orientation :horizontal :position 400))
	    (paned-right (make-instance 'gtk-paned :orientation :vertical :position 300))
	    )
	(g-signal-connect window "destroy"
			  (lambda (widget)
			    (declare (ignorable widget))
			    (leave-gtk-main)))
	(let* ((ghb (make-instance 'gtk-handle-box :snap-edge :top
				   :shadow-type :in :handle-position :left))
	       (scrolled (make-instance 'gtk-scrolled-window
					:border-width 1
					:hscrollbar-policy :automatic
					:vscrollbar-policy :automatic))
	       (clock (make-instance 'clock-face)))
	  (defparameter *canvas* clock)
	  (gtk-scrolled-window-add-with-viewport scrolled clock)
	  (setf (gtk-widget-size-request clock) (list 1920 1080))
	  ;(setf (gtk-widget-size-request scrolled) (list 200 200))
	  (gtk-container-add window paned)
	  (gtk-container-add ghb scrolled)
	  (gtk-paned-add1 paned ghb)
	  (gtk-paned-add2 paned paned-right)
	  (let ((scrolled (make-instance 'gtk-scrolled-window
					 :border-width 1
					 :hscrollbar-policy :automatic
					 :vscrollbar-policy :automatic))
		(table (make-instance 'gtk-table :n-rows 10
				      :n-columns 10
				      :row-spacing 0
				      :column-spacing 0
				      :homogeneous nil))
		)
	    (gtk-scrolled-window-add-with-viewport scrolled table)
	    (loop for j below 93 by 30 do
		 (loop for i below 118 by 30 do
		      (gtk-table-attach table
					(let* ((label (make-instance 'gtk-label
								     :use-markup t
								     :label (format nil "<span font='5'>~2,'0d|~2,'0d</span>" i j)))
					       (button (make-instance 'gtk-button)))
					  (gtk-container-add button label)
					  button)
					i (+ i 1) j (+ j 1))))
					;(setf (gtk-widget-size-request scrolled) (list 200 200))
	    (gtk-paned-add1 paned-right scrolled)
	    (progn
	      (setf *adjustments* nil)
	      (labels ((spin-box (name value upper)
			 (let* ((hb (make-instance 'gtk-box :orientation :horizontal))
				(lab (make-instance 'gtk-label
						    :label (format nil "~s" name)))
				(adj (make-instance 'gtk-adjustment
						    :value (* 1d0 value)
						    :lower 0d0
						    :upper (* 1d0 upper)
						    :step-increment 1d0
						    :page-increment 10d0
						    :page-size 0d0))
				(sb (make-instance 'gtk-spin-button :adjustment
						   adj
						   :climb-rate 0
						   :digits 1
						   :wrap t)))
			   (gtk-spin-button-set-value sb value)
			   (gtk-box-pack-start hb lab)
			   (gtk-box-pack-start hb sb)
			   (g-signal-connect sb "value-changed"
					     (lambda (adjustment)
					       (declare (ignorable adjustment))
					       (gtk-widget-queue-draw clock)))
			   (push (cons name hb) *adjustments*)
			   hb)))
		(let* ((frame1 (make-instance 'gtk-frame :label "settings"))
		       (vbox (make-instance 'gtk-box :orientation :vertical))
		       (rb-ft (gtk-check-button-new-with-label "ft"))
		       (rb-fit (gtk-check-button-new-with-label "fit"))
		       (xpos (spin-box 'xpos 1157.5 (- 1920 1)))
		       (ypos (spin-box 'ypos 251.5 (- 1080 1)))
		       (radius (spin-box 'radius 103.5 500))
		       (kxpos (spin-box 'kxpos 100 (- 1920 1)))
		       (kypos (spin-box 'kypos 100 (- 1080 1)))
		       (kradius (spin-box 'kradius 100 500)))
		  ;; (push (cons 'rb-ft rb-ft) *adjustments*)
		  ;; (push (cons 'rb-fit rb-fit) *adjustments*)
		  ;; (g-signal-connect rb-ft "clicked"
		  ;; 		    (lambda (widget) (declare (ignorable widget))
		  ;; 		      (gtk-widget-queue-draw clock)))
		  ;; (g-signal-connect rb-fit "clicked"
		  ;; 		    (lambda (widget) (declare (ignorable widget))
		  ;; 		      (gtk-widget-queue-draw clock)))
		  ;; (loop for (name . widget) in *adjustments* do
		  ;;      (format t "~a~%" name)
		  ;;      (gtk-box-pack-start vbox widget)
		  ;;      )
		  (defparameter *vbox* vbox)
		  (gtk-container-add frame1 vbox)
		  (defparameter *frame1* frame1)
		  (gtk-paned-add2 paned-right frame1))))))
	(gtk-widget-show-all window)))))



#+nil
(run)

#+nil
(gtk-widget-destroy *vbox*)

#+nil
(type-of (first (gtk-container-get-children (first (gtk-container-get-children *frame1*)))))

#+nil
(let ((vbox (first (gtk-container-get-children *frame1*))))
  (when (and vbox (eq 'gtk-box (type-of vbox)))
   (gtk-widget-destroy vbox)))

(defun button-checked-p (name)
  (let ((vbox (first (gtk-container-get-children *frame1*))))
    (when (and vbox (eq 'gtk-box (type-of vbox)))
     (let ((buttons (gtk-container-get-children vbox)))
       (when buttons
	 (let ((button-widget (find-if
			       #'(lambda (x) (string= name (gtk-button-label x)))
			       buttons)))
	   (when (and button-widget (eq 'gtk-check-button (type-of button-widget))) 
	     (gtk-toggle-button-active button-widget))))))))

#+nil
(let ((vbox (make-instance 'gtk-box :orientation :vertical)))
  (defparameter *vbox* vbox)
  (loop for p in (get-pics) do
       (let ((button (gtk-check-button-new-with-label (pic-name p))))
	 (gtk-box-pack-start vbox button)
	 (g-signal-connect button "toggled"
			 (lambda (adjustment)
			   (gtk-widget-queue-draw *canvas*)))))
    (gtk-container-add *frame1* vbox)
    (gtk-widget-show-all *frame1*))



#+nil
(list *vbox*
      (gtk-container-get-children *frame1*))

#+nil
(gtk-container-get-children *vbox*)


#+nil
(gtk-widget-show-all *frame1*)
#+nil
(gtk-widget-show-all *vbox*)
