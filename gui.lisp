(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-cffi-gtk))
#+nil
(sb-ext:save-lisp-and-die "/home/martin/cl-fiber-prop/sbcl-gtk-gsl.core")

(defpackage :fiber-gui 
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :cl))

(in-package :fiber-gui)

(defun surface-from-lisp-array (img)
  (declare (type (simple-array (unsigned-byte 8) 2) img))
  (destructuring-bind (h w) (array-dimensions img)
    (let* ((format :argb32)
	   (stride (cairo-format-stride-for-width format w))
	   (a (make-array (list h stride) :element-type '(unsigned-byte 8))))
      (declare (type (simple-array (unsigned-byte 8) 2) a))
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
   (array :accessor pic-array :initarg :pic-array)
   (x :accessor pic-x :initarg :pic-x)
   (y :accessor pic-y :initarg :pic-y)
   (name :accessor pic-name :initarg :pic-name)))

(defclass fiber-image ()
  ((xpos :accessor fiber-image-xpos :initarg :xpos)
   (ypos :accessor fiber-image-ypos :initarg :ypos)
   (radius :accessor fiber-image-radius :initarg :radius)))


(defmethod print-object ((pic pic) stream)
  (format stream "#<pic: ~a ~d,~d>" (pic-name pic) (pic-x pic) (pic-y pic)))

(let ((pics nil))
  (defun clear-pics ()
    (dolist (s pics)
      (cairo-surface-destroy (surface s)))
    (setf pics nil))
  (defun push-pic (x y a name)
    (push (make-instance 'pic :surface (surface-from-lisp-array a)
			 :pic-array a
			 :pic-x x :pic-y y :pic-name name) pics))
  (defun get-pics ()
    pics))

#+nil
(cairo-surface-reference (surface (first (get-pics))))

#+nil
(defun spin-button-value (widget-name)
  (let ((hbox-children (gtk-container-get-children (cdr (assoc widget-name *adjustments*)))))
    (when hbox-children
      (gtk-adjustment-get-value (gtk-spin-button-get-adjustment (second hbox-children))))))

#+nil
(gtk-label-label (first (gtk-container-get-children (first (gtk-container-get-children *spin-vbox*)))))
#+nil
(gtk-container-get-children *spin-vbox*)



(defun ensure-type (type obj)
  "Returns obj if it is of the expected type. Otherwise stop execution."
  (if (eq type (type-of obj))
      obj
      (break "unexpected type ~a~%" (list type obj))))

(defparameter *spin-vbox* nil)
(defparameter *view* nil)
(defparameter *renderer* nil)

;; (hbox (label spinbutton))
(defun spin-button-value (widget-symbol)
  nil
  #+nil
 (let* ((hboxes (gtk-container-get-children *spin-vbox*))
	(hbox (find-if #'(lambda (hbox)
			   (string= (symbol-name widget-symbol)
				    (gtk-label-label
				     (ensure-type 'gtk-label
						  (first (gtk-container-get-children hbox))))))
		       (gtk-container-get-children *spin-vbox*))))
   (gtk-spin-button-value (ensure-type 'gtk-spin-button (second (gtk-container-get-children (ensure-type 'gtk-box hbox)))))))


#+nil
(spin-button-value 'radius)

#+nil
(defun spin-button-value (widget-name)
  nil)

#+nil
(button-checked-p (pic-name (first (get-pics))))
#+nil
(spin-button-value 'radius)


(progn
 (defun draw-canvas (widget cr)
   (declare (ignorable widget))
   (let ((cr (pointer cr))
	 #+nil (window (gtk-widget-window widget))
	 )
     (cairo-set-source-rgb cr 1.0 1.0 1.0)
     ;(cairo-scale cr .2 .2)
     
     (dolist (pic (get-pics))
       (when (button-checked-p (pic-name pic))
	 (cairo-save cr)
	 #+nil
	 (cairo-set-source-surface cr (surface pic) (pic-x pic) (pic-y pic))
	 (let ((surf (surface-from-lisp-array (pic-array pic))))
	  (cairo-set-source-surface cr surf (pic-x pic) (pic-y pic))
	  )
	 (cairo-paint cr)
	 (cairo-restore cr)))

     
     (let* ((radius (or (tree-gui::get-tree-value 'fiber1 'cam1 'radius)
		        40d0))
	    (x (or (tree-gui::get-tree-value 'fiber1 'cam1 'xpos) 120d0))
	    (y (or (tree-gui::get-tree-value 'fiber1 'cam1 'ypos) 110d0)))
       (cairo-arc cr x y radius 0 (* 2 pi))
					;(cairo-set-source-rgb cr 1 1 1)
					;(cairo-fill-preserve cr)
       (cairo-set-source-rgb cr 1 0 1)
       (cairo-stroke cr))
    
     (cairo-destroy cr)
     
     t))
 (defparameter *draw-canvas* #'draw-canvas))

(defparameter *frame1* nil)
(defparameter *canvas* nil)

#+nil
(gtk-widget-queue-draw *canvas*)


(defun add-spinbox-to-vbox (container name value upper canvas)
  "Make a horizontal box containing a label on the left and a spin
button right of it and add it to container. Changing a value will
signal canvas."
  (let* ((hb (make-instance 'gtk-box :orientation :horizontal))
     (lab (make-instance 'gtk-label
                 :label (symbol-name name)))
	 (adj (make-instance 'gtk-adjustment
                 :value (* 1d0 value)
                 :lower 0d0
                 :upper (* 1d0 upper)
                 :step-increment 1d0
                 :page-increment 10d0
                 :page-size 0d0))
     (sb (make-instance 'gtk-spin-button :adjustment adj
                :climb-rate 0
                :digits 1
                :wrap t)))
    (gtk-spin-button-set-value sb value)
    (gtk-box-pack-start hb lab)
    (gtk-box-pack-start hb sb)
    (g-signal-connect sb "value-changed"
              (lambda (adjustment)
            (declare (ignorable adjustment))
            (gtk-widget-queue-draw canvas)))
    (gtk-box-pack-start container hb)
    hb))


(defun add-spinbox-to-grid (grid name value upper canvas row col)
  "Make a horizontal box containing a label on the left and a spin
button right of it and add it to the grid. Changing a value will
signal canvas."
  (let* ((lab (make-instance 'gtk-label
			     :label (symbol-name name)))
	 (adj (make-instance 'gtk-adjustment
			     :value (* 1d0 value)
			     :lower 0d0
			     :upper (* 1d0 upper)
			     :step-increment 1d0
			     :page-increment 10d0
			     :page-size 0d0))
	 (sb (make-instance 'gtk-spin-button :adjustment adj
			    :climb-rate 0
			    :digits 1
			    :wrap t)))
    (gtk-spin-button-set-value sb value)
    (g-signal-connect sb "value-changed"
              (lambda (adjustment)
            (declare (ignorable adjustment))
            (gtk-widget-queue-draw canvas)))
    (when name
      (gtk-grid-attach grid lab 0 row 1 1))
    (when col
      (gtk-grid-attach grid sb col row 1 1))
    sb))





(defun run ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let ((window (make-instance 'gtk-window :title "holography"
				   :default-width (/ 1920 2)
				   :default-height (/ 1080 2)
				   :border-width 12
				   :type :toplevel))
	    (paned (make-instance 'gtk-paned :orientation :horizontal :position 660))
	    (paned-right (make-instance 'gtk-paned :orientation :vertical :position 120)))
	(g-signal-connect window "destroy" (lambda (widget)
					     (declare (ignorable widget))
					     (leave-gtk-main)))
	(let* ((scrolled (make-instance 'gtk-scrolled-window
					:border-width 1
					:hscrollbar-policy :automatic
					:vscrollbar-policy :automatic))
	       (canvas (make-instance 'gtk-drawing-area)))
	  (setf *canvas* canvas
		tree-gui::*canvas* canvas)
	  (g-signal-connect canvas "draw"
			    (lambda (widget cr)
			      (funcall *draw-canvas* widget cr)))
	  
	  (gtk-scrolled-window-add-with-viewport scrolled canvas)
	  (setf (gtk-widget-size-request canvas) (list 2048 2048))
	  (gtk-container-add window paned)
	  (gtk-paned-add1 paned scrolled)
	  (gtk-paned-add2 paned paned-right)
	  (let ((scrolled (make-instance 'gtk-scrolled-window
					 :border-width 1
					 :hscrollbar-policy :automatic
					 :vscrollbar-policy :automatic))
		(grid (make-instance 'gtk-grid
				      ;:column-homogeneous t
				      ;:row-homogeneous t
				     )))
	    (gtk-scrolled-window-add-with-viewport scrolled grid)
	    (loop for j below 93 by 30 do
		 (loop for i below 118 by 30 do
		      (gtk-grid-attach
		       grid
		       (let* ((label (make-instance
				      'gtk-label :use-markup t :label
				      (format nil "<span font='5'>~2,'0d|~2,'0d</span>" i j)))
			      (button (make-instance 'gtk-button)))
			 (gtk-container-add button label)
			 button)
		       i j 1 1)))
	    (gtk-paned-add1 paned-right scrolled))
	  (let* ((frame1 (make-instance 'gtk-expander :expanded t :label "show image")))
	    (setf *frame1* frame1)
	    (gtk-paned-add2 paned-right
			    (multiple-value-bind (view renderer) (tree-gui::make-view)
			      (let ((expander (make-instance 'gtk-expander :expanded t :label "settings"))
				    (scrolled (make-instance 'gtk-scrolled-window
							     :border-width 1
							     :hscrollbar-policy :automatic
							     :vscrollbar-policy :automatic))
				    (paned-right2 (make-instance 'gtk-paned :orientation :vertical)))
				(setf *renderer* renderer
				     *view* view)
				(let ((model (tree-gui::make-model)))
				  (setf tree-gui::*model* model)
				 (tree-gui::view-update-model *view* *renderer* model))
			       (gtk-container-add scrolled view)
			       (gtk-container-add expander scrolled)
			       (gtk-paned-add1 paned-right2 expander)
			       (gtk-paned-add2 paned-right2 frame1)
			       paned-right2)))))
	(gtk-widget-show-all window)))))

#+nil
(run)

#+nil
(tree-gui::get-tree-value 'fiber1 'cam1 'ky)

#+nil
(tree-gui::view-update-model *view* *renderer* tree-gui::*model*)
#+nil
tree-gui::*hash*

#+nil
(let* ((model (tree-gui::make-model)))
  (defparameter tree-gui::*model* model)
  (tree-gui::view-update-model *view* *renderer* model))
#+nil
(tree-gui::view-update-model *view* *renderer* tree-gui::*model*)

(defun button-checked-p (name)
  (let ((vbox (first (gtk-container-get-children *frame1*))))
    (when (and vbox (eq 'gtk-box (type-of vbox)))
      (let ((hboxes (gtk-container-get-children vbox)))
	(when hboxes
	  (let ((button-widget (first (gtk-container-get-children
				       (find-if
					#'(lambda (x)
					    (let ((button (first (gtk-container-get-children x))))
					      (if (eq (type-of button) 'gtk-check-button)
						  (string= name (gtk-button-label button))
						  (error "type is wrong"))))
					hboxes)))))
	    (when (and button-widget (eq 'gtk-check-button (type-of button-widget))) 
	      (gtk-toggle-button-active button-widget))))))))

#+nil
(button-checked-p "i")

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


#+nil
(when *frame1*
  (let ((frame-contents (gtk-container-get-children *frame1*)))
    (when (< 1 (length frame-contents))
     (let ((widget (first frame-contents)))
       (if (and widget (or
			(eq 'gtk-grid (type-of widget))
			(eq 'gtk-box (type-of widget))))
	   (gtk-widget-destroy widget)
	   (break "not a grid or box")))))
  (let* ((pics (get-pics))
	 (vbox (make-instance 'gtk-box :orientation :vertical)))
    (loop for p in pics and j from 0 do
	 (let* ((hbox (make-instance 'gtk-box :orientation :horizontal))
		(button (gtk-check-button-new-with-label (pic-name p))))
	   (g-signal-connect button "toggled"
			     (lambda (adjustment)
			       (gtk-widget-queue-draw *canvas*)))
	   (gtk-box-pack-start hbox button :expand nil)
	   (gtk-box-pack-start vbox hbox)))
    (gtk-container-add *frame1* vbox)
    (gtk-widget-show-all *frame1*)))


