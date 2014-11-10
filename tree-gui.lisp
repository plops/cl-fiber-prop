(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-cffi-gtk))
sb-alien::*shared-objects* 
(defpackage :tree-gui 
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :cl))

(in-package :tree-gui)


(defun make-model ()
  (let* ((model (make-instance 'gtk-tree-store :column-types '("gchararray" "gdouble"))))
    (loop for f in '(fiber1 fiber2 fiber3) do
	  (let ((parent-f (gtk-tree-store-set model
					      (gtk-tree-store-append model nil)
					      (symbol-name f) 0d0)))
	    (loop for cam in '(cam1 cam2 cam3) do
		  (let ((parent-cam (gtk-tree-store-set model
							(gtk-tree-store-append model parent-f)
							(symbol-name cam) 0d0)))
		    (loop for var in '(xpos ypos radius kx ky kradius) do
			  (gtk-tree-store-set model (gtk-tree-store-append model parent-cam)
					      (symbol-name var) 0d0))))))
    model))

(defun make-view ()
  (let ((view (make-instance 'gtk-tree-view :hover-selection t)))
    (let* ((renderer (gtk-cell-renderer-text-new))
	   (col (gtk-tree-view-column-new-with-attributes "name" renderer "text" 0)))
      (gtk-tree-view-append-column view col))
    (let* ((adj (make-instance 'gtk-adjustment
			       :value (* 1d0 100)
			       :lower 0d0
			       :upper (* 1d0 2000)
			       :step-increment 1d0
			       :page-increment 10d0
			       :page-size 0d0))
	   (renderer (make-instance 'gtk-cell-renderer-spin :editable t :digits 0 :adjustment adj))
	   (col (gtk-tree-view-column-new-with-attributes "value" renderer "text" 1)))
      (gtk-tree-view-column-set-cell-data-func col renderer
					       (lambda (tree-view-column cell-renderer-spin tree-store tree-iter)
						 (declare (ignorable tree-view-column))
						 (let* ((column-number 1)
						        (value (first (gtk-tree-model-get tree-store tree-iter column-number))))
						   (g-object-set-property cell-renderer-spin "text" (format nil "~d" (floor value))))))
      (gtk-tree-view-append-column view col)
      (values view renderer))))


(defparameter *hash* nil)
(defparameter *model* nil)
(defparameter *canvas* nil)
(defparameter *view* nil)
(defparameter *selection* nil)
(defparameter *adjustment* nil)

(defmacro print-signal (obj event)
  `(g-signal-connect ,obj ,event #'(lambda (&rest rest) (format t "~a: ~a ~a~%" ',obj ',event rest
								)
					   (force-output)
					   nil)))

(defun view-update-model (view renderer model)
  (gtk-tree-view-set-model view model)
  (g-signal-connect renderer "edited" (lambda (renderer path-string newtext) ;; 4th parameter should be GtkTreeView *treeview
					(declare (ignore newtext))
					;; according to documentation the string in newtext should not be used,
					;; instead a double value obtained from the adjustemtn
					(let* ((adj (g-object-get-property renderer "adjustment"))
					       (value (gtk-adjustment-get-value adj))
					       (iter (gtk-tree-model-get-iter-from-string model path-string))
					       ;(path (gtk-tree-path-new-from-string path-string))
					       ;(iter (gtk-tree-model-get-iter model path))
					       )
					  (gtk-tree-store-set-value model iter 1 value)
					  (setf *adjustment* nil)
					  (when *canvas*
					    (gtk-widget-queue-draw *canvas*)))))
  

  #+nil (let ((selection (gtk-tree-view-get-selection *view*)))
    (gtk-tree-selection-set-mode selection :single)
    (setf *selection* selection)
    
    ;;     (gtk-tree-view-get-path-at-pos model x y)
    (g-signal-connect selection "changed"
		      #'(lambda (selection)
			  (let ((iter (gtk-tree-selection-get-selected selection)))
			    (if iter
				(format t "selection changed ~a~%" (list selection iter))
				(format t "selection changed none~%"))))))
  (g-signal-connect
   renderer "editing-started"
   #'(lambda (renderer editable path-string)
       ;; editable is a spin-box
       (g-signal-connect (gtk-spin-button-get-adjustment editable) "value-changed"
			 (lambda (adjustment)
			   (let* ((path (gtk-tree-path-new-from-string path-string))
				  (iter (gtk-tree-model-get-iter model path))
				  (value (gtk-adjustment-get-value adjustment)))
			     #+nil (setf *adjustment* iter)
			     #+nil (gtk-tree-store-set-value model iter 1 value)
			     
			     #+nil (g-signal-connect model "row-changed"
					       #'(lambda (tree-store tree-path tree-iter)
						   (format t "model row-changed: ~a~%"
							   (list tree-store tree-path tree-iter))
						   #+nil (g-signal-stop-emission-by-name (pointer tree-store) "row-changed")))
			     #+nil (g-signal-emit model "row-changed" path iter)
			     (g-signal-emit view "realize" view)
			     (g-signal-connect-after view "realize" #'(lambda (&rest rest)
									(FORMAT t "view realize ~a~%" rest)))
			     (when *canvas*
			       (gtk-widget-queue-draw *canvas*))
			     (format t "spin-box value-changed: ~a~%" (list value
									    (first (gtk-tree-model-get model iter 1)))))))
       (format t "editing-started: ~a~%" (list renderer editable path-string))))
  (let ((a (make-hash-table)))
    (preorder (gtk-tree-model-get-iter-first model)
	      #'(lambda (iter acc)
		  (let ((key (sxhash (append (loop for iter in acc collect
						  (first (gtk-tree-model-get model iter 0)))
					     (gtk-tree-model-get model iter 0))))
			(val iter))
		    (format t "entering into hash: ~a~%" (list key val))
		    (setf (gethash key a) val)))
	      nil)
    (setf *hash* a)))

#+nil
(g-type-from-instance (pointer *model*))
#+nil
(defparameter *bla* (g-signal-parse-name (g-type-from-instance (pointer *model*)) "row-changed"))


(defun run ()
     (sb-int:with-float-traps-masked
	 (:divide-by-zero)
       (within-main-loop
	 (let ((window (make-instance 'gtk-window :title "holography"
				      :default-width 480
				      :default-height 240
				      :border-width 12
				      :type :toplevel)))
	   (g-signal-connect window "destroy" (lambda (widget)
						(declare (ignorable widget))
						(leave-gtk-main)))
	   (defparameter *window* window)
      
	   (multiple-value-bind (view renderer) (make-view)
	     (defparameter *view* view)
	     (defparameter *renderer* renderer)
	     (gtk-container-add window view))
	   (gtk-widget-show-all window)))))


#+nil
(let* ((model (make-model)))
  (defparameter *model* model)
  (view-update-model *view* *renderer* model))

(defun preorder (node f acc)
  (when node
    (funcall f node acc)
    (preorder (gtk-tree-model-iter-children *model* node) f (append acc (list node)))
    (preorder (gtk-tree-model-iter-next *model* node)  f acc)))

(defun get-tree-hash (fiber camera slot)
  (declare (type symbol fiber camera slot))
  (gethash (sxhash (mapcar #'symbol-name (list fiber camera slot))) *hash*))

#+nil
(get-tree-hash 'fiber1 'cam1 'ky)

(defun get-tree-value (fiber camera slot)
  (declare (type symbol fiber camera slot))
  (let ((iter (get-tree-hash fiber camera slot)))
    (first (gtk-tree-model-get *model* iter 1))))

#+nil
(get-tree-value 'fiber1 'cam1 'ky)

(defun set-tree-value (fiber camera slot value)
  (declare (type symbol fiber camera slot)
	   (type double-float value))
  (let ((iter (get-tree-hash fiber camera slot)))
    (gtk-tree-store-set-value *model* iter 1 value)))

#+nil
(dotimes (i 10)
  (sleep .4)
 (set-tree-value 'fiber1 'cam1 'ky (* i 36.0d0)))

#+nil
(run)
