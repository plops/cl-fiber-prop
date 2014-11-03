
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-cffi-gtk))

(defpackage :tree-gui 
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :cl))

(in-package :tree-gui)


(defun make-model ()
 (let* ((model (make-instance 'gtk-tree-store :column-types '("gchararray" "gdouble"))))
   (let ((parent (gtk-tree-store-set model
				     (gtk-tree-store-append model nil)
				     "fiber" 1d0)))
     (gtk-tree-store-set model (gtk-tree-store-append model parent) "Klausf-Dieter Mustermann" 1351d0)
     (gtk-tree-store-set model (gtk-tree-store-append model parent) "Ulrikef Langhals" 213d0))
   model))

(defun make-view ()
  (let ((view (make-instance 'gtk-tree-view)))
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


(defun view-update-model (view renderer model)
  (gtk-tree-view-set-model view model)	  
  (g-signal-connect renderer "edited" (lambda (renderer path-string newtext) ;; 4th parameter should be GtkTreeView *treeview
					(declare (ignore newtext))
					;; according to documentation the string in newtext should not be used,
					;; instead a double value obtained from the adjustemtn
					(let* ((adj (g-object-get-property renderer "adjustment"))
					       (value (gtk-adjustment-get-value adj))
					       (path (gtk-tree-path-new-from-string path-string))
					       ;; (model (gtk-tree-view-get-model ))
					       (iter (gtk-tree-model-get-iter model path))
					       )
					  (gtk-tree-store-set-value model iter 1 value)))))

(defun run ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let ((window (make-instance 'gtk-window :title "holography"
				   :default-width 320
				   :default-height 240
				   :border-width 12
				   :type :toplevel)))
	(g-signal-connect window "destroy" (lambda (widget)
					     (declare (ignorable widget))
					     (leave-gtk-main)))
	(defparameter *window* window)
	
	(let* ((model (make-model)))
	  (multiple-value-bind (view renderer) (make-view)
	    (defparameter *view* view)
	    (defparameter *renderer* renderer)
	    (view-update-model view renderer model)
	    (gtk-container-add window view)))
	(gtk-widget-show-all window))))))


#+nil
(let* ((model (make-model)))
  (view-update-model *view* *renderer* model))

#+nil
(run)

