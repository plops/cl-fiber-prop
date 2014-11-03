
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-cffi-gtk))
#+nil
(sb-ext:save-lisp-and-die "/home/martin/cl-fiber-prop/sbcl-gtk-gsl.core")

(defpackage :tree-gui 
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :cl))

(in-package :tree-gui)


(defun make-model ()
 (let* ((model (make-instance 'gtk-tree-store :column-types '("gchararray" "gdouble"))))
   (let ((parent (gtk-tree-store-set model
				     (gtk-tree-store-append model nil)
				     "fiber" 1d0)))
     (gtk-tree-store-set model (gtk-tree-store-append model parent) "Klaus-Dieter Mustermann" 51d0)
     (gtk-tree-store-set model (gtk-tree-store-append model parent) "Ulrike Langhals" 23d0))
   model))

(defun make-view (model)
  (let ((view (make-instance 'gtk-tree-view :model model)))
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
						 (let*((column-number 1)
						       (value (first (gtk-tree-model-get tree-store tree-iter column-number))))
						   (g-object-set-property cell-renderer-spin "text" (format nil "~d" (floor value))))))
      (g-signal-connect renderer "edited" (lambda (renderer path-string newtext)
					    (declare (ignore newtext))
					    ;; according to documentation the string in newtext should not be used,
					    ;; instead a double value obtained from the adjustemtn
					    (let* ((adj (g-object-get-property renderer "adjustment"))
						   (value (gtk-adjustment-get-value adj))
						   (path (gtk-tree-path-new-from-string path-string))
						   ;; (model (gtk-tree-view-get-model tree-view))
						   (iter (gtk-tree-model-get-iter model path))
						   )
					      (gtk-tree-store-set-value model iter 1 value))))
      (gtk-tree-view-append-column view col))
    view))

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


	
	(let* ((model (make-model))
	       (view (make-view model))
	       )
	  ;(gtk-tree-view-set-model view model)	  
	  (gtk-container-add window view)
	  )
	(gtk-widget-show-all window)))))


#+nil
(run)
