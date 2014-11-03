
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-cffi-gtk))
#+nil
(sb-ext:save-lisp-and-die "/home/martin/cl-fiber-prop/sbcl-gtk-gsl.core")

(defpackage :tree-gui 
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :cl))

(in-package :tree-gui)

(defun run ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let ((window (make-instance 'gtk-window :title "holography"
				   :default-width (/ 1920 2)
				   :default-height (/ 1080 2)
				   :border-width 12
				   :type :toplevel)))
	(g-signal-connect window "destroy" (lambda (widget)
					     (declare (ignorable widget))
					     (leave-gtk-main)))

	(let* ((model (make-instance 'gtk-tree-store :column-types '("gchararray" "guint")))
	       )
	  ;(gtk-tree-store-append model nil)
	  (let ((parent (gtk-tree-store-set model
					    (gtk-tree-store-append model nil)
					    "fiber" 1)))
	    (gtk-tree-store-set model (gtk-tree-store-append model parent)
				"Klaus-Dieter Mustermann" 51)
	    (gtk-tree-store-set model (gtk-tree-store-append model parent)
				"Ulrike Langhals" 23))
	  (let ((view (make-instance 'gtk-tree-view :model model)))
	   (let* ((renderer (gtk-cell-renderer-text-new))
		  (col (gtk-tree-view-column-new-with-attributes "name" renderer "text" 0)))
	     (gtk-tree-view-append-column view col))
	   
	   (let* ((renderer (gtk-cell-renderer-text-new))
		  (col (gtk-tree-view-column-new-with-attributes "value" renderer "text" 1)))
	     (gtk-tree-view-append-column view col))
	   
	   (gtk-container-add window view)))
	(gtk-widget-show-all window)))))

#+nil
(run)
