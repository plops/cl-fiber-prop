
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

	(let ((model (make-instance 'gtk-tree-store :column-types '("gchararray" "gint")))
	      (view (make-instance 'gtk-tree-view))
	      (col1 (make-instance 'gtk-tree-view-column :title "name"))
	      (col2 (make-instance 'gtk-tree-view-column :title "value"))
	      (renderer (make-instance 'gtk-cell-renderer-text :text "bla")))
	  (gtk-tree-store-append model nil)
	  (gtk-tree-view-append-column view col1)
	  (gtk-tree-view-append-column view col2)
	  (gtk-tree-view-column-pack-start col1 renderer)
	  (gtk-container-add window view))
	(gtk-widget-show-all window)))))

#+nil
(run)
