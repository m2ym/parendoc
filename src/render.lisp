(in-package :cl-user)

(defpackage :parendoc.render
  (:use :cl
        :parendoc.model
        :parendoc.doc)
  (:export #:*render-stream*
           #:render-format
           #:render
           #:render-to-string
           #:render-to-file))
(in-package :parendoc.render)

(defvar *render-stream* t)

(defclass render-format () ())

(defmethod render (format node)
  (princ node *render-stream*))

(defmethod render (format (list list))
  (dolist (node list) (render format node)))

(defmethod render (format (element element))
  (render format (element-children element)))

(defmethod render (format (page-feed page-feed))
  (terpri *render-stream*))

(defun render-to-string (format node)
  (with-output-to-string (*render-stream*)
    (render format node)))

(defun render-to-file (format node file &key (if-exists :error) (if-does-not-exist :create))
  (with-open-file (*render-stream* file
                                   :direction :output
                                   :if-exists if-exists
                                   :if-does-not-exist if-does-not-exist)
    (render format node)))
