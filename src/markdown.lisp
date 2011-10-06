(in-package :cl-user)

(defpackage :parendoc.markdown
  (:use :cl
        :parendoc.model
        :parendoc.doc
        :parendoc.render)
  (:export #:markdown-format
           #:standard-markdown-format
           #:render-markdown
           #:render-markdown-to-string
           #:render-markdown-to-file))
(in-package :parendoc.markdown)

(defun make-underlined-string (string &key (underline-char #\-))
  (format nil "~A~%~A"
          string
          (make-string (length string)
                       :initial-element underline-char)))

(defmacro with-line-prefix (prefix &body body)
  `(pprint-logical-block (*render-stream* nil :per-line-prefix ,prefix) ,@body))

(defmacro with-surround ((open &optional close) &body body)
  `(progn
     (princ ,open *render-stream*)
     ,@body
     (princ ,(or close open) *render-stream*)))

(defclass markdown-format (render-format) ())
(defclass standard-markdown-format (markdown-format) ())

(defparameter *section-depth* 0)

(defun render-of-section-title (title &optional (depth *section-depth*))
  (case depth
    (0 (format *render-stream* "~&~A~%" (make-underlined-string title :underline-char #\=)))
    (1 (format *render-stream* "~&~%~A~%" (make-underlined-string title :underline-char #\-)))
    (t (format *render-stream* "~&~%~A ~A~%" (make-string depth :initial-element #\#) title))))

(defmethod render ((format standard-markdown-format) (section section))
  (render-of-section-title (render-to-string format (section-title section)))
  (let ((*section-depth* (1+ *section-depth*)))
    (call-next-method)))

(defmethod render :around ((format standard-markdown-format) (text-block text-block))
  (format *render-stream* "~&~%")
  (call-next-method)
  (format *render-stream* "~&"))

(defmethod render ((format standard-markdown-format) (blockquote blockquote))
  (with-line-prefix "> " (call-next-method)))

(defmethod render ((format standard-markdown-format) (unordered-list unordered-list))
  (dolist (item (element-children unordered-list))
    (format *render-stream* "~&* ")
    (render format item)))

(defmethod render ((format standard-markdown-format) (ordered-list ordered-list))
  (loop for i from 1
        for item in (element-children ordered-list)
        do (format *render-stream* "~&~D. " i)
           (render format item)))

(defmethod render ((format standard-markdown-format) (code-block code-block))
  (with-line-prefix "    " (call-next-method)))

(defmethod render ((format standard-markdown-format) (horizontal-rule horizontal-rule))
  (write-string "----" *render-stream*))

(defmethod render ((format standard-markdown-format) (link link))
  (with-surround ("[" "]")
    (call-next-method))
  (with-surround ("(" ")")
    (render format (link-address link))
    (when (link-title link)
      (with-surround (" \"" "\"")
        (render format (link-title link))))))

(defmethod render ((format standard-markdown-format) (emphasis emphasis))
  (with-surround ("*") (call-next-method)))

(defmethod render ((format standard-markdown-format) (code code))
  (with-surround ("`") (call-next-method)))

(defmethod render ((format standard-markdown-format) (image image))
  (with-surround ("![" "]")
    (call-next-method))
  (with-surround ("(" ")")
    (render format (image-address image))
    (when (link-title image)
      (with-surround (" \"" "\"")
        (render format (image-title image))))))

(defun render-markdown (node)
  (render (make-instance 'standard-markdown-format) node))

(defun render-markdown-to-string (node)
  (render-to-string (make-instance 'standard-markdown-format) node))

(defun render-markdown-to-file (node file &key (if-exists :error) (if-does-not-exist :create))
  (render-to-file (make-instance 'standard-markdown-format) node file
                  :if-exists if-exists
                  :if-does-not-exist if-does-not-exist))
