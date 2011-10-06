(in-package :cl-user)

(defpackage :parendoc.model
  (:use :cl)
  (:import-from :alexandria
                #:make-keyword)
  (:import-from :trivial-types
                #:property-list)
  (:export #:node
           #:element
           #:element-attributes
           #:element-children
           #:element-attribute
           #:defelement))
(in-package :parendoc.model)

(deftype node () 'atom)

(defclass element ()
  ((attributes :type (property-list node)
               :initarg :attributes
               :initform '()
               :accessor element-attributes)
   (children :type proper-list
             :initarg :children
             :initform '()
             :accessor element-children)))

(defmacro element-attribute (element name)
  `(getf (element-attributes ,element) ,name))

(defmacro defelement (name &optional (superclasses '(element)) slots)
  `(progn
     (defclass ,name ,superclasses
       ,(loop for slot in slots
              for initarg = (make-keyword slot)
              for accessor = (intern (format nil "~A-~A" name slot))
              collect `(,slot :initarg ,initarg :initform nil :accessor ,accessor)))
     (defmacro ,name (&body children)
       (let ((attributes (first children)))
         (if (and (consp attributes)
                  (keywordp (car attributes)))
             (pop children)
             (setq attributes nil))
         `(make-instance ',',name
                         :attributes (list ,@attributes)
                         :children (list ,@children)
                         ,,@(loop for slot in slots
                                  for initarg = (make-keyword slot)
                                  collect initarg
                                  collect `(getf attributes ,initarg)))))))
