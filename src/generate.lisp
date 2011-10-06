(in-package :cl-user)

(defpackage :parendoc.generate
  (:use :cl 
        :parendoc.util
        :parendoc.model
        :parendoc.doc)
  (:import-from :anaphora
                #:it
                #:awhen)
  (:export #:generate))
(in-package :parendoc.generate)

(defun find-package-location (package)
  (awhen (let ((swank-backend::*source-snippet-size* #x0fffff))
           (swank:find-definition-for-thing package))
    (when (eq (car it) :location)
      (cdr it))))

(defun find-package-file (package)
  (cadr (assoc :file (find-package-location package))))

(defun find-package-snippet (package)
  (cadr (assoc :snippet (find-package-location package))))

(defun find-package-definition-form (package)
  (awhen (find-package-snippet package)
    (ignore-errors (read-from-string it))))

(defun list-exported-symbols (package)
  (awhen (find-package-definition-form package)
    (mapcar (lambda (name) (intern (string name) package))
            (cdr (assoc :export (cddr it))))))

(defun list-external-symbols (package)
  (loop for symbol being the external-symbol of package collect symbol))

(defun list-all-cl-source-files (component)
  (etypecase component
    (asdf:cl-source-file (list (namestring (asdf:component-pathname component))))
    (asdf:static-file nil)
    (asdf:component (mapcan #'list-all-cl-source-files
                            (slot-value component 'asdf::components)))))

(defun list-all-packages-defined-in (component)
  (loop with package-table = (loop for package in (list-all-packages)
                                   for file = (find-package-file package)
                                   if file collect `(,file . ,package))
        for cl-source-file in (list-all-cl-source-files component)
        for package = (cdr (assoc cl-source-file package-table :test #'string=))
        if package collect package))

(defgeneric generate (object))

(defmethod generate ((system asdf:system))
  (let ((system-name (asdf:component-name system)))
    (section (:title (if (slot-boundp system 'asdf::description)
                         (format nil "~(~A~) - ~A"
                                 system-name
                                 (asdf::system-description system))
                         (format nil "~(~A~)" system-name)))
      (awhen (slot-value-if-bound system 'asdf::long-description)
        (paragraph it))
      (mapcar #'generate (list-all-packages-defined-in system))
      (awhen (slot-value-if-bound system 'asdf::author)
        (section (:title "Authors")
          (unordered-list it)))
      (awhen (slot-value-if-bound system 'asdf::maintainer)
        (section (:title "Maintainers")
          (unordered-list it)))
      (awhen (slot-value-if-bound system 'asdf::licence)
        (section (:title "License")
          (paragraph it))))))

(defmethod generate ((package package))
  (section (:title (format nil "[Package] ~(~A~)" (package-name package)))
    (awhen (documentation package t)
      (paragraph it))
    (loop with exported-symbols = (list-exported-symbols package)
          with external-symbols = (list-external-symbols package)
          with symbols = (append exported-symbols (set-difference exported-symbols external-symbols))
          for symbol in symbols
          collect (generate symbol))))

(defmethod generate ((class class))
  (let ((name (class-name class)))
    (section (:title (format nil "[Class] ~(~A~)" name))
      (awhen (documentation name t)
        (paragraph it)))))

(defmethod generate ((symbol symbol))
  (list
   (awhen (find-class symbol nil)
     (generate it))
   (when (fboundp symbol)
     (section (:title (format nil "[~A] ~(~A~)"
                              (cond ((special-operator-p symbol) "Special Operator")
                                    ((macro-function symbol) "Macro")
                                    (t "Function"))
                              symbol))
       (code-block
         (format nil "~(~A~)~{ ~(~A~)~}" symbol (swank::arglist symbol)))
       (awhen (documentation symbol 'function)
         (paragraph it))))
   (when (boundp symbol)
     (section (:title (format nil "[~A] ~(~A~)"
                              (if (constantp symbol) "Constant" "Variable")
                              symbol))
       (awhen (documentation symbol 'variable)
         (paragraph it))))))
