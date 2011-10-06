(in-package :cl-user)

(defpackage :parendoc.util
  (:use :cl)
  (:export #:slot-value-if-bound))
(in-package :parendoc.util)

(defun slot-value-if-bound (object slot-name &optional unbound-value)
  (if (slot-boundp object slot-name)
      (slot-value object slot-name)
      unbound-value))
