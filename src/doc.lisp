(in-package :cl-user)

(defpackage :parendoc.doc
  (:use :cl
        :parendoc.model)
  (:export #:page-feed

           #:section
           #:section-title

           #:text-block
           #:paragraph
           #:blockquote
           #:unordered-list
           #:ordered-list
           #:code-block
           #:horizontal-rule

           #:span
           #:link
           #:link-title
           #:link-address
           #:emphasis
           #:code
           #:image
           #:image-title
           #:image-address))
(in-package :parendoc.doc)

(defelement page-feed (element))

(defelement section (element) (title))

(defelement text-block (element))
(defelement paragraph (text-block))
(defelement blockquote (text-block))
(defelement unordered-list (text-block))
(defelement ordered-list (text-block))
(defelement code-block (text-block))
(defelement horizontal-rule (text-block))

(defelement span (element))
(defelement link (span) (title address))
(defelement emphasis (span))
(defelement code (span))
(defelement image (span) (title address))
