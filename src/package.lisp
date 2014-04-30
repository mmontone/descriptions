(defpackage descriptions
  (:nicknames :desc :{})
  (:use :cl :sheeple :alexandria :anaphora)
  (:export #:define-description
	   #:make-description
	   #:{description}
	   #:add-attribute
	   #:get-attribute
	   #:description-attributes
	   #:description-name
	   #:with-description-attributes
	   #:with-described-object
	   #:define-attribute
	   #:make-attribute
	   #:=>
	   #:attribute-properties
	   #:attribute-documentation
	   #:=>valued
	   #:=>string
	   #:=>boolean
	   #:=>integer
	   #:=>email
	   #:=>password
	   #:=>view
	   #:=>single-option
	   #:=>multiple-option
	   #:=>to-many-relation
	   #:=>to-one-relation
	   #:display-object

	   ;; Attributes accessors
	   #:attribute-name
	   #:attribute-type
	   #:attribute-value
	   #:attribute-reader
	   #:attribute-writer
	   #:attribute-serialize
	   #:attribute-reference
	   #:attribute-sorted
	   #:attribute-sorter
	   #:attribute-options
	   #:attribute-label
	   #:attribute-view
	   #:attribute-formatter))
