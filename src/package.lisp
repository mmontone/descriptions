(defpackage descriptions
  (:nicknames :desc :{})
  (:use :cl :sheeple :alexandria :anaphora)
  (:export #:define-description
	   #:make-description
	   #:{description}
	   #:add-attribute
	   #:get-attribute
	   #:description-attributes
	   #:with-description-attributes
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
	   #:display-object))
