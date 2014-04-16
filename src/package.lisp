(defpackage descriptions
  (:nicknames :desc :{})
  (:use :cl :sheeple :alexandria :anaphora)
  (:export #:define-description
	   #:make-description
	   #:{description}
	   #:add-attribute
	   #:get-attribute
	   #:description-attributes
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
	   #:display-object))
