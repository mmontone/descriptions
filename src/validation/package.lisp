(defpackage :descriptions.validation
  (:nicknames :{}.validation)
  (:use :cl :{} :clavier :sheeple :alexandria)
  (:export #:validate-object
	   #:=>validatable
	   #:=>validatable-string
	   #:=>validatable-number
	   #:=>validatable-password
	   #:=>validatable-email
	   #:attribute-validator
	   #:attribute-validate
	   #:attribute-required))
