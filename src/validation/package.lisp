(defpackage :descriptions.validation
  (:nicknames :{}.validation)
  (:use :cl :{})
  (:export #:validate-object
	   #:=>validatable
	   #:=>validatable-string
	   #:=>validatable-number
	   #:=>validatable-password
	   #:=>validatable-email))
