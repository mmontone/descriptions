(defpackage :descriptions.serialization
  (:nicknames :{}.serialization)
  (:use :cl :{})
  (:export #:serialize-object
	   #:=>serializable
	   #:attribute-serialize
	   #:attribute-serialization-name
	   #:attribute-serializer))
