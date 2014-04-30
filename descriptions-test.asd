(asdf:defsystem #:descriptions-test
  :serial t
  :description "{} descriptions unit tests"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:descriptions
	       #:descriptions.serialization
	       #:descriptions.validation
	       #:stefil)
  :components ((:module test
			:components
			((:file "test"))
			:serial t)))
