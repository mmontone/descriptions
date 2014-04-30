;;;; descriptions.asd

(asdf:defsystem #:descriptions
  :serial t
  :description "A domain model meta level description library"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:alexandria
               #:anaphora
               #:sheeple
	       #:closer-mop)
  :components ((:module src
			:components
			((:file "package")
			 (:file "descriptions")
			 (:file "mop"))
			:serial t)))
