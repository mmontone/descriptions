(asdf:defsystem #:descriptions.serialization
  :serial t
  :description "The {} descriptions serialization module"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:descriptions #:cl-json)
  :components ((:module src
			:components
			((:module serialization
				  :components
				  ((:file "package")
				   (:file "serialization"))
				  :serial t)))))
