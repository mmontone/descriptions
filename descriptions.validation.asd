(asdf:defsystem #:descriptions.validation
  :serial t
  :description "The {} descriptions validation module"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:descriptions #:clavier)
  :components ((:module src
			:components
			((:module validation
				  :components
				  ((:file "package")
				   (:file "validation"))
				  :serial t)))))
