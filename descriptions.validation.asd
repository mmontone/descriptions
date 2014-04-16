(asdf:defsystem #:descriptions.validation
  :serial t
  :description "The {} descriptions validation module"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:descriptions #:cl-ppcre)
  :components ((:module src
			:components
			((:module validation
				  :components
				  ((:file "package")
				   (:file "validation"))
				  :serial t)))))
