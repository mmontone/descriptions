(in-package :{})

;; Syntax and api
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol->keyword (symbol)
    (intern (symbol-name symbol) :keyword))
  
  (defmacro define-description (name parents attributes &rest options)
    "Define a new description"
    (flet ((parse-description-attribute (attribute-spec)
	     (destructuring-bind (attribute-name
				  attribute-type
				  &rest initargs) attribute-spec
	       `(make-attribute ,attribute-type :name ',attribute-name
				,@initargs))))
      `(progn
	 ;; The static description definition
	 (defproto ,name ,(if parents
			      parents
			      (list '{description}))
	 
	   ,(cons `(description-name ',name)
		  (loop for attribute in attributes
		     collect (list (first attribute) (parse-description-attribute attribute) :accessor nil)))
	   ,@options)
       
	 ,(when (getf options :documentation)
		`(setf (documentation ',name 'variable)
		       ,(getf options :documentation)))

	 ;; A description builder function
	 (defun ,name (&rest parents)
	   ,(format nil "Create a ~A description. Takes a list of descriptions to use as parents of the new description." name)
	   (make-description :parents (or parents
					  ,(if parents
					       `(list ,@parents)
					       '(list {description})))
			     :attributes
			     (list ,@(loop
					for attribute in attributes
					collect `(list ',(first attribute)
						       ,(parse-description-attribute attribute)))))))))

  (defmacro with-description-attributes (attributes description &body body)
    "Run body with description attributes bound"
    `(let ,(loop for attribute in attributes
	      collect
		(list attribute `(get-attribute ,description ',attribute)))
       ,@body))

  (defmacro with-described-object (attributes (object &optional (description (default-description object))) &body body)
    `(let ,(loop for attribute in attributes
		collect (list attribute `(funcall (attribute-reader
						   (get-attribute ,description ',attribute)) ,object)))
       ,@body))

  (defmacro define-attribute (name parents properties &rest options)
    "Define an attribute type

:param name: the attribute type name.
:param parents: the attribute type parents.
:param properties: list of properties.
:param options: options, like :documentation, etc"
  
    (flet ((parse-attribute-property (property-spec)
	     (destructuring-bind (property-name property-value &key reader writer accessor)
		 property-spec
	       `(,(symbol->keyword property-name)
		  ,property-value
		  ,@(when reader
			  `(:reader ',reader))
		  ,@(when writer
			  `(:writer ',writer))
		  ,@(when accessor
			  `(:accessor ',accessor))))))
      `(progn
	 (defproto ,name ,(if parents
			      parents
			      (list '=>))
	   ,(loop for property in properties
	       collect (parse-attribute-property property))
	   ,@(remove-from-plist options :alias))
	 ,(when (getf options :documentation)
		`(setf (documentation ',name 'variable)
		       ,(getf options :documentation)))
	 ,(when (getf options :alias)
		`(setf (get ',(getf options :alias) :attribute-alias)
		       ',name))
	 (defun ,name (&rest property-values)
	   ,(format nil "Create a ~A attribute. Takes a plist of property values for the created attribute" name)
	   (apply #'make-attribute ,name property-values)))))

  (defmacro => (type-spec &rest property-values)
    "Attribute builder macro"
    (if (listp type-spec)
	`(make-attribute (list ,@type-spec) ,@property-values)
	`(make-attribute ,type-spec ,@property-values)))
  )

(defun make-attribute (attribute-type &rest property-values)
    "Create an attribute.

:param attribute-type: The attribute type
:property-values: A plist of attribute properties values
"
    (flet ((plist-properties (plist)
	     (loop for key in plist by #'cddr
		for value in (cdr plist) by #'cddr
		collect (list key value))))
      (object :parents (if (listp attribute-type)
			   attribute-type
			   (list attribute-type))
	      :properties (cons (list :type attribute-type)
				(plist-properties property-values)))))

(defun description-attributes (description)
  "Obtain a description attributes.
:param description: the description."
  (mapcar (lambda (prop)
            (get-attribute description prop))
          (remove-if (lambda (prop)
                       (member prop (list 'description-name 'sheeple::nickname 'documentation)))
                     (available-properties description))))

;; Attributes

(defproto => ()
  ((:name nil :accessor 'attribute-name)
   (:type 'attribute :accessor 'attribute-type))
  :documentation "Top level attribute")

(defun attribute-properties (attribute)
  (remove-if (lambda (property-name)
	       (member property-name (list 'sheeple::nickname 'documentation)))
	     (available-properties attribute)))

(defun attribute-documentation (attribute)
  (property-value attribute 'documentation))

(defstruct undefined)
(defvar %undefined% (make-undefined))
(define-symbol-macro +undefined+ %undefined%)
(defun undefinedp (value)
  (undefined-p value))

(define-attribute =>valued (=>)
  ((value +undefined+ :accessor attribute-value)
   (reader +undefined+ :accessor attribute-reader)
   (writer +undefined+ :accessor attribute-writer)))

(defreply attribute-undefinedp ((attribute =>valued))
  (undefinedp (attribute-value attribute)))

(define-attribute =>symbol (=>valued)
  ()
  :documentation "Symbol attribute"
  :alias symbol)

(define-attribute =>keyword (=>valued)
  ()
  :documentation "Keyword attribute"
  :alias keyword)

(define-attribute =>string (=>valued)
  ()
  :documentation "String attribute"
  :alias string)

(define-attribute =>email (=>valued)
  ()
  :documentation "Email attribute"
  :alias email)

(define-attribute =>password (=>valued)
  ((serialize nil :accessor attribute-serialize))
  :documentation "Password attribute"
  :alias password)

(define-attribute =>integer (=>valued)
  ()
  :documentation "Integer attribute"
  :alias integer)

(define-attribute =>boolean (=>valued)
  ()
  :documentation "Boolean attribute"
  :alias boolean)

(define-attribute =>reference (=>valued)
  ((reference nil :accessor attribute-reference)))

(defreply print-sheeple-object ((attribute =>reference) stream)
  (format stream " ref: ~A" (attribute-reference attribute))
  (call-next-reply))

(define-attribute =>option (=>reference)
  ((options nil :accessor attribute-options)
   (sorted nil :accessor attribute-sorted)
   (sorter nil :accessor attribute-sorter)))

(defreply print-sheeple-object ((attribute =>option) stream)
  (format stream " (~{~a~^, ~})" (attribute-options attribute))
  (call-next-reply))

(define-attribute =>single-option (=>option)
  ())

(define-attribute =>multiple-option (=>option)
  ())

(define-attribute =>relation (=>reference)
  ())

(define-attribute =>to-many-relation (=>relation)
  ((sorted nil :accessor attribute-sorted)
   (sorter nil :accessor attribute-sorter)))

(define-attribute =>to-one-relation (=>relation)
  ())

;; Descriptions

(defproto {description} ()
  ((description-name nil :accessor 'description-name)))

(defun add-attribute (description attribute &key reader writer accessor)
  (funcall #'(setf property-value)
	   attribute description (attribute-name attribute)
           :reader reader :writer writer :accessor accessor))

(defun get-attribute (description attribute-name)
  (let ((attributes
         (flatten
          (mapcar
           (lambda (description)
             (mapcar (lambda (prop-name)
                       (property-value description prop-name))
                     (remove-if-not (lambda (prop)
                                      (equalp prop attribute-name))
                                    (direct-properties description))))
           (object-precedence-list description)))))
    (object :parents attributes)))

(defun make-description (&key parents attributes)
  (object
   :parents parents
   :properties attributes))

;; Displaying

(define-attribute =>view (=>)
  ((label nil :accessor attribute-label)
   (view t :accessor attribute-view)
   (formatter #'prin1-to-string :accessor attribute-formatter))) 

(defun display-object (object &optional (description (default-description object))
				(stream t))
  (loop for attribute in (description-attributes description)
       when (and (descendantp attribute =>view)
		 (attribute-view attribute))
       do (let ((attribute-label (or (attribute-label attribute)
				     (string-capitalize
				      (string-downcase
				       (symbol-name (attribute-name attribute))))))
		(attribute-value (funcall (attribute-formatter attribute)
					  (funcall
					   (attribute-reader attribute)
					   object))))
	    (format stream "~A: ~A~%" attribute-label attribute-value))))

(defreply print-sheeple-object ((description {description}) stream)
  (print-unreadable-object (description stream :identity t)
    (if (description-name description)
	(format stream "~A" (description-name description))
	(format stream "[~{~A~}]" (object-parents description)))))

(defreply print-sheeple-object :around ((attribute =>) stream)
  (print-unreadable-object (attribute stream :identity t)
    (if (attribute-name attribute)
	(format stream " ~A : ~A"
		(attribute-name attribute)
		(object-nickname attribute))
	(format stream " ~A" (object-nickname attribute)))
    (call-next-reply)))

(defreply print-sheeple-object ((attribute =>) stream)
  )
