(in-package :{})

;; Syntax and api

(defun make-attribute (attribute-type &rest property-values)
  (flet ((plist-to-properties (property-values)
	   (loop
	      for prop in property-values by #'cddr
	      for value in (cdr property-values) by #'cddr
	      collect (list (if (keywordp prop)
				(intern (symbol-name prop))
				prop)
			    value))))
    (object :parents (if (listp attribute-type)
			 attribute-type
			 (list attribute-type))
	    :properties (cons (list 'type attribute-type)
			      (plist-to-properties property-values)))))

(defmacro define-description (name parents attributes &rest options)
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

       ;; A description builder function
       (defun ,name (&rest parents)
	 (make-description :parents (or parents
					,(if parents
					     `(list ,@parents)
					     '(list {description})))
			   :attributes
			   (list ,@(loop
				      for attribute in attributes
				      collect `(list ',(first attribute)
						     ,(parse-description-attribute attribute)))))))))
(defun description-attributes (description)
  (mapcar (lambda (prop)
            (get-attribute description prop))
          (remove-if (lambda (prop)
                       (member prop (list 'description-name 'sheeple::nickname 'documentation)))
                     (available-properties description))))

(defmacro with-description-attributes (attributes description &body body)
  `(let ,(loop for attribute in attributes
	      collect
	      (list attribute `(get-attribute ,description ',attribute)))
     ,@body))

(defmacro define-attribute (name parents properties &rest options)
  (flet ((parse-attribute-property (property-spec)
	   (destructuring-bind (property-name property-value &key reader writer accessor)
	       property-spec
	     `(,property-name ,property-value
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
       ,@options)
       (defun ,name (&rest property-values)
	 (apply #'make-attribute ,name property-values)))))

;; Attributes

(DEFPROTO => ()
  ((NAME NIL :ACCESSOR 'ATTRIBUTE-NAME)
   (TYPE 'ATTRIBUTE :ACCESSOR 'ATTRIBUTE-TYPE))
  :DOCUMENTATION "An attribute")

(defun attribute-properties (attribute)
  (remove-if (lambda (property-name)
	       (member property-name (list 'sheeple::nickname 'documentation)))
	     (available-properties attribute)))

(defun attribute-documentation (attribute)
  (property-value attribute 'documentation))

(define-attribute =>valued (=>)
  ((value nil :accessor attribute-value)
   (reader nil :accessor attribute-reader)
   (writer nil :accessor attribute-writer)))

(define-attribute =>string (=>valued)
  ()
  :documentation "String attribute")

(define-attribute =>email (=>valued)
  ()
  :documentation "Email attribute")

(define-attribute =>password (=>valued)
  ((serialize nil :accessor attribute-serialize))
  :documentation "Password attribute")

(define-attribute =>integer (=>valued)
  ()
  :documentation "Integer attribute")

(define-attribute =>boolean (=>valued)
  ()
  :documentation "Boolean attribute")

(define-attribute =>reference (=>valued)
  ((reference nil :accessor attribute-reference)))

(define-attribute =>option (=>reference)
  ((options nil :accessor attribute-options)
   (sorted nil :accessor attribute-sorted)
   (sorter nil :accessor attribute-sorter)))

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
   :properties (cons (list 'description-name '{anonymous})
		     attributes)))

;; Displaying

(define-attribute =>view (=>)
  ((label nil :accessor attribute-label)
   (view t :accessor attribute-view)
   (formatter #'prin1-to-string :accessor attribute-formatter))) 

(defun display-object (object description &optional (stream t))
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
    (if (equalp (description-name description) '{anonymous})
	(format stream "{ANONYMOUS} [~{~A~}]" (object-parents description))
	(format stream "~A" (description-name description)))))

(defreply print-sheeple-object  ((attribute =>) stream)
  (print-unreadable-object (attribute stream :identity t)
    (format stream "~A : ~A"
	    (attribute-name attribute)
	    (object-nickname attribute))))
