(defpackage descriptions
  (:nicknames :desc :{})
  (:use :cl :sheeple :alexandria :json))

(in-package :descriptions)

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
       ,(loop for attribute in attributes
	   collect (list (first attribute) (parse-description-attribute attribute) :accessor nil))
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
    `(defproto ,name ,(if parents
			  parents
			  (list '=>))
       ,(loop for property in properties
	   collect (parse-attribute-property property))
       ,@options)))

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

(defparameter *attribute* (make-attribute => :name 'my-attribute))

(attribute-name *attribute*)

(attribute-properties *attribute*)

(describe *attribute*)

(property-value *attribute* 'name)

(define-attribute =>valued (=>)
  ((value nil :accessor attribute-value)
   (reader nil :accessor attribute-reader)
   (writer nil :accessor attribute-writer)))

(defparameter *value-attribute* (make-attribute =>valued
						'name 'my-value-attribute
						'value "Mariano"))

(attribute-name *value-attribute*)
(attribute-value *value-attribute*)
(attribute-reader *value-attribute*)

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

(defclass person ()
  ((id :accessor id)
   (username :initarg :username
	     :accessor username)
   (email :initarg :email
          :accessor email)
   (fullname :initarg :fullname
             :accessor fullname)
   (password :initarg :password
	     :accessor password)))

(define-description {person} ()
  ((username =>string
	     :reader #'username
	     :writer #'(setf username))
   (email =>email
	  :reader #'email
	  :writer #'(setf email))
   (fullname =>string
	     :reader #'fullname
	     :writer #'(setf fullname))
   (password =>password
	     :reader #'password
	     :writer #'(setf password)))
  :documentation "A person description")

(description-attributes {person})

(attribute-name (get-attribute {person} 'fullname))
(attribute-type (get-attribute {person} 'fullname))

(define-attribute =>view (=>)
  ((label nil :accessor attribute-label)
   (view t :accessor attribute-view)
   (formatter #'prin1-to-string :accessor attribute-formatter))) 

(define-description {person-view} ({person})
  ((fullname =>view :label "Full name")
   (username =>view)
   (email =>view :label "E-mail")
   (password =>view :view nil)))

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

(display-object (make-instance 'person :username "mmontone"
			       :email "mariano@copyleft.no"
			       :password "lalala"
			       :fullname "Mariano Montone")
		{person-view})
       

(define-attribute =>serializable ()
  ((serialize nil :accessor attribute-serialize)
   (serializer nil :accessor attribute-serializer)
   (serialization-name nil :accessor attribute-serialization-name)))

(define-description {person-serialization} ({person})
  ((fullname =>serializable :serialize t)
   (email =>serializable :serialize t)
   (password =>serializable :serialize nil)))

(description-attributes {person-serialization})

(attribute-serialize (get-attribute {person-serialization} 'fullname))
(attribute-serialize (get-attribute {person-serialization} 'password))

(defun serialize-object (object description &optional (stream json:*json-output*))
  (json:with-object (stream)
    (loop for attribute in (description-attributes description)
       when (and (descendantp attribute =>serializable)
		 (attribute-serialize attribute))
       do (let ((serialization-name (or (attribute-serialization-name attribute)
					(string-downcase (symbol-name (attribute-name attribute)))))
		(attribute-value (if (attribute-reader attribute)
					 (funcall (attribute-reader attribute)
						  object)
					 (attribute-value attribute))))
	    (json:as-object-member (serialization-name stream)
	      (if (attribute-serializer attribute)
		  (funcall (attribute-serializer) attribute stream)
		  (json:encode-json attribute-value stream)))))))

;; Error!! And it is ok
;(attribute-serialize (get-attribute person-description 'fullname))

(defmessage accept (visitor attribute))
(defreply accept (visitor attribute)
  ;; Do nothing
  )

(defreply accept (visitor (attribute =>string))
  (visit-string-attribute visitor attribute)
  (call-next-reply))

(defun visit-string-attribute (visitor attribute)
  (format t "Visiting string attribute: ~A~%" attribute))

(accept nil
        (get-attribute {person-serialization} 'fullname))

(defreply accept (visitor (attribute =>serializable))
  (visit-serializable-attribute visitor attribute)
  (call-next-reply))

(defun visit-serializable-attribute (visitor attribute)
  (format t "Visiting serialize attribute: ~A~%" attribute))

(accept nil
        (get-attribute {person-serialization} 'fullname))

(define-attribute =>persistent (=>)
  ((persistent t :accessor attribute-persistent)
   (type-spec nil :accessor attribute-type-spec)))

(define-attribute =>persistent-string (=>persistent =>string)
  ((type-spec "VARCHAR")
   (type-size 255 :accessor attribute-type-size)))

(define-attribute =>validatable (=>)
  ((validate t :accessor attribute-validate)
   (validator nil :accessor attribute-validator)
   (required nil :accessor attribute-required)))

(define-attribute =>validatable-string (=>validatable =>string)
  ((validator (lambda (thing)
		(stringp thing)))))

(define-attribute =>validatable-boolean (=>validatable =>boolean)
  ((validator (lambda (thing)
		(typep thing 'boolean)))))

(define-attribute =>validatable-password (=>validatable =>password)
  ((validator (lambda (thing)
		(and (stringp thing)
		     (> (length thing) 6))))))

(defun valid-email-address-p (string)
  (not (null
	(ppcre:scan "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}$" string))))

(valid-email-address-p "hello")
(valid-email-address-p "mariano@copyleft.no")

(define-attribute =>validatable-email (=>validatable =>email)
  ((validator #'valid-email-address-p)))

(define-description {persistent-person} ({person})
  ((fullname =>persistent-string :persistent t)
   (email =>persistent-string :persistent t)
   (password =>persistent-string :persistent t)))

(attribute-persistent (get-attribute {persistent-person}'fullname))
(attribute-type-spec (get-attribute {persistent-person} 'fullname))

(define-attribute =>editable (=>)
  ((editable t :accessor attribute-editable)
   (editor nil :accessor attribute-editor)))

(define-attribute =>repl-editable-string (=>editable =>string)
  ((editor (lambda (attribute &optional object)
             (format t "Enter ~A:" (attribute-name attribute))
             (let ((input (read-line)))
               (if (attribute-writer attribute)
                   (funcall (attribute-writer attribute)
                            input
                            object)
                   (setf (attribute-value attribute) input)))))))

(define-description {person-repl-editing} ({person})
  ((fullname =>repl-editable-string)
   (email =>repl-editable-string)
   (password =>repl-editable-string))
  :documentation "Person REPL editing description")

(description-attributes {person-repl-editing})

(defun edit-object (object description)
  (loop for attribute in (description-attributes description)
     when (and (descendantp attribute =>editable)
	       (attribute-editable attribute))
     collect (funcall (attribute-editor attribute)
                      attribute
                      object))
  object)

(get-attribute {person-repl-editing} 'password)

(let ((person (make-instance 'person
			     :fullname "Mariano Montone"
                             :email "mariano@copyleft.no"
                             :password "lala")))
  (edit-object person
               {person-repl-editing})
  (describe person))

(define-description {person-validation} ({person})
  ((fullname =>validatable-string  :required t)
   (email =>validatable-email :required t)
   (password =>validatable-password :required t)))

(attribute-required (get-attribute {person-validation} 'fullname))

(defun validate-object (object description)
  (loop for attribute in (description-attributes description)
       when (and (descendantp attribute =>validatable)
		 (attribute-validate attribute))
       do (progn
	    ;(format t "Validating ~A~%" (attribute-name attribute))
	    (when (attribute-required attribute)
	      (when (not (slot-boundp object (attribute-name attribute)))
		(error "~A is required but not bound in ~A"
		       (attribute-name attribute)
		       object)))
	    (when (and (attribute-validator attribute)
		       (not (funcall (attribute-validator attribute)
				     (if (attribute-reader attribute)
					 (funcall (attribute-reader attribute)
						  object)
					 (attribute-value attribute)))))
	      (error "Validation error: ~A is not valid" (attribute-name attribute)))))
  t)	      

(let ((description {person-validation}))
  (let ((person (make-instance 'person)))
    (validate-object person description)))

(let ((description {person-validation}))
  (let ((person (make-instance 'person
			       :fullname "Mariano"
			       :email "email"
			       :password "1234567")))
    (validate-object person description)))

(let ((description {person-validation}))
  (let ((person (make-instance 'person
			       :fullname "Mariano"
			       :email "mariano@copyleft.no"
			       :password "1234567")))
    (validate-object person description)))

;; Descriptions compositions

(let ((description ({person-validation}
		    {person-repl-editing})))
    (let ((person (make-instance 'person)))
      (edit-object person description)
      (validate-object person description)
      (describe person)))

;; Another way of composing

(let ((description (make-description :parents
				     (list {person-validation}
					   {person-repl-editing}))))
  (let ((person (make-instance 'person)))
      (edit-object person description)
      (validate-object person description)
      (describe person)))

;; Or work with separate descriptions, without composing
(let ((person (make-instance 'person)))
  (edit-object person {person-repl-editing})
  (validate-object person {person-validation})
  (describe person)
  (print
   (with-output-to-string (s)
     (serialize-object person {person-serialization} s))))
