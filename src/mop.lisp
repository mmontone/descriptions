(in-package :descriptions)

(defclass described-object ()
  ((default-description
       :initarg :default-description
     :initform nil
     :accessor default-description)
   (parent-descriptions
    :initarg :parent-descriptions
    :initform (list {description})
    :accessor parent-descriptions)
   (default-description-name
       :initarg :default-description-name
     :initform nil
     :accessor default-description-name)))

(defclass described-object-class (standard-class)
  ((default-description
       :initarg :default-description
     :initform nil
     :accessor default-description)))

(defmethod closer-mop:validate-superclass
    ((class described-object-class)
     (superclass standard-class))
  t)

(defclass described-object-slot-mixin ()
  ((attribute :initarg :attribute
	      :accessor slot-attribute
	      :initform nil)
   (described-p :initarg :described-p 
		:initform t
		:accessor described-p)))

(defmethod described-p (slot)
  nil)

(defmethod slot-attribute-name (slot)
  nil)

(defmethod slot-attribute (slot)
  nil)

(defclass described-object-direct-slot-definition
    (described-object-slot-mixin closer-mop:standard-direct-slot-definition)
  ())

(defclass described-object-effective-slot-definition
    (described-object-slot-mixin closer-mop:standard-effective-slot-definition)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class described-object-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'described-object-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class described-object-class)
						       &rest initargs)
  (declare (ignore initargs))
  (find-class 'described-object-effective-slot-definition))

(defmethod closer-mop:compute-effective-slot-definition ((class described-object-class) name direct-slots)
   (let ((effective-slot (call-next-method)))
     (setf (described-p effective-slot)
           (some #'described-p direct-slots))
     (setf (slot-attribute effective-slot)
	   (some #'slot-attribute direct-slots))
     effective-slot))

(defgeneric described-slot-p (slot)
  (:method (slot)
    nil)
  (:method ((slot described-object-slot-mixin))
    (described-p slot)))

(defmethod shared-initialize :around ((class described-object-class) slot-names &rest args &key direct-superclasses)
  "Ensures we inherit from described-object."
  (let* ((described-object-metaclass (find-class 'described-object-class))
	 (described-object (find-class 'described-object))
	 (not-already-described-object (loop for superclass in direct-superclasses
					 never (eq (class-of superclass) described-object-metaclass))))
    (if (and (not (eq class described-object)) not-already-described-object)
	(apply #'call-next-method class slot-names
	       :direct-superclasses (append direct-superclasses (list described-object)) args)
	(call-next-method))))

(defmethod shared-initialize :after ((obj described-object) slot-names &rest args)
  "Initialize the default object description"
  (declare (ignore args))
  (let ((description-parents (parent-descriptions obj))
	(description-attributes
	 (cons
	  `(description-name ,(or (default-description-name obj)
				  (format nil "{~A}"
					  (string (class-name (class-of obj))))))
	  (loop for slot in (closer-mop:class-slots (class-of obj))
	     when (described-p slot)
	     collect
	       (let ((attribute-name (or (slot-attribute-name slot)
					 (closer-mop:slot-definition-name slot)))
		     (attribute (eval (slot-attribute slot))))
		 (setf (attribute-name attribute) attribute-name)
		 (list attribute-name
		       attribute))))))
    (let ((default-description (make-description :parents description-parents
						 :attributes description-attributes)))
      (setf (default-description obj) default-description)
      (setf (default-description (class-of obj)) default-description))))

(defun plist-keys (plist)
  (loop for key in plist by #'cddr
       collect key))

(defun plist-values (plist)
  (loop for value in (cdr plist) by #'cddr
       collect value))

(defmacro define-described-class (name direct-superclasses direct-slots &rest options)
  (let ((standard-slot-initargs
	 (list :initform
	       :accessor
	       :reader
	       :writer
	       :initarg
	       :documentation
	       :type
	       :described-p))
	(base-attribute-type (loop for option in options
				when (equalp (first option) :base-attribute-type)
				return (second option))))
    (flet ((collect-attribute-args (args)
	     (loop for arg in args by #'cddr
		for value in (cdr args) by #'cddr
		when (and (not (member arg standard-slot-initargs))
			  (not (member arg (list :described-p :attribute-type))))
		appending (list arg value))))
      `(defclass ,name ,direct-superclasses
	 ,(loop for slot in direct-slots
	     for slot-name = (first slot)
	     for slot-args = (cdr slot)
	     collect
	       (let ((attribute
		      (when (or (not (member :described-p (plist-keys slot-args)))
				(and (member :described-p (plist-keys slot-args))
				     (getf slot-args :described-p)))
			;; The slot is being described
			;; It needs to have a type
			
			;; Look for the attribute-type in attribute aliases
			(let ((attribute-type
			       (cond
				 ((getf slot-args :attribute-type)
				  (getf slot-args :attribute-type))
				 ((and (getf slot-args :type)
				       (get (getf slot-args :type) :attribute-alias))
				  (get (getf slot-args :type) :attribute-alias))
				 (t
				  (error "Provide the attribute type for ~A in class ~A" slot-name name)))))
			  ;; Collect the attribute arguments
			  (let ((attr-args (collect-attribute-args slot-args)))
			    ;; Add attribute properties from standard slot initargs, like :reader, :accessor, etc

			    ;; attribute name
			    (when (not (getf attr-args :name))
			      (setf attr-args (append `(:name ',slot-name) attr-args)))
		     
			    ;; attribute value
			    (when (and (not (getf attr-args :value))
				       (getf slot-args :initform)
				       (descendantp attribute-type =>valued))
			      ;; There's no :value property but :initform is provided on a =>valued attribute
			      ;; Use :initform as :value
			      (setf attr-args (append (list :value (getf slot-args :initform)) attr-args)))

			    ;; attribute reader
			    (when (not (getf attr-args :reader))
			      (cond
				((getf slot-args :reader)
				 (setf attr-args (append (list :reader `(function ,(getf slot-args :reader)))
							 attr-args)))
				((getf slot-args :accessor)
				 (setf attr-args (append (list :reader `(function ,(getf slot-args :accessor)))
							 attr-args)))
				(t (error "No accessor or reader specified for described slot ~A in class ~A" slot-name name))))

			    ;; attribute writer
			    (when (not (getf attr-args :writer))
			      (cond
				((getf slot-args :writer)
				 (setf attr-args (append (list :writer `(function (setf ,(getf slot-args :writer))))
							 attr-args)))
				((getf slot-args :accessor)
				 (setf attr-args (append (list :writer `(function (setf ,(getf slot-args :accessor))))
							 attr-args)))
				(t (error "No accessor or writer specified for described slot ~A in class ~A" slot-name name))))

			    ;; finally make the attribute
			    ;; compose the attribute type with a possible base-attribute-type
			    (let ((final-attribute-type (if base-attribute-type
							    (if (listp base-attribute-type)
								(if (listp attribute-type)
								    (append attribute-type base-attribute-type)
					;else
								    (cons attribute-type base-attribute-type))
					;else
								(if (listp attribute-type)
								    (append attribute-type (list base-attribute-type))
								    (list attribute-type base-attribute-type)))
					;else
							    attribute-type)))
			      `(make-attribute ,(if (listp final-attribute-type)
						    `(list ,@final-attribute-type)
						    final-attribute-type)
					       ,@attr-args)))))))
		 (if attribute
		     `(,slot-name ,@(loop for arg in slot-args by #'cddr
				       for value in (cdr slot-args) by #'cddr
				       when (member arg standard-slot-initargs)
				       appending (list arg value))
				  :attribute ,attribute)
		     `(,slot-name ,@slot-args))))
	 (:metaclass described-object-class)
	 ,@(let ((described-object-class-options (list :base-attribute-type)))
		(loop for option in options
		   when (destructuring-bind (key value) option
			  (declare (ignore value))
			  (not (member key described-object-class-options)))
		   collect option))))))
