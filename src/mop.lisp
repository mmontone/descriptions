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
  ())

(defmethod closer-mop:validate-superclass
    ((class described-object-class)
     (superclass standard-class))
  t)

(defclass described-object-slot-mixin ()
  ((attribute-name :initarg :attribute-name
		   :initform nil
		   :accessor slot-attribute-name)
   (attribute-type :initarg :attribute-type
		   :initform nil
		   :accessor slot-attribute-type)
   (described-p :initarg :described-p 
		:initform t
		:accessor described-p)))

(defmethod described-p (slot)
  nil)

(defmethod slot-attribute-type (slot)
  nil)

(defmethod slot-attribute-name (slot)
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
     (setf (slot-attribute-type effective-slot)
	   (some #'slot-attribute-type direct-slots))
     (setf (slot-attribute-name effective-slot)
	   (some #'slot-attribute-name direct-slots))
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
		     (attribute (eval (slot-attribute-type slot))))
		 (setf (attribute-name attribute)
		       attribute-name)
		 ;; We want to set readers and writers here too
		 ;; But I don't know how to do it at the moment,
		 ;; closer-mop:slot-definition-readers and
		 ;; closer-mop:slot-definition-writers is not giving anything useful
		 
		 ;; (setf (attribute-reader attribute)
		 ;;       (first (closer-mop:slot-definition-readers slot)))
		 ;; (setf (attribute-writer attribute)
		 ;;       (first (closer-mop:slot-definition-writers slot)))
		 
		 (list attribute-name
		       attribute))))))
    (setf (default-description obj)
	  (make-description :parents description-parents
			    :attributes description-attributes))))
