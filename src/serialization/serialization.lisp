(in-package :{}.serialization)

(define-attribute =>serializable ()
  ((serialize nil :accessor attribute-serialize)
   (serializer nil :accessor attribute-serializer)
   (serialization-name nil :accessor attribute-serialization-name)))

(defun serialize-object (object &optional (description (default-description object))
				  (stream json:*json-output*))
  (json:with-object (stream)
    (loop for attribute in (description-attributes description)
       when (and (sheeple:descendantp attribute =>serializable)
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
