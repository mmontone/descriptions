(in-package :{}.validation)

(define-attribute =>validatable (=>)
  ((validate t :accessor attribute-validate)
   (validator nil :accessor attribute-validator)
   (required nil :accessor attribute-required)))

(defreply attribute-validator ((attribute =>string))
  (lambda (thing)
    (stringp thing)))

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

(defreply attribute-validator ((attribute =>email))
  #'valid-email-address-p)

(define-attribute =>validatable-email (=>validatable =>email)
  ((validator #'valid-email-address-p)))

(defreply attribute-validator ((attribute =>single-option))
  (lambda (val)
    (member val (attribute-options attribute))))

(defun validate-object (object &optional (description (default-description object)))
  (loop for attribute in (description-attributes description)
       when (and (sheeple:descendantp attribute =>validatable)
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
