(in-package :{}.validation)

(define-attribute =>validatable (=>)
  ((validate t :accessor attribute-validate)
   (validator nil :accessor attribute-validator)
   (required nil :accessor attribute-required)))

(defreply attribute-validator ((attribute =>string))
  (is-a-string))

(defreply attribute-validator ((attribute =>boolean))
  (is-a-boolean))

(defreply attribute-validator ((attribute =>password))
  (&& (is-a-string)
      (fn (lambda (x)
	    (> (length x) 6))
	  "Password is too short")))

(defreply attribute-validator ((attribute =>email))
  (valid-email))

(defreply attribute-validator ((attribute =>single-option))
  (one-of (attribute-options attribute)))  

(defun validate-object (object &optional (description (default-description object)))
  (with-signal-validation-errors (t)
    (loop for attribute in (description-attributes description)
       when (and (sheeple:descendantp attribute =>validatable)
		 (attribute-validate attribute))
       do (progn
					;(format t "Validating ~A~%" (attribute-name attribute))
	    (when (attribute-required attribute)
	      (when (not (slot-boundp object (attribute-name attribute)))
		(validation-error attribute
				  "~A is required but not bound in ~A"
				  (attribute-name attribute)
				  object)))
	    (when (and (attribute-validator attribute)
		       (funcall (attribute-validator attribute)
				(if (attribute-reader attribute)
				    (funcall (attribute-reader attribute)
					     object)
				    (attribute-value attribute)))))))))
