(defpackage #:descriptions.test
  (:nicknames :{}.test)
  (:use #:cl
	#:descriptions
	#:descriptions.serialization
	#:descriptions.validation
	#:stefil
	#:alexandria)
  (:export #:descriptions-tests))

(in-package :{}.test)

(in-root-suite)

(defsuite descriptions-tests)

(in-suite descriptions-tests)

(defclass person ()
  ((id :accessor id)
   (username :initarg :username
         :accessor username)
   (fullname :initarg :fullname
             :accessor fullname)
   (email :initarg :email
          :accessor email)
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

(deftest undefined-attribute-test ()
  (loop for attribute in (description-attributes {person})
     do
       (is (undefinedp (attribute-value attribute)))
       (is (attribute-undefinedp attribute)))
  (let ((attribute (make-attribute =>string :value "Hello")))
    (is (not (undefinedp (attribute-value attribute))))
    (is (not (attribute-undefinedp attribute))))
  (let ((attribute (make-attribute =>string)))
    (is (undefinedp (attribute-value attribute)))
    (is (attribute-undefinedp attribute))))
  
(deftest with-described-object-test ()
  (let ((person (make-instance 'person
			       :username "johnnash"
			       :fullname "John Nash"
			       :email "johnnash@gmail.com"
			       :password "123456")))
    (with-described-object (username email fullname password)
	(person {person})
      (is (equalp username "johnnash"))
      (is (equalp fullname "John Nash"))
      (is (equalp email "johnnash@gmail.com"))
      (is (equalp password "123456")))))

(deftest make-attribute-test ()
  (let ((attribute (make-attribute =>string :value "Hello")))
    (is (equalp (attribute-value attribute) "Hello"))
    (is (set-equal (attribute-properties attribute)
		   (list :WRITER :READER :VALUE :TYPE :NAME)))
    (signals error
      (attribute-view attribute)))
  (let ((attribute (make-attribute (list =>string =>view) :value "Bye"
				   :view t)))
    (is (equalp (attribute-value attribute) "Bye"))
    (is (equalp (attribute-view attribute) t))
    (is (set-equal (attribute-properties attribute)
		   (list :WRITER :READER :VALUE :TYPE :NAME :LABEL :VIEW :FORMATTER)))))

(define-description {person-view} ({person})
  ((fullname =>view :label "Full name")
   (username =>view)
   (email =>view :label "E-mail")
   (password =>view :view nil)))

(define-description {person-serialization} ({person})
  ((fullname =>serializable :serialize t)
   (username =>serializable :serialize nil)
   (email =>serializable :serialize t)
   (password =>serializable :serialize nil)))

(deftest description-composition-test ()
  (signals error
    (attribute-view (get-attribute {person} 'fullname)))
  (signals error
    (attribute-view (get-attribute {person} 'password)))
  
  (is (equalp (attribute-view (get-attribute {person-view} 'fullname)) t))
  (is (equalp (attribute-view (get-attribute {person-view} 'password)) nil))
  
  (signals error
    (attribute-serialize (get-attribute {person} 'fullname)))
  (signals error
    (attribute-serialize (get-attribute {person-view} 'fullname)))

  (is (equalp (attribute-serialize (get-attribute {person-serialization} 'fullname)) t))
  (is (equalp (attribute-serialize (get-attribute {person-serialization} 'password)) nil))
  
  (let ((composed-description (make-description :parents (list {person-view} {person-serialization}))))
    (is (equalp (attribute-view (get-attribute composed-description 'fullname)) t))
    (is (equalp (attribute-view (get-attribute composed-description 'password)) nil))
    (is (equalp (attribute-serialize (get-attribute composed-description 'fullname)) t))
    (is (equalp (attribute-serialize (get-attribute composed-description 'password)) nil)))

  (let ((composed-description ({person-view} {person-serialization})))
    (is (equalp (attribute-view (get-attribute composed-description 'fullname)) t))
    (is (equalp (attribute-view (get-attribute composed-description 'password)) nil))
    (is (equalp (attribute-serialize (get-attribute composed-description 'fullname)) t))
    (is (equalp (attribute-serialize (get-attribute composed-description 'password)) nil)))

  (let ((composed-description ({person-serialization} {person-view})))
    (is (equalp (attribute-view (get-attribute composed-description 'fullname)) t))
    (is (equalp (attribute-view (get-attribute composed-description 'password)) nil))
    (is (equalp (attribute-serialize (get-attribute composed-description 'fullname)) t))
    (is (equalp (attribute-serialize (get-attribute composed-description 'password)) nil))))


(define-described-class user ()
  ((id :accessor id
       :described-p nil)
   (username :initarg :username
	     :accessor username
	     :type string)
   (fullname :initarg :fullname
             :accessor fullname
	     :type string
	     :attribute-type =>string
	     :view t
	     :serialize t)
   (email :initarg :email
          :accessor email
	  :attribute-type =>email
	  :view t
	  :serialize t)
   (password :initarg :password
	     :accessor password
	     :attribute-type =>password
	     :view nil)
   (sex :initarg :sex
	:accessor sex
	:attribute-type =>single-option
	:reference =>keyword
	:options (list :male :female)
	:formatter (lambda (val)
		     (string-capitalize (symbol-name val)))
	:view t)
   (friends :initarg :friends
	    :accessor friends
	    :initform nil
	    :attribute-type =>to-many-relation
	    :reference (default-description (find-class 'user))
	    ))
  (:base-attribute-type (=>view =>serializable =>validatable))
  (:documentation "A user"))

(deftest described-class-test ()
  (finishes
    (let ((user (make-instance 'user
			       :username "mmontone"
			       :password "1234567"
			       :fullname "Mariano Montone"
			       :email "marianomontone@gmail.com"
			       :sex :male)))
      (default-description user)
      (description-attributes
       (default-description user))
      (validate-object user)
      (display-object user)
      (with-output-to-string (json:*json-output*)
	(serialize-object user))))

  (signals clavier:validation-error
    (let ((user (make-instance 'user
			       :username nil
			       :password "1234567"
			       :fullname "Mariano Montone"
			       :email "marianomontone@gmail.com"
			       :sex :male)))
      (default-description user)
      (description-attributes
       (default-description user))
      (validate-object user)
      (display-object user)
      (with-output-to-string (json:*json-output*)
	(serialize-object user)))))
