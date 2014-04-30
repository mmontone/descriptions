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


(defclass user ()
  ((id :accessor id
       :described-p nil)
   (username :initarg :username
	     :accessor username
	     :attribute-type =>string)
   (fullname :initarg :fullname
             :accessor fullname
	     :attribute-type (=> (=>view =>string) :view t))
   (email :initarg :email
          :accessor email
	  :attribute-type =>email)
   (password :initarg :password
	     :accessor password
	     :attribute-type =>password))
  (:metaclass described-object-class))

(let ((user (make-instance 'user)))
  (description-attributes
   (default-description user))
  (display-object user))

(closer-mop:class-direct-slots (find-class 'user))

(=> (=>string =>view) :view t)
