* MOP for descriptions in defclass:

```lisp
(defclass person ()
  ((id :accessor id)
   (username :initarg :username
	     :accessor username
	     :type =>string
	     :view t
	     :serialize t
	     :required t)
   (email :initarg :email
          :accessor email
	  :type =>email
	  :validate t
	  :serialize t
	  :required t)
   (fullname :initarg :fullname
             :accessor fullname
	     :type =>string
	     :required t)
   (password :initarg :password
	     :accessor password
	     :type =>password
	     :view nil
	     :serialize nil))
  (:metaclass described-class)
  (:attribute-type (=>validated =>view =>serialized)))

(description (make-instance 'person))

(define-described-class person
  ((id :accessor id)
   (username :initarg :username
	     :accessor username
	     :view t
	     :serialize t
	     :required t)
   (email :initarg :email
          :accessor email
	  :validate t
	  :serialize t
	  :required t)
   (fullname :initarg :fullname
             :accessor fullname
	     :required t)
   (password :initarg :password
	     :accessor password
	     :view nil
	     :serialize nil))
   (:attribute-type (=>validated =>view =>serialized)))
```

* Attributes composition at definition

```lisp
(define-description {person}
   ((fullname (=>string =>validated =>view)
       :validate t
       :required t
       :view t
       :label "Full name")))
```
* Descriptions of descriptions (meta described descriptions)

* More unit tests
