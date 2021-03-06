Getting started
---------------

**{} descriptions** is a very dynamic meta-description library. Meta description of the application's domain model is done defining descriptions for the different aspects is desired to represent. This helps automate the generation of different views, editors, serialization schemas, validation procedures for the domain model objects and to avoid very repetitive and error-prone work when those objects change its shape.

Domain model meta descriptions consist of the definition of description objects for its different aspects (viewing, editing, validation, persistence, serialization, etc). Description objects are a collections of attributes, can inherit from each other, and are composable.

Let's go through an example to see how this works.

Say we have a *person* model object:

.. code-block:: common-lisp
		
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

We would like to print instances of that model on the screen. So first we define a *{person}* description that just describes which attributes *person* model objecs possess and their "types".

.. code-block:: common-lisp
		
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

Descriptions names are enclosed between brackets ``{`` ``}`` as a naming convention. Also, notice that the attribute types (``=>string``, ``=>email``, ``=>password``), begin with ``=>``. Attributes types begin with ``=>`` as a naming convention.

Now we can use the information on the attributes of a person, and its types, to print a description of a person on the screen. To do that we define a new description that indicates which person slots we would like to print, and in which order.

.. code-block:: common-lisp
		
   (define-description {person-view} ({person})
     ((fullname =>view :label "Full name")
      (username =>view)
      (email =>view :label "E-mail")
      (password =>view :view nil)))

We can see that the password is disabled for viewing, that "Full name" and "E-mail" are used for labels, and that fullname, username, email and password are to be displayed in that order.

We can see that in action:

.. code-block:: common-lisp
		
   (display-object (make-instance 'person :username "mmontone"
				  :email "mariano@copyleft.no"
				  :password "lalala"
				  :fullname "Mariano Montone")
		   {person-view})

prints::
		
   Full name: "Mariano Montone"
   Username: "mmontone"
   E-mail: "mariano@copyleft.no"

   ### Descriptions composition

Descriptions can be composed using inheritance. Multiple inheritance is supported and they are implemented on top of a prototype based object system, so they can be composed in run time on the fly.

Attributes composition
~~~~~~~~~~~~~~~~~~~~~~

When inheriting from other descriptions, attributes with the same name are collapsed into one containing all the attribute properties.

For example, consider the basic *{person}* description we had before. We can ask for an attribute name and type, but asking if that attribute is going to be displayed throws an error, because that attribute property does not belong to the {person} description, but the *{person-view}* description we saw before.

.. code-block:: common-lisp-repl

   {}> (attribute-name (get-attribute {person} 'fullname))
   FULLNAME
   {}> (attribute-type (get-attribute {person} 'fullname))
   #<Object =>STRING {1005A0A0A3}>
   {}> (attribute-view (get-attribute {person} 'fullname))
   ;; Error


But if we ask the same to the *{person-view}* description, we can access to all the attributes properties.

.. code-block:: common-lisp-repl
   
   {}> (attribute-name (get-attribute {person-view} 'fullname))
   FULLNAME
   {}> (attribute-type (get-attribute {person-view} 'fullname))
   #<Object =>STRING {1005A0A0A3}>
   {}> (attribute-view (get-attribute {person-view} 'fullname))
   T

This makes the approach layered, with each description describing different aspects of the same model and extending other more basic descriptions.

Descriptions composition
~~~~~~~~~~~~~~~~~~~~~~~~

As we mentioned before, descriptions can be composed on the fly thanks to the prototype object system the library is implemented in.

For example, we can create new descriptions with the *make-description* function, passing the descriptions we want to compose as parents:

.. code-block:: common-lisp
   
   (let ((description (make-description :parents
					(list {person-validation} {person-repl-editing}))))
     (let ((person (make-instance 'person)))
	 (edit-object person description)
	 (validate-object person description)
	 (describe person)))

A prettier way of doing it is using the description name as a function:

.. code-block:: common-lisp
   
   (let ((description ({person-validation} {person-repl-editing})))
       (let ((person (make-instance 'person)))
	 (edit-object person description)
	 (validate-object person description)
	 (describe person)))

We can also choose not to compose descriptions, but work with them separately on the different aspects of the model objects:

.. code-block:: common-lisp
   
   (let ((person (make-instance 'person)))
     (edit-object person {person-repl-editing})
     (validate-object person {person-validation})
     (describe person)
     (print
      (with-output-to-string (s)
	(serialize-object person {person-serialization} s))))
