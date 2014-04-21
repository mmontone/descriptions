Overview
--------

**{} descriptions** is a meta level descriptions library for Common Lisp.

It is inspired by `Smalltalk Magritte`_, as well as `Lisp On Lines`_.

.. cl:package:: descriptions

.. cl:macro:: with-description-attributes

   :param attributes: the description attributes names.
   :param description: the description with the attributes.
   :param body: the body of the macro.

   .. code-block:: common-lisp

      (with-description-attributes (username password) {person}
	(print username)
	(print password))

.. cl:macro:: define-attribute

.. cl:function:: make-attribute
.. cl:function:: description-attributes		 

.. _Smalltalk Magritte: http://code.google.com/p/magritte-metamodel
.. _Lisp On Lines: http://www.cliki.net/lisp-on-lines
