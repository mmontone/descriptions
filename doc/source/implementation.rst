Implementation
--------------

**{} descriptions** is implemented as a thin layer over Sheeple_ prototypes. As a consequence, descriptions and its attributes can be easily be combined in run time.

.. _Sheeple: http://www.cliki.net/sheeple

Why not just use metaclasses instead?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is not possible to achieve quite the same with metaclasses. They do not allow to keep the metamodel independent of the actual implementation of the class. It should be possible to exchange descriptions on the fly, and even use multiple descriptions at the same time for the same underlying domain object.

