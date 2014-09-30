.. highlightlang:: common-lisp
.. cl:package:: rest-server

Error handling
==============

APIs can be run with different error handling modes. This is controlled passing the desired mode to :cl:function:`start-api`. Can be one of ``:development``, ``:testing``, or ``:production``. Default is ``:production``.

.. cl:variable:: *development-mode*

Production mode
---------------

In production mode, when an error occurs, 505 internal server error is returned.

Testing mode
------------

In testing mode, when an error occurs the condition is serialized so it is possible to see what went wrong when accessing the API to some extent.

Development mode
----------------

In development mode, when an error occurs, the Lisp debugger is entered.

Global error mode
-----------------

To setup a global error hanling mode, that has precedence to individual running apis error handling modes, assign one of ``:development``, ``:testing``, or ``:production`` to the :cl:function:`*SERVER-DEVELOPMENT-MODE*` variable.

.. cl:variable:: *server-development-mode*
