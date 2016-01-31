.. highlightlang:: common-lisp
.. cl:package:: rest-server

Error handling
==============

APIs can be run with different error handling modes. This is controlled via the argument `:catch-errors` in :cl:function:`start-api`. Default is NIL.

.. cl:variable:: *catch-errors*

If T, then the error is serialize and the corresponding HTTP is returned. Otherwise, when an error occurs, the Lisp debugger is entered.

Global error mode
-----------------

To setup a global error handling mode, that has precedence to individual running apis error handling modes, set :cl:function:`*SERVER-CATCH-ERRORS*` variable.

.. cl:variable:: *server-catch-errors*
