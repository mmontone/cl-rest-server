.. highlightlang:: common-lisp
.. cl:package:: rest-server
		   
API configuration
-----------------

Some aspects of the api can be configured either passing the configuration parameters to the :cl:function:`start-api` function, or via the :cl:function:`configure-api` function.

.. cl:function:: configure-api

CORS configuration
==================

APIs can be configured to append `CORS <https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS>`_ headers to responses.

Syntax::

  (configure-api api '(:cors &rest options))

Options:
^^^^^^^^

- ``:enabled``: Boolean. CORS enabled when ``T``.
- ``:allow-origin``: The "AllowOrigin" header. Default: ``"*"``
- ``:allow-headers``: A list. The "AllowHeaders" header.
- ``:allow-methods``: A list. The "AllowMethods" header. Default: ``(list :get :put :post :delete)``

Logging configuration
=====================

Log api requests and responses.

Syntax::

  (configure-api '(:logging &rest options))

Then evaluate :cl:function::`start-api-logging`

.. cl:function:: start-api-logging
