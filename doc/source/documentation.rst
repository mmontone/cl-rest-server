.. highlightlang:: common-lisp
.. cl:package:: rest-server
		   
API documentation
-----------------

There's an (incomplete) implementation of a `Swagger <https://helloreverb.com/developers/swagger>`_ export.

First, configure the api for Swagger::

  (define-swagger-resource api)

This will enable `CORS <https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS>`_ on the API, as Swagger needs it to make requests.

After this you can download the Swagger documentation tool and point to the api HTTP address.
