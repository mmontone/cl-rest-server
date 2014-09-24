API definition
--------------

APIs are defined using the define-api macro. APIs contain resources and resources contain api-functions.

This is the syntax:

.. code-block:: common-lisp
		
   (define-api <api-name> <options-plist>
      &rest
      <resources>)

API options
===========

- ``:title``: The API title. This appears in the generated API documentation
- ``:documentation``: A string with the API description. This appears in the generated API documentation.

Resources
=========

Resources have the following syntax:

.. code-block:: common-lisp

   (<resource-name> <resource-options> <api-functions>)

Resource options
^^^^^^^^^^^^^^^^

- ``:produces``: A list of content types produced by this resource. The content types can be ``:json``, ``:html``, ``:xml``, ``:lisp``
- ``:consumes``: A list of content types consumed by this resource.
- ``:documentation``: A string describing the resource. This appears in the generated API documentation.
- ``:path``: The resource path. Should start with the `/` character. Ex: `"/users"`
- ``:models``: A list of `models` used by the resource

API functions
=============

API functions belong to resources. They are the operations to be executed on the resource.

They have the following syntax:

.. code-block:: common-lisp

   (<api-function-name> <api-function-options> <api-function-arguments>)

API function options
^^^^^^^^^^^^^^^^^^^^

- ``:request-method``: The HTTP request method
- ``:path``: The operation path. Arguments in the operation are enclosed between ``{}``. For example: ``"/users/{id}"``.  
- ``:produces``: A list of content types produced by the operation. The content types can be ``:json``, ``:html``, ``:xml``, ``:lisp``. This is matched with the HTTP "Accept" header.
- ``:consumes``: A list of content types that the operation can consume.
- ``:authorizations``: A list with the authorizations required for the operation. Can be one of ``:token``, ``:oauth``, ``:oauth``, or a custom authorization type.  
- ``:documentation``: A string describing the operation. This appears in the generated API documentation.

API function arguments
^^^^^^^^^^^^^^^^^^^^^^

Arguments lists have the following syntax:

.. code-block:: common-lisp

   (*<required-arguments> &optional <optional-arguments>)

Required arguments are those appearing in the api function path between ``{}``.
They are specified like this:

.. code-block:: common-lisp
		
   (<argument-name> <argument-type> <documentation-string>)

Argument type can be one of: ``string``, ``integer``, ``boolean``, ``list``.

Optional arguments are those that can be passed after the ``?`` in the url. For instance, the ``page`` parameter in this url: ``/users?page=1``. They are listed after the ``&optional`` symbol, and have the following syntax:

.. code-block:: common-lisp

   (<argument-name> <argument-type> <default-value> <documentation-string>)

Here is an example of an api function arguments list:

.. code-block:: common-lisp

   ((id :integer "The user id")
     &optional (boolean :boolean nil "A boolean parameter")
               (integer :integer nil "An integer parameter")
	       (string :string nil "A string parameter")
	       (list :list nil "A list parameter"))

API example
===========

Here is a complete example of an API interface:

.. code-block:: common-lisp

   (define-api api-test
       (:title "Api test"
	       :documentation "This is an api test")
     (parameters (:produces (:json)
			    :consumes (:json)
			    :documentation "Parameters test"
			    :path "/parameters")
		 (parameters (:produces (:json)
					:consumes (:json)
					:documentation "Parameters test"
					:path "/parameters")
			     (&optional (boolean :boolean nil "A boolean parameter")
					(integer :integer nil "An integer parameter")
					(string :string nil "A string parameter")
					(list :list nil "A list parameter"))))
     (users (:produces (:json :xml)
		       :consumes (:json)
		       :documentation "Users operations"
		       :models (user)
		       :path "/users")
	    (get-users (:request-method :get
					:produces (:json)
					:path "/users"
					:documentation "Retrive the users list")       
		       (&optional (page :integer 1 "The page")
				  (expand :list nil "Attributes to expand")))
	    (get-user (:request-method :get
				       :produces (:json)
				       :path "/users/{id}"
				       :documentation "Retrive an user")
		      ((id :integer "The user id")
		       &optional
		       (expand :list nil "Attributes to expand")))

