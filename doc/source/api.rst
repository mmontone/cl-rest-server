.. highlightlang:: common-lisp
.. cl:package:: rest-server
		   
API definition
--------------

APIs are defined using the :cl:function:`DEFINE-API` macro. APIs contain resources and resources contain api-functions.

.. cl:macro:: define-api

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

Resources can be added to an already defined API via the :cl:function::`with-api` and :cl:function:`define-api-resource` macros

.. cl:macro:: with-api
.. cl:macro:: define-api-resource	      

Resource options
^^^^^^^^^^^^^^^^

- ``:produces``: A list of content types produced by this resource. The content types can be ``:json``, ``:html``, ``:xml``, ``:lisp``
- ``:consumes``: A list of content types consumed by this resource.
- ``:documentation``: A string describing the resource. This appears in the generated API documentation.
- ``:path``: The resource path. Should start with the `/` character. Ex: `"/users"`
- ``:models``: A list of `models` used by the resource

Resource operations
===================

Resources provide a set of operations to access them.

They have the following syntax:

.. code-block:: common-lisp

   (<resource-operation-name> <resource-operation-options> <resource-operation-arguments>)

New operations can be added to an already defined resource via the :cl:function:`with-api-resource`

.. cl:macro:: with-api-resource

Resource operation options
^^^^^^^^^^^^^^^^^^^^^^^^^^

- ``:request-method``: The HTTP request method
- ``:path``: The operation path. Arguments in the operation are enclosed between ``{}``. For example: ``"/users/{id}"``.  
- ``:produces``: A list of content types produced by the operation. The content types can be ``:json``, ``:html``, ``:xml``, ``:lisp``. This is matched with the HTTP "Accept" header.
- ``:consumes``: A list of content types that the operation can consume.
- ``:authorizations``: A list with the authorizations required for the operation. Can be one of ``:token``, ``:oauth``, ``:oauth``, or a custom authorization type.  
- ``:documentation``: A string describing the operation. This appears in the generated API documentation.

Resource operation arguments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
		       (expand :list nil "Attributes to expand")))))

API implementation
------------------

APIs need to implement its resources operations. This is done via the :cl:function:`implement-resource-operation` macro.

.. cl:macro:: implement-resource-operation

The required arguments of the resource operation appear as normal arguments in the function, in the order in which they were declared. The optional arguments of a resource operation appear as `&key` arguments of the function. In case the resource operation request method is either **PUT** or **POST**, then a ``posted-content` argument should be added to the implementation function as the first argument.

Some examples:

For this operation::

  (get-user (:request-method :get
   		       :produces (:json)
		       :path "/users/{id}"
		       :documentation "Retrive an user")
		      ((id :integer "The user id")
		       &optional
		       (expand :list nil "Attributes to expand")))
  
The following resource implementation should be defined::

  (implement-resource-operation get-user (id &key expand)
     (serialize (find-user id) :expand expand))

And for this POST operation::

  (create-user (:request-method :post
			       :consumes (:json)
			       :path "/users"
			       :documentation "Create a user"
			       :body-type user)
		      ())

The ``posted-content`` argument should be included::

  (implement-resource-operation create-user (posted-content)
     (with-posted-content (name age) posted-content
         (serialize (model:create-user :name name :age age))))

Starting the API
----------------

APIs are started calling the function :cl:function:`start-api`

.. cl:function:: start-api
		       

Accessing the API
-----------------

The :cl:function:`define-api` macro creates a function for accessing the api for each resource operation.

Before using the generated functions, the api backend needs to be selected via the :cl:function:`with-api-backend`.

.. cl:macro:: with-api-backend

For instance, for the api defined above, an ``get-user`` and a ``get-users`` functions are created, which can be used like this::

  (with-api-backend "http://localhost/api"
     (get-user 22))

Assuming the api is running on http://localhost/api     






