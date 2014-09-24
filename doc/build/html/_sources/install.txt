Install
-------

Download the source code from https://github.com/mmontone/cl-rest-server and point `.asd` system definition files from ``./sbcl/system (ln -s <system definition file path>)`` and then evaluate:

.. code-block:: common-lisp

   (require :rest-server)

from your lisp listener. 

You will also need to satisfy these system dependencies:

- `alexandria`
- `cxml` and `cl-json` for the serialization module
- `cl-ppcre` for the validation module

The easiest way of installing those packages is via Quicklisp_

This library is under the MIT licence.

.. _Quicklisp: http://www.quicklisp.org
