Common Lisp Interface Manager: The Original Series
==================================================

Description
-----------

Common Lisp Interface Manager 2.0 based on the open source release by
Franz, Inc. TOS stands for The Original Series as a reference to Star
Trek: The Original Series.

See the file LICENSE for information on the license for this source
code.

Running the software
--------------------

To manage dependencies `clim-tos` uses ASDF system definitions and
Quicklisp. To use the software clone its source code to
`~/quicklisp/local-projects` and call:

    (ql:quickload '(clim-tos clim-tos/clx-backend clim-tos/examples))
    (clim-demo:start-demo)

Development status
------------------

Currently software has many problems which may result in application
crashes. It is not overly stable, but runs on SBCL and CCL with X11
server backend.

Project goals are to make it work reliably on conforming Common Lisp
implementations which feature Gray Streams and MOP extensions.
