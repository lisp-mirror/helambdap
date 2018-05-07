HEΛP
====

Copyright (c) 2012-2018 Marco Antoniotti and Stefano Gandelli
See file COPYING for licensing information


DESCRIPTION
-----------

HEΛP ("HELP") is yet another CL code documentation utility, which has
been inspired by JavaDoc and doxygen.

HEΛP produces, by default, a (mostly) XHTML set of documents. The
system strives to be simple to use by making a few choices, while
maintaining a number of hooks that can be used to customize the
system, e.g., by producing a different kind of output (e.g., Texinfo).

The simplest way to get HEΛP (pun intended) is to use the function
document, as in the example below, where a few assumptions are made.

We are in the directory where the code to be documented resides.  The
actual documentation will end up in the directory (using POSIX syntax
and conventions) `./docs/html/`.

~~~~
    cl-prompt> (document *the-current-directory* :documentation-title "My Library")
    ;;; Several messages, some warnings and error messages.
    ;;; More messages, more warnings and error messages (these last ones ignored.).
    ;;; Yadda, yadda, yadda....
    NIL
~~~~

As stated, the previous command will create the documentation (with an
index.htm file in the `./docs/html/` folder.  Alas, note that the
`*the-current-directory*` must be obtained in an
implementation-dependent way.  Note also that the default format for
the documentation is (mostly) XHTML.

The documentation thus generated is incomplete.  Some HTML files are
either skeletons or are not there (and your browser may complain
accordingly). It is up to you to fill these files with appropriate
content: a prominent example is the file named introduction.htm which
appears on the first page. It usually appears blank.


INSTALLATION
------------

HEΛP is available on quicklisp.

Otherwise, installation should be simple if you have quicklisp, ASDF,
or MK-DEFSYSTEM set up.  You will need also XHTMΛ and a few other
libraries.  Check out the system files for the current list.


A NOTE ON FORKING
-----------------

Of course you are free to fork the project subject to the current
licensing scheme.  However, before you do so, I ask you to consider
plain old "cooperation" by becoming asking me to become a developer.
It helps keeping the entropy level at an acceptable level.


Enjoy

Marco Antoniotti 2017-03-29
