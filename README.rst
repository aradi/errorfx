**************
Error handling
**************

Demonstration project for an error handling strategy in Fortran. It provides an
error handling mechanism which is

* robust: An unhandled error stops the code automatically,

* convenient: propagation of errors through call stack is possible,

* pure: it only requires Fortran 2008 constructs.

It is written in pure Fortran 2008, but using Fypp-constructs can help to
simplify its usage somewhat. See the `src/test_errorhandling.f90
<src/test_errorhandling.f90>`_ and `src/test_errorhandling_fypp.fpp
<src/test_errorhandling_fypp.fpp>`_ for an example with and without Fypp
constructs, respectively.


Building
========

Use the usual CMake workflow to build the project::

  mkdir build
  cd build
  cmake ..
  make

You need to have `Fypp preprocessor` installed so that the Fypp-based example
can be built. If you want to build without the Fypp-based example, pass the
``-DWITH_FYPP=FALSE`` option to CMake.


License
=======

The project is released under the *BSD 2-clause license*.
