********
FX-Error
********

Demonstration project for an error handling strategy in modern Fortran. It
provides an error handling mechanism which is

* robust: An unhandled error stops the code automatically,

* convenient: propagation of errors through call stack is possible,

* pure: it only requires Fortran 2008 constructs.

It is written in pure Fortran 2008, but using Fypp-constructs can help to
simplify its usage somewhat. See the `app/test_errorfx.f90
<app/test_errorfx.f90>`_ and `app/test_errorfx_fypp.fpp
<app/test_errorfx_fypp.fpp>`_ for an example with and without Fypp constructs,
respectively.


Building
========

Use the usual CMake workflow to build the project::

  mkdir build
  cd build
  cmake ..
  make

The executables cann be found in the `./app` folder after the build.

You need the `Fypp preprocessor <https://github.com/aradi/fypp>`_ to build the
Fypp-based example. If you want to build the project without the Fypp-based
example, pass the ``-DWITH_FYPP=NO`` option to CMake.


License
=======

The project is released under the *BSD 2-clause license*.
