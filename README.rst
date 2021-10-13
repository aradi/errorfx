*******
ErrorFx
*******

Simple library for demonstrating exception like error handling strategy in
Fortran. It provides an error handling mechanism which is

* robust: An unhandled error stops the code automatically,

* convenient: propagation of errors through call stack is easily possible,

* pure: it only requires Fortran 2008 constructs.

It is written in pure Fortran 2008, but Fypp-constructs can help to
simplify its usage even further. See the `<examples>`_ folder for various
examples. The ``*.f90`` files are written in pure Fortran, while their
``*_fypp.fpp`` equivalent are demonstrating the same functionality using also
Fypp-macros for more convenience and more compact code.


Building
========

Use the usual CMake workflow to build the project::

  mkdir build
  cd build
  cmake ..
  make

The executables built from the examples can be found in the `./examples` folder
in the build directory.

You need the `Fypp preprocessor <https://github.com/aradi/fypp>`_ to build the
Fypp-based example. If you want to build the project without the Fypp-based
example, pass the ``-DWITH_FYPP=NO`` option to CMake.


License
=======

The project is released under the *BSD 2-clause license*.
