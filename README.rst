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

The project is released under the *BSD 2-clause license*.


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


General concept
===============

The general recipe for an exception like error handling is pretty
straightforward:

* Define a derived type which allows to store the relevant information about
  your critical errors. Add an activation flag with ``.false.`` (deactivated)
  as default value. Add a finalizer, which calls ``error stop`` if it is called
  for an activated instance.

* Create an allocatable (but non-allocated) instance of the error type and
  pass it to the routine which may experience fatal error. The
  corresponding dummy argument should have the ``allocatable, intent(out)``
  attributes. The allocation status of the variable on return will signalize the
  error status (non-allocated: no error, allocated: error).

* If an error occurs within the called routine, it must allocate the
  error variable, fill it up with the necessary information, set its
  activation flag to ``.true.`` and return.

* In the calling routine, check the allocation status of the error variable.
  immediately after the call. If it is allocated (error occured), either
  propagate it further up or handle it locally. If you handle it locally,
  deactivate the error (by setting its activation flag to ``.false.``), execute
  the handling code and then finally deallocate the error.

This mechanism provides a very flexible and robust error handling. Errors
can be easily propgated upwards to the top level (as it should be the case for
robust libraries). Additionally, it ensures, that errors and not overlooked
by mistake, as an activated error stops the code when it goes out of scope.
