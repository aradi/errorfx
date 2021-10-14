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
Fypp-based examples. If you want to build the project without the Fypp-based
examples, pass the ``-DWITH_FYPP=NO`` option to CMake.


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


Implementation details of ErrorFx
=================================

Error type
----------

The error type in ErroFx is ``type(fatal_error)``. When a subroutine with
possible critical error is called, the code in the caller would like ::

  type(fatal_error), allocatable :: error
  :
  call routine_with_possible_error(..., error)

while the routine possible raising the error would look as ::

  subroutine routine_with_possible_error(..., error)
    :
    type(fatal_error), allocatable, intent(out) :: error
    :

  end subroutine routine_with_possible_error


Throwing an error
-----------------

Throwing an error consists of allocation, filling information, activation and
returning as explained above. ErrorFx offers convenience function to
make this as simple as possible ::

  subroutine routine_with_possible_error(..., error)
    :
    type(fatal_error), allocatable, intent(out) :: error
    :
    ! Creating and throwing an error (activation included)
    call create(error, message="Error created in routine_with_possible_error")
    return

  end subroutine routine_with_possible_error

If you happen to use Fypp, you can further simplify the code, by writing ::

  subroutine routine_with_possible_error(..., error)
    :
    type(fatal_error), allocatable, intent(out) :: error
    :
    ! Creating and throwing an error (activation included)
    @:throw_error(error, message="Error created in routine_with_possible_error")

  end subroutine routine_with_possible_error


Propagating an error upwards
----------------------------

If a called routine signalizes an error, you can either handle it in the caller
or propagate it further upwards. The propagation happens by simply returning
if the error is allocated. Of course, the routine propagating the error
upwards must itself have a corresponding dummy argument::

  subroutine routine_propagating_error(..., error)
    :
    type(fatal_error), allocatable, intent(out) :: error
    :
    call routine_with_possible_error(..., error)
    ! If error happend, we propagate it upwards, otherwise we continue
    if (allocated(error)) return
    print "(a)", "Apparently no error occured"
    :
  end subroutine routine_propagating_error

Again, you can also use the Fypp-syntax to be more descriptive::

  subroutine routine_propagating_error(..., error)
    :
    type(fatal_error), allocatable, intent(out) :: error
    :
    call routine_with_possible_error(..., error)
    ! If error happend, we propagate it upwards, otherwise we continue
    @:propagate_error(error)
    print "(a)", "Apparently no error occured"
    :
  end subroutine routine_propagating_error


Catching an error
-----------------

If you do not want to propagate the error upwards, you have to handle it
locally, deactivate it (and eventually also deallocate it). The corresponding
catching pattern in ErrorFx would look as ::

    call routine_with_possible_error(..., error)
    if (allocated(error)) then
      call error%deactivate()
      ! Do whatever is needed to resolve the error
      print "(a,a,a,i0,a)", "Fatal error found: '", error%message, "' (code: ", error%code, ")"
      deallocate(error)
    end if

And of course, with Fypp you can write it a lot more compact and descriptive as ::

    call routine_with_possible_error(..., error)
    #:block catch_error("error")
      ! Do whatever is needed to resolve the error
      print "(a,a,a,i0,a)", "Fatal error found: '", error%message, "' (code: ", error%code, ")"
    #:endblock


Failure due to an uncaught error
--------------------------------

If an error is not caught (deactivated), it will trigger an ``error stop`` when
it goes out of scope. You will get an appropriate error message and given on
your compilation flags, you may also obtain some traceback information, where
the error was going out of scope ::

  subroutine routine_failing_due_unhandled_error()

    type(fatal_error), allocatable :: error

    call routine_with_possible_error(..., error)
    ! Error was neither caught nor propagated. It would trigger an error stop at
    ! the end of the subroutine

  end subroutine routine_failing_due_unhandled_error

Running the above example, you would obtain an error stop with some information ::

  Stopping due to unhandled critical error
  Error message: Error created in routine_with_possible_error
  Error code: 0
  ERROR STOP

  Error termination. Backtrace:
  #0  0x7f5a2fb30d21 in ???
  #1  0x7f5a2fb31869 in ???
  #2  0x7f5a2fb32f97 in ???
  #3  0x55e8d176876a in __errorfx_MOD_fatal_error_final
          at errorfx/src/errorfx.f90:125
  #4  0x55e8d1767abb in __errorfx_MOD___final_errorfx_Fatal_error
          at errorfx/src/errorfx.f90:196
  #5  0x55e8d176638a in main
          at errorfx/examples/fail_uncaught.f90:18

If you use Fypp, the error message will be much more informative, as it will
also contain the propagation path of the error, so you will know, where it
was triggered and how it was propagated up without going out of scope. Latter
can be very useful, if the error was propagated up several levels. ::

  Stopping due to unhandled critical error
  Error message: An error occured in routine1()
  Error code: 0
  Error propagation path:
  errorfx/examples/fail_uncaught_fypp.fpp:26
  ERROR STOP

  Error termination. Backtrace:
  #0  0x7fd723fe5d21 in ???
  #1  0x7fd723fe6869 in ???
  #2  0x7fd723fe7f97 in ???
  #3  0x559f121b279f in __errorfx_MOD_fatal_error_final
          at errorfx/src/errorfx.f90:125
  #4  0x559f121b1af0 in __errorfx_MOD___final_errorfx_Fatal_error
          at errorfx/src/errorfx.f90:196
  #5  0x559f121b03bf in main
          at errorfx/examples/fail_uncaught_fypp.fpp:20
