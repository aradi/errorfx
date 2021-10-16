.. highlight:: fortran

*******
ErrorFx
*******

Fortran library providing exception like error handling using modern Fortran.
The error handling mechanism it implements is

* robust: an uncaught error stops the code automatically,

* convenient: propagation of errors upwards through the call stack is easily
  possible,

* pure: it only requires Fortran 2008 constructs.

It is written in Fortran 2008. If your project uses an appropriate preprocessor
(such as the `Fypp preprocessor <https://github.com/aradi/fypp>`_), preprocessor
macros can be used to make the error handling code even more compact.

Several examples demonstrating various error handling scenarios can be found in
the `<examples/>`_ folder. Files ending with ``*.f90`` contain the pure Fortran
examples, why files ending with `*_fypp.fpp` their simplified equivalent using
Fypp-constructs.

The project is released under the `BSD 2-clause license <LICENSE>`_.

Building
========

Use the usual CMake workflow to build the library and the examples::

  mkdir build
  cd build
  cmake ..
  make

The executables built from the examples can be found in the `./examples` folder
in the build directory.

By default, the Fypp-based examples are included when building the project. In
case you want to build without Fypp, pass the ``-DWITH_FYPP=NO`` option to CMake.


General concept
===============

The general recipe for the exception like error handling in Fortran is can be
formulated as follows:

* Define a derived type which allows to store the relevant information about
  your critical errors. Add an activation flag with ``.false.`` (deactivated)
  as default value. Add a finalizer, which calls ``error stop`` if it is called
  for an activated instance.

* Create an allocatable (but non-allocated) instance of the error type and
  pass it to the routine which may throw a fatal error. The
  corresponding dummy argument should have the ``allocatable, intent(out)``
  attributes. The allocation status of the variable on return will signalize the
  error status (non-allocated: no error, allocated: error).

* If an error occurs (within the called routine), allocate the
  error variable, fill it up with the necessary information, set its
  activation flag to ``.true.`` and return.

* In the calling routine, check the allocation status of the error variable
  immediately after the call. If it is allocated (error occured), either
  propagate it further upwards or handle it locally. If you handle it locally,
  deactivate the error (by setting its activation flag to ``.false.``), execute
  the handling code and then finally deallocate the error.

This mechanism provides a very flexible and robust error handling. Errors
can be easily propagated upwards to the top level (as it should be the case for
robust libraries). Additionally, it ensures, that errors are not overlooked
by mistake, as an activated error stops the code when it goes out of scope.


Implementation details
======================

Error type
----------

The error type in ErrorFx is ``type(fatal_error)`` [`<src/errorfx.f90>`_].
When a subroutine is called, which may throw a critical error, the code in the
caller would look as ::

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
returning as explained above. ErrorFx offers convenience functions to
make this as simple as possible, so that you only have to write
[`<examples/catch.f90>`_] ::

  subroutine routine_with_possible_error(..., error)
    :
    type(fatal_error), allocatable, intent(out) :: error
    :
    ! Creating and throwing an error (activation included)
    call create(error, message="Error created in routine_with_possible_error")
    return

  end subroutine routine_with_possible_error

If you happen to use Fypp, you can further simplify the code, by writing
[`<examples/catch_fypp.fpp>`_]::

  subroutine routine_with_possible_error(..., error)
    :
    type(fatal_error), allocatable, intent(out) :: error
    :
    ! Creating and throwing an error (activation included)
    @:throw(error, message="Error created in routine_with_possible_error")

  end subroutine routine_with_possible_error


Propagating an error upwards
----------------------------

If a called routine signalizes an error, you can either handle it in the caller
or propagate it further upwards. The propagation happens by simply returning
if the error is allocated. Of course, the routine propagating the error
upwards must itself have a corresponding error dummy argument
[`<examples/propagate_error.f90>`_]::

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

Again, you can use some Fypp magic to be more descriptive [`<examples/propagate_error_fypp.fpp>`_]::

  subroutine routine_propagating_error(..., error)
    :
    type(fatal_error), allocatable, intent(out) :: error
    :
    call routine_with_possible_error(..., error)
    ! If error happend, we propagate it upwards, otherwise we continue
    @:propagate(error)
    print "(a)", "Apparently no error occured"
    :
  end subroutine routine_propagating_error


Catching an error
-----------------

If you do not want to propagate the error upwards, you have to handle it
locally, deactivate it (and eventually also deallocate it). The corresponding
catching pattern in ErrorFx would look as [`<examples/catch.f90>`_] ::

    call routine_with_possible_error(..., error)
    if (allocated(error)) then
      call error%deactivate()
      ! Do whatever is needed to resolve the error
      print "(a,a,a,i0,a)", "Fatal error found: '", error%message, "' (code: ", error%code, ")"
      deallocate(error)
    end if

Doing the deactivation (``call error%deactivate()``) as the very first step
warranties, that the pattern works also in those cases, where you leave the
scope (e.g. via ``return``) during the error handling. If the error handling
code does not leave the scope, you can do the deactivation and deallocation
together at the end of the error handling block using the convenience
routine ``destroy()``::

    call routine_with_possible_error(..., error)
    if (allocated(error)) then
      ! Do whatever is needed to resolve the error
      ! Make sure you do not leave the scope, as the error is still active!
      print "(a,a,a,i0,a)", "Fatal error found: '", error%message, "' (code: ", error%code, ")"
      ! Deactivate and destroy in one step
      call destroy(error)
    end if

As the "manual" error handling is somewhat error prone (you may forget to
deactivate or deallocate), ErrorFx offers you the possibility to handle the error
via a dedicated (internal or external) subroutine. The library will first
deactivate the error, then call the error handling routine and finally
deallocate the error [`<examples/catch.f90>`_]::

  subroutine main()

    type(fatal_error), allocatable :: error

    call routine_with_possible_error(..., error)
    call catch(error, error_handler)
    :

  contains

    subroutine error_handler(error)
      type(fatal_error), intent(in) :: error

      ! Do whatever is needed to resolve the error
      ! (Deactivation/deallocation is done by the library automatically.)
      print "(a,a,a,i0,a)", "Fatal error found: '", error%message, "' (code: ", error%code, ")"

    end subroutine error_handler

  end subroutine main

The error handler routine can be an arbitrary subroutine, which takes the thrown
error type as ``intent(in)`` argument. If it is an internal subroutine, it will
even have access to all variables of the hosting scope (e.g. ``error_handler()``
can access all variables defined in ``main()`` above).

Of course, with Fypp you can write a compact, robust and descriptive error
catching construct even without explicit error handling routines as
[`<examples/catch_fypp.fpp>`_]::

    call routine_with_possible_error(..., error)
    #:block catch("error")
      ! Do whatever is needed to resolve the error
      print "(a,a,a,i0,a)", "Fatal error found: '", error%message, "' (code: ", error%code, ")"
    #:endblock


Rethrowing an error
-------------------

If during error handling of a caught error it turns out, that the error can not
be handled locally, the code may either throw (create and propagate) a new error
or just rethrow the original one. Latter can be achieved by activating the
error again (in case it was deactivated already) and returning::

  subroutine routine_rethrowing_error(error)
    type(fatal_error), allocatable, intent(out) :: error

    call routine_throwing_error(error)
    if (allocated(error)) then
      call error%deactivate()
      :
      ! Rethrowing error
      call error%activate()
      return
    end if
    :

Note, that if you do not leave the scope via return during the error handling
(except when rethrowing the error), the calls ``error%deactivate()`` and
``error%activate()`` can be omitted.

The compact Fypp based analog would be ::

  subroutine routine_rethrowing_error(error)
    type(fatal_error), allocatable, intent(out) :: error

    call routine_throwing_error(error)
    #:block catch("error")
      :
      ! Rethrowing error
      @:rethrow(error)
    #:endblock
    :


Failure due to an uncaught error
--------------------------------

If an error is not caught (deactivated), it will trigger an ``error stop`` when
it goes out of scope. You will get an appropriate error message and given on
your compilation flags, you may also obtain some traceback information starting
from the location where the error went out of scope
[`<examples/fail_uncaught.f90>`_]::

  subroutine routine_failing_due_unhandled_error()

    type(fatal_error), allocatable :: error

    call routine_with_possible_error(..., error)
    ! Error was neither caught nor propagated. It would trigger an error stop at
    ! the end of the subroutine

  end subroutine routine_failing_due_unhandled_error

Running the above example, you would obtain an error stop with some
information::

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

If you use Fypp for the same example [`<examples/fail_uncaught_fypp.fpp>`_],
the error message will be more informative, as it will also contain the
propagation path of the error itself, so you will know, where it was triggered
and how it was propagated up without going out of scope. Latter can be very
useful, if the error was propagated upwards through several levels::

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


Extending errors
----------------

Sometimes, it may be desirable to extend the error type. Either, because
you wish to create some errors which carry more information than the base type
does, or because you wish to differentiate between errors based on their class
(by creating an error class hierarchy as you find for example in Python).

The extension is straightforward. The following example demonstrates, how an I/O
error could be introduced, which also contains the filename and the unit
associated with the I/O problems. Appart of the type extension, one should also
provide convenience function to catch an error of the extended type and of the
extended class [`<examples/error_extension.f90>`_]::

  module error_extension
    use errorfx, only : fatal_error, init
    implicit none

    private
    public :: io_error, init, create
    public :: catch, catch_io_error_class

    !> Specific I/O error created by extending the general type
    type, extends(fatal_error) :: io_error
      integer :: unit = -1
      character(:), allocatable :: filename
    end type io_error

    !> Error initializer (use only in init routines of extending types)
    interface init
      module procedure io_error_init
    end interface init

    !> Error creator (use to create an error in the code)
    interface create
      module procedure io_error_create
    end interface create

    !> Catches specific error types
    interface catch
      module procedure catch_io_error
    end interface catch

  contains

    !> Creates an IO error.
    pure subroutine io_error_create(this, code, message, unit, filename)
      type(io_error), allocatable, intent(out) :: this
      integer, optional, intent(in) :: code
      character(*), optional, intent(in) :: message
      integer, optional, intent(in) :: unit
      character(*), optional, intent(in) :: filename

      allocate(this)
      call init(this, code=code, message=message, unit=unit, filename=filename)

    end subroutine io_error_create


    !> Initializes an io_error instance.
    pure subroutine io_error_init(this, code, message, unit, filename)
      type(io_error), intent(out) :: this
      integer, optional, intent(in) :: code
      character(*), optional, intent(in) :: message
      integer, optional, intent(in) :: unit
      character(*), optional, intent(in) :: filename

      call init(this%fatal_error, code=code, message=message)
      if (present(unit)) then
        this%unit = unit
      end if
      if (present(filename)) then
        this%filename = filename
      end if

    end subroutine io_error_init


    !> Catches an io_error and executes an error handler
    subroutine catch_io_error(error, errorhandler)
      type(io_error), allocatable, intent(inout) :: error
      interface
        subroutine errorhandler(error)
          import :: io_error
          type(io_error), intent(in) :: error
        end subroutine errorhandler
      end interface

      call error%deactivate()
      call errorhandler(error)
      deallocate(error)

    end subroutine catch_io_error


    !> Catches a generic error class and executes an error handler
    subroutine catch_io_error_class(error, errorhandler)
      class(fatal_error), allocatable, intent(inout) :: error
      interface
        subroutine errorhandler(error)
          import :: io_error
          class(io_error), intent(in) :: error
        end subroutine errorhandler
      end interface

      logical :: caught

      if (allocated(error)) then
        caught = .false.
        select type (error)
        class is (io_error)
          call error%deactivate()
          call errorhandler(error)
          caught = .true.
        end select
        if (caught) deallocate(error)
      end if

    end subroutine catch_io_error_class

  end module error_extension


Given different extensions of the base type, the patterns to generate and catch
the errors change slightly. One would typically use ``class(fatal_error)``
variables instead of ``type(fatal_error)``. Additionally the ``select type``
construct can be used to find out which actual error subclass was thrown.
Let's assume that two extending error types ``io_error`` and ``linalg_error``
had been created, a pattern, which can distinguish between the two would look
as [`<examples/catch_class.f90>`_]::

    class(fatal_error), allocatable :: error

    call routine_throwing_error(..., error)
    call catch_io_error_class(error, handle_io_error)
    call catch_linalg_error_class(error, handle_linalg_error)

  contains

    ! Handler for io error
    subroutine handle_io_error(error)
      class(io_error), intent(in) :: error

      print "(2a)", "IO Error found: ", error%message

    end subroutine handle_io_error


    ! Handler for linalg error
    subroutine handle_linalg_error(error)
      class(linalg_error), intent(in) :: error

      print "(2a)", "Linear algebra error found: ", error%message

    end subroutine handle_linalg_error


Alternatively, with manual deactivation and deallocation without explicit
error handler routines::

    class(fatal_error), allocatable :: error

    call routine_throwing_error(..., error)
    if (allocated(error)) then
      select type (error)
      class is (io_error)
        call error%deactivate()
        print "(2a)", "IO Error found: ", error%message
      class is (linalg_error)
        call error%deactivate()
        print "(2a)", "Linear algebra error found: ", error%message
      class default
        print "(a)", "Thrown error had not been handled by this block"
      end select
      if (.not. error%is_active()) deallocate(error)
    end if

Or in the more compact Fypp-form [`<examples/catch_class_fypp.fpp>`_]::

    class(fatal_error), allocatable :: error

    call routine_throwing_error(..., error)
    #:block catch_class("error")
    #:contains io_error
        print "(2a)", "IO Error found: ", error%message
    #:contains linalg_error
        print "(2a)", "Linear algebra error found: ", error%message
        print "(a,i0)", "Additional info: ", error%info
    #:endblock

When the error is created, it should be converted from the specialized type
to the generic type, easily accomplished with a ``move_alloc()`` statement::

  subroutine routine_throwing_error(error)
    class(fatal_error), allocatable, intent(out) :: error

    type(io_error), allocatable :: ioerr

    call create(ioerr, message="Failed to open file", filename="test.dat")
    call move_alloc(ioerr, error)
    return
    print "(a)", "you should not see this as an error was thrown before"

  end subroutine routine_throwing_error

When using Fypp, it reduces to ::

  subroutine routine_throwing_error(error)
    class(fatal_error), allocatable, intent(out) :: error

    type(io_error), allocatable :: ioerr

    @:throw_class(ioerr, io_error, message="Failed to open file", filename="test.dat")
    print "(a)", "you should not see this as an error was thrown before"

  end subroutine routine_throwing_error
