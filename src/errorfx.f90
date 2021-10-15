!> Implements an exception like error handling mechanism in Fortran.
module errorfx
  use iso_fortran_env, only : stderr => error_unit
  implicit none

  private
  public :: fatal_error, create, init, catch, catch_fatal_error


  type :: linked_location
    character(:), allocatable :: file
    integer :: line
    type(linked_location), allocatable :: previous
  contains
    procedure :: write_formatted => linked_location_write_formatted
  end type linked_location


  !> Base class for fatal errors (stops code if an unhandled error goes out of scope)
  !>
  !> You can use it directly (as type), in case you do not need further classification of the
  !> error type, or as a base class, if you wish to build class hierarchy of errors.
  !>
  type :: fatal_error

    !> Error code (Zero if not set explicitely)
    integer :: code = 0

    !> Error message (Unallocated, if not set explicitely)
    character(:), allocatable :: message

    !> Contains (if filled up) the path along which the error was propagated upwards
    type(linked_location), allocatable :: propagation_path

    !> Whether error is active
    logical, private :: active = .false.

  contains

    !> Deactivtes the error
    procedure :: deactivate => fatal_error_deactivate

    !> Queries whether the error is still active
    procedure :: is_active => fatal_error_is_active

    !> Adds information about the error propagation path
    procedure :: add_propagation_info => fatal_error_add_propagation_info

    !> Finalizer for the error. (You may consider to include it in debug-mode only)
    final :: fatal_error_final

  end type fatal_error


  !> Initializes an error
  !>
  !> Use it only in init routines of extending types, otherwise use create() to create errors.
  !>
  interface init
    module procedure fatal_error_init
  end interface init

  !> Allocates and initializes an error
  interface create
    module procedure fatal_error_create
  end interface create

  !> Catches a specific error type (not class) and exectues an error handling subroutine
  interface catch
    module procedure catch_fatal_error
  end interface catch


contains

  !> Allocates and initializes a fatal error.
  pure subroutine fatal_error_create(this, code, message)

    !> Instance.
    type(fatal_error), allocatable, intent(out) :: this

    !> Error code (0 if nothing had been explicitely set)
    integer, optional, intent(in) :: code

    !> Error message (empty string, if not set expicitely)
    character(*), optional, intent(in) :: message

    allocate(this)
    call fatal_error_init(this, code=code, message=message)

  end subroutine fatal_error_create


  !> Initializes a fatal_error
  pure subroutine fatal_error_init(this, code, message)

    !> Instance.
    type(fatal_error), intent(out) :: this

    !> Error code
    integer, optional, intent(in) :: code

    !> Error message
    character(*), optional, intent(in) :: message

    if (present(code)) this%code = code
    if (present(message)) this%message = message
    this%active = .true.

  end subroutine fatal_error_init


  !> Finalizer for a critical error. Stops the code if the error is still active.
  subroutine fatal_error_final(this)

    !> Instance
    type(fatal_error), intent(inout) :: this

    character(:), allocatable :: errormsg

    if (this%active) then
      write(stderr, "(a)") "Stopping due to unhandled critical error"
      if (allocated(this%message)) write(stderr, "(2a)") "Error message: ", this%message
      write(stderr, "(a, i0)") "Error code: ", this%code
      if (allocated(this%propagation_path)) then
        write(stderr, "(a)") "Error propagation path:"
        call this%propagation_path%write_formatted(stderr)
      end if
      error stop
    end if

  end subroutine fatal_error_final


  !> Deactivates the error (error would not stop the code if going out of scope)
  pure subroutine fatal_error_deactivate(this)

    !> Instance
    class(fatal_error), intent(inout) :: this

    this%active = .false.

  end subroutine fatal_error_deactivate


  !> Queries, whether the error is still active or it had been already deactivated
  pure function fatal_error_is_active(this) result(is_active)

    !> Instance.
    class(fatal_error), intent(in) :: this

    !> Whether error is still active
    logical :: is_active

    is_active = this%active

  end function fatal_error_is_active


  !> Adds propagation information to the error
  pure subroutine fatal_error_add_propagation_info(this, file, line)

    !> Instance
    class(fatal_error), intent(inout) :: this

    !> File where error occured
    character(*), intent(in) :: file

    !> Line where error occured
    integer, intent(in) :: line

    type(linked_location), allocatable :: curloc

    allocate(curloc)
    curloc%file = file
    curloc%line = line
    call move_alloc(this%propagation_path, curloc%previous)
    call move_alloc(curloc, this%propagation_path)

  end subroutine fatal_error_add_propagation_info


  !> Writes the chain of locations to a unit
  subroutine linked_location_write_formatted(this, unit)

    !> Instance
    class(linked_location), intent(in) :: this

    !> Unit to write the location chain
    integer, intent(in) :: unit

    if (allocated(this%previous)) then
      call this%previous%write_formatted(unit)
    end if
    write(unit, "(a, ':', i0)") this%file, this%line

  end subroutine linked_location_write_formatted


  !> Catches a fatal_error and executes an error handler
  subroutine catch_fatal_error(error, errorhandler)

    !> Error to catch
    type(fatal_error), allocatable, intent(inout) :: error

    interface

      !> Error handler routine
      subroutine errorhandler(error)
        import :: fatal_error
        implicit none

        !> Error which was caught
        type(fatal_error), intent(in) :: error

      end subroutine errorhandler

    end interface

    call error%deactivate()
    call errorhandler(error)
    deallocate(error)

  end subroutine catch_fatal_error


  !> Catches a generic error class and executes an error handler
  subroutine catch_fatal_error_class(error, errorhandler)

    !> Error to catch
    class(fatal_error), allocatable, intent(inout) :: error

    interface

      !> Error handler routine
      subroutine errorhandler(error)
        import :: fatal_error
        implicit none

        !> Error which was caught
        class(fatal_error), intent(in) :: error

      end subroutine errorhandler

    end interface

    logical :: caught

    caught = .false.
    if (allocated(error)) then
      select type (error)
      class is (fatal_error)
        call error%deactivate()
        call errorhandler(error)
        caught = .true.
      end select
      if (caught) deallocate(error)
    end if

  end subroutine catch_fatal_error_class


end module errorfx
