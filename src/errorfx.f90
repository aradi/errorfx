!> Implements an exception like error handling mechanism in Fortran.
module errorfx
  implicit none

  private
  public :: fatal_error, create, init


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

    !> Whether error is active
    logical, private :: active = .false.

  contains

    !> Deactivtes the error
    procedure :: deactivate => fatal_error_deactivate

    !> Queries whether the error is still active
    procedure :: is_active => fatal_error_is_active

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
  pure subroutine fatal_error_final(this)

    !> Instance
    type(fatal_error), intent(inout) :: this

    character(:), allocatable :: errormsg

    if (this%active) then
      errormsg = "Stopping due to unhandled critical error: " // this%message
      error stop errormsg
    end if

  end subroutine fatal_error_final


  !> Deactivates the error (error would not stop the code if going out of scope)
  pure subroutine fatal_error_deactivate(this)
    class(fatal_error), intent(inout) :: this

    this%active = .false.

  end subroutine fatal_error_deactivate


  !> Queries, whether the error is still active or it had been already deactivated
  pure function fatal_error_is_active(this) result(is_active)
    class(fatal_error), intent(in) :: this
    logical :: is_active

    is_active = this%active

  end function fatal_error_is_active

end module errorfx