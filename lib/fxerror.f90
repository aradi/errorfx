!
! Implements a simple error handling mechanism.
!
module fxerror
  implicit none
  private

  public :: critical_error, os_error, num_error
  public :: init

  ! Base class for critical errors (stops code if unhandled error goes out of scope)
  type :: critical_error
    logical, private :: active_ = .false.
    character(:), allocatable, private :: msg_
  contains
    procedure :: as_char => critical_error_as_char
    procedure :: activate => critical_error_activate
    procedure :: deactivate => critical_error_deactivate
    procedure :: is_active => critical_error_is_active
    final :: critical_error_final
  end type critical_error


  ! Specific error class
  type, extends(critical_error) :: os_error
  end type os_error


  ! Specific error class
  type, extends(critical_error) :: num_error
  end type num_error


  ! Initializes an error
  interface init
    module procedure init_critical_error, init_os_error, init_num_error
  end interface init


contains

  pure subroutine init_critical_error(this, msg)
    type(critical_error), intent(out) :: this
    character(*), intent(in) :: msg

    this%msg_ = msg
    this%active_ = .true.

  end subroutine init_critical_error


  ! Finalizer for a critical error. Stops the code if the error is still active.
  pure subroutine critical_error_final(this)
    type(critical_error), intent(inout) :: this

    character(:), allocatable :: errormsg

    if (this%active_) then
      errormsg = "Stopping due to unhandled critical error: " // this%as_char()
      error stop errormsg
    end if

  end subroutine critical_error_final


  ! Returns the character representation (the error message) of an error.
  !
  ! Should be overriden in derived classes, if further information is available.
  !
  pure function critical_error_as_char(this) result(charrep)
    class(critical_error), intent(in) :: this
    character(:), allocatable :: charrep

    charrep = this%msg_

  end function critical_error_as_char


  ! Activates the error (active errors stop the code if going out of scope)
  pure subroutine critical_error_activate(this)
    class(critical_error), intent(inout) :: this

    this%active_ = .true.

  end subroutine critical_error_activate


  ! Deactivates the error (error would not stop the code if going out of scope)
  pure subroutine critical_error_deactivate(this)
    class(critical_error), intent(inout) :: this

    this%active_ = .false.

  end subroutine critical_error_deactivate


  ! Queries, whether the error is currently active.
  pure function critical_error_is_active(this) result(isactive)
    class(critical_error), intent(in) :: this
    logical :: isactive

    isactive = this%active_

  end function critical_error_is_active


  ! Constructs an os_error instance.
  pure subroutine init_os_error(this, msg)
    type(os_error), intent(out) :: this
    character(*), intent(in) :: msg

    call init(this%critical_error, msg)

  end subroutine init_os_error


  ! Constructs a num_error instance.
  subroutine init_num_error(this, msg)
    type(num_error), intent(out) :: this
    character(*), intent(in) :: msg

    call init(this%critical_error, msg)

  end subroutine init_num_error

end module fxerror
