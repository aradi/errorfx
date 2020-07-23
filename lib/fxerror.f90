!
! Implements simple error handling mechanism.
!
module fxerror
  implicit none
  private

  public :: status, general_error, os_error, num_error

  ! Base class for all errors
  type :: general_error
    character(:), allocatable :: msg
  end type general_error

  interface general_error
    module procedure construct_general_error
  end interface general_error

  ! Specific error class
  type, extends(general_error) :: os_error
  end type os_error

  interface os_error
    module procedure construct_os_error
  end interface os_error

  ! Specific error class
  type, extends(general_error) :: num_error
  end type num_error

  interface num_error
    module procedure construct_num_error
  end interface num_error


  ! Contains the status of the program.
  !
  ! Note: If an instance contains an error, it would stop the code when going out of scope.
  !
  type :: status
    class(general_error), allocatable :: error
    logical, private :: has_error_ = .false.
  contains
    procedure :: set_error => status_set_error
    procedure :: clear => status_clear
    procedure :: has_error => status_has_error
    procedure :: is_clear => status_is_clear
    final :: status_final
  end type status


contains


  ! Sets an error.
  !
  ! Note: if not cleared explicitely, the instance will stop the code when going out of scope.
  !
  subroutine status_set_error(this, error)
    class(status), intent(inout) :: this
    class(general_error), intent(in) :: error

    this%has_error_ = .true.
    this%error = error

  end subroutine status_set_error


  ! Clears the status (if it contains an error, it will be canceled).
  subroutine status_clear(this)
    class(status), intent(inout) :: this

    this%has_error_ = .false.
    if (allocated(this%error)) then
      deallocate(this%error)
    end if

  end subroutine status_clear


  ! Checks, whether status contains an error
  function status_has_error(this) result(has_error)
    class(status), intent(in) :: this
    logical :: has_error

    has_error = this%has_error_

  end function status_has_error


  ! Checks, whether status is clear (no error raised)
  function status_is_clear(this) result(is_clear)
    class(status), intent(in) :: this
    logical :: is_clear

    is_clear = this%has_error_

  end function status_is_clear


  ! Finalizes status and stops the code, if it is not clear.
  subroutine status_final(this)
    type(status), intent(inout) :: this

    if (this%has_error_) then
      error stop "Unhandled error found"
    end if

  end subroutine status_final


  ! Constructs a general error instance.
  function construct_general_error(msg) result(this)
    character(*), intent(in) :: msg
    type(general_error) :: this

    this%msg = msg

  end function construct_general_error


  ! Constructs an os_error instance.
  function construct_os_error(msg) result(this)
    character(*), intent(in) :: msg
    type(os_error) :: this

    this%general_error = general_error(msg)

  end function construct_os_error


  ! Constructs a num_error instance.
  function construct_num_error(msg) result(this)
    character(*), intent(in) :: msg
    type(num_error) :: this

    this%general_error = general_error(msg)

  end function construct_num_error


end module fxerror
