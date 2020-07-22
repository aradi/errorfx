module exceptions
  implicit none
  private

  public :: exception, os_error, num_error

  type :: exception
    private
    character(:), allocatable, public :: msg
    logical :: active_ = .false.
  contains
    procedure :: raise => exception_raise
    procedure :: clear => exception_clear
    procedure :: is_active => exception_is_active
    final :: exception_final
  end type exception


  type, extends(exception) :: os_error
  end type os_error

  type, extends(exception) :: num_error
  end type num_error


contains

  subroutine exception_raise(this, msg)
    class(exception), intent(inout) :: this
    character(*), intent(in) :: msg

    this%msg = msg
    this%active_ = .true.

  end subroutine exception_raise


  subroutine exception_clear(this)
    class(exception), intent(inout) :: this

    this%active_= .false.
    deallocate(this%msg)

  end subroutine exception_clear


  function exception_is_active(this) result(is_active)
    class(exception), intent(in) :: this
    logical :: is_active

    is_active = this%active_

  end function exception_is_active


  subroutine exception_final(this)
    type(exception), intent(inout) :: this

    print *, "Finalizing exception", this%is_active()
    if (this%is_active()) then
      error stop "Exception was active during finalization"
    end if

  end subroutine exception_final


end module exceptions
