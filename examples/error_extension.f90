!> Extension of the original error type for creating more specific error types
module error_extension
  use errorfx, only : fatal_error, fatal_error_init
  implicit none

  private
  public :: io_error, io_error_init, catch_io_error_class
  public :: linalg_error, linalg_error_init, catch_linalg_error_class
  public :: create_error, catch_error, destroy_error


  !> Specific I/O error created by extending the general type
  type, extends(fatal_error) :: io_error

    !> Unit number (-1 if no unit is associated with the I/O error)
    integer :: unit = -1

    !> File name (unallocated if no file is associted with the I/O error)
    character(:), allocatable :: filename

  end type io_error


  !> Specific linear algebra error created by extending the general type
  type, extends(fatal_error) :: linalg_error

    !> Additional info flag, typically the info flag returned by the underlying library.
    integer :: info = 0

  end type linalg_error


  !> Error creator (use those routines to create an error in the code)
  interface create_error
    module procedure create_io_error, create_linalg_error
  end interface create_error

  !> Catches specific error types
  interface catch_error
    module procedure catch_io_error, catch_linalg_error
  end interface catch_error

  !> Deactivates and deallocates a specific error type
  interface destroy_error
    module procedure destroy_io_error, destroy_linalg_error
  end interface destroy_error


contains

  !> Creates an IO error.
  pure subroutine create_io_error(this, code, message, unit, filename)

    !> Instance.
    type(io_error), allocatable, intent(out) :: this

    !> Error code
    integer, optional, intent(in) :: code

    !> Error message
    character(*), optional, intent(in) :: message

    !> Unit number the io error is associated with
    integer, optional, intent(in) :: unit

    !> File name the io error is associated with
    character(*), optional, intent(in) :: filename

    allocate(this)
    call io_error_init(this, code=code, message=message, unit=unit, filename=filename)

  end subroutine create_io_error


  !> Initializes an io_error instance.
  pure subroutine io_error_init(this, code, message, unit, filename)

    !> Instance.
    type(io_error), intent(out) :: this

    !> Error code
    integer, optional, intent(in) :: code

    !> Error message
    character(*), optional, intent(in) :: message

    !> Unit number the io error is associated with
    integer, optional, intent(in) :: unit

    !> File name the io error is associated with
    character(*), optional, intent(in) :: filename

    call fatal_error_init(this%fatal_error, code=code, message=message)
    if (present(unit)) then
      this%unit = unit
    end if
    if (present(filename)) then
      this%filename = filename
    end if

  end subroutine io_error_init


  !> Destroys an error explicitely (after deactivating it)
  subroutine destroy_io_error(this)

    !> Existing instance, unallocated on exit
    type(io_error), allocatable, intent(inout) :: this

    if (allocated(this)) then
      call this%deactivate()
      deallocate(this)
    end if

  end subroutine destroy_io_error


  !> Catches an io_error and executes an error handler
  subroutine catch_io_error(error, errorhandler)

    !> Error to catch
    type(io_error), allocatable, intent(inout) :: error

    interface

      !> Error handler routine
      subroutine errorhandler(error)
        import :: io_error
        implicit none

        !> Error which was caught
        type(io_error), intent(in) :: error

      end subroutine errorhandler

    end interface

    call error%deactivate()
    call errorhandler(error)
    deallocate(error)

  end subroutine catch_io_error


  !> Catches a generic error class and executes an error handler
  subroutine catch_io_error_class(error, errorhandler)

    !> Error to catch
    class(fatal_error), allocatable, intent(inout) :: error

    interface

      !> Error handler routine
      subroutine errorhandler(error)
        import :: io_error
        implicit none

        !> Error which was caught
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


  !> Creates a linear algebra error
  pure subroutine create_linalg_error(this, code, message, info)

    !> Instance.
    type(linalg_error), allocatable, intent(out) :: this

    !> Error code
    integer, optional, intent(in) :: code

    !> Error message
    character(*), optional, intent(in) :: message

    !> Info flag (e.g. info flag returned by BLAS/LAPACK)
    integer, optional, intent(in) :: info

    allocate(this)
    call linalg_error_init(this, code=code, message=message, info=info)

  end subroutine create_linalg_error


  !> Initializes an linalg_error instance.
  pure subroutine linalg_error_init(this, code, message, info)

    !> Instance
    type(linalg_error), intent(out) :: this

    !> Error code
    integer, optional, intent(in) :: code

    !> Error message
    character(*), optional, intent(in) :: message

    !> Info flag (e.g. info flag returned by BLAS/LAPACK)
    integer, optional, intent(in) :: info

    call fatal_error_init(this%fatal_error, code=code, message=message)
    if (present(info)) then
      this%info = info
    end if

  end subroutine linalg_error_init


  !> Destroys an error explicitely (after deactivating it)
  subroutine destroy_linalg_error(this)

    !> Existing instance, unallocated on exit
    type(linalg_error), allocatable, intent(inout) :: this

    if (allocated(this)) then
      call this%deactivate()
      deallocate(this)
    end if

  end subroutine destroy_linalg_error


  !> Catches an linalg_error and executes an error handler
  subroutine catch_linalg_error(error, errorhandler)

    !> Error to catch
    type(linalg_error), allocatable, intent(inout) :: error

    interface

      !> Error handler routine
      subroutine errorhandler(error)
        import :: linalg_error
        implicit none

        !> Error which was caught
        type(linalg_error), intent(in) :: error

      end subroutine errorhandler

    end interface

    call error%deactivate()
    call errorhandler(error)
    deallocate(error)

  end subroutine catch_linalg_error


  !> Catches a generic error class and executes an error handler
  subroutine catch_linalg_error_class(error, errorhandler)

    !> Error to catch
    class(fatal_error), allocatable, intent(inout) :: error

    interface

      !> Error handler routine
      subroutine errorhandler(error)
        import :: linalg_error
        implicit none

        !> Error which was caught
        class(linalg_error), intent(in) :: error

      end subroutine errorhandler

    end interface

    logical :: caught

    if (allocated(error)) then
      caught = .false.
      select type (error)
      class is (linalg_error)
        call error%deactivate()
        call errorhandler(error)
        caught = .true.
      end select
      if (caught) deallocate(error)
    end if

  end subroutine catch_linalg_error_class

end module error_extension
