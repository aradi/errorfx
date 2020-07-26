!
! Demonstrates error handling
!
program test_fxerror
  use fxerror, only : critical_error, os_error, num_error, init
  implicit none

  print *, "Testing error ignoring (would stop if an error had occured!)"
  block
    type(os_error), allocatable :: error
    call os_error_raiser(error, raise_error=.false.)
  end block
  print *, "No error at this point"


  print *, "Testing simple error handling"
  block
    type(os_error), allocatable :: error
    call os_error_raiser(error, raise_error=.true.)
    if (allocated(error)) then
      print *, "OS Error found: ", error%as_char()
      call error%deactivate()
    end if
  end block
  print *, "Error had been handled/deactivated successfully"


  print *, "Testing error propagation"
  block
    type(os_error), allocatable :: error
    call os_error_propagator(error, raise_error=.true.)
    if (allocated(error)) then
      print *, "OS Error found: ", error%as_char()
      call error%deactivate()
    end if
  end block


  print *, "Testing cases, where multiple errors can be raised"
  block
    class(critical_error), allocatable :: error
    call arbitrary_error_raiser(error)
    if (allocated(error)) then
      select type (error)
      class is (os_error)
        print *, "OS Error found: ", error%as_char()
      class is (num_error)
        print *, "NUM Error found: ", error%as_char()
      class default
        print *, "Some other error type was raised"
      end select
      call error%deactivate()
    end if
  end block


  block
    type(os_error), allocatable :: error
    call os_error_raiser(error, raise_error=.true.)
  end block


  ! NOTE: comment out the block above, if you want to test this way of failure
  print *, "Testing failure due to passing of unhandled error"
  block
    type(os_error), allocatable :: error
    call os_error_raiser(error, raise_error=.true.)
    call os_error_raiser(error, raise_error=.false.)
    print *, "You should not see this, as an unhandled error was passed into a routine"
  end block


contains

  ! Raises an os_error, if asked so.
  subroutine os_error_raiser(oserror, raise_error)
    type(os_error), allocatable, intent(out) :: oserror
    logical, intent(in) :: raise_error

    if (raise_error) then
      ! Some recoverable error happened, raising error
      allocate(oserror)
      call init(oserror, "Some OS error occured")
      return
    end if

  end subroutine os_error_raiser


  ! Calls error raiser and propagates the error without interacting
  subroutine os_error_propagator(oserror, raise_error)
    type(os_error), allocatable, intent(out) :: oserror
    logical, intent(in) :: raise_error

    call os_error_raiser(oserror, raise_error)
    if (allocated(oserror)) then
      return
    end if

    ! Here we could do something and assume that no error was raised

  end subroutine os_error_propagator


  ! Raises a num_error, if asked so.
  subroutine num_error_raiser(numerror, raise_error)
    type(num_error), allocatable, intent(out) :: numerror
    logical, intent(in) :: raise_error

    if (raise_error) then
      allocate(numerror)
      call init(numerror, "Some NUM error occured")
      return
    end if

  end subroutine num_error_raiser



  ! Returns an arbitrary error type (use select type to find out actual type)
  subroutine arbitrary_error_raiser(error)
    class(critical_error), allocatable, intent(out) :: error

    type(num_error), allocatable :: numerror
    call num_error_raiser(numerror, .true.)
    call move_alloc(numerror, error)

  end subroutine arbitrary_error_raiser


end program test_fxerror
