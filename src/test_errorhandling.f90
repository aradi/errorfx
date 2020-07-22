!
! Demonstrates error handling
!
program test_errorhandling
  use errorhandling, only : status, os_error, num_error
  implicit none

  print *, "Testing error ignoring (would error stop if an error had occured!)"
  block
    type(status) :: stat
    call error_raiser(stat, raise_error=.false.)
  end block
  print *, "No error at this point"


  print *, "Testing simple error handling"
  block
    type(status) :: stat
    call error_raiser(stat, raise_error=.true.)

    if (stat%has_error()) then
      select type (error => stat%error)
      class is (os_error)
        print *, "OS Error found: ", error%msg
        call stat%clear_error()
      class is (num_error)
        print *, "NUM Error found: ", error%msg
        call stat%clear_error()
      end select
    end if
  end block
  print *, "Error had been handled successfully"


  print *, "Testing error propagation"
  block
    type(status) :: stat
    call error_propagator(stat, raise_error=.true.)

    if (stat%has_error()) then
      select type (error => stat%error)
      class is (os_error)
        print *, "OS Error found: ", error%msg
        call stat%clear_error()
      class is (num_error)
        print *, "NUM Error found: ", error%msg
        call stat%clear_error()
      end select
    end if
  end block
  print *, "Error had been handled successfully"


  print *, "Testing failure due to missing error-clearing"
  block
    type(status) :: stat
    call error_propagator(stat, raise_error=.true.)

    if (stat%has_error()) then
      select type (error => stat%error)
      class is (num_error)
        print *, "OS Error found", error%msg
        call stat%clear_error()
      end select
    end if
  end block
  print *, "You should not see this message as error was not correctly handled"


  ! NOTE: comment out the block above, if you want to test this way of failure
  print *, "Testing failure due to passing of unhandled error"
  block
    type(status) :: stat
    call error_raiser(stat, raise_error=.true.)
    call error_raiser(stat, raise_error=.false.)
    print *, "You should not see this, as an unhandled error was passed into a routine"
  end block


contains

  ! Raises an error, if asked so.
  subroutine error_raiser(stat, raise_error)
    type(status), intent(out) :: stat
    logical, intent(in) :: raise_error

    if (raise_error) then
      ! Some recoverable error happened, raising error
      call stat%set_error(os_error("Some OS error occured"))
      return
    end if

  end subroutine error_raiser


  ! Calls error raiser and propagates the status without interacting
  subroutine error_propagator(stat, raise_error)
    type(status), intent(out) :: stat
    logical, intent(in) :: raise_error

    call error_raiser(stat, raise_error)
    if (stat%has_error()) then
      return
    end if

    ! Here we could do something and assume that no error was raised

  end subroutine error_propagator


end program test_errorhandling
