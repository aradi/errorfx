#:include "fxerror.fypp"

!
! Demonstrates error handling with the help of Fypp constructs
!
! NOTE: Currently, Fypp must be used with Python 3.6 or above to ensure that the specific errors
! are handled in the right order. (Should be fixed with special options for Fypp later)
!
program test_fxerror_fypp
  use fxerror, only : status, os_error, num_error
  implicit none

  print *, "Testing error ignoring (would error stop if an error had occured!)"
  #:block handle_error
    call error_raiser(stat, raise_error=.false.)
  #:endblock
  print *, "No error at this point"


  print *, "Testing simple error handling"
  #:block handle_error
    call error_raiser(stat, raise_error=.true.)
  #:contains os_error
    print *, "OS Error found: ", error%msg
  #:contains num_error
    print *, "NUM Error found: ", error%msg
  #:endblock
  print *, "Error had been handled successfully"


  print *, "Testing error propagation"
  #:block handle_error
    call error_propagator(stat, raise_error=.true.)
  #:contains os_error
    print *, "OS Error found: ", error%msg
  #:contains num_error
    print *, "NUM Error found: ", error%msg
  #:endblock
  print *, "Error had been handled successfully"


  print *, "Testing failure due to missing error-clearing"
  #:block handle_error
    call error_propagator(stat, raise_error=.true.)
  #:contains num_error
    print *, "OS Error found", error%msg
  #:endblock
  print *, "You should not see this message as error was not correctly handled"


  ! NOTE: comment out the block above, if you want to test this way of failure
  print *, "Testing failure due to passing of unhandled error"
  #:block handle_error
    call error_raiser(stat, raise_error=.true.)
    call error_raiser(stat, raise_error=.false.)
  #:endblock
  print *, "You should not see this, as an unhandled error was passed into a routine"


contains

  ! Raises an error, if asked so.
  subroutine error_raiser(stat, raise_error)
    type(status), intent(out) :: stat
    logical, intent(in) :: raise_error

    if (raise_error) then
      @:raise_error(os_error("Some OS error occured"))
    end if

  end subroutine error_raiser


  ! Calls error raiser and propagates the status without interacting
  subroutine error_propagator(stat, raise_error)
    type(status), intent(out) :: stat
    logical, intent(in) :: raise_error

    call error_raiser(stat, raise_error)
    @:propagate_error()

    ! Here we could do something and assume that no error was raised

  end subroutine error_propagator


end program test_fxerror_fypp
