#:include "fxerror.fypp"

!
! Demonstrates error handling
!
program test_fxerror
  use fxerror, only : critical_error, os_error, num_error, init
  implicit none

  print *, "Testing error ignoring (would stop if an error had occured!)"
  #:block handle_error_type('os_error', 'error')
    call os_error_raiser(error, raise_error=.false.)
  #:endblock
  print *, "No error at this point"


  print *, "Testing simple error handling"
  #:block handle_error_type('os_error', 'error')
    call os_error_raiser(error, raise_error=.true.)
  #:contains
    print *, "OS Error found: ", error%as_char()
  #:endblock
  print *, "Error had been handled/deactivated successfully"


  print *, "Testing error propagation"
  #:block handle_error_type('os_error', 'error')
    call os_error_propagator(error, raise_error=.true.)
  #:contains
    print *, "OS Error found: ", error%as_char()
  #:endblock

  print *, "Testing cases, where multiple errors can be raised"
  #:block handle_error_class('critical_error', 'error')
    call arbitrary_error_raiser(error)
  #:contains os_error
    print *, "OS Error found: ", error%as_char()
  #:contains num_error
    print *, "NUM Error found: ", error%as_char()
  #:endblock


  print *, "Testing failure due to missing error-deactivation"
  #:block handle_error_type('os_error', 'error')
    call os_error_raiser(error, raise_error=.true.)
  #:endblock
  print *, "You should not see this message as error was not handled"


  ! NOTE: comment out the block above, if you want to test this way of failure
  print *, "Testing failure due to passing of unhandled error"
  #:block handle_error_type('os_error', 'error')
    call os_error_raiser(error, raise_error=.true.)
    call os_error_raiser(error, raise_error=.false.)
    print *, "You should not see this, as an unhandled error was passed to the 2nd routine"
  #:endblock


contains

  ! Raises an os_error, if asked so.
  subroutine os_error_raiser(oserror, raise_error)
    type(os_error), allocatable, intent(out) :: oserror
    logical, intent(in) :: raise_error

    if (raise_error) then
      ! Some recoverable error happened, raising error
      @:raise_error(oserror, "Some OS error occured")
    end if

  end subroutine os_error_raiser


  ! Calls error raiser and propagates the error without interacting
  subroutine os_error_propagator(oserror, raise_error)
    type(os_error), allocatable, intent(out) :: oserror
    logical, intent(in) :: raise_error

    call os_error_raiser(oserror, raise_error)
    @:propagate_error(oserror)

    ! Here we could do something and assume that no error was raised

  end subroutine os_error_propagator


  ! Raises a num_error, if asked so.
  subroutine num_error_raiser(numerror, raise_error)
    type(num_error), allocatable, intent(out) :: numerror
    logical, intent(in) :: raise_error

    if (raise_error) then
      @:raise_error(numerror, "Some NUM error occured")
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
