#:include "exceptions.fypp"

program test_exceptions
  use exceptions, only : exception, os_error, num_error
  implicit none

  #:block handle_exception('exc')
    call exception_raiser(exc, .false.)
  #:endblock
  print *, "Exception was not active at this point"

  #:block handle_exception('exc')
    call exception_raiser(exc, .true.)
  #:contains os_error
    print *, "OS ERROR found: ", exc%msg
  #:contains num_error
    print *, "NUM ERROR found: " , exc%msg
  #:endblock
  print *, "Exception was not active at this point"

  #:block handle_exception('exc')
    call exception_propagator(exc, .true.)
  #:contains os_error
    print *, "OS ERROR found: ", exc%msg
  #:contains num_error
    print *, "NUM ERROR found: " , exc%msg
  #:endblock
  print *, "Exception was not active at this point"

  ! Now, we will not handle the generated exceptions
  #:block handle_exception('exc')
    call exception_propagator(exc, .true.)
  #:contains num_error
    print *, "NUM ERROR found: ", exc%msg
  #:endblock
  print *, "You should not see this message!"


contains

  subroutine exception_raiser(exc, raise_exc)
    class(exception), allocatable, intent(out) :: exc
    logical, intent(in) :: raise_exc

    if (raise_exc) then
      ! Some recoverable error happened, raising exception
      @:raise_exception(exc, os_error, "Some OS error occured")
    end if

  end subroutine exception_raiser


  subroutine exception_propagator(exc, raise_exc)
    class(exception), allocatable, intent(out) :: exc
    logical, intent(in) :: raise_exc

    #:block propagate_exception('exc')
       call exception_raiser(exc, raise_exc)
    #:endblock

   end subroutine exception_propagator

end program test_exceptions
