#:include "errorfx.fypp"

module rethrow_error_fypp
  use errorfx, only : fatal_error, create_error, catch_error
  implicit none

contains

  ! A routine (throw_fatal_error) is called, which may throw an error. In case an error occured
  ! it is handled

  subroutine main()

    type(fatal_error), allocatable :: error
    print "(a)", "Calling routine1"
    call routine1(error)
    print "(a)", "Handling error returned by routine1"
    #:block catch_error("error")
      print "(a,a,a,i0,a)", "Fatal error found: '", error%message, "' (code: ", error%code, ")"
    #:endblock

  end subroutine main


  subroutine routine1(error)
    type(fatal_error), allocatable, intent(out) :: error

    call routine2(error)
    #:block catch_error("error")
      ! Pattern for reraising an error.
      ! We inspect the error here
      print "(a,a,a,i0,a)", "Handling error: '", error%message, "' (code: ", error%code, ")"
      ! We can not handle the error, so we dedice to reraise it: we activate it again and return
      print "(a)", "Rethrowing the error"
      @:rethrow_error(error)
    #:endblock
    print "(a)", "if you see this, routine2 returned without error"

  end subroutine routine1


  subroutine routine2(error)
    type(fatal_error), allocatable, intent(out) :: error

    call create_error(error, message="Routine2 experienced a fatal error")
    return
    print "(a)", "you shoud not see this, as we returned due to an error already"

  end subroutine routine2


end module rethrow_error_fypp


program rethrow_error_fypp_program
  use rethrow_error_fypp, only : main
  implicit none

  call main()

end program rethrow_error_fypp_program
