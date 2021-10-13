#:include "errorfx.fypp"

program catch_error_fypp
  use errorfx, only : fatal_error, create
  implicit none

  call main()

contains

  ! A routine is called, which may throw an error (in this case it does). In case an error occured
  ! it will be caught/handled, so it won't do any harm when going out of scope.

  subroutine main()

    type(fatal_error), allocatable :: error

    call routine1(error)
    #:block catch_error("error")
      print "(a,a,a,i0,a)", "Fatal error found: '", error%message, "' (code: ", error%code, ")"
    #:endblock

  end subroutine main


  subroutine routine1(error)
    type(fatal_error), allocatable, intent(out) :: error

    ! Creating and throwing an error
    @:throw_error(error, message="Error created in routine1")
    print "(a)", "you shoud not see this, as we returned due to an error already"

  end subroutine routine1

end program catch_error_fypp
