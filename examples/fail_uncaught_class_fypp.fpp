#:include "errorfx.fypp"

program fail_uncaught_class
  use errorfx, only : fatal_error, create
  use error_extension, only : io_error, linalg_error, create
  implicit none

  call main()

contains

  ! A routine is called, which may throw an error (in this case it does). In case an error occured
  ! it will be handled, but we "forgot" to add the correct class, which was actuall thrown
  ! by the routine. Consequently, it will stop the code, when going out of scope.

  subroutine main()

    ! Note, this is now defined as class, not as type to accomodate multiple error types
    class(fatal_error), allocatable :: error

    call routine1(error)
    #:block catch_error_class("error")
    #:contains io_error
        print "(2a)", "IO Error found: ", error%message
    #!#:contains linalg_error
    #!    print "(2a)", "Linear algebra error found: ", error%message
    #!    print "(a,i0)", "Additional info: ", error%info
    #:endblock

  end subroutine main


  ! A routine, which may emit different error-types. (The actual implementation will a linalg_error)

  subroutine routine1(error)
    class(fatal_error), allocatable, intent(out) :: error

    call routine2(error)
    @:propagate_error(error)
    print "(a)", "if you see this, routine2() did not throw any errors"

  end subroutine routine1


  subroutine routine2(error)
    class(fatal_error), allocatable, intent(out) :: error

    type(linalg_error), allocatable :: linalgerr

    @:throw_error_class(error, linalg_error, &
        & message="Matrix is not positive definite", code=-1, info=12)
    print "(a)", "you should not see this, as routine2 throwed an error and returned"

  end subroutine routine2

end program fail_uncaught_class
