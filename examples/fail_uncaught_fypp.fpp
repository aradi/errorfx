#:include "errorfx.fypp"

program fail_uncaught
  use errorfx, only : fatal_error, create_error
  implicit none

  call main()

contains

  ! A routine is called, which may throw an error (in this case it does). Since the error is not
  ! handled, it stops the program, when it goes out of scope.

  subroutine main()

    type(fatal_error), allocatable :: error

    call routine1(error)

  end subroutine main


  subroutine routine1(error)
    type(fatal_error), allocatable, intent(out) :: error

    @:throw_error(error, message="An error occured in routine1()")

  end subroutine routine1


end program fail_uncaught
