program ignore_unactivated
  use errorfx, only : fatal_error
  implicit none

  call main()

contains

  ! A routine (routine1) is called, which may throw an error. The error is not handled
  ! after the call (very bad practice!). However, since the error is not allocated/activated in the
  ! called routine, nothing happens when it goes out of scope

  subroutine main()

    type(fatal_error), allocatable :: error
    call routine1(error)

  end subroutine main


  subroutine routine1(error)
    type(fatal_error), allocatable, intent(out) :: error

    print "(a)", "Subroutine routine1() called, but it did not produced an error"

  end subroutine routine1

end program ignore_unactivated
