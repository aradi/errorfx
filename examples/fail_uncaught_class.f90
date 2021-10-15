module fail_uncaught_class
  use errorfx, only : fatal_error, create
  use error_extension, only : io_error, linalg_error, create, catch_io_error_class,&
      & catch_linalg_error_class
  implicit none

contains

  ! A routine is called, which may throw an error (in this case it does). In case an error occured
  ! it will be handled, but we "forgot" to add the correct class, which was actuall thrown
  ! by the routine. Consequently, it will stop the code, when going out of scope.

  subroutine main()

    ! Note, this is now defined as class, not as type to accomodate multiple error types
    class(fatal_error), allocatable :: error

    call routine1(error)
    call catch_io_error_class(error, handle_io_error)
    !call catch_linalg_error_class(error, handle_linalg_error)

  contains

    ! Handler for io error
    subroutine handle_io_error(error)
      class(io_error), intent(in) :: error

      print "(2a)", "IO Error found: ", error%message

    end subroutine handle_io_error


    ! Handler for linalg error
    subroutine handle_linalg_error(error)
      class(linalg_error), intent(in) :: error

      print "(2a)", "Linear algebra error found: ", error%message

    end subroutine handle_linalg_error


  end subroutine main


  ! A routine, which may emit different error-types. (The actual implementation will a linalg_error)

  subroutine routine1(error)
    class(fatal_error), allocatable, intent(out) :: error

    call routine2(error)
    ! Wrapping the specific error type routine2() returned into the general one and propagating it
    if (allocated(error)) return
    print "(a)", "if you see this, routine2() did not throw any errors"

  end subroutine routine1


  subroutine routine2(error)
    class(fatal_error), allocatable, intent(out) :: error

    type(linalg_error), allocatable :: linalgerr

    call create(linalgerr, message="Matrix is not positive definite", code=-1, info=12)
    ! Putting specific error type into generic class
    call move_alloc(linalgerr, error)
    return
    print "(a)", "you should not see this, as routine2 throwed an error and returned"

  end subroutine routine2

end module fail_uncaught_class


program fail_uncaught_class_program
  use fail_uncaught_class, only : main
  implicit none

  call main()

end program fail_uncaught_class_program
