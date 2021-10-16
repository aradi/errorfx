module rethrow_error
  use errorfx, only : fatal_error, create, catch
  implicit none

contains

  ! A routine (throw_fatal_error) is called, which may throw an error. In case an error occured
  ! it is handled

  subroutine main()

    type(fatal_error), allocatable :: error
    print "(a)", "Calling routine1"
    call routine1(error)
    print "(a)", "Handling error returned by routine1"
    call catch(error, handle_error)

  contains

    subroutine handle_error(error)
      type(fatal_error), intent(in) :: error

      print "(a,a,a,i0,a)", "Fatal error found: '", error%message, "' (code: ", error%code, ")"

    end subroutine handle_error

  end subroutine main


  subroutine routine1(error)
    type(fatal_error), allocatable, intent(out) :: error

    call routine2(error)
    if (allocated(error)) then
      ! Pattern for reraising an error. The error%activate() call can be away, provided also
      ! the first error%deactivate() call is omitted. Otherwise you end up with a non-blocking
      ! error being propagated upwards.
      call error%deactivate()
      ! We inspect the error here
      ! We could even return, as error had been deactivated.
      print "(a,a,a,i0,a)", "Handling error: '", error%message, "' (code: ", error%code, ")"
      ! We can not handle the error, so we dedice to reraise it: we activate it again and return
      print "(a)", "Rethrowing the error"
      call error%activate()
      return
    end if
    print "(a)", "if you see this, routine2 returned without error"

  end subroutine routine1


  subroutine routine2(error)
    type(fatal_error), allocatable, intent(out) :: error

    call create(error, message="Routine2 experienced a fatal error")
    return
    print "(a)", "you shoud not see this, as we returned due to an error already"

  end subroutine routine2


end module rethrow_error


program rethrow_program
  use rethrow_error, only : main
  implicit none

  call main()

end program rethrow_program
