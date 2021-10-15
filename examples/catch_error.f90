module catch_module
  use errorfx, only : fatal_error, create, catch
  implicit none

contains

  ! A routine is called, which may throw an error (in this case it does). In case an error occured
  ! it will be caught/handled, so it won't do any harm when going out of scope.

  subroutine main()

    type(fatal_error), allocatable :: error

    ! Handling the error using error handling routine
    call routine1(error)
    call catch(error, handle_error)

    ! Handling the error with manual deactivation and deallocation
    call routine1(error)
    if (allocated(error)) then
      call error%deactivate()
      print "(a,a,a,i0,a)", "Fatal error found: '", error%message, "' (code: ", error%code, ")"
      deallocate(error)
    end if

  contains

    subroutine handle_error(error)
      type(fatal_error), intent(in) :: error

      ! Note if needed, you have access to all variables in the routine above
      print "(a,a,a,i0,a)", "Fatal error found: '", error%message, "' (code: ", error%code, ")"

    end subroutine handle_error

  end subroutine main


  subroutine routine1(error)
    type(fatal_error), allocatable, intent(out) :: error

    ! Creating and throwing an error
    call create(error, message="Error created in routine1")
    return
    print "(a)", "you shoud not see this, as we returned due to an error already"

  end subroutine routine1

end module catch_module


program catch_program
  use catch_module, only : main
  implicit none

  call main()

end program catch_program
