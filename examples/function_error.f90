module function_error
  use errorfx, only : fatal_error, create_error, destroy_error
  implicit none

  type :: int_result
    integer, allocatable :: value
    type(fatal_error), allocatable :: error
  end type int_result


contains

  subroutine main()

    type(fatal_error), allocatable :: error

    call subroutine_with_error(error)
    if (allocated(error)) then
      print "(2a)", "Main obtained an error: ", error%message
      call destroy_error(error)
    else
      print "(a)", "Subroutine returned without error to main"
    end if

  end subroutine main


  subroutine subroutine_with_error(error)
    type(fatal_error), allocatable, intent(out) :: error

    integer :: ii

    block
      type(int_result) :: res
      res = function_with_error(fail=.false.)
      print "(a,i0)", "Value in res: ", res%value
    end block

    block
      type(int_result) :: res
      res = function_with_error(fail=.true.)
      if (allocated(res%error)) then
        call move_alloc(res%error, error)
        return
      end if
      ii = res%value
    end block
    print "(a,i0)", "Value obtained in the subroutine (no error thrown): ", ii

  end subroutine subroutine_with_error


  function function_with_error(fail) result(res)
    logical, intent(in) :: fail
    type(int_result) :: res

    if (fail) then
      call create_error(res%error, message="Function throwed error")
      return
    else
      res%value = 42
    end if

  end function function_with_error

end module function_error


program function_error_program
  use function_error, only : main

  call main()

end program function_error_program