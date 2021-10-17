#:include "errorfx.fypp"

module function_error_fypp
  use errorfx, only : fatal_error, create_error
  implicit none

  @:declare_result_type(name=int_result, value_type=integer, error_type=type(fatal_error))

contains

  subroutine main()

    type(fatal_error), allocatable :: error

    call subroutine_with_error(error)
    if (allocated(error)) then
      print "(2a)", "Main obtained an error: ", error%message
      call error%deactivate()
      deallocate(error)
    else
      print "(a)", "Subroutine returned without error to main"
    end if

  end subroutine main


  subroutine subroutine_with_error(error)
    type(fatal_error), allocatable, intent(out) :: error

    integer :: ii
    type(int_result), allocatable :: ires

    block
      type(int_result) :: res
      res = function_with_error(fail=.false.)
      print "(a,i0)", "Value in res: ", res%value
    end block

    @:try_assign(lhs=ii, rhs=function_with_error(fail=.true.),&
        & result_type=int_result, error=error)
    print "(a,i0)", "Value obtained in the subroutine (no error thrown): ", ii

    #! ires = function_with_error(fail=.true.)
    #! @:propagate_error(ires%error, target=error)
    #! ii = ires%value
    print "(a,i0)", "Value obtained in the subroutine (no error thrown): ", ii


  end subroutine subroutine_with_error


  function function_with_error(fail) result(res)
    logical, intent(in) :: fail
    type(int_result) :: res

    if (fail) then
      @:throw_error(res%error, message="Function throwed error")
    else
      res%value = 42
    end if

  end function function_with_error



end module function_error_fypp


program function_error_program
  use function_error_fypp, only : main

  call main()

end program function_error_program