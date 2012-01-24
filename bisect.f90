subroutine bisect(xl,xr,fun,x,f,tol)

implicit none
  !formal variables
  real, intent(inout)   :: xl  !Left bracket (lower bround)
  real, intent(inout)   :: xr  !Right bracket (upper bound)
  real, intent(out)     :: x   ! solution
  real, intent(inout)   :: f   ! residual
  real, intent(in)      :: tol ! tolerance/limit
 
  ! external function, making it so that you can make the 
  ! function anything you want
  real :: fun
  external fun
  
  ! local variables
  integer :: i   ! loop variable
  real    :: fl  ! left bracket residual
  real    :: fr  ! right bracket residual
  

  
  ! begin loop
  do i = 1,10000
    ! compute guess value
    x = 0.5*(xl + xr)
    
    ! evaluate all the residuals
    fl=fun(xl)
    fr=fun(xr)
    f=fun(x)
    
    ! Check answer
    if (abs(f) < tol) exit
    
    ! Move brackets accordingly
    ! Move left bracket
    if (fl*f>0.0) then
      xl=x
    else
    ! Move right bracket
      xr=x
    end if
    ! print out progress
    print *,'Iteration:',i,' Solution:',x,' Residual:',f
  end do
  
end subroutine bisect