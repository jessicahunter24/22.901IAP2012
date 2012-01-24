program nonlinear

implicit none
 real :: xl   ! initial left bracket
 real :: xr   ! initial right bracket
 real :: x    ! answer
 real :: f    ! residual
 real :: tol  ! tolerance
 
 real :: myfun ! function we're using
 external myfun
  
  !ask user for brackets
  print *,'Enter left and then right bracket:'
  read *,xl,xr
  print *,'Enter solution tolerance:'
  read *, tol
  
  call bisect(xl,xr,myfun,x,f,tol)
  
  print *,'Solution is:',x,'with nonlinear residual:',f
  
  stop

end program nonlinear