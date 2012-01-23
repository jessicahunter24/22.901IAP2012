program zeta

implicit none
  real :: x !input
  real ::zeta_var !output

  ! Ask user fo rinput
  print *, 'Enter x'
  read *, v

  ! Call subroutine zeta_sub()
  call zeta_sub(x,zeta_var)

  !print results
  print *,'For x=',x, 'zeta is', zeta_var
  
  ! terminate code
  STOP
end program zeta

!!!!!!!!!!!!

subroutine zeta_sub(x,zeta_var)

implicit none
  !formal variables
  real, intent(in) :: x         ! input
  real, intent(out):: zeta_var  ! output

  !local variables
  real,parameter   :: pi=3.1415926 !the constant pi
  
  !Compute zeta
  zeta_var=pi/x

end subroutine zeta_sub

!!!!!!!!!!

function zeta_fun(x)

implicit none
  ! function name
  real :: zeta_fun
  ! formal variable
  real :: x !input variable
  !local variable
  real :: pi=3.1415926 !the constant pi
  
  !math
  zeta_fun=pi/x


end function zeta_fun()