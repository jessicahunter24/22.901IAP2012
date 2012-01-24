function myfun(x)

implicit none
  ! formal variables
  real :: x        !input
  real :: myfun    ! output
  
  ! evaluate function
  myfun=4.0-x**2
  
end function myfun