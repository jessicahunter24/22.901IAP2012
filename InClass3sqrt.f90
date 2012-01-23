program squareroot

implicit none
  real :: x !Input
  real :: y !Output if greater than zero
  real :: z !Output if less than zero
  
  ! retrieve number from user
  print *,'Enter number'
  read *,x
 
  ! If X<0, negate, take square root and send it back to user
  if (x < 0.0) then
    !negate x
    x=-1.0*x
    z=sqrt(x)
    print *,'The square root is',z,'i'
  
  else
    y=sqrt(x)
    print *,'The square root is',y
    
  end if
  STOP
 
 end program squareroot