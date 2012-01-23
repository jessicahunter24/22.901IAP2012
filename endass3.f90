program endass3

implicit none
  real :: a,b,c ! Coefficients of quadratic formula
  real :: discrim !discriminant
  real :: x !double root
  real :: y !The real part of a complex answer
  real :: z !The imaginary part of a complex answer
  real :: first,second !!first and second roots when there are two distinct roots

  ! retrieve coefficients from user
  print *,'Enter coefficients a,b,and c'
  read *,a,b,c
  
  !Find discriminant
  discrim=b**2-(4.0*a*c)
  
  !If we have double roots
  If (discrim==0) then
    x=(-1.0*b)/(2.0*a)
    print *,'The answer is a double root with a value of',x,'.'
 
 ! If we have imaginary roots  
  Else if (discrim<0) then
    y=(-1.0*b)/(2.0*a)
    z=sqrt(-1.0*discrim)/(2.0*a)
    print *,'There are two complex roots. The first is',y,'-',z,'i and the second is',y,'+',z,'i.'
  
  ! Otherwise it has to be positive
  Else if (discrim>0) then
    first=((-1.0*b)+sqrt(discrim))/(2.0*a)
    second=((-1.0*b)-sqrt(discrim))/(2.0*a)
    print *,'There are two distinct roots. The first root is',first,' and the second root is',second,'.'  
  end if
  STOP

end program endass3