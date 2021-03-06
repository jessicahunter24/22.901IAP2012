program endass2

implicit none
  
  real              :: a,b,c,gamma,pi,rad
  character(len=11) :: unitstr
  
  print *, "Enter the length of sides a and b"
  read *, a,b
  print *, "Enter the units of sides a and b"
  read *, unitstr
  print *, "Enter the angle between a and b (in degrees)"
  read *, gamma
  
  !convert degrees to radians
  pi=4.0*atan(1.0)
  rad=gamma*(pi/180.0)
  
  !Do law of cosines
  c=sqrt(a**2+b**2-(2.0*a*b*cos(rad)))
  
  !print out answer to reader
  print *,'The length of side c is:',c,''//trim(unitstr)

  STOP

end program endass2