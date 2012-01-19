program endass2

implicit none
  
  real              :: a,b,c,gamma
  character(len=11) :: unitstr
  
  print *, "Enter the length of sides a and b"
  read *, a,b
  print *, "Enter the units of sides a and b"
  read *, unitstr
  print *, "Enter the angle between a and b (in degrees)"
  read *, gamma
  
  !convert degrees to radians
  
  
  
  
  print *,'The length of side c is:',c,' '//trim(unitstr)

  STOP

end program endass2