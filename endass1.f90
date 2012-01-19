program endass1

implicit none

  integer :: i ! reading in from screen
  integer :: j ! math
  integer :: k ! find the first digit trhough truncation
  integer :: l ! find the second digit
  integer :: m ! add the two
  integer :: n ! subtract 5, final answer
  
  ! request and retrieve number from reader
  print *,'Please enter any whole number 1-9'
  read *, i
  
  ! Math
  j=i*9
  k=j/10
  l=mod(j,10)
  m=k+l
  n=m-5
  
  print *, n
  
  stop



end program endass1