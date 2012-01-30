program mccircle

implicit none
  integer   :: rn=rand(23) !random number generator seed
  real(8)   :: npart       !number of particles run
  real(8)   :: x           !bounds of circle and square in x direction
  real(8)   :: y           !bounds of circle and square in y direction
  real(8)   :: xc          !random x coordinate of any given particle
  real(8)   :: yc          !random y coordinate of any given particle
  integer   :: i           !counter variable
  real(8)   :: nsucc       !number of successes (inside circle)
  real(8)   :: radius      !radius of a given particle trial
  real(8)   :: Ac          !Area of the circle
  real(8)   :: pi          !piiiiiii

  !read in number of particles
  write(*,*) 'Enter number of particles:'
  read(*,*) npart

  !read in radius of circle, which is also the x and y bound
  write(*,*) 'Enter radius of circle:'
  read(*,*) x
  
  !set the y bound as well
  y=x
  
  !set the initial number of successes to 0
  nsucc=0

  !Do monte carlo loop
  do i=1,npart
    xc=x*rand(0)
    yc=y*rand(0)
    ! calculate radius of new circle
    radius=sqrt(xc**2+yc**2)
    ! test if it is a success or not
    if (radius<x) then
      nsucc=nsucc+1
    end if
  end do
  
  !Calculate pi
  Ac=x**2*(nsucc/npart)
  write(*,*) 'The area of the circle is: ',Ac
  pi=4*Ac/(x**2)
  
  !print out to user
  write(*,*) 'The approximation of pi is:',pi
  
stop
end program mccircle