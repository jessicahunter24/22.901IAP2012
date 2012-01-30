program transpose
implicit none

  integer           :: n        ! dimension of matrix
  real, allocatable :: M(:,:)   !matrix
  integer           :: i        !loop counter
  integer           :: j        !loop counter
  real              :: tmp      !temp var
  
  !read dimension from user
  write(*,*) 'Enter dimension of matrix'
  read(*,*) n
  
  !allocate matrix
  allocate(M(n,n))
  
  write(*,*)'Enter matrix in column major order'
  read(*,*) M

 !perform transpose
 cols: do j=1,n
   rows: do i=1,n
     !check for diagonals
     if (i==j) cycle
     !swap values
     tmp=M(j,i)
     M(j,i)=M(i,j)
     M(i,j)=tmp
   end do rows
 end do cols
 
 !Print out results to user
 do i=1,n
   write(*,'(100(F9.4))') (M(i,j),j=1,n)
 end do
 
 !deallocate matrix
 deallocate(M)
 
  stop
end program transpose