program mxn

implicit none

  integer               :: m,n     !dimensions of matrix A
  integer               :: p,q     !dimensions of matrix B
  integer               :: f       !loop counter
  real, allocatable     :: A(:,:)  !matrix A
  real, allocatable     :: B(:,:)  !matrix B
  real, allocatable     :: C(:,:)  !matrix C, product matrix
  
  !Ask user for dimension of matrix A
  print *,'Enter the dimensions of matrix A'
  read *,m,n
  
  !Ask user for dimension of matrix B
  print *,'Enter the dimensions of matrix B'
  read *,p,q
  
  checkdimensions: if (n/=p) then
    print *,'Dimensions given do not allow for matrix multiplication.'
    stop
  end if checkdimensions
  
  !Allocate the matrices
  allocate(A(m,n))
  allocate(B(p,q))
  allocate(C(m,q))
  
  !Ask user to enter matrices A amd B
  print *,'Enter matrix A in column-major order.'
  read *,A
  print *,'Enter matrix B in column-major order.'
  read *,B
  

  !do matrix multiplication
  call matrixmulti(A,m,n,B,p,q,C)
  
  !print resulting matrix to reader
  print *,'Resulting matrix:'
  do f=1,m
    print *,C(f,:)
  end do

  ! deallocate memory
  deallocate(A)
  deallocate(B)
  deallocate(C)
  
  stop
end program mxn


subroutine matrixmulti(A,m,n,B,p,q,C)

implicit none
 integer   :: m,n,p,q               !dimensions
 real      :: A(m,n),B(p,q),C(m,q)  !matrices
 ! local variables
 integer   :: i,j,k                   !loop counters
 
 
 !nested do loops for multiplication
 do i=1,m
   do j=1,q
     do k=1,n
       C(i,j)=C(i,j)+A(i,k)*B(k,j)
     end do
   end do
 end do

end subroutine matrixmulti