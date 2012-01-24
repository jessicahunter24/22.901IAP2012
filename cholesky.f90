program cholesky

implicit none
  integer             :: n      !extent of matrix
  integer             :: i      !loop counter
  real,allocatable    :: A(:,:) !matrix

  !ask user for dimensions
  print *,'Enter dimension:'
  read *,n
  
  !Allocate
  allocate(A(n,n))

  !read in matrix
  print *,'Enter matrix:'
  read *,A
  
  !perform decomposition
  call cholesky_sub(A,n)
  
  !print result
  do i=1,n
    print *,A(n,:)
  end do
  
  
  !free up memory
  deallocate(A)
  
  stop
end program cholesky



subroutine cholesky_sub(A,n)

implicit none
  ! formal variables
  integer :: n      !extent of dimension
  real    :: A(n,n) !matrix
  ! local variables
  integer ::j       !loop counter
  
  ! begin loop
  do j=1,n
  
    A(j,j)=sqrt(A(j,j)-dot_product(A(j,1:j,1),A(j-1:j-1)))
    
    ! do off diagonal
    if (j<n) then
      A(j+1,j)=(A(j+1:n,j)-matmul(A(j+1:n,1:j-1),A(j,1:j-1)))/A(j,j)
    end if
  end do
  
end subroutine cholesky_sub