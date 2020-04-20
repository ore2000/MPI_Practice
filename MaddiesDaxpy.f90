Program DaxpyProgram
	  implicit none
	  include 'mpif.h'
	  real, dimension(:,:), allocatable :: x, y, z, xpart, ypart, zpart
	  integer alpha, n, i, ops, p, rank
	  real :: start, finish
	 
	  !initialize variables to test
	  i = 0
	  n = 10
	  alpha = 2
	
	  call MPI_INIT(error)
	  call MPI_COMM_SIZE(MPI_COMM_WORLD, p, error)
	  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, error)
	
	  print *, 'process', rank, 'running'
	
	  allocate (x(n))
          allocate(y(n))
          allocate(z(n))
	  call initialize (a, b, n)
	
	  if (MOD(n,p)/=0) then
	    ops = n/p + 1
	  else
	    ops = n/p
	  end if
	
	  if (id.eq.0) then
	    print *, 'operations: ', ops
	  end if
	
	  allocate(xpart(ops), ypart(ops), zpart(ops))
	
	
	call MPI_Scatter(x, ops, MPI_REAL, xpart, ops, MPI_REAL, 0, MPI_COMM_WORLD, error)
	call MPI_Scatter(y, ops, MPI_REAL, ypart, ops, MPI_REAL, 0, MPI_COMM_WORLD, error)
	call MPI_Scatter(z, ops, MPI_REAL, zpart, ops, MPI_REAL, 0, MPI_COMM_WORLD, error)
	
	print *,' process  ', id, 'section of vectors'
	
	call CPU_TIME(start)
	
	call MultAndAdd(alpha, xpart, ypart, zpart, ops, rank, n)
	call MPI_GATHER(zpart, ops, MPI_REAL, z, ops, MPI_REAL, 0, MPI_COMM_WORLD, error)
	
	if (id.eq.0) then
	  do i = 1,n
	    print *, z(i)
	  end do
	end if
	
	call CPU_TIME(finish)
	write (*,*) 'time of operation: ', finish-start, 's'
	
	deallocate(x)
        deallocate( y)
        deallocate( z)
        deallocate( xpart)
        deallocate( ypart)
        deallocate( zpart)
	
	print *, 'exiting on process: ', rank
	
	call MPI_Finalize(error)
end Program DaxpyProgram


