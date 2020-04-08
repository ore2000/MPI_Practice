Program DaxpyProgram
      implicit none
      include 'mpif.h'
      real,dimension(:,:),allocatable :: x,y
      integer i,j,alpha,n
      real :: start,finish
      integer :: rank,mpi_size,ierror,messageItem,scatter_Data,sc2

      !initializing the variables i,alpha and n.
      i = 0
      alpha = 4.0
      n = 50000
      
      !allocating a size of n x n memory to matrix x and y 
      allocate(x(n,n))
      allocate(y(n,n))

      call cpu_time(start)
      call MPI_INIT(ierror)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,mpi_size,ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)
      call MPI_Scatter(y,1,MPI_INT,scatter_Data,1,MPI_INT,0,MPI_COMM_WORLD,ierror)
      call MPI_Scatter(x,1,MPI_INT,sc2,1,MPI_INT,0,MPI_COMM_WORLD,ierror)
      !Start timing
      !call cpu_time(start)
      
      !do loop to initialize the x and y matrix
      do i =1,n
         do j = 1,n
            x(i,j) = (10.2*i)
            y(i,j) = 10.2
         enddo
      enddo
     !call cpu_time(start)
     !editing refilling the y matrix with scalar multiples of the x matrix
         do i = 1,n
            do j =1,n
               y(i,j) = alpha*x(i,j) + y(i,j)
            enddo
         enddo
   call cpu_time(finish)
    !print out the results
    if(rank == 0) then
    print *,'This is the result: '
    do i =1,n 
       do j = 1,n
         if( i .lt. 6 .and. j .lt. 6) then
          print *,'Matrix(',i,',',j,')= ',y(i,j)
       endif
       enddo
    enddo
    endif
    !deallocate the x and y memory from device
    deallocate(x)
    deallocate(y)
    !stop timing
    !call cpu_time(finish)
    if(rank  == 0) then
       print *,'Finished in time',(finish - start)
    endif
    call MPI_FINALIZE(ierror)
end Program DaxpyProgram
