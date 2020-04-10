Program DaxpyProgram

      implicit none
      include 'mpif.h'
      real,dimension(:,:),allocatable :: xTotal,xPart,yPart,yTotal
      integer i,j,alpha,n,npart,iter,ix,iy
      real :: start,finish,scatter_Data,scatter_Data2,gather
      integer :: rank,procSize,ierror,messageItem,loops

      !initializing the variables i,alpha and n.
      i = 0
      alpha = 4.0
      n = 100
      npart = 10
      !allocating a size of n x n memory to matrix x and y 
      allocate(xTotal(n,n))
      allocate(yTotal(n,n))
      allocate(xPart(npart,npart))
      allocate(yPart(npart,npart))


      call MPI_INIT(ierror)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,procSize,ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)
      call MPI_Scatter(yPart,25,MPI_REAL,yTotal,25,MPI_REAL,0,MPI_COMM_WORLD,ierror)
      call MPI_Scatter(xPart,25,MPI_REAL,xTotal,25,MPI_REAL,0,MPI_COMM_WORLD,ierror)

      loops = ((n**2))/procSize
      !do loop to initialize the x and y matrix
      do i =1,n
         do j = 1,n
            x(i,j) = (10.2*i)
            y(i,j) = 10.2
         enddo
      enddo


      iter = ((rank+1)*5)
      do ix =((rank*5)+1),iter
         do iy = 1,iter
            y(ix,iy) = alpha*x(ix,iy) +y(ix,iy)
         enddo
      enddo
    

     call MPI_Gather(yPart,25,MPI_REAL,yTotal,25,MPI_REAL,0,MPI_COMM_WORLD,ierror)
     !Start timing
     call cpu_time(start)

     !Stop timing.
     call cpu_time(finish)

    !print out the results
    if(rank == 0) then
    print *,'This is the result: '
    do i =1,n 
       do j = 1,n
         if( i .lt. 6 .and. j .lt. 6) then
          print *,'Matrix(',i,',',j,')= ',y(i,j),' ', loops
       endif
       enddo
    enddo
    endif

    !deallocate the x and y memory from device
    deallocate(x)
    deallocate(y)
    
  
    if(rank  == 0) then
       print *,'Finished in time',(finish - start)
    endif
    call MPI_FINALIZE(ierror)
end Program DaxpyProgram
