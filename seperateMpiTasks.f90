Program DaxpyProgram

      implicit none
      include 'mpif.h'
      real,dimension(:,:),allocatable :: xTotal,xPart,yPart,yTotal
      integer i,j,alpha,n,npart,iter,ix,iy
      real :: start,finish
      integer :: rank,procSize,ierror

      !initializing the variables i,alpha and n.
      i = 0
      alpha = 4.0
      n = 100
      npart = 5
      !allocating a size of n x n memory to matrix x and y 
      allocate(xTotal(n,n))
      allocate(yTotal(n,n))
      allocate(xPart(npart,npart))
      allocate(yPart(npart,npart))


      !do loop to initialize the x and y matrix
      do i =1,n
         do j = 1,n
            xTotal(i,j) = (10.2*i)
            yTotal(i,j) = 10.2
         enddo
      enddo


      call MPI_INIT(ierror)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,procSize,ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)
      call MPI_Scatter(yPart,25,MPI_REAL,yTotal,25,MPI_REAL,0,MPI_COMM_WORLD,ierror)
      call MPI_Scatter(xPart,25,MPI_REAL,xTotal,25,MPI_REAL,0,MPI_COMM_WORLD,ierror)
      !Start timing
      call cpu_time(start)

      iter = ((rank+1)*5)
      do ix =((rank*5)+1),iter
        do iy = ((rank*5)+1),iter
            yPart(ix,iy) = alpha*xPart(ix,iy) +yPart(ix,iy)
        enddo
      enddo

     call MPI_Gather(yPart,25,MPI_REAL,yTotal,25,MPI_REAL,0,MPI_COMM_WORLD,ierror)
    
     !Stop timing.
     call cpu_time(finish)

    call MPI_FINALIZE(ierror)

    !print out the results
    print *,'Finished in time',(finish - start)

    print *,'This is the result: '
    do i =1,n 
       do j = 1,n
         if( i .lt. 6 .and. j .lt. 6) then
           print *,'Matrix(',i,',',j,')= ',yTotal(i,j)
         endif
       enddo
    enddo

    !deallocate the x and y memory from device
    deallocate(xTotal)
    deallocate(yTotal)
    deallocate(yPart)
    deallocate(xPart)
    
end Program DaxpyProgram
