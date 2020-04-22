Program DaxpyProgram

      implicit none
      include 'mpif.h'
      real,dimension(:),allocatable :: xTotal,xPart,yPart,yTotal
      integer i,j,alpha,n,npart,iter,ix,iy
      real :: start,finish
      integer :: rank,procSize,ierror,shareSize

      call MPI_INIT(ierror)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,procSize,ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)

      !initializing the variables

      i = 0
      alpha = 4.0
      n = 1237
      shareSize = ((n)/procSize) + 1

      !allocating a size of n x n memory to matrix x and y

      if(rank.eq.0) then
      allocate(xTotal(n))
      allocate(yTotal(n))
      endif
      allocate(xPart(shareSize))
      allocate(yPart(shareSize))

      !do loop to initialize the x and y matrix

      if(rank .eq.0) then 
         do j = 1,n
            xTotal(j) = (10.2*j)
            yTotal(j) = 10.2
         enddo
      endif

      do i = 1,shareSize
        xPart(i) = (10.2*i)
        yPart(i) = 10.2
     enddo

      
     !Scatter array across all processes

     if(rank.eq.0) then
      call MPI_Scatter(yTotal,shareSize,MPI_REAL,yPart,shareSize,MPI_REAL,0,MPI_COMM_WORLD,ierror)
      call MPI_Scatter(xTotal,shareSize,MPI_REAL,xPart,shareSize,MPI_REAL,0,MPI_COMM_WORLD,ierror)
     endif 

     !Start timing
     if(rank .eq. 0) then
     call cpu_time(start)
     endif

     do iy = 1,shareSize
         if(shareSize*rank +iy <= n)then 
             yPart(iy) = alpha*xPart(iy) + yPart(iy)
         endif
     enddo
      
            
     call MPI_Gather(yPart,shareSize,MPI_REAL,yTotal,shareSize,MPI_REAL,0,MPI_COMM_WORLD,ierror)
      

     !Stop timing.
     if(rank .eq. 0) then
     call cpu_time(finish)
     endif

     !print out the results
     if(rank .eq. 0) then
     print *,'Finished in time',(finish - start)
     print *,'This is the result: '
       do j = 1,n
           if(j .lt. 10) then
              print *,'Array(',j,')= ',yTotal(j)
           endif
       enddo
     endif

     !deallocate the x and y memory from device
     if(rank .eq. 0) then
     deallocate(xTotal)
     deallocate(yTotal)
     endif
     deallocate(yPart)
     deallocate(xPart)
    
     call MPI_FINALIZE(ierror)
end Program DaxpyProgram
