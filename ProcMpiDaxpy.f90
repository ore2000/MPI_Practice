Program DaxpyProgram

      implicit none
      include 'mpif.h'
      real,dimension(:,:),allocatable :: xTotal,xPart,yPart,yTotal
      integer i,j,alpha,n,npart,iter,ix,iy
      real :: start,finish,shareSize
      integer :: rank,procSize,ierror

      call MPI_INIT(ierror)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,procSize,ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)

      !initializing the variables

      i = 0
      alpha = 4.0
      n = 10
      shareSize = (n**2)/procSize
      npart = sqrt(shareSize)

       !allocating a size of n x n memory to matrix x and y

      if(rank.eq.0) then
      allocate(xTotal(n,n))
      allocate(yTotal(n,n))
      endif
      allocate(xPart(npart,npart))
      allocate(yPart(npart,npart))

      !do loop to initialize the x and y matrix

      if(rank .eq.0) then
      do i =1,n
         do j = 1,n
            xTotal(i,j) = (10.2*i)
            yTotal(i,j) = 10.2
         enddo
      enddo
      endif

      !Scatter array across all processes

      call MPI_Scatter(yTotal,shareSize,MPI_REAL,yPart,shareSize,MPI_REAL,0,MPI_COMM_WORLD,ierror)
      call MPI_Scatter(xTotal,shareSize,MPI_REAL,xPart,shareSize,MPI_REAL,0,MPI_COMM_WORLD,ierror)


      !Start timing
      if(rank .eq. 0) then
      call cpu_time(start)
      endif

      !iter = ((rank+1)*npart)
      !do ix =((rank*npart)+1),iter
        !do iy = ((rank*npart)+1),iter
            !yPart(ix,iy) = alpha*xPart(ix,iy) +yPart(ix,iy)
        !enddo
      !enddo

      do ix = 1,npart
         do iy = 1,npart
             yPart(ix,iy) = alpha*xPart(ix,iy) + yPart(ix,iy)
         enddo
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
     do i =1,n 
       do j = 1,n
         if( i .lt. 6 .and. j .lt. 6) then
           print *,'Matrix(',i,',',j,')= ',yTotal(i,j)
         endif
       enddo
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
