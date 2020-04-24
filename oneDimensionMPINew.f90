Program DaxpyProgram

      implicit none
      include 'mpif.h'
      real,dimension(:),allocatable :: xTotal,xPart,yPart,yTotal
      integer i,j,alpha,n,npart,iter,ix,iy
      real :: start,finish,compare
      integer :: rank,procSize,ierror,shareSize
      logical :: verification

      call MPI_INIT(ierror)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,procSize,ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)

      !initializing the variables

      i = 0
      alpha = 4.0
      n = 2048
      shareSize = ((n)/procSize) + 1
      verification = .true.
      !allocating a size of n x n memory to matrix x and y

      if(rank.eq.0) then
      allocate(xTotal(n))
      allocate(yTotal(n))
      endif
      allocate(xPart(shareSize))
      allocate(yPart(shareSize))

      !do loop to initialize the x and y matrix

      if(rank .eq.0) then 
         do i = 1,n
            xTotal(i) = (10.2*i)
            yTotal(i) = 10.2
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
     
     !Verification
     if(rank.eq.0) then
          open(2,file = 'data1.dat',status = 'old')
          do i =1,n
             read(2,*) compare
             if((compare) .eq. (yTotal(i))) then
                verification = .True.
             else
                verification = .False.
                print *, 'The value at ',i,'which are ',compare,'and',yTotal(i),'dont match'
                exit
             endif
          enddo
     endif

     !print out the results
     if(rank .eq. 0) then
     print *,'Finished in time',(finish - start)
     print *,'This is the result: '
     print *,'The results compared to regular code is', verification
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
