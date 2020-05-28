Program DaxpyProgram

      implicit none
      include 'mpif.h'
      real,dimension(:),allocatable :: xTotal,yTotal
      real,dimension(:),allocatable :: xPart,yPart
      integer i,j,alpha,n,npart,iter,ix,iy
      real :: start,finish,compare
      integer :: rank,procSize,ierror,shareSize
      logical :: verification

      !initialize MPI
      call MPI_INIT(ierror)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,procSize,ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)

      !initializing the variables
      i = 0
      alpha = 4.0
      n = 204800
      shareSize = (n/procSize)
      verification = .true.

      !allocating a size of n x n memory to matrix x and y
      allocate(xTotal(n))
      allocate(yTotal(n))
      allocate(xPart(shareSize))
      allocate(yPart(shareSize))

     !do loop to initialize the x and y matrix
     do i = 1,n
        xTotal(i) = 10.2*i
        yTotal(i) = 5.2*i*i
     enddo

     !share parts of the array to be calculated by the ranks 
     xPart = xTotal((rank*shareSize) + 1 : ((rank+1)*shareSize))
     yPart = yTotal((rank*shareSize) + 1 : ((rank+1)*shareSize))

     !Start timing
     if(rank .eq. 0) then
     call cpu_time(start)
     endif

     !calculate the new values for y
     do iy = 1,shareSize
        if(shareSize*rank +iy <= n)then 
          yPart(iy) = alpha*xPart(iy) + yPart(iy)
        endif
     enddo

     !save all data calculated by the different processes to the Total Y variable
     yTotal((rank*shareSize) + 1 : ((rank+1)*shareSize)) = yPart

     !Wait to make sure all Processes have saved their data
     call MPI_Barrier(MPI_COMM_WORLD, ierror)

     !Stop timing.
     if(rank .eq. 0) then
     call cpu_time(finish)
     endif
     
     !Verification
     if(rank.eq.0) then
          open(2,file = 'data1.dat',status = 'old')
          open(3,file = 'results.log',status = 'new')
          do i =1,n
             read(2,*) compare
             if((compare) == (yTotal(i))) then
                verification = .True.
             else
                verification = .False.
                write(3,*) 'The value at ',i,'which are ',compare,'and',yTotal(i),'dont match'
                exit
             endif
          enddo
     endif

     close(2)
     
     !print out the results
     if(rank .eq. 0) then
     write(3,*) 'Finished in time',(finish - start)
     write(3,*) 'This is the result: '
     write(3,*) 'The results compared to regular code is', verification
       do j = 1,n
              write(3,*) 'Array(',j,')= ',yTotal(j)
       enddo
     close(3)
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
