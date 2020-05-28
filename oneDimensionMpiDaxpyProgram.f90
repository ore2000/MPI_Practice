Program DaxpyProgram

      implicit none
      include 'mpif.h'
      real,dimension(:),allocatable :: xTotal,yTotal
      real,dimension(:),allocatable :: xPart,yPart
      integer i,j,alpha,n,npart,iter,ix,iy
      real :: start,finish,compare
      integer :: rank,procSize,ierror,shareSize,dest,source,tag
      logical :: verification

      call MPI_INIT(ierror)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,procSize,ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)

      !initializing the variables

      i = 0
      alpha = 4.0
      n = 2048
      shareSize = (n/procSize)
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
            yTotal(i) = 10.2*i*i
         enddo
      endif

      do i = 1,shareSize
        xPart(i) = (10.2*i)
        yPart(i) = 10.2
     enddo

      
     !Scatter array across all processes

     if(rank.eq.0) then
      do i = 1,(procSize-1)
           dest = i
           tag = i
           print *, 'About to start sending'
           xPart = xTotal(((dest)*shareSize) + 1 : ((dest+1)*shareSize))
           yPart = yTotal(((dest)*shareSize) + 1 : ((dest+1)*shareSize))
           print *, 'Finished sharing'
           call MPI_Isend(xPart,shareSize,MPI_FLOAT,dest,tag,MPI_COMM_WORLD,ierror)
           tag = tag+1
           print *, 'Sent x for process: ',dest
           call MPI_Isend(yPart,shareSize,MPI_FLOAT,dest,tag,MPI_COMM_WORLD,ierror)
           print *, 'Sent y for process: ',dest
      enddo
      print*, 'Just sent out the data'
     endif 


     if(rank.ne.0) then
        tag = rank
        call MPI_Recv(xPart,shareSize,MPI_FLOAT,0,tag,MPI_COMM_WORLD,ierror)
        tag = tag + 1
        call MPI_Recv(yPart,shareSize,MPI_FLOAT,0,tag,MPI_COMM_WORLD,ierror)
        print *, 'recieved data for process',rank
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
     
     if(rank .ne. 0) then
        tag = rank
        call MPI_SEND(xPart,shareSize,MPI_FLOAT,0,tag,MPI_COMM_WORLD,ierror)
        tag = tag + 1 
        call MPI_SEND(yPart,shareSize,MPI_FLOAT,0,tag,MPI_COMM_WORLD,ierror)
     endif

     if(rank .eq. 0) then
       do i =1,procSize
             source = i
             tag = i
             call MPI_Recv(xPart,shareSize,MPI_FLOAT,source,tag,MPI_COMM_WORLD,ierror)
             tag = tag +1
             call MPI_Recv(yPart,shareSize,MPI_FLOAT,source,tag,MPI_COMM_WORLD,ierror)
             yTotal((source*shareSize) + 1 : ((source+1)*shareSize)) = yPart
       enddo
     endif

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
             if((compare) .eq. (yTotal(i))) then
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
