Program DaxpyProgram


      implicit none
      real,dimension(:),allocatable :: x,y
      integer i,j,alpha,n
      real :: start,finish

      !initializing the variables i,alpha and n.
      i = 0
      alpha = 4.0
      n = 204800
      
      !allocating a size of n x n memory to matrix x and y 
      allocate(x(n))
      allocate(y(n))

      !Start timing
      !call cpu_time(start)

      !do loop to initialize the x and y matrix
      do i =1,n
            x(i) = 10.2*i
            y(i) = 5.2*i*i
      enddo

     !Open a file to write all array results 
     open(1,file = 'data1.dat',status = 'new')

     !Start timing
     call cpu_time(start)

     !editing refilling the y matrix with scalar multiples of the x matrix
     do i = 1,n
        y(i) = alpha*x(i) + y(i)
     enddo
   
     !Stop timing
     call cpu_time(finish)

     !Write array results to file created.
     do i =1,n
        write(1,*) y(i)
     enddo
     close(1)

     !print out the results
     print *,'This is the result: '
     print *,'Finished in time',(finish-start)
     do i =1,n 
       if( i .lt. 10 ) then
          print *,'Array(',i,')= ',y(i)
       endif
     enddo
   
    !deallocate the x and y memory from device
    deallocate(x)
    deallocate(y)
 
end Program DaxpyProgram
