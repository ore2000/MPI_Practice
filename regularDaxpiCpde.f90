Program DaxpyProgram


      implicit none
      real,dimension(:,:),allocatable :: x,y
      integer i,j,alpha,n
      real :: start,finish

      !initializing the variables i,alpha and n.
      i = 0
      alpha = 4.0
      n = 5
      
      !allocating a size of n x n memory to matrix x and y 
      allocate(x(n,n))
      allocate(y(n,n))

      !Start timing
      call cpu_time(start)

      !do loop to initialize the x and y matrix
      do i =1,n
         do j = 1,n
            x(i,j) = (10.2*i)
            y(i,j) = 10.2
         enddo
      enddo
     
     !editing refilling the y matrix with scalar multiples of the x matrix
         do i = 1,n
            do j =1,n
               y(i,j) = alpha*x(i,j) + y(i,j)
            enddo
         enddo
   
    !print out the results
    print *,'This is the result: '
    do i =1,n 
       do j = 1,n
          print *,'Matrix(',i,',',j,')= ',y(i,j)
       enddo
    enddo
    !deallocate the x and y memory from device
    deallocate(x)
    deallocate(y)
    !stop timing
    call cpu_time(finish)
    print *, 'Finished in time', (finish - start)
end Program DaxpyProgram
