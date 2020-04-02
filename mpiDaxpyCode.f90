Program DaxpyProgram

      implicit none
      include 'mpif.h'
      real,dimension(:,:),allocatable :: x,y
      integer i,j
      real :: start,finish
      integer :: ierror
      interface
         SUBROUTINE Daxpy(x,y,ierror)

             real,dimension(:,:),intent(in) :: x
             real,dimension(:,:), intent(out):: y
             integer, intent(out):: ierror
        end SUBROUTINE
     end interface

     call cpu_time(start)
     allocate(x(3,3))
     allocate(y(3,3))

     do i =1,3
        do j = 1,3
           x(i,j) = (10.2*i)
           y(i,j) = 10.2
        enddo
     enddo

     call Daxpy(x, y,ierror)
     print *,'This is the result: '
 
     do i =1,3 
        do j = 1,3
           print *,'Matrix(',i,',',j,')= ',y(i,j)
        enddo
     enddo

     deallocate(x)
     deallocate(y)

     call cpu_time(finish)
     print *, 'Finished in time', (finish - start)
     call MPI_FINALIZE(ierror)
end Program DaxpyProgram


 SUBROUTINE Daxpy(x,y,ierror)
      implicit none
      include 'mpif.h'
      real,dimension(:,:),intent(in) :: x
      real,dimension(:,:), intent(out):: y
      integer, intent(out) :: ierror
      !local variables
      integer :: n
      real :: alpha,ix,iy
      integer :: incx,incy,i,j
      integer :: rank,mpi_size,message_Item,scattered_Data
      

      n = 3
      alpha = 4.0
      incx = 1
      incy = 1
      i = 0  
      call MPI_INIT(ierror)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,mpi_size,ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)
      call MPI_Scatter(y,1,MPI_INT,scattered_Data,1,MPI_INT,0,MPI_COMM_WORLD,ierror)

      if((incx.eq.1) .and.(incy.eq.1)) then
        do i = 1,n
           do j =1,n
              y(i,j) = alpha*x(i,j) + y(i,j)
           enddo
       enddo
       end if
       return
       
end SUBROUTINE



