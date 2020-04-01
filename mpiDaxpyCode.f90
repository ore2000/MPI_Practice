Program DaxpyProgram
      implicit none      
      !include 'mpif.h'


      real,dimension(:,:),allocatable :: x,y
      integer i,j
      real :: startTime,finishTime
      !MPI variables
      integer :: process_Rank,size_of_cluster,ierror,mpi_size,rank

      interface
         SUBROUTINE Daxpy(x,y)

            real,dimension(:,:),intent(in) :: x
            real,dimension(:,:), intent(out):: y

         end SUBROUTINE
      end interface

      !Timer
      call cpu_time(startTime)
      !MPI optimization code
      !call MPI_INIT(ierror)
      !call MPI_COMM_SIZE(MPI_COMM_WORLD,mpi_size,ierror)
      !call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)

      !allocating memory to the the previously undefined matrix
      allocate(x(10,10))
      allocate(y(10,10))
      
      !initializing the matrix x and y with dummy values
      do i =1,10
         do j = 1,10
             x(i,j) = (10.2*i)
             y(i,j) = 10.2
         enddo
     enddo


     call Daxpy(x, y)
     print *,'This is the result: '
 
     do i =1,10 
        do j = 1,10
           print *, 'Matrix(',i,',',j,')= ',y(i,j)
        enddo
     enddo

     deallocate(x)
     deallocate(y)


     !call MPI_FINALIZE(ierror)
     call cpu_time(finishTime)
     print *,'Finished in time: ',(finishTime - startTime)

end Program DaxpyProgram




SUBROUTINE Daxpy(x,y)
      implicit none
      include 'mpif.h'
      real,dimension(:,:),intent(in) :: x
      real,dimension(:,:), intent(out):: y
       integer :: process_Rank,size_of_cluster,ierror,mpi_size,rank
 
      !Local Variables
      integer :: n,incx,incy,i,j
      real :: alpha,ix,iy

      !MPI Variables
      call MPI_INIT(ierror)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,mpi_size,ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)

      n = 10
      alpha = 4.0
      incx = 1
      incy = 1
      i=0

      if((incx.eq.1) .and.(incy.eq.1)) then
          do i = 1,n
            do j =1,n
               y(i,j) = alpha*x(i,j) + y(i,j)
            enddo
          enddo
      end if
     return
     call MPI_FINALIZE(ierror)
end SUBROUTINE

