Program DaxpyProgram


      implicit none
      real,dimension(:,:),allocatable :: x,y
      integer i,j
      real :: start,finish
  
interface
  SUBROUTINE Daxpy(x,y)

      real,dimension(:,:),intent(in) :: x
      real,dimension(:,:), intent(out):: y
  end SUBROUTINE
end interface

call cpu_time(start)
 allocate(x(10,10))
 allocate(y(10,10))

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
     print *,'Matrix(',i,',',j,')= ',y(i,j)
   enddo
enddo

deallocate(x)
deallocate(y)

call cpu_time(finish)
print *, 'Finished in time', (finish - start)
end Program DaxpyProgram


 SUBROUTINE Daxpy(x,y)
      implicit none
      real,dimension(:,:),intent(in) :: x
      real,dimension(:,:), intent(out):: y

      !local variables
      integer :: n
      real :: alpha,ix,iy
      integer :: incx,incy,i,j
 
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
  end SUBROUTINE

