Program DaxpyProgram

      implicit none
      real,dimension(:),allocatable :: x,y
      integer arraySize,i
      
   
interface
  SUBROUTINE Daxpy(x,y)

      real,dimension(:),intent(in) :: x
      real,dimension(:), intent(out):: y
  end SUBROUTINE
end interface

 allocate(x(10))
 allocate(y(10))

do i = 1,10
 x(i) = (10.2*i)
 y(i) = 10.2
enddo

 call Daxpy(x, y)
 print *,'This is the result: '
 
 arraySize = size(y)
 
 do i = 1,arraySize
 print *, y(i)
 enddo

deallocate(x(10))
deallocate(y(10))
end Program DaxpyProgram


 SUBROUTINE Daxpy(x,y)
      implicit none
      real,dimension(:),intent(in) :: x
      real,dimension(:), intent(out):: y

      !local variables
      integer :: n
      real :: alpha,ix,iy
      integer :: incx,incy,i
 
      n = 10
      alpha = 4.0
      incx = 1
      incy = 1
      i=0

  if((incx.eq.1) .and.(incy.eq.1)) then
    do i =1,n
       y(i) = alpha*x(i) + y(i)
    enddo
  else
    ix = 1
    iy = 1
    do i = 1,n
       ix = 1 + (i-1)*incx
       iy = 1 + (i-1)*incy
    enddo
  end if
  return
  end SUBROUTINE

