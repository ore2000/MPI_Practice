
  program DaxpyProgram
      implicit none
      integer :: n,incx,incy
      real :: alpha
      real,dimension(:) :: x,y
      n = 10
      incx = 2
      incy = 2
      alpha = 4.
      call Daxpy(n, alpha, x, incx, y, incy)
      print *,'This is the result: ',y

  end Program DaxpyProgram


  SUBROUTINE Daxpy(n, alpha, x, incx, y, incy)
      implicit none
      integer, intent(in) :: n,incx,incy
      real,dimension(:),intent(in) :: x
      real,intent(in) :: alpha
      real,dimension(:), intent(out):: y
      !local variables
      real :: i,ix,iy

  if((incx.eq.1) .and. (incy.eq.1)) then
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
