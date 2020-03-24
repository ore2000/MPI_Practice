! Decalration of variables 
  implicit none
  integer :: n,incx,incy
  double precision :: i,ix,iy
  double precision :: alpha,x(:),y(:)

  program DaxpyProgram
      call Daxpy(n, alpha, x, incx, y, incy)
      print *,'This is the result: ',y
  end Program DaxpyProgram


  SUBROUTINE Daxpy(n, alpha, x, incx, y, incy)
  
  if((incx.eq.1) .and. (incry.eq.1)) then
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
