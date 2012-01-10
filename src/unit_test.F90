#include "defines.h"
program unit_test
    use maths_mod 
    implicit none
    
    integer, parameter :: n = 101
    real(r8), parameter :: pi = atan(1.0d0)*4.0d0
    real(r8) :: x(n), y(n), y2(n), yp1, yp2, x_p, y_p
    integer :: i, j, k
    
    do i = 1, n
        x(i) = (i-1)*pi/dble(n-1)
    end do
       
        yp1 =   1.0d0
        yp2 =   -1.0d0
        y   =   sin(x)
    
    call spline(x,y,n,yp1,yp2,y2)
    
    x_p =   1.57d0
    call splint(x,y,y2,n,x_p,y_p)
    print *,y_p
end program unit_test
