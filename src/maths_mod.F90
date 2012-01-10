#include "defines.h"
module maths_mod
! Purpose:
! This module is devised for ...
!
!
! Record of revisions:
!    Date             Programmer                Description of change  
!==============    ===================   =============================
! 2011/12/22         WenYu Huang                Original Code
! ...
!
!Any comments please send to huangwenyu@mail.tsinghua.edu.cn                      
contains
    !---------------------------------------------------------------------
    !                          Subroutine spline
    !---------------------------------------------------------------------
    subroutine spline(x,y,n,yp1,ypn,y2)
        implicit none
        integer, intent(in) :: n
        real(r8), intent(in) :: x(n), y(n), yp1, ypn
        real(r8), intent(out) :: y2(n)
        integer, parameter :: nmax = 500
        integer :: i, j, k
        real(r8) :: p, qn, sig, un, u(nmax)
        
        if (yp1 >= 1.0d30) then
            y2(1) = 0.0d0; u(1) = 0.0d0
        else
            y2(1) = -0.5d0; u(1) = (3.d0/(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
        end if

        loopii: do i = 2, n-1
            sig     =   (x(i)-x(i-1))/(x(i+1)-x(i-1))
            p       =   sig*y2(i-1)+2.d0
            y2(i)   =   (sig-1.d0)/p
            u(i)    =   (6.d0*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))&
                        /(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
        end do loopii

        if (ypn >= 1.0d30) then
            qn  =   0.0d0
            un  =   0.0d0
        else
            qn  =   0.5d0
            un  =   (3.0d0/(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
        end if

        y2(n)   =   (un-qn*u(n-1))/(qn*y2(n-1)+1.d0)
        
        loopkk: do k = n-1, 1, -1
            y2(k)=y2(k)*y2(k+1)+u(k)
        end do loopkk
        return
    end subroutine spline
    !---------------------------------------------------------------------
    !                      End of Subroutine spline
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine splint
    !---------------------------------------------------------------------
    subroutine splint(xa,ya,y2a,n,x,y)
        implicit none
        integer, intent(in) :: n
        real(r8), intent(in) :: xa(n), ya(n), y2a(n)
        real(r8), intent(in) :: x
        real(r8), intent(out) :: y
        integer :: i, j, k, khi, klo
        real(r8) :: a, b, h

        klo     =   1
        khi     =   n

        do while ((khi-klo) > 1) 
            k   =   (khi+klo)/2
            if(xa(k) > x) then
                khi = k
            else
                klo = k
            end if
        end do

        h   =   xa(khi) - xa(klo)
        if (abs(h) <= 1.0d-10) then
            write(*,*) 'error: bad input xa'
            stop
        end if

        a   =   (xa(khi)-x)/h
        b   =   (x-xa(klo))/h
        y   =   a*ya(klo)+b*ya(khi)+&
                ((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.d0 
    end subroutine splint
    !---------------------------------------------------------------------
    !                      End of Subroutine splint
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                   Subroutine interpolat2d_var_from_t2v
    !---------------------------------------------------------------------
    subroutine interpolat2d_var_from_t2v(var_in,var_out,miss,num_lon,num_lat)
        implicit none
        integer, intent(in) :: num_lon, num_lat
        real(r8), intent(in) :: miss
        real(r8), intent(in) :: var_in(num_lon,num_lat)
        real(r8), intent(out) :: var_out(num_lon,num_lat)
        integer :: i, j, k

        var_out = miss    
        do j = 1, num_lat-1
            do i = 1, num_lon
                if (var_in(i,  j  ) /= miss .and.&
                    var_in(i+1,j  ) /= miss .and.&
                    var_in(i,  j+1) /= miss .and.&
                    var_in(i+1,j+1) /= miss .and.&
                    i <= num_lon-1) then
                    
                    var_out(i,j) = var_in(i,  j  )/4.0d0+&
                                   var_in(i+1,j  )/4.0d0+&
                                   var_in(i,  j+1)/4.0d0+&
                                   var_in(i+1,j+1)/4.0d0
                    
                elseif (var_in(i,  j  ) /= miss .and.&
                        var_in(1,  j  ) /= miss .and.&
                        var_in(i,  j+1) /= miss .and.&
                        var_in(1,  j+1) /= miss .and.&
                        i == num_lon) then
                        
                    var_out(i,j) = var_in(i,  j  )/4.0d0+&
                                   var_in(1,  j  )/4.0d0+&
                                   var_in(i,  j+1)/4.0d0+&
                                   var_in(1,  j+1)/4.0d0

                end if
            end do
        end do

    end subroutine interpolat2d_var_from_t2v
    !---------------------------------------------------------------------
    !               End of Subroutine interpolat2d_var_from_t2v
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                   Subroutine bilinear_interpolation
    !---------------------------------------------------------------------
    subroutine bilinear_interpolation(lon_in,  lat_in,  num_lon_in,  num_lat_in,  data_in, &
                                      lon_out, lat_out, num_lon_out, num_lat_out, data_out )
        implicit none
        integer, intent(in) :: num_lon_in, num_lat_in
        real(r8), intent(in) :: lon_in(num_lon_in), lat_in(num_lat_in)
        real(r8), intent(in) :: data_in(num_lon_in, num_lat_in)
        integer, intent(in) :: num_lon_out, num_lat_out
        real(r8), intent(in) :: lon_out(num_lon_out), lat_out(num_lat_out)
        real(r8), intent(out) :: data_out(num_lon_out, num_lat_out)

    end subroutine bilinear_interpolation
    !---------------------------------------------------------------------
    !               End of Subroutine bilinear_interpolation
    !---------------------------------------------------------------------


end module maths_mod
