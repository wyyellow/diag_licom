#include "defines.h"
module parameters
! Purpose:
! This module is devised for setting the parameters.
!
!
! Record of revisions:
!    Date             Programmer                Description of change  
!==============    ===================   =============================
! 2011/12/22         WenYu Huang                Original Code
! ...
!
!Any comments please send to huangwenyu@mail.tsinghua.edu.cn                      
real(r8), parameter :: Re = 6.371d6 ! earth radius
real(r8), parameter :: pi = 4*atan(1.0d0)
real(r8), parameter :: torad = pi/180.0d0
real(r8), parameter :: todeg = 180.0d0/pi
real(r8), parameter :: Cp = 3901.0d0

end module parameters
