#include "defines.h"
module file_get
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
    !                     Subroutine get_all_year_file
    !---------------------------------------------------------------------
    subroutine get_all_year_file(main_file, main_start, main_end,&
                                 sub_file,  sub_start,  sub_end)
    
        implicit none
        integer, intent(in) :: sub_start, sub_end
        integer, intent(in) :: main_start, main_end
        character(len = *), intent(in) :: main_file(main_end-main_start+1,12)
        character(len = charlen), intent(out) :: sub_file(sub_end-sub_start+1,12)
        integer :: i, j, k, diff
        
        diff = sub_start-main_start
        
        do i = 1, sub_end-sub_start+1
            sub_file(i,:) = main_file(i+diff,:)
        end do
    end subroutine get_all_year_file
    !---------------------------------------------------------------------
    !                  End of Subroutine get_all_year_file
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                   Subroutine get_all_month_file
    !---------------------------------------------------------------------
    subroutine get_all_month_file(main_file,       main_start, main_end,&
                                  sub_month_file,  sub_start,  sub_end)
        implicit none
        integer, intent(in) :: sub_start, sub_end
        integer, intent(in) :: main_start, main_end
        character(len = *), intent(in) :: main_file(main_end-main_start+1,12)
        character(len = charlen),intent(out) :: sub_month_file((sub_end-sub_start+1)*12)
        character(len = charlen) :: sub_file(sub_end-sub_start+1,12)
        
        integer :: i, j, k, diff
        
        diff = sub_start-main_start
        do i = 1, sub_end-sub_start+1
            sub_file(i,:) = main_file(i+diff,:)
        end do

        do i = 1, sub_end-sub_start+1
            do j = 1, 12
                sub_month_file((i-1)*12+j) = sub_file(i,j)
            end do
        end do
    end subroutine get_all_month_file
    !---------------------------------------------------------------------
    !                      End of Subroutine get_all_month_file
    !---------------------------------------------------------------------
end module file_get
