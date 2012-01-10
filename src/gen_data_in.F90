#include "defines.h"
module gen_data_in
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
  implicit none
  character(len = charlen) :: licom_mon_data_dir
  character(len = charlen) :: licom_mon_data_list
  character(len = charlen) :: namelist_file_name
  integer :: start_year, end_year

  namelist /model_data_in/ licom_mon_data_dir, licom_mon_data_list,&
                           start_year, end_year

  contains
  !---------------------------------------------------------------------
  !                      Subroutine gen_data_in_list
  !---------------------------------------------------------------------
    subroutine gen_data_in_list()
      implicit none
      character(len = charlen) :: temp(12)
      integer :: i, j, k , counter
      logical :: alive, start
      call get_command_argument(1,namelist_file_name)
      open(11, file=namelist_file_name)
      read(11, nml=model_data_in)
      close(11)
      
      open(11,file=licom_mon_data_list)
      close(11,status='delete')
      loop1:do i = start_year, end_year
          temp=" "
        loop2:do j = 1, 12
          write(temp(j),"(A,I4.4,A,I2.2,A)") trim(licom_mon_data_dir)//"/"//&
               "MMEAN",i,"-",j,".nc"
          inquire(file=temp(j),exist=alive)
          if(alive==.false.) exit loop1
        end do loop2

        loop3:do j = 1, 12
          open(unit=11,file=licom_mon_data_list,access='append')
          write(11,"(TL1,A)") trim(temp(j))
          close(11)
        end do loop3
      end do loop1



    end subroutine gen_data_in_list
  !---------------------------------------------------------------------
  !                 End of Subroutine gen_data_in_list
  !---------------------------------------------------------------------

end module gen_data_in
