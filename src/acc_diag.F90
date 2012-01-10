#include "defines.h"
program acc_diag
    use main_basics  
! Purpose:
! This code is devised for diagnose the licom outputs.
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
!   set up the diagnostic grid
    call set_up_grid

!   generate the month data list

!   read in the month data list and generate the year data list
    call read_in_month_data_list

!   read something about outputs    
    call read_in_about_output
!   set up time
    call set_up_time

!   calculate the time series of Antarctic Circumpolar Current
    call acc_ts_calc
contains
    !---------------------------------------------------------------------
    !                       Subroutine acc_ts_calc
    !---------------------------------------------------------------------
    subroutine acc_ts_calc()
        implicit none
        integer :: i, j, k, ts_length, out_zero_year
        real(r8) :: acc_ts(1:3000)
        character(len = charlen), allocatable :: all_year_file(:,:)
        
        ts_length = acc_end_year_ts - acc_start_year_ts + 1
        out_zero_year = time_diff + acc_start_year_ts - 1
        allocate(all_year_file(ts_length,12))
        call file_delete(acc_ts_file)
        
        if(.not.if_acc_ts) return 
        acc_ts = miss
        call get_all_year_file(model_fi_year,start_year,end_year,all_year_file,acc_start_year_ts,acc_end_year_ts)
        do i = 1, ts_length
            call update_one_year_data(all_year_file(i,:),["us"],1)
            call acc_calc
            acc_ts(out_zero_year+i) = acc_index
            print *, acc_index
        end do
        call create_nc(acc_ts_file)
        call add_dim2nc_int(acc_ts_file,"time","time","year",3000,time)
        call add_1dvar2nc(acc_ts_file,["time"],"acc","Antarctic Circumpolar Current","Sv",miss,acc_ts,[3000],1)
       
    end subroutine acc_ts_calc
    !---------------------------------------------------------------------
    !                      End of Subroutine acc_ts_calc
    !---------------------------------------------------------------------
end program acc_diag
