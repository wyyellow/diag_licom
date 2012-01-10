#include "defines.h"
program nht_diag
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
    
    call nht_ts_calc

    call nht_average_calc
contains
    !---------------------------------------------------------------------
    !                       Subroutine nht_ts_calc
    !---------------------------------------------------------------------
    subroutine nht_ts_calc()
        implicit none
        integer :: i, j, k, ts_length, out_zero_year
        real(r8) :: nht_ts(1:num_lat,1:3000)
        character(len = charlen), allocatable :: all_year_file(:,:)
        
        ts_length = nht_end_year_ts - nht_start_year_ts + 1
        out_zero_year = time_diff + nht_start_year_ts - 1
        allocate(all_year_file(ts_length,12))
        call file_delete(nht_ts_file)
    
        if(.not.if_nht_ts) return 
        nht_ts = miss
        call get_all_year_file(model_fi_year,start_year,end_year,all_year_file,nht_start_year_ts,nht_end_year_ts)

        do i = 1, ts_length
            call update_one_year_data(all_year_file(i,:),["vs","ss","ts"],3)
            call nht_calc
            nht_ts(:,out_zero_year+i) = nht
        end do
        call create_nc(nht_ts_file)
        call add_dim2nc_int(nht_ts_file,"time","time","year",3000,time)
        call add_dim2nc(nht_ts_file,"lat","latitude of V grid","degrees_north",num_lat,lat_v)
        call add_2dvar2nc(nht_ts_file,["lat","time"],"nht","Northward heat transport","Pw",miss,nht_ts,[num_lat,3000],2)
    end subroutine nht_ts_calc
    !---------------------------------------------------------------------
    !                      End of Subroutine nht_ts_calc
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                    Subroutine nht_average_calc
    !---------------------------------------------------------------------
    subroutine nht_average_calc()
        implicit none
        integer :: i, j, k, num_all_month
        real(r8) :: nht_average(1:num_lat)
        character(len = charlen) :: all_month_file((nht_end_year_average-nht_start_year_average+1)*12)

        num_all_month = (nht_end_year_average-nht_start_year_average+1)*12
        call file_delete(nht_average_file)
        
        if(.not.if_nht_average) return 
        nht_average = miss
        call get_all_month_file(model_fi_year,start_year,end_year,all_month_file,nht_start_year_average,nht_end_year_average)
        call update_all_month_data(all_month_file,num_all_month,["vs","ss","ts"],3) 
        call nht_calc
        nht_average = nht
        call create_nc(nht_average_file)
        call add_dim2nc(nht_average_file,"lat","latitude of the V grid","degrees_north",num_lat,lat_v)
        call add_1dvar2nc(nht_average_file,["lat"],"nht","Northward Heat Transport","Pw",miss,nht_average,[num_lat],1)
    end subroutine nht_average_calc
    !---------------------------------------------------------------------
    !                   End of Subroutine nht_average_calc
    !---------------------------------------------------------------------
end program nht_diag
