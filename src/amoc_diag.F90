#include "defines.h"
program amoc_diag
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
!   calculate the time series of Atlantic Meridional Overturning Circulation (index)
    call amoc_ts_calc
!   calculate the time average of Atlantic Meridional Overturning Circulation (2d)
    call amoc_2d_calc

contains
    !---------------------------------------------------------------------
    !                       Subroutine amoc_ts_calc
    !---------------------------------------------------------------------
    subroutine amoc_ts_calc()
        implicit none
        integer :: i, j, k, ts_length, out_zero_year
        real(r8) :: amoc_ts(1:3000)
        character(len = charlen), allocatable :: all_year_file(:,:)
        
        ts_length = amoc_end_year_ts - amoc_start_year_ts + 1
        out_zero_year = time_diff + amoc_start_year_ts - 1
        allocate(all_year_file(ts_length,12))

        call file_delete(amoc_ts_file)
        
        if(.not.if_amoc_ts) return 
        amoc_ts = miss
        call get_all_year_file(model_fi_year,start_year,end_year,all_year_file,amoc_start_year_ts,amoc_end_year_ts)
        do i = 1, ts_length
            call update_one_year_data(all_year_file(i,:),["vs"],1)
            call moc_calc
            where(amoc == miss) amoc = 0.0d0
            amoc_ts(out_zero_year+i) = maxval(amoc)*1.0d-6
            print *,amoc_ts(out_zero_year+i)
        end do

        call create_nc(amoc_ts_file)
        call add_dim2nc_int(amoc_ts_file,"time","time","year",3000,time)
        call add_1dvar2nc(amoc_ts_file,["time"],"amoc","Atlantic Meridional Overturning Circulation","Sv",miss,amoc_ts,[3000],1)
    end subroutine amoc_ts_calc
    !---------------------------------------------------------------------
    !                      End of Subroutine amoc_ts_calc
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine amoc_2d_calc
    !---------------------------------------------------------------------
    subroutine amoc_2d_calc()
        implicit none 
        integer :: i, j, k, num_all_month
        character(len = charlen) :: all_month_file((amoc_end_year_2d-amoc_start_year_2d+1)*12)
        
        num_all_month = (amoc_end_year_2d-amoc_start_year_2d+1)*12
        call file_delete(amoc_2d_file)
        if(.not.if_amoc_2d) return
        
        call get_all_month_file(model_fi_year,start_year,end_year,all_month_file,amoc_start_year_2d,amoc_end_year_2d)
        call update_all_month_data(all_month_file,num_all_month,["vs"],1) 
        call moc_calc
        
        where (amoc == miss) amoc = miss
        where (global_moc == miss) global_moc = miss

        where (amoc /= miss) amoc = amoc*1.0d-6
        where (global_moc /= miss) global_moc = global_moc*1.d-6
        call create_nc(amoc_2d_file)
        call add_dim2nc(amoc_2d_file,"lat","latitude of the V grid","degrees_north",num_lat,lat_v)
        call add_dim2nc(amoc_2d_file,"lev","depth of the W level","metres",num_lev_half,lev_half)
        call add_2dvar2nc(amoc_2d_file,["lat","lev"],"amoc","Atlantic Meridional Overturning Circulation","Sv",miss,amoc,[num_lat,num_lev_half],2)
        call add_2dvar2nc(amoc_2d_file,["lat","lev"],"global_moc","Global Meridional Overturning Circulation","Sv",miss,global_moc,[num_lat,num_lev_half],2)

    end subroutine amoc_2d_calc
    !---------------------------------------------------------------------
    !                      End of Subroutine amoc_2d_calc
    !---------------------------------------------------------------------
end program amoc_diag
