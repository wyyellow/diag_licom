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
    
    call sst_cycle_calc
contains
    !---------------------------------------------------------------------
    !                          Subroutine sst_cycle_calc
    !---------------------------------------------------------------------
    subroutine sst_cycle_calc()
        implicit none
        integer :: i, j, k, ts_length, j_start, j_end
        character(len = charlen), allocatable :: all_year_file(:,:)
        real(r8) :: lat_s, lat_n, small_area, small_total
        real(r8) :: sst_cycle(num_lon,12)
        lat_s = -2.0d0
        lat_n =  2.0d0
        do j = 1, num_lat
            if (lat_t(j)<=lat_n) then 
                j_start = j
                exit
            end if
        end do
        do j = num_lat, 1, -1
            if (lat_t(j)>=lat_s) then 
                j_end = j
                exit
            end if
        end do
        ts_length = sst_cycle_end - sst_cycle_start + 1
        allocate(all_year_file(ts_length,12))
        call file_delete(sst_cycle_file)
        if(.not.if_sst_cycle) return 
        call get_all_year_file(model_fi_year,start_year,end_year,all_year_file,sst_cycle_start,sst_cycle_end)
        call get_seasonal_cycle(all_year_file,ts_length,"ts")
        
        do k = 1, 12
            do i = 1, num_lon
                if(all(vit(i,j_start:j_end,1)==0))then
                    sst_cycle(i,k) = miss
                else
                    small_area  =   0.0d0
                    small_total =   0.0d0
                    do j = j_start, j_end
                        small_area  = small_area + dble(vit(i,j,1))*dx_t(j)*dy_t(j)
                        small_total = small_total + dble(vit(i,j,1))*dx_t(j)*dy_t(j)*ts_anomaly(i,j,1,k)  
                        sst_cycle(i,k) = small_total/small_area
                    end do
                end if
            end do
        end do

        call create_nc(sst_cycle_file)
        call add_dim2nc_int(sst_cycle_file,"time","time","month",12,time(1:12))
        call add_dim2nc(sst_cycle_file,"lon","longitude of the T grid","degrees_east",num_lon,lon_t)
        call add_4dvar2nc(sst_cycle_file,["lon","time"],"sst_cycle","sst cycle","K",miss,sst_cycle,[num_lon,12],2)
    
    end subroutine sst_cycle_calc
    !---------------------------------------------------------------------
    !                      End of Subroutine sst_cycle_calc
    !---------------------------------------------------------------------
end program amoc_diag
