#include "defines.h"
program amoc_diag
    use diagnostics_mod 
    use gen_data_in
    use file_get
    use netcdf
  
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
    character(len = charlen), allocatable :: model_fi_month(:)
    character(len = charlen), allocatable :: model_fi_year(:,:)
    integer :: num_total_year, num_total_month
    integer :: time(1:3000)
    integer :: model_year, image_year, time_diff

    logical :: if_amoc_ts, if_amoc_2d
    character(len = charlen) :: amoc_ts_file, amoc_2d_file
    integer :: amoc_start_year_ts, amoc_end_year_ts
    integer :: amoc_start_year_2d, amoc_end_year_2d

    logical :: if_nht_ts
    character(len = charlen) :: nht_ts_file
    integer :: nht_start_year_ts, nht_end_year_ts
    
    logical :: if_nht_average
    character(len = charlen) :: nht_average_file
    integer :: nht_start_year_average, nht_end_year_average

    logical :: if_acc_ts
    character(len = charlen) :: acc_ts_file
    integer :: acc_start_year_ts, acc_end_year_ts

    logical :: if_sst_cycle
    character(len = charlen) :: sst_cycle_file
    integer :: sst_cycle_start, sst_cycle_end
        
    integer :: j
    namelist /output_settings/ model_year, image_year
    namelist /amoc_settings/ if_amoc_ts, if_amoc_2d,&
                             amoc_start_year_ts,  amoc_end_year_ts,&
                             amoc_start_year_2d,  amoc_end_year_2d,&
                             amoc_ts_file, amoc_2d_file
    namelist /nht_settings/ if_nht_ts, nht_ts_file,&
                            nht_start_year_ts, nht_end_year_ts,&
                            if_nht_average, nht_average_file,&
                            nht_start_year_average, nht_end_year_average
    namelist /acc_settings/ if_acc_ts, acc_ts_file,&
                            acc_start_year_ts, acc_end_year_ts
    namelist /sst_settings/ if_sst_cycle, sst_cycle_file,&
                            sst_cycle_start, sst_cycle_end

!   set up the diagnostic grid
    call set_up_grid

!   generate the month data list
    call gen_data_in_list

!   read in the month data list and generate the year data list
    call read_in_month_data_list

!   read something about outputs    
    call read_in_about_output
!   set up time
    call set_up_time
    
    call sst_cycle_calc
contains
    !---------------------------------------------------------------------
    !                  Subroutine read_in_month_data_list
    !---------------------------------------------------------------------
    subroutine read_in_month_data_list()
        implicit none
        integer :: i, j

        open(11,file="licom_mon_data_list")
        num_total_month = 0
        do 
            read(11,*,end=110)
            num_total_month = num_total_month+1
        end do
110     close(11)
        
        num_total_year = num_total_month/12

        allocate(model_fi_month(num_total_month))
        allocate(model_fi_year(num_total_year,12))
        
        open(11,file="licom_mon_data_list")

        do i = 1, num_total_month
            read(11,"(A)") model_fi_month(i)
        end do

        close(11)
        
        do i = 1, num_total_year
            do j = 1, 12
                model_fi_year(i,j) = model_fi_month((i-1)*12+j)
            end do
        end do
     
    end subroutine read_in_month_data_list
    !---------------------------------------------------------------------
    !              End of Subroutine read_in_month_data_list
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                   Subroutine read_in_about_output
    !---------------------------------------------------------------------
    subroutine read_in_about_output()
        implicit none
        character(len = charlen) :: namelist_file_name
        call get_command_argument(1, namelist_file_name)
        open(11, file=namelist_file_name)
            read(11, nml=output_settings)
            read(11, nml=amoc_settings)
            read(11, nml=nht_settings)
            read(11, nml=acc_settings)
            read(11, nml=sst_settings)
        close(11)
        time_diff = image_year - model_year
    end subroutine read_in_about_output
    !---------------------------------------------------------------------
    !               End of Subroutine read_in_about_output
    !---------------------------------------------------------------------
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
    !---------------------------------------------------------------------
    !                       Subroutine set_up_time
    !---------------------------------------------------------------------
    subroutine set_up_time()
        implicit none
        integer :: i, j, k
        do i = 1, 3000
            time(i) = i;
        end do
    end subroutine set_up_time
    !---------------------------------------------------------------------
    !                      End of Subroutine set_up_time
    !---------------------------------------------------------------------

end program amoc_diag
