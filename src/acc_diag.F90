#include "defines.h"
program acc_diag
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

!   calculate the time series of Antarctic Circumpolar Current
    call acc_ts_calc
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
        close(11)
        time_diff = image_year - model_year
    end subroutine read_in_about_output
    !---------------------------------------------------------------------
    !               End of Subroutine read_in_about_output
    !---------------------------------------------------------------------
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

end program acc_diag
