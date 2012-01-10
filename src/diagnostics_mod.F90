#include "defines.h"
module diagnostics_mod
! Purpose:
! This module is devised for set up the grid information for the diagnostics.
!
!
! Record of revisions:
!    Date               Programmer           Description of change  
!==============    ===================   =============================
! 2011/12/22           WenYu Huang              Original Code
! ...
!
! Any comments please send to huangwenyu@mail.tsinghua.edu.cn                      
    use utils
    use parameters
    use maths_mod
    use nc_read_write

    implicit none  
  
    public set_up_grid

    integer :: num_lon, num_lat, num_lev, num_lev_half
    real(r8) :: miss 
    real(r8), allocatable :: lon_t(:), lat_t(:)
    real(r8), allocatable :: lon_v(:), lat_v(:)
    real(r8), allocatable :: lev(:), lev_half(:)
    real(r8), allocatable :: dlon_t(:), dlat_t(:)
    real(r8), allocatable :: dlon_v(:), dlat_v(:)
    real(r8), allocatable :: lon_t_bnds(:), lat_t_bnds(:)
    real(r8), allocatable :: lon_v_bnds(:), lat_v_bnds(:)
    real(r8), allocatable :: lev_bnds(:), lev_half_bnds(:)
    real(r8), allocatable :: dx_t(:), dy_t(:)
    real(r8), allocatable :: dx_v(:), dy_v(:)
    real(r8), allocatable :: dz(:), dz_w(:)
    integer, allocatable :: basin(:,:)
    real(r8), allocatable :: work_1(:,:,:), work_2(:,:,:)
    integer, allocatable :: vit(:,:,:), viv(:,:,:), viv_iso(:,:,:)
    real(r8), allocatable :: to(:), po(:), so(:), c(:,:), pdens(:,:,:)
    real(r8), allocatable :: us_iso(:,:,:), vs_iso(:,:,:)
    real(r8), allocatable :: us(:,:,:), vs(:,:,:), ws(:,:,:)
    real(r8), allocatable :: z0(:,:), ts(:,:,:), ss(:,:,:)
    real(r8), allocatable :: mld(:,:), net1(:,:), net2(:,:)
    real(r8), allocatable :: us_month(:,:,:), vs_month(:,:,:), ws_month(:,:,:)
    real(r8), allocatable :: z0_month(:,:), ts_month(:,:,:), ss_month(:,:,:)
    real(r8), allocatable :: mld_month(:,:), net1_month(:,:), net2_month(:,:)
    real(r8), allocatable :: us_cycle(:,:,:,:), vs_cycle(:,:,:,:), ws_cycle(:,:,:,:)
    real(r8), allocatable :: ss_cycle(:,:,:,:), ts_cycle(:,:,:,:), z0_cycle(:,:,:)
    real(r8), allocatable :: mld_cycle(:,:,:), net1_cycle(:,:,:), net2_cycle(:,:,:)
    real(r8), allocatable :: us_anomaly(:,:,:,:), vs_anomaly(:,:,:,:), ws_anomaly(:,:,:,:)
    real(r8), allocatable :: ss_anomaly(:,:,:,:), ts_anomaly(:,:,:,:), z0_anomaly(:,:,:)
    real(r8), allocatable :: mld_anomaly(:,:,:), net1_anomaly(:,:,:), net2_anomaly(:,:,:)
    real(r8), allocatable :: us_mean(:,:,:), vs_mean(:,:,:), ws_mean(:,:,:)
    real(r8), allocatable :: z0_mean(:,:), ts_mean(:,:,:), ss_mean(:,:,:)
    real(r8), allocatable :: mld_mean(:,:), net1_mean(:,:), net2_mean(:,:)
    real(r8), allocatable :: amoc(:,:), global_moc(:,:),nht(:)
    real(r8), allocatable :: pdens_temp(:,:,:)
    real(r8) :: global_sst, global_sss
    real(r8) :: acc_index
    character(len = charlen) :: namelist_file_name
    character(len = charlen) :: file_for_grid_info
    character(len = charlen) :: basin_file_name
    character(len = charlen) :: density_file_name

!------------------------
!   variables for acc
!------------------------
    integer :: i_drake, j_drake
    namelist /grid_information/ num_lon, num_lat, num_lev, file_for_grid_info,&
                                basin_file_name, density_file_name

  contains
    !---------------------------------------------------------------------
    !                      Subroutine read_namelist
    !---------------------------------------------------------------------
      subroutine read_namelist()
        call get_command_argument(1, namelist_file_name)
        open(11, file=namelist_file_name)
        read(11, nml=grid_information)
        close(11)
      end subroutine read_namelist
    !---------------------------------------------------------------------
    !                   End of Subroutine read_namelist
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                   Subroutine set_up_grid
    !---------------------------------------------------------------------
      subroutine set_up_grid()
        implicit none
        real(r8), allocatable :: tt(:,:,:), vv(:,:,:)
        integer :: ierr, ncid, lon_t_id, lat_t_id, lev_id, lev_half_id
        integer :: tt_id, vv_id, basin_id
        integer :: i, j, k
        call read_namelist
        num_lev_half = num_lev + 1
        allocate(lon_t(num_lon),lat_t(num_lat))
        allocate(lon_t_bnds(num_lon+1),lat_t_bnds(num_lat+1))
        allocate(lon_v(num_lon),lat_v(num_lat))
        allocate(lon_v_bnds(num_lon+1),lat_v_bnds(num_lat+1))
        allocate(lev(num_lev),lev_half(num_lev_half))
        allocate(dx_t(num_lat),dy_t(num_lat))
        allocate(dx_v(num_lat),dy_v(num_lat))
        allocate(dz(num_lev),dz_w(2:num_lev))
        allocate(dlon_t(num_lon-1),dlat_t(num_lon-1))
        allocate(dlon_v(num_lon-1),dlat_v(num_lon-1))
        allocate(lev_bnds(num_lev+1))
        allocate(lev_half_bnds(num_lev_half+1))
        allocate(tt(num_lon,num_lat,num_lev))
        allocate(vv(num_lon,num_lat,num_lev))
        allocate(us_iso(num_lon,num_lat,num_lev))
        allocate(vs_iso(num_lon,num_lat,num_lev))
        allocate(vit(num_lon,num_lat,num_lev)) 
        allocate(viv(num_lon,num_lat,num_lev)) 
        allocate(viv_iso(num_lon,num_lat,num_lev))
        allocate(basin(num_lon,num_lat))
        allocate(to(num_lev),po(num_lev),so(num_lev),c(num_lev,9))
        allocate(pdens(num_lon,num_lat,num_lev))
        allocate(pdens_temp(num_lon,num_lat,num_lev))
        allocate(nht(num_lat))
        allocate(work_1(num_lon,num_lat,num_lev))
        allocate(work_2(num_lon,num_lat,num_lev))

        allocate(us_month(num_lon,num_lat,num_lev))  
        allocate(vs_month(num_lon,num_lat,num_lev))  
        allocate(ws_month(num_lon,num_lat,num_lev))  
        allocate(ss_month(num_lon,num_lat,num_lev))  
        allocate(ts_month(num_lon,num_lat,num_lev))  
        allocate(z0_month(num_lon,num_lat))  
        allocate(mld_month(num_lon,num_lat))  
        allocate(net1_month(num_lon,num_lat))  
        allocate(net2_month(num_lon,num_lat))  
        allocate(us(num_lon,num_lat,num_lev))  
        allocate(vs(num_lon,num_lat,num_lev))  
        allocate(ws(num_lon,num_lat,num_lev))  
        allocate(ss(num_lon,num_lat,num_lev))  
        allocate(ts(num_lon,num_lat,num_lev))  
        allocate(z0(num_lon,num_lat))  
        allocate(mld(num_lon,num_lat))  
        allocate(net1(num_lon,num_lat))  
        allocate(net2(num_lon,num_lat))  
        call get_missing_from_nc(file_for_grid_info,miss,"ts")
        call read_1dvar(file_for_grid_info,"lon",lon_t,[1],[num_lon],[num_lon],1)
        call read_1dvar(file_for_grid_info,"lat",lat_t,[1],[num_lat],[num_lat],1)
        call read_1dvar(file_for_grid_info,"lev",lev,[1],[num_lev],[num_lev],1)
        call read_1dvar(file_for_grid_info,"lev1",lev_half,[1],[num_lev_half],[num_lev_half],1)
       
        call read_3dvar(file_for_grid_info,"ts",tt,[1,1,1],[num_lon,num_lat,num_lev],[num_lon,num_lat,num_lev],3) 
        call read_3dvar(file_for_grid_info,"vs",vv,[2,1,1],[num_lon,num_lat,num_lev],[num_lon,num_lat,num_lev],3) 

!------------------------
!       basin ocean
!------------------------
!basin = 0 land
!basin = 1 arctic
!basin = 2 Atlantic Northern part
!basin = 3 Indian Northern part
!basin = 4 Pacific Northern part
!basin = 5 Atlantic Southern part
!basin = 6 Indian Southern part
!basin = 7 Pacific Southern part
        call read_2dvar_int(basin_file_name,"ind",basin,[1,1],[num_lon,num_lat],[num_lon,num_lat],2)
!------------------------
!read in density parameters
!------------------------
        open(11,file=density_file_name,form='formatted')
        read(11,*)
        read(11,*) to
        read(11,*)
        read(11,*) so
        read(11,*) 
        read(11,*) po
        do i = 1, num_lev
            read(11,*)
            read(11,*) (c(i,k),k=1,9)
        end do
        close(11)
            c = 1.0d3*c
        do i = 1, num_lon-1
          lon_v(i) = lon_t(i)/2.0d0+lon_t(i+1)/2.0d0 
        end do
          lon_v(i) = lon_v(num_lon-1)+1.0d0

        do j = 1, num_lat-1
          lat_v(j) = lat_t(j)/2.0d0+lat_t(j+1)/2.0d0 
        end do
          lat_v(num_lat) = lat_v(num_lat-1)-1.0d0
!------------------------
!      dlon and dlat
!------------------------
        do i = 1, num_lon-1
          dlon_t(i) = lon_t(i+1)-lon_t(i)
          dlon_v(i) = lon_v(i+1)-lon_v(i)
        end do

        do j = 1, num_lat-1
          dlat_t(j) = lat_t(j)-lat_t(j+1)
          dlat_v(j) = lat_v(j)-lat_v(j+1)
        end do
!------------------------
!        lon_bnds
!------------------------          
          lon_t_bnds(1) = lon_t(1)-dlon_t(num_lon-1)*0.5d0
          lon_v_bnds(1) = lon_v(1)-dlon_v(num_lon-1)*0.5d0
        do i = 2, num_lon
          lon_t_bnds(i) = lon_t(i)-dlon_t(i-1)*0.5d0
          lon_v_bnds(i) = lon_v(i)-dlon_v(i-1)*0.5d0 
        end do
          lon_t_bnds(num_lon+1) = lon_t(num_lon)+dlon_t(1)*0.5d0
          lon_v_bnds(num_lon+1) = lon_v(num_lon)+dlon_v(1)*0.5d0
!------------------------
!        lat_bnds
!------------------------
          lat_t_bnds(1) = lat_t(1)+dlat_t(1)*0.5d0
          lat_v_bnds(1) = lat_v(1)+dlat_v(1)*0.5d0
        do j = 2, num_lat
          lat_t_bnds(j) = lat_t(j)+dlat_t(j-1)*0.5d0
          lat_v_bnds(j) = lat_v(j)+dlat_v(j-1)*0.5d0
        end do
          lat_t_bnds(num_lat+1) = lat_t(num_lat)-dlat_t(num_lat-1)*0.5d0
          lat_v_bnds(num_lat+1) = lat_v(num_lat)-dlat_v(num_lat-1)*0.5d0
 !------------------------
 !       dx and dy
 !------------------------
        do j = 1, num_lat
          dx_t(j) = Re*cos(lat_t(j)*torad)*(lon_t(2)-lon_t(1))*torad
          dy_t(j) = Re*(lat_t_bnds(j)-lat_t_bnds(j+1))*torad
          dx_v(j) = Re*cos(lat_v(j)*torad)*(lon_v(2)-lon_v(1))*torad
          dy_v(j) = Re*(lat_v_bnds(j)-lat_v_bnds(j+1))*torad
        end do
!------------------------
!lev_bnds and lev_half_bnds
!------------------------
          lev_bnds =  lev_half
        do k = 1, num_lev
          dz(k) = lev_bnds(k)-lev_bnds(k+1)
        end do

          lev_half_bnds(1) = lev_half(1)
        do k = 2, num_lev_half
          lev_half_bnds(k) = lev(k-1)          
        end do
          lev_half_bnds(num_lev_half+1) = lev_half(num_lev_half)

        do k = 2, num_lev
          dz_w(k) = lev(k-1) - lev(k) 
        end do
        
        where(tt/=miss) vit=1
        where(tt==miss) vit=0
        where(vv/=miss) viv=1
        where(vv==miss) viv=0
        
        do k = 3, num_lev-2
            do j = 1, num_lat
                do i = 1, num_lon
                    if (all(viv(i,j,k-1:k+1)==1)) then
                    viv_iso(i,j,k) = 1
                    else
                    viv_iso(i,j,k) = 0
                    end if
                end do
            end do
        end do
        
        deallocate(tt)
        deallocate(vv)

      end subroutine set_up_grid
    !---------------------------------------------------------------------
    !                  End of Subroutine set_up_grid
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine acc_calc
    !---------------------------------------------------------------------
      subroutine acc_calc()
        implicit none
        real(r8), parameter :: drake_lon=-69.5d0+360.0d0
        real(r8), parameter :: ref_south=-45.d0
        integer :: i, j, k
        real(r8) :: dis
        do j = 1, num_lat
          if (lat_v(j)<=ref_south)then
            j_drake = j
            exit
          end if
        end do

        dis = abs(lon_v(1)-drake_lon)
        i_drake = 1
        do i = 1, num_lon
          if (abs(lon_v(i)-drake_lon)<dis) then
            i_drake = i
            dis = abs(lon_v(i)-drake_lon)
          end if
        end do

        acc_index = 0.0d0
        do k = 1, num_lev
          do j = j_drake, num_lat
            acc_index = acc_index + &
                        viv(i_drake,j,k)*dy_v(j)*dz(k)*us(i_drake,j,k)
          end do
        end do
            acc_index = acc_index*1.0d-6
      end subroutine acc_calc
    !---------------------------------------------------------------------
    !                      End of Subroutine acc_calc
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine nht_calc
    !---------------------------------------------------------------------
    subroutine nht_calc()
        implicit none
        integer :: i, j, k

        call density
        
        do k = 1, num_lev
            call interpolat2d_var_from_t2v(pdens(:,:,k),pdens_temp(:,:,k),miss,num_lon,num_lat)          
        end do

        nht = miss
        
        work_1(:,:,k) = 0.0d0

        do k = 1, num_lev
            where (basin == 1) 
                work_1(:,:,k) = 1.0d0*dble(vit(:,:,k))
            elsewhere (basin == 2)
                work_1(:,:,k) = 1.0d0*dble(vit(:,:,k))
            endwhere
        end do

        do k = 1, num_lev
            do j = 1, num_lat-1
                do i = 1, num_lon-1
                    work_2(i,j,k)       = work_1(i,      j, k)*work_1(i,      j+1, k)*&
                                          work_1(i+1,    j, k)*work_1(i+1,    j+1, k)
                end do
                    work_2(num_lon,j,k) = work_1(1,      j, k)*work_1(1,      j+1, k)*&
                                          work_1(num_lon,j, k)*work_1(num_lon,j+1, k)
            end do
                do i = 1, num_lon
                    work_2(i,num_lat,k) = 0.0d0
                end do
        end do

        nht = 0.0d0

        do j = 1, num_lat
            do k = 1, num_lev
                do i = 1, num_lon
                    nht(j) = nht(j)+pdens_temp(i,j,k)*Cp*vs(i,j,k)*ts(i,j,k)*dx_v(j)*dz(k)*work_2(i,j,k)
                end do
            end do
        end do
        nht = nht*1.0d-15
        do j = 1, num_lat
            if(all(work_2(:,j,:)<0.5)) nht(j) = miss
        end do
        
    end subroutine nht_calc
    !---------------------------------------------------------------------
    !                      End of Subroutine nht_calc
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine moc_calc
    !---------------------------------------------------------------------
      subroutine moc_calc()
        implicit none
        real(r8), allocatable :: moc(:,:,:), moc_temp(:,:)
        real(r8) :: max_value
        integer, allocatable :: nta(:)
        integer :: i, j, k, l

        if (.not.allocated(moc)) allocate(moc(num_lat,num_lev_half,3))
        if (.not.allocated(amoc)) allocate(amoc(num_lat,num_lev_half))
        if (.not.allocated(global_moc)) allocate(global_moc(num_lat,num_lev_half))
        if (.not.allocated(moc_temp)) allocate(moc_temp(num_lat,num_lev))
        if (.not.allocated(nta)) allocate(nta(num_lat))
    loop_l:do l = 1, 3
        do k = 1, num_lev
          do j = 1, num_lat
            do i = 1, num_lon
              if(l==1)then
                if(basin(i,j)==1.or.basin(i,j)==2)then
                  work_1(i,j,k) = 1.0d0*dble(vit(i,j,k))
                else
                  work_1(i,j,k) = 0.0d0
                endif
              elseif(l==2)then
                if(basin(i,j)==3.or.basin(i,j)==4)then
                  work_1(i,j,k) = 1.0d0*dble(vit(i,j,k))
                else
                  work_1(i,j,k) = 0.0d0
                endif
              elseif(l==3)then
                  work_1(i,j,k) = 1.0d0*dble(vit(i,j,k))
                  work_2(i,j,k) = 1.0d0*dble(viv(i,j,k))
              endif
            end do
          end do
        end do
        
        do k = 1, num_lev
          do j = 1, num_lat-1
            do i = 1, num_lon-1
              work_2(i,j,k)  =      work_1(i,  j, k)*work_1(i,  j+1, k)*&
                                    work_1(i+1,j, k)*work_1(i+1,j+1, k)
            end do
              work_2(num_lon,j,k) = work_1(1,  j, k)*work_1(1,  j+1, k)*&
                                    work_1(num_lon,j,k)*work_1(num_lon,j+1,k)
          end do
            do i = 1, num_lon
              work_2(i,num_lat,k) = 0.0d0
            end do
        end do

            moc(1:num_lat,1:num_lev_half,l) = 0.0d0
            moc_temp(1:num_lat,1:num_lev) =0.0d0
        
        do k = 1, num_lev
          do j = 1, num_lat
            do i = 1, num_lon
              if (viv(i,j,k)==1)then
                moc_temp(j,k) = moc_temp(j,k) - work_2(i,j,k)*&
                                                (vs(i,j,k))*&
                                                dx_v(j)*dz(k)
              end if
            end do
          end do
        end do

        do j = 1, num_lat
          do k = 2, num_lev_half-1
            moc(j,k,l) = moc(j,k-1,l)-moc_temp(j,k-1)
          end do
        end do
        
        loop1:do j = 1, num_lat
            nta(j) = 0
        loop2: do k = 1, num_lev
                    do i = 1, num_lon
                        if (work_2(i,j,k).gt.0.5) then
                            nta(j) = nta(j) + 1
                            cycle loop2
                        end if
                    end do
        end do loop2
        end do loop1
        do j = 1, num_lat
            if (nta(j).lt.1) moc(j,k,l) = miss
        end do

        do j = 1, num_lat
        do k = 2, num_lev_half
            if (nta(j).lt.(k-1)) moc(j,k,l) = miss
        end do
        end do
    end do loop_l
    
    amoc = moc(:,:,1)
    
    global_moc = moc(:,:,3) 
    end subroutine moc_calc
    !---------------------------------------------------------------------
    !                      End of Subroutine moc_calc
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                  Subroutine licom_update_model_data
    !---------------------------------------------------------------------
    subroutine licom_update_month_data(data_in_file,var_name,var_num)
        integer, intent(in) :: var_num
        character(len = charlen), intent(in) :: data_in_file
        character(len = *), intent(in) :: var_name(var_num)
        
        
        if (any(var_name == "us")) then
            call read_3dvar(data_in_file,"us",us_month,[2,1,1],[num_lon,num_lat,num_lev],[num_lon,num_lat,num_lev],3) 
        end if

        if (any(var_name == "vs")) then
            call read_3dvar(data_in_file,"vs",vs_month,[2,1,1],[num_lon,num_lat,num_lev],[num_lon,num_lat,num_lev],3) 
        end if

        if (any(var_name == "ws")) then
            call read_3dvar(data_in_file,"ws",ws_month,[1,1,1],[num_lon,num_lat,num_lev],[num_lon,num_lat,num_lev],3) 
        end if

        
        if (any(var_name == "ss")) then
            call read_3dvar(data_in_file,"ss",ss_month,[1,1,1],[num_lon,num_lat,num_lev],[num_lon,num_lat,num_lev],3) 
        end if

        if (any(var_name == "ts")) then
            call read_3dvar(data_in_file,"ts",ts_month,[1,1,1],[num_lon,num_lat,num_lev],[num_lon,num_lat,num_lev],3) 
        end if

        if (any(var_name == "z0")) then
            call read_2dvar(data_in_file,"z0",z0_month,[1,1],[num_lon,num_lat],[num_lon,num_lat],2)
        end if

        if (any(var_name == "mld")) then
            call read_2dvar(data_in_file,"mld",mld_month,[1,1],[num_lon,num_lat],[num_lon,num_lat],2)
        end if
        
        if (any(var_name == "net1")) then
            call read_2dvar(data_in_file,"net1",net1_month,[1,1],[num_lon,num_lat],[num_lon,num_lat],2)
        end if
        
        if (any(var_name == "net2")) then
            call read_2dvar(data_in_file,"net2",net2_month,[1,1],[num_lon,num_lat],[num_lon,num_lat],2)
        end if

    end subroutine licom_update_month_data
    !---------------------------------------------------------------------
    !               End of Subroutine licom_update_model_data
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                   Subroutine update_all_month_data
    !---------------------------------------------------------------------
    subroutine update_all_month_data(all_month_file,num_all_month,var_name,var_num)
        implicit none
        integer, intent(in) :: num_all_month, var_num
        character(len = *), intent(in) :: all_month_file(num_all_month)
        character(len = *), intent(in) :: var_name(var_num)
        integer :: i, j, k

        us  = 0.0d0; vs = 0.0d0; ws = 0.0d0;
        ss  = 0.0d0; ts = 0.0d0; z0 = 0.0d0;
        mld = 0.0d0;net1= 0.0d0;net2= 0.0d0;

        do i = 1, num_all_month
            call licom_update_month_data(all_month_file(i),var_name,var_num)
            if (any(var_name == "us")) then
                where(us_month == miss) us = miss
                where(us_month /= miss) us = us + us_month/dble(num_all_month)
            end if
            if (any(var_name == "vs")) then
                where(vs_month == miss) vs = miss
                where(vs_month /= miss) vs = vs + vs_month/dble(num_all_month)
            end if
            if (any(var_name == "ws")) then
                where(ws_month == miss) ws = miss
                where(ws_month /= miss) ws = ws + ws_month/dble(num_all_month)
            end if
            if (any(var_name == "ss")) then
                where(ss_month == miss) ss = miss
                where(ss_month /= miss) ss = ss + ss_month/dble(num_all_month)
            end if
            if (any(var_name == "ts")) then
                where(ts_month == miss) ts = miss
                where(ts_month /= miss) ts = ts + ts_month/dble(num_all_month)
            end if
            if (any(var_name == "z0")) then
                where(z0_month == miss) z0 = miss
                where(z0_month /= miss) z0 = z0 + z0_month/dble(num_all_month)
            end if
            if (any(var_name == "mld")) then
                where(mld_month == miss) mld = miss
                where(mld_month /= miss) mld = mld + mld_month/dble(num_all_month)
            end if
            if (any(var_name == "net1")) then
                where(net1_month == miss) net1 = miss
                where(net1_month /= miss) net1 = net1 + net1_month/dble(num_all_month)
            end if
            if (any(var_name == "net2")) then
                where(net2_month == miss) net2 = miss
                where(net2_month /= miss) net2 = net2 + net2_month/dble(num_all_month)
            end if
        end do
    end subroutine update_all_month_data
    !---------------------------------------------------------------------
    !                   End of Subroutine update_all_month_data
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine update_one_year_data
    !---------------------------------------------------------------------
    subroutine update_one_year_data(one_year_file,var_name,var_num)
        implicit none
        integer, intent(in) :: var_num
        character(len = *), intent(in) :: one_year_file(12)
        character(len = *), intent(in) :: var_name(var_num)
        integer :: i, j, k

        us  = 0.0d0; vs = 0.0d0; ws = 0.0d0;
        ss  = 0.0d0; ts = 0.0d0; z0 = 0.0d0;
        mld = 0.0d0;net1= 0.0d0;net2= 0.0d0;

        do i = 1, 12
            call licom_update_month_data(one_year_file(i),var_name,var_num)
            if (any(var_name == "us")) then
                where(us_month == miss) us = miss
                where(us_month /= miss) us = us + us_month/dble(12)
            end if
            if (any(var_name == "vs")) then
                where(vs_month == miss) vs = miss
                where(vs_month /= miss) vs = vs + vs_month/dble(12)
            end if
            if (any(var_name == "ws")) then
                where(ws_month == miss) ws = miss
                where(ws_month /= miss) ws = ws + ws_month/dble(12)
            end if
            if (any(var_name == "ss")) then
                where(ss_month == miss) ss = miss
                where(ss_month /= miss) ss = ss + ss_month/dble(12)
            end if
            if (any(var_name == "ts")) then
                where(ts_month == miss) ts = miss
                where(ts_month /= miss) ts = ts + ts_month/dble(12)
            end if
            if (any(var_name == "z0")) then
                where(z0_month == miss) z0 = miss
                where(z0_month /= miss) z0 = z0 + z0_month/dble(12)
            end if
            if (any(var_name == "mld")) then
                where(mld_month == miss) mld = miss
                where(mld_month /= miss) mld = mld + mld_month/dble(12)
            end if
            if (any(var_name == "net1")) then
                where(net1_month == miss) net1 = miss
                where(net1_month /= miss) net1 = net1 + net1_month/dble(12)
            end if
            if (any(var_name == "net2")) then
                where(net2_month == miss) net2 = miss
                where(net2_month /= miss) net2 = net2 + net2_month/dble(12)
            end if
        end do
    
    end subroutine update_one_year_data
    !---------------------------------------------------------------------
    !                      End of Subroutine update_one_year_data
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                       Subroutine get_seasonal_cycle
    !---------------------------------------------------------------------
    subroutine get_seasonal_cycle(all_year_file,num_all_year,var_name)
        implicit none
        integer, intent(in) :: num_all_year
        character(len = *), intent(in) :: all_year_file(num_all_year,12)
        character(len = *), intent(in) :: var_name
        integer :: i, j, k, num_all_month

        num_all_month   =   num_all_year*12

        if (var_name == "ts") then
            allocate(ts_cycle(num_lon,num_lat,num_lev,12))
            allocate(ts_mean(num_lon,num_lat,num_lev))
            allocate(ts_anomaly(num_lon,num_lat,num_lev,12))
            ts_cycle = 0.0d0
            ts_mean  = 0.0d0
            do j = 1, 12
                do i = 1, num_all_year
                    call licom_update_month_data(all_year_file(i,j),["ts"],1)
                     where(ts_month /= miss) ts_cycle(:,:,:,j) = ts_cycle(:,:,:,j) + ts_month/dble(num_all_year)
                     where(ts_month == miss) ts_cycle(:,:,:,j) = miss
                     where(ts_month /= miss) ts_mean = ts_mean + ts_month/dble(num_all_month)        
                     where(ts_month == miss) ts_mean = miss
                end do
            end do
            
            do j = 1, 12
                where(ts_cycle(:,:,:,j) == miss) ts_anomaly(:,:,:,j) = miss
                where(ts_cycle(:,:,:,j) /= miss) ts_anomaly(:,:,:,j) = ts_cycle(:,:,:,j) - ts_mean
            end do
        end if
    end subroutine get_seasonal_cycle
    !---------------------------------------------------------------------
    !                   End of Subroutine get_seasonal_cycle
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine density
    !---------------------------------------------------------------------
      subroutine density()
        implicit none
        integer :: i, j, k
        real(r8) :: tq, sq
        
        where(vit/=1) pdens = miss 
        do k = 1, num_lev
            do j = 1, num_lat
                do i = 1, num_lon
                    if(vit(i,j,k)==1) then
                        tq = ts(i,j,k)-to(k)
                        sq = (ss(i,j,k)-35.0d0)/1.0d3-so(k)
                        pdens(i,j,k) = 1.0d3+po(k)+&
                                (C(k,1)+(C(k,4)+C(k,7)*SQ)*SQ &
                               +(C(k,3)+C(k,8)*SQ+C(k,6)*TQ)*TQ)*TQ &
                               +(C(k,2)+(C(k,5)+C(k,9)*SQ)*SQ)*SQ
                    end if
                end do
            end do
        end do
        
      end subroutine density
    !---------------------------------------------------------------------
    !                      End of Subroutine density
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                 Subroutine meso_scale_eddy_transports
    !---------------------------------------------------------------------
      subroutine meso_scale_eddy_transports()
        implicit none
        integer :: i, j, k
        real(r8) :: rho_up, rho_mid, rho_down
        real(r8) :: rho_dz_up, rho_dz_down
        real(r8) :: rho_dy_up, rho_dy_mid, rho_dy_down
        real(r8) :: iso_velocity_y
        real(r8) :: s_up, s_down, s_max
        !vit(i,j)   vit(i+1,j)
        !       viv(i,j)
        !vit(i,j+1) vit(i+1,j+1)
        !print *,lev_half
    !    v(k-1)    
    !    w(k)
    !    v(k)
    !    w(k+1)
    !    v(k+1)
        s_max  = 2.d-4
        us_iso = 0.0d0
        vs_iso = 0.0d0
 loop1: do k = 1, num_lev
        do j = 1, num_lat
        do i = 1, num_lon
            if(viv_iso(i,j,k)==1) then 
            rho_up      =   pdens(i,j,  k-1)+pdens(i+1,j,  k-1)+&
                            pdens(i,j+1,k-1)+pdens(i+1,j+1,k-1)
            rho_mid     =   pdens(i,j,  k  )+pdens(i+1,j,  k  )+&
                            pdens(i,j+1,k  )+pdens(i+1,j+1,k  )
            rho_down    =   pdens(i,j,  k+1)+pdens(i+1,j,  k+1)+&
                            pdens(i,j+1,k+1)+pdens(i+1,j+1,k+1)
            rho_up      =   rho_up/4.0d0
            rho_mid     =   rho_mid/4.0d0
            rho_down    =   rho_down/4.0d0
            
            rho_dz_up   =   rho_up-rho_mid
            rho_dz_down =   rho_mid-rho_down
            rho_dz_up   =   rho_dz_up/dz_w(k)
            rho_dz_down =   rho_dz_down/dz_w(k+1)
            
            rho_dy_up   =   pdens(i,j,  k-1)+pdens(i+1,j,  k-1)-&
                            pdens(i,j+1,k-1)-pdens(i+1,j+1,k-1)
            rho_dy_mid  =   pdens(i,j,  k  )+pdens(i+1,j,  k  )-&
                            pdens(i,j+1,k  )-pdens(i+1,j+1,k  )
            rho_dy_down =   pdens(i,j,  k+1)+pdens(i+1,j,  k+1)-&
                            pdens(i,j+1,k+1)-pdens(i+1,j+1,k+1)
            
            rho_dy_up   =   rho_dy_up/dy_v(j)/2.0d0
            rho_dy_mid  =   rho_dy_mid/dy_v(j)/2.0d0
            rho_dy_down =   rho_dy_down/dy_v(j)/2.0d0

            rho_dy_up   =   rho_dy_up*dz(k)+rho_dy_mid*dz(k-1)/(dz(k-1)+dz(k))
            rho_dy_down =   rho_dy_down*dz(k)+rho_dy_mid*dz(k+1)/(dz(k)+dz(k+1))
            
            s_up        =   dsign(dmin1(dabs(rho_dy_up/rho_dz_up),s_max),rho_dy_up/rho_dz_up)
            s_down      =   dsign(dmin1(dabs(rho_dy_down/rho_dz_down),s_max),rho_dy_down/rho_dz_down)
            iso_velocity_y  =   s_up-s_down
            iso_velocity_y  =   1.d3*iso_velocity_y/dz(k)
            vs_iso(i,j,k)   =   iso_velocity_y
            end if
        end do
        end do
        end do loop1
      end subroutine meso_scale_eddy_transports
    !---------------------------------------------------------------------
    !            End of Subroutine meso_scale_eddy_transports
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !           Subroutine global_averaged_sea_surface_temperature
    !---------------------------------------------------------------------
    subroutine global_averaged_sea_surface_temperature()
        implicit none
        integer :: i, j, k
        real(r8) :: global_sst_area

        global_sst  =   0.0d0
        global_sst_area = 0.0d0
     
      
        do j = 1, num_lat
            do i = 1, num_lon
                if (vit(i,j,1) == 1) then
                    global_sst  =   global_sst + ts(i,j,1)*dx_t(j)*dy_t(j)
                    global_sst_area = global_sst_area + dx_t(j)*dy_t(j)
                end if
            end do
        end do
        
        global_sst  =   global_sst/global_sst_area
    end subroutine global_averaged_sea_surface_temperature
    !---------------------------------------------------------------------
    !       End of Subroutine global_averaged_sea_surface_temperature
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !           Subroutine global_averaged_sea_surface_salinity
    !---------------------------------------------------------------------
    subroutine global_averaged_sea_surface_salinity()
        implicit none
        integer :: i, j, k
        real(r8) :: global_sss_area

        global_sss = 0.0d0
        global_sss_area = 0.0d0

        do j = 1, num_lat
            do i = 1, num_lon
                if (vit(i,j,1) == 1) then
                    global_sss  =   global_sss + ss(i,j,1)*dx_t(j)*dy_t(j)
                    global_sss_area =   global_sss_area + dx_t(j)*dy_t(j)
                end if
            end do
        end do

        global_sss  =   global_sss/global_sss_area
    end subroutine global_averaged_sea_surface_salinity
    !---------------------------------------------------------------------
    !      End of Subroutine global_averaged_sea_surface_salinity
    !---------------------------------------------------------------------
end module diagnostics_mod 
