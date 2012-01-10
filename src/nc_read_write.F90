#include "defines.h"
module nc_read_write
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
    use utils
    use parameters
    
    contains
    !---------------------------------------------------------------------
    !                          Subroutine add_dim2nc_int
    !---------------------------------------------------------------------
    subroutine add_dim2nc_int(file_name,dim_name,long_name,units,num_dim,dim_var)
        implicit none
        character(len = *), intent(in) :: file_name, dim_name
        character(len = *), intent(in) :: long_name, units
        integer, intent(in) :: num_dim
        integer, intent(in) :: dim_var(num_dim)
        integer :: ierr, ncid, dim_dimid, dim_varid
        
        ierr = nf90_open(file_name, nf90_write, ncid)
        call handle_err(ierr)

        ierr = nf90_redef(ncid)
        call handle_err(ierr)

        ierr = nf90_def_dim(ncid,dim_name,num_dim,dim_dimid)
        call handle_err(ierr)

        ierr = nf90_def_var(ncid,dim_name,nf90_int,dim_dimid,dim_varid)
        call handle_err(ierr)
        
        ierr = nf90_put_att(ncid,dim_varid,"long_name",long_name)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dim_varid,"units",units)
        call handle_err(ierr)

        ierr = nf90_enddef(ncid)
        call handle_err(ierr)

        ierr = nf90_put_var(ncid,dim_varid,dim_var)
        call handle_err(ierr)
        
        ierr = nf90_close(ncid)
        call handle_err(ierr)
    
    end subroutine add_dim2nc_int
    !---------------------------------------------------------------------
    !                      End of Subroutine add_dim2nc_int
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_2dvar_int
    !---------------------------------------------------------------------
    subroutine read_2dvar_int(file_name,var_name,var,start_count,volume_count,dim_count,n_d)
        implicit none
        integer, intent(in) :: n_d
        integer, intent(in) :: start_count(n_d), volume_count(n_d), dim_count(n_d)
        character(len = *), intent(in) :: file_name, var_name
        integer :: ierr, ncid, var_id, i, j, k

        integer, allocatable, intent(out) :: var(:,:)
        allocate(var(dim_count(1),dim_count(2)))
        
        call check_file_exist(file_name)
        
        ierr = nf90_open(file_name, nf90_nowrite, ncid)
        call handle_err(ierr)

        ierr = nf90_inq_varid(ncid,var_name,var_id) 
        call handle_err(ierr)

        ierr = nf90_get_var(ncid,var_id,var,start_count,volume_count)
        call handle_err(ierr)

        ierr = nf90_close(ncid)
        call handle_err(ierr)
    
    end subroutine read_2dvar_int
    !---------------------------------------------------------------------
    !                      End of Subroutine read_2dvar_int
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !              Subroutine get_missing_from_nc
    !---------------------------------------------------------------------
    subroutine get_missing_from_nc(file_name,miss,varname)
        implicit none
        character(len = *), intent(in) :: file_name, varname
        real(r8), intent(out) :: miss
        integer :: ncid, varid, ierr

        ierr = nf90_open(file_name,nf90_nowrite,ncid)
        call handle_err(ierr)

        ierr = nf90_inq_varid(ncid,varname,varid)
        call handle_err(ierr)

        ierr = nf90_get_att(ncid,varid,"_FillValue",miss)
        call handle_err(ierr)

        ierr = nf90_close(ncid)
        call handle_err(ierr)
    end subroutine get_missing_from_nc
    !---------------------------------------------------------------------
    !              End of Subroutine get_missing_from_nc
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_1dvar
    !---------------------------------------------------------------------
    subroutine read_1dvar(file_name,var_name,var,start_count,volume_count,dim_count,n_d)
        implicit none
        integer, intent(in) :: n_d
        integer, intent(in) :: start_count(n_d), volume_count(n_d), dim_count(n_d)
        character(len = *), intent(in) :: file_name, var_name
        integer :: ierr, ncid, var_id, i, j, k

        real(r8), allocatable, intent(out) :: var(:)
        allocate(var(dim_count(1)))
        
        call check_file_exist(file_name)
        
        ierr = nf90_open(file_name, nf90_nowrite, ncid)
        call handle_err(ierr)

        ierr = nf90_inq_varid(ncid,var_name,var_id) 
        call handle_err(ierr)

        ierr = nf90_get_var(ncid,var_id,var,start_count,volume_count)
        call handle_err(ierr)

        ierr = nf90_close(ncid)
        call handle_err(ierr)
    end subroutine read_1dvar
    !---------------------------------------------------------------------
    !                      End of Subroutine read_1dvar
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_2dvar
    !---------------------------------------------------------------------
    subroutine read_2dvar(file_name,var_name,var,start_count,volume_count,dim_count,n_d)
        implicit none
        integer, intent(in) :: n_d
        integer, intent(in) :: start_count(n_d), volume_count(n_d), dim_count(n_d)
        character(len = *), intent(in) :: file_name, var_name
        integer :: ierr, ncid, var_id, i, j, k

        real(r8), allocatable, intent(out) :: var(:,:)
        allocate(var(dim_count(1),dim_count(2)))
        
        call check_file_exist(file_name)
        
        ierr = nf90_open(file_name, nf90_nowrite, ncid)
        call handle_err(ierr)

        ierr = nf90_inq_varid(ncid,var_name,var_id) 
        call handle_err(ierr)

        ierr = nf90_get_var(ncid,var_id,var,start_count,volume_count)
        call handle_err(ierr)

        ierr = nf90_close(ncid)
        call handle_err(ierr)
    
    end subroutine read_2dvar
    !---------------------------------------------------------------------
    !                      End of Subroutine read_2dvar
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_3dvar
    !---------------------------------------------------------------------
    subroutine read_3dvar(file_name,var_name,var,start_count,volume_count,dim_count,n_d)
        implicit none
        integer, intent(in) :: n_d
        integer, intent(in) :: start_count(n_d), volume_count(n_d), dim_count(n_d)
        character(len = *), intent(in) :: file_name, var_name
        integer :: ierr, ncid, var_id, i, j, k

        real(r8), allocatable, intent(out) :: var(:,:,:)
        allocate(var(dim_count(1),dim_count(2),dim_count(3)))
        
        call check_file_exist(file_name)
        
        ierr = nf90_open(file_name, nf90_nowrite, ncid)
        call handle_err(ierr)

        ierr = nf90_inq_varid(ncid,var_name,var_id) 
        call handle_err(ierr)

        ierr = nf90_get_var(ncid,var_id,var,start_count,volume_count)
        call handle_err(ierr)

        ierr = nf90_close(ncid)
        call handle_err(ierr)
     
    end subroutine read_3dvar
    !---------------------------------------------------------------------
    !                      End of Subroutine read_3dvar
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_4dvar
    !---------------------------------------------------------------------
    subroutine read_4dvar(file_name,var_name,var,start_count,volume_count,dim_count,n_d)
        implicit none
        integer, intent(in) :: n_d
        integer, intent(in) :: start_count(n_d), volume_count(n_d), dim_count(n_d)
        character(len = *), intent(in) :: file_name, var_name
        integer :: ierr, ncid, var_id, i, j, k

        real(r8), allocatable, intent(out) :: var(:,:,:,:)
        allocate(var(dim_count(1),dim_count(2),dim_count(3),dim_count(4)))
        
        call check_file_exist(file_name)
        
        ierr = nf90_open(file_name, nf90_nowrite, ncid)
        call handle_err(ierr)

        ierr = nf90_inq_varid(ncid,var_name,var_id) 
        call handle_err(ierr)

        ierr = nf90_get_var(ncid,var_id,var,start_count,volume_count)
        call handle_err(ierr)

        ierr = nf90_close(ncid)
        call handle_err(ierr)
    
    end subroutine read_4dvar
    !---------------------------------------------------------------------
    !                      End of Subroutine read_4dvar
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_5dvar
    !---------------------------------------------------------------------
    subroutine read_5dvar(file_name,var_name,var,start_count,volume_count,dim_count,n_d)
        implicit none
        integer, intent(in) :: n_d
        integer, intent(in) :: start_count(n_d), volume_count(n_d), dim_count(n_d)
        character(len = *), intent(in) :: file_name, var_name
        integer :: ierr, ncid, var_id, i, j, k

        real(r8), allocatable, intent(out) :: var(:,:,:,:,:)
        allocate(var(dim_count(1),dim_count(2),dim_count(3),dim_count(4),dim_count(5)))
        
        call check_file_exist(file_name)
        
        ierr = nf90_open(file_name, nf90_nowrite, ncid)
        call handle_err(ierr)

        ierr = nf90_inq_varid(ncid,var_name,var_id) 
        call handle_err(ierr)

        ierr = nf90_get_var(ncid,var_id,var,start_count,volume_count)
        call handle_err(ierr)

        ierr = nf90_close(ncid)
        call handle_err(ierr)
    
    end subroutine read_5dvar
    !---------------------------------------------------------------------
    !                      End of Subroutine read_5dvar
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine create_nc
    !---------------------------------------------------------------------
    subroutine create_nc(file_name)
        implicit none
        character(len = *), intent(in) :: file_name
        logical :: alive
        integer :: ncid, ierr, i, j, k

        call check_file_exist(file_name)
        ierr = nf90_create(file_name, nf90_noclobber,ncid)
        call handle_err(ierr)
        ierr = nf90_close(ncid)
        call handle_err(ierr)
    end subroutine create_nc
    !---------------------------------------------------------------------
    !                      End of Subroutine create_nc
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine add_dim2nc
    !---------------------------------------------------------------------
    subroutine add_dim2nc(file_name,dim_name,long_name,units,num_dim,dim_var)
        implicit none
        character(len = *), intent(in) :: file_name, dim_name
        character(len = *), intent(in) :: long_name, units
        integer, intent(in) :: num_dim
        real(r8), intent(in) :: dim_var(num_dim)
        integer :: ierr, ncid, dim_dimid, dim_varid
        
        ierr = nf90_open(file_name, nf90_write, ncid)
        call handle_err(ierr)

        ierr = nf90_redef(ncid)
        call handle_err(ierr)

        ierr = nf90_def_dim(ncid,dim_name,num_dim,dim_dimid)
        call handle_err(ierr)

        ierr = nf90_def_var(ncid,dim_name,nf90_double,dim_dimid,dim_varid)
        call handle_err(ierr)
        
        ierr = nf90_put_att(ncid,dim_varid,"long_name",long_name)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dim_varid,"units",units)
        call handle_err(ierr)

        ierr = nf90_enddef(ncid)
        call handle_err(ierr)

        ierr = nf90_put_var(ncid,dim_varid,dim_var)
        call handle_err(ierr)
        
        ierr = nf90_close(ncid)
        call handle_err(ierr)


    end subroutine add_dim2nc
    !---------------------------------------------------------------------
    !                      End of Subroutine add_dim2nc
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine add_1dvar2nc
    !---------------------------------------------------------------------
    subroutine add_1dvar2nc(file_name,dim_name,dvar_name,long_name,units,Fill_Value,dvar,dim_count,n_d)
        implicit none
        integer, intent(in) :: n_d
        character(len = *), intent(in) :: file_name, dim_name(n_d), dvar_name, long_name, units
        real(r8), intent(in) :: Fill_Value
        integer, intent(in) :: dim_count(n_d)
        integer :: ierr, ncid, dim_dimid(n_d), dvar_id
        integer :: i, j, k
        real(r8), intent(in) :: dvar(dim_count(1))

        ierr = nf90_open(file_name, nf90_write, ncid)
        call handle_err(ierr)

        ierr = nf90_redef(ncid)
        call handle_err(ierr)
        do k = 1, n_d
            ierr = nf90_inq_dimid(ncid, dim_name(k), dim_dimid(k))
            call handle_err(ierr)
        end do

        ierr = nf90_def_var(ncid,dvar_name,nf90_double,(/dim_dimid/),dvar_id)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"long_name",long_name)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"units",units)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"_FillValue",Fill_Value)
        call handle_err(ierr)

        ierr = nf90_enddef(ncid)
        call handle_err(ierr)

        ierr = nf90_put_var(ncid,dvar_id,dvar)
        call handle_err(ierr)

        ierr = nf90_close(ncid)
        call handle_err(ierr)

    end subroutine add_1dvar2nc
    !---------------------------------------------------------------------
    !                      End of Subroutine add_1dvar2nc
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine add_2dvar2nc
    !---------------------------------------------------------------------
    subroutine add_2dvar2nc(file_name,dim_name,dvar_name,long_name,units,Fill_Value,dvar,dim_count,n_d)
        implicit none
        integer, intent(in) :: n_d
        character(len = *), intent(in) :: file_name, dim_name(n_d), dvar_name, long_name, units
        real(r8), intent(in) :: Fill_Value
        integer, intent(in) :: dim_count(n_d)
        integer :: ierr, ncid, dim_dimid(n_d), dvar_id
        integer :: i, j, k
        real(r8), intent(in) :: dvar(dim_count(1),dim_count(2))

        ierr = nf90_open(file_name, nf90_write, ncid)
        call handle_err(ierr)

        ierr = nf90_redef(ncid)
        call handle_err(ierr)
        do k = 1, n_d
            ierr = nf90_inq_dimid(ncid, dim_name(k), dim_dimid(k))
            call handle_err(ierr)
        end do
        ierr = nf90_def_var(ncid,dvar_name,nf90_double,(/dim_dimid/),dvar_id)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"long_name",long_name)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"units",units)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"_FillValue",Fill_Value)
        call handle_err(ierr)

        ierr = nf90_enddef(ncid)
        call handle_err(ierr)

        ierr = nf90_put_var(ncid,dvar_id,dvar)
        call handle_err(ierr)

        ierr = nf90_close(ncid)
        call handle_err(ierr)

    
    end subroutine add_2dvar2nc
    !---------------------------------------------------------------------
    !                      End of Subroutine add_2dvar2nc
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine add_3dvar2nc
    !---------------------------------------------------------------------
    subroutine add_3dvar2nc(file_name,dim_name,dvar_name,long_name,units,Fill_Value,dvar,dim_count,n_d)
        implicit none
        integer, intent(in) :: n_d
        character(len = *), intent(in) :: file_name, dim_name(n_d), dvar_name, long_name, units
        real(r8), intent(in) :: Fill_Value
        integer, intent(in) :: dim_count(n_d)
        integer :: ierr, ncid, dim_dimid(n_d), dvar_id
        integer :: i, j, k
        real(r8), intent(in) :: dvar(dim_count(1),dim_count(2),dim_count(3))

        ierr = nf90_open(file_name, nf90_write, ncid)
        call handle_err(ierr)

        ierr = nf90_redef(ncid)
        call handle_err(ierr)
        do k = 1, n_d
            ierr = nf90_inq_dimid(ncid, dim_name(k), dim_dimid(k))
            call handle_err(ierr)
        end do

        ierr = nf90_def_var(ncid,dvar_name,nf90_double,(/dim_dimid/),dvar_id)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"long_name",long_name)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"units",units)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"_FillValue",Fill_Value)
        call handle_err(ierr)

        ierr = nf90_enddef(ncid)
        call handle_err(ierr)

        ierr = nf90_put_var(ncid,dvar_id,dvar)
        call handle_err(ierr)

        ierr = nf90_close(ncid)
        call handle_err(ierr)

    
    end subroutine add_3dvar2nc
    !---------------------------------------------------------------------
    !                      End of Subroutine add_3dvar2nc
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine add_4dvar2nc
    !---------------------------------------------------------------------
    subroutine add_4dvar2nc(file_name,dim_name,dvar_name,long_name,units,Fill_Value,dvar,dim_count,n_d)
        implicit none
        integer, intent(in) :: n_d
        character(len = *), intent(in) :: file_name, dim_name(n_d), dvar_name, long_name, units
        real(r8), intent(in) :: Fill_Value
        integer, intent(in) :: dim_count(n_d)
        integer :: ierr, ncid, dim_dimid(n_d), dvar_id
        integer :: i, j, k
        real(r8), intent(in) :: dvar(dim_count(1),dim_count(2),dim_count(3),dim_count(4))

        ierr = nf90_open(file_name, nf90_write, ncid)
        call handle_err(ierr)

        ierr = nf90_redef(ncid)
        call handle_err(ierr)
        do k = 1, n_d
            ierr = nf90_inq_dimid(ncid, dim_name(k), dim_dimid(k))
            call handle_err(ierr)
        end do

        ierr = nf90_def_var(ncid,dvar_name,nf90_double,(/dim_dimid/),dvar_id)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"long_name",long_name)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"units",units)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"_FillValue",Fill_Value)
        call handle_err(ierr)

        ierr = nf90_enddef(ncid)
        call handle_err(ierr)

        ierr = nf90_put_var(ncid,dvar_id,dvar)
        call handle_err(ierr)

        ierr = nf90_close(ncid)
        call handle_err(ierr)

    
    end subroutine add_4dvar2nc
    !---------------------------------------------------------------------
    !                      End of Subroutine add_4dvar2nc
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine add_5dvar2nc
    !---------------------------------------------------------------------
    subroutine add_5dvar2nc(file_name,dim_name,dvar_name,long_name,units,Fill_Value,dvar,dim_count,n_d)
        implicit none
        integer, intent(in) :: n_d
        character(len = *), intent(in) :: file_name, dim_name(n_d), dvar_name, long_name, units
        real(r8), intent(in) :: Fill_Value
        integer, intent(in) :: dim_count(n_d)
        integer :: ierr, ncid, dim_dimid(n_d), dvar_id
        integer :: i, j, k
        real(r8), intent(in) :: dvar(dim_count(1),dim_count(2),dim_count(3),dim_count(4),dim_count(5))

        ierr = nf90_open(file_name, nf90_write, ncid)
        call handle_err(ierr)

        ierr = nf90_redef(ncid)
        call handle_err(ierr)
        do k = 1, n_d
            ierr = nf90_inq_dimid(ncid, dim_name(k), dim_dimid(k))
            call handle_err(ierr)
        end do

        ierr = nf90_def_var(ncid,dvar_name,nf90_double,(/dim_dimid/),dvar_id)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"long_name",long_name)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"units",units)
        call handle_err(ierr)

        ierr = nf90_put_att(ncid,dvar_id,"_FillValue",Fill_Value)
        call handle_err(ierr)

        ierr = nf90_enddef(ncid)
        call handle_err(ierr)

        ierr = nf90_put_var(ncid,dvar_id,dvar)
        call handle_err(ierr)

        ierr = nf90_close(ncid)
        call handle_err(ierr)

    
    end subroutine add_5dvar2nc
    !---------------------------------------------------------------------
    !                      End of Subroutine add_5dvar2nc
    !---------------------------------------------------------------------
    
end module nc_read_write
