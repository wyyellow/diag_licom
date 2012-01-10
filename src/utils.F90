#include "defines.h"
module utils
! Purpose:
! This module is devised for report errors, show in-format message,
!   handle errors for the netcdf-f90 interface, et al.
!
! Record of revisions:
!    Date             Programmer                Description of change  
!==============    ===================   =============================
! 2011/12/22         WenYu Huang                Original Code
! ...
!
!Any comments please send to huangwenyu@mail.tsinghua.edu.cn
  use netcdf
  contains

    !---------------------------------------------------------------------
    !                       Subroutine handle_err
    !---------------------------------------------------------------------
      subroutine handle_err(ierr)
        integer, intent(in) :: ierr

        if(ierr /= nf90_noerr) then
          write (6,*) trim(nf90_strerror(ierr))
        stop "Stopped"
        end if
      end subroutine handle_err
    !---------------------------------------------------------------------
    !                   End of Subroutine handle_err
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine check_file_exist
    !---------------------------------------------------------------------
    subroutine check_file_exist(file_name)
        implicit none
        character(len = charlen) :: file_name
        logical alive

        inquire(file = file_name, exist = alive)
        if (.not. alive) then
            write(*,*) "File "//trim(file_name)//" not existed!"
        end if
    end subroutine check_file_exist
    !---------------------------------------------------------------------
    !                      End of Subroutine check_file_exist
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                    Subroutine check_file_not_exist
    !---------------------------------------------------------------------
    subroutine check_file_not_exist()
        implicit none
        character(len = charlen) :: file_name
        logical alive
        inquire(file = file_name, exist = alive)
        if (alive) then
            write(*,*) "File "//trim(file_name)//" existed!"
        end if
    end subroutine check_file_not_exist
    !---------------------------------------------------------------------
    !                End of Subroutine check_file_not_exist
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                       Subroutine file_delete
    !---------------------------------------------------------------------
    subroutine file_delete(file_name)
        implicit none
        character(len = charlen) :: file_name
        logical alive
        inquire(file = file_name, exist = alive)
        if (alive) then
            open(11,file=file_name)
            close(11,status='delete')
        end if
    end subroutine file_delete
    !---------------------------------------------------------------------
    !                   End of Subroutine file_delete
    !---------------------------------------------------------------------

end module utils
