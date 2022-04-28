      module modal_aero_data

!--------------------------------------------------------------
! ... Basic aerosol mode parameters and arrays
!--------------------------------------------------------------
      use shr_kind_mod,    only: r8 => shr_kind_r8
      use constituents,    only: pcnst
      use radconstants,    only: nswbands, nlwbands

      implicit none
      save
      
      integer, parameter :: ntot_amode = 0 
   
      integer, private :: qqcw(pcnst)=-1 ! Remaps modal_aero indices into pbuf

      contains

        subroutine qqcw_set_ptr(index, iptr)
          use cam_abortutils, only : endrun
          

          integer, intent(in) :: index, iptr

          if(index>0 .and. index <= pcnst ) then
             qqcw(index)=iptr
          else
             call endrun('qqcw_set_ptr: attempting to set qqcw pointer already defined')
          end if
        end subroutine qqcw_set_ptr

!--------------------------------------------------------------
!--------------------------------------------------------------
        function qqcw_get_field(pbuf, index, lchnk, errorhandle)
          use cam_abortutils, only : endrun
          use physics_buffer, only : physics_buffer_desc, pbuf_get_field

          integer, intent(in) :: index, lchnk
          real(r8), pointer :: qqcw_get_field(:,:)
          logical, optional :: errorhandle
          type(physics_buffer_desc), pointer :: pbuf(:)

          logical :: error

          nullify(qqcw_get_field)
          error = .false.
          if (index>0 .and. index <= pcnst) then
             if (qqcw(index)>0) then 
                call pbuf_get_field(pbuf, qqcw(index), qqcw_get_field)
             else
                error = .true.
             endif
          else
             error = .true.             
          end if

          if (error .and. .not. present(errorhandle)) then
             call endrun('qqcw_get_field: attempt to access undefined qqcw')
          end if

        end function qqcw_get_field

      end module modal_aero_data

