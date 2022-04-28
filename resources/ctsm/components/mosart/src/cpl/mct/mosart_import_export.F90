module mosart_import_export

  use shr_kind_mod        , only : r8 => shr_kind_r8, cl=>shr_kind_cl
  use shr_sys_mod         , only : shr_sys_abort
  use mosart_cpl_indices  , only : index_x2r_Flrl_rofsur, index_x2r_Flrl_rofi
  use mosart_cpl_indices  , only : index_x2r_Flrl_rofgwl, index_x2r_Flrl_rofsub
  use mosart_cpl_indices  , only : index_x2r_Flrl_irrig
  use mosart_cpl_indices  , only : index_r2x_Forr_rofl, index_r2x_Forr_rofi
  use mosart_cpl_indices  , only : index_r2x_Flrr_flood
  use mosart_cpl_indices  , only : index_r2x_Flrr_volr, index_r2x_Flrr_volrmch
  use RunoffMod           , only : rtmCTL, TRunoff
  use RtmVar              , only : iulog, ice_runoff, nt_rtm, rtm_tracers
  use RtmSpmd             , only : masterproc, iam
  use RtmTimeManager      , only : get_nstep

  implicit none

  private ! except

  public :: mosart_import
  public :: mosart_export

  integer     ,parameter :: debug = 1 ! internal debug level
  character(*),parameter :: F01 = "('(mosart_import_export) ',a,i5,2x,i8,2x,d21.14)"

!===============================================================================
contains
!===============================================================================

  subroutine mosart_import( x2r )

    !---------------------------------------------------------------------------
    ! Obtain the runoff input from the coupler
    ! convert from kg/m2s to m3/s
    !
    ! Arguments:
    real(r8), intent(in) :: x2r(:,:)     ! driver import state to mosart
    !
    ! Local variables
    integer :: n2, n, nt, begr, endr, nliq, nfrz
    character(len=32), parameter :: sub = 'mosart_import'
    !---------------------------------------------------------------------------

    ! Note that ***runin*** are fluxes

    nliq = 0
    nfrz = 0
    do nt = 1,nt_rtm
       if (trim(rtm_tracers(nt)) == 'LIQ') then
          nliq = nt
       endif
       if (trim(rtm_tracers(nt)) == 'ICE') then
          nfrz = nt
       endif
    enddo
    if (nliq == 0 .or. nfrz == 0) then
       write(iulog,*) trim(sub),': ERROR in rtm_tracers LIQ ICE ',nliq,nfrz,rtm_tracers
       call shr_sys_abort()
    endif

    begr = rtmCTL%begr
    endr = rtmCTL%endr
    do n = begr,endr
       n2 = n - begr + 1

       rtmCTL%qsur(n,nliq) = x2r(index_x2r_Flrl_rofsur,n2) * (rtmCTL%area(n)*0.001_r8)
       rtmCTL%qsub(n,nliq) = x2r(index_x2r_Flrl_rofsub,n2) * (rtmCTL%area(n)*0.001_r8)
       rtmCTL%qgwl(n,nliq) = x2r(index_x2r_Flrl_rofgwl,n2) * (rtmCTL%area(n)*0.001_r8)

       rtmCTL%qsur(n,nfrz) = x2r(index_x2r_Flrl_rofi ,n2) * (rtmCTL%area(n)*0.001_r8)
       rtmCTL%qirrig(n)    = x2r(index_x2r_Flrl_irrig,n2) * (rtmCTL%area(n)*0.001_r8)

       rtmCTL%qsub(n,nfrz) = 0.0_r8
       rtmCTL%qgwl(n,nfrz) = 0.0_r8
    enddo

    if (debug > 0 .and. masterproc .and. get_nstep() < 5) then
       do n = begr,endr
          write(iulog,F01)'import: nstep, n, Flrl_rofsur = ',get_nstep(),n,rtmCTL%qsur(n,nliq)
          write(iulog,F01)'import: nstep, n, Flrl_rofsub = ',get_nstep(),n,rtmCTL%qsub(n,nliq)
          write(iulog,F01)'import: nstep, n, Flrl_rofgwl = ',get_nstep(),n,rtmCTL%qgwl(n,nliq)
          write(iulog,F01)'import: nstep, n, Flrl_rofi   = ',get_nstep(),n,rtmCTL%qsur(n,nfrz)
          write(iulog,F01)'import: nstep, n, Flrl_irrig  = ',get_nstep(),n,rtmCTL%qirrig(n)
       end do
    end if

  end subroutine mosart_import

  !====================================================================================

  subroutine mosart_export( r2x )

    !---------------------------------------------------------------------------
    ! Send the runoff model export state to the coupler
    ! convert from m3/s to kg/m2s
    !
    ! Arguments:
    real(r8), intent(out) :: r2x(:,:)     ! mosart export state to driver
    !
    ! Local variables
    integer :: ni, n, nt, nliq, nfrz
    logical,save :: first_time = .true.
    character(len=32), parameter :: sub = 'mosart_export'
    !---------------------------------------------------------------------------

    nliq = 0
    nfrz = 0
    do nt = 1,nt_rtm
       if (trim(rtm_tracers(nt)) == 'LIQ') then
          nliq = nt
       endif
       if (trim(rtm_tracers(nt)) == 'ICE') then
          nfrz = nt
       endif
    enddo
    if (nliq == 0 .or. nfrz == 0) then
       write(iulog,*) trim(sub),': ERROR in rtm_tracers LIQ ICE ',nliq,nfrz,rtm_tracers
       call shr_sys_abort()
    endif

    r2x(:,:) = 0._r8

    if (first_time) then
       if (masterproc) then
          if ( ice_runoff )then
             write(iulog,*)'Snow capping will flow out in frozen river runoff'
          else
             write(iulog,*)'Snow capping will flow out in liquid river runoff'
          endif
       endif
       first_time = .false.
    end if

    ni = 0
    if ( ice_runoff )then
       ! separate liquid and ice runoff
       do n = rtmCTL%begr,rtmCTL%endr
          ni = ni + 1
          r2x(index_r2x_Forr_rofl,ni) =  rtmCTL%direct(n,nliq) / (rtmCTL%area(n)*0.001_r8)
          r2x(index_r2x_Forr_rofi,ni) =  rtmCTL%direct(n,nfrz) / (rtmCTL%area(n)*0.001_r8)
          if (rtmCTL%mask(n) >= 2) then
             ! liquid and ice runoff are treated separately - this is what goes to the ocean
             r2x(index_r2x_Forr_rofl,ni) = r2x(index_r2x_Forr_rofl,ni) + rtmCTL%runoff(n,nliq) / (rtmCTL%area(n)*0.001_r8)
             r2x(index_r2x_Forr_rofi,ni) = r2x(index_r2x_Forr_rofi,ni) + rtmCTL%runoff(n,nfrz) / (rtmCTL%area(n)*0.001_r8)
             if (ni > rtmCTL%lnumr) then
                write(iulog,*) sub, ' : ERROR runoff count',n,ni
                call shr_sys_abort( sub//' : ERROR runoff > expected' )
             endif
          endif
       end do
    else
       ! liquid and ice runoff added to liquid runoff, ice runoff is zero
       do n = rtmCTL%begr,rtmCTL%endr
          ni = ni + 1
          r2x(index_r2x_Forr_rofl,ni) = (rtmCTL%direct(n,nfrz)+rtmCTL%direct(n,nliq)) / (rtmCTL%area(n)*0.001_r8)
          if (rtmCTL%mask(n) >= 2) then
             r2x(index_r2x_Forr_rofl,ni) = r2x(index_r2x_Forr_rofl,ni) + &
                  (rtmCTL%runoff(n,nfrz)+rtmCTL%runoff(n,nliq)) / (rtmCTL%area(n)*0.001_r8)
             if (ni > rtmCTL%lnumr) then
                write(iulog,*) sub, ' : ERROR runoff count',n,ni
                call shr_sys_abort( sub//' : ERROR runoff > expected' )
             endif
          endif
       end do
    end if

    ! Flooding back to land, sign convention is positive in land->rof direction
    ! so if water is sent from rof to land, the flux must be negative.
    ni = 0
    do n = rtmCTL%begr, rtmCTL%endr
       ni = ni + 1
       r2x(index_r2x_Flrr_flood,ni)   = -rtmCTL%flood(n) / (rtmCTL%area(n)*0.001_r8)
       !scs: is there a reason for the wr+wt rather than volr (wr+wt+wh)?
       !r2x(index_r2x_Flrr_volr,ni)    = (Trunoff%wr(n,nliq) + Trunoff%wt(n,nliq)) / rtmCTL%area(n)

       r2x(index_r2x_Flrr_volr,ni)    = rtmCTL%volr(n,nliq)/ rtmCTL%area(n)
       r2x(index_r2x_Flrr_volrmch,ni) = Trunoff%wr(n,nliq) / rtmCTL%area(n)
    end do

    if (debug > 0 .and. masterproc .and. get_nstep() <  5) then
       ni = 0
       do n = rtmCTL%begr, rtmCTL%endr
          ni = ni + 1
          write(iulog,F01)'export: nstep, n, Flrr_flood   = ',get_nstep(), n, r2x(index_r2x_Flrr_flood  ,ni)
          write(iulog,F01)'export: nstep, n, Flrr_volr    = ',get_nstep(), n, r2x(index_r2x_Flrr_volr   ,ni)
          write(iulog,F01)'export: nstep, n, Flrr_volrmch = ',get_nstep(), n, r2x(index_r2x_Flrr_volrmch,ni)
          write(iulog,F01)'export: nstep, n, Forr_rofl    = ',get_nstep() ,n, r2x(index_r2x_Forr_rofl  , ni)
          write(iulog,F01)'export: nstep, n, Forr_rofi    = ',get_nstep() ,n, r2x(index_r2x_Forr_rofi  , ni)
       end do
    end if

  end subroutine mosart_export

end module mosart_import_export
