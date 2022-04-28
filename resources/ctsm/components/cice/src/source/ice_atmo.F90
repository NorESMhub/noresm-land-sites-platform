!  SVN:$Id: ice_atmo.F90 936 2015-03-17 15:46:44Z eclare $
!=======================================================================

! Atmospheric boundary interface (stability based flux calculations)

! author: Elizabeth C. Hunke, LANL
!
! 2003: Vectorized by Clifford Chen (Fujitsu) and William Lipscomb
! 2004: Block structure added by William Lipscomb
! 2006: Converted to free source form (F90) by Elizabeth Hunke
! 2013: Form drag routine added (neutral_drag_coeffs) by David Schroeder
! 2014: Adjusted form drag and added high frequency coupling by Andrew Roberts

      module ice_atmo

      use ice_kinds_mod
      use ice_blocks, only: nx_block, ny_block
      use ice_constants
      use ice_domain_size, only: max_blocks, n_iso, max_iso

      implicit none
      save

      private
      public :: atmo_boundary_layer, atmo_boundary_const, neutral_drag_coeffs

      character (len=char_len), public :: &
         atmbndy ! atmo boundary method, 'default' ('ccsm3') or 'constant'

      logical (kind=log_kind), public :: &
         calc_strair, &  ! if true, calculate wind stress components
         formdrag,    &  ! if true, calculate form drag
         highfreq        ! if true, use high frequency coupling

      integer (kind=int_kind), public :: &
         natmiter        ! number of iterations for boundary layer calculations

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks), public :: &
         Cdn_atm     , & ! atm drag coefficient
         Cdn_ocn     , & ! ocn drag coefficient
                         ! form drag
         hfreebd,      & ! freeboard (m)
         hdraft,       & ! draft of ice + snow column (Stoessel1993)
         hridge,       & ! ridge height
         distrdg,      & ! distance between ridges
         hkeel,        & ! keel depth
         dkeel,        & ! distance between keels
         lfloe,        & ! floe length
         dfloe,        & ! distance between floes
         Cdn_atm_skin, & ! neutral skin drag coefficient
         Cdn_atm_floe, & ! neutral floe edge drag coefficient
         Cdn_atm_pond, & ! neutral pond edge drag coefficient
         Cdn_atm_rdg,  & ! neutral ridge drag coefficient
         Cdn_ocn_skin, & ! skin drag coefficient
         Cdn_ocn_floe, & ! floe edge drag coefficient
         Cdn_ocn_keel, & ! keel drag coefficient
         Cd_atm          ! atm drag ratio at measurement height

      ! This are set in the drv_in namelist and passed using infodata
      real(kind=dbl_kind), public :: flux_convergence_tolerance
      integer, public :: flux_convergence_max_iteration
      logical, public :: use_coldair_outbreak_mod

!=======================================================================

      contains

!=======================================================================

! Compute coefficients for atm/ice fluxes, stress, and reference
! temperature and humidity. NOTE: \\
! (1) All fluxes are positive downward,  \\
! (2) Here, tstar = (WT)/U*, and qstar = (WQ)/U*,  \\
! (3) wind speeds should all be above a minimum speed (eg. 1.0 m/s). \\
! (4) highfreq is from Roberts et al. (2015), Annals, doi:10.3189/2015AoG69A760
!
! ASSUME:
!  The saturation humidity of air at T(K): qsat(T)  (kg/m**3)
!
! Code originally based on CSM1

      subroutine atmo_boundary_layer (nx_block, ny_block, &
                                      sfctype,  icells,   &
                                      indxi,    indxj,    &
                                      Tsf,      potT,     &
                                      uatm,     vatm,     &
                                      wind,     zlvl,     &
                                      Qa,       rhoa,     &
                                      strx,     stry,     &
                                      Tref,     Qref,     &
                                      delt,     delq,     &
                                      lhcoef,   shcoef,   &
                                      Cdn_atm,          &
                                      Cd_atm_n,    &
                                      Qa_iso, Qref_iso,   &
                                      uice,     vice,     &
                                      Uref                )


      use ice_fileunits, only: nu_diag
      use ice_communicate, only: my_task, master_task
      use ice_exit, only: abort_ice

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells                ! number of cells that require atmo fluxes

      integer (kind=int_kind), dimension(nx_block*ny_block), &
         intent(in) :: &
         indxi, indxj    ! compressed i and j indices

      character (len=3), intent(in) :: &
         sfctype      ! ice or ocean

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(in) :: &
         Tsf      , & ! surface temperature of ice or ocean
         potT     , & ! air potential temperature  (K)
         uatm     , & ! x-direction wind speed (m/s)
         vatm     , & ! y-direction wind speed (m/s)
         wind     , & ! wind speed (m/s)
         zlvl     , & ! atm level height (m)
         Qa       , & ! specific humidity (kg/kg)
         rhoa         ! air density (kg/m^3)

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(inout) :: &
         Cdn_atm      ! neutral drag coefficient

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(out) :: &
         Cd_atm_n ! atm drag coefficient

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(inout) :: &
         strx     , & ! x surface stress (N)
         stry         ! y surface stress (N)

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(out) :: &
         Tref     , & ! reference height temperature  (K)
         Qref     , & ! reference height specific humidity (kg/kg)
         delt     , & ! potential T difference   (K)
         delq     , & ! humidity difference      (kg/kg)
         shcoef   , & ! transfer coefficient for sensible heat
         lhcoef       ! transfer coefficient for latent heat

      real (kind=dbl_kind), dimension (nx_block,ny_block), optional, intent(out) :: &
         Uref         ! reference height wind speed (m/s)

      real (kind=dbl_kind), dimension (nx_block,ny_block), optional, intent(in) :: &
         uice     , & ! x-direction ice speed (m/s)
         vice         ! y-direction ice speed (m/s)

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_iso), &
         optional, intent(in) :: &
         Qa_iso      ! specific humidity (kg/kg)

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_iso), &
         optional, intent(out) :: &
         Qref_iso    ! specific humidity (kg/kg)

      ! local variables

      logical (kind=log_kind), save :: &
         firstpass=.true. ! checks on first pass

      integer (kind=int_kind) :: &
         iter,  & ! iteration in flux convergence calculation
         k     , & ! iteration index
         i, j  , & ! horizontal indices
         n     , & ! counter
         ij        ! combined ij index

      real (kind=dbl_kind) :: &
         TsfK  , & ! surface temperature in Kelvin (K)
         xqq   , & ! temporary variable
         psimh , & ! stability function at zlvl   (momentum)
         tau   , & ! stress at zlvl
         fac   , & ! interpolation factor
         al2   , & ! ln(z10   /zTrf)
         psix2 , & ! stability function at zTrf   (heat and water)
         psimhs, & ! stable profile
         ssq   , & ! sat surface humidity     (kg/kg)
         qqq   , & ! for qsat, dqsfcdt
         TTT   , & ! for qsat, dqsfcdt
         qsat  , & ! the saturation humidity of air (kg/m^3)
         Lheat , & ! Lvap or Lsub, depending on surface type
         umin      ! minimum wind speed (m/s)

      real (kind=dbl_kind), dimension (icells) :: &
         ustar , & ! ustar (m/s)
         tstar , & ! tstar
         qstar , & ! qstar
         rdn   , & ! sqrt of neutral exchange coefficient (momentum)
         rhn   , & ! sqrt of neutral exchange coefficient (heat)
         ren   , & ! sqrt of neutral exchange coefficient (water)
         rd    , & ! sqrt of exchange coefficient (momentum)
         re    , & ! sqrt of exchange coefficient (water)
         rh    , & ! sqrt of exchange coefficient (heat)
         vmag  , & ! surface wind magnitude   (m/s)
         alz   , & ! ln(zlvl  /z10)
         thva  , & ! virtual temperature      (K)
         cp    , & ! specific heat of moist air
         hol   , & ! H (at zlvl  ) over L
         stable, & ! stability factor
         psixh     ! stability function at zlvl   (heat and water)

      real (kind=dbl_kind), parameter :: &
         cpvir = cp_wv/cp_air-c1, & ! defined as cp_wv/cp_air - 1.
         zTrf  = c2                 ! reference height for air temp (m)

      real (kind=dbl_kind) :: &
         ratio, vscl, ustar_prev(icells)
       !--- cold air outbreak parameters  (Mahrt & Sun 1995,MWR) -------------
      real(kind=dbl_kind),parameter    :: alpha = 1.4_dbl_kind
      real(kind=dbl_kind),parameter    :: maxscl = c2  ! maximum wind scaling for flux
      real(kind=dbl_kind),parameter    :: td0 = -c10   ! start t-ts for scaling

      ! local functions
      real (kind=dbl_kind) :: &
         xd    , & ! dummy argument
         psimhu, & ! unstable part of psimh
         psixhu    ! unstable part of psimx

      !------------------------------------------------------------
      ! Define functions
      !------------------------------------------------------------

      psimhu(xd)  = log((c1+xd*(c2+xd))*(c1+xd*xd)/c8) &
                  - c2*atan(xd) + pih
!ech                  - c2*atan(xd) + 1.571_dbl_kind

      psixhu(xd)  =  c2 * log((c1 + xd*xd)/c2)

      al2 = log(zref/zTrf)

      !------------------------------------------------------------
      ! Initialize
      !------------------------------------------------------------
      if (firstpass) then

      endif

      if (highfreq) then

       ! high frequency coupling follows Roberts et al. (2015)
       if (my_task==master_task.and.firstpass.and.sfctype(1:3)=='ice') then
         if (present(uice) .and. present(vice)) then
          write(nu_diag,*)'Using high frequency RASM atmospheric coupling'
         else
          call abort_ice('High frequency RASM coupling missing uice and vice')
         endif
       endif

       umin  = p5 ! minumum allowable wind-ice speed difference of 0.5 m/s

      else

       umin  = c1 ! minumum allowable wind speed of 1m/s

      endif

      do j = 1, ny_block
      do i = 1, nx_block
         if (present(Uref)) then
           Uref(i,j) = c0
         endif
         if (present(Qref_iso)) then
           do n=1, n_iso
              Qref_iso(i,j,n) = c0
           enddo
         endif
         Tref(i,j) = c0
         Qref(i,j) = c0
         delt(i,j) = c0
         delq(i,j) = c0
         shcoef(i,j) = c0
         lhcoef(i,j) = c0
      enddo
      enddo

      !------------------------------------------------------------
      ! Compute turbulent flux coefficients, wind stress, and
      ! reference temperature and humidity.
      !------------------------------------------------------------

      !------------------------------------------------------------
      ! define variables that depend on surface type
      !------------------------------------------------------------

      if (sfctype(1:3)=='ice') then

         qqq  = qqqice          ! for qsat
         TTT  = TTTice          ! for qsat
         Lheat = Lsub           ! ice to vapor
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

            if (highfreq) then
               vmag(ij) = max(umin, sqrt( (uatm(i,j)-uice(i,j))**2 + &
                                          (vatm(i,j)-vice(i,j))**2) )
            else
               vmag(ij) = max(umin, wind(i,j))
            endif

            if (formdrag .and. Cdn_atm(i,j) > puny) then
               rdn(ij)  = sqrt(Cdn_atm(i,j))
            else
               rdn(ij)  = vonkar/log(zref/iceruf) ! neutral coefficient
               Cdn_atm(i,j) = rdn(ij) * rdn(ij)
            endif

         enddo   ! ij

      elseif (sfctype(1:3)=='ocn') then

         qqq  = qqqocn
         TTT  = TTTocn
         Lheat = Lvap           ! liquid to vapor
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

            vmag(ij) = max(umin, wind(i,j))

            rdn(ij)  = sqrt(0.0027_dbl_kind/vmag(ij) &
                    + .000142_dbl_kind + .0000764_dbl_kind*vmag(ij))

         enddo   ! ij

      endif   ! sfctype

      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)

         TsfK      = Tsf(i,j) + Tffresh     ! surface temp (K)
         ! Cold Air Outbreak Modification:
         ! Increase windspeed for negative delt
         ! based on Mahrt & Sun 1995,MWR
         if (use_coldair_outbreak_mod) then
            ! Mahrt and Sun adjustment
            delt(i,j) = potT(i,j) - TsfK

            if (delt(i,j).lt.td0) then
               vscl=min((c1+alpha*(abs(delt(i,j)-td0)**p5/abs(vmag(ij)))),maxscl)
               vmag(ij)=vmag(ij)*vscl
            endif
         endif

      !------------------------------------------------------------
      ! define some more needed variables
      !------------------------------------------------------------

         qsat       = qqq * exp(-TTT/TsfK)   ! saturation humidity (kg/m^3)
         ssq        = qsat / rhoa(i,j)       ! sat surf hum (kg/kg)

         thva(ij)   = potT(i,j) * (c1 + zvir * Qa(i,j)) ! virtual pot temp (K)

         delq(i,j)  = Qa(i,j) - ssq          ! spec hum dif (kg/kg)
         alz(ij)    = log(zlvl(i,j)/zref)
         cp(ij)     = cp_air*(c1 + cpvir*ssq)

      !------------------------------------------------------------
      ! first estimate of Z/L and ustar, tstar and qstar
      !------------------------------------------------------------

         ! neutral coefficients, z/L = 0.0
         rhn(ij) = rdn(ij)
         ren(ij) = rdn(ij)

         ! ustar,tstar,qstar
         ustar(ij) = rdn(ij) * vmag(ij)
         tstar(ij) = rhn(ij) * delt(i,j)
         qstar(ij) = ren(ij) * delq(i,j)

      enddo                     ! ij

      !------------------------------------------------------------
      ! iterate to converge on Z/L, ustar, tstar and qstar
      !------------------------------------------------------------
      ustar_prev = 2.0_SHR_KIND_R8 * ustar
      do iter =1,flux_convergence_max_iteration
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)
            if (.not. (flux_convergence_tolerance > c0 .and. &
                 abs((ustar(ij) - ustar_prev(ij))/ustar(ij)) <= flux_convergence_tolerance)) then

               ! compute stability & evaluate all stability functions
               ustar_prev(ij) = ustar(ij)
               hol(ij) = vonkar * gravit * zlvl(i,j) &
                    * (tstar(ij)/thva(ij) &
                    + qstar(ij)/(c1/zvir+Qa(i,j))) &
                    / ustar(ij)**2
               hol(ij)    = sign( min(abs(hol(ij)),c10), hol(ij) )
               stable(ij) = p5 + sign(p5 , hol(ij))
               xqq    = max(sqrt(abs(c1 - c16*hol(ij))) , c1)
               xqq    = sqrt(xqq)

               ! Jordan et al 1999
               psimhs = -(0.7_dbl_kind*hol(ij) &
                    + 0.75_dbl_kind*(hol(ij)-14.3_dbl_kind) &
                    * exp(-0.35_dbl_kind*hol(ij)) + 10.7_dbl_kind)
               psimh  = psimhs*stable(ij) &
                    + (c1 - stable(ij))*psimhu(xqq)
               psixh(ij)  = psimhs*stable(ij) &
                    + (c1 - stable(ij))*psixhu(xqq)

               ! shift all coeffs to measurement height and stability
               rd(ij) = rdn(ij) / (c1+rdn(ij)/vonkar*(alz(ij)-psimh))
               rh(ij) = rhn(ij) / (c1+rhn(ij)/vonkar*(alz(ij)-psixh(ij)))
               re(ij) = ren(ij) / (c1+ren(ij)/vonkar*(alz(ij)-psixh(ij)))

               ! update ustar, tstar, qstar using updated, shifted coeffs
               ustar(ij) = rd(ij) * vmag(ij)
               tstar(ij) = rh(ij) * delt(i,j)
               qstar(ij) = re(ij) * delq(i,j)
            endif
         enddo                  ! ij
      enddo                     ! iter
      if (iter <= 0) then
         call abort_ice('error in ice_atmo flux calculation ')
      endif

      if (calc_strair) then

      ! initialize
      do j = 1, ny_block
      do i = 1, nx_block
         strx(i,j) = c0
         stry(i,j) = c0
      enddo
      enddo

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)

         if (highfreq .and. sfctype(1:3)=='ice') then

            !------------------------------------------------------------
            ! momentum flux for RASM
            !------------------------------------------------------------
            ! tau = rhoa(i,j) * rd * rd
            ! strx = tau * |Uatm-U| * (uatm-u)
            ! stry = tau * |Uatm-U| * (vatm-v)
            !------------------------------------------------------------

            tau = rhoa(i,j) * rd(ij) * rd(ij) ! not the stress at zlvl(i,j)

            ! high frequency momentum coupling following Roberts et al. (2015)
            strx(i,j) = tau * sqrt((uatm(i,j)-uice(i,j))**2 + &
                                   (vatm(i,j)-vice(i,j))**2) * &
                              (uatm(i,j)-uice(i,j))
            stry(i,j) = tau * sqrt((uatm(i,j)-uice(i,j))**2 + &
                                   (vatm(i,j)-vice(i,j))**2) * &
                              (vatm(i,j)-vice(i,j))

         else

            !------------------------------------------------------------
            ! momentum flux
            !------------------------------------------------------------
            ! tau = rhoa(i,j) * ustar * ustar
            ! strx = tau * uatm(i,j) / vmag
            ! stry = tau * vatm(i,j) / vmag
            !------------------------------------------------------------

            tau = rhoa(i,j) * ustar(ij) * rd(ij) ! not the stress at zlvl(i,j)
            strx(i,j) = tau * uatm(i,j)
            stry(i,j) = tau * vatm(i,j)

         endif

         Cd_atm_n(i,j) = rd(ij) * rd(ij)

      enddo                     ! ij

      endif                     ! calc_strair

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)

         !------------------------------------------------------------
         ! coefficients for turbulent flux calculation
         !------------------------------------------------------------
         ! add windless coefficient for sensible heat flux
         ! as in Jordan et al (JGR, 1999)
         !------------------------------------------------------------

         shcoef(i,j) = rhoa(i,j) * ustar(ij) * cp(ij) * rh(ij) + c1
         lhcoef(i,j) = rhoa(i,j) * ustar(ij) * Lheat  * re(ij)

         !------------------------------------------------------------
         ! Compute diagnostics: 2m ref T, Q, U
         !------------------------------------------------------------

         hol(ij)  = hol(ij)*zTrf/zlvl(i,j)
         xqq      = max( c1, sqrt(abs(c1-c16*hol(ij))) )
         xqq      = sqrt(xqq)
         psix2    = -c5*hol(ij)*stable(ij) + (c1-stable(ij))*psixhu(xqq)
         fac      = (rh(ij)/vonkar) &
                  * (alz(ij) + al2 - psixh(ij) + psix2)
         Tref(i,j)= potT(i,j) - delt(i,j)*fac
         Tref(i,j)= Tref(i,j) - p01*zTrf ! pot temp to temp correction
         fac      = (re(ij)/vonkar) &
                  * (alz(ij) + al2 - psixh(ij) + psix2)
         Qref(i,j)= Qa(i,j) - delq(i,j)*fac

         if (present(Uref)) then
            if (highfreq .and. sfctype(1:3)=='ice') then
               Uref(i,j) = sqrt((uatm(i,j)-uice(i,j))**2 + &
                                (vatm(i,j)-vice(i,j))**2) * &
                           rd(ij) / rdn(ij)
            else
               Uref(i,j) = vmag(ij) * rd(ij) / rdn(ij)
            endif
         endif ! (present(Uref))

         if (present(Qref_iso)) then
            do n = 1, n_iso
               ratio = c1
               if (Qa_iso(i,j,2) > puny)  &
                  ratio = Qa_iso(i,j,n)/Qa_iso(i,j,2)
               Qref_iso(i,j,n) = Qa_iso(i,j,n) - ratio*delq(i,j)*fac
            enddo
         endif

      enddo                     ! ij
      firstpass = .false.

      end subroutine atmo_boundary_layer

!=======================================================================

! Compute coefficients for atm/ice fluxes, stress
! NOTE: \\
! (1) all fluxes are positive downward,  \\
! (2) reference temperature and humidity are NOT computed

      subroutine atmo_boundary_const (nx_block, ny_block, &
                                      sfctype,  icells,   &
                                      indxi,    indxj,    &
                                      uatm,     vatm,     &
                                      wind,     rhoa,     &
                                      strx,     stry,     &
                                      Tsf,      potT,     &
                                      Qa,                 &
                                      delt,     delq,     &
                                      lhcoef,   shcoef,   &
                                      Cdn_atm)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells                ! number of cells that require atmo fluxes

      integer (kind=int_kind), dimension(nx_block*ny_block), &
         intent(in) :: &
         indxi, indxj    ! compressed i and j indices

      character (len=3), intent(in) :: &
         sfctype      ! ice or ocean

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(in) :: &
         Tsf      , & ! surface temperature of ice or ocean
         potT     , & ! air potential temperature  (K)
         Qa       , & ! specific humidity (kg/kg)
         uatm     , & ! x-direction wind speed (m/s)
         vatm     , & ! y-direction wind speed (m/s)
         wind     , & ! wind speed (m/s)
         rhoa         ! air density (kg/m^3)

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(in) :: &
         Cdn_atm      ! neutral drag coefficient

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(inout):: &
         strx     , & ! x surface stress (N)
         stry         ! y surface stress (N)

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(out):: &
         delt     , & ! potential T difference   (K)
         delq     , & ! humidity difference      (kg/kg)
         shcoef   , & ! transfer coefficient for sensible heat
         lhcoef       ! transfer coefficient for latent heat

       ! local variables

       integer (kind=int_kind) :: &
         i, j, & ! horizontal indices
         ij      ! combined ij index

      real (kind=dbl_kind) :: &
         TsfK, & ! surface temperature in Kelvin (K)
         qsat, & ! the saturation humidity of air (kg/m^3)
         ssq , & ! sat surface humidity     (kg/kg)
         tau, &  ! stress at zlvl
         Lheat   ! Lvap or Lsub, depending on surface type

      !------------------------------------------------------------
      ! Initialize
      !------------------------------------------------------------

      do j = 1, ny_block
      do i = 1, nx_block
         delt(i,j) = c0
         delq(i,j) = c0
         shcoef(i,j) = c0
         lhcoef(i,j) = c0
      enddo
      enddo

      if (calc_strair) then

      do j = 1, ny_block
      do i = 1, nx_block
         strx(i,j) = c0
         stry(i,j) = c0
      enddo
      enddo

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)

      !------------------------------------------------------------
      ! momentum flux
      !------------------------------------------------------------
         tau = rhoa(i,j) * 0.0012_dbl_kind * wind(i,j)
!AOMIP         tau = rhoa(i,j) * (1.10_dbl_kind + c4*p01*wind(i,j)) &
!AOMIP                         * wind(i,j) * p001
         strx(i,j) = tau * uatm(i,j)
         stry(i,j) = tau * vatm(i,j)

      enddo                     ! ij

      endif                     ! calc_strair

      !------------------------------------------------------------
      ! define variables that depend on surface type
      !------------------------------------------------------------

      if (sfctype(1:3)=='ice') then
         Lheat = Lsub           ! ice to vapor
      elseif (sfctype(1:3)=='ocn') then
         Lheat = Lvap           ! liquid to vapor
      endif   ! sfctype

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)

      !------------------------------------------------------------
      ! potential temperature and specific humidity differences
      !------------------------------------------------------------

         TsfK     = Tsf(i,j) + Tffresh    ! surface temp (K)
         qsat     = qqqocn * exp(-TTTocn/TsfK) ! sat humidity (kg/m^3)
         ssq      = qsat / rhoa(i,j)      ! sat surf hum (kg/kg)

         delt(i,j)= potT(i,j) - TsfK      ! pot temp diff (K)
         delq(i,j)= Qa(i,j) - ssq         ! spec hum dif (kg/kg)

      !------------------------------------------------------------
      ! coefficients for turbulent flux calculation
      !------------------------------------------------------------

         shcoef(i,j) = (1.20e-3_dbl_kind)*cp_air*rhoa(i,j)*wind(i,j)
         lhcoef(i,j) = (1.50e-3_dbl_kind)*Lheat *rhoa(i,j)*wind(i,j)

      enddo                     ! ij

      end subroutine atmo_boundary_const

!=======================================================================

! Neutral drag coefficients for ocean and atmosphere also compute the
! intermediate necessary variables ridge height, distance, floe size
! based upon Tsamados et al. (2014), JPO, DOI: 10.1175/JPO-D-13-0215.1.
! Places where the code varies from the paper are commented.
!
! authors: Michel Tsamados, CPOM
!          David Schroeder, CPOM
!
! changes: Andrew Roberts, NPS (RASM/CESM coupling and documentation)


      subroutine neutral_drag_coeffs (nx_block, ny_block, &
                                      ilo, ihi, jlo, jhi, &
                                      apnd,     hpnd,     &
                                      ipnd,               &
                                      alvl,     vlvl,     &
                                      aice,     vice,     &
                                      vsno,     aicen,    &
                                      vicen,    vsnon,    &
                                      Cdn_ocn,  Cdn_ocn_skin,    &
                                      Cdn_ocn_floe, Cdn_ocn_keel,&
                                      Cdn_atm,  Cdn_atm_skin,    &
                                      Cdn_atm_floe, Cdn_atm_pond,&
                                      Cdn_atm_rdg, hfreebd,      &
                                      hdraft,   hridge,          &
                                      distrdg,  hkeel,           &
                                      dkeel,    lfloe,           &
                                      dfloe,    ncat)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         ilo,ihi,jlo,jhi,    & ! beginning and end of physical domain
         ncat

      integer (kind=int_kind), dimension(nx_block*ny_block) :: &
         indxi, indxj    ! compressed i and j indices

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), &
         intent(in) :: &
         apnd     ,& ! melt pond fraction of sea ice
         hpnd     ,& ! mean melt pond depth over sea ice
         ipnd     ,& ! mean ice pond depth over sea ice in cat n
         alvl     ,& ! level ice area fraction (of grid cell ?)
         vlvl        ! level ice mean thickness

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(in) :: &
         aice     , & ! concentration of ice
         vice     , & ! volume per unit area of ice
         vsno         ! volume per unit area of snow

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), intent(in) :: &
         aicen    , & ! concentration of ice
         vicen    , & ! volume per unit area of ice (m)
         vsnon        ! volume per unit area of snow (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(out) :: &
         hfreebd      , & ! freeboard (m)
         hdraft       , & ! draft of ice + snow column (Stössel & Clausse 199)3
         hridge       , & ! ridge height
         distrdg      , & ! distance between ridges
         hkeel        , & ! keel depth
         dkeel        , & ! distance between keels
         lfloe        , & ! floe length (m)
         dfloe        , & ! distance between floes
         Cdn_ocn      , & ! ocean-ice neutral drag coefficient
         Cdn_ocn_skin , & ! drag coefficient due to skin drag
         Cdn_ocn_floe , & ! drag coefficient due to floe edges
         Cdn_ocn_keel , & ! drag coefficient dur to keels
         Cdn_atm      , & ! ice-atmosphere drag coefficient
         Cdn_atm_skin , & ! drag coefficient due to skin drag
         Cdn_atm_floe , & ! drag coefficient due to floe edges
         Cdn_atm_pond , & ! drag coefficient dur to ponds
         Cdn_atm_rdg      ! drag coefficient dur to ridges

      real (kind=dbl_kind), parameter :: &
                                      ! [,] = range of values that can be tested
         csw       = 0.002_dbl_kind ,&! ice-ocn drag coefficient [0.0005,0.005]
         csa       = 0.0005_dbl_kind,&! ice-air drag coefficient [0.0001,0.001]
         dragia    = 0.0012_dbl_kind,&! ice-air drag coefficient [0.0005,0.002]
         mrdg      = c20            ,&! screening effect see Lu2011 [5,50]
         mrdgo     = c10            ,&! screening effect see Lu2011 [5,50]
         beta      = p5             ,&! power exponent appearing in astar and
                                      ! L=Lmin(A*/(A*-A))**beta [0,1]
         Lmin      = c8             ,&! min length of floe (m) [5,100]
         Lmax      = 300._dbl_kind  ,&! max length of floe (m) [30,3000]
         Lmoy      = 300._dbl_kind  ,&! average length of floe (m) [30,1000]
         cfa       = p2             ,&! Eq. 12 ratio of local from drag over
                                      ! geometrical parameter [0,1]
         cfw       = p2             ,&! Eq. 15 ratio of local from drag over
                                      ! geometrical parameter [0,1]
         cpa       = p2             ,&! Eq. 16 ratio of local form drag over
                                      ! geometrical parameter [0,1]
         cra       = p2             ,&! Eq. 10 local form drag coefficient [0,1]
         crw       = p2             ,&! Eq. 11 local form drag coefficient [0,1]
         sl        = 22._dbl_kind   ,&! Sheltering parameter Lupkes2012 [10,30]
         lpmin     = 2.26_dbl_kind  ,&! min pond length (m) see Eq. 17 [1,10]
         lpmax     = 24.63_dbl_kind ,&! max pond length (m) see Eq. 17 [10,100]
         tanar     = p4             ,&! 0.25 sail slope = 14 deg [0.4,1]
         tanak     = p4             ,&! 0.58 keel slope = 30 deg [0.4,1]
         invsqrte  = 0.6065_dbl_kind,&!
         phir      = 0.8_dbl_kind   ,&! porosity of ridges [0.4,1]
         phik      = 0.8_dbl_kind   ,&! porosity of keels  [0.4,1]
         hkoverhr  = c4             ,&! hkeel/hridge ratio [4,8]
         dkoverdr  = c1             ,&! dkeel/distrdg ratio [1,5]
         sHGB      = 0.18_dbl_kind  ,&! Lupkes2012 Eq. 28, Hanssen1988,
                                      ! Steele1989 suggest instead 0.18
         alpha2    = c0             ,&! weight functions for area of
         beta2     = p75              ! ridged ice [0,1]

       integer (kind=int_kind) :: &
         icells, & ! number of cells that require atmo fluxes
         n     , & ! category index
         i, j  , & ! horizontal indices
         ij        ! combined ij index

      real (kind=dbl_kind) :: &
         astar,     & ! new constant for form drag
         ctecaf,    & ! constante
         ctecwf,    & ! constante
         sca,       & ! wind attenuation function
         scw,       & ! ocean attenuation function
         lp,        & ! pond length (m)
         ctecar,    &
         ctecwk,    &
         ai, aii,   & ! ice area and its inverse
         tmp1         ! temporary

      real (kind=dbl_kind) :: &
         apond    , & ! melt pond fraction of grid cell
         vpond    , & ! mean melt pond depth over grid cell
         ipond    , & ! mean melt pond ice depth over grid cell
         ardg     , & ! ridged ice area fraction of grid cell
         vrdg         ! ridged ice mean thickness

      real (kind=dbl_kind), parameter :: &
         ocnruf   = 0.000327_dbl_kind, & ! ocean surface roughness (m)
         ocnrufi  = c1/ocnruf, & ! inverse ocean roughness
         icerufi  = c1/iceruf    ! inverse ice roughness

      real (kind=dbl_kind), parameter :: &
         camax    = 0.02_dbl_kind , & ! Maximum for atmospheric drag
         cwmax    = 0.06_dbl_kind     ! Maximum for ocean drag

      astar = c1/(c1-(Lmin/Lmax)**(c1/beta))


      !-----------------------------------------------------------------
      ! Initialize across entire grid
      !-----------------------------------------------------------------

      hfreebd(:,:)=c0
      hdraft (:,:)=c0
      hridge (:,:)=c0
      distrdg(:,:)=c0
      hkeel  (:,:)=c0
      dkeel  (:,:)=c0
      lfloe  (:,:)=c0
      dfloe  (:,:)=c0
      Cdn_ocn(:,:)=dragio
      Cdn_ocn_skin(:,:)=c0
      Cdn_ocn_floe(:,:)=c0
      Cdn_ocn_keel(:,:)=c0
      Cdn_atm(:,:) = (vonkar/log(zref/iceruf)) * (vonkar/log(zref/iceruf))
      Cdn_atm_skin(:,:)=c0
      Cdn_atm_floe(:,:)=c0
      Cdn_atm_pond(:,:)=c0
      Cdn_atm_rdg (:,:)=c0

      !-----------------------------------------------------------------
      ! Identify cells with nonzero ice area
      !-----------------------------------------------------------------

      icells = 0
      do j = jlo, jhi
        do i = ilo, ihi
          if (aice(i,j) > p001) then
            icells = icells + 1
            indxi(icells) = i
            indxj(icells) = j
          endif
        enddo               ! i
      enddo               ! j

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
      do ij = 1, icells
        i = indxi(ij)
        j = indxj(ij)

      !------------------------------------------------------------
      ! Initialize inside loop where concentration > 0.1%
      !------------------------------------------------------------

        Cdn_atm_skin(i,j) = csa
        Cdn_ocn_skin(i,j) = csw

        ai  = aice(i,j)
        aii = c1/ai

      !------------------------------------------------------------
      ! Compute average quantities
      !------------------------------------------------------------

        ! ponds
        apond = c0
        vpond = c0
        ipond = c0
        do n = 1,ncat
          ! area of pond per unit area of grid cell
          apond = apond+apnd(i,j,n)*aicen(i,j,n)
          ! volume of pond per unit area of grid cell
          vpond = vpond+apnd(i,j,n)*hpnd(i,j,n)*aicen(i,j,n)
          ! volume of lid per unit area of grid cell
          ipond = ipond+apnd(i,j,n)*ipnd(i,j,n)*aicen(i,j,n)
        enddo

        ! draft and freeboard (see Eq. 27)
        hdraft(i,j) = (rhoi*vice(i,j)+rhos*vsno(i,j))*aii/rhow ! without ponds
        hfreebd(i,j) = (vice(i,j)+vsno(i,j))*aii-hdraft(i,j)

        ! Do not allow draft larger than ice thickness (see Eq. 28)
        if (hdraft(i,j) >= vice(i,j)*aii) then
          ! replace excess snow with ice so hi~=hdraft
          hfreebd(i,j) = (hdraft(i,j)*ai*(c1-rhoi/rhow) + &
                    (vsno(i,j)-(vice(i,j)-hdraft(i,j)*ai)*rhoi/rhos) * &
                    (c1-rhos/rhow))*aii ! Stoessel1993
        endif

        ! floe size parameterization see Eq. 13
        lfloe(i,j) = Lmin * (astar / (astar - ai))**beta

        ! distance between floes parameterization see Eq. 14
        dfloe(i,j) = lfloe(i,j) * (c1/sqrt(ai) - c1)

        ! Relate ridge height and distance between ridges to
        ! ridged ice area fraction and ridged ice mean thickness
        ! Assumes total volume of ridged ice is split into ridges and keels.
        ! Then assume total ridges volume over total area of ridges =
        ! volume of one average ridge / area of one average ridge
        ! Same for keels.

        ardg=c0
        vrdg=c0
        do n=1,ncat
          ! ridged ice area fraction over grid cell
          ardg=ardg+(c1-alvl(i,j,n))*aicen(i,j,n)
          ! total ridged ice volume per unit grid cell area
          vrdg=vrdg+(c1-vlvl(i,j,n))*vicen(i,j,n)
        enddo

        ! hridge, hkeel, distrdg and dkeel estimates from CICE for
        ! simple triangular geometry

        if (ardg > p001) then

          ! see Eq. 25 and Eq. 26
          hridge(i,j) = vrdg/ardg*c2 &
                      * (alpha2+beta2*hkoverhr/dkoverdr*tanar/tanak) &
                      / (phir*c1+phik*tanar/tanak*hkoverhr**c2/dkoverdr)
          distrdg(i,j) = c2*hridge(i,j)*ai/ardg &
                       * (alpha2/tanar+beta2/tanak*hkoverhr/dkoverdr)
          hkeel(i,j) = hkoverhr * hridge(i,j)
          dkeel(i,j) = dkoverdr * distrdg(i,j)

          ! Use the height of ridges relative to the mean freeboard of
          ! the pack.  Therefore skin drag and ridge drag differ in
          ! this code as compared to  Tsamados et al. (2014) equations
          ! 10 and 18, which reference both to sea level.
          tmp1 = max(c0,hridge(i,j) - hfreebd(i,j))

      !------------------------------------------------------------
      ! Skin drag (atmo)
      !------------------------------------------------------------

          Cdn_atm_skin(i,j) = csa*(c1 - mrdg*tmp1/distrdg(i,j))
          Cdn_atm_skin(i,j) = max(min(Cdn_atm_skin(i,j),camax),c0)

      !------------------------------------------------------------
      ! Ridge effect (atmo)
      !------------------------------------------------------------

          if (tmp1 > puny) then
            sca = c1 - exp(-sHGB*distrdg(i,j)/tmp1) ! see Eq. 9
            ctecar = cra*p5
            Cdn_atm_rdg(i,j) = ctecar*tmp1/distrdg(i,j)*sca* &
                       (log(tmp1*icerufi)/log(zref*icerufi))**c2
            Cdn_atm_rdg(i,j) = min(Cdn_atm_rdg(i,j),camax)
          endif

          ! Use the depth of keels relative to the mean draft of
          ! the pack.  Therefore skin drag and keel drag differ in
          ! this code as compared to  Tsamados et al. (2014) equations
          ! 11 and 19, which reference both to  sea level. In some
          ! circumstances, hkeel can be less than hdraft because hkoverhr
          ! is constant, and max(c0,...) temporarily addresses this.
          tmp1 = max(c0,hkeel(i,j) - hdraft(i,j))

      !------------------------------------------------------------
      ! Skin drag bottom ice (ocean)
      !------------------------------------------------------------

          Cdn_ocn_skin(i,j) = csw * (c1 - mrdgo*tmp1/dkeel(i,j))
          Cdn_ocn_skin(i,j) = max(min(Cdn_ocn_skin(i,j),cwmax), c0)

      !------------------------------------------------------------
      ! Keel effect (ocean)
      !------------------------------------------------------------

          if (tmp1 > puny) then
            scw = c1 - exp(-sHGB*dkeel(i,j)/tmp1)
            ctecwk = crw*p5

            Cdn_ocn_keel(i,j) = ctecwk*tmp1/dkeel(i,j)*scw* &
                        (log(tmp1*icerufi)/log(zref*icerufi))**c2
            Cdn_ocn_keel(i,j) = max(min(Cdn_ocn_keel(i,j),cwmax),c0)
          endif

        endif ! ardg > 0.001

      !------------------------------------------------------------
      ! Floe edge drag effect (atmo)
      !------------------------------------------------------------

        if (hfreebd(i,j) > puny) then
          sca = c1 - exp(-sl*beta*(c1-ai))
          ctecaf = cfa*p5*(log(hfreebd(i,j)*ocnrufi)/log(zref*ocnrufi))**c2*sca
          Cdn_atm_floe(i,j) = ctecaf * hfreebd(i,j) / lfloe(i,j)
          Cdn_atm_floe(i,j) = max(min(Cdn_atm_floe(i,j),camax),c0)
        endif

      !------------------------------------------------------------
      ! Pond edge effect (atmo)
      !------------------------------------------------------------

        if (hfreebd(i,j) > puny) then
          sca = (apond)**(c1/(zref*beta))
          lp  = lpmin*(1-apond)+lpmax*apond
          Cdn_atm_pond(i,j) = cpa*p5*sca*apond*hfreebd(i,j)/lp &
                   * (log(hfreebd(i,j)*ocnrufi)/log(zref*ocnrufi))**c2
          Cdn_atm_pond(i,j) = min(Cdn_atm_pond(i,j),camax)
        endif

      !------------------------------------------------------------
      ! Floe edge drag effect (ocean)
      !------------------------------------------------------------

        if (hdraft(i,j) > puny) then
          scw = c1 - exp(-sl*beta*(c1-ai))
          ctecwf = cfw*p5*(log(hdraft(i,j)*ocnrufi)/log(zref*ocnrufi))**c2*scw
          Cdn_ocn_floe(i,j) = ctecwf * hdraft(i,j) / lfloe(i,j)
          Cdn_ocn_floe(i,j) = max(min(Cdn_ocn_floe(i,j),cwmax),c0)
        endif

      !------------------------------------------------------------
      ! Total drag coefficient (atmo)
      !------------------------------------------------------------

        Cdn_atm(i,j) = Cdn_atm_skin(i,j) + Cdn_atm_floe(i,j) + &
                       Cdn_atm_pond(i,j) + Cdn_atm_rdg(i,j)
        Cdn_atm(i,j) = min(Cdn_atm(i,j),camax)

      !------------------------------------------------------------
      ! Total drag coefficient (ocean)
      !------------------------------------------------------------

        Cdn_ocn(i,j) = Cdn_ocn_skin(i,j) + Cdn_ocn_floe(i,j) + &
                       Cdn_ocn_keel(i,j)
        Cdn_ocn(i,j) = min(Cdn_ocn(i,j),cwmax)

      enddo                     ! ij

      end subroutine neutral_drag_coeffs

!=======================================================================

      end module ice_atmo

!=======================================================================
