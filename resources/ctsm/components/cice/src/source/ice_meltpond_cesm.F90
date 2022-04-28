!  SVN:$Id: ice_meltpond_cesm.F90 806 2014-07-31 19:00:00Z tcraig $
!=======================================================================

! CESM meltpond parameterization
!
! This meltpond parameterization was developed for use with the delta-
! Eddington radiation scheme, and only affects the radiation budget in
! the model.  That is, although the pond volume is tracked, that liquid
! water is not used elsewhere in the model for mass budgets or other
! physical processes.
!
! authors David A. Bailey (NCAR)
!         Marika M. Holland (NCAR)
!         Elizabeth C. Hunke (LANL)

      module ice_meltpond_cesm

      use ice_kinds_mod
      use ice_constants

      implicit none

      private
      public :: init_meltponds_cesm, compute_ponds_cesm, &
                write_restart_pond_cesm, read_restart_pond_cesm

      logical (kind=log_kind), public :: & 
         restart_pond_cesm ! if .true., read meltponds restart file

      real (kind=dbl_kind), public :: &
         hs0               ! snow depth for transition to bare sea ice (m)

!=======================================================================

      contains

!=======================================================================

!  Initialize melt ponds.

      subroutine init_meltponds_cesm(nx_block, ny_block, ncat, &
                                     apnd, hpnd)

      integer(kind=int_kind), intent(in) :: &
             nx_block , &
             ny_block , &
             ncat

      real(kind=dbl_kind), dimension(nx_block,ny_block,ncat), &
             intent(out) :: &
             apnd , & ! melt pond area fraction
             hpnd     ! melt pond depth

      apnd(:,:,:) = c0
      hpnd(:,:,:) = c0

      end subroutine init_meltponds_cesm

!=======================================================================

      subroutine compute_ponds_cesm(nx_block,ny_block,  &
                                   ilo, ihi, jlo, jhi,  &
                                   dt,    hi_min,       &
                                   pndaspect,           &
                                   rfrac, meltt,        &
                                   melts, frain,        &
                                   aicen, vicen, vsnon, &
                                   Tsfcn, apnd,  hpnd)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         ilo,ihi,jlo,jhi       ! beginning and end of physical domain

      real (kind=dbl_kind), intent(in) :: &
         dt,       & ! time step (s)
         hi_min,   & ! minimum ice thickness allowed for thermo (m)
         pndaspect   ! ratio of pond depth to pond fraction

      real (kind=dbl_kind), dimension(nx_block,ny_block), &
         intent(in) :: &
         rfrac, &    ! water fraction retained for melt ponds
         meltt, &
         melts, &
         frain, &
         aicen, &
         vicen, &
         vsnon

      real (kind=dbl_kind), dimension(nx_block,ny_block), &
         intent(in) :: &
         Tsfcn

      real (kind=dbl_kind), dimension(nx_block,ny_block), &
         intent(inout) :: &
         apnd, &
         hpnd

!     local temporary variables

      real (kind=dbl_kind), dimension(nx_block,ny_block) :: &
         volpn

      integer (kind=int_kind), dimension (nx_block*ny_block) :: &
         indxi, indxj     ! compressed indices for cells with ice melting

      integer (kind=int_kind) :: i,j,ij,icells

      real (kind=dbl_kind) :: &
         hi                     , & ! ice thickness (m)
         hs                     , & ! snow depth (m)
         dTs                    , & ! surface temperature diff for freeze-up (C)
         Tp                     , & ! pond freezing temperature (C)
         apondn, &
         hpondn   

      real (kind=dbl_kind), parameter :: &
         Td       = c2          , & ! temperature difference for freeze-up (C)
         rexp     = p01         , & ! pond contraction scaling
         dpthhi   = 0.9_dbl_kind    ! ratio of pond depth to ice thickness

      !-----------------------------------------------------------------
      ! Initialize 
      !-----------------------------------------------------------------
      volpn(:,:) = hpnd(:,:) * apnd(:,:) * aicen(:,:)

      !-----------------------------------------------------------------
      ! Identify grid cells where ice can melt
      !-----------------------------------------------------------------

      icells = 0
      do j = jlo, jhi
      do i = ilo, ihi
         if (aicen(i,j) > puny) then
            icells = icells + 1
            indxi(icells) = i
            indxj(icells) = j
         endif
      enddo                     ! i
      enddo                     ! j

      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)

         hi = vicen(i,j)/aicen(i,j)
         hs = vsnon(i,j)/aicen(i,j)

         if (hi < hi_min) then

         !--------------------------------------------------------------
         ! Remove ponds on thin ice
         !--------------------------------------------------------------
            apondn = c0
            hpondn = c0
            volpn (i,j) = c0

         else

            !-----------------------------------------------------------
            ! Update pond volume
            !-----------------------------------------------------------
            volpn(i,j) = volpn(i,j) &
                       + rfrac(i,j)/rhofresh*(meltt(i,j)*rhoi &
                       +                      melts(i,j)*rhos &
                       +                      frain(i,j)*  dt)&
                       * aicen(i,j)

            !-----------------------------------------------------------
            ! Shrink pond volume under freezing conditions
            !-----------------------------------------------------------
            Tp = Timelt - Td
            dTs = max(Tp - Tsfcn(i,j),c0)
            volpn(i,j) = volpn(i,j) * exp(rexp*dTs/Tp)
            volpn(i,j) = max(volpn(i,j), c0)

            ! fraction of ice covered by ponds
            apondn = min (sqrt(volpn(i,j)/(pndaspect*aicen(i,j))), c1)
            hpondn = pndaspect * apondn
            ! fraction of grid cell covered by ponds
            apondn = apondn * aicen(i,j)

            !-----------------------------------------------------------
            ! Limit pond depth
            !-----------------------------------------------------------
             hpondn = min(hpondn, dpthhi*hi)

         endif

         !-----------------------------------------------------------
         ! Reload tracer array
         !-----------------------------------------------------------
         apnd(i,j) = apondn / aicen(i,j)
         hpnd(i,j) = hpondn

      enddo

      end subroutine compute_ponds_cesm

!=======================================================================
!
! Dumps all values needed for restarting
!
! authors Elizabeth C. Hunke, LANL
!         David A. Bailey, NCAR

      subroutine write_restart_pond_cesm()

      use ice_domain_size, only: ncat
      use ice_fileunits, only: nu_dump_pond
      use ice_state, only: trcrn, nt_apnd, nt_hpnd
      use ice_restart, only: write_restart_field

      ! local variables

      logical (kind=log_kind) :: diag

      diag = .true.

      call write_restart_field(nu_dump_pond,0,trcrn(:,:,nt_apnd,:,:),'ruf8', &
                               'apnd',ncat,diag)
      call write_restart_field(nu_dump_pond,0,trcrn(:,:,nt_hpnd,:,:),'ruf8', &
                               'hpnd',ncat,diag)

      end subroutine write_restart_pond_cesm

!=======================================================================

! Reads all values needed for a meltpond volume restart
!
! authors Elizabeth C. Hunke, LANL
!         David A. Bailey, NCAR

      subroutine read_restart_pond_cesm()

      use ice_domain_size, only: ncat
      use ice_communicate, only: my_task, master_task
      use ice_fileunits, only: nu_diag, nu_restart_pond 
      use ice_state, only: trcrn, nt_apnd, nt_hpnd
      use ice_restart, only: read_restart_field

      ! local variables

      logical (kind=log_kind) :: &
         diag

      diag = .true.

      if (my_task == master_task) write(nu_diag,*) 'min/max cesm ponds'

      call read_restart_field(nu_restart_pond,0,trcrn(:,:,nt_apnd,:,:),'ruf8', &
                              'apnd',ncat,diag,field_loc_center,field_type_scalar)
      call read_restart_field(nu_restart_pond,0,trcrn(:,:,nt_hpnd,:,:),'ruf8', &
                              'hpnd',ncat,diag,field_loc_center,field_type_scalar)

      end subroutine read_restart_pond_cesm

!=======================================================================

      end module ice_meltpond_cesm

!=======================================================================
