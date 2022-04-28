!  SVN:$Id: ice_meltpond_topo.F90 915 2015-02-08 02:50:33Z tcraig $
!=======================================================================

! Melt pond evolution based on the ice topography as inferred from
! the ice thickness distribution.  This code is based on (but differs
! from) that described in
!
! Flocco, D. and D. L. Feltham, 2007.  A continuum model of melt pond 
! evolution on Arctic sea ice.  J. Geophys. Res. 112, C08016, doi: 
! 10.1029/2006JC003836.
!
! Flocco, D., D. L. Feltham and A. K. Turner, 2010.  Incorporation of a
! physically based melt pond scheme into the sea ice component of a
! climate model.  J. Geophys. Res. 115, C08012, doi: 10.1029/2009JC005568.
!
! authors Daniela Flocco (UCL)
!         Adrian Turner (UCL)
! 2010 ECH added module based on original code from Daniela Flocco, UCL
! 2012 DSCHR modifications

      module ice_meltpond_topo

      use ice_kinds_mod
      use ice_constants
      use ice_domain_size, only: nilyr, ncat

      implicit none

      private
      public :: init_meltponds_topo, compute_ponds_topo, &
                write_restart_pond_topo, read_restart_pond_topo

      logical (kind=log_kind), public :: & 
         restart_pond_topo ! if .true., read meltponds restart file

      real (kind=dbl_kind), public :: &
         hp1               ! critical parameter for pond ice thickness

!=======================================================================

      contains

!=======================================================================

!  Initialize melt ponds.

      subroutine init_meltponds_topo(nx_block, ny_block, ncat, &
                                     apnd, hpnd, ipnd)

      integer(kind=int_kind), intent(in) :: &
             nx_block , &
             ny_block , &
             ncat

      real(kind=dbl_kind), dimension(nx_block,ny_block,ncat), &
             intent(out) :: &
             apnd , & ! melt pond area fraction
             hpnd , & ! melt pond depth
             ipnd     ! melt pond refrozen lid thickness

      apnd(:,:,:) = c0
      hpnd(:,:,:) = c0
      ipnd(:,:,:) = c0
        
      end subroutine init_meltponds_topo

!=======================================================================

      subroutine compute_ponds_topo(nx_block,ny_block,  &
                                    ilo, ihi, jlo, jhi, &
                                    dt,                 &
                                    aice,  aicen,       &
                                    vice,  vicen,       &
                                    vsno,  vsnon,       &
                                    potT,  meltt,       &
                                    fsurf, fpond,       &
                                    Tsfcn, Tf,          &
                                    qicen, sicen,       &
                                    apnd,  hpnd, ipnd)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         ilo,ihi,jlo,jhi       ! beginning and end of physical domain

      real (kind=dbl_kind), intent(in) :: &
         dt ! time step (s)

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         aice, &    ! total ice area fraction
         vsno, &    ! total snow volume (m)
         Tf   ! ocean freezing temperature [= ice bottom temperature] (degC) 

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(inout) :: &
         vice, &    ! total ice volume (m)
         fpond      ! fresh water flux to ponds (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), &
         intent(in) :: &
         aicen, &   ! ice area fraction, per category
         vsnon      ! snow volume, per category (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), &
         intent(inout) :: &
         vicen      ! ice volume, per category (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), &
         intent(in) :: &
         Tsfcn

      real (kind=dbl_kind), dimension (nx_block,ny_block,nilyr,ncat), &
         intent(in) :: &
         qicen, &
         sicen

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), &
         intent(inout) :: &
         apnd, &
         hpnd, &
         ipnd

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         potT,  &   ! air potential temperature
         meltt, &   ! total surface meltwater flux
         fsurf      ! thermodynamic heat flux at ice/snow surface (W/m^2)

      ! local variables

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat) :: &
         volpn, & ! pond volume per unit area, per category (m)
         vuin     ! water-equivalent volume of ice lid on melt pond ('upper ice', m) 

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat) :: &
         apondn,& ! pond area fraction, per category
         hpondn   ! pond depth, per category (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block) :: &
         volp       ! total volume of pond, per unit area of pond (m)

      real (kind=dbl_kind) :: &
         hi,    & ! ice thickness (m)
         dHui,  & ! change in thickness of ice lid (m)
         omega,	& ! conduction
         dTice, & ! temperature difference across ice lid (C)
         dvice, & ! change in ice volume (m)
         Tavg,  & ! mean surface temperature across categories (C)
         Tp,    & ! pond freezing temperature (C)
         dvn      ! change in melt pond volume for fresh water budget
      integer (kind=int_kind), dimension (nx_block*ny_block) :: &
         indxi, indxj    ! compressed indices for cells with ice melting

      integer (kind=int_kind) :: n,i,j,ij,icells,indxij ! loop indices

      integer (kind=int_kind), dimension (ncat) :: &
         kcells          ! cells where ice lid combines with vice

      integer (kind=int_kind), dimension (nx_block*ny_block,ncat) :: &
         indxii, indxjj  ! i,j indices for kcells loop

      real (kind=dbl_kind), parameter :: &
         hicemin = p1           , & ! minimum ice thickness with ponds (m) 
         Td      = p15          , & ! temperature difference for freeze-up (C)
         rhoi_L  = Lfresh * rhoi, & ! (J/m^3)
         min_volp = 1.e-4_dbl_kind  ! minimum pond volume (m)

      !---------------------------------------------------------------
      ! initialize
      !---------------------------------------------------------------

      do j = 1, ny_block
         do i = 1, nx_block
            volp(i,j) = c0
         enddo
      enddo
      do n = 1, ncat
         do j = jlo, jhi
            do i = ilo, ihi
               ! load tracers
               volp(i,j) = volp(i,j) + hpnd(i,j,n) &
                                     * apnd(i,j,n) * aicen(i,j,n)
               vuin (i,j,n) = ipnd(i,j,n) &
                            * apnd(i,j,n) * aicen(i,j,n)

               hpondn(i,j,n) = c0     ! pond depth, per category
               apondn(i,j,n) = c0     ! pond area,  per category
            enddo
         enddo
         indxii(:,n) = 0
         indxjj(:,n) = 0
         kcells  (n) = 0
      enddo

      ! The freezing temperature for meltponds is assumed slightly below 0C,
      ! as if meltponds had a little salt in them.  The salt budget is not
      ! altered for meltponds, but if it were then an actual pond freezing 
      ! temperature could be computed.

      Tp = Timelt - Td

      !-----------------------------------------------------------------
      ! Identify grid cells with ponds
      !-----------------------------------------------------------------

      icells = 0
      do j = jlo, jhi
      do i = ilo, ihi
         hi = c0
         if (aice(i,j) > puny) hi = vice(i,j)/aice(i,j)
         if ( aice(i,j) > p01 .and. hi > hicemin .and. &
            volp(i,j) > min_volp*aice(i,j)) then
            icells = icells + 1
            indxi(icells) = i
            indxj(icells) = j
         else  ! remove ponds on thin ice
            fpond(i,j) = fpond(i,j) - volp(i,j)
            volpn(i,j,:) = c0
            vuin (i,j,:) = c0
            volp (i,j) = c0
         endif
      enddo                     ! i
      enddo                     ! j

      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)

         !--------------------------------------------------------------
         ! calculate pond area and depth
         !--------------------------------------------------------------
         call pond_area(dt,aice(i,j),   vice(i,j),vsno(i,j), &
                        aicen(i,j,:),   vicen(i,j,:), vsnon(i,j,:), &
                        qicen(i,j,:,:), sicen(i,j,:,:), &
                        volpn(i,j,:),   volp(i,j), &
                        Tsfcn(i,j,:),   Tf(i,j), & 
                        apondn(i,j,:),  hpondn(i,j,:), dvn)

         fpond(i,j) = fpond(i,j) - dvn

         ! mean surface temperature
         Tavg = c0
         do n = 1, ncat
            Tavg = Tavg + Tsfcn(i,j,n)*aicen(i,j,n)
         enddo
         Tavg = Tavg / aice(i,j)

         do n = 1, ncat-1
                       
            if (vuin(i,j,n) > puny) then

         !----------------------------------------------------------------
         ! melting: floating upper ice layer melts in whole or part
         !----------------------------------------------------------------
               ! Use Tsfc for each category
               if (Tsfcn(i,j,n) > Tp) then

                  dvice = min(meltt(i,j)*apondn(i,j,n), vuin(i,j,n))
                  if (dvice > puny) then
                     vuin (i,j,n) = vuin (i,j,n) - dvice
                     volpn(i,j,n) = volpn(i,j,n) + dvice
                     volp (i,j)   = volp (i,j)   + dvice
                     fpond(i,j)   = fpond(i,j)   + dvice
                       
                     if (vuin(i,j,n) < puny .and. volpn(i,j,n) > puny) then
                        ! ice lid melted and category is pond covered
                        volpn(i,j,n) = volpn(i,j,n) + vuin(i,j,n)
                        fpond(i,j)   = fpond(i,j)   + vuin(i,j,n)
                        vuin(i,j,n)  = c0
                     endif
                     hpondn(i,j,n) = volpn(i,j,n) / apondn(i,j,n)
                  endif

         !----------------------------------------------------------------
         ! freezing: existing upper ice layer grows
         !----------------------------------------------------------------
               else if (volpn(i,j,n) > puny) then ! Tsfcn(i,j,n) <= Tp

                  ! differential growth of base of surface floating ice layer
                  dTice = max(-Tsfcn(i,j,n)-Td, c0) ! > 0   
                  omega = kice*DTice/rhoi_L
                  dHui = sqrt(c2*omega*dt + (vuin(i,j,n)/aicen(i,j,n))**2) &
                                           - vuin(i,j,n)/aicen(i,j,n)

                  dvice = min(dHui*apondn(i,j,n), volpn(i,j,n))   
                  if (dvice > puny) then
                     vuin (i,j,n) = vuin (i,j,n) + dvice
                     volpn(i,j,n) = volpn(i,j,n) - dvice
                     volp (i,j)   = volp (i,j)   - dvice
                     fpond(i,j)   = fpond(i,j)   - dvice
                     hpondn(i,j,n) = volpn(i,j,n) / apondn(i,j,n)
                  endif

               endif ! Tsfcn(i,j,n)

         !----------------------------------------------------------------
         ! freezing: upper ice layer begins to form
         ! note: albedo does not change
         !----------------------------------------------------------------
            else ! vuin < puny
                    
               ! thickness of newly formed ice
               ! the surface temperature of a meltpond is the same as that
               ! of the ice underneath (0C), and the thermodynamic surface 
               ! flux is the same
               dHui = max(-fsurf(i,j)*dt/rhoi_L, c0)
               dvice = min(dHui*apondn(i,j,n), volpn(i,j,n))  
               if (dvice > puny) then
                  vuin (i,j,n) = dvice
                  volpn(i,j,n) = volpn(i,j,n) - dvice
                  volp (i,j)   = volp (i,j)   - dvice
                  fpond(i,j)   = fpond(i,j)   - dvice
                  hpondn(i,j,n)= volpn(i,j,n) / apondn(i,j,n)
               endif
                    
            endif  ! vuin

         enddo ! ncat

      enddo ! ij

      !---------------------------------------------------------------
      ! remove ice lid if there is no liquid pond
      ! vuin may be nonzero on category ncat due to dynamics
      !---------------------------------------------------------------

      do j = jlo, jhi
      do i = ilo, ihi
         do n = 1, ncat
            if (aicen(i,j,n) > puny .and. volpn(i,j,n) < puny &
                                    .and. vuin (i,j,n) > puny) then
               kcells(n) = kcells(n) + 1
               indxij    = kcells(n)
               indxii(indxij,n) = i
               indxjj(indxij,n) = j
            endif
         enddo
      enddo                     ! i
      enddo                     ! j

      do n = 1, ncat

         if (kcells(n) > 0) then
         do ij = 1, kcells(n)
            i = indxii(ij,n)
            j = indxjj(ij,n)
            vuin(i,j,n) = c0
         enddo    ! ij
         endif

         ! reload tracers
         do j = jlo, jhi
            do i = ilo, ihi
               if (apondn(i,j,n) > puny) then
                  ipnd(i,j,n) = vuin(i,j,n) / apondn(i,j,n)
               else
                  vuin(i,j,n) = c0
                  ipnd(i,j,n) = c0
               endif
               if (aicen(i,j,n) > puny) then
                  apnd(i,j,n) = apondn(i,j,n) / aicen(i,j,n)
                  hpnd(i,j,n) = hpondn(i,j,n)
               else
                  apnd(i,j,n) = c0
                  hpnd(i,j,n) = c0
                  ipnd(i,j,n) = c0
               endif
            enddo ! i
         enddo    ! j

      enddo       ! n

 end subroutine compute_ponds_topo

!=======================================================================

! Computes melt pond area, pond depth and melting rates

      subroutine pond_area(dt,                    &
                           aice,   vice,   vsno,  &
                           aicen,  vicen,  vsnon, &
                           qicen,  sicen,         &
                           volpn,  volp,          &
                           Tsfcn,  Tf,            &
                           apondn, hpondn, dvolp)

      use ice_constants, only: viscosity_dyn
      use ice_exit, only: abort_ice
      use ice_therm_shared, only: ktherm
    
      real (kind=dbl_kind), intent(in) :: &
         dt, aice, vice, vsno, Tf

      real (kind=dbl_kind), dimension(ncat), intent(in) :: &
         aicen, vicen, vsnon, Tsfcn

      real (kind=dbl_kind), dimension(nilyr,ncat), intent(in) :: &
         qicen, &
         sicen

      real (kind=dbl_kind), dimension(ncat), intent(inout) :: &
         volpn

      real (kind=dbl_kind), intent(inout) :: &
         volp, dvolp

      real (kind=dbl_kind), dimension(ncat), intent(out) :: &
         apondn, hpondn

      ! local variables

      integer (kind=int_kind) :: &
         n, ns,   &
	 m_index, &
         permflag

      real (kind=dbl_kind), dimension(ncat) :: &
         hicen, &
         hsnon, &
         asnon, &
         alfan, &
         betan, &
         cum_max_vol, &
         reduced_aicen        

      real (kind=dbl_kind), dimension(0:ncat) :: &
         cum_max_vol_tmp

      real (kind=dbl_kind) :: &
         hpond, &
         drain, &
         floe_weight, &
         pressure_head, &
         hsl_rel, &
         deltah, &
         perm, &
         apond

 !-----------|
 !           |
 !           |-----------|
 !___________|___________|______________________________________sea-level
 !           |           |
 !           |           |---^--------|
 !           |           |   |        |
 !           |           |   |        |-----------|              |-------
 !           |           |   |alfan(n)|           |              |
 !           |           |   |        |           |--------------|
 !           |           |   |        |           |              |
 !---------------------------v-------------------------------------------
 !           |           |   ^        |           |              |
 !           |           |   |        |           |--------------|
 !           |           |   |betan(n)|           |              |
 !           |           |   |        |-----------|              |-------
 !           |           |   |        |
 !           |           |---v------- |
 !           |           |
 !           |-----------|
 !           |
 !-----------|
    
      !-------------------------------------------------------------------
      ! initialize
      !-------------------------------------------------------------------

      do n = 1, ncat

         apondn(n) = c0
         hpondn(n) = c0

         if (aicen(n) < puny)  then
            hicen(n) =  c0 
            hsnon(n) = c0
            reduced_aicen(n) = c0
            asnon(n) = c0
         else
            hicen(n) = vicen(n) / aicen(n)
            hsnon(n) = vsnon(n) / aicen(n)
            reduced_aicen(n) = c1 ! n=ncat
            if (n < ncat) reduced_aicen(n) = aicen(n) &
                * max(0.2_dbl_kind,(-0.024_dbl_kind*hicen(n) + 0.832_dbl_kind))
            asnon(n) = reduced_aicen(n) 
         endif

! This choice for alfa and beta ignores hydrostatic equilibium of categories.
! Hydrostatic equilibium of the entire ITD is accounted for below, assuming
! a surface topography implied by alfa=0.6 and beta=0.4, and rigidity across all
! categories.  alfa and beta partition the ITD - they are areas not thicknesses!
! Multiplying by hicen, alfan and betan (below) are thus volumes per unit area.
! Here, alfa = 60% of the ice area (and since hice is constant in a category, 
! alfan = 60% of the ice volume) in each category lies above the reference line, 
! and 40% below. Note: p6 is an arbitrary choice, but alfa+beta=1 is required.

         alfan(n) = p6 * hicen(n)
         betan(n) = p4 * hicen(n)
       
         cum_max_vol(n)     = c0
         cum_max_vol_tmp(n) = c0
    
      enddo ! ncat

      cum_max_vol_tmp(0) = c0
      drain = c0
      dvolp = c0
    
      !--------------------------------------------------------------------------
      ! the maximum amount of water that can be contained up to each ice category
      !--------------------------------------------------------------------------
    
      do n = 1, ncat-1 ! last category can not hold any volume

         if (alfan(n+1) >= alfan(n) .and. alfan(n+1) > c0) then

            ! total volume in level including snow
            cum_max_vol_tmp(n) = cum_max_vol_tmp(n-1) + &
               (alfan(n+1) - alfan(n)) * sum(reduced_aicen(1:n)) 


            ! subtract snow solid volumes from lower categories in current level
            do ns = 1, n 
               cum_max_vol_tmp(n) = cum_max_vol_tmp(n) &
                  - rhos/rhow  * &    ! fraction of snow that is occupied by solid
                    asnon(ns)  * &    ! area of snow from that category
                    max(min(hsnon(ns)+alfan(ns)-alfan(n), alfan(n+1)-alfan(n)), c0)  
                                      ! thickness of snow from ns layer in n layer
            enddo

         else ! assume higher categories unoccupied
            cum_max_vol_tmp(n) = cum_max_vol_tmp(n-1)
         endif
         if (cum_max_vol_tmp(n) < c0) call abort_ice('negative melt pond volume')

      enddo
      cum_max_vol_tmp(ncat) = cum_max_vol_tmp(ncat-1)  ! last category holds no volume
      cum_max_vol  (1:ncat) = cum_max_vol_tmp(1:ncat)
    
      !----------------------------------------------------------------
      ! is there more meltwater than can be held in the floe?
      !----------------------------------------------------------------
      if (volp >= cum_max_vol(ncat)) then
         drain = volp - cum_max_vol(ncat) + puny
         volp = volp - drain
         dvolp = drain
         if (volp < puny) then
            dvolp = dvolp + volp
            volp = c0
         endif
      endif
    
      ! height and area corresponding to the remaining volume

      call calc_hpond(reduced_aicen, asnon, hsnon, alfan, &
                      volp, cum_max_vol, hpond, m_index)
    
      do n=1, m_index
         hpondn(n) = max((hpond - alfan(n) + alfan(1)), c0)
         apondn(n) = reduced_aicen(n) 
      enddo
      apond = sum(apondn(1:m_index))
    
      !------------------------------------------------------------------------
      ! drainage due to ice permeability - Darcy's law
      !------------------------------------------------------------------------
    
      ! sea water level 
      floe_weight = (vsno*rhos + rhoi*vice + rhow*volp) / aice
      hsl_rel = floe_weight / rhow &
              - ((sum(betan(:)*aicen(:))/aice) + alfan(1))
    
      deltah = hpond - hsl_rel
      pressure_head = gravit * rhow * max(deltah, c0)

      ! drain if ice is permeable    
      permflag = 0
      if (ktherm /= 2 .and. pressure_head > c0) then
      do n = 1, ncat-1
         if (hicen(n) > c0) then
            call permeability_phi(qicen(:,n),sicen(:,n),Tsfcn(n),Tf,vicen(n),perm)
            if (perm > c0) permflag = 1
            drain = perm*apondn(n)*pressure_head*dt / (viscosity_dyn*hicen(n))
            dvolp = dvolp + min(drain, volp)
            volp = max(volp - drain, c0)
            if (volp < puny) then
               dvolp = dvolp + volp
               volp = c0
            endif
         endif
      enddo
 
      ! adjust melt pond dimensions
      if (permflag > 0) then
         ! recompute pond depth    
         call calc_hpond(reduced_aicen, asnon, hsnon, alfan, &
                         volp, cum_max_vol, hpond, m_index)
         do n=1, m_index
            hpondn(n) = hpond - alfan(n) + alfan(1)
            apondn(n) = reduced_aicen(n) 
         enddo
         apond = sum(apondn(1:m_index))
      endif
      endif ! pressure_head

      !------------------------------------------------------------------------
      ! total melt pond volume in category does not include snow volume
      ! snow in melt ponds is not melted
      !------------------------------------------------------------------------

      ! Calculate pond volume for lower categories
      do n=1,m_index-1
         volpn(n) = apondn(n) * hpondn(n) &
                  - (rhos/rhow) * asnon(n) * min(hsnon(n), hpondn(n))
      enddo

      ! Calculate pond volume for highest category = remaining pond volume
      if (m_index == 1) volpn(m_index) = volp
      if (m_index > 1) then
        if (volp > sum(volpn(1:m_index-1))) then
          volpn(m_index) = volp - sum(volpn(1:m_index-1))
        else
          volpn(m_index) = c0
          hpondn(m_index) = c0
          apondn(m_index) = c0
          ! If remaining pond volume is negative reduce pond volume of 
          ! lower category
          if (volp+puny < sum(volpn(1:m_index-1))) & 
            volpn(m_index-1) = volpn(m_index-1) - sum(volpn(1:m_index-1)) + &
                               volp
        endif
      endif

      do n=1,m_index
         if (apondn(n) > puny) then
             hpondn(n) = volpn(n) / apondn(n)
         else
            dvolp = dvolp + volpn(n)
            hpondn(n) = c0
            volpn(n) = c0
            apondn(n) = c0
         end if
      enddo
      do n = m_index+1, ncat
         hpondn(n) = c0
         apondn(n) = c0
         volpn (n) = c0
      enddo

      end subroutine pond_area
  
!=======================================================================
  
  subroutine calc_hpond(aicen, asnon, hsnon, alfan, &
       volp, cum_max_vol, &
       hpond, m_index)
    
    real (kind=dbl_kind), dimension(ncat), intent(in) :: &
         aicen, &
         asnon, &
         hsnon, &
         alfan, &
         cum_max_vol
    
    real (kind=dbl_kind), intent(in) :: &
         volp
    
    real (kind=dbl_kind), intent(out) :: &
         hpond
    
    integer (kind=int_kind), intent(out) :: &
         m_index
    
    integer :: n, ns
    
    real (kind=dbl_kind), dimension(0:ncat+1) :: &
         hitl, &
         aicetl
    
    real (kind=dbl_kind) :: &
         rem_vol, &
         area, &
         vol, &
         tmp
    
    !----------------------------------------------------------------
    ! hpond is zero if volp is zero - have we fully drained? 
    !----------------------------------------------------------------
    
    if (volp < puny) then
       hpond = c0
       m_index = 0
    else
       
       !----------------------------------------------------------------
       ! Calculate the category where water fills up to 
       !----------------------------------------------------------------
       
       !----------|
       !          |
       !          |
       !          |----------|                                     -- --
       !__________|__________|_________________________________________ ^
       !          |          |             rem_vol     ^                | Semi-filled
       !          |          |----------|-- -- -- - ---|-- ---- -- -- --v layer
       !          |          |          |              |
       !          |          |          |              |hpond
       !          |          |          |----------|   |     |-------
       !          |          |          |          |   |     |
       !          |          |          |          |---v-----|
       !          |          | m_index  |          |         |
       !-------------------------------------------------------------
       
       m_index = 0  ! 1:m_index categories have water in them
       do n = 1, ncat
          if (volp <= cum_max_vol(n)) then
             m_index = n
             if (n == 1) then
                rem_vol = volp
             else
                rem_vol = volp - cum_max_vol(n-1)
             endif
             exit ! to break out of the loop
          endif
       enddo
       m_index = min(ncat-1, m_index)
       
       !----------------------------------------------------------------
       ! semi-filled layer may have m_index different snows in it
       !----------------------------------------------------------------
       
       !-----------------------------------------------------------  ^
       !                                                             |  alfan(m_index+1)
       !                                                             |
       !hitl(3)-->                             |----------|          |
       !hitl(2)-->                |------------| * * * * *|          |
       !hitl(1)-->     |----------|* * * * * * |* * * * * |          |
       !hitl(0)-->-------------------------------------------------  |  ^
       !                various snows from lower categories          |  |alfa(m_index)
       
       ! hitl - heights of the snow layers from thinner and current categories
       ! aicetl - area of each snow depth in this layer
       
       hitl(:) = c0
       aicetl(:) = c0
       do n = 1, m_index
          hitl(n)   = max(min(hsnon(n) + alfan(n) - alfan(m_index), &
                                 alfan(m_index+1) - alfan(m_index)), c0)
          aicetl(n) = asnon(n)
          
          aicetl(0) = aicetl(0) + (aicen(n) - asnon(n))
       enddo
       hitl(m_index+1) = alfan(m_index+1) - alfan(m_index)
       aicetl(m_index+1) = c0
       
       !----------------------------------------------------------------
       ! reorder array according to hitl 
       ! snow heights not necessarily in height order
       !----------------------------------------------------------------
       
       do ns = 1, m_index+1
          do n = 0, m_index - ns + 1
             if (hitl(n) > hitl(n+1)) then ! swap order
                tmp = hitl(n)
                hitl(n) = hitl(n+1)
                hitl(n+1) = tmp
                tmp = aicetl(n)
                aicetl(n) = aicetl(n+1)
                aicetl(n+1) = tmp
             endif
          enddo
       enddo
       
       !----------------------------------------------------------------
       ! divide semi-filled layer into set of sublayers each vertically homogenous
       !----------------------------------------------------------------
       
       !hitl(3)----------------------------------------------------------------
       !                                                       | * * * * * * * *  
       !                                                       |* * * * * * * * * 
       !hitl(2)----------------------------------------------------------------
       !                                    | * * * * * * * *  | * * * * * * * *  
       !                                    |* * * * * * * * * |* * * * * * * * * 
       !hitl(1)----------------------------------------------------------------
       !                 | * * * * * * * *  | * * * * * * * *  | * * * * * * * *  
       !                 |* * * * * * * * * |* * * * * * * * * |* * * * * * * * * 
       !hitl(0)----------------------------------------------------------------
       !    aicetl(0)         aicetl(1)           aicetl(2)          aicetl(3)            
       
       ! move up over layers incrementing volume
       do n = 1, m_index+1
          
          area = sum(aicetl(:)) - &                 ! total area of sub-layer
               (rhos/rhow) * sum(aicetl(n:ncat+1)) ! area of sub-layer occupied by snow
          
          vol = (hitl(n) - hitl(n-1)) * area      ! thickness of sub-layer times area
          
          if (vol >= rem_vol) then  ! have reached the sub-layer with the depth within
             hpond = rem_vol / area + hitl(n-1) + alfan(m_index) - alfan(1)
             exit
          else  ! still in sub-layer below the sub-layer with the depth
             rem_vol = rem_vol - vol
          endif
          
       enddo
       
    endif
    
  end subroutine calc_hpond
  
!=======================================================================

! determine the liquid fraction of brine in the ice and the permeability

      subroutine permeability_phi(qicen, sicen, Tsfcn, Tf, vicen, perm)

      use ice_exit, only: abort_ice
      use ice_therm_shared, only: calculate_Tin_from_qin, heat_capacity
      use ice_constants, only: ice_ref_salinity

      real (kind=dbl_kind), dimension(nilyr), intent(in) :: &
         qicen, &  ! energy of melting for each ice layer (J/m2)
         sicen     ! salinity (ppt)   
    
      real (kind=dbl_kind), intent(in) :: &
         vicen, &  ! ice volume
         Tsfcn, &  ! sea ice surface skin temperature (degC)     
         Tf     ! ocean freezing temperature [= ice bottom temperature] (degC) 
    
      real (kind=dbl_kind), intent(out) :: &
         perm      ! permeability

      ! local variables

      real (kind=dbl_kind) ::   &
         Tmlt, &   ! melting temperature 
         Sbr       ! brine salinity

      real (kind=dbl_kind), dimension(nilyr) ::   &
         Tin, &    ! ice temperature
         phi       ! liquid fraction

      integer (kind=int_kind) :: k
    
      !-----------------------------------------------------------------
      ! Compute ice temperatures from enthalpies using quadratic formula
      !-----------------------------------------------------------------

      if (heat_capacity) then
        do k = 1,nilyr
           Tmlt = -sicen(k) * depressT
           Tin(k) = calculate_Tin_from_qin(qicen(k),Tmlt)
        enddo
      else
        Tin(1) = (Tsfcn + Tf) / c2
      endif  

      !-----------------------------------------------------------------
      ! brine salinity and liquid fraction
      !-----------------------------------------------------------------

      if (maxval(Tin) <= -c2) then

         ! Assur 1958
         do k = 1,nilyr
            Sbr = - 1.2_dbl_kind                 &
                  -21.8_dbl_kind     * Tin(k)    &
                  - 0.919_dbl_kind   * Tin(k)**2 &
                  - 0.01878_dbl_kind * Tin(k)**3
            if (heat_capacity) then
              phi(k) = sicen(k)/Sbr ! liquid fraction
            else
              phi(k) = ice_ref_salinity / Sbr ! liquid fraction
            endif
         enddo ! k
       
      else

         ! Notz 2005 thesis eq. 3.2
         do k = 1,nilyr
            Sbr = -17.6_dbl_kind    * Tin(k)    &
                  - 0.389_dbl_kind  * Tin(k)**2 &
                  - 0.00362_dbl_kind* Tin(k)**3
            if (Sbr == c0) call abort_ice( &
               'zero brine salinity in topo pond permeability')
            if (heat_capacity) then
              phi(k) = sicen(k) / Sbr         ! liquid fraction
            else
              phi(k) = ice_ref_salinity / Sbr ! liquid fraction
            endif

         enddo

      endif

      !-----------------------------------------------------------------
      ! permeability
      !-----------------------------------------------------------------

      perm = 3.0e-08_dbl_kind * (minval(phi))**3
    
      end subroutine permeability_phi

!=======================================================================

! Dumps all values needed for restarting
!
! authors Elizabeth C. Hunke, LANL
!         David A. Bailey, NCAR

      subroutine write_restart_pond_topo()

      use ice_domain_size, only: ncat
      use ice_fileunits, only: nu_dump_pond
      use ice_state, only: trcrn, nt_apnd, nt_hpnd, nt_ipnd
      use ice_restart, only: write_restart_field

      ! local variables

      logical (kind=log_kind) :: diag

      diag = .true.

      call write_restart_field(nu_dump_pond,0,trcrn(:,:,nt_apnd,:,:),'ruf8', &
                               'apnd',ncat,diag)
      call write_restart_field(nu_dump_pond,0,trcrn(:,:,nt_hpnd,:,:),'ruf8', &
                               'hpnd',ncat,diag)
      call write_restart_field(nu_dump_pond,0,trcrn(:,:,nt_ipnd,:,:),'ruf8', &
                               'ipnd',ncat,diag)

      end subroutine write_restart_pond_topo

!=======================================================================

! Reads all values needed for a meltpond volume restart
!
! authors Elizabeth C. Hunke, LANL
!         David A. Bailey, NCAR

      subroutine read_restart_pond_topo()

      use ice_domain_size, only: ncat
      use ice_communicate, only: my_task, master_task
      use ice_fileunits, only: nu_diag, nu_restart_pond 
      use ice_state, only: trcrn, nt_apnd, nt_hpnd, nt_ipnd
      use ice_restart, only: read_restart_field

      ! local variables

      logical (kind=log_kind) :: &
         diag

      diag = .true.

      if (my_task == master_task) write(nu_diag,*) 'min/max topo ponds'

      call read_restart_field(nu_restart_pond,0,trcrn(:,:,nt_apnd,:,:),'ruf8', &
                              'apnd',ncat,diag,field_loc_center,field_type_scalar)
      call read_restart_field(nu_restart_pond,0,trcrn(:,:,nt_hpnd,:,:),'ruf8', &
                              'hpnd',ncat,diag,field_loc_center,field_type_scalar)
      call read_restart_field(nu_restart_pond,0,trcrn(:,:,nt_ipnd,:,:),'ruf8', &
                              'ipnd',ncat,diag,field_loc_center,field_type_scalar)

      end subroutine read_restart_pond_topo

!=======================================================================

      end module ice_meltpond_topo

!=======================================================================
