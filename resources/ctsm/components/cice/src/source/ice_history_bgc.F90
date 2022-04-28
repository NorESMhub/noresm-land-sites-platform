!  SVN:$Id: ice_history_bgc.F90 745 2013-09-28 18:22:36Z eclare $
!=======================================================================
! Biogeochemistry history output
!
! authors Elizabeth C. Hunke and Nicole Jeffery, LANL
!
! 2012 Elizabeth Hunke split code from ice_history.F90

      module ice_history_bgc

      use ice_kinds_mod
      use ice_constants
      use ice_domain_size, only: max_nstrm, max_aero, n_aero, max_iso, n_iso, nblyr
      use ice_zbgc_shared

      implicit none
      private
      public :: init_hist_bgc_2D, init_hist_bgc_3Dc, &
                init_hist_bgc_4Db, accum_hist_bgc
      
      !---------------------------------------------------------------
      ! flags: write to output file if true or histfreq value
      !---------------------------------------------------------------

      character (len=max_nstrm), public :: &
           f_faero_atm    = 'x', f_faero_ocn    = 'x', &
           f_aero         = 'x', f_aeron        = 'x', &
           f_fiso_atm     = 'x', f_fiso_ocn     = 'x', &
           f_iso          = 'x', f_ison         = 'x', &
           f_fNO          = 'x', f_fNO_ai       = 'x', &
           f_fNH          = 'x', f_fNH_ai       = 'x', &
           f_fN           = 'x', f_fN_ai        = 'x', &
           f_fSil         = 'x', f_fSil_ai      = 'x', &
           f_bgc_N_sk     = 'x', f_bgc_C_sk     = 'x', &
           f_bgc_chl_sk   = 'x', f_bgc_Nit_sk   = 'x', &
           f_bgc_Am_sk    = 'x', f_bgc_Sil_sk   = 'x', &
           f_bgc_DMSPp_sk = 'x', f_bgc_DMSPd_sk = 'x', &
           f_bgc_DMS_sk   = 'x', f_bgc_Sil_ml   = 'x', &
           f_bgc_Nit_ml   = 'x', f_bgc_Am_ml    = 'x', &
           f_bgc_DMSP_ml  = 'x', f_bgc_DMS_ml   = 'x', &
           f_bTin         = 'x', f_bphi         = 'x', &
           f_bgc_NO       = 'x', &
           f_bgc_N        = 'x', f_bgc_NH       = 'x', &
           f_bgc_C        = 'x', f_bgc_chl      = 'x', &
           f_bgc_DMSPp    = 'x', f_bgc_DMSPd    = 'x', &
           f_bgc_DMS      = 'x', f_bgc_Sil      = 'x', &
           f_bgc_S        = 'x', &
           f_fbri         = 'x', f_hbri         = 'x', &
           f_grownet      = 'x', f_PPnet        = 'x'

      !---------------------------------------------------------------
      ! namelist variables
      !---------------------------------------------------------------

      namelist / icefields_bgc_nml /     &
           f_faero_atm   , f_faero_ocn   , &
           f_aero        , f_aeron       , &
           f_fiso_atm    , f_fiso_ocn    , &
           f_iso         , f_ison        , &
           f_fNO         , f_fNO_ai      , &
           f_fNH         , f_fNH_ai      , &
           f_fN          , f_fN_ai       , &
           f_fSil        , f_fSil_ai     , &
           f_bgc_N_sk    , f_bgc_C_sk    , &
           f_bgc_chl_sk  , f_bgc_Nit_sk  , &
           f_bgc_Am_sk   , f_bgc_Sil_sk  , &
           f_bgc_DMSPp_sk, f_bgc_DMSPd_sk, &
           f_bgc_DMS_sk  , f_bgc_Sil_ml  , &
           f_bgc_Nit_ml  , f_bgc_Am_ml   , &
           f_bgc_DMSP_ml , f_bgc_DMS_ml  , &
           f_bTin        , f_bphi        , &
           f_bgc_NO      , &
           f_bgc_N       , f_bgc_NH      , &
           f_bgc_C       , f_bgc_chl     , &
           f_bgc_DMSPp   , f_bgc_DMSPd   , &
           f_bgc_DMS     , f_bgc_Sil     , &
           f_bgc_S       , &
           f_fbri        , f_hbri        , &
           f_grownet     , f_PPnet       

      !---------------------------------------------------------------
      ! field indices
      !---------------------------------------------------------------

      integer(kind=int_kind), dimension(max_aero,max_nstrm) :: &
           n_faero_atm    , &
           n_faero_ocn    , &
           n_aerosn1      , &
           n_aerosn2      , &
           n_aeroic1      , &
           n_aeroic2

      integer(kind=int_kind), dimension(max_iso,max_nstrm) :: &
           n_fiso_atm    , &
           n_fiso_ocn    , &
           n_isosn1      , &
           n_isosn2      , &
           n_isoic1      , &
           n_isoic2

      integer(kind=int_kind), dimension(max_nstrm) :: &
           n_fNO         , n_fNO_ai      , &
           n_fNH         , n_fNH_ai      , &
           n_fN          , n_fN_ai       , &
           n_fSil        , n_fSil_ai     , &
           n_bgc_N_sk    , n_bgc_C_sk    , &
           n_bgc_chl_sk  , n_bgc_Nit_sk  , &
           n_bgc_Am_sk   , n_bgc_Sil_sk  , &
           n_bgc_DMSPp_sk, n_bgc_DMSPd_sk, &
           n_bgc_DMS_sk  , n_bgc_Sil_ml  , &
           n_bgc_Nit_ml  , n_bgc_Am_ml   , &
           n_bgc_DMSP_ml , n_bgc_DMS_ml  , &
           n_bTin        , n_bphi        , &
           n_bgc_NO      , &
           n_bgc_N       , n_bgc_NH      , &
           n_bgc_C       , n_bgc_chl     , &
           n_bgc_DMSPp   , n_bgc_DMSPd   , &
           n_bgc_DMS     , n_bgc_Sil     , &
           n_bgc_S       , &
           n_fbri        , n_hbri        , &
           n_grownet     , n_PPnet       

!=======================================================================

      contains

!=======================================================================

      subroutine init_hist_bgc_2D

      use ice_broadcast, only: broadcast_scalar
      use ice_calendar, only: nstreams
      use ice_communicate, only: my_task, master_task
      use ice_constants, only: c0, c1
      use ice_exit, only: abort_ice
      use ice_fileunits, only: nu_nml, nml_filename, &
          get_fileunit, release_fileunit
      use ice_history_shared, only: tstr2D, tcstr, define_hist_field
      use ice_state, only: tr_aero, tr_iso, tr_brine

      integer (kind=int_kind) :: n, ns
      integer (kind=int_kind) :: nml_error ! namelist i/o error flag
      character (len=3) :: nchar
      character (len=16) :: vname_in     ! variable name

      !-----------------------------------------------------------------
      ! read namelist
      !-----------------------------------------------------------------

      call get_fileunit(nu_nml)
      if (my_task == master_task) then
         open (nu_nml, file=nml_filename, status='old',iostat=nml_error)
         if (nml_error /= 0) then
            nml_error = -1
         else
            nml_error =  1
         endif
         do while (nml_error > 0)
            read(nu_nml, nml=icefields_bgc_nml,iostat=nml_error)
         end do
         if (nml_error == 0) close(nu_nml)
      endif
      call release_fileunit(nu_nml)

      call broadcast_scalar(nml_error, master_task)
      if (nml_error /= 0) then
         close (nu_nml)
         call abort_ice('ice: error reading icefields_bgc_nml')
      endif

      if (.not. tr_aero) then
         f_faero_atm = 'x'
         f_faero_ocn = 'x'
         f_aero      = 'x' 
         f_aeron     = 'x' ! NOTE not implemented
      endif
      
      if (.not. tr_iso) then
         f_fiso_atm = 'x'
         f_fiso_ocn = 'x'
         f_iso      = 'x' 
         f_ison     = 'x' ! NOTE not implemented
      endif

      if (.not. tr_brine)  then
              f_fbri  = 'x'
              f_hbri  = 'x'
      endif
      if (.not. skl_bgc) then
          f_bgc_N_sk = 'x'
          f_bgc_C_sk = 'x'
          f_bgc_chl_sk = 'x'
          f_bgc_Nit_sk = 'x'
          f_bgc_Am_sk = 'x'
          f_bgc_Sil_sk = 'x'
          f_bgc_DMSPp_sk = 'x'
          f_bgc_DMSPd_sk = 'x'
          f_bgc_DMS_sk = 'x'
          f_bgc_Nit_ml = 'x'
          f_bgc_Am_ml = 'x'
          f_bgc_Sil_ml = 'x'
          f_bgc_DMSP_ml = 'x'
          f_bgc_DMS_ml = 'x'

          f_fNO = 'x'
          f_fNO_ai = 'x'
          f_fNH = 'x'
          f_fNH_ai = 'x'
          f_fN = 'x'
          f_fN_ai = 'x'
          f_fSil = 'x'
          f_fSil_ai = 'x'
          f_PPnet = 'x'
      endif
      if (.not. tr_bgc_C_sk) f_bgc_C_sk = 'x'
      if (.not. tr_bgc_chl_sk) f_bgc_chl_sk = 'x'
      if (.not. tr_bgc_Nit_sk) then
         f_bgc_Nit_sk = 'x'
         f_bgc_Nit_ml = 'x'
      endif
      if (.not. tr_bgc_Am_sk) then
         f_bgc_Am_sk = 'x'
         f_bgc_Am_ml = 'x'
      endif
      if (.not. tr_bgc_Sil_sk) then
         f_bgc_Sil_sk = 'x'
         f_bgc_Sil_ml = 'x'
      endif
      if (.not. tr_bgc_DMS_sk) then
         f_bgc_DMS_sk = 'x'
         f_bgc_DMSPp_sk = 'x'
         f_bgc_DMSPd_sk = 'x'
         f_bgc_DMS_ml = 'x'
         f_bgc_DMS_ml = 'x'
      endif

      call broadcast_scalar (f_faero_atm,    master_task)
      call broadcast_scalar (f_faero_ocn,    master_task)
      call broadcast_scalar (f_aero,         master_task)
      call broadcast_scalar (f_aeron,        master_task)
      call broadcast_scalar (f_fiso_atm,     master_task)
      call broadcast_scalar (f_fiso_ocn,     master_task)
      call broadcast_scalar (f_iso,          master_task)
      call broadcast_scalar (f_ison,         master_task)

      call broadcast_scalar (f_fbri,         master_task)
      call broadcast_scalar (f_hbri,         master_task)

      call broadcast_scalar (f_fNO,          master_task)
      call broadcast_scalar (f_fNO_ai,       master_task)
      call broadcast_scalar (f_fNH,          master_task)
      call broadcast_scalar (f_fNH_ai,       master_task)
      call broadcast_scalar (f_fN,           master_task)
      call broadcast_scalar (f_fN_ai,        master_task)
      call broadcast_scalar (f_fSil,         master_task)
      call broadcast_scalar (f_fSil_ai,      master_task)
      call broadcast_scalar (f_bgc_N_sk,     master_task)
      call broadcast_scalar (f_bgc_C_sk,     master_task)
      call broadcast_scalar (f_bgc_chl_sk,   master_task)
      call broadcast_scalar (f_bgc_Nit_sk,   master_task)
      call broadcast_scalar (f_bgc_Am_sk,    master_task)
      call broadcast_scalar (f_bgc_Sil_sk,   master_task)
      call broadcast_scalar (f_bgc_DMSPp_sk, master_task)
      call broadcast_scalar (f_bgc_DMSPd_sk, master_task)
      call broadcast_scalar (f_bgc_DMS_sk,   master_task)
      call broadcast_scalar (f_bgc_Nit_ml,   master_task)
      call broadcast_scalar (f_bgc_Am_ml,    master_task)
      call broadcast_scalar (f_bgc_Sil_ml,   master_task)
      call broadcast_scalar (f_bgc_DMSP_ml,  master_task)
      call broadcast_scalar (f_bgc_DMS_ml,   master_task)     
      call broadcast_scalar (f_bTin,         master_task)
      call broadcast_scalar (f_bphi,         master_task)
      call broadcast_scalar (f_PPnet,        master_task)
      call broadcast_scalar (f_grownet,      master_task)

      ! 2D variables
      do ns = 1, nstreams

      ! Aerosols
      if (f_aero(1:1) /= 'x') then
         do n=1,n_aero
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'aerosnossl', trim(nchar)
            call define_hist_field(n_aerosn1(n,:),vname_in,"kg/kg",   &
                tstr2D, tcstr,"snow ssl aerosol mass","none", c1, c0, &
                ns, f_aero)
            write(vname_in,'(a,a)') 'aerosnoint', trim(nchar)
            call define_hist_field(n_aerosn2(n,:),vname_in,"kg/kg",   &
                tstr2D, tcstr,"snow int aerosol mass","none", c1, c0, &
                ns, f_aero)
            write(vname_in,'(a,a)') 'aeroicessl', trim(nchar)
            call define_hist_field(n_aeroic1(n,:),vname_in,"kg/kg",  &
                tstr2D, tcstr,"ice ssl aerosol mass","none", c1, c0, &
                ns, f_aero)
            write(vname_in,'(a,a)') 'aeroiceint', trim(nchar)
            call define_hist_field(n_aeroic2(n,:),vname_in,"kg/kg",  &
                tstr2D, tcstr,"ice int aerosol mass","none", c1, c0, &
                ns, f_aero)
         enddo
      endif

      if (f_faero_atm(1:1) /= 'x') then
         do n=1,n_aero
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'faero_atm', trim(nchar)
            call define_hist_field(n_faero_atm(n,:),vname_in,"kg/m^2 s", &
                tstr2D, tcstr,"aerosol deposition rate","none", c1, c0,  &
                ns, f_faero_atm)
         enddo
      endif

      if (f_faero_ocn(1:1) /= 'x') then
         do n=1,n_aero
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'faero_ocn', trim(nchar)
            call define_hist_field(n_faero_ocn(n,:),vname_in,"kg/m^2 s", &
                tstr2D, tcstr,"aerosol flux to ocean","none", c1, c0,    &
                ns, f_faero_ocn)
         enddo
      endif

      ! Isotopes
      if (f_iso(1:1) /= 'x') then
         do n=1,n_iso
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'isosnossl', trim(nchar)
            call define_hist_field(n_isosn1(n,:),vname_in,"kg/kg",   &
                tstr2D, tcstr,"snow ssl isotope mass","none", c1, c0, &
                ns, f_iso)
            write(vname_in,'(a,a)') 'isosnoint', trim(nchar)
            call define_hist_field(n_isosn2(n,:),vname_in,"kg/kg",   &
                tstr2D, tcstr,"snow int isotope mass","none", c1, c0, &
                ns, f_iso)
            write(vname_in,'(a,a)') 'isoicessl', trim(nchar)
            call define_hist_field(n_isoic1(n,:),vname_in,"kg/kg",  &
                tstr2D, tcstr,"ice ssl istope mass","none", c1, c0, &
                ns, f_iso)
            write(vname_in,'(a,a)') 'isoiceint', trim(nchar)
            call define_hist_field(n_isoic2(n,:),vname_in,"kg/kg",  &
                tstr2D, tcstr,"ice int isotope mass","none", c1, c0, &
                ns, f_iso)
         enddo
      endif

      if (f_fiso_atm(1:1) /= 'x') then
         do n=1,n_iso
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'fiso_atm', trim(nchar)
            call define_hist_field(n_fiso_atm(n,:),vname_in,"kg/m^2 s", &
                tstr2D, tcstr,"isotope deposition rate","none", c1, c0, &
                ns, f_fiso_atm)
         enddo
      endif

      if (f_fiso_ocn(1:1) /= 'x') then
         do n=1,n_iso
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'fiso_ocn', trim(nchar)
            call define_hist_field(n_fiso_ocn(n,:),vname_in,"kg/m^2 s", &
                tstr2D, tcstr,"isotope flux to ocean","none", c1, c0, &
                ns, f_fiso_ocn)
         enddo
      endif

      ! skeletal layer tracers
      if (f_bgc_N_sk(1:1) /= 'x') &
         call define_hist_field(n_bgc_N_sk,"algal_N","mmol/m^2",tstr2D, tcstr, &
             "ice bottom algae (nitrogen)",                                    &
             "skeletal layer: bottom 2-3 cm", c1, c0,                &
             ns, f_bgc_N_sk)
      if (f_bgc_C_sk(1:1) /= 'x') &
         call define_hist_field(n_bgc_C_sk,"algal_C","mmol/m^2",tstr2D, tcstr, &
             "ice bottom algae (carbon)",                                      &
             "skeletal layer: bottom 2-3 cm", c1, c0,                &
             ns, f_bgc_C_sk)
      if (f_bgc_chl_sk(1:1) /= 'x') &
         call define_hist_field(n_bgc_chl_sk,"algal_chl","mmol/m^2?",tstr2D, tcstr, &
             "ice bottom algae (chlorophyll)",                                      &
             "skeletal layer: bottom 2-3 cm", c1, c0,                &
             ns, f_bgc_chl_sk)
      if (f_bgc_Nit_sk(1:1) /= 'x') &
         call define_hist_field(n_bgc_Nit_sk,"skl_Nit","mmol/m^2",tstr2D, tcstr, &
             "skeletal nutrient (nitrate)",                                      &
             "skeletal layer: bottom 2-3 cm", c1, c0,                &
             ns, f_bgc_Nit_sk)
      if (f_bgc_Am_sk(1:1) /= 'x') &
         call define_hist_field(n_bgc_Am_sk,"skl_Am","mmol/m^2",tstr2D, tcstr, &
             "skeletal nutrient (ammonia/um)",                                 &
             "skeletal layer: bottom 2-3 cm", c1, c0,                &
             ns, f_bgc_Am_sk)
      if (f_bgc_Sil_sk(1:1) /= 'x') &
         call define_hist_field(n_bgc_Sil_sk,"skl_Sil","mmol/m^2",tstr2D, tcstr, &
             "skeletal nutrient (silicate)",                                     &
             "skelelal layer: bottom 2-3 cm", c1, c0,                &
             ns, f_bgc_Sil_sk)
      if (f_bgc_Nit_ml(1:1) /= 'x') &
         call define_hist_field(n_bgc_Nit_ml,"ml_Nit","mmol/m^3",tstr2D, tcstr, &
             "mixed layer nutrient (nitrate)",                                  &
             "upper ocean", c1, c0,                                  &
             ns, f_bgc_Nit_ml)
      if (f_bgc_Am_ml(1:1) /= 'x') &
         call define_hist_field(n_bgc_Am_ml,"ml_Am","mmol/m^3",tstr2D, tcstr, &
             "mixed layer nutrient (ammonia/um)",                             &
             "upper ocean", c1, c0,                                  &
             ns, f_bgc_Am_ml)
      if (f_bgc_Sil_ml(1:1) /= 'x') &
         call define_hist_field(n_bgc_Sil_ml,"ml_Sil","mmol/m^3",tstr2D, tcstr, &
             "mixed layer nutrient (silicate)",                                 &
             "upper ocean", c1, c0,                                  &
             ns, f_bgc_Sil_ml)
      if (f_bgc_DMSPp_sk(1:1) /= 'x') &
         call define_hist_field(n_bgc_DMSPp_sk,"skl_DMSPp","mmol/m^2",tstr2D, tcstr, &
             "particulate S in algae (DMSPp)",                                       &
             "skeletal layer: bottom 2-3 cm", c1, c0,                &
             ns, f_bgc_DMSPp_sk)
      if (f_bgc_DMSPd_sk(1:1) /= 'x') &
         call define_hist_field(n_bgc_DMSPd_sk,"skl_DMSPd","mmol/m^2",tstr2D, tcstr, &
             "dissolved skl precursor (DSMPd)",                                      &
             "skeletal layer: bottom 2-3 cm", c1, c0,                &
             ns, f_bgc_DMSPd_sk)
      if (f_bgc_DMS_sk(1:1) /= 'x') &
         call define_hist_field(n_bgc_DMS_sk,"skl_DMS","mmol/m^2",tstr2D, tcstr, &
             "dissolved skl trace gas (DMS)",                                    &
             "skeletal layer: bottom 2-3 cm", c1, c0,                &
             ns, f_bgc_DMS_sk)
      if (f_bgc_DMSP_ml(1:1) /= 'x') &
         call define_hist_field(n_bgc_DMSP_ml,"ml_DMSP","mmol/m^3",tstr2D, tcstr, &
             "mixed layer precursor (DMSP)",                                      &
             "upper ocean", c1, c0,                                  &
             ns, f_bgc_DMSP_ml)
      if (f_bgc_DMS_ml(1:1) /= 'x') &
         call define_hist_field(n_bgc_DMS_ml,"ml_DMS","mmol/m^3",tstr2D, tcstr, &
             "mixed layer trace gas (DMS)",                                     &
             "upper ocean", c1, c0,                                  &
             ns, f_bgc_DMS_ml) 

      ! zbgc
      if (f_fNO(1:1) /= 'x') &
         call define_hist_field(n_fNO,"fNO","mmol/m^2/s",tstr2D, tcstr, &
             "nitrate flux ice to ocn (cpl)",                           &
             "if positive, ocean gains nitrate", c1, c0,                &
             ns, f_fNO)
      
      if (f_fNO_ai(1:1) /= 'x') &
         call define_hist_field(n_fNO_ai,"fNO_ai","mmol/m^2/s",tstr2D, tcstr, &
             "nitrate flux ice to ocean",                                     &
             "weighted by ice area", c1, c0,                                  &
             ns, f_fNO_ai)
      
      if (f_fNH(1:1) /= 'x') &
         call define_hist_field(n_fNH,"fNH","mmol/m^2/s",tstr2D, tcstr, &
             "ammonium flux ice to ocn (cpl)",                          &
             "if positive, ocean gains ammonium", c1, c0,               &
             ns, f_fNH)
      
      if (f_fNH_ai(1:1) /= 'x') &
         call define_hist_field(n_fNH_ai,"fNH_ai","mmol/m^2/s",tstr2D, tcstr, &
             "ammonium flux ice to ocean",                                    &
             "weighted by ice area", c1, c0,                                  &
             ns, f_fNH_ai)
      
      if (f_fN(1:1) /= 'x') &
         call define_hist_field(n_fN,"fN","mmol/m^2/s",tstr2D, tcstr, &
             "algal N flux ice to ocn (cpl)",                         &
             "if positive, ocean gains algal N", c1, c0,              &
             ns, f_fN)
      
      if (f_fN_ai(1:1) /= 'x') &
         call define_hist_field(n_fN_ai,"fN_ai","mmol/m^2/s",tstr2D, tcstr, &
             "algal N flux ice to ocean",                                   &
             "weighted by ice area", c1, c0,                                &
             ns, f_fN_ai)
      
      if (f_fSil(1:1) /= 'x') &
         call define_hist_field(n_fSil,"fSil","mmol/m^2/s",tstr2D, tcstr, &
             "silicate flux ice to ocn (cpl)",                            &
             "if positive, ocean gains silicate", c1, c0,                 &
             ns, f_fSil)
      
      if (f_fSil_ai(1:1) /= 'x') &
         call define_hist_field(n_fSil_ai,"fSil_ai","mmol/m^2/s",tstr2D, tcstr, &
             "silicate flux ice to ocean",                                      &
             "weighted by ice area", c1, c0,                                    &
             ns, f_fSil_ai)
      
     ! both skl and zbgc
       
      if (f_PPnet(1:1) /= 'x') &
         call define_hist_field(n_PPnet,"PP_net","mg C/d/m^2",tstr2D, tcstr, &
             "Net Primary Production",                                       &
             "weighted by ice area", secday, c0,                             &
             ns, f_PPnet)
      if (f_grownet(1:1) /= 'x') &
         call define_hist_field(n_grownet,"grow_net","/d",tstr2D, tcstr, &
             "Net specific growth",                                      &
             "weighted by ice area", secday, c0,                         &
             ns, f_grownet)

      ! brine
      if (f_hbri(1:1) /= 'x') &
         call define_hist_field(n_hbri,"hbrine","m",tstr2D, tcstr, &
             "Area-averaged brine height",                         &
             "distance from ice bottom to brine surface", c1, c0,  &
             ns, f_hbri)

      enddo ! nstreams
      
      end subroutine init_hist_bgc_2D

!=======================================================================

      subroutine init_hist_bgc_3Dc

      use ice_broadcast, only: broadcast_scalar
      use ice_calendar, only: nstreams
      use ice_constants, only: c0, c1
      use ice_history_shared, only: tstr3Dc, tcstr, define_hist_field

      integer (kind=int_kind) :: ns
      
      ! 3D (category) variables must be looped separately
      do ns = 1, nstreams
        if (f_fbri(1:1) /= 'x') &
         call define_hist_field(n_fbri,"fbrine","1",tstr3Dc, tcstr, &
             "brine tracer fraction of ice volume, cat",             &
             "none", c1, c0,       &
             ns, f_fbri)
      enddo ! ns

      end subroutine init_hist_bgc_3Dc

!=======================================================================

      subroutine init_hist_bgc_4Db

      use ice_calendar, only: nstreams
      use ice_constants, only: c0, c1, c100, secday
      use ice_history_shared, only: tstr4Db, tcstr, define_hist_field

      integer (kind=int_kind) :: ns
      
      ! biology vertical grid

      do ns = 1, nstreams
      
       if (f_bTin(1:1) /= 'x') &
            call define_hist_field(n_bTin,"bTizn","C",tstr4Db, tcstr, &
                "ice internal temperatures on bio grid", &
                "interpolated to bio grid", c1, c0,      &
                ns, f_bTin)
      
       if (f_bphi(1:1) /= 'x') &
            call define_hist_field(n_bphi,"bphizn","%",tstr4Db, tcstr, &
                "porosity", "brine volume fraction", c100, c0, &
                ns, f_bphi)
         
      enddo  !ns

      end subroutine init_hist_bgc_4Db

!=======================================================================

! write average ice quantities or snapshots

      subroutine accum_hist_bgc (iblk)

      use ice_blocks, only: block, get_block, nx_block, ny_block
      use ice_constants, only: c0, puny
      use ice_domain, only: blocks_ice
      use ice_domain_size, only: ncat, nblyr
      use ice_flux, only: faero_atm, faero_ocn, fiso_atm, fiso_ocn, sss
      use ice_history_shared, only: n2D, a2D, a3Dc, n3Dccum, a3Db, &
          n4Dscum, a4Db, &
          ncat_hist, accum_hist_field, nzblyr
      use ice_state, only: trcrn, trcr, aicen, vice, vicen, nt_aero, nt_iso, nt_fbri, &
          nt_bgc_N_sk, nt_bgc_C_sk, nt_bgc_chl_sk, nt_bgc_Nit_sk, &
          nt_bgc_Am_sk, nt_bgc_Sil_sk, nt_bgc_DMSPp_sk, nt_bgc_DMSPd_sk, &
          nt_bgc_DMS_sk, nt_bgc_Nit_ml, nt_bgc_Am_ml, nt_bgc_Sil_ml, &
          nt_bgc_DMSP_ml, nt_bgc_DMS_ml

      integer (kind=int_kind), intent(in) :: &
           iblk                 ! block index

      ! local variables

      integer (kind=int_kind) :: &
         i, j, n,           & ! loop indices
         ilo,ihi,jlo,jhi      ! beginning and end of physical domain

      real (kind=dbl_kind), dimension (nx_block,ny_block,nblyr+2,ncat) :: &
         workzn 

      type (block) :: &
         this_block           ! block information for current block

         this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi

      !---------------------------------------------------------------
      ! increment field
      !---------------------------------------------------------------

      ! Aerosols
      if (f_faero_atm(1:1) /= 'x') then
         do n=1,n_aero
            call accum_hist_field(n_faero_atm(n,:),iblk, &
                                    faero_atm(:,:,n,iblk), a2D)
         enddo
      endif
      if (f_faero_ocn(1:1) /= 'x') then
         do n=1,n_aero
            call accum_hist_field(n_faero_ocn(n,:),iblk, &
                                    faero_ocn(:,:,n,iblk), a2D)
         enddo
      endif
      if (f_aero(1:1) /= 'x') then
         do n=1,n_aero
            call accum_hist_field(n_aerosn1(n,:), iblk, &
                               trcr(:,:,nt_aero  +4*(n-1),iblk)/rhos, a2D)
            call accum_hist_field(n_aerosn2(n,:), iblk, &
                               trcr(:,:,nt_aero+1+4*(n-1),iblk)/rhos, a2D)
            call accum_hist_field(n_aeroic1(n,:), iblk, &
                               trcr(:,:,nt_aero+2+4*(n-1),iblk)/rhoi, a2D)
            call accum_hist_field(n_aeroic2(n,:), iblk, &
                               trcr(:,:,nt_aero+3+4*(n-1),iblk)/rhoi, a2D)
         enddo
      endif

      ! Isotopes
      if (f_fiso_atm(1:1) /= 'x') then
         do n=1,n_iso
            call accum_hist_field(n_fiso_atm(n,:),iblk, &
                                    fiso_atm(:,:,n,iblk), a2D)
         enddo
      endif
      if (f_fiso_ocn(1:1) /= 'x') then
         do n=1,n_iso
            call accum_hist_field(n_fiso_ocn(n,:),iblk, &
                                    fiso_ocn(:,:,n,iblk), a2D)
         enddo
      endif
      if (f_iso(1:1) /= 'x') then
         do n=1,n_iso
            call accum_hist_field(n_isosn1(n,:), iblk, &
                               trcr(:,:,nt_iso  +4*(n-1),iblk)/rhos, a2D)
            call accum_hist_field(n_isosn2(n,:), iblk, &
                               trcr(:,:,nt_iso+1+4*(n-1),iblk)/rhos, a2D)
            call accum_hist_field(n_isoic1(n,:), iblk, &
                               trcr(:,:,nt_iso+2+4*(n-1),iblk)/rhoi, a2D)
            call accum_hist_field(n_isoic2(n,:), iblk, &
                               trcr(:,:,nt_iso+3+4*(n-1),iblk)/rhoi, a2D)
         enddo
      endif

      ! skeletal layer bgc
      if (f_bgc_N_sk(1:1)/= 'x') &
         call accum_hist_field(n_bgc_N_sk,    iblk, &
                     trcr(:,:,nt_bgc_N_sk,    iblk), a2D)
  
      if (f_bgc_C_sk(1:1)/= 'x') &
         call accum_hist_field(n_bgc_C_sk,    iblk, &
                     trcr(:,:,nt_bgc_C_sk,    iblk), a2D)  
      if (f_bgc_chl_sk(1:1)/= 'x') &
         call accum_hist_field(n_bgc_chl_sk,  iblk, &
                     trcr(:,:,nt_bgc_chl_sk,  iblk), a2D)  
      if (f_bgc_Nit_sk(1:1)/= 'x') &
         call accum_hist_field(n_bgc_Nit_sk,  iblk, &
                     trcr(:,:,nt_bgc_Nit_sk,  iblk), a2D)  
      if (f_bgc_Am_sk(1:1)/= 'x') &
         call accum_hist_field(n_bgc_Am_sk,   iblk, &
                     trcr(:,:,nt_bgc_Am_sk,   iblk), a2D)  
      if (f_bgc_Sil_sk(1:1)/= 'x') &
         call accum_hist_field(n_bgc_Sil_sk,  iblk, &
                     trcr(:,:,nt_bgc_Sil_sk,  iblk), a2D)  
      if (f_bgc_DMSPp_sk(1:1)/= 'x') &
         call accum_hist_field(n_bgc_DMSPp_sk,iblk, &
                     trcr(:,:,nt_bgc_DMSPp_sk,iblk), a2D)  

      if (f_bgc_DMSPd_sk(1:1)/= 'x') &
         call accum_hist_field(n_bgc_DMSPd_sk,iblk, &
                     trcr(:,:,nt_bgc_DMSPd_sk,iblk), a2D)  
      if (f_bgc_DMS_sk(1:1)/= 'x') &
         call accum_hist_field(n_bgc_DMS_sk,  iblk, &
                     trcr(:,:,nt_bgc_DMS_sk,  iblk), a2D)  
      if (f_bgc_Nit_ml(1:1)/= 'x') &
         call accum_hist_field(n_bgc_Nit_ml,  iblk, &
               ocean_bio(:,:,nlt_bgc_NO,      iblk), a2D)  
      if (f_bgc_Am_ml(1:1)/= 'x') &
         call accum_hist_field(n_bgc_Am_ml,   iblk, &
               ocean_bio(:,:,nlt_bgc_NH,      iblk), a2D)  
      if (f_bgc_Sil_ml(1:1)/= 'x') &
         call accum_hist_field(n_bgc_Sil_ml,  iblk, &
               ocean_bio(:,:,nlt_bgc_Sil,     iblk), a2D)  
      if (f_bgc_DMSP_ml(1:1)/= 'x') &
         call accum_hist_field(n_bgc_DMSP_ml, iblk, &
               ocean_bio(:,:,nlt_bgc_DMSPp,   iblk), a2D)  
      if (f_bgc_DMS_ml(1:1)/= 'x') &
         call accum_hist_field(n_bgc_DMS_ml,  iblk, &
               ocean_bio(:,:,nlt_bgc_DMS,     iblk), a2D)  
 
      ! zbgc
      if (f_fNO  (1:1) /= 'x') &
         call accum_hist_field(n_fNO,     iblk, &
                  flux_bio(:,:,nlt_bgc_NO,iblk), a2D)
      if (f_fNO_ai(1:1)/= 'x') &
         call accum_hist_field(n_fNO_ai,  iblk, &
               flux_bio_ai(:,:,nlt_bgc_NO,iblk), a2D)

      if (f_fNH  (1:1) /= 'x') &
         call accum_hist_field(n_fNH,     iblk, &
                  flux_bio(:,:,nlt_bgc_NH,iblk), a2D)
      if (f_fNH_ai(1:1)/= 'x') &
         call accum_hist_field(n_fNH_ai,  iblk, &
               flux_bio_ai(:,:,nlt_bgc_NH,iblk), a2D)

      if (f_fN  (1:1) /= 'x') &
         call accum_hist_field(n_fN,      iblk, &
                   flux_bio(:,:,nlt_bgc_N,iblk), a2D)
      if (f_fN_ai(1:1)/= 'x') &
         call accum_hist_field(n_fN_ai,   iblk, &
                flux_bio_ai(:,:,nlt_bgc_N,iblk), a2D)

      if (f_fSil  (1:1) /= 'x') &
         call accum_hist_field(n_fSil,    iblk, &
                 flux_bio(:,:,nlt_bgc_Sil,iblk), a2D)
      if (f_fSil_ai(1:1)/= 'x') &
         call accum_hist_field(n_fSil_ai, iblk, &
              flux_bio_ai(:,:,nlt_bgc_Sil,iblk), a2D)
      if (f_PPnet  (1:1) /= 'x') &
         call accum_hist_field(n_PPnet,   iblk, &
                               PP_net(:,:,iblk), a2D)
      if (f_grownet  (1:1) /= 'x') &
         call accum_hist_field(n_grownet, iblk, &
                             grow_net(:,:,iblk), a2D)
      if (f_hbri  (1:1) /= 'x') &
         call accum_hist_field(n_hbri,    iblk, &
                                 hbri(:,:,iblk), a2D)

      ! 3D category fields

      if (f_fbri   (1:1) /= 'x') &
         call accum_hist_field(n_fbri-n2D, iblk, ncat_hist, &
                               trcrn(:,:,nt_fbri,1:ncat_hist,iblk), a3Dc)

      if (f_bTin  (1:1) /= 'x')  &
         call accum_hist_field(n_bTin-n4Dscum, iblk, nzblyr, ncat_hist, &
                               bTiz(:,:,1:nzblyr,1:ncat_hist,iblk), a4Db)

      if (f_bphi  (1:1) /= 'x') then
         workzn(:,:,:,:) = c0
         do n = 1, ncat_hist
            do j = jlo, jhi
               do i = ilo, ihi
                  if (aicen(i,j,n,iblk) > c0) then
                      workzn(i,j,1:nzblyr,n) = bphi(i,j,1:nzblyr,n,iblk)
                  endif
               enddo  !j
            enddo  !i
         enddo     !n
         call accum_hist_field(n_bphi-n4Dscum, iblk, nzblyr, ncat_hist, &
                               workzn(:,:,1:nzblyr,1:ncat_hist), a4Db)
      endif

      end subroutine accum_hist_bgc

!=======================================================================

      end module ice_history_bgc

!=======================================================================
