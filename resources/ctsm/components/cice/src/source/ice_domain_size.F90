!  SVN:$Id: ice_domain_size.F90 700 2013-08-15 19:17:39Z eclare $
!=======================================================================

! Defines the global domain size and number of categories and layers.
! Code originally based on domain_size.F in POP
!
! author Elizabeth C. Hunke, LANL
! 2004: Block structure and snow parameters added by William Lipscomb
!       Renamed (used to be ice_model_size)
! 2006: Converted to free source form (F90) by Elizabeth Hunke
!       Removed hardwired sizes (NX...can now be set in compile scripts)

      module ice_domain_size

      use ice_kinds_mod

!=======================================================================

      implicit none
      private

      integer (kind=int_kind), parameter, public :: &
        nx_global = NXGLOB    , & ! i-axis size
        ny_global = NYGLOB    , & ! j-axis size
        ncat      = NICECAT   , & ! number of categories
        nilyr     = NICELYR   , & ! number of ice layers per category
        nslyr     = NSNWLYR   , & ! number of snow layers per category

        max_aero  =   6       , & ! maximum number of aerosols 
        n_aero    = NTRAERO   , & ! number of aerosols in use
        max_iso   =   3       , & ! maximum number of isotopes
        n_iso     = NTRISO    , & ! number of isotopes in use

        nblyr     = NBGCLYR   , & ! number of bio/brine layers per category
        max_nbtrcr=   9       , & ! maximum number of biology tracers
!        nltrcr    = max_nbtrcr*TRBRI, & ! maximum layer bgc tracers (for zbgc)

        max_ntrcr =   1         & ! 1 = surface temperature              
                  + nilyr       & ! ice salinity
                  + nilyr       & ! ice enthalpy
                  + nslyr       & ! snow enthalpy
                              !!!!! optional tracers:
                  + TRAGE       & ! age
                  + TRFY        & ! first-year area
                  + TRLVL*2     & ! level/deformed ice
                  + TRPND*3     & ! ponds
                  + n_aero*4    & ! number of aerosols * 4 aero layers
                  + n_iso*4     & ! number of isotopes * 4 isotope layers
                  + TRBRI       & ! brine height
                  + TRBGCS    , & ! skeletal layer BGC
!                  + TRBGCZ*nltrcr*nblyr ! for zbgc (off if TRBRI=0)
        max_nstrm =   5           ! max number of history output streams

      integer (kind=int_kind), parameter, public :: &
        block_size_x = BLCKX  , & ! size of block in first horiz dimension
        block_size_y = BLCKY      ! size of block in second horiz dimension

   !*** The model will inform the user of the correct
   !*** values for the parameter below.  A value higher than
   !*** necessary will not cause the code to fail, but will
   !*** allocate more memory than is necessary.  A value that
   !*** is too low will cause the code to exit.  
   !*** A good initial guess is found using
   !*** max_blocks = (nx_global/block_size_x)*(ny_global/block_size_y)/
   !***               num_procs
 
      integer (kind=int_kind), parameter, public :: &
        max_blocks = MXBLCKS      ! max number of blocks per processor

!=======================================================================

      end module ice_domain_size

!=======================================================================
