module cam_diagnostics

!---------------------------------------------------------------------------------
! Module to compute a variety of diagnostics quantities for history files
!---------------------------------------------------------------------------------

#ifdef OSLO_AERO
#include <preprocessorDefinitions.h>
#endif

use shr_kind_mod,    only: r8 => shr_kind_r8
use camsrfexch,      only: cam_in_t, cam_out_t
use cam_control_mod, only: moist_physics
use physics_types,   only: physics_state, physics_tend
use ppgrid,          only: pcols, pver, begchunk, endchunk
use physics_buffer,  only: physics_buffer_desc, pbuf_add_field, dtype_r8
use physics_buffer,  only: dyn_time_lvls, pbuf_get_field, pbuf_get_index, pbuf_old_tim_idx

use cam_history,     only: outfld, write_inithist, hist_fld_active, inithist_all
use constituents,    only: pcnst, cnst_name, cnst_longname, cnst_cam_outfld
use constituents,    only: ptendnam, dmetendnam, apcnst, bpcnst, cnst_get_ind
use dycore,          only: dycore_is
use phys_control,    only: phys_getopts
use wv_saturation,   only: qsat, qsat_water, svp_ice
use time_manager,    only: is_first_step

use scamMod,         only: single_column, wfld
use cam_abortutils,  only: endrun

#ifdef OSLO_AERO
use opttab,        only: RF
#endif

implicit none
private
save

! Public interfaces

public :: &
   diag_readnl,              &! read namelist options
   diag_register,            &! register pbuf space
   diag_init,                &! initialization
   diag_allocate,            &! allocate memory for module variables
   diag_deallocate,          &! deallocate memory for module variables
   diag_conv_tend_ini,       &! initialize convective tendency calcs
   diag_phys_writeout,       &! output diagnostics of the dynamics
   diag_phys_tend_writeout,  &! output physics tendencies
   diag_state_b4_phys_write, &! output state before physics execution
   diag_conv,                &! output diagnostics of convective processes
   diag_surf,                &! output diagnostics of the surface
   diag_export,              &! output export state
   diag_physvar_ic,          &
   diag_phys_writeout_dry,   &! output diagnostics of the dynamics
   nsurf


! Private data

integer :: dqcond_num                     ! number of constituents to compute convective
character(len=16) :: dcconnam(pcnst)      ! names of convection tendencies
                                          ! tendencies for
real(r8), allocatable :: dtcond(:,:,:)    ! temperature tendency due to convection
type dqcond_t
   real(r8), allocatable :: cnst(:,:,:)   ! constituent tendency due to convection
end type dqcond_t
type(dqcond_t), allocatable :: dqcond(:)

character(len=8) :: diag_cnst_conv_tend = 'q_only' ! output constituent tendencies due to convection
                                                   ! 'none', 'q_only' or 'all'

integer, parameter :: surf_100000 = 1
integer, parameter :: surf_092500 = 2
integer, parameter :: surf_085000 = 3
integer, parameter :: surf_070000 = 4
integer, parameter :: nsurf = 4

logical          :: history_amwg                   ! output the variables used by the AMWG diag package
logical          :: history_vdiag                  ! output the variables used by the AMWG variability diag package
logical          :: history_eddy                   ! output the eddy variables
logical          :: history_budget                 ! output tendencies and state variables for CAM4
                                                   ! temperature, water vapor, cloud ice and cloud
                                                   ! liquid budgets.
integer          :: history_budget_histfile_num    ! output history file number for budget fields
logical          :: history_waccm                  ! outputs typically used for WACCM

! Physics buffer indices

integer  ::      psl_idx    = 0 
integer  ::      relhum_idx = 0 
integer  ::      qcwat_idx  = 0 
integer  ::      tcwat_idx  = 0 
integer  ::      lcwat_idx  = 0 
integer  ::      cld_idx    = 0 
integer  ::      concld_idx = 0 
integer  ::      tke_idx    = 0 
integer  ::      kvm_idx    = 0 
integer  ::      kvh_idx    = 0 
integer  ::      cush_idx   = 0 
integer  ::      t_ttend_idx = 0

integer  ::      prec_dp_idx  = 0
integer  ::      snow_dp_idx  = 0
integer  ::      prec_sh_idx  = 0
integer  ::      snow_sh_idx  = 0
integer  ::      prec_sed_idx = 0
integer  ::      snow_sed_idx = 0
integer  ::      prec_pcw_idx = 0
integer  ::      snow_pcw_idx = 0


integer :: tpert_idx=-1, qpert_idx=-1, pblh_idx=-1

integer :: trefmxav_idx = -1, trefmnav_idx = -1

contains

!==============================================================================

  subroutine diag_readnl(nlfile)
    use namelist_utils,  only: find_group_name
    use units,           only: getunit, freeunit
    use spmd_utils,      only: masterproc, masterprocid, mpi_character, mpicom

    character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

    ! Local variables
    integer :: unitn, ierr
    character(len=*), parameter :: subname = 'diag_readnl'

    namelist /cam_diag_opts/ diag_cnst_conv_tend
    !--------------------------------------------------------------------------

    if (masterproc) then
      unitn = getunit()
      open( unitn, file=trim(nlfile), status='old' )
      call find_group_name(unitn, 'cam_diag_opts', status=ierr)
      if (ierr == 0) then
        read(unitn, cam_diag_opts, iostat=ierr)
        if (ierr /= 0) then
          call endrun(subname // ':: ERROR reading namelist')
        end if
      end if
      close(unitn)
      call freeunit(unitn)
    end if

    ! Broadcast namelist variables
    call mpi_bcast(diag_cnst_conv_tend, len(diag_cnst_conv_tend), mpi_character, masterprocid, mpicom, ierr)

  end subroutine diag_readnl

!==============================================================================

  subroutine diag_register_dry()

    call pbuf_add_field('PSL', 'physpkg', dtype_r8, (/pcols/), psl_idx)

    ! Request physics buffer space for fields that persist across timesteps.
    call pbuf_add_field('T_TTEND', 'global', dtype_r8, (/pcols,pver,dyn_time_lvls/), t_ttend_idx)
  end subroutine diag_register_dry

  subroutine diag_register_moist()
    ! Request physics buffer space for fields that persist across timesteps.
    call pbuf_add_field('TREFMXAV', 'global', dtype_r8, (/pcols/), trefmxav_idx)
    call pbuf_add_field('TREFMNAV', 'global', dtype_r8, (/pcols/), trefmnav_idx)
  end subroutine diag_register_moist

  subroutine diag_register()
    call diag_register_dry()
    if (moist_physics) then
      call diag_register_moist()
    end if
  end subroutine diag_register

!==============================================================================
  
  subroutine diag_init_dry(pbuf2d)
    ! Declare the history fields for which this module contains outfld calls.

    use cam_history,        only: addfld, add_default, horiz_only
    use cam_history,        only: register_vector_field
    use constituent_burden, only: constituent_burden_init
    use physics_buffer,     only: pbuf_set_field
    use tidal_diag,         only: tidal_diag_init
!+
#ifdef AEROCOM
    use commondefinitions,  only: nbmodes 
!#ifdef RFMIPIRF
!    use radconstants,       only: nswbands, nlwbands
!#endif  
#endif  
!-

    type(physics_buffer_desc), pointer, intent(in) :: pbuf2d(:,:)

    integer :: k, m
    integer :: ixcldice, ixcldliq ! constituent indices for cloud liquid and ice water.
!AL
    integer :: ixcldni, ixcldnc ! constituent indices for cloud liquid and ice water.
!AL
    integer :: ierr

!+
#ifdef AEROCOM
    character(len=10) :: modeString
    character(len=20) :: varname
    integer :: i, irh
!#ifdef RFMIPIRF
!    character(len=2) :: c2
!    integer :: ib
!#endif
#endif  
!-

    ! outfld calls in diag_phys_writeout
    call addfld (cnst_name(1), (/ 'lev' /), 'A', 'kg/kg',    cnst_longname(1))
    call addfld ('NSTEP',      horiz_only,  'A', 'timestep', 'Model timestep')
    call addfld ('PHIS',       horiz_only,  'I', 'm2/s2',    'Surface geopotential')

    call addfld ('PS',         horiz_only,  'A', 'Pa',       'Surface pressure')
    call addfld ('T',          (/ 'lev' /), 'A', 'K',        'Temperature')
    call addfld ('U',          (/ 'lev' /), 'A', 'm/s',      'Zonal wind')
    call addfld ('UA010',      horiz_only,  'A', 'm/s',      'Zonal wind U at 10 mbar pressure surface')
    call addfld ('V',          (/ 'lev' /), 'A', 'm/s',      'Meridional wind')

    call register_vector_field('U','V')

    ! State before physics
    call addfld ('TBP',     (/ 'lev' /), 'A','K',             'Temperature (before physics)')
    call addfld (bpcnst(1), (/ 'lev' /), 'A','kg/kg',         trim(cnst_longname(1))//' (before physics)')
    ! State after physics
    call addfld ('TAP',     (/ 'lev' /), 'A','K',             'Temperature (after physics)'       )
    call addfld ('UAP',     (/ 'lev' /), 'A','m/s',           'Zonal wind (after physics)'        )
    call addfld ('VAP',     (/ 'lev' /), 'A','m/s',           'Meridional wind (after physics)'   )

    call register_vector_field('UAP','VAP')

    call addfld (apcnst(1), (/ 'lev' /), 'A','kg/kg',         trim(cnst_longname(1))//' (after physics)')
    if ( dycore_is('LR') .or. dycore_is('SE') ) then
      call addfld ('TFIX',    horiz_only,  'A', 'K/s',        'T fixer (T equivalent of Energy correction)')
!+tht
      call addfld ('EBREAK', horiz_only, 'A','W/m2',          'Global-mean energy-nonconservation (W/m2)')
      call addfld ('PTTEND_DME', (/ 'lev' /), 'A', 'K/s ', &
                   'T-tendency due to dry mass adjustment at the end of tphysac'    )
      call addfld ('IETEND_DME',  horiz_only, 'A','W/m2 ', &
                   'Column DSE tendency due to mass adjustment at end of tphysac'   )
      call addfld ('EFLX    '  ,  horiz_only, 'A','W/m2 ', &
                   'Material enthalpy flux due to mass adjustment at end of tphysac')
!-tht
    end if
    call addfld ('TTEND_TOT', (/ 'lev' /), 'A', 'K/s',        'Total temperature tendency')

    call addfld ('Z3',         (/ 'lev' /), 'A', 'm',         'Geopotential Height (above sea level)')
    call addfld ('Z1000',      horiz_only,  'A', 'm',         'Geopotential Z at 1000 mbar pressure surface')
    call addfld ('Z700',       horiz_only,  'A', 'm',         'Geopotential Z at 700 mbar pressure surface')
    call addfld ('Z500',       horiz_only,  'A', 'm',         'Geopotential Z at 500 mbar pressure surface')
    call addfld ('Z300',       horiz_only,  'A', 'm',         'Geopotential Z at 300 mbar pressure surface')
    call addfld ('Z200',       horiz_only,  'A', 'm',         'Geopotential Z at 200 mbar pressure surface')
    call addfld ('Z100',       horiz_only,  'A', 'm',         'Geopotential Z at 100 mbar pressure surface')
    call addfld ('Z050',       horiz_only,  'A', 'm',         'Geopotential Z at 50 mbar pressure surface')
    call addfld ('Z010',       horiz_only,  'A', 'm',         'Geopotential Z at 10 mbar pressure surface')

    call addfld ('ZZ',         (/ 'lev' /), 'A', 'm2',        'Eddy height variance' )
    call addfld ('VZ',         (/ 'lev' /), 'A', 'm2/s',      'Meridional transport of geopotential height')
    call addfld ('VT',         (/ 'lev' /), 'A', 'K m/s   ',  'Meridional heat transport')
    call addfld ('VT100',      horiz_only,  'A', 'K m/s   ',  'Meridional heat transport at 100 mbar pressure level')
    call addfld ('VU',         (/ 'lev' /), 'A', 'm2/s2',     'Meridional flux of zonal momentum' )
    call addfld ('VV',         (/ 'lev' /), 'A', 'm2/s2',     'Meridional velocity squared' )
    call addfld ('OMEGAV',     (/ 'lev' /), 'A', 'm Pa/s2 ',  'Vertical flux of meridional momentum' )
    call addfld ('OMGAOMGA',   (/ 'lev' /), 'A', 'Pa2/s2',    'Vertical flux of vertical momentum' )

    call addfld ('UU',         (/ 'lev' /), 'A', 'm2/s2',     'Zonal velocity squared' )
    call addfld ('WSPEED',     (/ 'lev' /), 'X', 'm/s',       'Horizontal total wind speed maximum' )
    call addfld ('WSPDSRFMX',  horiz_only,  'X', 'm/s',       'Horizontal total wind speed maximum at the surface' )
    call addfld ('WSPDSRFAV',  horiz_only,  'A', 'm/s',       'Horizontal total wind speed average at the surface' )

    call addfld ('OMEGA',      (/ 'lev' /), 'A', 'Pa/s',      'Vertical velocity (pressure)')
    call addfld ('OMEGAT',     (/ 'lev' /), 'A', 'K Pa/s  ',  'Vertical heat flux' )
    call addfld ('OMEGAU',     (/ 'lev' /), 'A', 'm Pa/s2 ',  'Vertical flux of zonal momentum' )
    call addfld ('OMEGA850',   horiz_only,  'A', 'Pa/s',      'Vertical velocity at 850 mbar pressure surface')
    call addfld ('OMEGA500',   horiz_only,  'A', 'Pa/s',      'Vertical velocity at 500 mbar pressure surface')

    call addfld ('PSL',        horiz_only,  'A', 'Pa','Sea level pressure')

    call addfld ('T1000',      horiz_only,  'A', 'K','Temperature at 1000 mbar pressure surface')
    call addfld ('T925',       horiz_only,  'A', 'K','Temperature at 925 mbar pressure surface')
    call addfld ('T850',       horiz_only,  'A', 'K','Temperature at 850 mbar pressure surface')
    call addfld ('T700',       horiz_only,  'A', 'K','Temperature at 700 mbar pressure surface')
    call addfld ('T500',       horiz_only,  'A', 'K','Temperature at 500 mbar pressure surface')
    call addfld ('T400',       horiz_only,  'A', 'K','Temperature at 400 mbar pressure surface')
    call addfld ('T300',       horiz_only,  'A', 'K','Temperature at 300 mbar pressure surface')
    call addfld ('T200',       horiz_only,  'A', 'K','Temperature at 200 mbar pressure surface')
    call addfld ('T010',       horiz_only,  'A', 'K','Temperature at 10 mbar pressure surface')

    call addfld ('T7001000',   horiz_only,  'A', 'K','Temperature difference 700 mb - 1000 mb')
    call addfld ('TH7001000',  horiz_only,  'A', 'K','Theta difference 700 mb - 1000 mb')
    call addfld ('THE7001000', horiz_only,  'A', 'K','ThetaE difference 700 mb - 1000 mb')

    call addfld ('T8501000',   horiz_only,  'A', 'K','Temperature difference 850 mb - 1000 mb')
    call addfld ('TH8501000',  horiz_only,  'A', 'K','Theta difference 850 mb - 1000 mb')
    call addfld ('T9251000',   horiz_only,  'A', 'K','Temperature difference 925 mb - 1000 mb')
    call addfld ('TH9251000',  horiz_only,  'A', 'K','Theta difference 925 mb - 1000 mb')

    call addfld ('TT',         (/ 'lev' /), 'A', 'K2','Eddy temperature variance' )

    call addfld ('U850',       horiz_only,  'A', 'm/s','Zonal wind at 850 mbar pressure surface')
    call addfld ('U500',       horiz_only,  'A', 'm/s','Zonal wind at 500 mbar pressure surface')
    call addfld ('U250',       horiz_only,  'A', 'm/s','Zonal wind at 250 mbar pressure surface')
    call addfld ('U200',       horiz_only,  'A', 'm/s','Zonal wind at 200 mbar pressure surface')
    call addfld ('U010',       horiz_only,  'A', 'm/s','Zonal wind at  10 mbar pressure surface')
    call addfld ('V850',       horiz_only,  'A', 'm/s','Meridional wind at 850 mbar pressure surface')
    call addfld ('V500',       horiz_only,  'A', 'm/s','Meridional wind at 500 mbar pressure surface')
    call addfld ('V250',       horiz_only,  'A', 'm/s','Meridional wind at 250 mbar pressure surface')
    call addfld ('V200',       horiz_only,  'A', 'm/s','Meridional wind at 200 mbar pressure surface')

    call register_vector_field('U850', 'V850')
    call register_vector_field('U500', 'V500')
    call register_vector_field('U250', 'V250')
    call register_vector_field('U200', 'V200')

    call addfld ('UBOT',       horiz_only,  'A', 'm/s','Lowest model level zonal wind')
    call addfld ('VBOT',       horiz_only,  'A', 'm/s','Lowest model level meridional wind')
    call register_vector_field('UBOT', 'VBOT')

    call addfld ('ZBOT',       horiz_only,  'A', 'm','Lowest model level height')

    call addfld ('ATMEINT',    horiz_only,  'A', 'J/m2','Vertically integrated total atmospheric energy ')

!akc6+  CNVCLD is zero... 
!    call addfld ('CNVCLD',     horiz_only,  'A', 'fraction', 'Vertically integrated convective cloud cover')
!akc6-


#ifdef OSLO_AERO

#ifdef DIRIND
    call addfld ('AOD_VIS ',horiz_only, 'A','unitless','Aerosol optical depth at 0.442-0.625um') ! CAM4-Oslo: 0.35-0.64um
    call addfld ('ABSVIS  ',horiz_only, 'A','unitless','Aerosol absorptive optical depth at 0.442-0.625um') ! CAM4-Oslo: 0.35-0.64um
   call addfld ('AODVVOLC ',horiz_only, 'A','unitless','CMIP6 volcanic aerosol optical depth at 0.442-0.625um') ! CAM4-Oslo: 0.35-0.64um
   call addfld ('ABSVVOLC ',horiz_only, 'A','unitless','CMIP6 volcanic aerosol absorptive optical depth at 0.442-0.625um') ! CAM4-Oslo: 0.35-0.64um
    call addfld ('CAODVIS ',horiz_only, 'A','unitless','Clear air aerosol optical depth')  
    call addfld ('CABSVIS ',horiz_only, 'A','unitless','Clear air aerosol absorptive optical depth')
    call addfld ('CLDFREE ',horiz_only, 'A','unitless','Cloud free fraction wrt CAODVIS and CABSVIS')
    call addfld ('DAYFOC  ',horiz_only, 'A','unitless','Daylight fraction')
    call addfld ('N_AER   ',(/'lev'/),  'A', 'unitless','Aerosol number concentration')
!-   call addfld ('N_AERORG','unitless',pver, 'A','Aerosol number concentration',phys_decomp)
    call addfld ('SSAVIS  ',(/'lev'/),  'A','unitless','Aerosol single scattering albedo in visible wavelength band')    
    call addfld ('ASYMMVIS',(/'lev'/),  'A','unitless','Aerosol assymetry factor in visible wavelength band')    
    call addfld ('EXTVIS  ',(/'lev'/),  'A','1/km    ','Aerosol extinction')     
!=0    call addfld ('RELH    ',(/'lev'/),  'A', 'unitless','Fictive relative humidity')
!akc6+
   call addfld ('BVISVOLC ',(/'lev'/),   'A','1/km    ','CMIP6 volcanic aerosol extinction at 0.442-0.625um')
!akc6-
!#ifdef SPAERO
!   call addfld ('AODVISSP',horiz_only, 'A','unitless' ,'Simple plumes aerosol optical depth at 0.35-0.64um')  
!   call addfld ('ABSVISSP',horiz_only, 'A','unitless' ,'Simple plumes aerosol absorptive optical depth at 0.35-0.64um')
!   call addfld ('XCDNC_SP',horiz_only, 'A','unitless' ,'CDNC modification factor for simple plume aerosols')
!   call addfld ('AODV3DSP',(/'lev'/),  'A','unitless','Simple plumes 3D aerosol optical depth at 0.35-0.64um')    
!   call addfld ('ABSV3DSP',(/'lev'/),  'A','unitless','Simple plumes 3D absorption AOD at 0.35-0.64um')    
!#endif
#ifdef COLTST4INTCONS 
! optical depth for each mode/mixture:
    call addfld ('TAUKC0 ',horiz_only, 'A','unitless','Aerosol optical depth at 0.442-0.625um for kcomp 0')
    call addfld ('TAUKC1 ',horiz_only, 'A','unitless','Aerosol optical depth at 0.442-0.625um for kcomp 1')
    call addfld ('TAUKC2 ',horiz_only, 'A','unitless','Aerosol optical depth at 0.442-0.625um for kcomp 2')
    call addfld ('TAUKC4 ',horiz_only, 'A','unitless','Aerosol optical depth at 0.442-0.625um for kcomp 4')
    call addfld ('TAUKC5 ',horiz_only, 'A','unitless','Aerosol optical depth at 0.442-0.625um for kcomp 5')
    call addfld ('TAUKC6 ',horiz_only, 'A','unitless','Aerosol optical depth at 0.442-0.625um for kcomp 6')
    call addfld ('TAUKC7 ',horiz_only, 'A','unitless','Aerosol optical depth at 0.442-0.625um for kcomp 7')
    call addfld ('TAUKC8 ',horiz_only, 'A','unitless','Aerosol optical depth at 0.442-0.625um for kcomp 8')
    call addfld ('TAUKC9 ',horiz_only, 'A','unitless','Aerosol optical depth at 0.442-0.625um for kcomp 9')
    call addfld ('TAUKC10',horiz_only, 'A','unitless','Aerosol optical depth at 0.442-0.625um for kcomp 10')
    call addfld ('TAUKC12',horiz_only, 'A','unitless','Aerosol optical depth at 0.442-0.625um for kcomp 12')
    call addfld ('TAUKC14',horiz_only, 'A','unitless','Aerosol optical depth at 0.442-0.625um for kcomp 14')
! mass specific extinction (including condensed water) for each mode/mixture:
    call addfld ('MECKC0 ',(/'lev'/), 'A','m2/g','Aerosol MEC at 0.442-0.625um for kcomp 0')
    call addfld ('MECKC1 ',(/'lev'/), 'A','m2/g','Aerosol MEC at 0.442-0.625um for kcomp 1')
    call addfld ('MECKC2 ',(/'lev'/), 'A','m2/g','Aerosol MEC at 0.442-0.625um for kcomp 2')
    call addfld ('MECKC4 ',(/'lev'/), 'A','m2/g','Aerosol MEC at 0.442-0.625um for kcomp 4')
    call addfld ('MECKC5 ',(/'lev'/), 'A','m2/g','Aerosol MEC at 0.442-0.625um for kcomp 5')
    call addfld ('MECKC6 ',(/'lev'/), 'A','m2/g','Aerosol MEC at 0.442-0.625um for kcomp 6')
    call addfld ('MECKC7 ',(/'lev'/), 'A','m2/g','Aerosol MEC at 0.442-0.625um for kcomp 7')
    call addfld ('MECKC8 ',(/'lev'/), 'A','m2/g','Aerosol MEC at 0.442-0.625um for kcomp 8')
    call addfld ('MECKC9 ',(/'lev'/), 'A','m2/g','Aerosol MEC at 0.442-0.625um for kcomp 9')
    call addfld ('MECKC10',(/'lev'/), 'A','m2/g','Aerosol MEC at 0.442-0.625um for kcomp 10')
    call addfld ('MECKC12',(/'lev'/), 'A','m2/g','Aerosol MEC at 0.442-0.625um for kcomp 12')
    call addfld ('MECKC14',(/'lev'/), 'A','m2/g','Aerosol MEC at 0.442-0.625um for kcomp 14')
#ifdef AEROCOM
! dry mass for  each mode/mixture (for calculation of specific extinction without condensed water):
    call addfld ('CMDRY0 ',horiz_only, 'A','unitless','Total dry mass load for kcomp 0')
    call addfld ('CMDRY1 ',horiz_only, 'A','unitless','Total dry mass load for kcomp 1')
    call addfld ('CMDRY2 ',horiz_only, 'A','unitless','Total dry mass load for kcomp 2')
    call addfld ('CMDRY4 ',horiz_only, 'A','unitless','Total dry mass load for kcomp 4')
    call addfld ('CMDRY5 ',horiz_only, 'A','unitless','Total dry mass load for kcomp 5')
    call addfld ('CMDRY6 ',horiz_only, 'A','unitless','Total dry mass load for kcomp 6')
    call addfld ('CMDRY7 ',horiz_only, 'A','unitless','Total dry mass load for kcomp 7')
    call addfld ('CMDRY8 ',horiz_only, 'A','unitless','Total dry mass load for kcomp 8')
    call addfld ('CMDRY9 ',horiz_only, 'A','unitless','Total dry mass load for kcomp 9')
    call addfld ('CMDRY10',horiz_only, 'A','unitless','Total dry mass load for kcomp 10')
    call addfld ('CMDRY12',horiz_only, 'A','unitless','Total dry mass load for kcomp 12')
    call addfld ('CMDRY14',horiz_only, 'A','unitless','Total dry mass load for kcomp 14')
#endif !aerocom
#endif !extra tests
#ifdef AEROFFL
    call addfld ('FSNT_DRF',horiz_only, 'A','W/m^2','Total column absorbed solar flux (DIRind)')
    call addfld ('FSNTCDRF',horiz_only, 'A','W/m^2','Clear sky total column absorbed solar flux (DIRind)' )
    call addfld ('FSNS_DRF',horiz_only, 'A','W/m^2   ','Surface absorbed solar flux (DIRind)' )
    call addfld ('FSNSCDRF',horiz_only, 'A','W/m^2   ','Clear sky surface absorbed solar flux (DIRind)' )
    call addfld ('QRS_DRF ',(/'lev'/), 'A','K/s     ','Solar heating rate (DIRind)')
    call addfld ('QRSC_DRF',(/'lev'/), 'A','K/s     ','Clearsky solar heating rate (DIRind)' )
    call addfld ('FLNT_DRF',horiz_only, 'A','W/m^2   ','Total column longwave flux (DIRind)' )
    call addfld ('FLNTCDRF',horiz_only, 'A','W/m^2   ','Clear sky total column longwave flux (DIRind)' )
    call addfld ('FSUTADRF',horiz_only, 'A','W/m^2   ','SW upwelling flux at TOA')
    call addfld ('FSDS_DRF',horiz_only, 'A','W/m^2   ','SW downelling flux at surface')
    call addfld ('FSUS_DRF',horiz_only, 'A','W/m^2   ','SW upwelling flux at surface')
    call addfld ('FSDSCDRF',horiz_only, 'A','W/m^2   ','SW downwelling clear sky flux at surface')
    call addfld ('FLUS    ',horiz_only, 'A','W/m^2   ','LW surface upwelling flux')
!->ut    call addfld ('FLNT_ORG',horiz_only, 'A','W/m^2   ','Total column longwave flux (CAM5)' )
#endif  ! aeroffl
#ifdef AEROCOM 
      call addfld ('AKCXS   ',horiz_only, 'A','mg/m2   ','Scheme excess aerosol mass burden')     
      call addfld ('PMTOT   ',horiz_only, 'A','ug/m3   ','Aerosol PM, all sizes')
      call addfld ('PM25    ',horiz_only, 'A','ug/m3   ','Aerosol PM2.5')
!akc6+
      call addfld ('PM2P5   ',(/'lev'/), 'A','ug/m3   ','3D aerosol PM2.5')
      call addfld ('MMRPM2P5',(/'lev'/), 'A','kg/kg   ','3D aerosol PM2.5 mass mixing ratio')
      call addfld ('MMRPM1  ',(/'lev'/), 'A','kg/kg   ','3D aerosol PM1.0 mass mixing ratio')
      call addfld ('MMRPM2P5_SRF',horiz_only, 'A','kg/kg   ','Aerosol PM2.5 mass mixing ratio in bottom layer')   
!akc6-
      call addfld ('GRIDAREA',horiz_only, 'A','m2      ','Grid area for 1.9x2.5 horizontal resolution')
      call addfld ('DAERH2O ',horiz_only, 'A', 'mg/m2   ','Aerosol water load')
      call addfld ('MMR_AH2O',(/'lev'/), 'A', 'kg/kg   ','Aerosol water mmr')
      call addfld ('ECDRYAER',(/'lev'/), 'A', 'kg/kg   ','Dry aerosol extinction at 550nm')  
      call addfld ('ABSDRYAE',(/'lev'/), 'A','m-1     ','Dry aerosol absorption at 550nm')  
      call addfld ('ECDRY440',(/'lev'/), 'A','m-1     ','Dry aerosol extinction at 440nm')  
      call addfld ('ABSDR440',(/'lev'/),'A','m-1     ','Dry aerosol absorption at 440nm')  
      call addfld ('ECDRY870',(/'lev'/),'A','m-1     ','Dry aerosol extinction at 870nm')  
      call addfld ('ABSDR870',(/'lev'/),'A','m-1     ','Dry aerosol absorption at 870nm')  
      call addfld ('ASYMMDRY',(/'lev'/),'A','unitless','Dry asymmetry factor in visible wavelength band')  
      call addfld ('ECDRYLT1',(/'lev'/),'A','m-1     ','Dry aerosol extinction at 550nm lt05')
      call addfld ('ABSDRYBC',(/'lev'/),'A','m-1     ','Dry BC absorption at 550nm')
      call addfld ('ABSDRYOC',(/'lev'/),'A','m-1     ','Dry OC absorption at 550nm')
      call addfld ('ABSDRYSU',(/'lev'/),'A','m-1     ','Dry sulfate absorption at 550nm')
      call addfld ('ABSDRYSS',(/'lev'/),'A','m-1     ','Dry sea-salt absorption at 550nm')
      call addfld ('ABSDRYDU',(/'lev'/),'A','m-1     ','Dry dust absorption at 550nm')
      call addfld ('OD550DRY',horiz_only,'A','unitless','Dry aerosol optical depth at 550nm')  
      call addfld ('AB550DRY',horiz_only, 'A','unitless','Dry aerosol absorptive optical depth at 550nm')   
      call addfld ('DERLT05 ',horiz_only, 'A','um      ','Effective aerosol dry radius<0.5um')
      call addfld ('DERGT05 ',horiz_only, 'A','um      ','Effective aerosol dry radius>0.5um') 
      call addfld ('DER     ',horiz_only, 'A','um      ','Effective aerosol dry radius') 
      call addfld ('DOD440  ',horiz_only, 'A', 'unitless','Aerosol optical depth at 440nm')  
      call addfld ('ABS440  ',horiz_only, 'A', 'unitless','Aerosol absorptive optical depth at 440nm')   
      call addfld ('DOD500  ',horiz_only, 'A', 'unitless','Aerosol optical depth at 500nm')  
      call addfld ('ABS500  ',horiz_only, 'A', 'unitless','Aerosol absorptive optical depth at 500nm')   
      call addfld ('DOD550  ',horiz_only, 'A','unitless','Aerosol optical depth at 550nm')  
!tst
!      call addfld ('DOD5503D',(/'lev'/),'A','unitless','3D aerosol optical depth at 550 nm')  
!      call addfld ('AODVIS3D',(/'lev'/),'A','unitless','3D aerosol optical depth in visible wavelength band')  
!tst
      call addfld ('ABS550  ',horiz_only, 'A','unitless','Aerosol absorptive optical depth at 550nm')   
      call addfld ('ABS550AL',horiz_only, 'A','unitless','Alt. aerosol absorptive optical depth at 550nm')   
      call addfld ('DOD670  ',horiz_only, 'A','unitless','Aerosol optical depth at 670nm')  
      call addfld ('ABS670  ',horiz_only, 'A','unitless','Aerosol absorptive optical depth at 670nm')   
      call addfld ('DOD870  ',horiz_only, 'A','unitless','Aerosol optical depth at 870nm')  
      call addfld ('ABS870  ',horiz_only, 'A','unitless','Aerosol absorptive optical depth at 870nm')   
      call addfld ('DLOAD_MI',horiz_only, 'A','mg/m2   ','mineral aerosol load')     
      call addfld ('DLOAD_SS',horiz_only, 'A','mg/m2   ','sea-salt aerosol load')     
      call addfld ('DLOAD_S4',horiz_only, 'A','mg/m2   ','sulfate aerosol load')     
      call addfld ('DLOAD_OC',horiz_only, 'A','mg/m2   ','OC aerosol load')     
      call addfld ('DLOAD_BC',horiz_only, 'A','mg/m2   ','BC aerosol load')     

      call addfld ('LOADBCAC',horiz_only, 'A','mg/m2   ','BC aerosol coag load')     
      call addfld ('LOADBC0 ',horiz_only, 'A','mg/m2   ','BC aerosol mode 0 load')     
      call addfld ('LOADBC2 ',horiz_only, 'A','mg/m2   ','BC aerosol mode 2 load')     
      call addfld ('LOADBC4 ',horiz_only, 'A','mg/m2   ','BC aerosol mode 4 load')     
      call addfld ('LOADBC12',horiz_only, 'A','mg/m2   ','BC aerosol mode 12 load')     
      call addfld ('LOADBC14',horiz_only, 'A','mg/m2   ','BC aerosol mode 14 load')     
      call addfld ('LOADOCAC',horiz_only, 'A','mg/m2   ','OC aerosol coag load')     
      call addfld ('LOADOC3 ',horiz_only, 'A','mg/m2   ','OC aerosol mode 3 load')     
      call addfld ('LOADOC4 ',horiz_only, 'A','mg/m2   ','OC aerosol mode 4 load')     
      call addfld ('LOADOC13',horiz_only, 'A','mg/m2   ','OC aerosol mode 13 load')     
      call addfld ('LOADOC14',horiz_only, 'A','mg/m2   ','OC aerosol mode 14 load')     
#ifdef  COLTST4INTCONS
      call addfld ('COLRBC0 ',horiz_only, 'A','unitless','COLRAT BC mode 0 load ratio')     
      call addfld ('COLRBC2 ',horiz_only, 'A','unitless','COLRAT BC mode 2 load ratio')     
      call addfld ('COLRBC4 ',horiz_only, 'A','unitless','COLRAT BC mode 4 load ratio')     
      call addfld ('COLRBC12',horiz_only, 'A','unitless','COLRAT BC mode 12 load ratio')     
      call addfld ('COLRBC14',horiz_only, 'A','unitless','COLRAT BC mode 14 load ratio')     
      call addfld ('COLRBCAC',horiz_only, 'A','unitless','COLRAT BC mode AC load ratio')     
      call addfld ('COLROC4 ',horiz_only, 'A','unitless','COLRAT OC mode 4 load ratio')     
      call addfld ('COLROC14',horiz_only, 'A','unitless','COLRAT OC mode 14 load ratio')     
      call addfld ('COLROCAC',horiz_only, 'A','unitless','COLRAT OC mode AC load ratio')     
      call addfld ('COLRSULA',horiz_only, 'A','unitless','COLRAT Sulfate mode A load ratio')     
      call addfld ('COLRSUL1',horiz_only, 'A','unitless','COLRAT Sulfate mode 1 load ratio')     
      call addfld ('COLRSUL5',horiz_only, 'A','unitless','COLRAT Sulfate mode 5 load ratio')     
#endif  ! COLTST4INTCONS

!
      call addfld ('EC550AER',(/'lev'/),'A','m-1     ','aerosol extinction coefficient')     
      call addfld ('ABS550_A',(/'lev'/),'A','m-1     ','aerosol absorption coefficient')     
      call addfld ('BS550AER',(/'lev'/),'A','m-1 sr-1','aerosol backscatter coefficient')     
!
      call addfld ('EC550SO4',(/'lev'/),'A','m-1     ','SO4 aerosol extinction coefficient')     
      call addfld ('EC550BC ',(/'lev'/),'A','m-1     ','BC aerosol extinction coefficient')     
      call addfld ('EC550POM',(/'lev'/), 'A','m-1    ','POM aerosol extinction coefficient')     
      call addfld ('EC550SS ',(/'lev'/), 'A','m-1    ','SS aerosol extinction coefficient')     
      call addfld ('EC550DU ',(/'lev'/), 'A','m-1    ','DU aerosol extinction coefficient')     
!
      call addfld ('CDOD440  ',horiz_only, 'A','unitless','Clear air Aerosol optical depth at 440nm')  
      call addfld ('CDOD550  ',horiz_only, 'A','unitless','Clear air Aerosol optical depth at 550nm')  
      call addfld ('CABS550  ',horiz_only, 'A','unitless','Clear air Aerosol abs optical depth at 550nm')  
      call addfld ('CABS550A ',horiz_only, 'A','unitless','Clear air Aerosol abs optical depth at 550nm')  
      call addfld ('CDOD870 ' ,horiz_only, 'A','unitless','Clear air Aerosol optical depth at 870nm')  
      call addfld ('A550_DU ' ,horiz_only, 'A','unitless', 'mineral abs. aerosol optical depth 550nm')     
      call addfld ('A550_SS ' ,horiz_only, 'A','unitless','sea-salt abs aerosol optical depth 550nm')     
      call addfld ('A550_SO4' ,horiz_only, 'A','unitless','SO4 aerosol abs. optical depth 550nm')     
      call addfld ('A550_POM' ,horiz_only, 'A','unitless', 'OC abs. aerosol optical depth 550nm')     
      call addfld ('A550_BC ' ,horiz_only, 'A','unitless', 'BC abs. aerosol optical depth 550nm')
      call addfld ('D440_DU ',horiz_only, 'A','unitless','mineral aerosol optical depth 440nm')     
      call addfld ('D440_SS ',horiz_only, 'A','unitless','sea-salt aerosol optical depth 440nm')     
      call addfld ('D440_SO4',horiz_only, 'A','unitless','SO4 aerosol optical depth 440nm')     
      call addfld ('D440_POM',horiz_only, 'A','unitless','OC aerosol optical depth 440nm')     
      call addfld ('D440_BC ',horiz_only, 'A','unitless','BC aerosol optical depth 440nm')     
      call addfld ('D500_DU ',horiz_only, 'A','unitless','mineral aerosol optical depth 500nm')     
      call addfld ('D500_SS ',horiz_only, 'A','unitless','sea-salt aerosol optical depth 500nm')     
      call addfld ('D500_SO4',horiz_only, 'A','unitless','SO4 aerosol optical depth 500nm')     
      call addfld ('D500_POM',horiz_only, 'A','unitless','OC aerosol optical depth 500nm')     
      call addfld ('D500_BC ',horiz_only, 'A','unitless','BC aerosol optical depth 500nm')     
      call addfld ('D550_DU ',horiz_only, 'A','unitless','mineral aerosol optical depth 550nm')     
      call addfld ('D550_SS ',horiz_only, 'A','unitless','sea-salt aerosol optical depth 550nm')     
      call addfld ('D550_SO4',horiz_only, 'A','unitless','SO4 aerosol optical depth 550nm')     
      call addfld ('D550_POM',horiz_only, 'A','unitless','OC aerosol optical depth 550nm')     
      call addfld ('D550_BC ',horiz_only, 'A','unitless','BC aerosol optical depth 550nm')     
      call addfld ('D670_DU ',horiz_only, 'A','unitless','mineral aerosol optical depth 670nm')     
      call addfld ('D670_SS ',horiz_only, 'A','unitless','sea-salt aerosol optical depth 670nm')     
      call addfld ('D670_SO4',horiz_only, 'A','unitless','SO4 aerosol optical depth 670nm')     
      call addfld ('D670_POM',horiz_only, 'A','unitless','OC aerosol optical depth 670nm')     
      call addfld ('D670_BC ',horiz_only, 'A','unitless','BC aerosol optical depth 670nm')     
      call addfld ('D870_DU ',horiz_only, 'A','unitless','mineral aerosol optical depth 870nm')     
      call addfld ('D870_SS ',horiz_only, 'A','unitless','sea-salt aerosol optical depth 870nm')     
      call addfld ('D870_SO4',horiz_only, 'A','unitless','SO4 aerosol optical depth 870nm')     
      call addfld ('D870_POM',horiz_only, 'A','unitless','OC aerosol optical depth 870nm')     
      call addfld ('D870_BC ',horiz_only, 'A','unitless','BC aerosol optical depth 870nm')     
      call addfld ('DLT_DUST',horiz_only, 'A','unitless','mineral aerosol optical depth 550nm lt05')     
      call addfld ('DLT_SS  ',horiz_only, 'A','unitless','sea-salt aerosol optical depth 550nm lt05')     
      call addfld ('DLT_SO4 ',horiz_only, 'A','unitless','SO4 aerosol optical depth 550nm lt05')     
      call addfld ('DLT_POM ',horiz_only, 'A','unitless','OC aerosol optical depth 550nm lt05')     
      call addfld ('DLT_BC  ',horiz_only, 'A','unitless','BC aerosol optical depth 550nm lt05')     
      call addfld ('DGT_DUST',horiz_only, 'A','unitless','mineral aerosol optical depth 550nm gt05')     
      call addfld ('DGT_SS  ',horiz_only, 'A','unitless','sea-salt aerosol optical depth 550nm gt05')     
      call addfld ('DGT_SO4 ',horiz_only, 'A','unitless','SO4 aerosol optical depth 550nm gt05')     
      call addfld ('DGT_POM ',horiz_only, 'A','unitless','OC aerosol optical depth 550nm gt05')     
      call addfld ('DGT_BC  ',horiz_only, 'A','unitless','BC aerosol optical depth 550nm gt05')     
      call addfld ('AIRMASS ',horiz_only, 'A','kg/m2   ','Vertically integrated airmass')     !akc6
      call addfld ('NNAT_0  ',(/'lev'/),'A','1/cm3   ','Aerosol mode 0 number concentration')     
      call addfld ('NNAT_1  ',(/'lev'/),'A','1/cm3   ','Aerosol mode 1 number concentration')     
      call addfld ('NNAT_2  ',(/'lev'/),'A','1/cm3   ','Aerosol mode 2 number concentration')     
      call addfld ('NNAT_4  ',(/'lev'/),'A','1/cm3   ','Aerosol mode 4 number concentration')     
      call addfld ('NNAT_5  ',(/'lev'/),'A','1/cm3   ','Aerosol mode 5 number concentration')     
      call addfld ('NNAT_6  ',(/'lev'/),'A','1/cm3   ','Aerosol mode 6 number concentration')     
      call addfld ('NNAT_7  ',(/'lev'/),'A','1/cm3   ','Aerosol mode 7 number concentration')     
      call addfld ('NNAT_8  ',(/'lev'/),'A','1/cm3   ','Aerosol mode 8 number concentration')     
      call addfld ('NNAT_9  ',(/'lev'/),'A','1/cm3   ','Aerosol mode 9 number concentration')     
      call addfld ('NNAT_10 ',(/'lev'/),'A','1/cm3   ','Aerosol mode 10 number concentration')     
      call addfld ('NNAT_12 ',(/'lev'/),'A','1/cm3   ','Aerosol mode 12 number concentration')     
      call addfld ('NNAT_14 ',(/'lev'/),'A','1/cm3   ','Aerosol mode 14 number concentration')     
!ak      call addfld ('AIRMASS ',(/'lev'/),'A','kg/m3   ','Layer airmass')     
      call addfld ('AIRMASSL',(/'lev'/),'A','kg/m2   ','Layer airmass')     
      call addfld ('BETOTVIS',(/'lev'/),'A','1/km','Aerosol 3d extinction at 0.442-0.625')  ! CAM4-Oslo: 0.35-0.64um
      call addfld ('BATOTVIS',(/'lev'/),'A','1/km','Aerosol 3d absorption at 0.442-0.625') ! CAM4-Oslo: 0.35-0.64um
      call addfld ('BATSW13 ',(/'lev'/),'A','1/km','Aerosol 3d SW absorption at 3.077-3.846um')
      call addfld ('BATLW01 ',(/'lev'/),'A','1/km','Aerosol 3d LW absorption depth at 3.077-3.846um')
!akc6      call addfld ('AERLWA01',(/'lev'/),'A','unitless','CAM5 3d LW absorptive optical depth at 3.077-3.846um')
!+
      do i=1,nbmodes
         modeString="  "
         write(modeString,"(I2)"),i
         if(i.lt.10) modeString="0"//adjustl(modeString)
         varName = "Camrel"//trim(modeString)
         if(i.ne.3) call addfld(varName, (/'lev'/),'A','unitless', 'relative added mass for mode'//modeString)
      enddo  
      do i=1,nbmodes
         modeString="  "
         write(modeString,"(I2)"),i
         if(i.lt.10) modeString="0"//adjustl(modeString)
         varName = "Cxsrel"//trim(modeString)
         if(i.ne.3) call addfld(varName, horiz_only, 'A', 'unitless', 'relative exessive added mass column for mode'//modeString)
      enddo  

!#ifdef RFMIPIRF
!      do ib=1,nswbands
!        write(c2,'(I2)') ib
!        call addfld('AERTAUBND'//trim(adjustl(c2)), (/'lev'/),'A', 'unitless', 'aerosol extinction optical depth for wavelength band '//trim(adjustl(c2)))
!        call addfld('AERSSABND'//trim(adjustl(c2)), (/'lev'/),'A', 'unitless', 'aerosol single scattering albedo for wavelength band '//c2)
!        call addfld('AERASYBND'//trim(adjustl(c2)), (/'lev'/),'A', 'unitless', 'aerosol asymmetry parameter for wavelength band '//c2)
!
!        call addfld('SDBND'//trim(adjustl(c2)), (/'ilev'/),'A', 'W/m^2', 'shortwave spectral flux down for wavelength band '//c2)
!        call addfld('SUBND'//trim(adjustl(c2)), (/'ilev'/),'A', 'W/m^2', 'shortwave spectral flux up for wavelength band '//c2)
!      enddo
!      do ib=1,nlwbands
!        write(c2,'(I2)') ib
!        call addfld('LDBND'//trim(adjustl(c2)), (/'ilev'/),'A', 'W/m^2', 'longwave spectral flux down for wavelength band '//c2)
!        call addfld('LUBND'//trim(adjustl(c2)), (/'ilev'/),'A', 'W/m^2', 'longwave spectral flux up for wavelength band '//c2)
!      enddo
!#endif

#ifdef AEROCOM_INSITU        ! Note that this code has not yet been updated to CESM2 standard 

      do i=2,6  

          irh=RF(i)
          modeString="  "
          write(modeString,"(I2)"),irh
          if(RF(i).eq.0) modeString="00"

!-          varName = "EC44RH"//trim(modeString)
!-          call addfld(varName, 'unitless', pver, 'A', '3D EC440 at RH ='//modeString//'%', phys_decomp)
          varName = "EC55RH"//trim(modeString)
          call addfld(varName, 'unitless', pver, 'A', '3D EC550 at RH ='//modeString//'%', phys_decomp)
!-          varName = "EC87RH"//trim(modeString)
!-          call addfld(varName, 'unitless', pver, 'A', '3D EC870 at RH ='//modeString//'%', phys_decomp)

!-          varName = "AB44RH"//trim(modeString)
!-          call addfld(varName, 'unitless', pver, 'A', '3D ABS440 at RH ='//modeString//'%', phys_decomp)
          varName = "AB55RH"//trim(modeString)
          call addfld(varName, 'unitless', pver, 'A', '3D ABS550 at RH ='//modeString//'%', phys_decomp)
!-          varName = "AB87RH"//trim(modeString)
!-          call addfld(varName, 'unitless', pver, 'A', '3D ABS870 at RH ='//modeString//'%', phys_decomp)

      enddo  

#endif  ! AEROCOM_INSITU

#endif  ! aerocom
#endif  ! dirind

#endif  ! OSLO_AERO

 
    if (history_amwg) then
      call add_default ('PHIS    '  , 1, ' ')
      call add_default ('PS      '  , 1, ' ')
      call add_default ('T       '  , 1, ' ')
      call add_default ('U       '  , 1, ' ')
      call add_default ('V       '  , 1, ' ')
      call add_default ('Z3      '  , 1, ' ')
      call add_default ('OMEGA   '  , 1, ' ')
      call add_default ('VT      ', 1, ' ')
      call add_default ('VU      ', 1, ' ')
      call add_default ('VV      ', 1, ' ')
      call add_default ('UU      ', 1, ' ')
      call add_default ('OMEGAT  ', 1, ' ')
      call add_default ('PSL     ', 1, ' ')
    end if

    if (history_vdiag) then
      call add_default ('U200', 2, ' ')
      call add_default ('V200', 2, ' ')
      call add_default ('U850', 2, ' ')
      call add_default ('U200', 3, ' ')
      call add_default ('U850', 3, ' ')
      call add_default ('OMEGA500', 3, ' ')
    end if

    if (history_eddy) then
      call add_default ('VT      ', 1, ' ')
      call add_default ('VU      ', 1, ' ')
      call add_default ('VV      ', 1, ' ')
      call add_default ('UU      ', 1, ' ')
      call add_default ('OMEGAT  ', 1, ' ')
      call add_default ('OMEGAU  ', 1, ' ')
      call add_default ('OMEGAV  ', 1, ' ')
    endif

    if ( history_budget ) then
      call add_default ('PHIS    '  , history_budget_histfile_num, ' ')
      call add_default ('PS      '  , history_budget_histfile_num, ' ')
      call add_default ('T       '  , history_budget_histfile_num, ' ')
      call add_default ('U       '  , history_budget_histfile_num, ' ')
      call add_default ('V       '  , history_budget_histfile_num, ' ')
      call add_default ('TTEND_TOT' , history_budget_histfile_num, ' ')

      ! State before physics (FV)
      call add_default ('TBP     '  , history_budget_histfile_num, ' ')
      call add_default (bpcnst(1)   , history_budget_histfile_num, ' ')
      ! State after physics (FV)
      call add_default ('TAP     '  , history_budget_histfile_num, ' ')
      call add_default ('UAP     '  , history_budget_histfile_num, ' ')
      call add_default ('VAP     '  , history_budget_histfile_num, ' ')  
      call add_default (apcnst(1)   , history_budget_histfile_num, ' ')
      if ( dycore_is('LR') .or. dycore_is('SE') ) then
        call add_default ('TFIX    '    , history_budget_histfile_num, ' ')
!+tht
        call add_default ('EBREAK  '  , history_budget_histfile_num, ' ')
        call add_default ('PTTEND_DME', history_budget_histfile_num, ' ')
        call add_default ('IETEND_DME', history_budget_histfile_num, ' ')
        call add_default ('EFLX    '  , history_budget_histfile_num, ' ')
!-tht
      end if
    end if

    if (history_waccm) then
      call add_default ('PHIS', 7, ' ')
      call add_default ('PS', 7, ' ')
      call add_default ('PSL', 7, ' ')
    end if

    ! outfld calls in diag_phys_tend_writeout
    call addfld ('PTTEND',          (/ 'lev' /), 'A', 'K/s','T total physics tendency'                             )
    if ( history_budget ) then
      call add_default ('PTTEND'          , history_budget_histfile_num, ' ')
    end if

!akc6+ CNVCLD is zero
!   call add_default ('CNVCLD  ', 1, ' ')
!akc6-

    ! create history variables for fourier coefficients of the diurnal
    ! and semidiurnal tide in T, U, V, and Z3
    call tidal_diag_init()

    !
    ! energy diagnostics
    !
    call addfld ('SE_pBF',   horiz_only, 'A', 'J/m2','Dry Static Energy before energy fixer')
    call addfld ('SE_pBP',   horiz_only, 'A', 'J/m2','Dry Static Energy before parameterizations')
    call addfld ('SE_pAP',   horiz_only, 'A', 'J/m2','Dry Static Energy after parameterizations')
    call addfld ('SE_pAM',   horiz_only, 'A', 'J/m2','Dry Static Energy after dry mass correction')

    call addfld ('KE_pBF',   horiz_only, 'A', 'J/m2','Kinetic Energy before energy fixer')
    call addfld ('KE_pBP',   horiz_only, 'A', 'J/m2','Kinetic Energy before parameterizations')
    call addfld ('KE_pAP',   horiz_only, 'A', 'J/m2','Kinetic Energy after parameterizations')
    call addfld ('KE_pAM',   horiz_only, 'A', 'J/m2','Kinetic Energy after dry mass correction')

    call addfld ('TT_pBF',   horiz_only, 'A', 'kg/m2','Total column test tracer before energy fixer')
    call addfld ('TT_pBP',   horiz_only, 'A', 'kg/m2','Total column test tracer before parameterizations')
    call addfld ('TT_pAP',   horiz_only, 'A', 'kg/m2','Total column test tracer after parameterizations')
    call addfld ('TT_pAM',   horiz_only, 'A', 'kg/m2','Total column test tracer after dry mass correction')

    call addfld ('WV_pBF',   horiz_only, 'A', 'kg/m2','Total column water vapor before energy fixer')
    call addfld ('WV_pBP',   horiz_only, 'A', 'kg/m2','Total column water vapor before parameterizations')
    call addfld ('WV_pAP',   horiz_only, 'A', 'kg/m2','Total column water vapor after parameterizations')
    call addfld ('WV_pAM',   horiz_only, 'A', 'kg/m2','Total column water vapor after dry mass correction')

    call addfld ('WL_pBF',   horiz_only, 'A', 'kg/m2','Total column cloud water before energy fixer')
    call addfld ('WL_pBP',   horiz_only, 'A', 'kg/m2','Total column cloud water before parameterizations')
    call addfld ('WL_pAP',   horiz_only, 'A', 'kg/m2','Total column cloud water after parameterizations')
    call addfld ('WL_pAM',   horiz_only, 'A', 'kg/m2','Total column cloud water after dry mass correction')

    call addfld ('WI_pBF',   horiz_only, 'A', 'kg/m2','Total column cloud ice before energy fixer')
    call addfld ('WI_pBP',   horiz_only, 'A', 'kg/m2','Total column cloud ice before parameterizations')
    call addfld ('WI_pAP',   horiz_only, 'A', 'kg/m2','Total column cloud ice after parameterizations')
    call addfld ('WI_pAM',   horiz_only, 'A', 'kg/m2','Total column cloud ice after dry mass correction')
    !
    ! Axial Angular Momentum diagnostics
    !
    call addfld ('MR_pBF',   horiz_only, 'A', 'kg*m2/s*rad2',&
    'Total column wind axial angular momentum before energy fixer')
    call addfld ('MR_pBP',   horiz_only, 'A', 'kg*m2/s*rad2',&
    'Total column wind axial angular momentum before parameterizations')
    call addfld ('MR_pAP',   horiz_only, 'A', 'kg*m2/s*rad2',&
         'Total column wind axial angular momentum after parameterizations')
    call addfld ('MR_pAM',   horiz_only, 'A', 'kg*m2/s*rad2',&
         'Total column wind axial angular momentum after dry mass correction')

    call addfld ('MO_pBF',   horiz_only, 'A', 'kg*m2/s*rad2',&
    'Total column mass axial angular momentum before energy fixer')
    call addfld ('MO_pBP',   horiz_only, 'A', 'kg*m2/s*rad2',&
    'Total column mass axial angular momentum before parameterizations')
    call addfld ('MO_pAP',   horiz_only, 'A', 'kg*m2/s*rad2',&
         'Total column mass axial angular momentum after parameterizations')
    call addfld ('MO_pAM',   horiz_only, 'A', 'kg*m2/s*rad2',&
         'Total column mass axial angular momentum after dry mass correction')

#ifdef DIRIND
   call add_default ('AOD_VIS ', 1, ' ')
   call add_default ('ABSVIS  ', 1, ' ')
   call add_default ('AODVVOLC', 1, ' ')
   call add_default ('ABSVVOLC', 1, ' ')
   call add_default ('DAYFOC  ', 1, ' ')
   call add_default ('CAODVIS ', 1, ' ')
   call add_default ('CABSVIS ', 1, ' ')
   call add_default ('CLDFREE ', 1, ' ')
   call add_default ('N_AER   ', 1, ' ')
#ifdef COLTST4INTCONS 
   call add_default ('TAUKC0 ', 1, ' ')
   call add_default ('TAUKC1 ', 1, ' ')
   call add_default ('TAUKC2 ', 1, ' ')
   call add_default ('TAUKC4 ', 1, ' ')
   call add_default ('TAUKC5 ', 1, ' ')
   call add_default ('TAUKC6 ', 1, ' ')
   call add_default ('TAUKC7 ', 1, ' ')
   call add_default ('TAUKC8 ', 1, ' ')
   call add_default ('TAUKC9 ', 1, ' ')
   call add_default ('TAUKC10', 1, ' ')
   call add_default ('TAUKC12', 1, ' ')
   call add_default ('TAUKC14', 1, ' ')
!
   call add_default ('MECKC0 ', 1, ' ')
   call add_default ('MECKC1 ', 1, ' ')
   call add_default ('MECKC2 ', 1, ' ')
   call add_default ('MECKC4 ', 1, ' ')
   call add_default ('MECKC5 ', 1, ' ')
   call add_default ('MECKC6 ', 1, ' ')
   call add_default ('MECKC7 ', 1, ' ')
   call add_default ('MECKC8 ', 1, ' ')
   call add_default ('MECKC9 ', 1, ' ')
   call add_default ('MECKC10', 1, ' ')
   call add_default ('MECKC12', 1, ' ')
   call add_default ('MECKC14', 1, ' ')
#ifdef AEROCOM
   call add_default ('CMDRY0 ', 1, ' ')
   call add_default ('CMDRY1 ', 1, ' ')
   call add_default ('CMDRY2 ', 1, ' ')
   call add_default ('CMDRY4 ', 1, ' ')
   call add_default ('CMDRY5 ', 1, ' ')
   call add_default ('CMDRY6 ', 1, ' ')
   call add_default ('CMDRY7 ', 1, ' ')
   call add_default ('CMDRY8 ', 1, ' ')
   call add_default ('CMDRY9 ', 1, ' ')
   call add_default ('CMDRY10', 1, ' ')
   call add_default ('CMDRY12', 1, ' ')
   call add_default ('CMDRY14', 1, ' ')
#endif
#endif
!-   call add_default ('N_AERORG', 1, ' ')
   call add_default ('SSAVIS  ', 1, ' ')
   call add_default ('ASYMMVIS', 1, ' ')
   call add_default ('EXTVIS  ', 1, ' ')
!=0   call add_default ('RELH    ', 1, ' ')
!akc6+
   call add_default ('BVISVOLC', 1, ' ')
!akc6-
!#ifdef SPAERO
!   call add_default ('AODVISSP', 1, ' ')
!   call add_default ('ABSVISSP', 1, ' ')
!   call add_default ('XCDNC_SP', 1, ' ')
!   call add_default ('AODV3DSP', 1, ' ')
!   call add_default ('ABSV3DSP', 1, ' ')
!#endif
#ifdef AEROFFL
     call add_default ('FSNT_DRF', 1, ' ')
     call add_default ('FSNTCDRF', 1, ' ')
     call add_default ('FSNS_DRF', 1, ' ')
     call add_default ('FSNSCDRF', 1, ' ')
     call add_default ('QRS_DRF ', 1, ' ')
     call add_default ('QRSC_DRF', 1, ' ')
     call add_default ('FLNT_DRF', 1, ' ')
     call add_default ('FLNTCDRF', 1, ' ')
     call add_default ('FSUTADRF', 1, ' ')
     call add_default ('FSDS_DRF', 1, ' ')
     call add_default ('FSUS_DRF', 1, ' ')
     call add_default ('FSDSCDRF', 1, ' ')
     call add_default ('FLUS    ', 1, ' ')
!->ut     call add_default ('FLNT_ORG', 1, ' ')
#endif  ! aeroffl
#ifdef AEROCOM 
      call add_default ('AKCXS   ', 1, ' ')
      call add_default ('PMTOT   ', 1, ' ')
      call add_default ('PM25    ', 1, ' ')
!akc6+
      call add_default ('PM2P5   ', 1, ' ')
      call add_default ('MMRPM2P5', 1, ' ')
      call add_default ('MMRPM1  ', 1, ' ')
!akc6-
      call add_default ('GRIDAREA', 1, ' ')
      call add_default ('DAERH2O ', 1, ' ')
      call add_default ('MMR_AH2O', 1, ' ')
      call add_default ('ECDRYAER', 1, ' ')
      call add_default ('ABSDRYAE', 1, ' ')
      call add_default ('ECDRY440', 1, ' ')
      call add_default ('ABSDR440', 1, ' ')
      call add_default ('ECDRY870', 1, ' ')
      call add_default ('ABSDR870', 1, ' ')
      call add_default ('ASYMMDRY', 1, ' ')
      call add_default ('ECDRYLT1', 1, ' ')
      call add_default ('ABSDRYBC', 1, ' ')
      call add_default ('ABSDRYOC', 1, ' ')
      call add_default ('ABSDRYSU', 1, ' ')
      call add_default ('ABSDRYSS', 1, ' ')
      call add_default ('ABSDRYDU', 1, ' ')
      call add_default ('OD550DRY', 1, ' ')
      call add_default ('AB550DRY', 1, ' ')
      call add_default ('DERLT05 ', 1, ' ')
      call add_default ('DERGT05 ', 1, ' ')
      call add_default ('DER     ', 1, ' ')
      call add_default ('DOD440  ', 1, ' ')
      call add_default ('ABS440  ', 1, ' ')
      call add_default ('DOD500  ', 1, ' ')
      call add_default ('ABS500  ', 1, ' ')
      call add_default ('DOD550  ', 1, ' ')
!tst
!      call add_default ('DOD5503D', 1, ' ')
!      call add_default ('AODVIS3D', 1, ' ')
!tst
      call add_default ('ABS550  ', 1, ' ')
      call add_default ('ABS550AL', 1, ' ')
      call add_default ('DOD670  ', 1, ' ')
      call add_default ('ABS670  ', 1, ' ')
      call add_default ('DOD870  ', 1, ' ')
      call add_default ('ABS870  ', 1, ' ')
      call add_default ('DLOAD_MI', 1, ' ')
      call add_default ('DLOAD_SS', 1, ' ')
      call add_default ('DLOAD_S4', 1, ' ')
      call add_default ('DLOAD_OC', 1, ' ')
      call add_default ('DLOAD_BC', 1, ' ')
      call add_default ('LOADBCAC', 1, ' ')
      call add_default ('LOADBC0 ', 1, ' ')
      call add_default ('LOADBC2 ', 1, ' ')
      call add_default ('LOADBC4 ', 1, ' ')
      call add_default ('LOADBC12', 1, ' ')
      call add_default ('LOADBC14', 1, ' ')
      call add_default ('LOADOCAC', 1, ' ')
      call add_default ('LOADOC4 ', 1, ' ')
      call add_default ('LOADOC14', 1, ' ')
#ifdef  COLTST4INTCONS
      call add_default ('COLRBC0 ', 1, ' ')
      call add_default ('COLRBC2 ', 1, ' ')
      call add_default ('COLRBC4 ', 1, ' ')
      call add_default ('COLRBC12', 1, ' ')
      call add_default ('COLRBC14', 1, ' ')
      call add_default ('COLRBCAC', 1, ' ')
      call add_default ('COLROC4 ', 1, ' ')
      call add_default ('COLROC14', 1, ' ')
      call add_default ('COLROCAC', 1, ' ')
      call add_default ('COLRSULA', 1, ' ')
      call add_default ('COLRSUL1', 1, ' ')
      call add_default ('COLRSUL5', 1, ' ')
#endif  ! COLTST4INTCONS
!
      call add_default ('EC550AER', 1, ' ')
      call add_default ('ABS550_A', 1, ' ')
      call add_default ('BS550AER', 1, ' ')
!
      call add_default ('EC550SO4', 1, ' ')
      call add_default ('EC550BC ', 1, ' ')
      call add_default ('EC550POM', 1, ' ')
      call add_default ('EC550SS ', 1, ' ')
      call add_default ('EC550DU ', 1, ' ')
!
      call add_default ('CDOD440 ', 1, ' ')
      call add_default ('CDOD550 ', 1, ' ')
      call add_default ('CABS550 ', 1, ' ')
      call add_default ('CABS550A', 1, ' ')
      call add_default ('CDOD870 ', 1, ' ')
      call add_default ('A550_DU ', 1, ' ')
      call add_default ('A550_SS ', 1, ' ')
      call add_default ('A550_SO4', 1, ' ')
      call add_default ('A550_POM', 1, ' ')
      call add_default ('A550_BC ', 1, ' ')
      call add_default ('D440_DU ', 1, ' ')
      call add_default ('D440_SS ', 1, ' ')
      call add_default ('D440_SO4', 1, ' ')
      call add_default ('D440_POM', 1, ' ')
      call add_default ('D440_BC ', 1, ' ')
      call add_default ('D500_DU ', 1, ' ')
      call add_default ('D500_SS ', 1, ' ')
      call add_default ('D500_SO4', 1, ' ')
      call add_default ('D500_POM', 1, ' ')
      call add_default ('D500_BC ', 1, ' ')
      call add_default ('D550_DU ', 1, ' ')
      call add_default ('D550_SS ', 1, ' ')
      call add_default ('D550_SO4', 1, ' ')
      call add_default ('D550_POM', 1, ' ')
      call add_default ('D550_BC ', 1, ' ')
      call add_default ('D670_DU ', 1, ' ')
      call add_default ('D670_SS ', 1, ' ')
      call add_default ('D670_SO4', 1, ' ')
      call add_default ('D670_POM', 1, ' ')
      call add_default ('D670_BC ', 1, ' ')
      call add_default ('D870_DU ', 1, ' ')
      call add_default ('D870_SS ', 1, ' ')
      call add_default ('D870_SO4', 1, ' ')
      call add_default ('D870_POM', 1, ' ')
      call add_default ('D870_BC ', 1, ' ')
      call add_default ('DLT_DUST', 1, ' ')
      call add_default ('DLT_SS  ', 1, ' ')
      call add_default ('DLT_SO4 ', 1, ' ')
      call add_default ('DLT_POM ', 1, ' ')
      call add_default ('DLT_BC  ', 1, ' ')
      call add_default ('DGT_DUST', 1, ' ')
      call add_default ('DGT_SS  ', 1, ' ')
      call add_default ('DGT_SO4 ', 1, ' ')
      call add_default ('DGT_POM ', 1, ' ')
      call add_default ('DGT_BC  ', 1, ' ')
      call add_default ('NNAT_0  ', 1, ' ')
      call add_default ('NNAT_1  ', 1, ' ')
      call add_default ('NNAT_2  ', 1, ' ')
      call add_default ('NNAT_4  ', 1, ' ')
      call add_default ('NNAT_5  ', 1, ' ')
      call add_default ('NNAT_6  ', 1, ' ')
      call add_default ('NNAT_7  ', 1, ' ')
      call add_default ('NNAT_8  ', 1, ' ')
      call add_default ('NNAT_9  ', 1, ' ')
      call add_default ('NNAT_10 ', 1, ' ')
      call add_default ('NNAT_12 ', 1, ' ')
      call add_default ('NNAT_14 ', 1, ' ')
      call add_default ('AIRMASSL', 1, ' ')  !akc6
      call add_default ('AIRMASS ', 1, ' ')  !akc6
      call add_default ('BETOTVIS', 1, ' ')
      call add_default ('BATOTVIS', 1, ' ')
      call add_default ('BATSW13 ', 1, ' ')
      call add_default ('BATLW01 ', 1, ' ')
!akc6      call add_default ('AERLWA01', 1, ' ')
!+
      do i=1,nbmodes
         modeString="  "
         write(modeString,"(I2)"),i
         if(i.lt.10) modeString="0"//adjustl(modeString)
         varName = "Camrel"//trim(modeString)
         if(i.ne.3) call add_default(varName, 1, ' ')
      enddo  
      do i=1,nbmodes
         modeString="  "
         write(modeString,"(I2)"),i
         if(i.lt.10) modeString="0"//adjustl(modeString)
         varName = "Cxsrel"//trim(modeString)
         if(i.ne.3) call add_default(varName, 1, ' ')
      enddo  
!++

!#ifdef RFMIPIRF
!      do i=1,nbands
!      do ib=1,nswbands
!        write(c2,'(I2)') ib
!        call add_default('AERTAUBND'//trim(adjustl(c2)), 1, ' ') 
!        call add_default('AERSSABND'//trim(adjustl(c2)), 1, ' ') 
!        call add_default('AERASYBND'//trim(adjustl(c2)), 1, ' ') 
!
!        call add_default('SDBND'//trim(adjustl(c2)), 1, ' ') 
!        call add_default('SUBND'//trim(adjustl(c2)), 1, ' ') 
!      enddo
!      do ib=1,nlwbands
!        write(c2,'(I2)') ib
!        call add_default('LDBND'//trim(adjustl(c2)), 1, ' ') 
!        call add_default('LUBND'//trim(adjustl(c2)), 1, ' ') 
!      enddo
!#endif


#ifdef AEROCOM_INSITU

      do i=2,6  

          irh=RF(i)
          modeString="  "
          write(modeString,"(I2)"),irh
          if(RF(i).eq.0) modeString="00"

!-          varName = "EC44RH"//trim(modeString)
!-          call add_default(varName, 1, ' ')
          varName = "EC55RH"//trim(modeString)
          call add_default(varName, 1, ' ')
!-          varName = "EC87RH"//trim(modeString)
!-          call add_default(varName, 1, ' ')

!-          varName = "AB44RH"//trim(modeString)
!-          call add_default(varName, 1, ' ')
          varName = "AB55RH"//trim(modeString)
          call add_default(varName, 1, ' ')
!-          varName = "AB87RH"//trim(modeString)
!-          call add_default(varName, 1, ' ')

      enddo 
 
#endif  ! AEROCOM_INSITU

!--
!-
#endif  ! aerocom
#endif  ! dirind

!#ifdef SPAERO
!      call addfld ('FSNT_SP ', horiz_only, 'A','W/m^2','Total column absorbed solar flux (without SP aerosols)')
!      call addfld ('FSNTC_SP', horiz_only, 'A','W/m^2','Clear sky total column absorbed solar flux (without SP aerosols)')
!      call addfld ('FSNS_SP ', horiz_only, 'A','W/m^2','Surface absorbed solar flux (without SP aerosols)')
!      call addfld ('FSNSC_SP', horiz_only, 'A','W/m^2','Clear sky surface absorbed solar flux (without SP aerosols)')
!      call addfld ('FSNT_SP2', horiz_only, 'A','W/m^2','Total column absorbed solar flux (SP aerosols for DRF only)')
!      call addfld ('FSNTCSP2', horiz_only, 'A','W/m^2','Clear sky total column absorbed solar flux (SP aerosols for DRF only)')
!      call addfld ('FSNS_SP2', horiz_only, 'A','W/m^2','Surface absorbed solar flux (SP aerosols for DRF only)')
!      call addfld ('FSNSCSP2', horiz_only, 'A','W/m^2','Clear sky surface absorbed solar flux (SP aerosols for DRF only)')
!      call addfld ('FSNT_SP3', horiz_only, 'A','W/m^2','Total column absorbed solar flux (SP aerosols)')
!      call addfld ('FSNTCSP3', horiz_only, 'A','W/m^2','Clear sky total column absorbed solar flux (SP aerosols)')
!      call addfld ('FSNS_SP3', horiz_only, 'A','W/m^2','Surface absorbed solar flux (SP aerosols)')
!      call addfld ('FSNSCSP3', horiz_only, 'A','W/m^2','Clear sky surface absorbed solar flux (SP aerosols)')
!      call add_default ('FSNT_SP' , 1, ' ')
!      call add_default ('FSNTC_SP', 1, ' ')
!      call add_default ('FSNS_SP' , 1, ' ')
!      call add_default ('FSNSC_SP', 1, ' ')
!      call add_default ('FSNT_SP2', 1, ' ')
!      call add_default ('FSNTCSP2', 1, ' ')
!      call add_default ('FSNS_SP2', 1, ' ')
!      call add_default ('FSNSCSP2', 1, ' ')
!      call add_default ('FSNT_SP3', 1, ' ')
!      call add_default ('FSNTCSP3', 1, ' ')
!      call add_default ('FSNS_SP3', 1, ' ')
!      call add_default ('FSNSCSP3', 1, ' ')
!#endif

  end subroutine diag_init_dry

  subroutine diag_init_moist(pbuf2d)

    ! Declare the history fields for which this module contains outfld calls.

    use cam_history,        only: addfld, add_default, horiz_only
    use cam_history,        only: register_vector_field
    use constituent_burden, only: constituent_burden_init
    use physics_buffer,     only: pbuf_set_field

    type(physics_buffer_desc), pointer, intent(in) :: pbuf2d(:,:)

    integer :: k, m
    integer :: ixcldice, ixcldliq ! constituent indices for cloud liquid and ice water.
    integer :: ierr
    !AL
    integer :: ixcldnc
    integer :: ixcldni
    !AL
    ! column burdens for all constituents except water vapor
    call constituent_burden_init

    call cnst_get_ind('CLDLIQ', ixcldliq, abort=.false.)
    call cnst_get_ind('CLDICE', ixcldice, abort=.false.)

    ! outfld calls in diag_phys_writeout
    call addfld ('OMEGAQ',     (/ 'lev' /), 'A', 'kgPa/kgs', 'Vertical water transport' )
    call addfld ('VQ',         (/ 'lev' /), 'A', 'm/skg/kg',  'Meridional water transport')
    call addfld ('QQ',         (/ 'lev' /), 'A', 'kg2/kg2',   'Eddy moisture variance')

    call addfld ('MQ',         (/ 'lev' /), 'A', 'kg/m2','Water vapor mass in layer')
    call addfld ('TMQ',        horiz_only,  'A', 'kg/m2','Total (vertically integrated) precipitable water')
    call addfld ('RELHUM',     (/ 'lev' /), 'A', 'percent','Relative humidity')
    call addfld ('RHW',        (/ 'lev' /), 'A', 'percent','Relative humidity with respect to liquid')
    call addfld ('RHI',        (/ 'lev' /), 'A', 'percent','Relative humidity with respect to ice')
    call addfld ('RHCFMIP',    (/ 'lev' /), 'A', 'percent','Relative humidity with respect to water above 273 K, ice below 273 K')

    call addfld ('THE8501000', horiz_only,  'A', 'K','ThetaE difference 850 mb - 1000 mb')
    call addfld ('THE9251000', horiz_only,  'A', 'K','ThetaE difference 925 mb - 1000 mb')

    call addfld ('Q1000',      horiz_only,  'A', 'kg/kg','Specific Humidity at 1000 mbar pressure surface')
    call addfld ('Q925',       horiz_only,  'A', 'kg/kg','Specific Humidity at 925 mbar pressure surface')
    call addfld ('Q850',       horiz_only,  'A', 'kg/kg','Specific Humidity at 850 mbar pressure surface')
    call addfld ('Q200',       horiz_only,  'A', 'kg/kg','Specific Humidity at 700 mbar pressure surface')
    call addfld ('QBOT',       horiz_only,  'A', 'kg/kg','Lowest model level water vapor mixing ratio')

    call addfld ('PSDRY',      horiz_only,  'A', 'Pa', 'Dry surface pressure')
    call addfld ('PMID',       (/ 'lev' /), 'A', 'Pa', 'Pressure at layer midpoints')
    call addfld ('PDELDRY',    (/ 'lev' /), 'A', 'Pa', 'Dry pressure difference between levels')

    ! outfld calls in diag_conv

    call addfld ('DTCOND',       (/ 'lev' /), 'A','K/s','T tendency - moist processes')
    call addfld ('DTCOND_24_COS',(/ 'lev' /), 'A','K/s','T tendency - moist processes 24hr. cos coeff.')
    call addfld ('DTCOND_24_SIN',(/ 'lev' /), 'A','K/s','T tendency - moist processes 24hr. sin coeff.')
    call addfld ('DTCOND_12_COS',(/ 'lev' /), 'A','K/s','T tendency - moist processes 12hr. cos coeff.')
    call addfld ('DTCOND_12_SIN',(/ 'lev' /), 'A','K/s','T tendency - moist processes 12hr. sin coeff.')
    call addfld ('DTCOND_08_COS',(/ 'lev' /), 'A','K/s','T tendency - moist processes  8hr. cos coeff.')
    call addfld ('DTCOND_08_SIN',(/ 'lev' /), 'A','K/s','T tendency - moist processes  8hr. sin coeff.')
!AL
    call cnst_get_ind('NUMLIQ', ixcldnc)
    call cnst_get_ind('NUMICE', ixcldni)
!AL

    call addfld ('PRECL',    horiz_only, 'A', 'm/s','Large-scale (stable) precipitation rate (liq + ice)'                )
    call addfld ('PRECC',    horiz_only, 'A', 'm/s','Convective precipitation rate (liq + ice)'                          )
    call addfld ('PRECT',    horiz_only, 'A', 'm/s','Total (convective and large-scale) precipitation rate (liq + ice)'  )
    call addfld ('PREC_PCW', horiz_only, 'A', 'm/s','LS_pcw precipitation rate')
    call addfld ('PREC_zmc', horiz_only, 'A', 'm/s','CV_zmc precipitation rate')
    call addfld ('PRECTMX',  horiz_only, 'X','m/s','Maximum (convective and large-scale) precipitation rate (liq+ice)'   )
    call addfld ('PRECSL',   horiz_only, 'A', 'm/s','Large-scale (stable) snow rate (water equivalent)'                  )
    call addfld ('PRECSC',   horiz_only, 'A', 'm/s','Convective snow rate (water equivalent)'                            )
    call addfld ('PRECCav',  horiz_only, 'A', 'm/s','Average large-scale precipitation (liq + ice)'                      )
    call addfld ('PRECLav',  horiz_only, 'A', 'm/s','Average convective precipitation  (liq + ice)'                      )

    ! outfld calls in diag_surf

    call addfld ('SHFLX',    horiz_only, 'A', 'W/m2','Surface sensible heat flux')
    call addfld ('LHFLX',    horiz_only, 'A', 'W/m2','Surface latent heat flux')
    call addfld ('QFLX',     horiz_only, 'A', 'kg/m2/s','Surface water flux')

    call addfld ('TAUX',     horiz_only, 'A', 'N/m2','Zonal surface stress')
    call addfld ('TAUY',     horiz_only, 'A', 'N/m2','Meridional surface stress')
    call addfld ('TREFHT',   horiz_only, 'A', 'K','Reference height temperature')
    call addfld ('TREFHTMN', horiz_only, 'M','K','Minimum reference height temperature over output period')
    call addfld ('TREFHTMX', horiz_only, 'X','K','Maximum reference height temperature over output period')
    call addfld ('QREFHT',   horiz_only, 'A', 'kg/kg','Reference height humidity')
    call addfld ('U10',      horiz_only, 'A', 'm/s','10m wind speed')
    call addfld ('RHREFHT',  horiz_only, 'A', 'fraction','Reference height relative humidity')

    call addfld ('LANDFRAC', horiz_only, 'A', 'fraction','Fraction of sfc area covered by land')
    call addfld ('ICEFRAC',  horiz_only, 'A', 'fraction','Fraction of sfc area covered by sea-ice')
    call addfld ('OCNFRAC',  horiz_only, 'A', 'fraction','Fraction of sfc area covered by ocean')

    call addfld ('TREFMNAV', horiz_only, 'A', 'K','Average of TREFHT daily minimum')
    call addfld ('TREFMXAV', horiz_only, 'A', 'K','Average of TREFHT daily maximum')

    call addfld ('TS',       horiz_only, 'A', 'K','Surface temperature (radiative)')
    call addfld ('TSMN',     horiz_only, 'M','K','Minimum surface temperature over output period')
    call addfld ('TSMX',     horiz_only, 'X','K','Maximum surface temperature over output period')
    call addfld ('SNOWHLND', horiz_only, 'A', 'm','Water equivalent snow depth')
    call addfld ('SNOWHICE', horiz_only, 'A', 'm','Snow depth over ice', fill_value = 1.e30_r8)
    call addfld ('TBOT',     horiz_only, 'A', 'K','Lowest model level temperature')

    call addfld ('ASDIR',    horiz_only, 'A', '1','albedo: shortwave, direct')
    call addfld ('ASDIF',    horiz_only, 'A', '1','albedo: shortwave, diffuse')
    call addfld ('ALDIR',    horiz_only, 'A', '1','albedo: longwave, direct')
    call addfld ('ALDIF',    horiz_only, 'A', '1','albedo: longwave, diffuse')
    call addfld ('SST',      horiz_only, 'A', 'K','sea surface temperature')


    ! outfld calls in diag_phys_tend_writeout

    call addfld (ptendnam(       1),(/ 'lev' /), 'A', 'kg/kg/s',trim(cnst_name(       1))//' total physics tendency '      )

    if (ixcldliq > 0) then
       call addfld (ptendnam(ixcldliq),(/ 'lev' /), 'A', 'kg/kg/s',trim(cnst_name(ixcldliq))//' total physics tendency '      )
    end if
    if (ixcldice > 0) then
      call addfld (ptendnam(ixcldice),(/ 'lev' /), 'A', 'kg/kg/s',trim(cnst_name(ixcldice))//' total physics tendency ')
    end if
!AL
    call addfld (ptendnam(ixcldnc), (/ 'lev' /), 'A', '#/kg/s ',trim(cnst_name(ixcldnc))//' total physics tendency ')
    call addfld (ptendnam(ixcldni), (/ 'lev' /), 'A', '#/kg/s ',trim(cnst_name(ixcldni))//' total physics tendency ')
!AL
    if ( dycore_is('LR') )then
      call addfld (dmetendnam(       1),(/ 'lev' /), 'A','kg/kg/s', &
           trim(cnst_name(       1))//' dme adjustment tendency (FV) ')
      if (ixcldliq > 0) then
         call addfld (dmetendnam(ixcldliq),(/ 'lev' /), 'A','kg/kg/s', &
            trim(cnst_name(ixcldliq))//' dme adjustment tendency (FV) ')
      end if
      if (ixcldice > 0) then
        call addfld (dmetendnam(ixcldice),(/ 'lev' /), 'A','kg/kg/s', &
             trim(cnst_name(ixcldice))//' dme adjustment tendency (FV) ')
      end if
!AL
      call addfld (dmetendnam(ixcldnc),(/ 'lev' /), 'A','#/kg/s ',   &
           trim(cnst_name(ixcldnc))//' dme adjustment tendency (FV) ')
      call addfld (dmetendnam(ixcldni),(/ 'lev' /), 'A','#/kg/s ',   & 
           trim(cnst_name(ixcldni))//' dme adjustment tendency (FV) ')
!AL
    end if

    if ( history_budget ) then
!AL
      call add_default (ptendnam(ixcldnc), history_budget_histfile_num, ' ')
      call add_default (ptendnam(ixcldni), history_budget_histfile_num, ' ')
!AL
    end if

    ! outfld calls in diag_physvar_ic

    call addfld ('QCWAT&IC',  (/ 'lev' /),  'I','kg/kg','q associated with cloud water'                   )
    call addfld ('TCWAT&IC',  (/ 'lev' /),  'I','kg/kg','T associated with cloud water'                   )
    call addfld ('LCWAT&IC',  (/ 'lev' /),  'I','kg/kg','Cloud water (ice + liq'                          )
    call addfld ('CLOUD&IC',  (/ 'lev' /),  'I','fraction','Cloud fraction'                               )
    call addfld ('CONCLD&IC', (/ 'lev' /),  'I','fraction','Convective cloud fraction'                    )
    call addfld ('TKE&IC',    (/ 'ilev' /), 'I','m2/s2','Turbulent Kinetic Energy'                        )
    call addfld ('CUSH&IC',   horiz_only,   'I','m','Convective Scale Height'                             )
    call addfld ('KVH&IC',    (/ 'ilev' /), 'I','m2/s','Vertical diffusion diffusivities (heat/moisture)' )
    call addfld ('KVM&IC',    (/ 'ilev' /), 'I','m2/s','Vertical diffusion diffusivities (momentum)'      )
    call addfld ('PBLH&IC',   horiz_only,   'I','m','PBL height'                                          )
    call addfld ('TPERT&IC',  horiz_only,   'I','K','Perturbation temperature (eddies in PBL)'            )
    call addfld ('QPERT&IC',  horiz_only,   'I','kg/kg','Perturbation specific humidity (eddies in PBL)'  )

    ! CAM export state
    call addfld('a2x_BCPHIWET', horiz_only, 'A', 'kg/m2/s', 'wetdep of hydrophilic black carbon')
    call addfld('a2x_BCPHIDRY', horiz_only, 'A', 'kg/m2/s', 'drydep of hydrophilic black carbon')
    call addfld('a2x_BCPHODRY', horiz_only, 'A', 'kg/m2/s', 'drydep of hydrophobic black carbon')
    call addfld('a2x_OCPHIWET', horiz_only, 'A', 'kg/m2/s', 'wetdep of hydrophilic organic carbon')
    call addfld('a2x_OCPHIDRY', horiz_only, 'A', 'kg/m2/s', 'drydep of hydrophilic organic carbon')
    call addfld('a2x_OCPHODRY', horiz_only, 'A', 'kg/m2/s', 'drydep of hydrophobic organic carbon')
    call addfld('a2x_DSTWET1',  horiz_only, 'A',  'kg/m2/s', 'wetdep of dust (bin1)')
    call addfld('a2x_DSTDRY1',  horiz_only, 'A',  'kg/m2/s', 'drydep of dust (bin1)')
    call addfld('a2x_DSTWET2',  horiz_only, 'A',  'kg/m2/s', 'wetdep of dust (bin2)')
    call addfld('a2x_DSTDRY2',  horiz_only, 'A',  'kg/m2/s', 'drydep of dust (bin2)')
    call addfld('a2x_DSTWET3',  horiz_only, 'A',  'kg/m2/s', 'wetdep of dust (bin3)')
    call addfld('a2x_DSTDRY3',  horiz_only, 'A',  'kg/m2/s', 'drydep of dust (bin3)')
    call addfld('a2x_DSTWET4',  horiz_only, 'A',  'kg/m2/s', 'wetdep of dust (bin4)')
    call addfld('a2x_DSTDRY4',  horiz_only, 'A',  'kg/m2/s', 'drydep of dust (bin4)')

#ifdef AEROCOM 
    call add_default ('RHW     ', 1, ' ')
#endif  ! aerocom

    ! defaults
    if (history_amwg) then
      call add_default (cnst_name(1), 1, ' ')
      call add_default ('VQ      ', 1, ' ')
      call add_default ('TMQ     ', 1, ' ')
      call add_default ('PSL     ', 1, ' ')
      call add_default ('RELHUM  ', 1, ' ')

      call add_default ('DTCOND  ', 1, ' ')
      call add_default ('PRECL   ', 1, ' ')
      call add_default ('PRECC   ', 1, ' ')
      call add_default ('PRECSL  ', 1, ' ')
      call add_default ('PRECSC  ', 1, ' ')
      call add_default ('SHFLX   ', 1, ' ')
      call add_default ('LHFLX   ', 1, ' ')
      call add_default ('QFLX    ', 1, ' ')
      call add_default ('TAUX    ', 1, ' ')
      call add_default ('TAUY    ', 1, ' ')
      call add_default ('TREFHT  ', 1, ' ')
      call add_default ('LANDFRAC', 1, ' ')
      call add_default ('OCNFRAC ', 1, ' ')
      call add_default ('QREFHT  ', 1, ' ')
      call add_default ('U10     ', 1, ' ')
      call add_default ('ICEFRAC ', 1, ' ')
      call add_default ('TS      ', 1, ' ')
      call add_default ('TSMN    ', 1, ' ')
      call add_default ('TSMX    ', 1, ' ')
      call add_default ('SNOWHLND', 1, ' ')
      call add_default ('SNOWHICE', 1, ' ')
    end if

    if (dycore_is('SE')) then
      call add_default ('PSDRY', 1, ' ')
      call add_default ('PMID',  1, ' ')
    end if

    if (history_eddy) then
      call add_default ('VQ      ', 1, ' ')
    endif

    if ( history_budget ) then
      call add_default (cnst_name(1), history_budget_histfile_num, ' ')
      call add_default ('PTTEND'          , history_budget_histfile_num, ' ')
      call add_default (ptendnam(       1), history_budget_histfile_num, ' ')
      if (ixcldliq > 0) then
         call add_default (ptendnam(ixcldliq), history_budget_histfile_num, ' ')
      end if
      if (ixcldice > 0) then
        call add_default (ptendnam(ixcldice), history_budget_histfile_num, ' ')
      end if
      if ( dycore_is('LR') )then
        call add_default(dmetendnam(1)       , history_budget_histfile_num, ' ')
        if (ixcldliq > 0) then
           call add_default(dmetendnam(ixcldliq), history_budget_histfile_num, ' ')
        end if
        if (ixcldice > 0) then
          call add_default(dmetendnam(ixcldice), history_budget_histfile_num, ' ')
        end if
      end if
      if( history_budget_histfile_num > 1 ) then
        call add_default ('DTCOND  '         , history_budget_histfile_num, ' ')
      end if
    end if

    if (history_vdiag) then
      call add_default ('PRECT   ', 2, ' ')
      call add_default ('PRECT   ', 3, ' ')
      call add_default ('PRECT   ', 4, ' ')
    end if

    ! Initial file - Optional fields
    if (inithist_all.or.single_column) then
      call add_default ('CONCLD&IC  ',0, 'I')
      call add_default ('QCWAT&IC   ',0, 'I')
      call add_default ('TCWAT&IC   ',0, 'I')
      call add_default ('LCWAT&IC   ',0, 'I')
      call add_default ('PBLH&IC    ',0, 'I')
      call add_default ('TPERT&IC   ',0, 'I')
      call add_default ('QPERT&IC   ',0, 'I')
      call add_default ('CLOUD&IC   ',0, 'I')
      call add_default ('TKE&IC     ',0, 'I')
      call add_default ('CUSH&IC    ',0, 'I')
      call add_default ('KVH&IC     ',0, 'I')
      call add_default ('KVM&IC     ',0, 'I')
    end if

    ! determine number of constituents for which convective tendencies must be computed
    if (history_budget) then
      dqcond_num = pcnst
    else
      if (diag_cnst_conv_tend == 'none')   dqcond_num = 0
      if (diag_cnst_conv_tend == 'q_only') dqcond_num = 1
      if (diag_cnst_conv_tend == 'all')    dqcond_num = pcnst
    end if

    do m = 1, dqcond_num
      dcconnam(m) = 'DC'//cnst_name(m)
    end do

    if ((diag_cnst_conv_tend == 'q_only') .or. (diag_cnst_conv_tend == 'all') .or. history_budget) then
      call addfld (dcconnam(1),(/ 'lev' /),'A', 'kg/kg/s',trim(cnst_name(1))//' tendency due to moist processes')
      if ( diag_cnst_conv_tend == 'q_only' .or. diag_cnst_conv_tend == 'all' ) then
        call add_default (dcconnam(1),                           1, ' ')
      end if
      if( history_budget ) then
        call add_default (dcconnam(1), history_budget_histfile_num, ' ')
      end if
      if (diag_cnst_conv_tend == 'all' .or. history_budget) then
        do m = 2, pcnst
          call addfld (dcconnam(m),(/ 'lev' /),'A', 'kg/kg/s',trim(cnst_name(m))//' tendency due to moist processes')
          if( diag_cnst_conv_tend == 'all' ) then
            call add_default (dcconnam(m),                           1, ' ')
          end if
          if( history_budget .and. (m == ixcldliq .or. m == ixcldice) ) then
            call add_default (dcconnam(m), history_budget_histfile_num, ' ')
          end if
        end do
      end if
    end if

    ! Pbuf field indices for collecting output data
    relhum_idx = pbuf_get_index('RELHUM',  errcode=ierr)
    qcwat_idx  = pbuf_get_index('QCWAT',  errcode=ierr)
    tcwat_idx  = pbuf_get_index('TCWAT',  errcode=ierr)
    lcwat_idx  = pbuf_get_index('LCWAT',  errcode=ierr)
    cld_idx    = pbuf_get_index('CLD',    errcode=ierr)
    concld_idx = pbuf_get_index('CONCLD', errcode=ierr)

    tke_idx  = pbuf_get_index('tke',  errcode=ierr)
    kvm_idx  = pbuf_get_index('kvm',  errcode=ierr)
    kvh_idx  = pbuf_get_index('kvh',  errcode=ierr)
    cush_idx = pbuf_get_index('cush', errcode=ierr)

    pblh_idx  = pbuf_get_index('pblh',  errcode=ierr)
    tpert_idx = pbuf_get_index('tpert', errcode=ierr)
    qpert_idx = pbuf_get_index('qpert', errcode=ierr)

    prec_dp_idx  = pbuf_get_index('PREC_DP',  errcode=ierr)
    snow_dp_idx  = pbuf_get_index('SNOW_DP',  errcode=ierr)
    prec_sh_idx  = pbuf_get_index('PREC_SH',  errcode=ierr)
    snow_sh_idx  = pbuf_get_index('SNOW_SH',  errcode=ierr)
    prec_sed_idx = pbuf_get_index('PREC_SED', errcode=ierr)
    snow_sed_idx = pbuf_get_index('SNOW_SED', errcode=ierr)
    prec_pcw_idx = pbuf_get_index('PREC_PCW', errcode=ierr)
    snow_pcw_idx = pbuf_get_index('SNOW_PCW', errcode=ierr)

    if (is_first_step()) then
      call pbuf_set_field(pbuf2d, trefmxav_idx, -1.0e36_r8)
      call pbuf_set_field(pbuf2d, trefmnav_idx,  1.0e36_r8)
    end if

  end subroutine diag_init_moist

  subroutine diag_init(pbuf2d)
    use cam_history,        only: addfld

    ! Declare the history fields for which this module contains outfld calls.

    type(physics_buffer_desc), pointer, intent(in) :: pbuf2d(:,:)

    ! ----------------------------
    ! determine default variables
    ! ----------------------------
    call phys_getopts(history_amwg_out   = history_amwg    , &
         history_vdiag_out  = history_vdiag   , &
         history_eddy_out   = history_eddy    , &
         history_budget_out = history_budget  , &
         history_budget_histfile_num_out = history_budget_histfile_num, &
         history_waccm_out  = history_waccm)

    call diag_init_dry(pbuf2d)
    if (moist_physics) then
      call diag_init_moist(pbuf2d)
    end if

  end subroutine diag_init

!===============================================================================

  subroutine diag_allocate_dry()
    use infnan, only: nan, assignment(=)

    ! Allocate memory for module variables.
    ! Done at the begining of a physics step at same point as the pbuf allocate
    ! for variables with "physpkg" scope.

    ! Local variables
    character(len=*), parameter :: sub = 'diag_allocate_dry'
    character(len=128)          :: errmsg
    integer                     :: istat

    allocate(dtcond(pcols,pver,begchunk:endchunk), stat=istat)
    if ( istat /= 0 ) then
      write(errmsg, '(2a,i0)') sub, ': allocate failed, stat = ',istat
      call endrun (errmsg)
    end if
    dtcond = nan
  end subroutine diag_allocate_dry

  subroutine diag_allocate_moist()
    use infnan, only: nan, assignment(=)

    ! Allocate memory for module variables.
    ! Done at the begining of a physics step at same point as the pbuf allocate
    ! for variables with "physpkg" scope.

    ! Local variables
    character(len=*), parameter :: sub = 'diag_allocate_moist'
    character(len=128)          :: errmsg
    integer                     :: i, istat

    if (dqcond_num > 0) then
      allocate(dqcond(dqcond_num))
      do i = 1, dqcond_num
        allocate(dqcond(i)%cnst(pcols,pver,begchunk:endchunk), stat=istat)
        if ( istat /= 0 ) then
          write(errmsg, '(2a,i0)') sub, ': allocate failed, stat = ',istat
          call endrun (errmsg)
        end if
        dqcond(i)%cnst = nan
      end do
    end if

  end subroutine diag_allocate_moist

  subroutine diag_allocate()

    call diag_allocate_dry()
    if (moist_physics) then
      call diag_allocate_moist()
    end if

  end subroutine diag_allocate

!===============================================================================

  subroutine diag_deallocate_dry()
    ! Deallocate memory for module variables.
    ! Done at the end of a physics step at same point as the pbuf deallocate for
    ! variables with "physpkg" scope.

    ! Local variables
    character(len=*), parameter :: sub = 'diag_deallocate_dry'
    integer :: istat

    deallocate(dtcond, stat=istat)
    if ( istat /= 0 ) call endrun (sub//': ERROR: deallocate failed')
  end subroutine diag_deallocate_dry

  subroutine diag_deallocate_moist()

    ! Deallocate memory for module variables.
    ! Done at the end of a physics step at same point as the pbuf deallocate for
    ! variables with "physpkg" scope.

    ! Local variables
    character(len=*), parameter :: sub = 'diag_deallocate_moist'
    integer :: i, istat

    if (dqcond_num > 0) then
      do i = 1, dqcond_num
        deallocate(dqcond(i)%cnst, stat=istat)
        if ( istat /= 0 ) call endrun (sub//': ERROR: deallocate failed')
      end do
      deallocate(dqcond, stat=istat)
      if ( istat /= 0 ) call endrun (sub//': ERROR: deallocate failed')
    end if
  end subroutine diag_deallocate_moist

  subroutine diag_deallocate()

    call diag_deallocate_dry()
    if (moist_physics) then
      call diag_deallocate_moist()
    end if

  end subroutine diag_deallocate

!===============================================================================

  subroutine diag_conv_tend_ini(state,pbuf)

    ! Initialize convective tendency calcs.

    ! Arguments:
    type(physics_state), intent(in) :: state
    type(physics_buffer_desc), pointer :: pbuf(:)

    ! Local variables:

    integer :: i, k, m, lchnk, ncol
    real(r8), pointer, dimension(:,:) :: t_ttend

    lchnk = state%lchnk
    ncol  = state%ncol

    do k = 1, pver
      do i = 1, ncol
        dtcond(i,k,lchnk) = state%t(i,k)
      end do
    end do

    do m = 1, dqcond_num
      do k = 1, pver
        do i = 1, ncol
          dqcond(m)%cnst(i,k,lchnk) = state%q(i,k,m)
        end do
      end do
    end do

    !! initialize to pbuf T_TTEND to temperature at first timestep
    if (is_first_step()) then
      do m = 1, dyn_time_lvls
        call pbuf_get_field(pbuf, t_ttend_idx, t_ttend, start=(/1,1,m/), kount=(/pcols,pver,1/))
        t_ttend(:ncol,:) = state%t(:ncol,:)
      end do
    end if

  end subroutine diag_conv_tend_ini

!===============================================================================

  subroutine diag_phys_writeout_dry(state, pbuf, p_surf_t)

    !-----------------------------------------------------------------------
    !
    ! Purpose: output dry physics diagnostics
    !
    !-----------------------------------------------------------------------
    use physconst,          only: gravit, rga, rair, cpair, latvap, rearth, pi, cappa
    use time_manager,       only: get_nstep
    use interpolate_data,   only: vertinterp
    use constituent_burden, only: constituent_burden_comp
    use co2_cycle,          only: c_i, co2_transport

    use tidal_diag,         only: tidal_diag_write
    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    type(physics_state), intent(inout) :: state
    type(physics_buffer_desc), pointer :: pbuf(:)
    real(r8),            intent(out)   :: p_surf_t(pcols, nsurf)  ! data interpolated to a pressure surface
    !
    !---------------------------Local workspace-----------------------------
    !
    real(r8) :: ftem(pcols,pver)  ! temporary workspace
    real(r8) :: ftem1(pcols,pver) ! another temporary workspace
    real(r8) :: ftem2(pcols,pver) ! another temporary workspace
    real(r8) :: z3(pcols,pver)    ! geo-potential height
    real(r8) :: p_surf(pcols)     ! data interpolated to a pressure surface
    real(r8) :: tem2(pcols,pver)  ! temporary workspace
    real(r8) :: timestep(pcols)   ! used for outfld call
    real(r8) :: esl(pcols,pver)   ! saturation vapor pressures
    real(r8) :: esi(pcols,pver)   !
    real(r8) :: dlon(pcols)       ! width of grid cell (meters)

    real(r8), pointer :: psl(:)   ! Sea Level Pressure

    integer  :: i, k, m, lchnk, ncol, nstep
    !
    !-----------------------------------------------------------------------
    !
    lchnk = state%lchnk
    ncol  = state%ncol

    ! Output NSTEP for debugging
    nstep = get_nstep()
    timestep(:ncol) = nstep
    call outfld ('NSTEP   ',timestep, pcols, lchnk)

    call outfld('T       ',state%t , pcols   ,lchnk   )
    call outfld('PS      ',state%ps, pcols   ,lchnk   )
    call outfld('U       ',state%u , pcols   ,lchnk   )
    call outfld('V       ',state%v , pcols   ,lchnk   )

    call outfld('PHIS    ',state%phis,    pcols,   lchnk     )

#if (defined BFB_CAM_SCAM_IOP )
    call outfld('phis    ',state%phis,    pcols,   lchnk     )
#endif

    do m = 1, pcnst
      if (cnst_cam_outfld(m)) then
        call outfld(cnst_name(m), state%q(1,1,m), pcols, lchnk)
      end if
    end do

    !
    ! Add height of surface to midpoint height above surface
    !
    do k = 1, pver
      z3(:ncol,k) = state%zm(:ncol,k) + state%phis(:ncol)*rga
    end do
    call outfld('Z3      ',z3,pcols,lchnk)
    !
    ! Output Z3 on pressure surfaces
    !
    if (hist_fld_active('Z1000')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 100000._r8, z3, p_surf, &
          extrapolate='Z', ln_interp=.true., ps=state%ps, phis=state%phis, tbot=state%t(:,pver))
      call outfld('Z1000    ', p_surf, pcols, lchnk)
    end if
    if (hist_fld_active('Z700')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 70000._r8, z3, p_surf, &
          extrapolate='Z', ln_interp=.true., ps=state%ps, phis=state%phis, tbot=state%t(:,pver))
      call outfld('Z700    ', p_surf, pcols, lchnk)
    end if
    if (hist_fld_active('Z500')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 50000._r8, z3, p_surf, &
          extrapolate='Z', ln_interp=.true., ps=state%ps, phis=state%phis, tbot=state%t(:,pver))
      call outfld('Z500    ', p_surf, pcols, lchnk)
    end if
    if (hist_fld_active('Z300')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 30000._r8, z3, p_surf, ln_interp=.true.)
      call outfld('Z300    ', p_surf, pcols, lchnk)
    end if
    if (hist_fld_active('Z200')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 20000._r8, z3, p_surf, ln_interp=.true.)
      call outfld('Z200    ', p_surf, pcols, lchnk)
    end if
    if (hist_fld_active('Z100')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 10000._r8, z3, p_surf, ln_interp=.true.)
      call outfld('Z100    ', p_surf, pcols, lchnk)
    end if
    if (hist_fld_active('Z050')) then
      call vertinterp(ncol, pcols, pver, state%pmid,  5000._r8, z3, p_surf, ln_interp=.true.)
      call outfld('Z050    ', p_surf, pcols, lchnk)
    end if
    if (hist_fld_active('Z010')) then
      call vertinterp(ncol, pcols, pver, state%pmid,  1000._r8, z3, p_surf, ln_interp=.true.)
      call outfld('Z010    ', p_surf, pcols, lchnk)
    end if
    if (hist_fld_active('UA010')) then
      call vertinterp(ncol, pcols, pver, state%pmid,  1000._r8, state%u, p_surf, ln_interp=.true.)
      call outfld('UA010   ', p_surf, pcols, lchnk)
    end if
    !
    ! Quadratic height fiels Z3*Z3
    !
    ftem(:ncol,:) = z3(:ncol,:)*z3(:ncol,:)
    call outfld('ZZ      ',ftem,pcols,lchnk)

    ftem(:ncol,:) = z3(:ncol,:)*state%v(:ncol,:)
    call outfld('VZ      ',ftem,  pcols,lchnk)
    !
    ! Meridional advection fields
    !
    ftem(:ncol,:) = state%v(:ncol,:)*state%t(:ncol,:)
    call outfld ('VT      ',ftem    ,pcols   ,lchnk     )

    if (hist_fld_active('VT100')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 10000._r8, ftem, p_surf, ln_interp=.true.)
      call outfld('VT100  ', p_surf, pcols, lchnk)
    end if

    ftem(:ncol,:) = state%v(:ncol,:)**2
    call outfld ('VV      ',ftem    ,pcols   ,lchnk     )

    ftem(:ncol,:) = state%v(:ncol,:) * state%u(:ncol,:)
    call outfld ('VU      ',ftem    ,pcols   ,lchnk     )
    !
    ! zonal advection
    !
    ftem(:ncol,:) = state%u(:ncol,:)**2
    call outfld ('UU      ',ftem    ,pcols   ,lchnk     )

    ! Wind speed
    ftem(:ncol,:) = sqrt( state%u(:ncol,:)**2 + state%v(:ncol,:)**2)
    call outfld ('WSPEED  ',ftem    ,pcols   ,lchnk     )
    call outfld ('WSPDSRFMX',ftem(:,pver)   ,pcols   ,lchnk     )
    call outfld ('WSPDSRFAV',ftem(:,pver)   ,pcols   ,lchnk     )

    ! Vertical velocity and advection

    if (single_column) then
      call outfld('OMEGA   ',wfld,    pcols,   lchnk     )
    else
      call outfld('OMEGA   ',state%omega,    pcols,   lchnk     )
    endif

#if (defined BFB_CAM_SCAM_IOP )
    call outfld('omega   ',state%omega,    pcols,   lchnk     )
#endif

    ftem(:ncol,:) = state%omega(:ncol,:)*state%t(:ncol,:)
    call outfld('OMEGAT  ',ftem,    pcols,   lchnk     )
    ftem(:ncol,:) = state%omega(:ncol,:)*state%u(:ncol,:)
    call outfld('OMEGAU  ',ftem,    pcols,   lchnk     )
    ftem(:ncol,:) = state%omega(:ncol,:)*state%v(:ncol,:)
    call outfld('OMEGAV  ',ftem,    pcols,   lchnk     )
    ftem(:ncol,:) = state%omega(:ncol,:)*state%omega(:ncol,:)
    call outfld('OMGAOMGA',ftem,    pcols,   lchnk     )
    !
    ! Output omega at 850 and 500 mb pressure levels
    !
    if (hist_fld_active('OMEGA850')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 85000._r8, state%omega, p_surf)
      call outfld('OMEGA850', p_surf, pcols, lchnk)
    end if
    if (hist_fld_active('OMEGA500')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 50000._r8, state%omega, p_surf)
      call outfld('OMEGA500', p_surf, pcols, lchnk)
    end if

    ! Sea level pressure
    call pbuf_get_field(pbuf, psl_idx, psl)
    call cpslec(ncol, state%pmid, state%phis, state%ps, state%t, psl, gravit, rair)
    call outfld('PSL', psl, pcols, lchnk)

    ! Output T,u,v fields on pressure surfaces
    !
    if (hist_fld_active('T850')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 85000._r8, state%t, p_surf, &
          extrapolate='T', ps=state%ps, phis=state%phis)
      call outfld('T850    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('T500')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 50000._r8, state%t, p_surf, &
          extrapolate='T', ps=state%ps, phis=state%phis)
      call outfld('T500    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('T400')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 40000._r8, state%t, p_surf, &
          extrapolate='T', ps=state%ps, phis=state%phis)
      call outfld('T400    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('T300')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 30000._r8, state%t, p_surf)
      call outfld('T300    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('T200')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 20000._r8, state%t, p_surf)
      call outfld('T200    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('U850')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 85000._r8, state%u, p_surf)
      call outfld('U850    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('U500')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 50000._r8, state%u, p_surf)
      call outfld('U500    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('U250')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 25000._r8, state%u, p_surf)
      call outfld('U250    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('U200')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 20000._r8, state%u, p_surf)
      call outfld('U200    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('U010')) then
      call vertinterp(ncol, pcols, pver, state%pmid,  1000._r8, state%u, p_surf)
      call outfld('U010    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('V850')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 85000._r8, state%v, p_surf)
      call outfld('V850    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('V500')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 50000._r8, state%v, p_surf)
      call outfld('V500    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('V250')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 25000._r8, state%v, p_surf)
      call outfld('V250    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('V200')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 20000._r8, state%v, p_surf)
      call outfld('V200    ', p_surf, pcols, lchnk )
    end if

    ftem(:ncol,:) = state%t(:ncol,:)*state%t(:ncol,:)
    call outfld('TT      ',ftem    ,pcols   ,lchnk   )
    !
    ! Output U, V, T, P and Z at bottom level
    !
    call outfld ('UBOT    ', state%u(1,pver)  ,  pcols, lchnk)
    call outfld ('VBOT    ', state%v(1,pver)  ,  pcols, lchnk)
    call outfld ('ZBOT    ', state%zm(1,pver) , pcols, lchnk)

    !! Boundary layer atmospheric stability, temperature, water vapor diagnostics

    p_surf_t = -99.0_r8 ! Uninitialized to impossible value
    if  (hist_fld_active('T1000')     .or. &
         hist_fld_active('T9251000')  .or. &
         hist_fld_active('TH9251000') .or. &
         hist_fld_active('T8501000')  .or. &
         hist_fld_active('TH8501000') .or. &
         hist_fld_active('T7001000')  .or. &
         hist_fld_active('TH7001000')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 100000._r8, state%t, p_surf_t(:,surf_100000))
    end if

    if ( hist_fld_active('T925')       .or. &
         hist_fld_active('T9251000')   .or. &
         hist_fld_active('TH9251000')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 92500._r8, state%t, p_surf_t(:,surf_092500))
    end if

!!! at 1000 mb and 925 mb
    if (hist_fld_active('T1000')) then
      call outfld('T1000    ', p_surf_t(:,surf_100000), pcols, lchnk )
    end if

    if (hist_fld_active('T925')) then
      call outfld('T925    ', p_surf_t(:,surf_092500), pcols, lchnk )
    end if

    if (hist_fld_active('T9251000')) then
      p_surf = p_surf_t(:,surf_092500) - p_surf_t(:,surf_100000)
      call outfld('T9251000    ', p_surf, pcols, lchnk )
    end if

    if (hist_fld_active('TH9251000')) then
      p_surf = (p_surf_t(:,surf_092500)*(1000.0_r8/925.0_r8)**cappa) - (p_surf_t(:,surf_100000)*(1.0_r8)**cappa)
      call outfld('TH9251000    ', p_surf, pcols, lchnk )
    end if

    if (hist_fld_active('T8501000')  .or. &
         hist_fld_active('TH8501000')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 85000._r8, state%t, p_surf_t(:,surf_085000))
    end if

!!! at 1000 mb and 850 mb
    if (hist_fld_active('T8501000')) then
      p_surf = p_surf_t(:,surf_085000)-p_surf_t(:,surf_100000)
      call outfld('T8501000    ', p_surf, pcols, lchnk )
    end if

    if (hist_fld_active('TH8501000')) then
      p_surf = (p_surf_t(:,surf_085000)*(1000.0_r8/850.0_r8)**cappa)-(p_surf_t(:,surf_100000)*(1.0_r8)**cappa)
      call outfld('TH8501000    ', p_surf, pcols, lchnk )
    end if

    if (hist_fld_active('T7001000')  .or. &
         hist_fld_active('TH7001000') .or. &
         hist_fld_active('T700')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 70000._r8, state%t, p_surf_t(:,surf_070000))
    end if

!!! at 700 mb
    if (hist_fld_active('T700')) then
      call outfld('T700    ', p_surf_t(:,surf_070000), pcols, lchnk )
    end if

!!! at 1000 mb and 700 mb
    if (hist_fld_active('T7001000')) then
      p_surf = p_surf_t(:,surf_070000)-p_surf_t(:,surf_100000)
      call outfld('T7001000    ', p_surf, pcols, lchnk )
    end if

    if (hist_fld_active('TH7001000')) then
      p_surf = (p_surf_t(:,surf_070000)*(1000.0_r8/700.0_r8)**cappa)-(p_surf_t(:,surf_100000)*(1.0_r8)**cappa)
      call outfld('TH7001000    ', p_surf, pcols, lchnk )
    end if

    if (hist_fld_active('T010')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 1000._r8, state%t, p_surf)
      call outfld('T010           ', p_surf, pcols, lchnk )
    end if

    !---------------------------------------------------------
    ! tidal diagnostics
    !---------------------------------------------------------
    call tidal_diag_write(state)

    return
  end subroutine diag_phys_writeout_dry

!===============================================================================

  subroutine diag_phys_writeout_moist(state, pbuf, p_surf_t)

    !-----------------------------------------------------------------------
    !
    ! Purpose: record dynamics variables on physics grid
    !
    !-----------------------------------------------------------------------
    use physconst,          only: gravit, rga, rair, cpair, latvap, rearth, pi, cappa, &
                                  epsilo, rh2o
    use interpolate_data,   only: vertinterp
    use constituent_burden, only: constituent_burden_comp
    use co2_cycle,          only: c_i, co2_transport
    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    type(physics_state), intent(inout) :: state
    type(physics_buffer_desc), pointer :: pbuf(:)
    real(r8),            intent(inout) :: p_surf_t(pcols, nsurf)  ! data interpolated to a pressure surface
    !
    !---------------------------Local workspace-----------------------------
    !
    real(r8) :: ftem(pcols,pver) ! temporary workspace
    real(r8) :: ftem1(pcols,pver) ! another temporary workspace
    real(r8) :: ftem2(pcols,pver) ! another temporary workspace
    real(r8) :: z3(pcols,pver)   ! geo-potential height
    real(r8) :: p_surf(pcols)    ! data interpolated to a pressure surface
    real(r8) :: p_surf_q1(pcols)    ! data interpolated to a pressure surface
    real(r8) :: p_surf_q2(pcols)    ! data interpolated to a pressure surface
    real(r8) :: tem2(pcols,pver) ! temporary workspace
    real(r8) :: esl(pcols,pver)   ! saturation vapor pressures
    real(r8) :: esi(pcols,pver)   !

    real(r8), pointer :: ftem_ptr(:,:)

    integer :: i, k, m, lchnk, ncol
    !
    !-----------------------------------------------------------------------
    !
    lchnk = state%lchnk
    ncol  = state%ncol
    do m=1,pcnst
      if ( cnst_cam_outfld(m) ) then
        call outfld(cnst_name(m),state%q(1,1,m),pcols ,lchnk )
      end if
    end do

    if (co2_transport()) then
      do m = 1,4
        call outfld(trim(cnst_name(c_i(m)))//'_BOT', state%q(1,pver,c_i(m)), pcols, lchnk)
      end do
    end if

    ! column burdens of all constituents except water vapor
    call constituent_burden_comp(state)

    call outfld('PSDRY',   state%psdry,   pcols, lchnk)
    call outfld('PMID',    state%pmid,    pcols, lchnk)
    call outfld('PDELDRY', state%pdeldry, pcols, lchnk)

    !
    ! Meridional advection fields
    !
    ftem(:ncol,:) = state%v(:ncol,:)*state%q(:ncol,:,1)
    call outfld ('VQ      ',ftem    ,pcols   ,lchnk     )

    ftem(:ncol,:) = state%q(:ncol,:,1)*state%q(:ncol,:,1)
    call outfld ('QQ      ',ftem    ,pcols   ,lchnk     )

    ! Vertical velocity and advection
    ftem(:ncol,:) = state%omega(:ncol,:)*state%q(:ncol,:,1)
    call outfld('OMEGAQ  ',ftem,    pcols,   lchnk     )
    !
    ! Mass of q, by layer and vertically integrated
    !
    ftem(:ncol,:) = state%q(:ncol,:,1) * state%pdel(:ncol,:) * rga
    call outfld ('MQ      ',ftem    ,pcols   ,lchnk     )

    do k=2,pver
      ftem(:ncol,1) = ftem(:ncol,1) + ftem(:ncol,k)
    end do
    call outfld ('TMQ     ',ftem, pcols   ,lchnk     )

    ! Relative humidity
    if (hist_fld_active('RELHUM')) then
       if (relhum_idx > 0) then
          call pbuf_get_field(pbuf, relhum_idx, ftem_ptr)
          ftem(:ncol,:) = ftem_ptr(:ncol,:)
       else
          call qsat(state%t(:ncol,:), state%pmid(:ncol,:), &
                    tem2(:ncol,:), ftem(:ncol,:))
          ftem(:ncol,:) = state%q(:ncol,:,1)/ftem(:ncol,:)*100._r8
       end if
       call outfld ('RELHUM  ',ftem    ,pcols   ,lchnk     )
    end if

#ifdef AEROCOM
          ! We want RHW output always when AEROCOM is on (not only if added to a namelist)
          ! RH w.r.t liquid (water)
          call qsat_water (state%t(:ncol,:), state%pmid(:ncol,:), &
               esl(:ncol,:), ftem(:ncol,:))
          ftem(:ncol,:) = state%q(:ncol,:,1)/ftem(:ncol,:)*100._r8
          call outfld ('RHW  ',ftem    ,pcols   ,lchnk     )
#endif

    if (hist_fld_active('RHW') .or. hist_fld_active('RHI') .or. hist_fld_active('RHCFMIP') ) then

#ifndef AEROCOM
      ! RH w.r.t liquid (water)
      call qsat_water (state%t(:ncol,:), state%pmid(:ncol,:), &
           esl(:ncol,:), ftem(:ncol,:))
      ftem(:ncol,:) = state%q(:ncol,:,1)/ftem(:ncol,:)*100._r8
      call outfld ('RHW  ',ftem    ,pcols   ,lchnk     )
#endif AEROCOM

      ! Convert to RHI (ice)
      do i=1,ncol
        do k=1,pver
          esi(i,k)=svp_ice(state%t(i,k))
          ftem1(i,k)=ftem(i,k)*esl(i,k)/esi(i,k)
        end do
      end do
      call outfld ('RHI  ',ftem1    ,pcols   ,lchnk     )

      ! use temperature to decide if you populate with ftem (liquid, above 0 C) or ftem1 (ice, below 0 C)

      ftem2(:ncol,:)=ftem(:ncol,:)

      do i=1,ncol
        do k=1,pver
          if (state%t(i,k) .gt. 273) then
            ftem2(i,k)=ftem(i,k)  !!wrt water
          else
            ftem2(i,k)=ftem1(i,k) !!wrt ice
          end if
        end do
      end do

      call outfld ('RHCFMIP  ',ftem2    ,pcols   ,lchnk     )

    end if
    !
    ! Output q field on pressure surfaces
    !
    if (hist_fld_active('Q850')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 85000._r8, state%q(1,1,1), p_surf)
      call outfld('Q850    ', p_surf, pcols, lchnk )
    end if
    if (hist_fld_active('Q200')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 20000._r8, state%q(1,1,1), p_surf)
      call outfld('Q200    ', p_surf, pcols, lchnk )
    end if
    !
    ! Output Q at bottom level
    !
    call outfld ('QBOT    ', state%q(1,pver,1),  pcols, lchnk)

    ! Total energy of the atmospheric column for atmospheric heat storage calculations

    !! temporary variable to get surface geopotential in dimensions of (ncol,pver)
    do k=1,pver
      ftem1(:ncol,k)=state%phis(:ncol)  !! surface geopotential in units (m2/s2)
    end do

    !! calculate sum of sensible, kinetic, latent, and surface geopotential energy
    !! E=CpT+PHIS+Lv*q+(0.5)*(u^2+v^2)
    ftem(:ncol,:) = (cpair*state%t(:ncol,:) +  ftem1(:ncol,:) + latvap*state%q(:ncol,:,1) + &
         0.5_r8*(state%u(:ncol,:)**2+state%v(:ncol,:)**2))*(state%pdel(:ncol,:)/gravit)
    !! vertically integrate
    do k=2,pver
      ftem(:ncol,1) = ftem(:ncol,1) + ftem(:ncol,k)
    end do
    call outfld ('ATMEINT   ',ftem(:ncol,1)  ,pcols   ,lchnk     )

    !! Boundary layer atmospheric stability, temperature, water vapor diagnostics

    if ( hist_fld_active('THE9251000') .or. &
         hist_fld_active('THE8501000') .or. &
         hist_fld_active('THE7001000')) then
      if (p_surf_t(1, surf_100000) < 0.0_r8) then
        call vertinterp(ncol, pcols, pver, state%pmid, 100000._r8, state%t, p_surf_t(:, surf_100000))
      end if
    end if

    if ( hist_fld_active('TH9251000')  .or. &
         hist_fld_active('THE9251000')) then
      if (p_surf_t(1, surf_092500) < 0.0_r8) then
        call vertinterp(ncol, pcols, pver, state%pmid, 92500._r8, state%t, p_surf_t(:, surf_092500))
      end if
    end if

    if ( hist_fld_active('Q1000')      .or. &
         hist_fld_active('THE9251000') .or. &
         hist_fld_active('THE8501000') .or. &
         hist_fld_active('THE7001000')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 100000._r8, state%q(1,1,1), p_surf_q1)
    end if

    if (hist_fld_active('THE9251000')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 92500._r8, state%q(1,1,1), p_surf_q2)
    end if

!!! at 1000 mb and 925 mb
    if (hist_fld_active('Q1000')) then
      call outfld('Q1000    ', p_surf_q1, pcols, lchnk )
    end if

    if (hist_fld_active('Q925')) then
      call outfld('Q925    ', p_surf_q2, pcols, lchnk )
    end if

    if (hist_fld_active('THE9251000')) then
      p_surf = ((p_surf_t(:, surf_092500)*(1000.0_r8/925.0_r8)**cappa) *              &
                exp((2500000.0_r8*p_surf_q2)/(1004.0_r8*p_surf_t(:, surf_092500)))) - &
                (p_surf_t(:,surf_100000)*(1.0_r8)**cappa)*exp((2500000.0_r8*p_surf_q1)/(1004.0_r8*p_surf_t(:,surf_100000)))
      call outfld('THE9251000    ', p_surf, pcols, lchnk )
    end if

    if (hist_fld_active('THE8501000')) then
      if (p_surf_t(1, surf_085000) < 0.0_r8) then
        call vertinterp(ncol, pcols, pver, state%pmid, 85000._r8, state%t, p_surf_t(:, surf_085000))
      end if
    end if

!!! at 1000 mb and 850 mb
    if (hist_fld_active('THE8501000')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 85000._r8, state%q(1,1,1), p_surf_q2)
      p_surf = ((p_surf_t(:, surf_085000)*(1000.0_r8/850.0_r8)**cappa) *              &
                exp((2500000.0_r8*p_surf_q2)/(1004.0_r8*p_surf_t(:, surf_085000)))) - &
                (p_surf_t(:,surf_100000)*(1.0_r8)**cappa)*exp((2500000.0_r8*p_surf_q1)/(1004.0_r8*p_surf_t(:,surf_100000)))
      call outfld('THE8501000    ', p_surf, pcols, lchnk )
    end if

    if (hist_fld_active('THE7001000')) then
      if (p_surf_t(1, surf_070000) < 0.0_r8) then
        call vertinterp(ncol, pcols, pver, state%pmid, 70000._r8, state%t, p_surf_t(:, surf_070000))
      end if
    end if

!!! at 1000 mb and 700 mb
    if (hist_fld_active('THE7001000')) then
      call vertinterp(ncol, pcols, pver, state%pmid, 70000._r8, state%q(1,1,1), p_surf_q2)
      p_surf = ((p_surf_t(:, surf_070000)*(1000.0_r8/700.0_r8)**cappa) *              &
                exp((2500000.0_r8*p_surf_q2)/(1004.0_r8*p_surf_t(:, surf_070000)))) - &
                (p_surf_t(:,surf_100000)*(1.0_r8)**cappa)*exp((2500000.0_r8*p_surf_q1)/(1004.0_r8*p_surf_t(:,surf_100000)))
      call outfld('THE7001000    ', p_surf, pcols, lchnk )
    end if

    return
  end subroutine diag_phys_writeout_moist

!===============================================================================

  subroutine diag_phys_writeout(state, pbuf)

    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    type(physics_state), intent(inout) :: state
    type(physics_buffer_desc), pointer :: pbuf(:)

    !
    ! Local variable
    !
    real(r8) :: p_surf_t(pcols, nsurf)  ! data interpolated to a pressure surface

    call diag_phys_writeout_dry(state, pbuf, p_surf_t)

    if (moist_physics) then
      call diag_phys_writeout_moist(state, pbuf, p_surf_t)
    end if

  end subroutine diag_phys_writeout

!===============================================================================

  subroutine diag_conv(state, ztodt, pbuf)

    !-----------------------------------------------------------------------
    !
    ! Output diagnostics associated with all convective processes.
    !
    !-----------------------------------------------------------------------
    use physconst,     only: cpair
    use tidal_diag,    only: get_tidal_coeffs

    ! Arguments:

    real(r8),            intent(in) :: ztodt   ! timestep for computing physics tendencies
    type(physics_state), intent(in) :: state
    type(physics_buffer_desc), pointer :: pbuf(:)

    ! convective precipitation variables
    real(r8), pointer :: prec_dp(:)                 ! total precipitation   from ZM convection
    real(r8), pointer :: snow_dp(:)                 ! snow from ZM   convection
    real(r8), pointer :: prec_sh(:)                 ! total precipitation   from Hack convection
    real(r8), pointer :: snow_sh(:)                 ! snow from   Hack   convection
    real(r8), pointer :: prec_sed(:)                ! total precipitation   from ZM convection
    real(r8), pointer :: snow_sed(:)                ! snow from ZM   convection
    real(r8), pointer :: prec_pcw(:)                ! total precipitation   from Hack convection
    real(r8), pointer :: snow_pcw(:)                ! snow from Hack   convection

    ! Local variables:

    integer :: i, k, m, lchnk, ncol

    real(r8) :: rtdt

    real(r8):: precc(pcols)                ! convective precip rate
    real(r8):: precl(pcols)                ! stratiform precip rate
    real(r8):: snowc(pcols)                ! convective snow rate
    real(r8):: snowl(pcols)                ! stratiform snow rate
    real(r8):: prect(pcols)                ! total (conv+large scale) precip rate
    real(r8) :: dcoef(6)                   ! for tidal component of T tend

    lchnk = state%lchnk
    ncol  = state%ncol

    rtdt = 1._r8/ztodt

    if (moist_physics) then
      if (prec_dp_idx > 0) then
        call pbuf_get_field(pbuf, prec_dp_idx, prec_dp)
      else
        nullify(prec_dp)
      end if
      if (snow_dp_idx > 0) then
        call pbuf_get_field(pbuf, snow_dp_idx, snow_dp)
      else
        nullify(snow_dp)
      end if
      if (prec_sh_idx > 0) then
        call pbuf_get_field(pbuf, prec_sh_idx, prec_sh)
      else
        nullify(prec_sh)
      end if
      if (snow_sh_idx > 0) then
        call pbuf_get_field(pbuf, snow_sh_idx, snow_sh)
      else
        nullify(snow_sh)
      end if
      if (prec_sed_idx > 0) then
        call pbuf_get_field(pbuf, prec_sed_idx, prec_sed)
      else
        nullify(prec_sed)
      end if
      if (snow_sed_idx > 0) then
        call pbuf_get_field(pbuf, snow_sed_idx, snow_sed)
      else
        nullify(snow_sed)
      end if
      if (prec_pcw_idx > 0) then
        call pbuf_get_field(pbuf, prec_pcw_idx, prec_pcw)
      else
        nullify(prec_pcw)
      end if
      if (snow_pcw_idx > 0) then
        call pbuf_get_field(pbuf, snow_pcw_idx, snow_pcw)
      else
        nullify(snow_pcw)
      end if

      ! Precipitation rates (multi-process)
      if (associated(prec_dp) .and. associated(prec_sh)) then
        precc(:ncol) = prec_dp(:ncol)  + prec_sh(:ncol)
      else if (associated(prec_dp)) then
        precc(:ncol) = prec_dp(:ncol)
      else if (associated(prec_sh)) then
        precc(:ncol) = prec_sh(:ncol)
      else
        precc(:ncol) = 0._r8
      end if
      if (associated(prec_sed) .and. associated(prec_pcw)) then
        precl(:ncol) = prec_sed(:ncol) + prec_pcw(:ncol)
      else if (associated(prec_sed)) then
        precl(:ncol) = prec_sed(:ncol)
      else if (associated(prec_pcw)) then
        precl(:ncol) = prec_pcw(:ncol)
      else
        precl(:ncol) = 0._r8
      end if
      if (associated(snow_dp) .and. associated(snow_sh)) then
        snowc(:ncol) = snow_dp(:ncol)  + snow_sh(:ncol)
      else if (associated(snow_dp)) then
        snowc(:ncol) = snow_dp(:ncol)
      else if (associated(snow_sh)) then
        snowc(:ncol) = snow_sh(:ncol)
      else
        snowc(:ncol) = 0._r8
      end if
      if (associated(snow_sed) .and. associated(snow_pcw)) then
        snowl(:ncol) = snow_sed(:ncol) + snow_pcw(:ncol)
      else if (associated(snow_sed)) then
        snowl(:ncol) = snow_sed(:ncol)
      else if (associated(snow_pcw)) then
        snowl(:ncol) = snow_pcw(:ncol)
      else
        snowl(:ncol) = 0._r8
      end if
      prect(:ncol) = precc(:ncol)    + precl(:ncol)

      call outfld('PRECC   ', precc, pcols, lchnk )
      call outfld('PRECL   ', precl, pcols, lchnk )
      if (associated(prec_pcw)) then
        call outfld('PREC_PCW', prec_pcw,pcols   ,lchnk )
      end if
      if (associated(prec_dp)) then
        call outfld('PREC_zmc', prec_dp ,pcols   ,lchnk )
      end if
      call outfld('PRECSC  ', snowc, pcols, lchnk )
      call outfld('PRECSL  ', snowl, pcols, lchnk )
      call outfld('PRECT   ', prect, pcols, lchnk )
      call outfld('PRECTMX ', prect, pcols, lchnk )

      call outfld('PRECLav ', precl, pcols, lchnk )
      call outfld('PRECCav ', precc, pcols, lchnk )

#if ( defined BFB_CAM_SCAM_IOP )
      call outfld('Prec   ' , prect, pcols, lchnk )
#endif

      ! Total convection tendencies.

      do k = 1, pver
        do i = 1, ncol
          dtcond(i,k,lchnk) = (state%t(i,k) - dtcond(i,k,lchnk))*rtdt
        end do
      end do
      call outfld('DTCOND  ', dtcond(:,:,lchnk), pcols, lchnk)

      ! output tidal coefficients
      call get_tidal_coeffs( dcoef )
      call outfld( 'DTCOND_24_SIN', dtcond(:ncol,:,lchnk)*dcoef(1), ncol, lchnk )
      call outfld( 'DTCOND_24_COS', dtcond(:ncol,:,lchnk)*dcoef(2), ncol, lchnk )
      call outfld( 'DTCOND_12_SIN', dtcond(:ncol,:,lchnk)*dcoef(3), ncol, lchnk )
      call outfld( 'DTCOND_12_COS', dtcond(:ncol,:,lchnk)*dcoef(4), ncol, lchnk )
      call outfld( 'DTCOND_08_SIN', dtcond(:ncol,:,lchnk)*dcoef(5), ncol, lchnk )
      call outfld( 'DTCOND_08_COS', dtcond(:ncol,:,lchnk)*dcoef(6), ncol, lchnk )

      do m = 1, dqcond_num
        if ( cnst_cam_outfld(m) ) then
          do k = 1, pver
            do i = 1, ncol
              dqcond(m)%cnst(i,k,lchnk) = (state%q(i,k,m) - dqcond(m)%cnst(i,k,lchnk))*rtdt
            end do
          end do
          call outfld(dcconnam(m), dqcond(m)%cnst(:,:,lchnk), pcols, lchnk)
        end if
      end do

    end if
  end subroutine diag_conv

!===============================================================================

  subroutine diag_surf (cam_in, cam_out, state, pbuf)

    !-----------------------------------------------------------------------
    !
    ! Purpose: record surface diagnostics
    !
    !-----------------------------------------------------------------------

    use time_manager,     only: is_end_curr_day
    use co2_cycle,        only: c_i, co2_transport
    use constituents,     only: sflxnam

    !-----------------------------------------------------------------------
    !
    ! Input arguments
    !
    type(cam_in_t),  intent(in) :: cam_in
    type(cam_out_t), intent(in) :: cam_out
    type(physics_state), intent(in)    :: state
    type(physics_buffer_desc), pointer :: pbuf(:)
    !
    !---------------------------Local workspace-----------------------------
    !
    integer :: i, k, m      ! indexes
    integer :: lchnk        ! chunk identifier
    integer :: ncol         ! longitude dimension
    real(r8) tem2(pcols)    ! temporary workspace
    real(r8) ftem(pcols)    ! temporary workspace

    real(r8), pointer :: trefmnav(:) ! daily minimum tref
    real(r8), pointer :: trefmxav(:) ! daily maximum tref

    !
    !-----------------------------------------------------------------------
    !
    lchnk = cam_in%lchnk
    ncol  = cam_in%ncol

    if (moist_physics) then
      call outfld('SHFLX',    cam_in%shf,       pcols, lchnk)
      call outfld('LHFLX',    cam_in%lhf,       pcols, lchnk)
      call outfld('QFLX',     cam_in%cflx(1,1), pcols, lchnk)

      call outfld('TAUX',     cam_in%wsx,       pcols, lchnk)
      call outfld('TAUY',     cam_in%wsy,       pcols, lchnk)
      call outfld('TREFHT  ', cam_in%tref,      pcols, lchnk)
      call outfld('TREFHTMX', cam_in%tref,      pcols, lchnk)
      call outfld('TREFHTMN', cam_in%tref,      pcols, lchnk)
      call outfld('QREFHT',   cam_in%qref,      pcols, lchnk)
      call outfld('U10',      cam_in%u10,       pcols, lchnk)
      !
      ! Calculate and output reference height RH (RHREFHT)

      call qsat(cam_in%tref(:ncol), state%ps(:ncol), tem2(:ncol), ftem(:ncol))
      ftem(:ncol) = cam_in%qref(:ncol)/ftem(:ncol)*100._r8


      call outfld('RHREFHT',   ftem,      pcols, lchnk)


#if (defined BFB_CAM_SCAM_IOP )
      call outfld('shflx   ',cam_in%shf,   pcols,   lchnk)
      call outfld('lhflx   ',cam_in%lhf,   pcols,   lchnk)
      call outfld('trefht  ',cam_in%tref,  pcols,   lchnk)
#endif
      !
      ! Ouput ocn and ice fractions
      !
      call outfld('LANDFRAC', cam_in%landfrac, pcols, lchnk)
      call outfld('ICEFRAC',  cam_in%icefrac,  pcols, lchnk)
      call outfld('OCNFRAC',  cam_in%ocnfrac,  pcols, lchnk)
      !
      ! Compute daily minimum and maximum of TREF
      !
      call pbuf_get_field(pbuf, trefmxav_idx, trefmxav)
      call pbuf_get_field(pbuf, trefmnav_idx, trefmnav)
      do i = 1,ncol
        trefmxav(i) = max(cam_in%tref(i),trefmxav(i))
        trefmnav(i) = min(cam_in%tref(i),trefmnav(i))
      end do
      if (is_end_curr_day()) then
        call outfld('TREFMXAV', trefmxav,pcols,   lchnk     )
        call outfld('TREFMNAV', trefmnav,pcols,   lchnk     )
        trefmxav(:ncol) = -1.0e36_r8
        trefmnav(:ncol) =  1.0e36_r8
      endif

      call outfld('TBOT',     cam_out%tbot,     pcols, lchnk)
      call outfld('TS',       cam_in%ts,        pcols, lchnk)
      call outfld('TSMN',     cam_in%ts,        pcols, lchnk)
      call outfld('TSMX',     cam_in%ts,        pcols, lchnk)
      call outfld('SNOWHLND', cam_in%snowhland, pcols, lchnk)
      call outfld('SNOWHICE', cam_in%snowhice,  pcols, lchnk)
      call outfld('ASDIR',    cam_in%asdir,     pcols, lchnk)
      call outfld('ASDIF',    cam_in%asdif,     pcols, lchnk)
      call outfld('ALDIR',    cam_in%aldir,     pcols, lchnk)
      call outfld('ALDIF',    cam_in%aldif,     pcols, lchnk)
      call outfld('SST',      cam_in%sst,       pcols, lchnk)

      if (co2_transport()) then
        do m = 1,4
          call outfld(sflxnam(c_i(m)), cam_in%cflx(:,c_i(m)), pcols, lchnk)
        end do
      end if
    end if

  end subroutine diag_surf

!===============================================================================

  subroutine diag_export(cam_out)

    !-----------------------------------------------------------------------
    !
    ! Purpose: Write export state to history file
    !
    !-----------------------------------------------------------------------

    ! arguments
    type(cam_out_t), intent(inout) :: cam_out

    ! Local variables:
    integer :: lchnk        ! chunk identifier
    logical :: atm_dep_flux ! true ==> sending deposition fluxes to coupler.
    ! Otherwise, set them to zero.
    !-----------------------------------------------------------------------

    lchnk = cam_out%lchnk

    call phys_getopts(atm_dep_flux_out=atm_dep_flux)

    if (.not. atm_dep_flux) then
      ! set the fluxes to zero before outfld and sending them to the
      ! coupler
      cam_out%bcphiwet = 0.0_r8
      cam_out%bcphidry = 0.0_r8
      cam_out%bcphodry = 0.0_r8
      cam_out%ocphiwet = 0.0_r8
      cam_out%ocphidry = 0.0_r8
      cam_out%ocphodry = 0.0_r8
      cam_out%dstwet1  = 0.0_r8
      cam_out%dstdry1  = 0.0_r8
      cam_out%dstwet2  = 0.0_r8
      cam_out%dstdry2  = 0.0_r8
      cam_out%dstwet3  = 0.0_r8
      cam_out%dstdry3  = 0.0_r8
      cam_out%dstwet4  = 0.0_r8
      cam_out%dstdry4  = 0.0_r8
    end if

    if (moist_physics) then
      call outfld('a2x_BCPHIWET', cam_out%bcphiwet, pcols, lchnk)
      call outfld('a2x_BCPHIDRY', cam_out%bcphidry, pcols, lchnk)
      call outfld('a2x_BCPHODRY', cam_out%bcphodry, pcols, lchnk)
      call outfld('a2x_OCPHIWET', cam_out%ocphiwet, pcols, lchnk)
      call outfld('a2x_OCPHIDRY', cam_out%ocphidry, pcols, lchnk)
      call outfld('a2x_OCPHODRY', cam_out%ocphodry, pcols, lchnk)
      call outfld('a2x_DSTWET1',  cam_out%dstwet1,  pcols, lchnk)
      call outfld('a2x_DSTDRY1',  cam_out%dstdry1,  pcols, lchnk)
      call outfld('a2x_DSTWET2',  cam_out%dstwet2,  pcols, lchnk)
      call outfld('a2x_DSTDRY2',  cam_out%dstdry2,  pcols, lchnk)
      call outfld('a2x_DSTWET3',  cam_out%dstwet3,  pcols, lchnk)
      call outfld('a2x_DSTDRY3',  cam_out%dstdry3,  pcols, lchnk)
      call outfld('a2x_DSTWET4',  cam_out%dstwet4,  pcols, lchnk)
      call outfld('a2x_DSTDRY4',  cam_out%dstdry4,  pcols, lchnk)
    end if

  end subroutine diag_export

!#######################################################################

  subroutine diag_physvar_ic (lchnk,  pbuf, cam_out, cam_in)
    !
    !---------------------------------------------
    !
    ! Purpose: record physics variables on IC file
    !
    !---------------------------------------------
    !

    !
    ! Arguments
    !
    integer       , intent(in) :: lchnk  ! chunk identifier
    type(physics_buffer_desc), pointer :: pbuf(:)

    type(cam_out_t), intent(inout) :: cam_out
    type(cam_in_t),  intent(inout) :: cam_in
    !
    !---------------------------Local workspace-----------------------------
    !
    integer  :: k                 ! indices
    integer  :: itim_old          ! indices

    real(r8), pointer, dimension(:,:) :: cwat_var
    real(r8), pointer, dimension(:,:) :: conv_var_3d
    real(r8), pointer, dimension(:  ) :: conv_var_2d
    real(r8), pointer :: tpert(:), pblh(:), qpert(:)
    !
    !-----------------------------------------------------------------------
    !
    if( write_inithist() .and. moist_physics ) then

      !
      ! Associate pointers with physics buffer fields
      !
      itim_old = pbuf_old_tim_idx()

      if (qcwat_idx > 0) then
        call pbuf_get_field(pbuf, qcwat_idx, cwat_var, start=(/1,1,itim_old/), kount=(/pcols,pver,1/) )
        call outfld('QCWAT&IC   ',cwat_var, pcols,lchnk)
      end if

      if (tcwat_idx > 0) then
        call pbuf_get_field(pbuf, tcwat_idx,  cwat_var, start=(/1,1,itim_old/), kount=(/pcols,pver,1/) )
        call outfld('TCWAT&IC   ',cwat_var, pcols,lchnk)
      end if

      if (lcwat_idx > 0) then
        call pbuf_get_field(pbuf, lcwat_idx,  cwat_var, start=(/1,1,itim_old/), kount=(/pcols,pver,1/) )
        call outfld('LCWAT&IC   ',cwat_var, pcols,lchnk)
      end if

      if (cld_idx > 0) then
        call pbuf_get_field(pbuf, cld_idx,    cwat_var, start=(/1,1,itim_old/), kount=(/pcols,pver,1/) )
        call outfld('CLOUD&IC   ',cwat_var, pcols,lchnk)
      end if

      if (concld_idx > 0) then
        call pbuf_get_field(pbuf, concld_idx, cwat_var, start=(/1,1,itim_old/), kount=(/pcols,pver,1/) )
        call outfld('CONCLD&IC   ',cwat_var, pcols,lchnk)
      end if

      if (cush_idx > 0) then
        call pbuf_get_field(pbuf, cush_idx, conv_var_2d ,(/1,itim_old/),  (/pcols,1/))
        call outfld('CUSH&IC   ',conv_var_2d, pcols,lchnk)

      end if

      if (tke_idx > 0) then
        call pbuf_get_field(pbuf, tke_idx, conv_var_3d)
        call outfld('TKE&IC    ',conv_var_3d, pcols,lchnk)
      end if

      if (kvm_idx > 0) then
        call pbuf_get_field(pbuf, kvm_idx,  conv_var_3d)
        call outfld('KVM&IC    ',conv_var_3d, pcols,lchnk)
      end if

      if (kvh_idx > 0) then
        call pbuf_get_field(pbuf, kvh_idx,  conv_var_3d)
        call outfld('KVH&IC    ',conv_var_3d, pcols,lchnk)
      end if

      if (qpert_idx > 0) then
        call pbuf_get_field(pbuf, qpert_idx, qpert)
        call outfld('QPERT&IC   ', qpert, pcols, lchnk)
      end if

      if (pblh_idx > 0) then
        call pbuf_get_field(pbuf, pblh_idx,  pblh)
        call outfld('PBLH&IC    ', pblh,  pcols, lchnk)
      end if

      if (tpert_idx > 0) then
        call pbuf_get_field(pbuf, tpert_idx, tpert)
        call outfld('TPERT&IC   ', tpert, pcols, lchnk)
      end if

    end if

  end subroutine diag_physvar_ic


!#######################################################################

 !subroutine diag_phys_tend_writeout_dry(state, pbuf, tend, ztodt)
  subroutine diag_phys_tend_writeout_dry(state, pbuf, tend, ztodt, tmp_t, eflx, dsema) !tht

    !---------------------------------------------------------------
    !
    ! Purpose:  Dump physics tendencies for temperature
    !
    !---------------------------------------------------------------

    use check_energy,    only: check_energy_get_integrals
    use physconst,       only: cpair

    ! Arguments

    type(physics_state), intent(in)    :: state

    type(physics_buffer_desc), pointer :: pbuf(:)
    type(physics_tend ), intent(in)    :: tend
    real(r8),            intent(in)    :: ztodt             ! physics timestep

    real(r8)           , intent(inout) :: tmp_t     (pcols,pver) !tht: holds last physics_updated T (FV)
    real(r8)           , intent(in), optional ::eflx (pcols    ) !tht: surface sensible heat flux assoc.with mass adj.
    real(r8)           , intent(in), optional ::dsema(pcols    ) !tht: column enthalpy tendency assoc. with mass adj.

    !---------------------------Local workspace-----------------------------

    integer  :: lchnk             ! chunk index
    integer  :: ncol              ! number of columns in chunk
    real(r8) :: ftem2(pcols)      ! Temporary workspace for outfld variables
    real(r8) :: ftem3(pcols,pver) ! Temporary workspace for outfld variables
    real(r8) :: heat_glob         ! tht: T-tend from fixer (FV only)
    real(r8) :: tedif_glob        !+tht energy flux from fixer (FV only)
    ! CAM pointers to get variables from the physics buffer
    real(r8), pointer, dimension(:,:) :: t_ttend
    integer  :: itim_old,m

    !-----------------------------------------------------------------------

    lchnk = state%lchnk
    ncol  = state%ncol

    ! Dump out post-physics state (FV only)

    call outfld('TAP', state%t, pcols, lchnk   )
    call outfld('UAP', state%u, pcols, lchnk   )
    call outfld('VAP', state%v, pcols, lchnk   )

    !tht: heat tendencies from dme_adjust
    if (dycore_is('LR')) then
      tmp_t(:ncol,:pver) = (state%t(:ncol,:pver) - tmp_t(:ncol,:pver))/ztodt ! T tendency
      call outfld('PTTEND_DME', tmp_t, pcols, lchnk   )
      if(present(dsema))call outfld('IETEND_DME', dsema, pcols, lchnk)       ! dry enthalpy
      if(present(eflx) )call outfld('EFLX'      ,  eflx, pcols, lchnk)       ! moist enthalpy
    end if

    ! Total physics tendency for Temperature
    ! (remove global fixer tendency from total for FV and SE dycores)

    if (dycore_is('LR') .or. dycore_is('SE')) then
      call check_energy_get_integrals( heat_glob_out=heat_glob , tedif_glob_out=tedif_glob ) !+tht tedif
      ftem2(:ncol)  = heat_glob/cpair
      call outfld('TFIX', ftem2, pcols, lchnk   )
!+tht
      ftem2(:ncol)  = tedif_glob/ztodt
      call outfld('EBREAK', ftem2, pcols, lchnk   )
!-tht
      ftem3(:ncol,:pver)  = tend%dtdt(:ncol,:pver) - heat_glob/cpair
    else
      ftem3(:ncol,:pver)  = tend%dtdt(:ncol,:pver)
    end if
    call outfld('PTTEND',ftem3, pcols, lchnk )

    ! Total (physics+dynamics, everything!) tendency for Temperature

    !! get temperature stored in physics buffer
    itim_old = pbuf_old_tim_idx()
    call pbuf_get_field(pbuf, t_ttend_idx, t_ttend, start=(/1,1,itim_old/), kount=(/pcols,pver,1/))

    !! calculate and outfld the total temperature tendency
    ftem3(:ncol,:) = (state%t(:ncol,:) - t_ttend(:ncol,:))/ztodt
    call outfld('TTEND_TOT', ftem3, pcols, lchnk)

    !! update physics buffer with this time-step's temperature
    t_ttend(:ncol,:) = state%t(:ncol,:)

  end subroutine diag_phys_tend_writeout_dry

!#######################################################################

  subroutine diag_phys_tend_writeout_moist(state, pbuf,  tend, ztodt,         &
       tmp_q, tmp_cldliq, tmp_cldice, tmp_cldnc, tmp_cldni                   &
       ,qini, cldliqini, cldiceini , cldncini, cldniini )

    !---------------------------------------------------------------
    !
    ! Purpose:  Dump physics tendencies for moisture
    !
    !---------------------------------------------------------------

    ! Arguments

    type(physics_state), intent(in)    :: state

    type(physics_buffer_desc), pointer :: pbuf(:)
    type(physics_tend ), intent(in)    :: tend
    real(r8),            intent(in)    :: ztodt                  ! physics timestep
    real(r8),            intent(inout) :: tmp_q     (pcols,pver) ! As input, holds pre-adjusted tracers (FV)
    real(r8),            intent(inout) :: tmp_cldliq(pcols,pver) ! As input, holds pre-adjusted tracers (FV)
    real(r8),            intent(inout) :: tmp_cldice(pcols,pver) ! As input, holds pre-adjusted tracers (FV)
    real(r8),            intent(in)    :: qini      (pcols,pver) ! tracer fields at beginning of physics
    real(r8),            intent(in)    :: cldliqini (pcols,pver) ! tracer fields at beginning of physics
    real(r8),            intent(in)    :: cldiceini (pcols,pver) ! tracer fields at beginning of physics
!AL
   real(r8)           , intent(inout) :: tmp_cldnc(pcols,pver) ! As input, holds pre-adjusted tracers (FV)
   real(r8)           , intent(inout) :: tmp_cldni(pcols,pver) ! As input, holds pre-adjusted tracers (FV)
   real(r8)           , intent(in   ) :: cldncini (pcols,pver) ! tracer fields at beginning of physics
   real(r8)           , intent(in   ) :: cldniini (pcols,pver) ! tracer fields at beginning of physics
!AL
    !---------------------------Local workspace-----------------------------

    integer  :: lchnk  ! chunk index
    integer  :: ncol   ! number of columns in chunk
    real(r8) :: ftem3(pcols,pver) ! Temporary workspace for outfld variables
    real(r8) :: rtdt
    integer  :: ixcldice, ixcldliq! constituent indices for cloud liquid and ice water.
!AL
   integer  :: ixnumice, ixnumliq! constituent indices for cloud liquid and ice water.
!AL

    lchnk = state%lchnk
    ncol  = state%ncol
    rtdt  = 1._r8/ztodt
    call cnst_get_ind('CLDLIQ', ixcldliq, abort=.false.)
    call cnst_get_ind('CLDICE', ixcldice, abort=.false.)
!AL
    call cnst_get_ind('NUMLIQ', ixnumliq)
    call cnst_get_ind('NUMICE', ixnumice)
!AL

    if ( cnst_cam_outfld(       1) ) then
      call outfld (apcnst(       1), state%q(1,1,       1), pcols, lchnk)
    end if
    if (ixcldliq > 0) then
      if (cnst_cam_outfld(ixcldliq)) then
        call outfld (apcnst(ixcldliq), state%q(1,1,ixcldliq), pcols, lchnk)
      end if
    end if
    if (ixcldice > 0) then
      if ( cnst_cam_outfld(ixcldice) ) then
        call outfld (apcnst(ixcldice), state%q(1,1,ixcldice), pcols, lchnk)
      end if
    end if

    ! Tendency for dry mass adjustment of q (FV only)

    if (dycore_is('LR')) then
      tmp_q     (:ncol,:pver) = (state%q(:ncol,:pver,       1) - tmp_q     (:ncol,:pver))*rtdt
      if (ixcldliq > 0) then
        tmp_cldliq(:ncol,:pver) = (state%q(:ncol,:pver,ixcldliq) - tmp_cldliq(:ncol,:pver))*rtdt
      else
        tmp_cldliq(:ncol,:pver) = 0.0_r8
      end if
      if (ixcldice > 0) then
        tmp_cldice(:ncol,:pver) = (state%q(:ncol,:pver,ixcldice) - tmp_cldice(:ncol,:pver))*rtdt
      else
        tmp_cldice(:ncol,:pver) = 0.0_r8
      end if
      if ( cnst_cam_outfld(       1) ) then
        call outfld (dmetendnam(       1), tmp_q     , pcols, lchnk)
      end if
      if (ixcldliq > 0) then
        if ( cnst_cam_outfld(ixcldliq) ) then
          call outfld (dmetendnam(ixcldliq), tmp_cldliq, pcols, lchnk)
        end if
      end if
      if (ixcldice > 0) then
        if ( cnst_cam_outfld(ixcldice) ) then
          call outfld (dmetendnam(ixcldice), tmp_cldice, pcols, lchnk)
        end if
      end if
!AL
      tmp_cldnc(:ncol,:pver) = (state%q(:ncol,:pver,ixnumliq) - tmp_cldnc(:ncol,:pver))*rtdt
      tmp_cldni(:ncol,:pver) = (state%q(:ncol,:pver,ixnumice) - tmp_cldni(:ncol,:pver))*rtdt
      if ( cnst_cam_outfld(ixnumliq) ) call outfld (dmetendnam(ixnumliq), tmp_cldnc, pcols, lchnk)
      if ( cnst_cam_outfld(ixnumice) ) call outfld (dmetendnam(ixnumice), tmp_cldni, pcols, lchnk)
!AL
   end if

    ! Total physics tendency for moisture and other tracers

    if ( cnst_cam_outfld(       1) ) then
      ftem3(:ncol,:pver) = (state%q(:ncol,:pver,       1) - qini     (:ncol,:pver) )*rtdt
      call outfld (ptendnam(       1), ftem3, pcols, lchnk)
    end if
    if (ixcldliq > 0) then
      if (cnst_cam_outfld(ixcldliq) ) then
        ftem3(:ncol,:pver) = (state%q(:ncol,:pver,ixcldliq) - cldliqini(:ncol,:pver) )*rtdt
        call outfld (ptendnam(ixcldliq), ftem3, pcols, lchnk)
      end if
    end if
    if (ixcldice > 0) then
      if ( cnst_cam_outfld(ixcldice) ) then
        ftem3(:ncol,:pver) = (state%q(:ncol,:pver,ixcldice) - cldiceini(:ncol,:pver) )*rtdt
        call outfld (ptendnam(ixcldice), ftem3, pcols, lchnk)
      end if
    end if
!AL
    if ( cnst_cam_outfld(ixnumliq) ) then
      ftem3(:ncol,:pver) = (state%q(:ncol,:pver,ixnumliq) - cldncini(:ncol,:pver) )*rtdt
      call outfld (ptendnam(ixnumliq), ftem3, pcols, lchnk)
    end if
    if ( cnst_cam_outfld(ixnumice) ) then
      ftem3(:ncol,:pver) = (state%q(:ncol,:pver,ixnumice) - cldniini(:ncol,:pver) )*rtdt
      call outfld (ptendnam(ixnumice), ftem3, pcols, lchnk)
    end if

!AL

  end subroutine diag_phys_tend_writeout_moist

!#######################################################################

!AL
!  subroutine diag_phys_tend_writeout(state, pbuf,  tend, ztodt,               &
!       tmp_q, tmp_cldliq, tmp_cldice, qini, cldliqini, cldiceini)
!AL
  subroutine diag_phys_tend_writeout(state, pbuf,  tend, ztodt              &
       , tmp_q, tmp_t, tmp_cldliq, tmp_cldice, tmp_cldnc,tmp_cldni                 &
       , qini, cldliqini, cldiceini,cldncini, cldniini, eflx, dsema)
    !---------------------------------------------------------------
    !
    ! Purpose:  Dump physics tendencies for moisture and temperature
    !
    !---------------------------------------------------------------

    ! Arguments

    type(physics_state), intent(in)    :: state

    type(physics_buffer_desc), pointer :: pbuf(:)
    type(physics_tend ), intent(in)    :: tend
    real(r8),            intent(in)    :: ztodt                  ! physics timestep
    real(r8)           , intent(inout) :: tmp_q     (pcols,pver) ! As input, holds pre-adjusted tracers (FV)
    real(r8)           , intent(inout) :: tmp_t     (pcols,pver) !tht: holds last physics_updated T (FV)
    real(r8),            intent(inout) :: tmp_cldliq(pcols,pver) ! As input, holds pre-adjusted tracers (FV)
    real(r8),            intent(inout) :: tmp_cldice(pcols,pver) ! As input, holds pre-adjusted tracers (FV)
    real(r8),            intent(in)    :: qini      (pcols,pver) ! tracer fields at beginning of physics
    real(r8),            intent(in)    :: cldliqini (pcols,pver) ! tracer fields at beginning of physics
    real(r8),            intent(in)    :: cldiceini (pcols,pver) ! tracer fields at beginning of physics
!AL
   real(r8)           , intent(inout)    :: tmp_cldnc(pcols,pver) ! As input, holds pre-adjusted tracers (FV)
   real(r8)           , intent(inout)    :: tmp_cldni(pcols,pver) ! As input, holds pre-adjusted tracers (FV)
   real(r8)           , intent(in   ) :: cldncini (pcols,pver) ! tracer fields at beginning of physics
   real(r8)           , intent(in   ) :: cldniini (pcols,pver) ! tracer fields at beginning of physics
!AL
    real(r8)           , intent(in), optional ::eflx (pcols    ) !tht: surface sensible heat flux assoc.with mass adj.
    real(r8)           , intent(in), optional ::dsema(pcols    ) !tht: column enthalpy tendency assoc. with mass adj.

    !-----------------------------------------------------------------------

   !call diag_phys_tend_writeout_dry(state, pbuf, tend, ztodt)
    call diag_phys_tend_writeout_dry(state, pbuf, tend, ztodt, tmp_t, eflx, dsema) !tht
    if (moist_physics) then
      call diag_phys_tend_writeout_moist(state, pbuf,  tend, ztodt,           &
           tmp_q, tmp_cldliq, tmp_cldice, tmp_cldnc, tmp_cldni                &
           ,qini, cldliqini, cldiceini , cldncini, cldniini)
    end if

  end subroutine diag_phys_tend_writeout

!#######################################################################

  subroutine diag_state_b4_phys_write_dry (state)
    !
    !---------------------------------------------------------------
    !
    ! Purpose:  Dump dry state just prior to executing physics
    !
    !---------------------------------------------------------------
    !
    ! Arguments
    !
    type(physics_state), intent(in) :: state
    !
    !---------------------------Local workspace-----------------------------
    !
    integer :: lchnk              ! chunk index
    !
    !-----------------------------------------------------------------------
    !
    lchnk = state%lchnk

    call outfld('TBP', state%t, pcols, lchnk   )

  end subroutine diag_state_b4_phys_write_dry

  subroutine diag_state_b4_phys_write_moist (state)
    !
    !---------------------------------------------------------------
    !
    ! Purpose:  Dump moist state just prior to executing physics
    !
    !---------------------------------------------------------------
    !
    ! Arguments
    !
    type(physics_state), intent(in) :: state
    !
    !---------------------------Local workspace-----------------------------
    !
    integer :: ixcldice, ixcldliq ! constituent indices for cloud liquid and ice water.
    integer :: lchnk              ! chunk index
    !
    !-----------------------------------------------------------------------
    !
    lchnk = state%lchnk

    call cnst_get_ind('CLDLIQ', ixcldliq, abort=.false.)
    call cnst_get_ind('CLDICE', ixcldice, abort=.false.)

    if ( cnst_cam_outfld(       1) ) then
      call outfld (bpcnst(       1), state%q(1,1,       1), pcols, lchnk)
    end if
    if (ixcldliq > 0) then
      if (cnst_cam_outfld(ixcldliq)) then
        call outfld (bpcnst(ixcldliq), state%q(1,1,ixcldliq), pcols, lchnk)
      end if
    end if
    if (ixcldice > 0) then
      if (cnst_cam_outfld(ixcldice)) then
        call outfld (bpcnst(ixcldice), state%q(1,1,ixcldice), pcols, lchnk)
      end if
    end if

  end subroutine diag_state_b4_phys_write_moist

  subroutine diag_state_b4_phys_write (state)
    !
    !---------------------------------------------------------------
    !
    ! Purpose:  Dump state just prior to executing physics
    !
    !---------------------------------------------------------------
    !
    ! Arguments
    !
    type(physics_state), intent(in) :: state
    !

    call diag_state_b4_phys_write_dry(state)
    if (moist_physics) then
      call diag_state_b4_phys_write_moist(state)
    end if
  end subroutine diag_state_b4_phys_write

end module cam_diagnostics
