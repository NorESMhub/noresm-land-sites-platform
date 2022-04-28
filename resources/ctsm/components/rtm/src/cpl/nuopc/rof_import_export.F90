module rof_import_export

  ! This module provides the interface layer to transfer ESMF import/export data to the
  ! RTM data structures.

  use ESMF            , only : ESMF_GridComp, ESMF_State, ESMF_Mesh, ESMF_StateGet
  use ESMF            , only : ESMF_KIND_R8, ESMF_SUCCESS, ESMF_MAXSTR, ESMF_LOGMSG_INFO
  use ESMF            , only : ESMF_LogWrite, ESMF_LOGMSG_ERROR, ESMF_LogFoundError
  use ESMF            , only : ESMF_STATEITEM_NOTFOUND, ESMF_StateItem_Flag
  use ESMF            , only : operator(/=), operator(==)
  use NUOPC           , only : NUOPC_CompAttributeGet, NUOPC_Advertise, NUOPC_IsConnected
  use NUOPC_Model     , only : NUOPC_ModelGet
  use shr_kind_mod    , only : r8 => shr_kind_r8
  use shr_sys_mod     , only : shr_sys_abort
  use nuopc_shr_methods , only : chkerr
  use RunoffMod       , only : runoff
  use RtmVar          , only : iulog, nt_rtm, rtm_tracers, rtmlon, rtmlat
  use RtmSpmd         , only : masterproc
  use RtmTimeManager  , only : get_nstep

  implicit none
  private ! except

  public  :: advertise_fields
  public  :: realize_fields
  public  :: import_fields
  public  :: export_fields

  private :: fldlist_add
  private :: fldlist_realize
  private :: state_getimport
  private :: state_setexport
  private :: state_getfldptr
  private :: check_for_nans

  type fld_list_type
     character(len=128) :: stdname
  end type fld_list_type

  integer, parameter     :: fldsMax = 100
  integer                :: fldsToRof_num = 0
  integer                :: fldsFrRof_num = 0
  type (fld_list_type)   :: fldsToRof(fldsMax)
  type (fld_list_type)   :: fldsFrRof(fldsMax)

  integer     ,parameter :: debug = 0  ! internal debug level
  integer     ,parameter :: nmax  = 48 ! number of time steps to write debug output
  character(*),parameter :: F01 = "('(rof_import_export) ',a,i5,2x,3(i8,2x),d21.9)"
  character(*),parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine advertise_fields(gcomp, flds_scalar_name, rc)

    ! input/output variables
    type(ESMF_GridComp)            :: gcomp
    character(len=*) , intent(in)  :: flds_scalar_name
    integer          , intent(out) :: rc

    ! local variables
    type(ESMF_State)       :: importState
    type(ESMF_State)       :: exportState
    character(ESMF_MAXSTR) :: stdname
    character(ESMF_MAXSTR) :: cvalue
    integer                :: n, num
    character(len=128)     :: fldname
    character(len=*), parameter :: subname='(rof_import_export:advertise_fields)'
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------
    ! Advertise export fields
    !--------------------------------

    call fldlist_add(fldsFrRof_num, fldsFrRof, trim(flds_scalar_name))
    call fldlist_add(fldsFrRof_num, fldsFrRof, 'Forr_rofl')
    call fldlist_add(fldsFrRof_num, fldsFrRof, 'Forr_rofi')
    call fldlist_add(fldsFrRof_num, fldsFrRof, 'Flrr_flood')
    call fldlist_add(fldsFrRof_num, fldsFrRof, 'Flrr_volr')
    call fldlist_add(fldsFrRof_num, fldsFrRof, 'Flrr_volrmch')

    do n = 1,fldsFrRof_num
       call NUOPC_Advertise(exportState, standardName=fldsFrRof(n)%stdname, &
            TransferOfferGeomObject='will provide', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    enddo

    !--------------------------------
    ! Advertise import fields
    !--------------------------------

    call fldlist_add(fldsToRof_num, fldsToRof, trim(flds_scalar_name))

    call fldlist_add(fldsToRof_num, fldsToRof, 'Flrl_rofsur')
    call fldlist_add(fldsToRof_num, fldsToRof, 'Flrl_rofgwl')
    call fldlist_add(fldsToRof_num, fldsToRof, 'Flrl_rofsub')
    call fldlist_add(fldsToRof_num, fldsToRof, 'Flrl_rofi')
    call fldlist_add(fldsToRof_num, fldsToRof, 'Flrl_irrig')

    do n = 1,fldsToRof_num
       call NUOPC_Advertise(importState, standardName=fldsToRof(n)%stdname, &
            TransferOfferGeomObject='will provide', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    enddo

  end subroutine advertise_fields

!===============================================================================

  subroutine realize_fields(gcomp, Emesh, flds_scalar_name, flds_scalar_num, rc)

    ! input/output variables
    type(ESMF_GridComp) , intent(inout) :: gcomp
    type(ESMF_Mesh)     , intent(in)    :: Emesh
    character(len=*)    , intent(in)    :: flds_scalar_name
    integer             , intent(in)    :: flds_scalar_num
    integer             , intent(out)   :: rc

    ! local variables
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    character(len=*), parameter :: subname='(rof_import_export:realize_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call fldlist_realize( &
         state=ExportState, &
         fldList=fldsFrRof, &
         numflds=fldsFrRof_num, &
         flds_scalar_name=flds_scalar_name, &
         flds_scalar_num=flds_scalar_num, &
         tag=subname//':RTMExport',&
         mesh=Emesh, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call fldlist_realize( &
         state=importState, &
         fldList=fldsToRof, &
         numflds=fldsToRof_num, &
         flds_scalar_name=flds_scalar_name, &
         flds_scalar_num=flds_scalar_num, &
         tag=subname//':RTMImport',&
         mesh=Emesh, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine realize_fields

!===============================================================================

  subroutine import_fields( gcomp, totrunin, rc )

    !---------------------------------------------------------------------------
    ! Obtain the runoff input from the mediator and convert from kg/m2s to m3/s
    !---------------------------------------------------------------------------

    ! input/output variables
    type(ESMF_GridComp)   :: gcomp
    real(r8), intent(out) :: totrunin( runoff%begr: ,: )
    integer , intent(out) :: rc

    ! Local variables
    type(ESMF_State)  :: importState
    integer           :: n,nt,ix,iy
    integer           :: begr, endr
    integer           :: nliq, nfrz
    real(r8), pointer :: temp(:,:)
    character(len=*), parameter :: subname='(rof_import_export:import_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! Get import state
    call NUOPC_ModelGet(gcomp, importState=importState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Set tracers
    nliq = 0
    nfrz = 0
    do nt = 1,nt_rtm
       if (trim(rtm_tracers(nt)) == 'LIQ') nliq = nt
       if (trim(rtm_tracers(nt)) == 'ICE') nfrz = nt
    enddo
    if (nliq == 0 .or. nfrz == 0) then
       write(iulog,*) trim(subname),': ERROR in rtm_tracers LIQ ICE ',nliq,nfrz,rtm_tracers
       call shr_sys_abort()
    endif

    begr = runoff%begr
    endr = runoff%endr

    ! determine output array and scale by unit convertsion
    ! NOTE: the call to state_getimport will convert from input kg/m2s to m3/s

    allocate(temp(begr:endr,3))

    call state_getimport(importState, 'Flrl_rofsur', begr, endr, output=temp(:,1), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call state_getimport(importState, 'Flrl_rofsub', begr, endr, output=temp(:,2), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call state_getimport(importState, 'Flrl_rofgwl', begr, endr, output=temp(:,3), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call state_getimport(importState, 'Flrl_irrig', begr, endr, output=runoff%qirrig, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = begr,endr
       totrunin(n,nliq) = temp(n,1) + temp(n,2) + temp(n,3) + runoff%qirrig(n)
    enddo

    call state_getimport(importState, 'Flrl_rofi', begr, endr, output=totrunin(:,nfrz), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (debug > 0 .and. masterproc .and. get_nstep() < nmax) then
       do n = begr,endr
          iy = (n-1)/rtmlon + 1
          ix = n - (iy-1)*rtmlon
          write(iulog,F01)'import: nstep, n, ix, iy, Flrl_rofsur   = ',get_nstep(),n,ix,iy,temp(n,1)
          write(iulog,F01)'import: nstep, n, ix, iy, Flrl_rofsub   = ',get_nstep(),n,ix,iy,temp(n,2)
          write(iulog,F01)'import: nstep, n, ix, iy, Flrl_rofgwl   = ',get_nstep(),n,ix,iy,temp(n,3)
          write(iulog,F01)'import: nstep, n, ix, iy, qirrig        = ',get_nstep(),n,ix,iy,runoff%qirrig(n)
          write(iulog,F01)'import: nstep, n, ix, iy, totrunin(liq) = ',get_nstep(),n,ix,iy,totrunin(n,nliq)
          write(iulog,F01)'import: nstep, n, ix, iy, totrunin(frz) = ',get_nstep(),n,ix,iy,totrunin(n,nfrz)
       end do
    end if
    deallocate(temp)

  end subroutine import_fields

  !====================================================================================

  subroutine export_fields (gcomp, rc)

    !---------------------------------------------------------------------------
    ! Send the runoff model export state to the mediator and convert from m3/s to kg/m2s
    !---------------------------------------------------------------------------

    ! uses
    use RtmVar, only : ice_runoff

    ! input/output/variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_State)  :: exportState
    integer           :: n,nt,ix,iy
    integer           :: begr,endr
    integer           :: nliq, nfrz
    real(r8), pointer :: rofl(:)
    real(r8), pointer :: rofi(:)
    real(r8), pointer :: flood(:)
    real(r8), pointer :: volr(:)
    real(r8), pointer :: volrmch(:)
    logical, save     :: first_time = .true.
    integer           :: dbrc
    character(len=*), parameter :: subname='(rof_import_export:export_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! Get export state
    call NUOPC_ModelGet(gcomp, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Set tracers
    nliq = 0
    nfrz = 0
    do nt = 1,nt_rtm
       if (trim(rtm_tracers(nt)) == 'LIQ') nliq = nt
       if (trim(rtm_tracers(nt)) == 'ICE') nfrz = nt
    enddo
    if (nliq == 0 .or. nfrz == 0) then
       write(iulog,*) trim(subname),': ERROR in rtm_tracers LIQ ICE ',nliq,nfrz,rtm_tracers
       call shr_sys_abort()
    endif

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

    begr = runoff%begr
    endr = runoff%endr

    allocate(rofl(begr:endr))    ; rofl(:)  = 0._r8
    allocate(rofi(begr:endr))    ; rofi(:)  = 0._r8
    allocate(flood(begr:endr))   ; flood(:) = 0._r8
    allocate(volr(begr:endr))    ; volr(:)  = 0._r8
    allocate(volrmch(begr:endr)) ; volrmch  = 0._r8

    if ( ice_runoff )then
       ! separate liquid and ice runoff
       do n = begr,endr
          if (runoff%mask(n) >= 2) then
             ! liquid and ice runoff are treated separately - this is what goes to the ocean
             rofl(n) = runoff%runoff(n,nliq)/(runoff%area(n)*1.0e-6_r8*1000._r8)
             rofi(n) = runoff%runoff(n,nfrz)/(runoff%area(n)*1.0e-6_r8*1000._r8)
          end if
       end do
    else
       do n = begr,endr
          if (runoff%mask(n) == 2) then
             ! liquid and ice runoff are bundled together to liquid runoff
             ! and then ice runoff set to zero
             rofl(n) = (runoff%runoff(n,nfrz)+runoff%runoff(n,nliq)) / (runoff%area(n)*1.0e-6_r8*1000._r8)
             rofi(n) = 0._r8
          end if
       end do
    end if
    call state_setexport(exportState, 'Forr_rofl', begr, endr, input=rofl, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_setexport(exportState, 'Forr_rofi', begr, endr, input=rofi, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Flooding back to land, sign convention is positive in land->rof direction
    ! so if water is sent from rof to land, the flux must be negative.

    do n = begr, endr
       flood(n) = -runoff%flood(n) / runoff%area(n)
    end do
    call state_setexport(exportState, 'Flrr_flood', begr, endr, input=flood, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = begr, endr
       volr(n)    =  runoff%volr(n,nliq)/ runoff%area(n)
       volrmch(n) =  volr(n) ! main channel not defined in rtm so use total
    end do
    call state_setexport(exportState, 'Flrr_volr', begr, endr, input=volr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_setexport(exportState, 'Flrr_volrmch', begr, endr, input=volrmch, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (debug > 0 .and. masterproc .and. get_nstep() <  nmax) then
       do n = begr,endr
          iy = (n-1)/rtmlon + 1
          ix = n - (iy-1)*rtmlon
          write(iulog,F01)'export: nstep, n, ix, iy, Flrr_flood   = ',get_nstep(), n, ix, iy, flood(n)
          write(iulog,F01)'export: nstep, n, ix, iy, Flrr_volr    = ',get_nstep(), n, ix, iy, volr(n)
          write(iulog,F01)'export: nstep, n, ix, iy, Flrr_volrmch = ',get_nstep(), n, ix, iy, volrmch(n)
          write(iulog,F01)'export: nstep, n, ix, iy, Forr_rofl    = ',get_nstep() ,n, ix, iy, rofl(n)
          write(iulog,F01)'export: nstep, n, ix, iy, Forr_rofi    = ',get_nstep() ,n, ix, iy, rofi(n)
       end do
    end if

    deallocate(rofl, rofi, flood, volr, volrmch)

  end subroutine export_fields

  !===============================================================================

  subroutine fldlist_add(num, fldlist, stdname)
    integer,                    intent(inout) :: num
    type(fld_list_type),        intent(inout) :: fldlist(:)
    character(len=*),           intent(in)    :: stdname

    ! local variables
    integer :: rc
    character(len=*), parameter :: subname='(rof_import_export:fldlist_add)'
    !-------------------------------------------------------------------------------

    ! Set up a list of field information

    num = num + 1
    if (num > fldsMax) then
       call ESMF_LogWrite(trim(subname)//": ERROR num > fldsMax "//trim(stdname), &
            ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
       return
    endif
    fldlist(num)%stdname = trim(stdname)

  end subroutine fldlist_add

  !===============================================================================

  subroutine fldlist_realize(state, fldList, numflds, flds_scalar_name, flds_scalar_num, mesh, tag, rc)

    use NUOPC , only : NUOPC_IsConnected, NUOPC_Realize
    use ESMF  , only : ESMF_MeshLoc_Element, ESMF_FieldCreate, ESMF_TYPEKIND_R8
    use ESMF  , only : ESMF_MAXSTR, ESMF_Field, ESMF_State, ESMF_Mesh, ESMF_StateRemove
    use ESMF  , only : ESMF_LogFoundError, ESMF_LOGMSG_INFO, ESMF_SUCCESS
    use ESMF  , only : ESMF_LogWrite, ESMF_LOGMSG_ERROR, ESMF_LOGERR_PASSTHRU

    type(ESMF_State)    , intent(inout) :: state
    type(fld_list_type) , intent(in)    :: fldList(:)
    integer             , intent(in)    :: numflds
    character(len=*)    , intent(in)    :: flds_scalar_name
    integer             , intent(in)    :: flds_scalar_num
    character(len=*)    , intent(in)    :: tag
    type(ESMF_Mesh)     , intent(in)    :: mesh
    integer             , intent(inout) :: rc

    ! local variables
    integer                :: n
    type(ESMF_Field)       :: field
    character(len=80)      :: stdname
    character(len=*),parameter  :: subname='(rof_import_export:fldlist_realize)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    do n = 1, numflds
       stdname = fldList(n)%stdname
       if (NUOPC_IsConnected(state, fieldName=stdname)) then
          if (stdname == trim(flds_scalar_name)) then
             call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(stdname)//" is connected on root pe", &
                  ESMF_LOGMSG_INFO)
             ! Create the scalar field
             call SetScalarField(field, flds_scalar_name, flds_scalar_num, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          else
             call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(stdname)//" is connected using mesh", &
                  ESMF_LOGMSG_INFO)
             ! Create the field
             field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name=stdname, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          endif

          ! NOW call NUOPC_Realize
          call NUOPC_Realize(state, field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
       else
          if (stdname /= trim(flds_scalar_name)) then
             call ESMF_LogWrite(subname // trim(tag) // " Field = "// trim(stdname) // " is not connected.", &
                  ESMF_LOGMSG_INFO)
             call ESMF_StateRemove(state, (/stdname/), rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          end if
       end if
    end do

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine SetScalarField(field, flds_scalar_name, flds_scalar_num, rc)
      ! ----------------------------------------------
      ! create a field with scalar data on the root pe
      ! ----------------------------------------------
      use ESMF, only : ESMF_Field, ESMF_DistGrid, ESMF_Grid
      use ESMF, only : ESMF_DistGridCreate, ESMF_GridCreate, ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU
      use ESMF, only : ESMF_FieldCreate, ESMF_GridCreate, ESMF_TYPEKIND_R8

      type(ESMF_Field) , intent(inout) :: field
      character(len=*) , intent(in)    :: flds_scalar_name
      integer          , intent(in)    :: flds_scalar_num
      integer          , intent(inout) :: rc

      ! local variables
      type(ESMF_Distgrid) :: distgrid
      type(ESMF_Grid)     :: grid
      character(len=*), parameter :: subname='(rof_import_export:SetScalarField)'
      ! ----------------------------------------------

      rc = ESMF_SUCCESS

      ! create a DistGrid with a single index space element, which gets mapped onto DE 0.
      distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

      grid = ESMF_GridCreate(distgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

      field = ESMF_FieldCreate(name=trim(flds_scalar_name), grid=grid, typekind=ESMF_TYPEKIND_R8, &
           ungriddedLBound=(/1/), ungriddedUBound=(/flds_scalar_num/), gridToFieldMap=(/2/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    end subroutine SetScalarField

  end subroutine fldlist_realize

  !===============================================================================

  subroutine state_getimport(state, fldname, begr, endr, output, do_sum, rc)

    ! ----------------------------------------------
    ! Map import state field to output array
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_State)    , intent(in)    :: state
    character(len=*)    , intent(in)    :: fldname
    integer             , intent(in)    :: begr
    integer             , intent(in)    :: endr
    real(r8)            , intent(out)   :: output(begr:endr)
    logical, optional   , intent(in)    :: do_sum
    integer             , intent(out)   :: rc

    ! local variables
    integer                     :: g, i
    real(R8), pointer           :: fldptr(:)
    type(ESMF_StateItem_Flag)   :: itemFlag
    character(len=*), parameter :: subname='(rof_import_export:state_getimport)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    ! Determine if field with name fldname exists in state
    call ESMF_StateGet(state, trim(fldname), itemFlag, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! if field exists then create output array - else do nothing
    if (itemflag /= ESMF_STATEITEM_NOTFOUND) then

       ! get field pointer
       call state_getfldptr(state, trim(fldname), fldptr,  rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! determine output array
       do g = begr,endr
          if (present(do_sum)) then
             output(g) = output(g) + fldptr(g-begr+1)
          else
             output(g) = fldptr(g-begr+1)
          end if
       end do

       ! check for nans
       call check_for_nans(fldptr, trim(fldname), begr)
    end if

  end subroutine state_getimport

  !===============================================================================

  subroutine state_setexport(state, fldname, begr, endr, input, rc)

    use shr_const_mod, only : fillvalue=>SHR_CONST_SPVAL

    ! ----------------------------------------------
    ! Map input array to export state field
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_State)    , intent(inout) :: state
    character(len=*)    , intent(in)    :: fldname
    integer             , intent(in)    :: begr
    integer             , intent(in)    :: endr
    real(r8)            , intent(in)    :: input(begr:endr)
    integer             , intent(out)   :: rc

    ! local variables
    integer                     :: g, i
    real(R8), pointer           :: fldptr(:)
    type(ESMF_StateItem_Flag)   :: itemFlag
    character(len=*), parameter :: subname='(rof_import_export:state_setexport)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    ! Determine if field with name fldname exists in state
    call ESMF_StateGet(state, trim(fldname), itemFlag, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! if field exists then create output array - else do nothing
    if (itemflag /= ESMF_STATEITEM_NOTFOUND) then

       ! get field pointer
       call state_getfldptr(state, trim(fldname), fldptr, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       fldptr(:) = 0._r8

       ! set fldptr values to input array
       do g = begr,endr
          fldptr(g-begr+1) = input(g)
       end do

       ! check for nans
       call check_for_nans(fldptr, trim(fldname), begr)
    end if

  end subroutine state_setexport

  !===============================================================================

  subroutine state_getfldptr(State, fldname, fldptr, rc)
    ! ----------------------------------------------
    ! Get pointer to a state field
    ! ----------------------------------------------
    use ESMF , only : ESMF_State, ESMF_Field, ESMF_Mesh, ESMF_FieldStatus_Flag
    use ESMF , only : ESMF_StateGet, ESMF_FieldGet, ESMF_MeshGet
    use ESMF , only : ESMF_FIELDSTATUS_COMPLETE, ESMF_FAILURE

    type(ESMF_State),  intent(in)    :: State
    character(len=*),  intent(in)    :: fldname
    real(R8), pointer, intent(out)   :: fldptr(:)
    integer,           intent(out)   :: rc

    ! local variables
    type(ESMF_FieldStatus_Flag) :: status
    type(ESMF_Field)            :: lfield
    type(ESMF_Mesh)             :: lmesh
    integer                     :: dbrc
    integer                     :: nnodes, nelements
    character(len=*), parameter :: subname='(rof_import_export:state_getfldptr)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_StateGet(State, itemName=trim(fldname), field=lfield, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldGet(lfield, status=status, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
       call ESMF_LogWrite(trim(subname)//": ERROR data not allocated ", ESMF_LOGMSG_INFO, rc=rc)
       rc = ESMF_FAILURE
       return
    else
       call ESMF_FieldGet(lfield, mesh=lmesh, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       call ESMF_MeshGet(lmesh, numOwnedNodes=nnodes, numOwnedElements=nelements, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       if (nnodes == 0 .and. nelements == 0) then
          call ESMF_LogWrite(trim(subname)//": no local nodes or elements ", ESMF_LOGMSG_INFO, rc=dbrc)
          rc = ESMF_FAILURE
          return
       end if

       call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif  ! status

    call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine state_getfldptr

  !===============================================================================

  subroutine check_for_nans(array, fname, begg)

    ! uses
    use shr_infnan_mod, only : isnan => shr_infnan_isnan

    ! input/output variables
    real(r8), pointer             :: array(:)
    character(len=*) , intent(in) :: fname
    integer          , intent(in) :: begg

    ! local variables
    integer :: i
    !-------------------------------------------------------------------------------

    ! Check if any input from mediator or output to mediator is NaN

    if (any(isnan(array))) then
       write(iulog,*) '# of NaNs = ', count(isnan(array))
       write(iulog,*) 'Which are NaNs = ', isnan(array)
       do i = 1, size(array)
          if (isnan(array(i))) then
             write(iulog,*) "NaN found in field ", trim(fname), ' at gridcell index ',begg+i-1
          end if
       end do
       call shr_sys_abort(' ERROR: One or more of the output from RTM to the coupler are NaN ' )
    end if
  end subroutine check_for_nans

end module rof_import_export
