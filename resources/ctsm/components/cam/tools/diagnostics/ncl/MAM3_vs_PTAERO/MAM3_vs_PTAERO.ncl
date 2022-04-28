  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

MWH2SO4 = 98.0
MWNH4HSO4 = 114.0
MWScale = MWNH4HSO4/MWH2SO4
 

if (.not. isvar("format")) then        ; is format on command line?
  format = "pdf"
;  format = "png"
;  format = "pdf"
 end if

if (.not. isvar("var"))then
   var="undef"
end if

;filename_PT="/disk1/alfg/noresmrun/PTAER/PTAERO1_1YR_LR/out_YA.nc"
;filename_MAM="/disk1/alfg/noresmrun/PTAER/MAM3_1YR_LR/out_MAM_YA.nc"
; *************************************************************************
;filename_PT="/disk1/alfg/noresmrun/PTAER/PTAERO1_1YR_HR/PTAERO_10Mnth.nc"
;filename_MAM="/disk1/alfg/noresmrun/PTAER/MAM3_1YR_HR/MAM3_10Mnth.nc"
; No changes by the user should be necessary below...
filename_PT="/disk1/alfg/noresmrun/PTAER/PTAERO1_1YR_HR/BF1NudgePD2000.cam.h0.1979_AVG.nc"
filename_MAM="/disk1/alfg/noresmrun/PTAER/MAM3_1YR_HR/MAM3_HR_NUDGE2_1979_AVG.nc"
; *************************************************************************
f_PT     = addfile (filename_PT, "r")
f_MAM     = addfile (filename_MAM, "r")
varname=""
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BURDENS
plotType=1
if(var .eq. "BC")then
   varname_PT  = "BC burden (clean), PTAERO"
   varname_MAM = "BC burden (clean), MAM3"
   PT_AERO  = f_PT->cb_BC_NI ;
   PT_AERO  = PT_AERO + f_PT->cb_BC_AI
   PT_AERO  = PT_AERO + f_PT->cb_BC_A
   PT_AERO  = PT_AERO + f_PT->cb_BC_AC
   PT_AERO  = PT_AERO + f_PT->cb_BC_N
   PT_AERO  = PT_AERO + f_PT->cb_BC_AX

   MAM_AERO = f_MAM->BURDENBC
else if (var .eq. "DUST") then
   varname_PT  = "Dust burden (clean), PTAERO"
   varname_MAM = "Dust burden (clean), MAM3"
   PT_AERO  = f_PT->cb_DST_A2 ;
   PT_AERO  = PT_AERO + f_PT->cb_DST_A3

   MAM_AERO = f_MAM->BURDENDUST
else if (var .eq. "OM")then
   varname_PT  = "Organics burden (clean), PTAERO"
   varname_MAM = "Organics burden (clean), MAM3"
   PT_AERO  = f_PT->cb_OM_AC ;
   PT_AERO  = PT_AERO + f_PT->cb_OM_NI
   PT_AERO  = PT_AERO + f_PT->cb_OM_AI

   MAM_AERO = f_MAM->BURDENPOM
   MAM_AERO = MAM_AERO + f_MAM->BURDENSOA
else if (var .eq. "SS")then
   varname_PT  = "Sea salt burden (clean), PTAERO"
   varname_MAM = "Sea salt burden (clean), MAM3"
   PT_AERO  = f_PT->cb_SS_A1
   PT_AERO  = PT_AERO + f_PT->cb_SS_A2
   PT_AERO  = PT_AERO + f_PT->cb_SS_A3

   MAM_AERO = f_MAM->BURDENSEASALT
else if (var .eq. "SO4")then
   varname_PT  = "Sulfate burden (clean), PTAERO"
   varname_MAM = "Sulfate burden (clean), MAM3"
   PT_AERO  = f_PT->cb_SO4_A1*MWScale
   PT_AERO  = PT_AERO + f_PT->cb_SO4_A2*MWScale
   PT_AERO  = PT_AERO + f_PT->cb_SO4_PR*MWScale
   PT_AERO  = PT_AERO + f_PT->cb_SO4_N*MWScale
   PT_AERO  = PT_AERO + f_PT->cb_SO4_AC*MWScale
   PT_AERO  = PT_AERO + f_PT->cb_SO4_NA*MWScale

   copy_VarCoords(f_PT->cb_SO4_A1, PT_AERO) 

   MAM_AERO = f_MAM->BURDENSO4
end if
end if
end if
end if
end if

;Surface concentrations
if(var .eq. "BC_SRF")then
   varname_PT  = "BC surface conc (clean), PTAERO"
   varname_MAM = "BC surface conc (clean), MAM3"
   PT_AERO  = f_PT->BC_NI_SRF ;
   PT_AERO  = PT_AERO + f_PT->BC_AI_SRF
   PT_AERO  = PT_AERO + f_PT->BC_A_SRF
   PT_AERO  = PT_AERO + f_PT->BC_AC_SRF
   PT_AERO  = PT_AERO + f_PT->BC_N_SRF
   PT_AERO  = PT_AERO + f_PT->BC_AX_SRF

   MAM_AERO = f_MAM->bc_a1_SRF

else if (var .eq. "DUST_SRF") then

   varname_PT  = "Dust surface conc (clean), PTAERO"
   varname_MAM = "Dust surface conc (clean), MAM3"
   PT_AERO  = f_PT->DST_A2_SRF ;
   PT_AERO  = PT_AERO + f_PT->DST_A3_SRF

   MAM_AERO = f_MAM->dst_a1_SRF
   MAM_AERO = MAM_AERO + f_MAM->dst_a3_SRF
else if (var .eq. "OM_SRF")then
   varname_PT  = "Organics surface conc (clean), PTAERO"
   varname_MAM = "Organics surface conc (clean), MAM3"
   PT_AERO  = f_PT->OM_AC_SRF ;
   PT_AERO  = PT_AERO + f_PT->OM_NI_SRF
   PT_AERO  = PT_AERO + f_PT->OM_AI_SRF

   MAM_AERO = f_MAM->pom_a1_SRF
   MAM_AERO = MAM_AERO + f_MAM->soa_a1_SRF
   MAM_AERO = MAM_AERO + f_MAM->soa_a2_SRF
else if (var .eq. "SS_SRF")then
   varname_PT  = "Sea salt surface conc (clean), PTAERO"
   varname_MAM = "Sea salt surface conc (clean), MAM3"
   PT_AERO  = f_PT->SS_A1_SRF
   PT_AERO  = PT_AERO + f_PT->SS_A2_SRF
   PT_AERO  = PT_AERO + f_PT->SS_A3_SRF

   MAM_AERO = f_MAM->ncl_a1_SRF
   MAM_AERO = MAM_AERO + f_MAM->ncl_a2_SRF
   MAM_AERO = MAM_AERO + f_MAM->ncl_a3_SRF

else if (var .eq. "SO4_SRF")then
   varname_PT  = "Sulfate surface concentration (clean), PTAERO"
   varname_MAM = "Sulfate surface concentration (clean), MAM3"
   PT_AERO  = f_PT->SO4_A1_SRF*MWScale
   PT_AERO  = PT_AERO + f_PT->SO4_A2_SRF
   PT_AERO  = PT_AERO + f_PT->SO4_PR_SRF*MWScale
   PT_AERO  = PT_AERO + f_PT->SO4_N_SRF*MWScale
   PT_AERO  = PT_AERO + f_PT->SO4_AC_SRF*MWScale
   PT_AERO  = PT_AERO + f_PT->SO4_NA_SRF*MWScale

   ;Multiplying with scale factor messes up meta-info
   copy_VarCoords(f_PT->SO4_A1_SRF, PT_AERO) 

   MAM_AERO = f_MAM->so4_a1_SRF
   MAM_AERO = MAM_AERO + f_MAM->so4_a2_SRF
   MAM_AERO = MAM_AERO + f_MAM->so4_a3_SRF
else if (var .eq. "SO2_SRF")then
   varname_PT  = "SO2 surface concentration, PTAERO"
   varname_MAM = "SO2 surface concentration, MAM3"
   PT_AERO  = f_PT->SO2_SRF
   MAM_AERO = f_MAM->SO2_SRF
end if
end if
end if
end if
end if
end if

if (var .eq. "SF_DUST")then
   varname_PT  = "Dust emissions, PTAERO"
   varname_MAM = "Dust emissions, MAM3"
   PT_AERO  = f_PT->SFDST_A2
   PT_AERO  = PT_AERO + f_PT->SFDST_A3

   MAM_AERO = f_MAM->SFdst_a1
   MAM_AERO = MAM_AERO + f_MAM->SFdst_a3
else if (var .eq. "SF_SALT")then
   varname_PT  = "Sea salt emissions, PTAERO"
   varname_MAM = "Sea salt emissions, MAM3"
   PT_AERO  = f_PT->SFSS_A1
   PT_AERO  = PT_AERO + f_PT->SFSS_A2
   PT_AERO  = PT_AERO + f_PT->SFSS_A3


   MAM_AERO = f_MAM->SFncl_a1
   MAM_AERO = MAM_AERO + f_MAM->SFncl_a2
   MAM_AERO = MAM_AERO + f_MAM->SFncl_a3
end if
end if


;ZONAL MEANS
if(var .eq. "BC_ZM")then
   varname_PT  = "BC ZM, PTAERO"
   varname_MAM = "BC ZM, MAM3"
   PT_AERO  = f_PT->BC_NI ;
   PT_AERO  = PT_AERO + f_PT->BC_AI
   PT_AERO  = PT_AERO + f_PT->BC_A
   PT_AERO  = PT_AERO + f_PT->BC_AC
   PT_AERO  = PT_AERO + f_PT->BC_N
   PT_AERO  = PT_AERO + f_PT->BC_AX


   MAM_AERO = f_MAM->bc_a1

   plotType = 2

else if (var .eq. "DUST_ZM") then

   varname_PT  = "Dust ZM, PTAERO"
   varname_MAM = "Dust ZM, MAM3"
   PT_AERO  = f_PT->DST_A2 ;
   PT_AERO  = PT_AERO + f_PT->DST_A3

   MAM_AERO = f_MAM->dst_a1
   MAM_AERO = MAM_AERO + f_MAM->dst_a3
   plotType = 2
else if (var .eq. "OM_ZM")then
   varname_PT  = "Organics ZM, PTAERO"
   varname_MAM = "Organics ZM, MAM3"
   PT_AERO  = f_PT->OM_AC ;
   PT_AERO  = PT_AERO + f_PT->OM_NI
   PT_AERO  = PT_AERO + f_PT->OM_AI

   MAM_AERO = f_MAM->pom_a1
   MAM_AERO = MAM_AERO + f_MAM->soa_a1
   MAM_AERO = MAM_AERO + f_MAM->soa_a2
   plotType = 2
else if (var .eq. "SS_ZM")then
   varname_PT  = "Sea salt ZM, PTAERO"
   varname_MAM = "Sea salt ZM, MAM3"
   PT_AERO  = f_PT->SS_A1
   PT_AERO  = PT_AERO + f_PT->SS_A2
   PT_AERO  = PT_AERO + f_PT->SS_A3

   MAM_AERO = f_MAM->ncl_a1
   MAM_AERO = MAM_AERO + f_MAM->ncl_a2
   MAM_AERO = MAM_AERO + f_MAM->ncl_a3

   plotType = 2
else if (var .eq. "SO4_ZM")then
   varname_PT  = "Sulfate ZM, PTAERO"
   varname_MAM = "Sulfate ZM, MAM3"
   PT_AERO  = f_PT->SO4_A1*MWScale
   PT_AERO  = PT_AERO + f_PT->SO4_A2
   PT_AERO  = PT_AERO + f_PT->SO4_PR*MWScale
   PT_AERO  = PT_AERO + f_PT->SO4_N*MWScale
   PT_AERO  = PT_AERO + f_PT->SO4_AC*MWScale
   PT_AERO  = PT_AERO + f_PT->SO4_NA*MWScale

   copy_VarCoords(f_PT->SO4_A1, PT_AERO)

   MAM_AERO = f_MAM->so4_a1
   MAM_AERO = MAM_AERO + f_MAM->so4_a2
   MAM_AERO = MAM_AERO + f_MAM->so4_a3
   plotType = 2
end if
end if
end if
end if
end if

;Cloud concentrations
if(var .eq. "BC_CLOUD")then
   varname_PT  = "BC Cloud, PTAERO"
   varname_MAM = "BC cloud, MAM3"
   PT_AERO  = f_PT->BC_NI_OCW ;
   PT_AERO  = PT_AERO + f_PT->BC_AI_OCW
   PT_AERO  = PT_AERO + f_PT->BC_A_OCW
   PT_AERO  = PT_AERO + f_PT->BC_AC_OCW
   PT_AERO  = PT_AERO + f_PT->BC_N_OCW
   

   MAM_AERO = f_MAM->bc_c1

   plotType = 2

else if (var .eq. "DUST_CLOUD") then

   varname_PT  = "Dust cloud, PTAERO"
   varname_MAM = "Dust cloud, MAM3"
   PT_AERO  = f_PT->DST_A2_OCW ;
   PT_AERO  = PT_AERO + f_PT->DST_A3_OCW

   MAM_AERO = f_MAM->dst_c1
   MAM_AERO = MAM_AERO + f_MAM->dst_c3
   plotType = 2
else if (var .eq. "OM_CLOUD")then
   varname_PT  = "Organics cloud, PTAERO"
   varname_MAM = "Organics cloud, MAM3"
   PT_AERO  = f_PT->OM_AC_OCW ;
   PT_AERO  = PT_AERO + f_PT->OM_NI_OCW
   PT_AERO  = PT_AERO + f_PT->OM_AI_OCW

   MAM_AERO = f_MAM->pom_c1
   MAM_AERO = MAM_AERO + f_MAM->soa_c1
   MAM_AERO = MAM_AERO + f_MAM->soa_c2
   plotType = 2
else if (var .eq. "SS_CLOUD")then
   varname_PT  = "Sea salt cloud, PTAERO"
   varname_MAM = "Sea salt cloud, MAM3"
   PT_AERO  = f_PT->SS_A1_OCW
   PT_AERO  = PT_AERO + f_PT->SS_A2_OCW
   PT_AERO  = PT_AERO + f_PT->SS_A3_OCW

   MAM_AERO = f_MAM->ncl_c1
   MAM_AERO = MAM_AERO + f_MAM->ncl_c2
   MAM_AERO = MAM_AERO + f_MAM->ncl_c3

   plotType = 2
else if (var .eq. "SO4_CLOUD")then
   varname_PT  = "Sulfate cloud, PTAERO"
   varname_MAM = "Sulfate cloud, MAM3"
   PT_AERO  = f_PT->SO4_A1_OCW
   PT_AERO  = PT_AERO + f_PT->SO4_A2_OCW
   PT_AERO  = PT_AERO + f_PT->SO4_PR_OCW
   PT_AERO  = PT_AERO + f_PT->SO4_N_OCW
   PT_AERO  = PT_AERO + f_PT->SO4_AC_OCW
   PT_AERO  = PT_AERO + f_PT->SO4_NA_OCW

   MAM_AERO = f_MAM->so4_c1
   MAM_AERO = MAM_AERO + f_MAM->so4_c2
   MAM_AERO = MAM_AERO + f_MAM->so4_c3
   plotType = 2
end if
end if
end if
end if
end if

;Cloud concentration fractions
if(var .eq. "BC_CLFR")then
   varname_PT  = "BC CLFR, PTAERO"
   varname_MAM = "BC CLFR, MAM3"
   PT_AERO_1  = f_PT->BC_NI ;
   PT_AERO_1  = PT_AERO_1 + f_PT->BC_AI
   PT_AERO_1  = PT_AERO_1 + f_PT->BC_A
   PT_AERO_1  = PT_AERO_1 + f_PT->BC_AC
   PT_AERO_1  = PT_AERO_1 + f_PT->BC_N

   PT_AERO_2  = f_PT->BC_NI_OCW ;
   PT_AERO_2  = PT_AERO_2 + f_PT->BC_AI_OCW
   PT_AERO_2  = PT_AERO_2 + f_PT->BC_A_OCW
   PT_AERO_2  = PT_AERO_2 + f_PT->BC_AC_OCW
   PT_AERO_2  = PT_AERO_2 + f_PT->BC_N_OCW


   MAM_AERO_1 = f_MAM->bc_a1
   MAM_AERO_2 = f_MAM->bc_c1

   ;Divide the zonal means to find the fraction inside clouds!
   PT_AERO = (dim_avg_Wrap(PT_AERO_2)) /(dim_avg_Wrap(PT_AERO_1 + PT_AERO_2))
   MAM_AERO = (dim_avg_Wrap(MAM_AERO_2))/(dim_avg_Wrap(MAM_AERO_1 + MAM_AERO_2))

   copy_VarCoords(dim_avg_Wrap(PT_AERO_2), PT_AERO)
   copy_VarCoords(dim_avg_Wrap(MAM_AERO_2), MAM_AERO)

   plotType = 3
;Cloud concentration fractions
else if(var .eq. "DUST_CLFR")then
   varname_PT  = "DUST CLFR, PTAERO"
   varname_MAM = "DUST CLFR, MAM3"
   PT_AERO_1  = f_PT->DST_A2 ;
   PT_AERO_1  = PT_AERO_1 + f_PT->DST_A3

   PT_AERO_2  = f_PT->DST_A2_OCW ;
   PT_AERO_2  = PT_AERO_2 + f_PT->DST_A3_OCW

   MAM_AERO_1 = f_MAM->dst_a1
   MAM_AERO_1 = MAM_AERO_1 + f_MAM->dst_a3

   MAM_AERO_2 = f_MAM->dst_c1
   MAM_AERO_2 = MAM_AERO_2 + f_MAM->dst_c3

   ;Divide the zonal means to find the fraction inside clouds!
   PT_AERO = (dim_avg_Wrap(PT_AERO_2)) /(dim_avg_Wrap(PT_AERO_1 + PT_AERO_2))
   MAM_AERO = (dim_avg_Wrap(MAM_AERO_2))/(dim_avg_Wrap(MAM_AERO_1 + MAM_AERO_2))

   copy_VarCoords(dim_avg_Wrap(PT_AERO_2), PT_AERO)
   copy_VarCoords(dim_avg_Wrap(MAM_AERO_2), MAM_AERO)

   plotType = 3
;Cloud concentration fractions
else if(var .eq. "SS_CLFR")then
   varname_PT  = "SALT CLFR, PTAERO"
   varname_MAM = "SALT CLFR, MAM3"
   PT_AERO_1  = f_PT->SS_A1 ;
   PT_AERO_1  = PT_AERO_1 + f_PT->SS_A2
   PT_AERO_1  = PT_AERO_1 + f_PT->SS_A3

   PT_AERO_2  = f_PT->SS_A1_OCW ;
   PT_AERO_2  = PT_AERO_2 + f_PT->SS_A2_OCW
   PT_AERO_2  = PT_AERO_2 + f_PT->SS_A3_OCW

   MAM_AERO_1 = f_MAM->ncl_a1
   MAM_AERO_1 = MAM_AERO_1 + f_MAM->ncl_a2
   MAM_AERO_1 = MAM_AERO_1 + f_MAM->ncl_a3

   MAM_AERO_2 = f_MAM->ncl_c1
   MAM_AERO_2 = MAM_AERO_2 + f_MAM->ncl_c2
   MAM_AERO_2 = MAM_AERO_2 + f_MAM->ncl_c3

   ;Divide the zonal means to find the fraction inside clouds!
   PT_AERO = (dim_avg_Wrap(PT_AERO_2)) /(dim_avg_Wrap(PT_AERO_1 + PT_AERO_2))
   MAM_AERO = (dim_avg_Wrap(MAM_AERO_2))/(dim_avg_Wrap(MAM_AERO_1 + MAM_AERO_2))

   copy_VarCoords(dim_avg_Wrap(PT_AERO_2), PT_AERO)
   copy_VarCoords(dim_avg_Wrap(MAM_AERO_2), MAM_AERO)

   plotType = 3
;Cloud concentration fractions
else if(var .eq. "SO4_CLFR")then
   varname_PT  = "SO4 CLFR, PTAERO"
   varname_MAM = "SO4 CLFR, MAM3"
   PT_AERO_1  = f_PT->SO4_A1 ;
   PT_AERO_1  = PT_AERO_1 + f_PT->SO4_A2
   PT_AERO_1  = PT_AERO_1 + f_PT->SO4_NA
   PT_AERO_1  = PT_AERO_1 + f_PT->SO4_N
   PT_AERO_1  = PT_AERO_1 + f_PT->SO4_AC
   PT_AERO_1  = PT_AERO_1 + f_PT->SO4_PR

   PT_AERO_2  = f_PT->SO4_A1_OCW ;
   PT_AERO_2  = PT_AERO_2 + f_PT->SO4_A2_OCW
   PT_AERO_2  = PT_AERO_2 + f_PT->SO4_NA_OCW
   PT_AERO_2  = PT_AERO_2 + f_PT->SO4_N_OCW
   PT_AERO_2  = PT_AERO_2 + f_PT->SO4_AC_OCW
   PT_AERO_2  = PT_AERO_2 + f_PT->SO4_PR_OCW

   MAM_AERO_1 = f_MAM->so4_a1
   MAM_AERO_1 = MAM_AERO_1 + f_MAM->so4_a2
   MAM_AERO_1 = MAM_AERO_1 + f_MAM->so4_a3

   MAM_AERO_2 = f_MAM->so4_c1
   MAM_AERO_2 = MAM_AERO_2 + f_MAM->so4_c2
   MAM_AERO_2 = MAM_AERO_2 + f_MAM->so4_c3

   ;Divide the zonal means to find the fraction inside clouds!
   PT_AERO = (dim_avg_Wrap(PT_AERO_2)) /(dim_avg_Wrap(PT_AERO_1 + PT_AERO_2))
   MAM_AERO = (dim_avg_Wrap(MAM_AERO_2))/(dim_avg_Wrap(MAM_AERO_1 + MAM_AERO_2))

   copy_VarCoords(dim_avg_Wrap(PT_AERO_2), PT_AERO)
   copy_VarCoords(dim_avg_Wrap(MAM_AERO_2), MAM_AERO)

   plotType = 3
;Cloud concentration fractions
else if(var .eq. "OM_CLFR")then
   varname_PT  = "OM CLFR, PTAERO"
   varname_MAM = "OM CLFR, MAM3"
   PT_AERO_1  = f_PT->OM_NI ;
   PT_AERO_1  = PT_AERO_1 + f_PT->OM_AC
   PT_AERO_1  = PT_AERO_1 + f_PT->OM_AI

   PT_AERO_2  = f_PT->OM_NI_OCW ;
   PT_AERO_2  = PT_AERO_2 + f_PT->OM_AC_OCW
   PT_AERO_2  = PT_AERO_2 + f_PT->OM_AI_OCW

   MAM_AERO_1 = f_MAM->pom_a1
   MAM_AERO_1 = MAM_AERO_1 + f_MAM->soa_a1
   MAM_AERO_1 = MAM_AERO_1 + f_MAM->soa_a2

   MAM_AERO_2 = f_MAM->pom_c1
   MAM_AERO_2 = MAM_AERO_2 + f_MAM->soa_c1
   MAM_AERO_2 = MAM_AERO_2 + f_MAM->soa_c2

   ;Divide the zonal means to find the fraction inside clouds!
   PT_AERO = (dim_avg_Wrap(PT_AERO_2)) /(dim_avg_Wrap(PT_AERO_1 + PT_AERO_2))
   MAM_AERO = (dim_avg_Wrap(MAM_AERO_2))/(dim_avg_Wrap(MAM_AERO_1 + MAM_AERO_2))

   copy_VarCoords(dim_avg_Wrap(PT_AERO_2), PT_AERO)
   copy_VarCoords(dim_avg_Wrap(MAM_AERO_2), MAM_AERO)

   plotType = 3
end if
end if
end if
end if
end if

if(plotType .eq. 1)then
   plotArray_PT = PT_AERO
   plotArray_MAM = MAM_AERO
else if(plotType .eq. 2)then
   ;Create the zonal mean
   plotArray_PT = dim_avg_Wrap(PT_AERO)
   plotArray_MAM = dim_avg_Wrap(MAM_AERO)
else if(plotType .eq. 3)then
   ;Zonal mean already created
   plotArray_PT = PT_AERO
   plotArray_MAM = MAM_AERO
end if
end if
end if

;Set intervals for plotting
maxPT = max(plotArray_PT)
minPT = maxPT/20.0
maxMAM = max(plotArray_MAM)
minMAM = min(plotArray_MAM)

maxUsed = sqrt(maxMAM*maxPT)
minUsed = sqrt(minMAM*minPT)


maxint = 10
intervals = fspan(minUsed, maxUsed, maxint)

;Do the plotting
wks = gsn_open_wks(format,var)

plot=new(2,graphic)

res                   = True                  ; plot mods desired

res@cnFillOn          = True                  ; color fill 
res@cnLevelSelectionMode      = "ExplicitLevels"       ; manual levels
res@cnLevels          = sprintf("%3.1e",intervals)
res@gsnFrame          = False                 ; Do not draw plot
res@gsnDraw           = False                 ; Do not advance frame

if(plotType .eq. 1)then
   res@gsnLeftString   = varname_PT
   plot(0) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(plotArray_PT,0),res)    ; create the plot
   res@gsnLeftString   = varname_MAM
   plot(1) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(plotArray_MAM,0),res)   ; create the plot
else if (plotType .eq. 2 .or. plotType .eq. 3 )then
   res@trYReverse = True                            ; reverse Y axis
   res@gsnLeftString   = varname_PT
   plot(0)=gsn_csm_contour(wks,dim_avg_n_Wrap(plotArray_PT,0),res)
   res@gsnLeftString   = varname_MAM
   plot(1)=gsn_csm_contour(wks,dim_avg_n_Wrap(plotArray_MAM,0),res)
end if 
end if

pres                  = True
gsn_panel(wks,plot,(/1,2/),pres)              ; create panel plot

end
