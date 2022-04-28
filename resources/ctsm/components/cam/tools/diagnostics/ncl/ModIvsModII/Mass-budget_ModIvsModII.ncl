  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in aerosol and gas burdens and loss and production terms 
; from two versions of NorESM/CAM-Oslo and calculates global annual mass budget
; numbers, as e.g. found in Table 3 of Kirkevåg et al. (2013).

; Model independent constants
 g=9.80665
 pi=3.1415926
 re=6378.39e3            ; earth radius in m
 coffa=pi*re^2./180.      
 area1=4.*pi*re^2          
 small=1.0e-30             ; small number

; *************************************************************************
; **** To be edited by the user if the ncl script is run interactively ****
;
; Define plot type and plot output format
 if (.not. isvar("plot_type")) then      ; is plot_type on command line?
  plot_type = 5                      ;-1 => DMS mass budget
                                     ; 0 => SO2 mass budget
                                     ; 1 => SO4 mass budget
                                     ; 2 => BC  mass budget
                                     ; 3 => POM mass budget
                                     ; 4 => SS  mass budget
                                     ; 5 => DU  mass budget
 end if
;
; *************************************************************************
; No changes by the user should be necessary below...
; *************************************************************************
;old  all_files_I  = systemfunc ("ls /media/BackupAK/aerocomA2r128-tester/CTRL2000/aerocomA2r128_2006.cam2.h0.0007-*.nc")
      all_files_I  = systemfunc ("ls " + filepath_I + filenamep_I + "*")
      all_files_II = systemfunc ("ls " + filepath_II + filenamep_II + "*")
      f0_I      = addfile (filepath_I+filename_I, "r")
      f0_II     = addfile (filepath_II+filename_II, "r")
      f1_I      = addfiles (all_files_I, "r")   ; note the "s" of addfile
      f1_II     = addfiles (all_files_II, "r")   ; note the "s" of addfile

; Reading Gaussian weights and other required model variables
 gw0_I=doubletofloat(f0_I->gw)
 gw0_II=doubletofloat(f0_II->gw)

 lon_I=f0_I->lon
 dlon_I=360./dimsizes(lon_I)
 lon_II=f0_II->lon
 dlon_II=360./dimsizes(lon_II)
  
; Initialization (and obtain correct variable dimensions)
 tmp_I=f1_I[:]->PS
 tmp_II=f1_II[:]->PS         

 lifetime_I=tmp_I
 lifetime_II=tmp_II
 load_I=tmp_I
 load_II=tmp_II

; wetdepp_I=tmp_I
; wetdepp_II=tmp_II
 wet_I=tmp_I
 wet_II=tmp_II
 sink_I=tmp_I
 sink_II=tmp_II

 ;budget on dms/DMS/SO2 used by several plots
 if(ModI .eq. "CAM5-Oslo")then
   ;small dms_soa_msa budget
   tot_dms_lost_as_s_I           = -((/f1_I[:]->GS_DMS/))/1.938 ; (as s)
   terpenes_lost_as_soa_I = -1.0* ( 0.05*168/68*(/(f1_I[:]->GS_isoprene)/) + 0.15*168/136*(/(f1_I[:]->GS_monoterp)/)) ; (as SOA (> 0)) 
   soa_g_prod_as_soa_I    = ((/f1_I[:]->GS_SOA_LV + f1_I[:]->GS_SOA_SV /)) ; (as SOA)
   soa_g_prod_from_msa_I    = soa_g_prod_as_soa_I - terpenes_lost_as_soa_I ; as soa
   msa_prod_from_dms_as_s_I = soa_g_prod_from_msa_I*32/96                ; 32 ==> 96 is msa ==> s
   so2_formed_from_dms_as_s_I = tot_dms_lost_as_s_I - msa_prod_from_dms_as_s_I
   ;;
 end if
 ;small dms_soa_msa budget
 tot_dms_lost_as_s_II        = -((/f1_II[:]->GS_DMS/))/1.938 ; (as s)
 terpenes_lost_as_soa_II     = -1.0* ( 0.05*168/68*(/(f1_II[:]->GS_isoprene)/) + 0.15*168/136*(/(f1_II[:]->GS_monoterp)/)) ; (as SOA (> 0)) 
 soa_g_prod_as_soa_II        = ((/f1_II[:]->GS_SOA_LV + f1_II[:]->GS_SOA_SV /)) ; (as SOA)
 soa_g_prod_from_msa_II      = soa_g_prod_as_soa_II - terpenes_lost_as_soa_II ; as soa
 msa_prod_from_dms_as_s_II   = soa_g_prod_from_msa_II*32/96                ; 32 ==> 96 is msa ==> s
 so2_formed_from_dms_as_s_II = tot_dms_lost_as_s_II - msa_prod_from_dms_as_s_II
 ;;


 if (plot_type.eq.-1) then
 var="DMS"
 varname="DMS"
 if(ModI.eq."CAM4-Oslo") then
   emis_I=(/(f1_I[:]->EMI_DMS)/)
   sour_I=(/(f1_I[:]->EMI_DMS)/)
   load_I=(/f1_I[:]->C_DMS/)                           
   sink_I=-((/f1_I[:]->S2GA/)+(/f1_I[:]->MSAGA/))     
   chloss_I=-((/f1_I[:]->S2GA/)+(/f1_I[:]->MSAGA/))     
   chlossg_I=-(/f1_I[:]->MSAGA/)
 else
   emis_I = (/f1_I[:]->SFDMS/)*32/62; as S
   sour_I = emis_I
   sink_I = (/f1_I[:]->GS_DMS/)*32/62; as S
   load_I = (/f1_I[:]->cb_DMS/)*32/62; as S
   chloss_I = -1.0* (msa_prod_from_dms_as_s_I + so2_formed_from_dms_as_s_I)
   chlossg_I = -1.0* msa_prod_from_dms_as_s_I
 end if
 emis_II = (/f1_II[:]->SFDMS/)*32/62; as S
 sour_II = emis_II
 sink_II = (/f1_II[:]->GS_DMS/)*32/62; as S
 load_II = (/f1_II[:]->cb_DMS/)*32/62; as S
 chloss_II = -1.0*(msa_prod_from_dms_as_s_II + so2_formed_from_dms_as_s_II)
 chlossg_II = -1.0*msa_prod_from_dms_as_s_II

 else if (plot_type.eq.0) then
 var="SO2"
 varname="SO~B~2~N"
 if(ModI.eq."CAM4-Oslo") then
   emis_I=(/(f1_I[:]->EMI_SO2)/)
   sour_I=(/(f1_I[:]->EMI_SO2)/)+(/f1_I[:]->S2GA/)
   load_I=(/f1_I[:]->C_SO2/)
   wet_I=(/f1_I[:]->WET_SO2/)
   sink_I=(/f1_I[:]->WET_SO2/)+(/f1_I[:]->DRY_SO2/)-((/f1_I[:]->S4GA/)+(/f1_I[:]->S4AQ/))
   chloss_I=-((/f1_I[:]->S4GA/)+(/f1_I[:]->S4AQ/))
   chlossg_I=-((/f1_I[:]->S4GA/))
 else
   ; In CAM5 -- using mozart chemistry and buget-terms: 
   ; GS_SO2 = Prod_from_DMS + 3d_emis - wet-dep - oh-loss ==> rewrite this eqn to get the terms we need
   ; Note the SOA formed is supposed to be MSA which is factor 3 larger mole-weight than S

   if(GdepI .eq. "Neu") then
     emis_I=(/(f1_I[:]->SFSO2)/)/1.998 + (/(f1_I[:]->SO2_CMXF)/)/1.998
   else
     emis_I=(/(f1_I[:]->SFSO2)/)/1.998 + (/(f1_I[:]->SO2_CLXF)/)/1.998
   end if
   
   sour_I = emis_I + so2_formed_from_dms_as_s_I

   load_I=(/f1_I[:]->cb_SO2/)/1.998
   wet_I=-(/f1_I[:]->WD_A_SO2/)/1.998

   if(GdepI .eq. "Neu") then
 sink_I= -(/f1_I[:]->WD_A_SO2/)/1.998 \   ;kg/m2/sec (positive in output file)
          -(/f1_I[:]->DF_SO2/)/1.998 \     ;kg/m2/sec (positive in output file)  
          +(/f1_I[:]->AQ_SO2/)/1.998 \     ;kg/m2/ses (negative in output file)
          +(/f1_I[:]->GS_SO2/)/1.998 - (/f1_I[:]->SO2_CMXF/)/1.998 - so2_formed_from_dms_as_s_I  ; net chemical loss (gas phase)
 chlossg_I = (/f1_I[:]->GS_SO2/)/1.998 - (/f1_I[:]->SO2_CMXF/)/1.998 \ 
            - so2_formed_from_dms_as_s_I  ; net chemical loss (gas phase)
   else
 sink_I=-(/f1_I[:]->WD_A_SO2/)/1.998 \   ;kg/m2/sec (positive in output file)
          -(/f1_I[:]->DF_SO2/)/1.998 \     ;kg/m2/sec (positive in output file)  
          +(/f1_I[:]->AQ_SO2/)/1.998 \     ;kg/m2/ses (negative in output file)
          +(/f1_I[:]->GS_SO2/)/1.998 - (/f1_I[:]->SO2_CLXF/)/1.998 - so2_formed_from_dms_as_s_I + (/f1_I[:]->WD_A_SO2/)/1.998  ; net chemical loss (gas phase)
 chlossg_I = (/f1_I[:]->GS_SO2/)/1.998 - (/f1_I[:]->SO2_CLXF/)/1.998 \ 
            - so2_formed_from_dms_as_s_I + (/f1_I[:]->WD_A_SO2/)/1.998  ; net chemical loss (gas phase)
   end if

   chloss_I = chlossg_I + (/f1_I[:]->AQ_SO2/)/1.998                     ; net chemical loss (gas and wet-phase)

 end if

   if(GdepI .eq. "Neu") then
     emis_II=(/(f1_II[:]->SFSO2)/)/1.998 + (/(f1_II[:]->SO2_CMXF)/)/1.998
   else
     emis_II=(/(f1_II[:]->SFSO2)/)/1.998 + (/(f1_II[:]->SO2_CLXF)/)/1.998
   end if

sour_II=emis_II + so2_formed_from_dms_as_s_II

 load_II=(/f1_II[:]->cb_SO2/)/1.998
 wet_II=-(/f1_II[:]->WD_A_SO2/)/1.998
   if(GdepII .eq. "Neu") then
 sink_II= -(/f1_II[:]->WD_A_SO2/)/1.998 \   ;kg/m2/sec (positive in output file)
          -(/f1_II[:]->DF_SO2/)/1.998 \     ;kg/m2/sec (positive in output file)  
          +(/f1_II[:]->AQ_SO2/)/1.998 \     ;kg/m2/ses (negative in output file)
          +(/f1_II[:]->GS_SO2/)/1.998 - (/f1_II[:]->SO2_CMXF/)/1.998 - so2_formed_from_dms_as_s_II  ; net chemical loss (gas phase)
 chlossg_II = (/f1_II[:]->GS_SO2/)/1.998 - (/f1_II[:]->SO2_CMXF/)/1.998 \ 
            - so2_formed_from_dms_as_s_II  ; net chemical loss (gas phase)
   else
 sink_II= -(/f1_II[:]->WD_A_SO2/)/1.998 \   ;kg/m2/sec (positive in output file)
          -(/f1_II[:]->DF_SO2/)/1.998 \     ;kg/m2/sec (positive in output file)  
          +(/f1_II[:]->AQ_SO2/)/1.998 \     ;kg/m2/ses (negative in output file)
          +(/f1_II[:]->GS_SO2/)/1.998 - (/f1_II[:]->SO2_CLXF/)/1.998 - so2_formed_from_dms_as_s_II + (/f1_II[:]->WD_A_SO2/)/1.998  ; net chemical loss (gas phase)
 chlossg_II = (/f1_II[:]->GS_SO2/)/1.998 - (/f1_II[:]->SO2_CLXF/)/1.998 \ 
            - so2_formed_from_dms_as_s_II + (/f1_II[:]->WD_A_SO2/)/1.998  ; net chemical loss (gas phase)
   end if

 chloss_II = chlossg_II + (/f1_II[:]->AQ_SO2/)/1.998                     ; net chemical loss (gas and wet-phase)
 else if (plot_type.eq.1) then
 var="SO4"
 varname="SO~B~4~N~"
 if(ModI.eq."CAM4-Oslo") then
   emis_I=(/(f1_I[:]->EMI_SO4)/)
   sour_I=(/(f1_I[:]->EMI_SO4)/)+(/(f1_I[:]->S4GA)/)+(/(f1_I[:]->S4AQ)/)
   load_I=(/f1_I[:]->C_SO4/)
   wet_I=(/f1_I[:]->WET_SO4/)
   sink_I=(/f1_I[:]->WET_SO4/)+(/f1_I[:]->DRY_SO4/)
 else
   
   if(GdepI .eq. "Neu") then
     emis_I= (/(f1_I[:]->SFSO4_PR)/)/3.06 + (/(f1_I[:]->SO4_PR_CMXF)/)/3.06
   else
     emis_I= (/(f1_I[:]->SFSO4_PR)/)/3.06 + (/(f1_I[:]->SO4_PR_CLXF)/)/3.06
   end if

   if(GdepI .eq. "Neu") then
  sour_I=emis_I + (/(f1_I[:]->GS_H2SO4)/)/3.06 \ ; gas phase H2SO4 production
                  + (/f1_I[:]->AQ_SO4_A2_OCW/)/3.59 \ ; aq phase H2SO4 production
                  + (/f1_I[:]->AQ_H2SO4/)/3.06      ; some of the aq phase prod is just cond of gas phase

    else
  sour_I=emis_I + (/(f1_I[:]->GS_H2SO4)/)/3.06 + (/(f1_I[:]->WD_A_H2SO4)/)/3.06  \ ; gas phase H2SO4 production
                     + (/f1_I[:]->AQ_SO4_A2_OCW/)/3.59\ ; aq phase H2SO4 production
                     + (/f1_I[:]->AQ_H2SO4/)/3.06      ; some of the aq phase prod is just cond of gas phase
   end if


    load_I=(/(f1_I[:]->cb_SO4_A1)/)/3.06 + (/(f1_I[:]->cb_SO4_A2)/)/3.59 + (/(f1_I[:]->cb_SO4_AC)/)/3.06 \
          + (/(f1_I[:]->cb_SO4_NA)/)/3.06 + (/(f1_I[:]->cb_SO4_PR)/)/3.06  \
          + (/(f1_I[:]->cb_SO4_A1_OCW)/)/3.06 + (/(f1_I[:]->cb_SO4_A2_OCW)/)/3.59 + (/(f1_I[:]->cb_SO4_AC_OCW)/)/3.06 \
          + (/(f1_I[:]->cb_SO4_NA_OCW)/)/3.06 + (/(f1_I[:]->cb_SO4_PR_OCW)/)/3.06
    
    wet_I=(/f1_I[:]->SO4_A1SFWET/)/3.06 + (/f1_I[:]->SO4_A2SFWET/)/3.59 + (/f1_I[:]->SO4_ACSFWET/)/3.06  \
         + (/f1_I[:]->SO4_NASFWET/)/3.06 + (/f1_I[:]->SO4_PRSFWET/)/3.06   \
         + (/f1_I[:]->SO4_A1_OCWSFWET/)/3.06 + (/f1_I[:]->SO4_A2_OCWSFWET/)/3.59 + (/f1_I[:]->SO4_AC_OCWSFWET/)/3.06 \
         + (/f1_I[:]->SO4_NA_OCWSFWET/)/3.06 + (/f1_I[:]->SO4_PR_OCWSFWET/)/3.06 \ 
         + (/f1_I[:]->WD_A_H2SO4/)/3.06 
    
    dry_I=(/f1_I[:]->SO4_A1DDF/)/3.06 + (/f1_I[:]->SO4_A2DDF/)/3.59 + (/f1_I[:]->SO4_ACDDF/)/3.06  \
         + (/f1_I[:]->SO4_NADDF/)/3.06 + (/f1_I[:]->SO4_PRDDF/)/3.06   \
         + (/f1_I[:]->SO4_A1_OCWDDF/)/3.06 + (/f1_I[:]->SO4_A2_OCWDDF/)/3.59 + (/f1_I[:]->SO4_AC_OCWDDF/) /3.06 \
         + (/f1_I[:]->SO4_NA_OCWDDF/)/3.06 + (/f1_I[:]->SO4_PR_OCWDDF/)/3.06

    sink_I=wet_I-dry_I
   
 end if

   if(GdepII .eq. "Neu") then
    emis_II= (/(f1_II[:]->SFSO4_PR)/)/3.06 + (/(f1_II[:]->SO4_PR_CMXF)/)/3.06
   else
    emis_II= (/(f1_II[:]->SFSO4_PR)/)/3.06 + (/(f1_II[:]->SO4_PR_CLXF)/)/3.06
   end if

   if(GdepII .eq. "Neu") then
 sour_II=emis_II + (/(f1_II[:]->GS_H2SO4)/)/3.06 \ ; gas phase H2SO4 production
                  + (/f1_II[:]->AQ_SO4_A2_OCW/)/3.59 \ ; aq phase H2SO4 production
                  + (/f1_II[:]->AQ_H2SO4/)/3.06      ; some of the aq phase prod is just cond of gas phase
   else
 sour_II=emis_II + (/(f1_II[:]->GS_H2SO4)/)/3.06 + (/(f1_II[:]->WD_A_H2SO4)/)/3.06  \ ; gas phase H2SO4 production
                 + (/f1_II[:]->AQ_SO4_A2_OCW/)/3.59 \ ; aq phase H2SO4 production
                 + (/f1_II[:]->AQ_H2SO4/)/3.06      ; some of the aq phase prod is just cond of gas phase
   end if

 load_II=(/(f1_II[:]->cb_SO4_A1)/)/3.06 + (/(f1_II[:]->cb_SO4_A2)/)/3.59 + (/(f1_II[:]->cb_SO4_AC)/)/3.06 \
       + (/(f1_II[:]->cb_SO4_NA)/)/3.06 + (/(f1_II[:]->cb_SO4_PR)/)/3.06  \
       + (/(f1_II[:]->cb_SO4_A1_OCW)/)/3.06 + (/(f1_II[:]->cb_SO4_A2_OCW)/)/3.59 + (/(f1_II[:]->cb_SO4_AC_OCW)/)/3.06 \
       + (/(f1_II[:]->cb_SO4_NA_OCW)/)/3.06 + (/(f1_II[:]->cb_SO4_PR_OCW)/)/3.06
 
 wet_II=(/f1_II[:]->SO4_A1SFWET/)/3.06 + (/f1_II[:]->SO4_A2SFWET/)/3.59 + (/f1_II[:]->SO4_ACSFWET/)/3.06  \
      + (/f1_II[:]->SO4_NASFWET/)/3.06 + (/f1_II[:]->SO4_PRSFWET/)/3.06   \
      + (/f1_II[:]->SO4_A1_OCWSFWET/)/3.06 + (/f1_II[:]->SO4_A2_OCWSFWET/)/3.59 + (/f1_II[:]->SO4_AC_OCWSFWET/)/3.06 \
      + (/f1_II[:]->SO4_NA_OCWSFWET/)/3.06 + (/f1_II[:]->SO4_PR_OCWSFWET/)/3.06 \
      + (/f1_II[:]->WD_A_H2SO4/)/3.06
 
 dry_II=(/f1_II[:]->SO4_A1DDF/)/3.06 + (/f1_II[:]->SO4_A2DDF/)/3.59 + (/f1_II[:]->SO4_ACDDF/)/3.06  \
      + (/f1_II[:]->SO4_NADDF/)/3.06 + (/f1_II[:]->SO4_PRDDF/)/3.06   \
      + (/f1_II[:]->SO4_A1_OCWDDF/)/3.06 + (/f1_II[:]->SO4_A2_OCWDDF/)/3.59 + (/f1_II[:]->SO4_AC_OCWDDF/) /3.06 \
      + (/f1_II[:]->SO4_NA_OCWDDF/)/3.06 + (/f1_II[:]->SO4_PR_OCWDDF/)/3.06

 sink_II=wet_II-dry_II

 else if (plot_type.eq.2) then
 var="BC"  
 varname="BC"
 if(ModI.eq."CAM4-Oslo") then
   emis_I=(/(f1_I[:]->EMI_BC)/)
   sour_I=(/(f1_I[:]->EMI_BC)/)
   load_I=(/f1_I[:]->C_BC/)
   wet_I=(/f1_I[:]->WET_BC/)
   sink_I=(/f1_I[:]->WET_BC/)+(/f1_I[:]->DRY_BC/)
 else
  if(GdepII .eq. "Neu") then
   emis_I=(/(f1_I[:]->SFBC_A)/) + (/(f1_I[:]->SFBC_AC)/) + (/(f1_I[:]->SFBC_AX)/) + (/(f1_I[:]->SFBC_AI)/) + (/(f1_I[:]->SFBC_NI)/) + (/(f1_I[:]->SFBC_N)/) + (/(f1_I[:]->BC_AX_CMXF)/) + (/(f1_I[:]->BC_NI_CMXF)/) + (/(f1_I[:]->BC_N_CMXF)/)
  else
   emis_I=(/(f1_I[:]->SFBC_A)/) + (/(f1_I[:]->SFBC_AC)/) + (/(f1_I[:]->SFBC_AX)/) + (/(f1_I[:]->SFBC_AI)/) + (/(f1_I[:]->SFBC_NI)/) + (/(f1_I[:]->SFBC_N)/) + (/(f1_I[:]->BC_AX_CLXF)/) + (/(f1_I[:]->BC_NI_CLXF)/) + (/(f1_I[:]->BC_N_CLXF)/)
  end if
   sour_I=emis_I

   load_I=(/(f1_I[:]->cb_BC)/) + (/(f1_I[:]->cb_BC_A_OCW)/) + (/(f1_I[:]->cb_BC_AC_OCW)/) + (/(f1_I[:]->cb_BC_AI_OCW)/) + (/(f1_I[:]->cb_BC_NI_OCW)/) + (/(f1_I[:]->cb_BC_N_OCW)/)

   wet_I=(/f1_I[:]->BC_ASFWET/) + (/f1_I[:]->BC_ACSFWET/) + (/f1_I[:]->BC_AXSFWET/) + (/f1_I[:]->BC_AISFWET/) + (/f1_I[:]->BC_NISFWET/) + (/f1_I[:]->BC_NSFWET/) \
   + (/f1_I[:]->BC_A_OCWSFWET/) + (/f1_I[:]->BC_AC_OCWSFWET/) + (/f1_I[:]->BC_AI_OCWSFWET/) + (/f1_I[:]->BC_NI_OCWSFWET/) + (/f1_I[:]->BC_N_OCWSFWET/)

   dry_I=(/f1_I[:]->BC_ADDF/) + (/f1_I[:]->BC_ACDDF/) + (/f1_I[:]->BC_AXDDF/) + (/f1_I[:]->BC_AIDDF/) + (/f1_I[:]->BC_NIDDF/) + (/f1_I[:]->BC_NDDF/) \
       + (/f1_I[:]->BC_A_OCWDDF/) + (/f1_I[:]->BC_AC_OCWDDF/) + (/f1_I[:]->BC_AI_OCWDDF/) + (/f1_I[:]->BC_NI_OCWDDF/) + (/f1_I[:]->BC_N_OCWDDF/)

   sink_I=wet_I-dry_I
 end if
  if(GdepII .eq. "Neu") then
 emis_II=(/(f1_II[:]->SFBC_A)/) + (/(f1_II[:]->SFBC_AC)/) + (/(f1_II[:]->SFBC_AX)/) + (/(f1_II[:]->SFBC_AI)/) + (/(f1_II[:]->SFBC_NI)/) + (/(f1_II[:]->SFBC_N)/) + (/(f1_II[:]->BC_AX_CMXF)/) + (/(f1_II[:]->BC_NI_CMXF)/) + (/(f1_II[:]->BC_N_CMXF)/)
  else
 emis_II=(/(f1_II[:]->SFBC_A)/) + (/(f1_II[:]->SFBC_AC)/) + (/(f1_II[:]->SFBC_AX)/) + (/(f1_II[:]->SFBC_AI)/) + (/(f1_II[:]->SFBC_NI)/) + (/(f1_II[:]->SFBC_N)/) + (/(f1_II[:]->BC_AX_CLXF)/) + (/(f1_II[:]->BC_NI_CLXF)/) + (/(f1_II[:]->BC_N_CLXF)/)
  end if

sour_II=emis_II

 load_II=(/(f1_II[:]->cb_BC)/) + (/(f1_II[:]->cb_BC_A_OCW)/) + (/(f1_II[:]->cb_BC_AC_OCW)/) + (/(f1_II[:]->cb_BC_AI_OCW)/) + (/(f1_II[:]->cb_BC_NI_OCW)/) + (/(f1_II[:]->cb_BC_N_OCW)/)

 wet_II=(/f1_II[:]->BC_ASFWET/) + (/f1_II[:]->BC_ACSFWET/) + (/f1_II[:]->BC_AXSFWET/) + (/f1_II[:]->BC_AISFWET/) + (/f1_II[:]->BC_NISFWET/) + (/f1_II[:]->BC_NSFWET/) + (/f1_II[:]->BC_A_OCWSFWET/) + (/f1_II[:]->BC_AC_OCWSFWET/) + (/f1_II[:]->BC_AI_OCWSFWET/) + (/f1_II[:]->BC_NI_OCWSFWET/) + (/f1_II[:]->BC_N_OCWSFWET/)

 dry_II=(/f1_II[:]->BC_ADDF/) + (/f1_II[:]->BC_ACDDF/) + (/f1_II[:]->BC_AXDDF/) + (/f1_II[:]->BC_AIDDF/) + (/f1_II[:]->BC_NIDDF/) + (/f1_II[:]->BC_NDDF/) \
      + (/f1_II[:]->BC_A_OCWDDF/) + (/f1_II[:]->BC_AC_OCWDDF/) + (/f1_II[:]->BC_AI_OCWDDF/) + (/f1_II[:]->BC_NI_OCWDDF/) + (/f1_II[:]->BC_N_OCWDDF/)

 sink_II=wet_II-dry_II
 else if (plot_type.eq.3) then
 var="POM" 
 varname="POM"  
 if(ModI.eq."CAM4-Oslo") then
   MSAProd_I = 1.0*(/f1_I[:]->MSAGA /)*96/32 ; MSAProd is written out as "S", but MSA has Mw of 96
                                             ; hardcoded as factor "3" in gaschem.F90: dqdt(i,k,l_om_ni)= 3._r8*pmsa 
   SOAProd_I = 0.0*MSAProd_I ; Don't have this term from NorESM1
   terpeneLoss_I = 0.0*SOAProd_I ; Don't have any terpene loss in noresm1

   emis_I=(/(f1_I[:]->EMI_POM)/)
   sour_I=(/(f1_I[:]->EMI_POM)/) + MSAProd_I
   load_I=(/f1_I[:]->C_POM/)
   wet_I=(/f1_I[:]->WET_POM/)
   sink_I=(/f1_I[:]->WET_POM/)+(/f1_I[:]->DRY_POM/)
   
 else

   SOAProd_I =  (/(f1_I[:]->GS_SOA_LV)/) +  (/(f1_I[:]->GS_SOA_SV)/) 

   terpeneLoss_I = -1.0*terpenes_lost_as_soa_I

   MSAProd_I = 0.0*terpeneLoss_I

   if(GdepII .eq. "Neu") then
    emis_I=(/(f1_I[:]->SFOM_AI)/) + (/(f1_I[:]->SFOM_AC)/) + (/(f1_I[:]->SFOM_NI)/) +  (/f1_I[:]->OM_NI_CMXF/)
   else
    emis_I=(/(f1_I[:]->SFOM_AI)/) + (/(f1_I[:]->SFOM_AC)/) + (/(f1_I[:]->SFOM_NI)/) +  (/f1_I[:]->OM_NI_CLXF/)
   end if

   sour_I=emis_I + SOAProd_I 

   load_I=(/(f1_I[:]->cb_OM)/) \ 
       + (/(f1_I[:]->cb_OM_AI_OCW)/) + (/(f1_I[:]->cb_OM_AC_OCW)/) + (/(f1_I[:]->cb_OM_NI_OCW)/) \
       + (/(f1_I[:]->cb_SOA_NA_OCW)/) + (/(f1_I[:]->cb_SOA_A1_OCW)/) 

   wet_I=(/f1_I[:]->OM_AISFWET/) + (/f1_I[:]->OM_ACSFWET/) + (/f1_I[:]->OM_NISFWET/) \
      + (/f1_I[:]->OM_AI_OCWSFWET/) + (/f1_I[:]->OM_AC_OCWSFWET/) + (/f1_I[:]->OM_NI_OCWSFWET/) \
      + (/f1_I[:]->SOA_NASFWET/) + (/f1_I[:]->SOA_A1SFWET/) \
      + (/f1_I[:]->SOA_NA_OCWSFWET/) + (/f1_I[:]->SOA_A1_OCWSFWET/) 

   dry_I=(/f1_I[:]->OM_AIDDF/) + (/f1_I[:]->OM_ACDDF/) + (/f1_I[:]->OM_NIDDF/) \
      + (/f1_I[:]->OM_AI_OCWDDF/) + (/f1_I[:]->OM_AC_OCWDDF/) + (/f1_I[:]->OM_NI_OCWDDF/) \
      + (/f1_I[:]->SOA_NADDF/) + (/f1_I[:]->SOA_A1DDF/) \
      + (/f1_I[:]->SOA_NA_OCWDDF/) + (/f1_I[:]->SOA_A1_OCWDDF/)

   sink_I=wet_I-dry_I
 end if
 
 SOAProd_II =  (/(f1_II[:]->GS_SOA_LV)/) +  (/(f1_II[:]->GS_SOA_SV)/) 

 terpeneLoss_II = -1.0*terpenes_lost_as_soa_II

 MSAProd_II = 0.0*terpeneLoss_II

  if(GdepII .eq. "Neu") then
   emis_II=(/(f1_II[:]->SFOM_AI)/) + (/(f1_II[:]->SFOM_AC)/) + (/(f1_II[:]->SFOM_NI)/) +  (/f1_II[:]->OM_NI_CMXF/)
  else
    emis_I=(/(f1_I[:]->SFOM_AI)/) + (/(f1_I[:]->SFOM_AC)/) + (/(f1_I[:]->SFOM_NI)/) +  (/f1_I[:]->OM_NI_CLXF/)
  end if

 sour_II=emis_II + SOAProd_II

 load_II=(/(f1_II[:]->cb_OM)/) \ 
       + (/(f1_II[:]->cb_OM_AI_OCW)/) + (/(f1_II[:]->cb_OM_AC_OCW)/) + (/(f1_II[:]->cb_OM_NI_OCW)/) \
       + (/(f1_II[:]->cb_SOA_NA_OCW)/) + (/(f1_II[:]->cb_SOA_A1_OCW)/) 

 wet_II=(/f1_II[:]->OM_AISFWET/) + (/f1_II[:]->OM_ACSFWET/) + (/f1_II[:]->OM_NISFWET/) \
      + (/f1_II[:]->OM_AI_OCWSFWET/) + (/f1_II[:]->OM_AC_OCWSFWET/) + (/f1_II[:]->OM_NI_OCWSFWET/) \
      + (/f1_II[:]->SOA_NASFWET/) + (/f1_II[:]->SOA_A1SFWET/) \
      + (/f1_II[:]->SOA_NA_OCWSFWET/) + (/f1_II[:]->SOA_A1_OCWSFWET/) 

 dry_II=(/f1_II[:]->OM_AIDDF/) + (/f1_II[:]->OM_ACDDF/) + (/f1_II[:]->OM_NIDDF/) \
      + (/f1_II[:]->OM_AI_OCWDDF/) + (/f1_II[:]->OM_AC_OCWDDF/) + (/f1_II[:]->OM_NI_OCWDDF/) \
      + (/f1_II[:]->SOA_NADDF/) + (/f1_II[:]->SOA_A1DDF/) \
      + (/f1_II[:]->SOA_NA_OCWDDF/) + (/f1_II[:]->SOA_A1_OCWDDF/)

 sink_II=wet_II-dry_II
 else if (plot_type.eq.4) then
 var="SS"                       
 varname="Sea-salt"
 if(ModI.eq."CAM4-Oslo") then
   emis_I=(/(f1_I[:]->EMI_SS)/)
   sour_I=(/(f1_I[:]->EMI_SS)/)

   load_I=(/f1_I[:]->C_SS/)
   wet_I=(/f1_I[:]->WET_SS/)
   sink_I=(/f1_I[:]->WET_SS/)+(/f1_I[:]->DRY_SS/)
 else
   emis_I=(/(f1_I[:]->SFSS_A1)/) + (/(f1_I[:]->SFSS_A2)/) + (/(f1_I[:]->SFSS_A3)/)
   sour_I=emis_I
   load_I=(/(f1_I[:]->cb_SALT)/) + (/(f1_I[:]->cb_SS_A1_OCW)/) + (/(f1_I[:]->cb_SS_A2_OCW)/) + (/(f1_I[:]->cb_SS_A3_OCW)/) 
   wet_I=(/f1_I[:]->SS_A1SFWET/) + (/f1_I[:]->SS_A2SFWET/) + (/f1_I[:]->SS_A3SFWET/) + (/f1_I[:]->SS_A1_OCWSFWET/) + (/f1_I[:]->SS_A2_OCWSFWET/) + (/f1_I[:]->SS_A3_OCWSFWET/)
   dry_I=(/f1_I[:]->SS_A1DDF/) + (/f1_I[:]->SS_A2DDF/) + (/f1_I[:]->SS_A3DDF/) + (/f1_I[:]->SS_A1_OCWDDF/) + (/f1_I[:]->SS_A2_OCWDDF/) + (/f1_I[:]->SS_A3_OCWDDF/)
   sink_I=wet_I-dry_I
 end if
 emis_II=(/(f1_II[:]->SFSS_A1)/) + (/(f1_II[:]->SFSS_A2)/) + (/(f1_II[:]->SFSS_A3)/)
 sour_II=emis_II
 load_II=(/(f1_II[:]->cb_SALT)/) + (/(f1_II[:]->cb_SS_A1_OCW)/) + (/(f1_II[:]->cb_SS_A2_OCW)/) + (/(f1_II[:]->cb_SS_A3_OCW)/) 
 wet_II=(/f1_II[:]->SS_A1SFWET/) + (/f1_II[:]->SS_A2SFWET/) + (/f1_II[:]->SS_A3SFWET/) + (/f1_II[:]->SS_A1_OCWSFWET/) + (/f1_II[:]->SS_A2_OCWSFWET/) + (/f1_II[:]->SS_A3_OCWSFWET/)
 dry_II=(/f1_II[:]->SS_A1DDF/) + (/f1_II[:]->SS_A2DDF/) + (/f1_II[:]->SS_A3DDF/) + (/f1_II[:]->SS_A1_OCWDDF/) + (/f1_II[:]->SS_A2_OCWDDF/) + (/f1_II[:]->SS_A3_OCWDDF/)
 sink_II=wet_II-dry_II
 else if (plot_type.eq.5) then
 var="DU"   
 varname="Dust"  
 if(ModI.eq."CAM4-Oslo") then
   emis_I=(/(f1_I[:]->EMI_DUST)/)
   sour_I=(/(f1_I[:]->EMI_DUST)/)
   load_I=(/f1_I[:]->C_DUST/)
   wet_I=(/f1_I[:]->WET_DUST/)
   sink_I=(/f1_I[:]->WET_DUST/)+(/f1_I[:]->DRY_DUST/) 
 else
   emis_I=(/(f1_I[:]->SFDST_A2)/) + (/(f1_I[:]->SFDST_A3)/)
   sour_I=emis_I
   load_I=(/(f1_I[:]->cb_DUST)/) + (/(f1_I[:]->cb_DST_A2_OCW)/) + (/(f1_I[:]->cb_DST_A3_OCW)/)
   wet_I=(/f1_I[:]->DST_A2SFWET/) + (/f1_I[:]->DST_A3SFWET/) + (/f1_I[:]->DST_A2_OCWSFWET/) + (/f1_I[:]->DST_A3_OCWSFWET/)
   dry_I=(/f1_I[:]->DST_A2DDF/) + (/f1_I[:]->DST_A3DDF/) + (/f1_I[:]->DST_A2_OCWDDF/) + (/f1_I[:]->DST_A3_OCWDDF/)
   sink_I=wet_I-dry_I
 end if
 emis_II=(/(f1_II[:]->SFDST_A2)/) + (/(f1_II[:]->SFDST_A3)/)
 sour_II=emis_II
 load_II=(/(f1_II[:]->cb_DUST)/) + (/(f1_II[:]->cb_DST_A2_OCW)/) + (/(f1_II[:]->cb_DST_A3_OCW)/)
 wet_II=(/f1_II[:]->DST_A2SFWET/) + (/f1_II[:]->DST_A3SFWET/) + (/f1_II[:]->DST_A2_OCWSFWET/) + (/f1_II[:]->DST_A3_OCWSFWET/)
 dry_II=(/f1_II[:]->DST_A2DDF/) + (/f1_II[:]->DST_A3DDF/) + (/f1_II[:]->DST_A2_OCWDDF/) + (/f1_II[:]->DST_A3_OCWDDF/)
 sink_II=wet_II-dry_II
 end if
 end if
 end if
 end if
 end if
 end if
 end if

 lifetime_I=-load_I/(sink_I+small)
 lifetime_II=-load_II/(sink_II+small)

; Initializing and calculating area weighted values

 emis_Ia=emis_I
 emis_IIa=emis_II
 sour_Ia=sour_I
 sour_IIa=sour_II
 lifetime_Ia=lifetime_I
 lifetime_IIa=lifetime_II
 load_Ia=load_I 
 load_IIa=load_II
 wet_Ia=wet_I
 wet_IIa=wet_II
 sink_Ia=sink_I
 sink_IIa=sink_II
if (plot_type.eq.-1.or.plot_type.eq.0) then
 chloss_Ia=chloss_I
 chloss_IIa=chloss_II
 chlossg_Ia=chlossg_I
 chlossg_IIa=chlossg_II
end if
if(plot_type .eq. 3)then
  MSAProd_Ia  = MSAProd_I
  MSAProd_IIa = MSAProd_II
  SOAProd_Ia  = SOAProd_I 
  SOAProd_IIa = SOAProd_II
  terpeneLoss_Ia = terpeneLoss_I
  terpeneLoss_IIa = terpeneLoss_II 
end if

 xdims_I = dimsizes(gw0_I)
 ydims_I = dimsizes(lifetime_Ia)
 do i=0,dimsizes(gw0_I)-1
 sour_Ia(:,i,:)=sour_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 emis_Ia(:,i,:)=emis_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 lifetime_Ia(:,i,:)=lifetime_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 load_Ia(:,i,:)=load_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 wet_Ia(:,i,:)=wet_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 sink_Ia(:,i,:)=sink_I(:,i,:)*coffa*dlon_I*gw0_I(i)
if (plot_type.eq.-1.or.plot_type.eq.0) then
 chloss_Ia(:,i,:)=chloss_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 chlossg_Ia(:,i,:)=chlossg_I(:,i,:)*coffa*dlon_I*gw0_I(i)
end if
if (plot_type .eq. 3)then
 MSAProd_Ia(:,i,:)=MSAProd_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 terpeneLoss_Ia(:,i,:)=terpeneLoss_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 SOAProd_Ia(:,i,:)=SOAProd_I(:,i,:)*coffa*dlon_I*gw0_I(i)
end if
 end do
 emisave_I=sum(dim_avg_n(emis_Ia,0))/area1
 sourave_I=sum(dim_avg_n(sour_Ia,0))/area1
 loadave_I=sum(dim_avg_n(load_Ia,0))/area1
 wetave_I=sum(dim_avg_n(wet_Ia,0))/area1  
 sinkave_I=sum(dim_avg_n(sink_Ia,0))/area1  
if (plot_type.eq.-1.or.plot_type.eq.0) then
 chlossave_I=sum(dim_avg_n(chloss_Ia,0))/area1  
 chlossgave_I=sum(dim_avg_n(chlossg_Ia,0))/area1  
end if

 xdims_II = dimsizes(gw0_II)
 ydims_II = dimsizes(lifetime_IIa)
 do i=0,dimsizes(gw0_II)-1
 sour_IIa(:,i,:)=sour_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 emis_IIa(:,i,:)=emis_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 lifetime_IIa(:,i,:)=lifetime_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 load_IIa(:,i,:)=load_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 wet_IIa(:,i,:)=wet_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 sink_IIa(:,i,:)=sink_II(:,i,:)*coffa*dlon_II*gw0_II(i)
if (plot_type.eq.-1.or.plot_type.eq.0) then
 chloss_IIa(:,i,:)=chloss_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 chlossg_IIa(:,i,:)=chlossg_II(:,i,:)*coffa*dlon_II*gw0_II(i)
end if
if (plot_type .eq. 3)then
 MSAProd_IIa(:,i,:)=MSAProd_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 terpeneLoss_IIa(:,i,:)=terpeneLoss_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 SOAProd_IIa(:,i,:)=SOAProd_II(:,i,:)*coffa*dlon_II*gw0_II(i)
end if
 end do
 emisave_II=sum(dim_avg_n(emis_IIa,0))/area1
 sourave_II=sum(dim_avg_n(sour_IIa,0))/area1
 loadave_II=sum(dim_avg_n(load_IIa,0))/area1
 wetave_II=sum(dim_avg_n(wet_IIa,0))/area1  
 sinkave_II=sum(dim_avg_n(sink_IIa,0))/area1  
if (plot_type.eq.-1.or.plot_type.eq.0) then
   chlossave_II=sum(dim_avg_n(chloss_IIa,0))/area1  
   chlossgave_II=sum(dim_avg_n(chlossg_IIa,0))/area1  
end if
if (plot_type .eq. 3)then
   MSAProdave_I = sum(dim_avg_n(MSAProd_Ia,0))/area1
   MSAProdave_II = sum(dim_avg_n(MSAProd_IIa,0))/area1
   SOAProdave_I = sum(dim_avg_n(SOAProd_Ia,0))/area1
   SOAProdave_II = sum(dim_avg_n(SOAProd_IIa,0))/area1
   terpeneLossave_I = sum(dim_avg_n(terpeneLoss_Ia,0))/area1
   terpeneLossave_II = sum(dim_avg_n(terpeneLoss_IIa,0))/area1
end if
;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Print the table entries
;
;;;;;;;;;;;;;;;;;;;;;;;;;

; Note: Values for SO2 and SO4 are given as Sulfur (S) values
lifetimeave_I=-loadave_I/(sinkave_I+small)/3600.0/24.0    ; s -> days
lifetimeave_II=-loadave_II/(sinkave_II+small)/3600.0/24.0 ; s -> days
emisave_I=emisave_I*1.e-9*area1*3600*24*365               ; kg/m2/s -> Tg/yr
emisave_II=emisave_II*1.e-9*area1*3600*24*365             ; kg/m2/s -> Tg/yr
sourave_I=sourave_I*1.e-9*area1*3600*24*365               ; kg/m2/s -> Tg/yr
sourave_II=sourave_II*1.e-9*area1*3600*24*365             ; kg/m2/s -> Tg/yr
sinkave_I=-1.0*sinkave_I*1.e-9*area1*3600*24*365          ; kg/m2/s -> Tg/yr 
sinkave_II=-1.0*sinkave_II*1.e-9*area1*3600*24*365        ; kg/m2/s -> Tg/yr
wetave_I = -1.0*wetave_I*1.e-9*area1*3600*24*365          ; kg/m2/s -> Tg/yr
wetave_II = -1.0*wetave_II*1.e-9*area1*3600*24*365        ; kg/m2/s -> Tg/yr
if(plot_type.eq.-1 .or. plot_type.eq.0)then
   chlossave_I =   chlossave_I*1.e-9*area1*3600*24*365    ; kg/m2/s -> Tg/yr
   chlossave_II =  chlossave_II*1.e-9*area1*3600*24*365   ; kg/m2/s -> Tg/yr
   chlossgave_I =  chlossgave_I*1.e-9*area1*3600*24*365   ; kg/m2/s -> Tg/yr
   chlossgave_II = chlossgave_II*1.e-9*area1*3600*24*365  ; kg/m2/s -> Tg/yr
end if
loadave_I=loadave_I*1.e-9*area1                           ; kg/m2 -> Tg
loadave_II=loadave_II*1.e-9*area1                         ; kg/m2 -> Tg
wetdeppave_I=1.e2*wetave_I/(sinkave_I+small)              ; fraction -> %
wetdeppave_II=1.e2*wetave_II/(sinkave_II+small)           ; fraction -> %
if (plot_type.eq.-1.or.plot_type.eq.0) then
   chlosspave_I=1.e2*chlossave_I/(-1.0*sinkave_I+small)     ; fraction -> %
   chlosspave_II=1.e2*chlossave_II/(-1.0*sinkave_II+small)   ; fraction -> %
   chlossgpave_I=1.e2*chlossgave_I/(chlossave_I+small)       ; fraction -> %
   chlossgpave_II=1.e2*chlossgave_II/(chlossave_II+small)    ; fraction -> %
end if
if(plot_type .eq. 3)then
  MSAProdave_I = (MSAProdave_I \
                 + terpeneLossave_I \
                 + SOAProdave_I ) *1.e-9*area1*3600*24*365          ; kg/m2/s -> Tg/yr
  MSAProdave_II = ( MSAProdave_II \
                + terpeneLossave_II \
                + SOAProdave_II )*1.e-9*area1*3600*24*365          ; kg/m2/s -> Tg/yr
  SOAProdave_I = SOAProdave_I*1.e-9*area1*3600*24*365          ; kg/m2/s -> Tg/yr
  SOAProdave_II = SOAProdave_II*1.e-9*area1*3600*24*365          ; kg/m2/s -> Tg/yr
  terpeneLossave_I = terpeneLossave_I*1.e-9*area1*3600*24*365
  terpeneLossave_II = terpeneLossave_II*1.e-9*area1*3600*24*365
end if

print("Mass budget numbers for model version      I (old), II (new)")
print("Note: Values for DMS; SO2 and SO4 are given as Sulfur (S) values")
print("-------------------------------------------------------------")
print(var+" total emissions   (Tg/yr)             = "+sprintf("%4.3f",emisave_I)+", "+sprintf("%4.3f",emisave_II))
print(var+" total sources     (Tg/yr)             = "+sprintf("%4.3f",sourave_I)+", "+sprintf("%4.3f",sourave_II))
print(var+" total sink        (Tg/yr)             = "+sprintf("%4.3f",sinkave_I)+", "+sprintf("%4.3f",sinkave_II))
print(var+" burden            (Tg)                = "+sprintf("%4.3f",loadave_I)+", "+sprintf("%4.3f",loadave_II))
if (plot_type.ne.-1) then
print(var+" wet depostion     (% of sinks)        = "+sprintf("%4.3f",wetdeppave_I)+", "+sprintf("%4.3f",wetdeppave_II))
end if
print(var+" lifetime          (d)                 = "+sprintf("%4.3f",lifetimeave_I)+", "+sprintf("%4.3f",lifetimeave_II))
if (plot_type.eq.-1.or.plot_type.eq.0) then
print(var+" chemical loss     (%)                 = "+sprintf("%4.3f",chlosspave_I)+", "+sprintf("%4.3f",chlosspave_II))
end if
if (plot_type.eq.-1) then
print(var+" pct of chem. loss to MSA (%)          = "+sprintf("%4.3f",chlossgpave_I)+", "+sprintf("%4.3f",chlossgpave_II))
end if
if (plot_type.eq.0) then
print(var+" pct of chem loss in clear air (%)    = "+sprintf("%4.3f",chlossgpave_I)+", "+sprintf("%4.3f",chlossgpave_II))
end if
if (plot_type.eq.3)then
   print(var+ " chem prod from MSA (Tg/yr)            = "+sprintf("%4.3f",MSAProdave_I)+", "+sprintf("%4.3f",MSAProdave_II))
   print(var+ " chem prod (total) (Tg/yr)             = " +sprintf("%4.3f",SOAProdave_I)+", "+sprintf("%4.3f",SOAProdave_II))
   print(var+ " chem prod terpenes (Tg /yr)           = " +sprintf("%4.3f",-1.0*terpeneLossave_I)+", "+sprintf("%4.3f",-1.0*terpeneLossave_II))
end if

print("-------------------------------------------------------------")

end
