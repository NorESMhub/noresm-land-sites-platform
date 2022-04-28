  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in aerosol and gas burdens and loss and production terms 
; from two versions of NorESM/CAM-Oslo and makes global plots of the respective 
; annually averaged lifetimes, including global average as a number in the title 
; line for each figure. The global average is be calculated both as area load/loss 
; and as area averaged load / area averaged loss, with the latter value shown in 
; brackets.

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
  plot_type = 0                      ;-1 => DMS lifetime
                                     ; 0 => SO2 lifetime
                                     ; 1 => SO4 lifetime
                                     ; 2 => BC  lifetime
                                     ; 3 => POM lifetime
                                     ; 4 => SS  lifetime
                                     ; 5 => DU  lifetime
 end if
 if (.not. isvar("format")) then        ; is format on command line?
  format = "ps"
;  format = "eps"
;  format = "png"
;  format = "pdf"
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
 netloss_I=tmp_I
 netloss_II=tmp_II

 if (plot_type.eq.-1) then                                ; is skipped due to missing msga variables in CAM5-Oslo   
 var="Lifetime_DMS"                       ; name of plot
 varname="DMS lifetime (d)"               ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/f1_I[:]->C_DMS/)                                 ; variable to be plotted from I
   netloss_I=-((/f1_I[:]->S2GA/)+(/f1_I[:]->MSAGA/))        ; variable to be plotted from I
 else
   load_I=(/f1_I[:]->cb_DMS/)                               ; variable to be plotted from I 
   netloss_I=-((/f1_I[:]->S2GA/)+(/f1_I[:]->MSAGA/))     ; mangler ennå variable her...
 end if
 load_II=(/f1_II[:]->cb_DMS/)                               ; variable to be plotted from II 
 netloss_II=-((/f1_II[:]->S2GA/)+(/f1_II[:]->MSAGA/))     ; mangler ennå variable her...
 else if (plot_type.eq.0) then
 var="Lifetime_SO2"                       ; name of plot
 varname="SO~B~2~N~ lifetime (d)"         ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/f1_I[:]->C_SO2/)                                 ; variable to be plotted from I
   netloss_I=(/f1_I[:]->WET_SO2/)+(/f1_I[:]->DRY_SO2/)-((/f1_I[:]->S4GA/)+(/f1_I[:]->S4AQ/))      ; variable to be plotted from I
 else
   load_I=(/f1_I[:]->cb_SO2/)/1.998                               ; variable to be plotted from I 

if(GdepI .eq. "Neu") then
   netloss_I= -(/f1_I[:]->WD_A_SO2/)/1.998 \   ;kg/m2/sec (positive in output file)
              -(/f1_I[:]->DF_SO2/)/1.998 \     ;kg/m2/sec (positive in output file)  
              +(/f1_I[:]->AQ_SO2/)/1.998 \     ;kg/m2/ses (negative in output file)
              +(/f1_I[:]->GS_SO2/)/1.998 - (/f1_I[:]->SO2_CMXF/)/1.998 + (/f1_I[:]->GS_DMS/)/1.938  ; net chemical loss (gas phase)
else
   netloss_I= -(/f1_I[:]->WD_A_SO2/)/1.998 \   ;kg/m2/sec (positive in output file)
              -(/f1_I[:]->DF_SO2/)/1.998 \     ;kg/m2/sec (positive in output file)  
              +(/f1_I[:]->AQ_SO2/)/1.998 \     ;kg/m2/ses (negative in output file)
              +(/f1_I[:]->GS_SO2/)/1.998 - (/f1_I[:]->SO2_CLXF/)/1.998 + (/f1_I[:]->GS_DMS/)/1.938 + (/f1_I[:]->WD_A_SO2/)/1.998  ; net chemical loss (gas phase)
end if
 end if
 load_II=(/f1_II[:]->cb_SO2/)/1.998                               ; variable to be plotted from II 
 ;In new model, GS_SO2 is budget of all that goes on in the chemistry-routine which is 
 ;1) Gas phase chemistry, 2) Wet deposition and 3) 3D-emissions
 ;Gas phase chemistry is both production from DMS (GS_DMS) and loss through OH (GL_OH) 
 ;We are only interested in the loss through OH from the chemistry-term (GL_OH)
 ;GS_SO2 = GL_OH + SO2_CLXF - WD_A_SO2 - GS_DMS*64/62 -> GS_SO2 = GL_OH + SO2_CMXF - WD_A_SO2 - GS_DMS*64/62 in new version
 ;GL_OH = GS_SO2 -SO2_CLXF + WD_A_SO2 + GS_DMS*64/62  -> GL_OH = GS_SO2 -SO2_CMXF + WD_A_SO2 + GS_DMS*64/62 in new version
if(GdepII .eq. "Neu") then
 netloss_II= -(/f1_II[:]->WD_A_SO2/)/1.998 \   ;kg/m2/sec (positive in output file)
             -(/f1_II[:]->DF_SO2/)/1.998 \     ;kg/m2/sec (positive in output file)  
             +(/f1_II[:]->AQ_SO2/)/1.998 \     ;kg/m2/ses (negative in output file)
             +(/f1_II[:]->GS_SO2/)/1.998 - (/f1_II[:]->SO2_CMXF/)/1.998 + (/f1_II[:]->GS_DMS/)/1.938  ; net chemical loss (gas phase)
else
 netloss_II= -(/f1_II[:]->WD_A_SO2/)/1.998 \   ;kg/m2/sec (positive in output file)
             -(/f1_II[:]->DF_SO2/)/1.998 \     ;kg/m2/sec (positive in output file)  
             +(/f1_II[:]->AQ_SO2/)/1.998 \     ;kg/m2/ses (negative in output file)
             +(/f1_II[:]->GS_SO2/)/1.998 - (/f1_II[:]->SO2_CLXF/)/1.998 + (/f1_II[:]->GS_DMS/)/1.938 + (/f1_II[:]->WD_A_SO2/)/1.998  ; net chemical loss (gas phase)
end if
 else if (plot_type.eq.1) then
 var="Lifetime_SO4"                       ; name of input-variable and plot
 varname="SO~B~4~N~ lifetime (d)"         ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/f1_I[:]->C_SO4/)                                 ; variable to be plotted from I
   netloss_I=(/f1_I[:]->WET_SO4/)+(/f1_I[:]->DRY_SO4/)        ; variable to be plotted from I
 else
   load_I=(/(f1_I[:]->cb_SO4_A1)/)/3.06 + (/(f1_I[:]->cb_SO4_A2)/)/3.59 + (/(f1_I[:]->cb_SO4_AC)/)/3.06 + (/(f1_I[:]->cb_SO4_NA)/)/3.06 + (/(f1_I[:]->cb_SO4_PR)/)/3.06 + (/(f1_I[:]->cb_SO4_A1_OCW)/)/3.06 + (/(f1_I[:]->cb_SO4_A2_OCW)/)/3.59 + (/(f1_I[:]->cb_SO4_AC_OCW)/)/3.06 + (/(f1_I[:]->cb_SO4_NA_OCW)/)/3.06 + (/(f1_I[:]->cb_SO4_PR_OCW)/)/3.06
   wet_I=(/f1_I[:]->SO4_A1SFWET/)/3.06 + (/f1_I[:]->SO4_A2SFWET/)/3.59 + (/f1_I[:]->SO4_ACSFWET/)/3.06  + (/f1_I[:]->SO4_NASFWET/)/3.06 + (/f1_I[:]->SO4_PRSFWET/)/3.06 + (/f1_I[:]->SO4_A1_OCWSFWET/)/3.06 + (/f1_I[:]->SO4_A2_OCWSFWET/)/3.59 + (/f1_I[:]->SO4_AC_OCWSFWET/)/3.06 + (/f1_I[:]->SO4_NA_OCWSFWET/)/3.06 + (/f1_I[:]->SO4_PR_OCWSFWET/)/3.06 
   wet_I=-wet_I
   dry_I=(/f1_I[:]->SO4_A1DDF/)/3.06 + (/f1_I[:]->SO4_A2DDF/)/3.59 + (/f1_I[:]->SO4_ACDDF/)/3.06 + (/f1_I[:]->SO4_NADDF/) + (/f1_I[:]->SO4_PRDDF/)/3.06 + (/f1_I[:]->SO4_A1_OCWDDF/)/3.06 + (/f1_I[:]->SO4_A2_OCWDDF/)/3.59 + (/f1_I[:]->SO4_AC_OCWDDF/) /3.06+ (/f1_I[:]->SO4_NA_OCWDDF/)/3.06 + (/f1_I[:]->SO4_PR_OCWDDF/)/3.06
   netloss_I=-(wet_I+dry_I)
 end if
 load_II=(/(f1_II[:]->cb_SO4_A1)/)/3.06 + (/(f1_II[:]->cb_SO4_A2)/)/3.59 + (/(f1_II[:]->cb_SO4_AC)/)/3.06 + (/(f1_II[:]->cb_SO4_NA)/)/3.06 + (/(f1_II[:]->cb_SO4_PR)/)/3.06 + (/(f1_II[:]->cb_SO4_A1_OCW)/)/3.06 + (/(f1_II[:]->cb_SO4_A2_OCW)/)/3.59 + (/(f1_II[:]->cb_SO4_AC_OCW)/)/3.06 + (/(f1_II[:]->cb_SO4_NA_OCW)/)/3.06 + (/(f1_II[:]->cb_SO4_PR_OCW)/)/3.06
 wet_II=(/f1_II[:]->SO4_A1SFWET/)/3.06 + (/f1_II[:]->SO4_A2SFWET/)/3.59 + (/f1_II[:]->SO4_ACSFWET/)/3.06  +  (/f1_II[:]->SO4_NASFWET/)/3.06  + (/f1_II[:]->SO4_PRSFWET/)/3.06  + (/f1_II[:]->SO4_A1_OCWSFWET/)/3.06  + (/f1_II[:]->SO4_A2_OCWSFWET/)/3.59 + (/f1_II[:]->SO4_AC_OCWSFWET/)/3.06   + (/f1_II[:]->SO4_NA_OCWSFWET/)/3.06  + (/f1_II[:]->SO4_PR_OCWSFWET/)/3.06 
 wet_II=-wet_II
 dry_II=(/f1_II[:]->SO4_A1DDF/)/3.06  + (/f1_II[:]->SO4_A2DDF/)/3.59 + (/f1_II[:]->SO4_ACDDF/)/3.06 + (/f1_II[:]->SO4_NADDF/) + (/f1_II[:]->SO4_PRDDF/)/3.06 + (/f1_II[:]->SO4_A1_OCWDDF/)/3.06 + (/f1_II[:]->SO4_A2_OCWDDF/)/3.59 + (/f1_II[:]->SO4_AC_OCWDDF/) /3.06 + (/f1_II[:]->SO4_NA_OCWDDF/)/3.06 + (/f1_II[:]->SO4_PR_OCWDDF/)/3.06
 netloss_II=-(wet_II+dry_II)
 else if (plot_type.eq.2) then
 var="Lifetime_BC"                        ; name of input-variable and plot
 varname="BC lifetime (d)"                ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/f1_I[:]->C_BC/)                                  ; variable to be plotted from I
   netloss_I=(/f1_I[:]->WET_BC/)+(/f1_I[:]->DRY_BC/)          ; variable to be plotted from I
 else
   load_I=(/f1_I[:]->cb_BC/) + (/(f1_I[:]->cb_BC_A_OCW)/) + (/(f1_I[:]->cb_BC_AC_OCW)/) + (/(f1_I[:]->cb_BC_AI_OCW)/) + (/(f1_I[:]->cb_BC_NI_OCW)/) + (/(f1_I[:]->cb_BC_N_OCW)/)
   wet_I=(/f1_I[:]->BC_ASFWET/) + (/f1_I[:]->BC_ACSFWET/) + (/f1_I[:]->BC_AXSFWET/) + (/f1_I[:]->BC_AISFWET/) + (/f1_I[:]->BC_NISFWET/) + (/f1_I[:]->BC_NSFWET/) + (/f1_I[:]->BC_A_OCWSFWET/) + (/f1_I[:]->BC_AC_OCWSFWET/) + (/f1_I[:]->BC_AI_OCWSFWET/) + (/f1_I[:]->BC_NI_OCWSFWET/) + (/f1_I[:]->BC_N_OCWSFWET/)
   wet_I=-wet_I
   dry_I=(/f1_I[:]->BC_ACDDF/) + (/f1_I[:]->BC_AXDDF/) + (/f1_I[:]->BC_AIDDF/) + (/f1_I[:]->BC_NIDDF/) + (/f1_I[:]->BC_NDDF/) + (/f1_I[:]->BC_A_OCWDDF/) + (/f1_I[:]->BC_AC_OCWDDF/) + (/f1_I[:]->BC_AI_OCWDDF/) + (/f1_I[:]->BC_NI_OCWDDF/) + (/f1_I[:]->BC_N_OCWDDF/)
   netloss_I=-(wet_I+dry_I)
 end if
 load_II=(/f1_II[:]->cb_BC/) + (/(f1_II[:]->cb_BC_A_OCW)/) + (/(f1_II[:]->cb_BC_AC_OCW)/) + (/(f1_II[:]->cb_BC_AI_OCW)/) + (/(f1_II[:]->cb_BC_NI_OCW)/) + (/(f1_II[:]->cb_BC_N_OCW)/)
 wet_II=(/f1_II[:]->BC_ASFWET/) + (/f1_II[:]->BC_ACSFWET/) + (/f1_II[:]->BC_AXSFWET/) + (/f1_II[:]->BC_AISFWET/) + (/f1_II[:]->BC_NISFWET/) + (/f1_II[:]->BC_NSFWET/) + (/f1_II[:]->BC_A_OCWSFWET/) + (/f1_II[:]->BC_AC_OCWSFWET/) + (/f1_II[:]->BC_AI_OCWSFWET/) + (/f1_II[:]->BC_NI_OCWSFWET/) + (/f1_II[:]->BC_N_OCWSFWET/)
 wet_II=-wet_II
 dry_II=(/f1_II[:]->BC_ACDDF/) + (/f1_II[:]->BC_AXDDF/) + (/f1_II[:]->BC_AIDDF/) + (/f1_II[:]->BC_NIDDF/) + (/f1_II[:]->BC_NDDF/) + (/f1_II[:]->BC_A_OCWDDF/) + (/f1_II[:]->BC_AC_OCWDDF/) + (/f1_II[:]->BC_AI_OCWDDF/) + (/f1_II[:]->BC_NI_OCWDDF/) + (/f1_II[:]->BC_N_OCWDDF/)
 netloss_II=-(wet_II+dry_II)
 else if (plot_type.eq.3) then
 var="Lifetime_POM"                       ; name of input-variable and plot
 varname="POM lifetime (d)"               ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/f1_I[:]->C_POM/)                                 ; variable to be plotted from I
   netloss_I=(/f1_I[:]->WET_POM/)+(/f1_I[:]->DRY_POM/)        ; variable to be plotted from I
 else
   load_I=(/f1_I[:]->cb_OM/) + (/(f1_I[:]->cb_OM_AI_OCW)/) + (/(f1_I[:]->cb_OM_AC_OCW)/) + (/(f1_I[:]->cb_OM_NI_OCW)/)
   wet_I=(/f1_I[:]->OM_AISFWET/) + (/f1_I[:]->OM_ACSFWET/) + (/f1_I[:]->OM_NISFWET/) + (/f1_I[:]->OM_AI_OCWSFWET/) + (/f1_I[:]->OM_AC_OCWSFWET/) + (/f1_I[:]->OM_NI_OCWSFWET/) \
       + (/f1_I[:]->SOA_A1SFWET/) + (/f1_I[:]->SOA_NASFWET/) + (/f1_I[:]->SOA_A1_OCWSFWET/) + (/f1_I[:]->SOA_NA_OCWSFWET/)
   wet_I=-wet_I
   dry_I=(/f1_I[:]->OM_AIDDF/) + (/f1_I[:]->OM_ACDDF/) + (/f1_I[:]->OM_NIDDF/) + (/f1_I[:]->OM_AI_OCWDDF/) + (/f1_I[:]->OM_AC_OCWDDF/) + (/f1_I[:]->OM_NI_OCWDDF/) \ 
       + (/f1_I[:]->SOA_A1DDF/) + (/f1_I[:]->SOA_NADDF/) + (/f1_I[:]->SOA_A1_OCWDDF/) + (/f1_I[:]->SOA_NA_OCWDDF/)
   netloss_I=-(wet_I+dry_I)
 end if
 load_II=(/f1_II[:]->cb_OM/) + (/(f1_II[:]->cb_OM_AI_OCW)/) + (/(f1_II[:]->cb_OM_AC_OCW)/) + (/(f1_II[:]->cb_OM_NI_OCW)/)
 wet_II=(/f1_II[:]->OM_AISFWET/) + (/f1_II[:]->OM_ACSFWET/) + (/f1_II[:]->OM_NISFWET/) + (/f1_II[:]->OM_AI_OCWSFWET/) + (/f1_II[:]->OM_AC_OCWSFWET/) + (/f1_II[:]->OM_NI_OCWSFWET/) \
     + (/f1_II[:]->SOA_A1SFWET/) + (/f1_II[:]->SOA_NASFWET/) + (/f1_II[:]->SOA_A1_OCWSFWET/) + (/f1_II[:]->SOA_NA_OCWSFWET/)
 wet_II=-wet_II
 dry_II=(/f1_II[:]->OM_AIDDF/) + (/f1_II[:]->OM_ACDDF/) + (/f1_II[:]->OM_NIDDF/) + (/f1_II[:]->OM_AI_OCWDDF/) + (/f1_II[:]->OM_AC_OCWDDF/) + (/f1_II[:]->OM_NI_OCWDDF/) \
       + (/f1_II[:]->SOA_A1DDF/) + (/f1_II[:]->SOA_NADDF/) + (/f1_II[:]->SOA_A1_OCWDDF/) + (/f1_II[:]->SOA_NA_OCWDDF/)
 netloss_II=-(wet_II+dry_II)
 else if (plot_type.eq.4) then
 var="Lifetime_SS"                        ; name of input-variable and plot
 varname="Sea-salt lifetime (d)"          ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/f1_I[:]->C_SS/)                                  ; variable to be plotted from I
   netloss_I=(/f1_I[:]->WET_SS/)+(/f1_I[:]->DRY_SS/)          ; variable to be plotted from I
 else
   load_I=(/f1_I[:]->cb_SALT/)  + (/(f1_I[:]->cb_SS_A1_OCW)/) + (/(f1_I[:]->cb_SS_A2_OCW)/) + (/(f1_I[:]->cb_SS_A3_OCW)/) 
   wet_I=(/f1_I[:]->SS_A1SFWET/) + (/f1_I[:]->SS_A2SFWET/) + (/f1_I[:]->SS_A3SFWET/) + (/f1_I[:]->SS_A1_OCWSFWET/) + (/f1_I[:]->SS_A2_OCWSFWET/) + (/f1_I[:]->SS_A3_OCWSFWET/)
   wet_I=-wet_I
   dry_I=(/f1_I[:]->SS_A1DDF/) + (/f1_I[:]->SS_A2DDF/) + (/f1_I[:]->SS_A3DDF/) + (/f1_I[:]->SS_A1_OCWDDF/) + (/f1_I[:]->SS_A2_OCWDDF/) + (/f1_I[:]->SS_A3_OCWDDF/)
   netloss_I=-(wet_I+dry_I)
 end if
 load_II=(/f1_II[:]->cb_SALT/)  + (/(f1_II[:]->cb_SS_A1_OCW)/) + (/(f1_II[:]->cb_SS_A2_OCW)/) + (/(f1_II[:]->cb_SS_A3_OCW)/) 
 wet_II=(/f1_II[:]->SS_A1SFWET/) + (/f1_II[:]->SS_A2SFWET/) + (/f1_II[:]->SS_A3SFWET/) + (/f1_II[:]->SS_A1_OCWSFWET/) + (/f1_II[:]->SS_A2_OCWSFWET/) + (/f1_II[:]->SS_A3_OCWSFWET/)
 wet_II=-wet_II
 dry_II=(/f1_II[:]->SS_A1DDF/) + (/f1_II[:]->SS_A2DDF/) + (/f1_II[:]->SS_A3DDF/) + (/f1_II[:]->SS_A1_OCWDDF/) + (/f1_II[:]->SS_A2_OCWDDF/) + (/f1_II[:]->SS_A3_OCWDDF/)
 netloss_II=-(wet_II+dry_II)
 else if (plot_type.eq.5) then
 var="Lifetime_DUST"                      ; name of input-variable and plot
 varname="Dust lifetime (d)"              ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/f1_I[:]->C_DUST/)                                 ; variable to be plotted from I
   netloss_I=(/f1_I[:]->WET_DUST/)+(/f1_I[:]->DRY_DUST/)       ; variable to be plotted from I
 else
   load_I=(/f1_I[:]->cb_DUST/) + (/(f1_I[:]->cb_DST_A2_OCW)/) + (/(f1_I[:]->cb_DST_A3_OCW)/)
   wet_I=(/f1_I[:]->DST_A2SFWET/) + (/f1_I[:]->DST_A3SFWET/) + (/f1_I[:]->DST_A2_OCWSFWET/) + (/f1_I[:]->DST_A3_OCWSFWET/)
   wet_I=-wet_I
   dry_I=(/f1_I[:]->DST_A2DDF/) + (/f1_I[:]->DST_A3DDF/) + (/f1_I[:]->DST_A2_OCWDDF/) + (/f1_I[:]->DST_A3_OCWDDF/)
   netloss_I=-(wet_I+dry_I)
 end if
 load_II=(/f1_II[:]->cb_DUST/) + (/(f1_II[:]->cb_DST_A2_OCW)/) + (/(f1_II[:]->cb_DST_A3_OCW)/)
 wet_II=(/f1_II[:]->DST_A2SFWET/) + (/f1_II[:]->DST_A3SFWET/) + (/f1_II[:]->DST_A2_OCWSFWET/) + (/f1_II[:]->DST_A3_OCWSFWET/)
 wet_II=-wet_II
 dry_II=(/f1_II[:]->DST_A2DDF/) + (/f1_II[:]->DST_A3DDF/) + (/f1_II[:]->DST_A2_OCWDDF/) + (/f1_II[:]->DST_A3_OCWDDF/)
 netloss_II=-(wet_II+dry_II)
 end if
 end if
 end if
 end if
 end if
 end if
 end if
;for model 2, net loss is negative, so need to multiply by -1 before adding "small"
 lifetime_I=-load_I/(netloss_I+small)/3600.0/24.0      ; variable to be plotted from I
 lifetime_II=load_II/(-1.0*netloss_II+small)/3600.0/24.0   ; variable to be plotted from II 

; Calculating area weighted lifetimes

 lifetime_Ia=lifetime_I              ; initialization of global average variables
 lifetime_IIa=lifetime_II
 load_Ia=lifetime_I 
 load_IIa=lifetime_II
 netloss_Ia=lifetime_I
 netloss_IIa=lifetime_II

 xdims_I = dimsizes(gw0_I)
 ;print(xdims_I)
 ydims_I = dimsizes(lifetime_Ia)
 ;print(ydims_I)
 do i=0,dimsizes(gw0_I)-1
 lifetime_Ia(:,i,:)=lifetime_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 load_Ia(:,i,:)=load_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 netloss_Ia(:,i,:)=netloss_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 end do
 lifetimeave_I=sum(dim_avg_n(lifetime_Ia,0))/area1
 loadave_I=sum(dim_avg_n(load_Ia,0))/area1
 netlossave_I=sum(dim_avg_n(netloss_Ia,0))/area1  

 xdims_II = dimsizes(gw0_II)
 ;print(xdims_I)
 ydims_II = dimsizes(lifetime_IIa)
 ;print(ydims_II)
 do i=0,dimsizes(gw0_II)-1
 lifetime_IIa(:,i,:)=lifetime_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 load_IIa(:,i,:)=load_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 netloss_IIa(:,i,:)=netloss_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 end do
 lifetimeave_II=sum(dim_avg_n(lifetime_IIa,0))/area1
 loadave_II=sum(dim_avg_n(load_IIa,0))/area1
 netlossave_II=sum(dim_avg_n(netloss_IIa,0))/area1  

; Defining color scale
;  digg=(/0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95/)
;  digg=(/0.1,0.25,0.5,0.6,0.7,0.8,0.9,0.95,0.99/)
if (plot_type.eq.-1.or.plot_type.eq.4) then
  digg=(/0.2,0.3,0.5,1.0,1.5,2.0,3.0,5.0,7.0,10.0/)
else
  digg=(/1.0,1.5,2.0,3.0,5.0,7.0,10.0,20.0,30.0,50.0/)
end if
;;;;;;;;;;;;;;;;;;;;;;;;;
;
;    Make the Plot
;
;;;;;;;;;;;;;;;;;;;;;;;;;

  wks = gsn_open_wks(format,var)

  gsn_define_colormap(wks,"amwg_blueyellowred")
;  gsn_define_colormap(wks,"BlueDarkRed18")
;  gsn_define_colormap(wks,"precip2_15lev")
;  gsn_define_colormap(wks,"gui_default")
;  gsn_define_colormap(wks,"hotres")
  plot=new(2,graphic)
  
  res                   = True                  ; plot mods desired
  res@gsnSpreadColors   = False                 ; use full colormap
  res@mpFillOn          = False                 
  res@cnFillOn          = True                  ; color fill 
  res@cnLinesOn         = False                 ; no contour lines
  res@cnLineLabelsOn    = False                 
  res@gsnFrame          = False                 ; Do not draw plot
  res@gsnDraw           = False                 ; Do not advance frame
  res@lbLabelBarOn      = False 
  res@tmXBOn =False
  res@tmXTOn =False
  res@tmYLOn =False
  res@tmYROn =False
  res@cnMissingValFillPattern = 0
  res@cnMissingValFillColor = 16
  res@tiMainFontHeightF = 0.03
  res@tiMainFontThicknessF = 2
  res@txFontHeightF       = 0.02
  res@cnFillMode        = "RasterFill"  ; Turn on raster fill
  res@tiMainFont = "helvetica"
  res@tmYRMode             = "Automatic"
  res@cnInfoLabelOn        = False
  res@cnLevelSelectionMode      = "ExplicitLevels"       ; manual levels
;  res@cnFillColors   = (/3,4,5,6,7,8,9,0,10,11,12,13,14,15,16/)   ; gir hvitt midt i ?
;  res@cnFillColors   = (/2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/) 
  res@cnFillColors   = (/3,5,6,8,9,10,11,12,13,14,15,16/) 
;  res@cnLevels       =   sprintf("%4.1f",digg)             ; min level
  res@cnLevels       =   sprintf("%5.3f",digg)             ; min level

;  res@tiMainString   = "CAM4-Oslo"
  res@gsnRightString   = "avg = "+sprintf("%5.2f",lifetimeave_I)+" ("+sprintf("%4.2f",-loadave_I/(netlossave_I+small)/3600.0/24.0)+")"
  res@gsnLeftString   = varname
  plot(0) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(lifetime_I,0),res)    ; create the plot

;  res@tiMainString   = "CAM5-Oslo"
  res@gsnRightString  = "avg = "+sprintf("%5.2f",lifetimeave_II)+" ("+sprintf("%4.2f",-loadave_II/(netlossave_II+small)/3600.0/24.0)+")"
  res@gsnLeftString   = varname
  plot(1) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(lifetime_II,0),res)   ; create the plot

  pres                            = True        ; panel plot mods desired
;  pres@gsnMaximize                = True        ; fill the page
;  pres@txString   =   var
  pres@txFontHeightF              =0.015
  pres@txFontThicknessF              =2
  pres@gsnPanelLabelBar = True
  pres@lbLabelFontHeightF = 0.01
;  pres@lbOrientation        ="Vertical"

  gsn_panel(wks,plot,(/1,2/),pres)              ; create panel plot

end
