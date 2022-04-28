  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in aerosol wet and dry deposition from two versions 
; of NorESM/CAM-Oslo and makes global plots of the annually averaged ratio
; wet/(wet+dry), including global average as a number in the title line for 
; each figure. The global average is be calculated both as area wet/(wet+dry)
; and as area averaged wet / area averaged (wet+dry), with the latter value 
; shown in brackets.

; Model independent constants
 g=9.80665
 pi=3.1415926
 re=6378.39e3            ; earth radius in m
 coffa=pi*re^2./180.      
 area1=4.*pi*re^2          

; *************************************************************************
; **** To be edited by the user if the ncl script is run interactively ****
;
; Define plot type and plot output format
 if (.not. isvar("plot_type")) then      ; is plot_type on command line?
  plot_type = 5                      ; 0 => SO2 wet/(wet+dry) deposition
                                     ; 1 => SO4 wet/(wet+dry) deposition
                                     ; 2 => BC  wet/(wet+dry) deposition  
                                     ; 3 => POM wet/(wet+dry) deposition
                                     ; 4 => SS  wet/(wet+dry) deposition
                                     ; 5 => DU  wet/(wet+dry) deposition
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
 wratio_I=tmp_I
 wratio_II=tmp_II
 wet_I=tmp_I
 wet_II=tmp_II
 wetndry_I=tmp_I
 wetndry_II=tmp_II

 if (plot_type.eq.0) then
 var="WET_SO2"                             ; name of input-variable and plot
 varname="SO~B~2~N~ wet dep. ratio"        ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   wet_I=(/f1_I[:]->WET_SO2/)                                 ; variable to be plotted from I
   wetndry_I=(/f1_I[:]->WET_SO2/)+(/f1_I[:]->DRY_SO2/)        ; variable to be plotted from I
 else
   wet_I=(/f1_I[:]->WD_A_SO2/)                               ; variable to be plotted from I 
   wetndry_I=(/f1_I[:]->WD_A_SO2/)+(/f1_I[:]->DF_SO2/)     ; variable to be plotted from I 
 end if
 wet_II=(/f1_II[:]->WD_A_SO2/)                               ; variable to be plotted from II 
 wetndry_II=(/f1_II[:]->WD_A_SO2/)+(/f1_II[:]->DF_SO2/)     ; variable to be plotted from II 
 else if (plot_type.eq.1) then
 var="WET_SO4"                             ; name of input-variable and plot
 varname="SO~B~4~N~ wet dep. ratio"        ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   wet_I=(/f1_I[:]->WET_SO4/)                                 ; variable to be plotted from I
   wetndry_I=(/f1_I[:]->WET_SO4/)+(/f1_I[:]->DRY_SO4/)        ; variable to be plotted from I
  else
   wet_I=(/f1_I[:]->SO4_A1SFWET/) + (/f1_I[:]->SO4_A2SFWET/) + (/f1_I[:]->SO4_ACSFWET/) + (/f1_I[:]->SO4_NASFWET/) + (/f1_I[:]->SO4_PRSFWET/) + (/f1_I[:]->SO4_A1_OCWSFWET/) + (/f1_I[:]->SO4_A2_OCWSFWET/) + (/f1_I[:]->SO4_AC_OCWSFWET/) + (/f1_I[:]->SO4_NA_OCWSFWET/) + (/f1_I[:]->SO4_PR_OCWSFWET/)
 wet_I=-wet_I
 wetndry_I=wet_I + (/f1_I[:]->SO4_A1DDF/) + (/f1_I[:]->SO4_A2DDF/) + (/f1_I[:]->SO4_ACDDF/) + (/f1_I[:]->SO4_NADDF/) + (/f1_I[:]->SO4_PRDDF/) + (/f1_I[:]->SO4_A1_OCWDDF/) + (/f1_I[:]->SO4_A2_OCWDDF/) + (/f1_I[:]->SO4_AC_OCWDDF/) + (/f1_I[:]->SO4_NA_OCWDDF/) + (/f1_I[:]->SO4_PR_OCWDDF/)
  end if
 wet_II=(/f1_II[:]->SO4_A1SFWET/) + (/f1_II[:]->SO4_A2SFWET/) + (/f1_II[:]->SO4_ACSFWET/) + (/f1_II[:]->SO4_NASFWET/) + (/f1_II[:]->SO4_PRSFWET/) + (/f1_II[:]->SO4_A1_OCWSFWET/) + (/f1_II[:]->SO4_A2_OCWSFWET/) + (/f1_II[:]->SO4_AC_OCWSFWET/) + (/f1_II[:]->SO4_NA_OCWSFWET/) + (/f1_II[:]->SO4_PR_OCWSFWET/)
 wet_II=-wet_II
 wetndry_II=wet_II + (/f1_II[:]->SO4_A1DDF/) + (/f1_II[:]->SO4_A2DDF/) + (/f1_II[:]->SO4_ACDDF/) + (/f1_II[:]->SO4_NADDF/) + (/f1_II[:]->SO4_PRDDF/) + (/f1_II[:]->SO4_A1_OCWDDF/) + (/f1_II[:]->SO4_A2_OCWDDF/) + (/f1_II[:]->SO4_AC_OCWDDF/) + (/f1_II[:]->SO4_NA_OCWDDF/) + (/f1_II[:]->SO4_PR_OCWDDF/)
 else if (plot_type.eq.2) then
 var="WET_BC"                              ; name of input-variable and plot
 varname="BC wet dep. ratio"       ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   wet_I=(/f1_I[:]->WET_BC/)                                  ; variable to be plotted from I
   wetndry_I=(/f1_I[:]->WET_BC/)+(/f1_I[:]->DRY_BC/)          ; variable to be plotted from I
 else
   wet_I=(/f1_I[:]->BC_ASFWET/) + (/f1_I[:]->BC_ACSFWET/) + (/f1_I[:]->BC_AXSFWET/) + (/f1_I[:]->BC_AISFWET/) + (/f1_I[:]->BC_NISFWET/) + (/f1_I[:]->BC_NSFWET/) + (/f1_I[:]->BC_A_OCWSFWET/) + (/f1_I[:]->BC_AC_OCWSFWET/) + (/f1_I[:]->BC_AI_OCWSFWET/) + (/f1_I[:]->BC_NI_OCWSFWET/) + (/f1_I[:]->BC_N_OCWSFWET/)
   wet_I=-wet_I
   wetndry_I=wet_I + (/f1_I[:]->BC_ADDF/) + (/f1_I[:]->BC_ACDDF/) + (/f1_I[:]->BC_AXDDF/) + (/f1_I[:]->BC_AIDDF/) + (/f1_I[:]->BC_NIDDF/) + (/f1_I[:]->BC_NDDF/) + (/f1_I[:]->BC_A_OCWDDF/) + (/f1_I[:]->BC_AC_OCWDDF/) + (/f1_I[:]->BC_AI_OCWDDF/) + (/f1_I[:]->BC_NI_OCWDDF/) + (/f1_I[:]->BC_N_OCWDDF/)
 end if
 wet_II=(/f1_II[:]->BC_ASFWET/) + (/f1_II[:]->BC_ACSFWET/) + (/f1_II[:]->BC_AXSFWET/) + (/f1_II[:]->BC_AISFWET/) + (/f1_II[:]->BC_NISFWET/) + (/f1_II[:]->BC_NSFWET/) + (/f1_II[:]->BC_A_OCWSFWET/) + (/f1_II[:]->BC_AC_OCWSFWET/) + (/f1_II[:]->BC_AI_OCWSFWET/) + (/f1_II[:]->BC_NI_OCWSFWET/) + (/f1_II[:]->BC_N_OCWSFWET/)
 wet_II=-wet_II
 wetndry_II=wet_II + (/f1_II[:]->BC_ADDF/) + (/f1_II[:]->BC_ACDDF/) + (/f1_II[:]->BC_AXDDF/) + (/f1_II[:]->BC_AIDDF/) + (/f1_II[:]->BC_NIDDF/) + (/f1_II[:]->BC_NDDF/) + (/f1_II[:]->BC_A_OCWDDF/) + (/f1_II[:]->BC_AC_OCWDDF/) + (/f1_II[:]->BC_AI_OCWDDF/) + (/f1_II[:]->BC_NI_OCWDDF/) + (/f1_II[:]->BC_N_OCWDDF/)
 else if (plot_type.eq.3) then
 var="WET_POM"                             ; name of input-variable and plot
 varname="POM wet dep. ratio"      ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   wet_I=(/f1_I[:]->WET_POM/)                                 ; variable to be plotted from I
   wetndry_I=(/f1_I[:]->WET_POM/)+(/f1_I[:]->DRY_POM/)        ; variable to be plotted from I
 else
   wet_I=(/f1_I[:]->OM_AISFWET/) + (/f1_I[:]->OM_ACSFWET/) + (/f1_I[:]->OM_NISFWET/)+ (/f1_I[:]->SOA_A1SFWET/) +  (/f1_I[:]->SOA_NASFWET/) + \
         (/f1_I[:]->OM_AI_OCWSFWET/) + (/f1_I[:]->OM_AC_OCWSFWET/) + (/f1_I[:]->OM_NI_OCWSFWET/) + \
         (/f1_I[:]->SOA_A1_OCWSFWET/) +  (/f1_I[:]->SOA_NA_OCWSFWET/)
   wet_I=-wet_I
   wetndry_I= wet_I + (/f1_I[:]->OM_AIDDF/) + (/f1_I[:]->OM_ACDDF/) + (/f1_I[:]->OM_NIDDF/) + (/f1_I[:]->SOA_A1DDF/) +  (/f1_I[:]->SOA_NADDF/) +\ 
          (/f1_I[:]->OM_AI_OCWDDF/) + (/f1_I[:]->OM_AC_OCWDDF/) + (/f1_I[:]->OM_NI_OCWDDF/) + \
          (/f1_I[:]->SOA_A1_OCWDDF/) +  (/f1_I[:]->SOA_NA_OCWDDF/)
 end if
 wet_II=(/f1_II[:]->OM_AISFWET/) + (/f1_II[:]->OM_ACSFWET/) + (/f1_II[:]->OM_NISFWET/) +  (/f1_II[:]->SOA_A1SFWET/) +  (/f1_II[:]->SOA_NASFWET/) + \
         (/f1_II[:]->OM_AI_OCWSFWET/) + (/f1_II[:]->OM_AC_OCWSFWET/) + (/f1_II[:]->OM_NI_OCWSFWET/) + \
         (/f1_II[:]->SOA_A1_OCWSFWET/) +  (/f1_II[:]->SOA_NA_OCWSFWET/)
 wet_II=-wet_II
 wetndry_II= wet_II + (/f1_II[:]->OM_AIDDF/) + (/f1_II[:]->OM_ACDDF/) + (/f1_II[:]->OM_NIDDF/) + (/f1_II[:]->SOA_A1DDF/) + (/f1_II[:]->SOA_NADDF/) +\
         (/f1_II[:]->OM_AI_OCWDDF/) + (/f1_II[:]->OM_AC_OCWDDF/) + (/f1_II[:]->OM_NI_OCWDDF/) + \
         (/f1_II[:]->SOA_A1_OCWDDF/) +  (/f1_II[:]->SOA_NA_OCWDDF/)
 else if (plot_type.eq.4) then
 var="WET_SS"                              ; name of input-variable and plot
 varname="Sea-salt wet dep. ratio" ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   wet_I=(/f1_I[:]->WET_SS/)                                  ; variable to be plotted from I
   wetndry_I=(/f1_I[:]->WET_SS/)+(/f1_I[:]->DRY_SS/)          ; variable to be plotted from I
 else
   wet_I=(/f1_I[:]->SS_A1SFWET/) + (/f1_I[:]->SS_A2SFWET/) + (/f1_I[:]->SS_A3SFWET/) + (/f1_I[:]->SS_A1_OCWSFWET/) + (/f1_I[:]->SS_A2_OCWSFWET/) + (/f1_I[:]->SS_A3_OCWSFWET/)
   wet_I=-wet_I
   wetndry_I=wet_I + (/f1_I[:]->SS_A1DDF/) + (/f1_I[:]->SS_A2DDF/) + (/f1_I[:]->SS_A3DDF/) + (/f1_I[:]->SS_A1_OCWDDF/) + (/f1_I[:]->SS_A2_OCWDDF/) + (/f1_I[:]->SS_A3_OCWDDF/)
 end if
 wet_II=(/f1_II[:]->SS_A1SFWET/) + (/f1_II[:]->SS_A2SFWET/) + (/f1_II[:]->SS_A3SFWET/) + (/f1_II[:]->SS_A1_OCWSFWET/) + (/f1_II[:]->SS_A2_OCWSFWET/) + (/f1_II[:]->SS_A3_OCWSFWET/)
 wet_II=-wet_II
 wetndry_II=wet_II + (/f1_II[:]->SS_A1DDF/) + (/f1_II[:]->SS_A2DDF/) + (/f1_II[:]->SS_A3DDF/) + (/f1_II[:]->SS_A1_OCWDDF/) + (/f1_II[:]->SS_A2_OCWDDF/) + (/f1_II[:]->SS_A3_OCWDDF/)
 else if (plot_type.eq.5) then
 var="WET_DUST"                            ; name of input-variable and plot
 varname="Dust wet dep.ratio"     ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   wet_I=(/f1_I[:]->WET_DUST/)                                 ; variable to be plotted from I
   wetndry_I=(/f1_I[:]->WET_DUST/)+(/f1_I[:]->DRY_DUST/)       ; variable to be plotted from I
 else
   wet_I= (/f1_I[:]->DST_A2SFWET/) + (/f1_I[:]->DST_A3SFWET/) + (/f1_I[:]->DST_A2_OCWSFWET/) + (/f1_I[:]->DST_A3_OCWSFWET/)
   wet_I=-wet_I
   wetndry_I=wet_I + (/f1_I[:]->DST_A2DDF/) + (/f1_I[:]->DST_A3DDF/) + (/f1_I[:]->DST_A2_OCWDDF/) + (/f1_I[:]->DST_A3_OCWDDF/)
 end if
 wet_II= (/f1_II[:]->DST_A2SFWET/) + (/f1_II[:]->DST_A3SFWET/) + (/f1_II[:]->DST_A2_OCWSFWET/) + (/f1_II[:]->DST_A3_OCWSFWET/)
 wet_II=-wet_II
 wetndry_II=wet_II + (/f1_II[:]->DST_A2DDF/) + (/f1_II[:]->DST_A3DDF/) + (/f1_II[:]->DST_A2_OCWDDF/) + (/f1_II[:]->DST_A3_OCWDDF/)
 end if
 end if
 end if
 end if
 end if
 end if
 wratio_I=wet_I/wetndry_I      ; variable to be plotted from I
 wratio_II=wet_II/wetndry_II   ; variable to be plotted from II 

; Calculating area weighted extinctions

 wratio_Ia=wratio_I              ; initialization of global average variables
 wratio_IIa=wratio_II
 wet_Ia=wratio_I 
 wet_IIa=wratio_II
 wetndry_Ia=wratio_I
 wetndry_IIa=wratio_II

 xdims_I = dimsizes(gw0_I)
 ;print(xdims_I)
 ydims_I = dimsizes(wratio_Ia)
 ;print(ydims_I)
 do i=0,dimsizes(gw0_I)-1
 wratio_Ia(:,i,:)=wratio_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 wet_Ia(:,i,:)=wet_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 wetndry_Ia(:,i,:)=wetndry_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 end do
 wratioave_I=sum(dim_avg_n(wratio_Ia,0))/area1
 wetave_I=sum(dim_avg_n(wet_Ia,0))/area1
 wetndryave_I=sum(dim_avg_n(wetndry_Ia,0))/area1  

 xdims_II = dimsizes(gw0_II)
 ;print(xdims_I)
 ydims_II = dimsizes(wratio_IIa)
 ;print(ydims_II)
 do i=0,dimsizes(gw0_II)-1
 wratio_IIa(:,i,:)=wratio_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 wet_IIa(:,i,:)=wet_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 wetndry_IIa(:,i,:)=wetndry_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 end do
 wratioave_II=sum(dim_avg_n(wratio_IIa,0))/area1
 wetave_II=sum(dim_avg_n(wet_IIa,0))/area1
 wetndryave_II=sum(dim_avg_n(wetndry_IIa,0))/area1  

; Defining color scale
;  digg=(/0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95/)
  digg=(/0.1,0.25,0.5,0.6,0.7,0.8,0.9,0.95,0.99/)

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
  res@gsnRightString   = "avg = "+sprintf("%5.2f",wratioave_I)+" ("+sprintf("%4.2f",wetave_I/wetndryave_I)+")"
  res@gsnLeftString   = varname
  plot(0) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(wratio_I,0),res)    ; create the plot

;  res@tiMainString   = "CAM5-Oslo"
  res@gsnRightString  = "avg = "+sprintf("%5.2f",wratioave_II)+" ("+sprintf("%4.2f",wetave_II/wetndryave_II)+")"
  res@gsnLeftString   = varname
  plot(1) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(wratio_II,0),res)   ; create the plot

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
