  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in 3d aerosol properties from two versions of 
; NorESM/CAM-Oslo and makes global plots of the zonally and annually 
; averaged variables. Note: This script is only correct when the model
; has been run in AEROCOM mode. Otherwise EAK (SSAVIS) and GAK (AYMMVIS) 
; has to be divided by DAYFOC (which is a bit cumbersome due to the 
; different number of dimensions, 3d divided by 2d). 

; !!!!! Try changing to p-coordinates by use of vinth2p function in ncl !!!!!

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
  plot_type = 2                      ; 0 => WAK    Single scattering albedo 
                                     ; 1 => GAK  Assymtery factor
                                     ; 2 => DUST   Dust mass mixing ratio
                                     ; 3 => SS     Sea-salt mass mixing ratio
                                     ; 4 => BC     BC mass mixing ratio
                                     ; 5 => OM     OM mass mixing ratio
                                     ; 6 => SO4    SO4 mass mixing ratio
                                     ; 7 => SO2    SO2 mass mixing ratio
; *************************************************************************
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

 if (plot_type.eq.0) then
 var="WAK"                          ; name of main input-variable
 varname="Single Scattering Albedo" ; variable name used in text string
 plot_name="SSA_Zonal"              ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
   var_I  = addfiles_GetVar(f1_I,all_files_I,var)
 else
 varCAM5Oslo="SSAVIS"
 var_I  = addfiles_GetVar(f1_I,all_files_I,varCAM5Oslo)
 end if
 varCAM5Oslo="SSAVIS"
 var_II  = addfiles_GetVar(f1_II,all_files_II,varCAM5Oslo)
 else if (plot_type.eq.1) then
 var="GAK"                          ; name of main input-variable 
 varname="Asymmetry Factor"         ; variable name used in text string
 plot_name="G_Zonal"                ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
   var_I  = addfiles_GetVar(f1_I,all_files_I,var)
 else
   varCAM5Oslo="ASYMMVIS"
   var_I  = addfiles_GetVar(f1_I,all_files_I,varCAM5Oslo)
 end if
 varCAM5Oslo="ASYMMVIS"
 var_II  = addfiles_GetVar(f1_II,all_files_II,varCAM5Oslo)
 else if (plot_type.eq.2) then
 var="DST"                          ; name of main input-variable 
 varname="Dust"                     ; variable name used in text string
 plot_name="DUST_Zonal"             ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
   var_I = addfiles_GetVar(f1_I,all_files_I,"DST_A2") + addfiles_GetVar(f1_I,all_files_I,"DST_A3")
   var_I = var_I*1.e12
 else
   var_I = addfiles_GetVar(f1_I,all_files_I,"DST_A2") + addfiles_GetVar(f1_I,all_files_I,"DST_A3") + addfiles_GetVar(f1_I,all_files_I,"DST_A2_OCW") + addfiles_GetVar(f1_I,all_files_I,"DST_A3_OCW")
   var_I = var_I*1.e12
 end if
 var_II = addfiles_GetVar(f1_II,all_files_II,"DST_A2") + addfiles_GetVar(f1_II,all_files_II,"DST_A3") + addfiles_GetVar(f1_II,all_files_II,"DST_A2_OCW") + addfiles_GetVar(f1_II,all_files_II,"DST_A3_OCW")
 var_II = var_II*1.e12
 else if (plot_type.eq.3) then
 var="SS"                           ; name of main input-variable 
 varname="Sea-salt"                 ; variable name used in text string
 plot_name="SS_Zonal"               ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
   var_I = addfiles_GetVar(f1_I,all_files_I,"SS_A1") + addfiles_GetVar(f1_I,all_files_I,"SS_A2") + addfiles_GetVar(f1_I,all_files_I,"SS_A3")
   var_I = var_I*1.e12
 else
 var_I = addfiles_GetVar(f1_I,all_files_I,"SS_A1") + addfiles_GetVar(f1_I,all_files_I,"SS_A2") + addfiles_GetVar(f1_I,all_files_I,"SS_A3") + addfiles_GetVar(f1_I,all_files_I,"SS_A1_OCW") + addfiles_GetVar(f1_I,all_files_I,"SS_A2_OCW") + addfiles_GetVar(f1_I,all_files_I,"SS_A3_OCW")
 var_I = var_I*1.e12
 end if
 var_II = addfiles_GetVar(f1_II,all_files_II,"SS_A1") + addfiles_GetVar(f1_II,all_files_II,"SS_A2") + addfiles_GetVar(f1_II,all_files_II,"SS_A3") + addfiles_GetVar(f1_II,all_files_II,"SS_A1_OCW") + addfiles_GetVar(f1_II,all_files_II,"SS_A2_OCW") + addfiles_GetVar(f1_II,all_files_II,"SS_A3_OCW")
 var_II = var_II*1.e12
 else if (plot_type.eq.4) then
 var="BC"                           ; name of main input-variable 
 varname="BC"                       ; variable name used in text string
 plot_name="BC_Zonal"               ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
   var_I = addfiles_GetVar(f1_I,all_files_I,"BC_A") + addfiles_GetVar(f1_I,all_files_I,"BC_AC") + addfiles_GetVar(f1_I,all_files_I,"BC_AX") + addfiles_GetVar(f1_I,all_files_I,"BC_AI") + addfiles_GetVar(f1_I,all_files_I,"BC_NI") + addfiles_GetVar(f1_I,all_files_I,"BC_N")
   var_I = var_I*1.e12
 else
var_I = addfiles_GetVar(f1_I,all_files_I,"BC_A") + addfiles_GetVar(f1_I,all_files_I,"BC_AC") + addfiles_GetVar(f1_I,all_files_I,"BC_AX") + addfiles_GetVar(f1_I,all_files_I,"BC_AI") + addfiles_GetVar(f1_I,all_files_I,"BC_NI") + addfiles_GetVar(f1_I,all_files_I,"BC_N") + addfiles_GetVar(f1_I,all_files_I,"BC_A_OCW") + addfiles_GetVar(f1_I,all_files_I,"BC_AC_OCW") + addfiles_GetVar(f1_I,all_files_I,"BC_AI_OCW") + addfiles_GetVar(f1_I,all_files_I,"BC_NI_OCW") + addfiles_GetVar(f1_I,all_files_I,"BC_N_OCW")
 var_I = var_I*1.e12
 end if 
 var_II = addfiles_GetVar(f1_II,all_files_II,"BC_A") + addfiles_GetVar(f1_II,all_files_II,"BC_AC") + addfiles_GetVar(f1_II,all_files_II,"BC_AX") + addfiles_GetVar(f1_II,all_files_II,"BC_AI") + addfiles_GetVar(f1_II,all_files_II,"BC_NI") + addfiles_GetVar(f1_II,all_files_II,"BC_N") + addfiles_GetVar(f1_II,all_files_II,"BC_A_OCW") + addfiles_GetVar(f1_II,all_files_II,"BC_AC_OCW") + addfiles_GetVar(f1_II,all_files_II,"BC_AI_OCW") + addfiles_GetVar(f1_II,all_files_II,"BC_NI_OCW") + addfiles_GetVar(f1_II,all_files_II,"BC_N_OCW")
 var_II = var_II*1.e12
 else if (plot_type.eq.5) then
 var="OM"                          ; name of main input-variable 
 varname="OM"                      ; variable name used in text string
 plot_name="OM_Zonal"              ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
   var_I = addfiles_GetVar(f1_I,all_files_I,"OM_AI") + addfiles_GetVar(f1_I,all_files_I,"OM_AC") + addfiles_GetVar(f1_I,all_files_I,"OM_NI") 
   var_I = var_I*1.e12
 else
   var_I = addfiles_GetVar(f1_I,all_files_I,"OM_AI") + addfiles_GetVar(f1_I,all_files_I,"OM_AC") + addfiles_GetVar(f1_I,all_files_I,"OM_NI") \
         + addfiles_GetVar(f1_I,all_files_I,"OM_AI_OCW") + addfiles_GetVar(f1_I,all_files_I,"OM_AC_OCW") + addfiles_GetVar(f1_I,all_files_I,"OM_NI_OCW") \
         + addfiles_GetVar(f1_I,all_files_I,"SOA_NA") + addfiles_GetVar(f1_I,all_files_I,"SOA_A1") \
         + addfiles_GetVar(f1_I,all_files_I,"SOA_NA_OCW") + addfiles_GetVar(f1_I,all_files_I,"SOA_A1_OCW")
   var_I = var_I*1.e12
 end if
 var_II = addfiles_GetVar(f1_II,all_files_II,"OM_AI") + addfiles_GetVar(f1_II,all_files_II,"OM_AC") + addfiles_GetVar(f1_II,all_files_II,"OM_NI") \
         + addfiles_GetVar(f1_II,all_files_II,"OM_AI_OCW") + addfiles_GetVar(f1_II,all_files_II,"OM_AC_OCW") + addfiles_GetVar(f1_II,all_files_II,"OM_NI_OCW") \
         + addfiles_GetVar(f1_II,all_files_II,"SOA_NA") + addfiles_GetVar(f1_II,all_files_II,"SOA_A1") \
         + addfiles_GetVar(f1_II,all_files_II,"SOA_NA_OCW") + addfiles_GetVar(f1_II,all_files_II,"SOA_A1_OCW") 
 var_II = var_II*1.e12
else if (plot_type.eq.6) then
 var="SO4"                          ; name of main input-variable 
 varname="SO4"                      ; variable name used in text string
 plot_name="SO4_Zonal"              ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
   var_I = addfiles_GetVar(f1_I,all_files_I,"SO4_A1") + addfiles_GetVar(f1_I,all_files_I,"SO4_A2") + addfiles_GetVar(f1_I,all_files_I,"SO4_AC") + addfiles_GetVar(f1_I,all_files_I,"SO4_N") + addfiles_GetVar(f1_I,all_files_I,"SO4_NA") + addfiles_GetVar(f1_I,all_files_I,"SO4_PR")
   var_I = var_I*1.e12
 else
var_I = addfiles_GetVar(f1_I,all_files_I,"SO4_A1")/3.06 + addfiles_GetVar(f1_I,all_files_I,"SO4_A2")/3.59 + addfiles_GetVar(f1_I,all_files_I,"SO4_AC")/3.06 + addfiles_GetVar(f1_I,all_files_I,"SO4_NA")/3.06 + addfiles_GetVar(f1_I,all_files_I,"SO4_PR")/3.06 + addfiles_GetVar(f1_I,all_files_I,"SO4_A1_OCW")/3.06 + addfiles_GetVar(f1_I,all_files_I,"SO4_A2_OCW")/3.59 + addfiles_GetVar(f1_I,all_files_I,"SO4_AC_OCW")/3.06 + addfiles_GetVar(f1_I,all_files_I,"SO4_NA_OCW")/3.06 + addfiles_GetVar(f1_I,all_files_I,"SO4_PR_OCW")/3.06
 var_I = var_I*1.e12
 end if
 var_II = addfiles_GetVar(f1_II,all_files_II,"SO4_A1")/3.06 + addfiles_GetVar(f1_II,all_files_II,"SO4_A2")/3.59 + addfiles_GetVar(f1_II,all_files_II,"SO4_AC")/3.06 + addfiles_GetVar(f1_II,all_files_II,"SO4_NA")/3.06 + addfiles_GetVar(f1_II,all_files_II,"SO4_PR")/3.06 + addfiles_GetVar(f1_II,all_files_II,"SO4_A1_OCW")/3.06 + addfiles_GetVar(f1_II,all_files_II,"SO4_A2_OCW")/3.59 + addfiles_GetVar(f1_II,all_files_II,"SO4_AC_OCW")/3.06 + addfiles_GetVar(f1_II,all_files_II,"SO4_NA_OCW")/3.06 + addfiles_GetVar(f1_II,all_files_II,"SO4_PR_OCW")/3.06
 var_II = var_II*1.e12
else if (plot_type.eq.7) then
 var="SO2"                          ; name of main input-variable 
 varname="SO2"                      ; variable name used in text string
 plot_name="SO2_Zonal"              ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then 
  var_I = addfiles_GetVar(f1_I,all_files_I,"SO2") 
  var_I = var_I*1.e12
 else 
  var_I = addfiles_GetVar(f1_I,all_files_I,"SO2") 
  var_I = var_I*1.e12/1.998
 end if
 var_II = addfiles_GetVar(f1_II,all_files_II,"SO2") 
 var_II = var_II*1.e12/1.998

; conversion from mol(SO2)/mol to kg(SO2)/kg with the new code
 if(ModI.eq."CAM5-Oslo") then   
 var_I = var_I*66.066/28.9647
 end if
 var_II = var_II*66.066/28.9647

 end if
 end if
 end if
 end if
 end if
 end if
 end if
 end if
; printVarSummary(var_I)
; printVarSummary(var_II)

lat_I = f0_I->lat                                  ; pull lat off file
lat_II = f0_II->lat                                ; pull lat off file
;************************************************
;  calculate eta
;************************************************
   a=f0_I->hyam                                    ; select hyam 
   b=f0_I->hybm                                    ; select hybm
   p=f0_I->P0                                      ; select P0
   eta = (a+b)*p                                   ; calc eta
   eta_I = eta/100                                 ; scale eta by 100
   a_II=f0_II->hyam                                ; select hyam 
   b_II=f0_II->hybm                                ; select hybm
   p_II=f0_II->P0                                  ; select P0
   eta_II = (a_II+b_II)*p                          ; calc eta
   eta_II = eta_II/100                             ; scale eta by 100

 zave_I = dim_avg_Wrap(var_I)    ; calculate zonal ave
 zave_II = dim_avg_Wrap(var_II)  ; calculate zonal ave
; printVarSummary(zave_I)
; printVarSummary(zave_II)

; Defining color scales for each meteorology variable
if (var .eq. "WAK") then
 digg=(/0.5,0.6,0.7,0.8,0.85,0.9,0.95,0.98,0.99,0.995/)
  else if (var .eq. "GAK") then
   digg=(/0.6,0.62,0.64,0.68,0.7,0.72,0.74,0.76,0.78/)
   else if (var .eq. "DST" .or. var .eq. "SS") then
    digg=(/25,50,100,250,500,1000,2500,5000,10000,25000/)
     else if (var .eq. "BC" .or. var .eq. "OM" .or. var .eq. "SO4" .or. var .eq. "SO2") then
      digg=(/2.5,5,10,25,50,100,250,500,1000,2500/)
       else
       digg=(/0.0,1.0/)                         ; Replace with error message
     end if
    end if
  end if
end if

;;;;;;;;;;;;;;;;;;;;;;;;;
;
;    Make the Plot
;
;;;;;;;;;;;;;;;;;;;;;;;;;

;  wks = gsn_open_wks(format,var)
  wks = gsn_open_wks(format,plot_name)

  gsn_define_colormap(wks,"amwg_blueyellowred")
;  gsn_define_colormap(wks,"BlueDarkRed18")
;  gsn_define_colormap(wks,"precip2_15lev")
;  gsn_define_colormap(wks,"gui_default")
;  gsn_define_colormap(wks,"hotres")
  plot=new(2,graphic)
  
  res                   = True                  ; plot mods desired
  res@gsnSpreadColors   = False                 ; use full colormap
;  res@mpFillOn          = False                 
  res@cnFillOn          = True                  ; color fill 
  res@cnLinesOn         = False                 ; no contour lines
  res@cnLineLabelsOn    = False                 
  res@gsnFrame          = False                 ; Do not draw plot
  res@gsnDraw           = False                 ; Do not advance frame
;  res@lbLabelBarOn      = False 
;  res@tmXBOn =False
;  res@tmXTOn =False
;  res@tmYLOn =False
;  res@tmYROn =False
  res@cnMissingValFillPattern = 0
  res@cnMissingValFillColor = 16
  res@tiMainFontHeightF = 0.03
  res@tiMainFontThicknessF = 2
;  res@txFontHeightF       = 0.02
;  res@cnFillMode        = "RasterFill"  ; Turn on raster fill
  res@tiMainFont = "helvetica"
  res@tmYRMode             = "Automatic"
  res@cnInfoLabelOn        = False
  res@cnLevelSelectionMode      = "ExplicitLevels"       ; manual levels

  res@sfYArray              = eta_I                  ; use eta for y axis
  res@sfXArray              = lat_I                  ; use lat for x axis
  res@tiXAxisString         = "latitude"           ; x-axis label
  res@tiYAxisString         = "eta x 1000"         ; y-axis label
  res@trXReverse            = False                ; reverse x-axis
  res@trYReverse            = True                 ; reverse y-axis
;  res@gsnYAxisIrregular2Log = True                 ; set y-axis to log scale

  res@cnFillColors   = (/3,5,6,8,9,10,11,12,13,14,15,16/) 
  res@cnLevels       =   sprintf("%7.5f",digg)             ; min level

  res2                   = True                  ; plot mods desired
  res2@gsnSpreadColors   = False                 ; use full colormap
;  res2@mpFillOn          = False                 
  res2@cnFillOn          = True                  ; color fill 
  res2@cnLinesOn         = False                 ; no contour lines
  res2@cnLineLabelsOn    = False                 
  res2@gsnFrame          = False                 ; Do not draw plot
  res2@gsnDraw           = False                 ; Do not advance frame
;  res2@lbLabelBarOn      = False 
;  res2@tmXBOn =False
;  res2@tmXTOn =False
;  res2@tmYLOn =False
;  res2@tmYROn =False
  res2@cnMissingValFillPattern = 0
  res2@cnMissingValFillColor = 16
  res2@tiMainFontHeightF = 0.03
  res2@tiMainFontThicknessF = 2
;  res2@txFontHeightF       = 0.02
;  res2@cnFillMode        = "RasterFill"  ; Turn on raster fill
  res2@tiMainFont = "helvetica"
  res2@tmYRMode             = "Automatic"
  res2@cnInfoLabelOn        = False
  res2@cnLevelSelectionMode      = "ExplicitLevels"       ; manual levels

  res2@sfYArray              = eta_II                  ; use eta for y axis
  res2@sfXArray              = lat_II                  ; use lat for x axis
  res2@tiXAxisString         = "latitude"           ; x-axis label
  res2@tiYAxisString         = "eta x 1000"         ; y-axis label
  res2@trXReverse            = False                ; reverse x-axis
  res2@trYReverse            = True                 ; reverse y-axis
;  res2@gsnYAxisIrregular2Log = True                 ; set y-axis to log scale

  res2@cnFillColors   = (/3,5,6,8,9,10,11,12,13,14,15,16/) 
  res2@cnLevels       =   sprintf("%7.5f",digg)             ; min level

if (var .eq. "WAK") then
  res@tiMainString  = "Single Scattering Albedo"
else if (var .eq. "GAK") then
  res@tiMainString  = "Asymmetry Factor"
else if (var .eq. "DST") then
  res@tiMainString  = "Dust (ng kg~S~-1~N~)"
else if (var .eq. "SS") then
  res@tiMainString  = "Sea-salt (ng kg~S~-1~N~)"
else if (var .eq. "BC") then
  res@tiMainString  = "BC (ng kg~S~-1~N~)"
else if (var .eq. "OM") then
  res@tiMainString  = "OM (ng kg~S~-1~N~)"
else if (var .eq. "SO4") then
  res@tiMainString  = "SO4 (ng S kg~S~-1~N~)"
else if (var .eq. "SO2") then
  res@tiMainString  = "SO2 (ng S kg~S~-1~N~)"
end if
end if
end if
end if
end if
end if
end if
end if
 
 plot(0) = gsn_contour(wks,dim_avg_n_Wrap(zave_I,0),res)    ; create the plot

if (var .eq. "WAK") then
  res2@tiMainString  = "Single Scattering Albedo"
else if (var .eq. "GAK") then
  res2@tiMainString  = "Asymmetry Factor"
else if (var .eq. "DST") then
  res2@tiMainString  = "Dust (ng kg~S~-1~N~)"
else if (var .eq. "SS") then
  res2@tiMainString  = "Sea-salt (ng kg~S~-1~N~)"
else if (var .eq. "BC") then
  res2@tiMainString  = "BC (ng kg~S~-1~N~)"
else if (var .eq. "OM") then
  res2@tiMainString  = "OM (ng kg~S~-1~N~)"
else if (var .eq. "SO4") then
  res2@tiMainString  = "SO4 (ng S kg~S~-1~N~)"
else if (var .eq. "SO2") then
  res2@tiMainString  = "SO2 (ng S kg~S~-1~N~)"
end if
end if
end if
end if
end if
end if
end if
end if

  plot(1) = gsn_contour(wks,dim_avg_n_Wrap(zave_II,0),res2)    ; create the plot

  pres                            = True        ; panel plot mods desired
;  pres@gsnMaximize                = True        ; fill the page
;  pres@txString   =   var
  pres@txFontHeightF              =0.015
  pres@txFontThicknessF              =2
  pres@gsnPanelLabelBar = True
  pres@lbLabelFontHeightF = 0.01
  pres@lbOrientation        ="Vertical"

  gsn_panel(wks,plot,(/1,2/),pres)              ; create panel plot

end

