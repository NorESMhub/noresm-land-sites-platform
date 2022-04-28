  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in 3d cloud cover or ambient relative humidity or liquid
; or ice water content from two versions of NorESM/CAM-Oslo and makes global plots 
; of the zonally and annually averaged variables. 

; Model independent constants
 g=9.80665
 pi=3.1415926
 re=6378.39e3            ; earth radius in m
 coffa=pi*re^2./180.      
 area1=4.*pi*re^2          
 small=1.0e-15           ; small number

; *************************************************************************
; **** To be edited by the user if the ncl script is run interactively ****
;
; Define plot type and plot output format
 if (.not. isvar("plot_type")) then      ; is plot_type on command line?
  plot_type = 4                      ; 0 => CLOUD   Cloud fraction
                                     ; 1 => RH      Relative humidity RELHUM
                                     ; 2 => CLDLIQ  Cloud liquid amount 
                                     ; 3 => CLDICE  Cloud ice amount
                                     ; 4 => RHW     Relative humidity RHW
                                     ; 5 => CDNC    Cloud droplet number concentration
                                     ; 6 => REFFL   Cloud droplet effective radius
                                     ; 7 => ICNC    Ice crystal number concentration
                                     ; 8 => REFFL vs SPREFFL Cloud droplet effective radius
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
 var="CLOUD"                     ; name of input-variable
 varname="CLOUD"                 ; variable name used in text string
 plot_name="CLOUD_Zonal"         ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)
 else if (plot_type.eq.1) then
 var="RELHUM"                    ; name of input-variable and plot
 varname="RH"                    ; variable name used in text string
 plot_name="RELHUM_Zonal"        ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)
 else if (plot_type.eq.2) then
 var="CLDLIQ"                    ; name of input-variable and plot
 varname="Cloud liquid amount"   ; variable name used in text string
 plot_name="CLDLIQ_Zonal"        ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.e6
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.e6
 else if (plot_type.eq.3) then
 var="CLDICE"                    ; name of input-variable and plot
 varname="Cloud ice amount"      ; variable name used in text string
 plot_name="CLDICE_Zonal"        ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.e6
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.e6
 else if (plot_type.eq.4) then
 var="RELHUM"                    ; name of input-variable and plot
 varname="RH"                    ; variable name used in text string
 plot_name="RHW_Zonal"           ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I  = addfiles_GetVar(f1_I,all_files_I,var)
 else
  var_I  = addfiles_GetVar(f1_I,all_files_I,"RHW")
 end if
 var_II  = addfiles_GetVar(f1_II,all_files_II,"RHW")
 else if (plot_type.eq.5) then
 var="CDNC"                      ; name of plot
 varname="CDNC"                  ; variable name used in text string
 plot_name="CDNC_Zonal"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->CDNC)/)/((/(f1_I[:]->CLDFOC)/)+small)  ; variable to be plotted from I
 else
  var_I=1.e-6*(/(f1_I[:]->AWNC)/)/((/(f1_I[:]->FREQL)/)+small) ; variable to be plotted from II  
 end if
 var_II=1.e-6*(/(f1_II[:]->AWNC)/)/((/(f1_II[:]->FREQL)/)+small) ; variable to be plotted from II 
 else if (plot_type.eq.6) then
 var="REFFL"                      ; name of plot
 varname="REFFL"                  ; variable name used in text string
 plot_name="REFFL_Zonal"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->REFFL)/)/((/(f1_I[:]->CLDFOC)/)+small)  ; variable to be plotted from I
 else
  var_I=(/(f1_I[:]->AREL)/)/((/(f1_I[:]->FREQL)/)+small) ; variable to be plotted from II 
 end if
 var_II=(/(f1_II[:]->AREL)/)/((/(f1_II[:]->FREQL)/)+small) ; variable to be plotted from II 
 else if (plot_type.eq.7) then
 var="ICNC"                      ; name of plot
 varname="ICNC"                  ; variable name used in text string
 plot_name="ICNC_Zonal"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->CDNC)/)*0.0                            ; variable to be plotted from I
 else
  var_I=1.e-6*(/(f1_I[:]->AWNI)/)/((/(f1_I[:]->FREQI)/)+small)   ; variable to be plotted from II  
 end if
 var_II=1.e-6*(/(f1_II[:]->AWNI)/)/((/(f1_II[:]->FREQI)/)+small) ; variable to be plotted from II 

 else if (plot_type.eq.8) then
 var="SPREFFL"                      ; name of plot
 varname="REFFL"                  ; variable name used in text string
 plot_name="SPREFFL_Zonal"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->REFFL)/)/((/(f1_I[:]->CLDFOC)/)+small)  ; variable to be plotted from I
 else
  var_I=(/(f1_I[:]->AREL)/)/((/(f1_I[:]->FREQL)/)+small) ; variable to be plotted from II 
 end if
 var_II=(/(f1_II[:]->SPAREL)/)/((/(f1_II[:]->FREQL)/)+small) ; variable to be plotted from II 

 end if
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
if (var.eq."CLOUD") then
 digg=(/0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55/)
 else if (var .eq. "RELHUM") then
  digg=(/10,25,40,50,60,70,80,90,95,100/)
   else if (var .eq. "CLDLIQ") then
    digg=(/0.1,1,2,3,5,10,20,30,50,100/)
     else if (var .eq. "CLDICE") then
      digg=(/0.01,0.1,0.2,0.3,0.5,1,2,3,5,10/)
       else if (var .eq. "CDNC") then
        digg=(/0.1,1,2,3,5,10,20,30,50,100/)
         else if (var .eq. "REFFL" .or. var .eq. "SPREFFL") then
          digg=(/0.1,0.5,1,1.5,2,3,5,10,15,20/)
         else
           digg=(/0.0005,0.001,0.01,0.02,0.03,0.05,.1,.2,.3,.5/)
         end if
       end if
      end if
    end if
  end if
end if

;;;;;;;;;;;;;;;;;;;;;;;;;
;
;    Make the Plot
;
;;;;;;;;;;;;;;;;;;;;;;;;;

;if (plot_type.eq.4) then
;  wks = gsn_open_wks(format,"RHW")
;else
;  wks = gsn_open_wks(format,var)
  wks = gsn_open_wks(format,plot_name)
;end if

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

if (var .eq. "CLOUD") then
  res@tiMainString  = "Cloud Fraction"
else if (var .eq. "RELHUM") then
 if (plot_type.eq.4) then
  if(ModI.eq."CAM4-Oslo") then
   res@tiMainString  = "Relative Humidity RELHUM (%)"
  else
   res@tiMainString  = "Relative Humidity RHW (%)"
  end if
 else
  if(ModI.eq."CAM4-Oslo") then
   res@tiMainString  = "Relative Humidity RELHUM (%)"
  else
   res@tiMainString  = "Relative Humidity RELHUM (%)" 
  end if
 end if
else if (var .eq. "CLDLIQ") then
  res@tiMainString  = "Cloud Liquid Water (mg kg~S~-1~N~)"
else if (var .eq. "CLDICE") then
  res@tiMainString  = "Cloud Ice Water (mg kg~S~-1~N~)"
else if (var .eq. "CDNC") then
 if(ModI.eq."CAM4-Oslo") then
  res@tiMainString  = "CDNC (CDNC/CLDFOC) (cm~S~-3~N~)"
 else
  res@tiMainString  = "CDNC (AWNC/FREQL) (cm~S~-3~N~)"
 end if
else if (var .eq. "REFFL" .or. var .eq. "SPREFFL") then
 if(ModI.eq."CAM4-Oslo") then
  res@tiMainString  = "REFFL (REFFL/CLDFOC) (~F33~m~F21~m)"
 else
  res@tiMainString  = "REFFL (AREL/FREQL) (~F33~m~F21~m)"
 end if
else if (var .eq. "ICNC") then
 if(ModI.eq."CAM4-Oslo") then
  res@tiMainString  = "ICNC (N/A)"
 else
  res@tiMainString  = "ICNC (AWNI/FREQI) (cm~S~-3~N~)"
 end if
end if
end if
end if
end if
end if
end if
end if
  plot(0) = gsn_contour(wks,dim_avg_n_Wrap(zave_I,0),res)    ; create the plot

if (var .eq. "CLOUD") then
  res2@tiMainString  = "Cloud Fraction"
else if (var .eq. "RELHUM") then
 if (plot_type.eq.4) then
  if(ModI.eq."CAM4-Oslo") then
   res2@tiMainString  = "Relative Humidity RHW (%)"
  else
   res2@tiMainString  = "Relative Humidity RHW (%)"
  end if
 else
  if(ModI.eq."CAM4-Oslo") then
   res2@tiMainString  = "Relative Humidity RELHUM (%)"
  else
   res2@tiMainString  = "Relative Humidity RELHUM (%)" 
  end if
 end if
else if (var .eq. "CLDLIQ") then
  res2@tiMainString  = "Cloud Liquid Water (mg kg~S~-1~N~)"
else if (var .eq. "CLDICE") then
  res2@tiMainString  = "Cloud Ice Water (mg kg~S~-1~N~)"
else if (var .eq. "CDNC") then
  res2@tiMainString  = "CDNC (AWNC/FREQL) (cm~S~-3~N~)"
else if (var .eq. "REFFL") then
  res2@tiMainString  = "REFFL (AREL/FREQL) (~F33~m~F21~m)"
else if (var .eq. "ICNC") then
  res2@tiMainString  = "ICNC (AWNI/FREQI) (cm~S~-3~N~)"
else if (var .eq. "SPREFFL") then
  res2@tiMainString  = "SP REFFL (SPAREL/FREQL) (~F33~m~F21~m)"
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

