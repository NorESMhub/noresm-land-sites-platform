  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in 3d modal Number concentrations from two
; versions of NorESM/CAM-Oslo and makes global plots of the respective
; zonally and annually averaged variables. 

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
  plot_type = 1                      ; 1 => NNAT_1   Number concentration mode 1
                                     ; 2 => NNAT_2   Number concentration mode 2
                                     ; 3 => NNAT_4   Number concentration mode 4
                                     ; 4 => NNAT_5   Number concentration mode 5
                                     ; 5 => NNAT_6   Number concentration mode 6
                                     ; 6 => NNAT_7   Number concentration mode 7
                                     ; 7 => NNAT_8   Number concentration mode 8
                                     ; 8 => NNAT_9   Number concentration mode 9
                                     ; 9 => NNAT_10  Number concentration mode 10
                                     ;10 => NNAT_0   Number concentration mode 0
                                     ;11 => NNAT_12  Number concentration mode 12
                                     ;12 => NNAT_14  Number concentration mode 14
                                     ;13 => SURVNUC  Survival rate for/to mode 1
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


 if (plot_type.eq.1) then
 var="NNAT_1"                       ; name of input-variable and plot
 varname="NNAT_1"                   ; variable name used in text string
 plot_name="NNAT_1"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.0e0
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.0e0
 else if (plot_type.eq.2) then
 var="NNAT_2"                       ; name of input-variable and plot
 varname="NNAT_2"                   ; variable name used in text string
 plot_name="NNAT_2"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.0e0
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.0e0
 else if (plot_type.eq.3) then
 var="NNAT_4"                       ; name of input-variable and plot
 varname="NNAT_4"                   ; variable name used in text string
 plot_name="NNAT_4"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.0e0
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.0e0
 else if (plot_type.eq.4) then
 var="NNAT_5"                       ; name of input-variable and plot
 varname="NNAT_5"                   ; variable name used in text string
 plot_name="NNAT_5"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.0e0
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.0e0
 else if (plot_type.eq.5) then
 var="NNAT_6"                       ; name of input-variable and plot
 varname="NNAT_6"                   ; variable name used in text string
 plot_name="NNAT_6"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.0e0
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.0e0
 else if (plot_type.eq.6) then
 var="NNAT_7"                       ; name of input-variable and plot
 varname="NNAT_7"                   ; variable name used in text string
 plot_name="NNAT_7"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.0e0
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.0e0
 else if (plot_type.eq.7) then
 var="NNAT_8"                       ; name of input-variable and plot
 varname="NNAT_8"                   ; variable name used in text string
 plot_name="NNAT_8"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.0e0
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.0e0
 else if (plot_type.eq.8) then
 var="NNAT_9"                       ; name of input-variable and plot
 varname="NNAT_9"                   ; variable name used in text string
 plot_name="NNAT_9"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.0e0
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.0e0
 else if (plot_type.eq.9) then
 var="NNAT_10"                       ; name of input-variable and plot
 varname="NNAT_10"                   ; variable name used in text string
 plot_name="NNAT_10"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.0e0
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.0e0
 else if (plot_type.eq.10) then
 var="NNAT_0"                       ; name of input-variable and plot
 varname="NNAT_0"                   ; variable name used in text string
 plot_name="NNAT_0"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.0e0
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.0e0
 else if (plot_type.eq.11) then
 var="NNAT_12"                       ; name of input-variable and plot
 varname="NNAT_12"                   ; variable name used in text string
 plot_name="NNAT_12"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.0e0
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.0e0
 else if (plot_type.eq.12) then
 var="NNAT_14"                       ; name of input-variable and plot
 varname="NNAT_14"                   ; variable name used in text string
 plot_name="NNAT_14"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.0e0
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.0e0

else if (plot_type.eq.13) then
 var="SURVNUC"                       ; name of input-variable and plot
 varname="SURVNUC"                   ; variable name used in text string
 plot_name="SURVNUC"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,"FORMRATE") / (addfiles_GetVar(f1_I,all_files_I,"NUCLRATE") + 1.e-10)
 var_II  = addfiles_GetVar(f1_II,all_files_II,"FORMRATE") / (addfiles_GetVar(f1_II,all_files_II,"NUCLRATE") + 1.e-10)
 end if

end if
 end if
 end if
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
if (var.eq."NNAT_1") then
 digg=(/5,10,20,50,100,200,500,1000,2000,5000/)
 else if (var .eq. "NNAT_2" .or. var .eq. "NNAT_12") then
  digg=(/1,2,5,10,20,50,100,200,500,1000/)
   else if (var .eq. "NNAT_4" .or. var .eq. "NNAT_14") then
    digg=(/1,2,5,10,20,50,100,200,500,1000/)
     else if (var .eq. "NNAT_5") then
      digg=(/0.2,0.5,1,2,5,10,20,50,100,200/)
       else if (var .eq. "NNAT_6") then
        digg=(/0.2,0.5,1,5,10,20,50,100,200/)
         else if (var .eq. "NNAT_7") then
          digg=(/0.01,0.02,0.05,0.1,0.2,0.5,1,2,5,10/)
           else if (var .eq. "NNAT_8") then
            digg=(/0.2,0.5,1,5,10,20,50,100,200/)
             else if (var .eq. "NNAT_9" .or. var .eq. "NNAT_10" .or. var .eq. "NNAT_0") then
              digg=(/0.01,0.02,0.05,0.1,0.2,0.5,1,2,5,10/)
               else
                digg=(/0.01,0.02,0.03,0.05,0.1,0.2,0.3,0.5,0.75,1.0/)
             end if
           end if
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

if (var .eq. "NNAT_1") then
  res@tiMainString  = "NNAT for mode 1 (cm~S~-3~N~)"
else if (var .eq. "NNAT_2") then
  res@tiMainString  = "NNAT for mode 2 (cm~S~-3~N~)"
else if (var .eq. "NNAT_4") then
  res@tiMainString  = "NNAT for mode 4 (cm~S~-3~N~)"
else if (var .eq. "NNAT_5") then
  res@tiMainString  = "NNAT for mode 5 (cm~S~-3~N~)"
else if (var .eq. "NNAT_6") then
  res@tiMainString  = "NNAT for mode 6 (cm~S~-3~N~)"
else if (var .eq. "NNAT_7") then
  res@tiMainString  = "NNAT for mode 7 (cm~S~-3~N~)"
else if (var .eq. "NNAT_8") then
  res@tiMainString  = "NNAT for mode 8 (cm~S~-3~N~)"
else if (var .eq. "NNAT_9") then
  res@tiMainString  = "NNAT for mode 9 (cm~S~-3~N~)"
else if (var .eq. "NNAT_10") then
  res@tiMainString  = "NNAT for mode 10 (cm~S~-3~N~)"
else if (var .eq. "NNAT_0") then
  res@tiMainString  = "NNAT for mode 0 (cm~S~-3~N~)"
else if (var .eq. "NNAT_12") then
  res@tiMainString  = "NNAT for mode 12 (cm~S~-3~N~)"
else if (var .eq. "NNAT_14") then
  res@tiMainString  = "NNAT for mode 14 (cm~S~-3~N~)"
else
  res@tiMainString  = "Survival rate to mode 1"
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if

  plot(0) = gsn_contour(wks,dim_avg_n_Wrap(zave_I,0),res)    ; create the plot

if (var .eq. "NNAT_1") then
  res2@tiMainString  = "NNAT for mode 1 (cm~S~-3~N~)"
else if (var .eq. "NNAT_2") then
  res2@tiMainString  = "NNAT for mode 2 (cm~S~-3~N~)"
else if (var .eq. "NNAT_4") then
  res2@tiMainString  = "NNAT for mode 4 (cm~S~-3~N~)"
else if (var .eq. "NNAT_5") then
  res2@tiMainString  = "NNAT for mode 5 (cm~S~-3~N~)"
else if (var .eq. "NNAT_6") then
  res2@tiMainString  = "NNAT for mode 6 (cm~S~-3~N~)"
else if (var .eq. "NNAT_7") then
  res2@tiMainString  = "NNAT for mode 7 (cm~S~-3~N~)"
else if (var .eq. "NNAT_8") then
  res2@tiMainString  = "NNAT for mode 8 (cm~S~-3~N~)"
else if (var .eq. "NNAT_9") then
  res2@tiMainString  = "NNAT for mode 9 (cm~S~-3~N~)"
else if (var .eq. "NNAT_10") then
  res2@tiMainString  = "NNAT for mode 10 (cm~S~-3~N~)"
else if (var .eq. "NNAT_0") then
  res2@tiMainString  = "NNAT for mode 0 (cm~S~-3~N~)"
else if (var .eq. "NNAT_12") then
  res2@tiMainString  = "NNAT for mode 12 (cm~S~-3~N~)"
else if (var .eq. "NNAT_14") then
  res2@tiMainString  = "NNAT for mode 14 (cm~S~-3~N~)"
else
  res2@tiMainString  =  "Survival rate to mode 1"
end if
end if
end if
end if
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

