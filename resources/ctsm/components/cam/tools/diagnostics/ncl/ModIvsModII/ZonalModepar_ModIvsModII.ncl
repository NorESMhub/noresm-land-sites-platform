  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in 3d modal (size or number or hygroscopic) parameters 
; from two versions of NorESM/CAM-Oslo and makes global plots of the respective
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
  plot_type = 1                      ; 1 => NMR01   Number median radius mode 1
                                     ; 2 => NMR02   Number median radius mode 2
                                     ; 3 => NMR04   Number median radius mode 4
                                     ; 4 => NMR05   Number median radius mode 5
                                     ; 5 => NMR06   Number median radius mode 6
                                     ; 6 => NMR07   Number median radius mode 7
                                     ; 7 => NMR08   Number median radius mode 8
                                     ; 8 => NMR09   Number median radius mode 9
                                     ; 9 => NMR10   Number median radius mode 10
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
 var="NMR01"                       ; name of input-variable and plot
 varname="NMR01"                   ; variable name used in text string
 plot_name="NMR01"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.e6
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.e6
 else if (plot_type.eq.2) then
 var="NMR02"                       ; name of input-variable and plot
 varname="NMR02"                   ; variable name used in text string
 plot_name="NMR02"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.e6
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.e6
 else if (plot_type.eq.3) then
 var="NMR04"                       ; name of input-variable and plot
 varname="NMR04"                   ; variable name used in text string
 plot_name="NMR04"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.e6
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.e6
 else if (plot_type.eq.4) then
 var="NMR05"                       ; name of input-variable and plot
 varname="NMR05"                   ; variable name used in text string
 plot_name="NMR05"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.e6
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.e6
 else if (plot_type.eq.5) then
 var="NMR06"                       ; name of input-variable and plot
 varname="NMR06"                   ; variable name used in text string
 plot_name="NMR06"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.e6
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.e6
 else if (plot_type.eq.6) then
 var="NMR07"                       ; name of input-variable and plot
 varname="NMR07"                   ; variable name used in text string
 plot_name="NMR07"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.e6
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.e6
 else if (plot_type.eq.7) then
 var="NMR08"                       ; name of input-variable and plot
 varname="NMR08"                   ; variable name used in text string
 plot_name="NMR08"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.e6
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.e6
 else if (plot_type.eq.8) then
 var="NMR09"                       ; name of input-variable and plot
 varname="NMR09"                   ; variable name used in text string
 plot_name="NMR09"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.e6
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.e6
 else if (plot_type.eq.9) then
 var="NMR10"                       ; name of input-variable and plot
 varname="NMR10"                   ; variable name used in text string
 plot_name="NMR10"                 ; name of the plot/figure
 var_I  = addfiles_GetVar(f1_I,all_files_I,var)*1.e6
 var_II  = addfiles_GetVar(f1_II,all_files_II,var)*1.e6
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
if (var.eq."NMR01") then
 digg=(/0.012,0.014,0.016,0.018,0.02,0.022,0.025,0.03,0.035,0.04,0.05/)
 else if (var .eq. "NMR02") then
;  digg=(/0.012,0.013,0.014,0.015,0.016,0.017,0.018,0.019,0.020,0.021,0.022/)
;  digg=(/0.012,0.014,0.016,0.018,0.020,0.022,0.024,0.025,0.027,0.029,0.03/)
  digg=(/0.024,0.025,0.026,0.027,0.028,0.029,0.030,0.032,0.035,0.04,0.05/)
   else if (var .eq. "NMR04") then
    digg=(/0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.15,0.2,0.3,0.4/)
     else if (var .eq. "NMR05") then
      digg=(/0.075,0.08,0.09,0.1,0.12,0.14,0.16,0.18,0.2,0.25,0.3/)
       else if (var .eq. "NMR06") then
        digg=(/0.22,0.24,0.28,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65/)
         else if (var .eq. "NMR07") then
          digg=(/0.63,0.65,0.67,0.7,0.75,0.8,0.9,1.0,1.1,1.2,1.5/)
           else if (var .eq. "NMR08") then
            digg=(/0.0475,0.05,0.06,0.07,0.08,0.09,0.1,0.15,0.2,0.3,0.4/)
             else if (var .eq. "NMR09") then
              digg=(/0.3,0.31,0.32,0.34,0.36,0.38,0.4,0.45,0.5,0.55,0.6/)
               else if (var .eq. "NMR10") then
                digg=(/0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.82,0.84,0.86,0.88/)
               else
                digg=(/0.0,1.0/)                         ; Replace with error message
               end if
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

if (var .eq. "NMR01") then
  res@tiMainString  = "NMR for mode 1 (~F33~m~F21~m)"
else if (var .eq. "NMR02") then
  res@tiMainString  = "NMR for mode 2 (~F33~m~F21~m)"
else if (var .eq. "NMR04") then
  res@tiMainString  = "NMR for mode 4 (~F33~m~F21~m)"
else if (var .eq. "NMR05") then
  res@tiMainString  = "NMR for mode 5 (~F33~m~F21~m)"
else if (var .eq. "NMR06") then
  res@tiMainString  = "NMR for mode 6 (~F33~m~F21~m)"
else if (var .eq. "NMR07") then
  res@tiMainString  = "NMR for mode 7 (~F33~m~F21~m)"
else if (var .eq. "NMR08") then
  res@tiMainString  = "NMR for mode 8 (~F33~m~F21~m)"
else if (var .eq. "NMR09") then
  res@tiMainString  = "NMR for mode 9 (~F33~m~F21~m)"
else if (var .eq. "NMR10") then
  res@tiMainString  = "NMR for mode 10 (~F33~m~F21~m)"
else
  res@tiMainString  = "NMR for mode ... (~F33~m~F21~m)"
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

if (var .eq. "NMR01") then
  res2@tiMainString  = "NMR for mode 1 (~F33~m~F21~m)"
else if (var .eq. "NMR02") then
  res2@tiMainString  = "NMR for mode 2 (~F33~m~F21~m)"
else if (var .eq. "NMR04") then
  res2@tiMainString  = "NMR for mode 4 (~F33~m~F21~m)"
else if (var .eq. "NMR05") then
  res2@tiMainString  = "NMR for mode 5 (~F33~m~F21~m)"
else if (var .eq. "NMR06") then
  res2@tiMainString  = "NMR for mode 6 (~F33~m~F21~m)"
else if (var .eq. "NMR07") then
  res2@tiMainString  = "NMR for mode 7 (~F33~m~F21~m)"
else if (var .eq. "NMR08") then
  res2@tiMainString  = "NMR for mode 8 (~F33~m~F21~m)"
else if (var .eq. "NMR09") then
  res2@tiMainString  = "NMR for mode 9 (~F33~m~F21~m)"
else if (var .eq. "NMR10") then
  res2@tiMainString  = "NMR for mode 10 (~F33~m~F21~m)"
else
  res2@tiMainString  = "REFFL (AREL/FREQL) (~F33~m~F21~m)"
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

