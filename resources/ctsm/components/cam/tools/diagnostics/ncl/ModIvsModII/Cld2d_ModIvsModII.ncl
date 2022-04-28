  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in 2d cloud cover or liquid/ice water path from two 
; versions of NorESM/CAM-Oslo and makes global plots of the annually averaged 
; cloud cover or water path, including global average as a number in the title 
; line for each figure. 

; Model independent constants
 g=9.80665
 pi=3.1415926
 re=6378.39e3            ; earth radius in m
 coffa=pi*re^2./180.      
 area1=4.*pi*re^2          
 small=1.0e-15             ; small number

; *************************************************************************
; **** To be edited by the user if the ncl script is run interactively ****
;
; Define plot type and plot output format
 if (.not. isvar("plot_type")) then      ; is plot_type on command line?
  plot_type = 5                      ; 0 => CLDTOT
                                     ; 1 => CLDLOW
                                     ; 2 => CLDMED
                                     ; 3 => CLDHGH
                                     ; 4 => Liquid Water Path
                                     ; 5 => Ice Water Path
                                     ; 6 => Column integrated cloud droplet concentration, a
                                     ; 7 => Column integrated cloud droplet concentration, b
                                     ; 8 => Precipitation
                                     ; 9 => U10
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
 met_I=tmp_I
 met_II=tmp_II

 if (plot_type.eq.0) then
 var="CLDTOT"                    ; name of input-variable and plot
 varname="CLDTOT"                ; variable name used in text string:
 met_I=(/(f1_I[:]->CLDTOT)/)     ; variable to be plotted from I
 met_II=(/(f1_II[:]->CLDTOT)/)   ; variable to be plotted from II
 else if (plot_type.eq.1) then
 var="CLDLOW"                    ; name of input-variable and plot
 varname="CLDLOW"                ; variable name used in text string:
 met_I=(/(f1_I[:]->CLDLOW)/)     ; variable to be plotted from I
 met_II=(/(f1_II[:]->CLDLOW)/)   ; variable to be plotted from II 
 else if (plot_type.eq.2) then
 var="CLDMED"                    ; name of input-variable and plot
 varname="CLDMED"                ; variable name used in text string:
 met_I=(/(f1_I[:]->CLDMED)/)     ; variable to be plotted from I
 met_II=(/(f1_II[:]->CLDMED)/)   ; variable to be plotted from II 
 else if (plot_type.eq.3) then
 var="CLDHGH"                    ; name of input-variable and plot
 varname="CLDHGH"                ; variable name used in text string:
 met_I=(/(f1_I[:]->CLDHGH)/)     ; variable to be plotted from I
 met_II=(/(f1_II[:]->CLDHGH)/)   ; variable to be plotted from II 
 else if (plot_type.eq.4) then
 var="TGCLDLWP"                  ; name of input-variable and plot
 varname="Liquid water path"     ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
  met_I=(/(f1_I[:]->TGCLDLWP)/)   ; variable to be plotted from I
 else
  met_I=(/(f1_I[:]->TGCLDLWP)/)*1.e3 ; variable to be plotted from I 
 end if
 met_II=(/(f1_II[:]->TGCLDLWP)/)*1.e3 ; variable to be plotted from II 
 else if (plot_type.eq.5) then
 var="TGCLDIWP"                  ; name of input-variable and plot
 varname="Ice water path"        ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
  met_I=(/(f1_I[:]->TGCLDIWP)/)   ; variable to be plotted from I
 else
 met_I=(/(f1_I[:]->TGCLDIWP)/)*1.e3 ; variable to be plotted from I
 end if
 met_II=(/(f1_II[:]->TGCLDIWP)/)*1.e3 ; variable to be plotted from II 
 else if (plot_type.eq.6) then
 var="CDNUMC1"                   ; name of plot
 varname="CDNC col."             ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
  met_I=1.e-6*(/(f1_I[:]->CLDTOT)/)*(/(f1_I[:]->CDNCINT)/)/((/(f1_I[:]->FOCHANA)/)+small)  ; variable to be plotted from I
 else
  met_I=1.e-10*(/(f1_I[:]->CDNUMC)/) ; variable to be plotted from I 
 end if
 met_II=1.e-10*(/(f1_II[:]->CDNUMC)/) ; variable to be plotted from II 
 else if (plot_type.eq.7) then
 var="CDNUMC2"                   ; name of plot
 varname="CDNC col."             ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
  met_I=1.e-6*(/(f1_I[:]->CLDTOT)/)*(/(f1_I[:]->CDNCINT)/)  ; variable to be plotted from I
 else
  met_I=1.e-10*(/(f1_I[:]->CDNUMC)/) ; variable to be plotted from I 
 end if
 met_II=1.e-10*(/(f1_II[:]->CDNUMC)/) ; variable to be plotted from II 
 else if (plot_type.eq.8) then
 var="PRECIP"                   ; name of plot
 varname="Precipitation"             ; variable name used in text string:
 met_I=8.64e7*((/(f1_I[:]->PRECC)/)+(/(f1_I[:]->PRECL)/)) ; variable to be plotted from I 
 met_II=8.64e7*((/(f1_II[:]->PRECC)/)+(/(f1_II[:]->PRECL)/)) ; variable to be plotted from II 
 else if (plot_type.eq.9) then
 var="U10"                   ; name of plot
 varname="10m wind"          ; variable name used in text string:
 met_I=(/(f1_I[:]->U10)/)   ; variable to be plotted from I 
 met_II=(/(f1_II[:]->U10)/) ; variable to be plotted from II 
; met_I=(/(f1_I[:]->U10)/)*(/(f1_I[:]->U10)/)*(/(f1_I[:]->U10)/)  ; variable to be plotted from I 
; met_II=(/(f1_II[:]->U10)/)*(/(f1_II[:]->U10)/)*(/(f1_II[:]->U10)/) ; variable to be plotted from II 
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

; Calculating area weighted meteorology variables

 met_Ia=met_I              ; initialization of global average variable
 met_IIa=met_II

 xdims_I = dimsizes(gw0_I)
 ;print(xdims_I)
 ydims_I = dimsizes(met_Ia)
 ;print(ydims_I)
 do i=0,dimsizes(gw0_I)-1
 met_Ia(:,i,:)=met_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 end do

 xdims_II = dimsizes(gw0_II)
 ;print(xdims_I)
 ydims_II = dimsizes(met_IIa)
 ;print(ydims_II)
 do i=0,dimsizes(gw0_II)-1
 met_IIa(:,i,:)=met_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 end do

; Defining color scales for each meteorology variable
if (var.eq."CLDTOT".or.var.eq."CLDLOW".or.var.eq."CLDMED".or.var.eq."CLDHGH") then
 digg=(/0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95/)
 else if (var .eq. "TGCLDLWP") then
  digg=(/3,5,10,20,30,50,100,150,200,300/)
  else if (var .eq. "TGCLDIWP") then
   digg=(/0.5,1,2,3,5,10,20,30,50,100/)             
    else if (var .eq. "CDNUMC1" .or. var .eq. "CDNUMC2") then
      digg=(/0.1,0.2,0.3,0.5,1,2,4,6,8,10/)
      else if (var .eq. "PRECIP") then
        digg=(/0.05,0.1,0.25,0.5,1,1.5,2,3,5,10/)
         else if (var .eq. "U10") then
          digg=(/0.5,1.,2.,3.,4.,5.0,7.5,10.0,12.5,15./)
;          digg=(/0.1,1.,5.,10.,50.,100.,500.,1000.,5000.,10000./)
         else
          digg=(/0.0,1.0/)                         ; Replace with error message
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
if (var .eq. "TGCLDLWP" .or.  var .eq. "TGCLDIWP") then
  res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(met_Ia,0))/area1))+" g m~S~-2~N~"
else if (var .eq. "CDNUMC1") then
 if(ModI.eq."CAM4-Oslo") then
  res@gsnRightString   = "(CDNCINT*CLDTOT/FOCHANA) avg = "+sprintf("%5.2f",(sum(dim_avg_n(met_Ia,0))/area1))+" (10~S~6~N~ cm~S~-2~N~)"
 else
  res@gsnRightString   = "(CDNUMC) avg = "+sprintf("%5.2f",(sum(dim_avg_n(met_Ia,0))/area1))+" (10~S~6~N~ cm~S~-2~N~)"
 end if
else if (var .eq. "CDNUMC2") then
 if(ModI.eq."CAM4-Oslo") then
  res@gsnRightString   = "(CDNCINT*CLDTOT) avg = "+sprintf("%5.2f",(sum(dim_avg_n(met_Ia,0))/area1))+" (10~S~6~N~ cm~S~-2~N~)"
 else
  res@gsnRightString   = "(CDNUMC) avg = "+sprintf("%5.2f",(sum(dim_avg_n(met_Ia,0))/area1))+" (10~S~6~N~ cm~S~-2~N~)"
 end if
else if (var .eq. "PRECIP") then
  res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(met_Ia,0))/area1))+" (mm day~S~-1~N~)"
else if (var .eq. "U10") then
  res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(met_Ia,0))/area1))+" (m s~S~-1~N~)"
 else
  res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(met_Ia,0))/area1))
end if
end if
end if
end if
end if
  res@gsnLeftString   = varname
  plot(0) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(met_I,0),res)    ; create the plot

;  res@tiMainString   = "CAM5-Oslo"
if (var .eq. "TGCLDLWP" .or.  var .eq. "TGCLDIWP") then
  res@gsnRightString  = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(met_IIa,0))/area1))+" g m~S~-2~N~"
else if (var .eq. "CDNUMC1" .or. var .eq. "CDNUMC2") then
  res@gsnRightString   = "(CDNUMC) avg = "+sprintf("%5.2f",(sum(dim_avg_n(met_IIa,0))/area1))+" (10~S~6~N~ cm~S~-2~N~)"
else if (var .eq. "PRECIP") then
  res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(met_IIa,0))/area1))+" (mm day~S~-1~N~)"
else if (var .eq. "U10") then
  res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(met_IIa,0))/area1))+" (m s~S~-1~N~)"
 else
  res@gsnRightString  = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(met_IIa,0))/area1))
end if
end if
end if
end if
  res@gsnLeftString   = varname
  plot(1) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(met_II,0),res)   ; create the plot

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
