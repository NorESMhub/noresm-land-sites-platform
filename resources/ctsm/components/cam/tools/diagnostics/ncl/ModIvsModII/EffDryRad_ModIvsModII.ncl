  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in vertically integrated effective dry aerosol radii 
; (from total aerosol volume divided by area) for all sizes, and for dry r < 0.5um 
; and r > 0.5um, from two versions of NorESM/CAM-Oslo. It makes global plots of 
; the respective annually averaged effective dry radius, including global average 
; as a number in the title line for each figure. 

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
   plot_type = 2                     ; 0 => effective dry radius, all r 
                                     ; 1 => effective dry radius, r < 0.5 um
                                     ; 2 => effective dry radius, r > 0.5 um
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
 DER_I=tmp_I
 DER_II=tmp_II

 if (plot_type.eq.0) then
 var="DER"                            ; name of main input-variable and plot
 varname="Effective dry radius, all r"              ; variable name used in text string:
 DER_I=(/(f1_I[:]->DER)/)
 DER_II=(/(f1_II[:]->DER)/)
 else if (plot_type.eq.1) then
 var="DERLT05"                        ; name of plot to be made                    
 varname="Effective dry radius, r<0.5~F33~m~F21~m"  ; variable name used in text string:
 DER_I=(/(f1_I[:]->DERLT05)/)
 DER_II=(/(f1_II[:]->DERLT05)/)
 else if (plot_type.eq.2) then
 var="DERGT05"                        ; name of plot to be made                    
 varname="Effective dry radius, r>0.5~F33~m~F21~m" ; variable name used in text string:
 DER_I=(/(f1_I[:]->DERGT05)/)
 DER_II=(/(f1_II[:]->DERGT05)/)
 end if
 end if
 end if

; Calculating area weighted values

 DER_Ia=DER_I              ; initialization of global average variable
 DER_IIa=DER_II

 xdims_I = dimsizes(gw0_I)
 ;print(xdims_I)
 ydims_I = dimsizes(DER_Ia)
 ;print(ydims_I)
 do i=0,dimsizes(gw0_I)-1
 DER_Ia(:,i,:)=DER_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 end do

 xdims_II = dimsizes(gw0_II)
 ;print(xdims_I)
 ydims_II = dimsizes(DER_IIa)
 ;print(ydims_II)
 do i=0,dimsizes(gw0_II)-1
 DER_IIa(:,i,:)=DER_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 end do

; Defining color scales for each DER variable
 if (plot_type.eq.0) then
 digg=(/0.1,0.15,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9/)
 else if (plot_type.eq.1) then
 digg=(/0.06,0.07,0.08,0.1,0.12,0.15,0.2,0.25,0.3,0.4/)
 else
 digg=(/0.8,1.0,1.2,1.4,1.6,1.8,2.0,2.2,2.4,2.6/)
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
  if (plot_type.eq.3) then  
  res@cnLevels       =   sprintf("%7.4f",digg)             ; min level
  else
  res@cnLevels       =   sprintf("%5.2f",digg)             ; min level
  end if

;  res@tiMainString   = "CAM4-Oslo"
  res@gsnRightString   = "avg = "+sprintf("%4.2f",(sum(dim_avg_n(DER_Ia,0))/area1))+" ~F33~m~F21~m"
  res@gsnLeftString   = varname
  plot(0) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(DER_I,0),res)    ; create the plot

;  res@tiMainString   = "CAM5-Oslo"
  res@gsnRightString  = "avg = "+sprintf("%4.2f",(sum(dim_avg_n(DER_IIa,0))/area1))+" ~F33~m~F21~m"
  res@gsnLeftString   = varname
  plot(1) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(DER_II,0),res)   ; create the plot

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
