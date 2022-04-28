  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in clear-sky or all-sky aerosol optical depth (AOD) 
; from two versions of NorESM/CAM-Oslo and makes global plots of the annually 
; averaged AODs, including global average as a number in the title line for 
; each figure. 

; Model independent constants
 g=9.80665
 pi=3.1415926
 re=6378.39e3            ; earth radius in m
 coffa=pi*re^2./180.      
 area1=4.*pi*re^2          
 small=1.0e-15              ; small number
; small=1.0e-7              ; small number

; *************************************************************************
; **** To be edited by the user if the ncl script is run interactively ****
;
; Define plot type and plot output format
 if (.not. isvar("plot_type")) then      ; is plot_type on command line?
 plot_type = 1                           ; 1 => all-sky AOD
                                         ; 2 => all-sky Volcanic AOD
                                         ; 3 => ABSVIS
                                         ; 4 => ABSVVOLC
                                         ; 5 => AODVISSP
                                         ; 6 => all-sky Volcanic ABS
                                         ; 7 => all-sky ABS
                                         ; 8 => -
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
 AOD_I=tmp_I
 AOD_II=tmp_II


 if (plot_type.eq.1) then
 var="AOD_VIS"                       ; name of main input-variable and plot                    
 varname="AODvis"                      ; variable name used in text string:
 AOD_I=(/(f1_I[:]->AOD_VIS)/)     ; variable to be plotted from I
 AOD_II=(/(f1_II[:]->AOD_VIS)/)  ; variable to be plotted from II
; temporary work-around below, for Ingo's Pinatubo-simulations!
; AOD_I=((/(f1_I[:]->AOD_VIS)/)-(/(f1_I[:]->AODVVOLC)/))/((/(f1_I[:]->DAYFOC)/)+small)+(/(f1_I[:]->AODVVOLC)/)
; AOD_II=((/(f1_II[:]->AOD_VIS)/)-(/(f1_II[:]->AODVVOLC)/))/((/(f1_II[:]->DAYFOC)/)+small)+(/(f1_II[:]->AODVVOLC)/)

 else if (plot_type.eq.2) then
 var="AODVVOLC"                     ; name of main input-variable and plot                    
 varname="AODvisVolc"                  ; variable name used in text string:
 AOD_I=(/(f1_I[:]->AODVVOLC)/)     ; variable to be plotted from I
 AOD_II=(/(f1_II[:]->AODVVOLC)/)  ; variable to be plotted from II

 else if (plot_type.eq.3) then

 var="ABSVIS"                       ; name of main input-variable and plot                    
 varname="ABSvis"                      ; variable name used in text string:
; temporary work-around!
; AOD_I=((/(f1_I[:]->ABSVIS)/)-(/(f1_I[:]->ABSVVOLC)/))/((/(f1_I[:]->DAYFOC)/)+small)+(/(f1_I[:]->ABSVVOLC)/)
; AOD_II=((/(f1_II[:]->ABSVIS)/)-(/(f1_II[:]->ABSVVOLC)/))/((/(f1_II[:]->DAYFOC)/)+small)+(/(f1_II[:]->ABSVVOLC)/)
 AOD_I=(/(f1_I[:]->ABSVIS)/)
 AOD_II=(/(f1_II[:]->ABSVIS)/)

 else if (plot_type.eq.4) then
 var="ABSVVOLC"                      ; name of main input-variable and plot                    
 varname="ABSVisVocl"                 ; variable name used in text string:
; AOD_I=(/(f1_I[:]->ABSVVOLC)/)/((/(f1_I[:]->DAYFOC)/)+small)     ; variable to be plotted from I
; AOD_II=(/(f1_II[:]->ABSVVOLC)/)/((/(f1_II[:]->DAYFOC)/)+small)  ; variable to be plotted from II
; temporary work-around below, for Ingo's Pinatubo-simulations!
 AOD_I=(/(f1_I[:]->ABSVVOLC)/)     ; variable to be plotted from I
 AOD_II=(/(f1_II[:]->ABSVVOLC)/)   ; variable to be plotted from II

 else if (plot_type.eq.5) then
 var="AODVISSP"                      ; name of main input-variable and plot                    
 varname="AODVIS-SP"             ; variable name used in text string:
 AOD_I=(/(f1_I[:]->AOD_VIS)/)*0.0
 AOD_II=(/(f1_II[:]->AODVISSP)/)

 else if (plot_type.eq.6) then
 var="ABSVISSP"                      ; name of main input-variable and plot                    
 varname="ABSVIS-SP"             ; variable name used in text string:
 AOD_I=(/(f1_I[:]->ABSVIS)/)*0.0
 AOD_II=(/(f1_II[:]->ABSVISSP)/)

 end if
 end if
 end if
 end if
 end if
 end if

; Calculating area weighted AODs

 AOD_Ia=AOD_I              ; initialization of global average variable
 AOD_IIa=AOD_II

 xdims_I = dimsizes(gw0_I)
 ;print(xdims_I)
 ydims_I = dimsizes(AOD_Ia)
 ;print(ydims_I)
 do i=0,dimsizes(gw0_I)-1
 AOD_Ia(:,i,:)=AOD_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 end do

 xdims_II = dimsizes(gw0_II)
 ;print(xdims_I)
 ydims_II = dimsizes(AOD_IIa)
 ;print(ydims_II)
 do i=0,dimsizes(gw0_II)-1
 AOD_IIa(:,i,:)=AOD_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 end do

; Defining color scales for each AOD variable
; if (var .eq. "DOD550" .or.  var .eq. "CDOD550" .or. var .eq. "D550_DU") then
if (var .eq. "AOD_VIS" .or. var .eq. "AODVISSP") then
 digg=(/0.02,0.03,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1.0/)
;test digg=(/0.05,0.1,0.15,0.2,0.25,0.3,0.5,1.0, 1.4, 1.6/)                  ; Total, DU
; else if (var .eq. "D550_SO4" .or.  var .eq. "D550_POM" .or.  var .eq. "D550_SS") then
 else if (var .eq. "AODVVOLC" .or. var .eq. "ABSVISSP") then
 digg=(/0.002,0.003,0.005,0.01,0.02,0.03,0.05,0.07,0.1,0.13/)
   else if (var .eq. "D550_BC") then
    digg=(/0.0005,0.001,0.002,0.003,0.004,0.005,0.007,0.01,0.015,0.02/)  ; BC
     else if (var .eq. "ABSVIS" .or. var .eq. "ABSVVOLC") then
     digg=(/0.00005,0.0001,0.0002,0.0005,0.001,0.002,0.005,0.01,0.02,0.05/)  ; ABS
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
  res@cnLevels       =   sprintf("%7.4f",digg)             ; min level

;  res@tiMainString   = "CAM4-Oslo"
  res@gsnRightString   = "avg = "+sprintf("%9.7f",(sum(dim_avg_n(AOD_Ia,0))/area1))
  res@gsnLeftString   = varname
  plot(0) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(AOD_I,0),res)    ; create the plot

;  res@tiMainString   = "CAM5-Oslo"
  res@gsnRightString  = "avg = "+sprintf("%9.7f",(sum(dim_avg_n(AOD_IIa,0))/area1))
  res@gsnLeftString   = varname
  plot(1) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(AOD_II,0),res)   ; create the plot

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
