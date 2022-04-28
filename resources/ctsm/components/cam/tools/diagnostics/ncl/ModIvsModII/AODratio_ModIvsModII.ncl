  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in clear-sky and all-sky aerosol optical depth (AOD) for
; ambient and zero hunidities from two versions of NorESM/CAM-Oslo and makes global 
; plots of the annually averaged (clear-sky AOD)/AOD or (dry aerosol AOD)/AOD, 
; including global average as a number in the title line for each figure. Also 
; (fine aerosol AOD)/AOD, (fine aerosol AOD + larger aerosol AOD)/AOD, and    
; (sum of AOD for all constituents)/AOD can be plotted in the same way.

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
  plot_type = 6                      ; 0 => (dry aerosol AOD)/AOD
                                     ; 1 => (clear-sky AOD)/AOD,
                                     ; 2 => (small sizes (wet d<1um) AOD)/AOD
                                     ; 3 => (small & lage sizes AOD)/AOD
                                     ; 4 => (sum of AOD for all species)/AOD
                                     ; 5 => all-sky ANG = -ln(DOD870/DOD440)/ln(870/440)
                                     ; 6 => clear-sky ANG = -ln(CDOD870/CDOD440)/ln(870/440)
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
 AOD1_I=tmp_I
 AOD1_II=tmp_II
 AOD2_I=tmp_I
 AOD2_II=tmp_II

 if (plot_type.eq.0) then
 var="dryAODbyAOD"                    ; name of main input-variable and plot
 varname="AOD~B~dry ~N~/AOD"          ; variable name used in text string:
 AOD_I=(/(f1_I[:]->OD550DRY)/)/((/(f1_I[:]->DOD550)/)+small)
 AOD1_I=(/(f1_I[:]->OD550DRY)/)
 AOD2_I=(/(f1_I[:]->DOD550)/)
 AOD_II=(/(f1_II[:]->OD550DRY)/)/((/(f1_II[:]->DOD550)/)+small)
 AOD1_II=(/(f1_II[:]->OD550DRY)/)
 AOD2_II=(/(f1_II[:]->DOD550)/)
 else if (plot_type.eq.1) then
 var="CAODbyAOD"                      ; name of plot to be made                    
 varname="AOD~B~clear-sky ~N~/AOD "   ; variable name used in text string:
 AOD_I=(/(f1_I[:]->CDOD550)/)/((/(f1_I[:]->CLDFREE)/)+small)/((/(f1_I[:]->DOD550)/))
 AOD1_I=(/(f1_I[:]->CDOD550)/)/((/(f1_I[:]->CLDFREE)/)+small)
 AOD2_I=(/(f1_I[:]->DOD550)/)
 AOD_II=(/(f1_II[:]->CDOD550)/)/((/(f1_II[:]->CLDFREE)/)+small)/((/(f1_II[:]->DOD550)/))
 AOD1_II=(/(f1_II[:]->CDOD550)/)/((/(f1_II[:]->CLDFREE)/)+small)
 AOD2_II=(/(f1_II[:]->DOD550)/)
 else if (plot_type.eq.2) then
 var="DLTbyAOD"                       ; name of plot to be made                    
 varname="AOD~B~r<0.5~F33~m~F21~m ~N~/AOD " ; variable name used in text string:
 AOD_I=((/(f1_I[:]->DLT_BC+f1_I[:]->DLT_SO4+f1_I[:]->DLT_POM+f1_I[:]->DLT_SS+f1_I[:]->DLT_DUST)/))/((/(f1_I[:]->DOD550)/))
 AOD1_I=(/(f1_I[:]->DLT_BC+f1_I[:]->DLT_SO4+f1_I[:]->DLT_POM+f1_I[:]->DLT_SS+f1_I[:]->DLT_DUST)/)
 AOD2_I=(/(f1_I[:]->DOD550)/)
 AOD_II=((/(f1_II[:]->DLT_BC+f1_II[:]->DLT_SO4+f1_II[:]->DLT_POM+f1_II[:]->DLT_SS+f1_II[:]->DLT_DUST)/))/((/(f1_II[:]->DOD550)/))
 AOD1_II=(/(f1_II[:]->DLT_BC+f1_II[:]->DLT_SO4+f1_II[:]->DLT_POM+f1_II[:]->DLT_SS+f1_II[:]->DLT_DUST)/)
 AOD2_II=(/(f1_II[:]->DOD550)/)
 else if (plot_type.eq.3) then
 var="DLGTbyAOD"                      ; name of plot to be made                    
 varname="(AOD~B~r<0.5~F33~m~F21~m ~N~+AOD~B~r>0.5~F33~m~F21~m~N~)/AOD " ; variable name used in text string:
 AOD_I=(/(f1_I[:]->DLT_BC+f1_I[:]->DLT_SO4+f1_I[:]->DLT_POM+f1_I[:]->DLT_SS+f1_I[:]->DLT_DUST)/)
 AOD1_I=(AOD_I+(/(f1_I[:]->DGT_BC+f1_I[:]->DGT_SO4+f1_I[:]->DGT_POM+f1_I[:]->DGT_SS+f1_I[:]->DGT_DUST)/))
 AOD2_I=(/(f1_I[:]->DOD550)/)
 AOD_I=AOD1_I/AOD2_I
 AOD_II=(/(f1_II[:]->DLT_BC+f1_II[:]->DLT_SO4+f1_II[:]->DLT_POM+f1_II[:]->DLT_SS+f1_II[:]->DLT_DUST)/)
 AOD1_II=(AOD_II+(/(f1_II[:]->DGT_BC+f1_II[:]->DGT_SO4+f1_II[:]->DGT_POM+f1_II[:]->DGT_SS+f1_II[:]->DGT_DUST)/))
 AOD2_II=(/(f1_II[:]->DOD550)/)
 AOD_II=AOD1_II/AOD2_II
 else if (plot_type.eq.4) then
 var="SumODbyAOD"                     ; name of plot to be made                    
 varname="(AOD~B~SO4~N~+AOD~B~POM~N~+AOD~B~BC~N~+AOD~B~SS~N~+AOD~B~DU~N~)/AOD " ; variable name used in text string:
 AOD_I=(/(f1_I[:]->D550_BC+f1_I[:]->D550_SO4+f1_I[:]->D550_POM+f1_I[:]->D550_SS+f1_I[:]->D550_DU)/)/(/(f1_I[:]->DOD550)/)
 AOD_II=(/(f1_II[:]->D550_BC+f1_II[:]->D550_SO4+f1_II[:]->D550_POM+f1_II[:]->D550_SS+f1_II[:]->D550_DU)/)/(/(f1_II[:]->DOD550)/)
 else if (plot_type.eq.5) then
 var="ANG4487"                        ; name of plot to be made                    
 varname="ANG~B~4487~N~ "             ; variable name used in text string:
; AOD_I=-log((/f1_I[:]->DOD870/)/(/(f1_I[:]->DOD440)/))/log(870.0/440.0)
 AOD_I=-log((/(f1_I[:]->DOD870)/)/(/(f1_I[:]->DOD440)/))/log(870.0/440.0)
 AOD_II=-log((/(f1_II[:]->DOD870)/)/(/(f1_II[:]->DOD440)/))/log(870.0/440.0)
; AOD_I=-log((/(f1_I[:]->DOD670)/)/(/(f1_I[:]->DOD500)/))/log(670.0/500.0)       ; alternative definition (ANG5067)
; AOD_II=-log((/(f1_II[:]->DOD670)/)/(/(f1_II[:]->DOD500)/))/log(670.0/500.0)  ; alternative definition (ANG5067)
 else if (plot_type.eq.6) then
 var="CANG4487"                        ; name of plot to be made                    
 varname="Clear-sky ANG~B~4487~N~ "             ; variable name used in text string:
 AOD_I =-log(((/(f1_I[:]->CDOD870)/)+small/((/(f1_I[:]->CLDFREE)/)+small))/((/(f1_I[:]->CDOD440)/)+small/((/(f1_I[:]->CLDFREE)/)+small)))/log(870.0/440.0)
 AOD_II=-log((((/(f1_II[:]->CDOD870)/)+small)/((/(f1_II[:]->CLDFREE)/)+small))/(((/(f1_II[:]->CDOD440)/)+small)/((/(f1_II[:]->CLDFREE)/)+small)))/log(870.0/440.0)
 end if
 end if
 end if
 end if
 end if
 end if
 end if

; Calculating area weighted values

 AOD_Ia=AOD_I              ; initialization of global average variable
 AOD_IIa=AOD_II
 AOD1_Ia=AOD_I
 AOD2_Ia=AOD_I
 AOD1_IIa=AOD_II
 AOD2_IIa=AOD_II

 xdims_I = dimsizes(gw0_I)
 ;print(xdims_I)
 ydims_I = dimsizes(AOD_Ia)
 ;print(ydims_I)
 do i=0,dimsizes(gw0_I)-1
 AOD_Ia(:,i,:)=AOD_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 end do
 if (plot_type.le.3) then
  do i=0,dimsizes(gw0_I)-1
  AOD1_Ia(:,i,:)=AOD1_I(:,i,:)*coffa*dlon_I*gw0_I(i)
  AOD2_Ia(:,i,:)=AOD2_I(:,i,:)*coffa*dlon_I*gw0_I(i)
  end do
 end if

 xdims_II = dimsizes(gw0_II)
 ;print(xdims_I)
 ydims_II = dimsizes(AOD_IIa)
 ;print(ydims_II)
 do i=0,dimsizes(gw0_II)-1
 AOD_IIa(:,i,:)=AOD_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 end do
 if (plot_type.le.3) then
  do i=0,dimsizes(gw0_II)-1
  AOD1_IIa(:,i,:)=AOD1_II(:,i,:)*coffa*dlon_II*gw0_II(i)
  AOD2_IIa(:,i,:)=AOD2_II(:,i,:)*coffa*dlon_II*gw0_II(i)
  end do
 end if

; Defining color scales for each AOD variable
 if (plot_type.eq.1) then
 digg=(/0.5,0.75,0.9,0.95,0.99,1.01,1.05,1.1,1.25,1.5/)
 else if (plot_type.eq.3.or.plot_type.eq.4) then
 digg=(/0.998,0.999,0.9995,0.9998,0.9999,1.0/)
 else if (plot_type.eq.5.or.plot_type.eq.6) then
 digg=(/0.1,0.2,0.3,0.5,0.75,1.0,1.25,1.5,1.75,2.0/)
 else
 digg=(/0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95/)
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
  if (plot_type.eq.3) then  
  res@cnLevels       =   sprintf("%7.4f",digg)             ; min level
  else
  res@cnLevels       =   sprintf("%5.2f",digg)             ; min level
  end if

;  res@tiMainString   = "CAM4-Oslo"
;  res@gsnRightString   = "avg = "+sprintf("%5.2f",extave_I)+" ("+sprintf("%5.2f",aodave_I/loadave_I)+")" 

  if (plot_type.le.3) then
  res@gsnRightString   = "avg = "+sprintf("%5.3f",(sum(dim_avg_n(AOD_Ia,0))/area1))+" ("+sprintf("%5.3f",(sum(dim_avg_n(AOD1_Ia,0)))/(sum(dim_avg_n(AOD2_Ia,0))))+")" 
  else
  res@gsnRightString   = "avg = "+sprintf("%5.3f",(sum(dim_avg_n(AOD_Ia,0))/area1))
  end if
  res@gsnLeftString   = varname
  plot(0) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(AOD_I,0),res)    ; create the plot

;  res@tiMainString   = "CAM5-Oslo"
  if (plot_type.le.3) then
  res@gsnRightString   = "avg = "+sprintf("%5.3f",(sum(dim_avg_n(AOD_IIa,0))/area1))+" ("+sprintf("%5.3f",(sum(dim_avg_n(AOD1_IIa,0)))/(sum(dim_avg_n(AOD2_IIa,0))))+")" 
  else
  res@gsnRightString  = "avg = "+sprintf("%5.3f",(sum(dim_avg_n(AOD_IIa,0))/area1))
  end if
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
