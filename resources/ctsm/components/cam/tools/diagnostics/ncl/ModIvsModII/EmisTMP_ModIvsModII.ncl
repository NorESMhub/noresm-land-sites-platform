  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in aerosol emissions from two versions of NorESM/CAM-Oslo 
; and makes global plots of the annually averaged emissions, including global 
; average as a number in the title line for each figure. 

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
   plot_type = 5                     ;-1 => DMS emissions
                                     ; 0 => SO2 emissions
                                     ; 1 => SO4 emissions
                                     ; 2 => BC  emissions
                                     ; 3 => POM emissions
                                     ; 4 => SS  emissions
                                     ; 5 => DU  emissions
                                     ; 6 => Isoprene emissions
                                     ; 7 => Monoterpene emissions
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
 load_I=tmp_I
 load_II=tmp_II

 if (plot_type.eq.-1) then
 var="EMI_DMS"
 varname="DMS emissions"
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->EMI_DMS)/)*1.e12
 else
   load_I = (/f1_I[:]->SFDMS/)*32/62; as S
   load_I = load_I*1.e12
 end if
 load_II = (/f1_II[:]->SFDMS/)*32/62; as S
 load_II = load_II*1.e12
 else if (plot_type.eq.0) then
 var="EMI_SO2"                    ; name of input-variable and plot
 varname="SO2 emissions"          ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->EMI_SO2)/)*1.e12     ; variable to be plotted from I
 else
   load_I=(/(f1_I[:]->SFSO2)/)/1.998 + (/(f1_I[:]->SO2_XFRC_COL)/)/1.998  ; variable to be plotted from I
   load_I = load_I*1.e12
 end if
 load_II=(/(f1_II[:]->SFSO2)/)/1.998 + (/(f1_II[:]->SO2_CMXF)/)/1.998  ; variable to be plotted from II
 load_II = load_II*1.e12
 else if (plot_type.eq.1) then
 var="EMI_SO4"                    ; name of input-variable and plot
 varname="SO4 emissions"          ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->EMI_SO4)/)*1.e12     ; variable to be plotted from I
 else
   load_I=(/(f1_I[:]->SFSO4_A1)/)/3.06 + (/(f1_I[:]->SFSO4_A2)/)/3.59 + (/(f1_I[:]->SFSO4_AC)/)/3.06  + (/(f1_I[:]->SFSO4_NA)/)/3.06 + (/(f1_I[:]->SFSO4_PR)/)/3.06 + (/(f1_I[:]->SO4_PR_XFRC_COL)/)/3.06
   load_I = load_I*1.e12
 end if 
 load_II=(/(f1_II[:]->SFSO4_A1)/)/3.06 + (/(f1_II[:]->SFSO4_A2)/)/3.59 + (/(f1_II[:]->SFSO4_AC)/)/3.06  + (/(f1_II[:]->SFSO4_NA)/)/3.06 + (/(f1_II[:]->SFSO4_PR)/)/3.06 + (/(f1_II[:]->SO4_PR_CMXF)/)/3.06
 load_II = load_II*1.e12
 else if (plot_type.eq.2) then
 var="EMI_BC"                     ; name of input-variable and plot
 varname="BC emissions"           ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->EMI_BC)/)*1.e12     ; variable to be plotted from I
 else
   load_I=(/(f1_I[:]->SFBC_A)/) + (/(f1_I[:]->SFBC_AC)/) + (/(f1_I[:]->SFBC_AX)/) + (/(f1_I[:]->SFBC_AI)/) + (/(f1_I[:]->SFBC_NI)/) + (/(f1_I[:]->SFBC_N)/) + (/(f1_I[:]->BC_AX_XFRC_COL)/) + (/(f1_I[:]->BC_NI_XFRC_COL)/) + (/(f1_I[:]->BC_N_XFRC_COL)/)
   load_I = load_I*1.e12
 end if
 load_II=(/(f1_II[:]->SFBC_A)/) + (/(f1_II[:]->SFBC_AC)/) + (/(f1_II[:]->SFBC_AX)/) + (/(f1_II[:]->SFBC_AI)/) + (/(f1_II[:]->SFBC_NI)/) + (/(f1_II[:]->SFBC_N)/) + (/(f1_II[:]->BC_AX_CMXF)/) + (/(f1_II[:]->BC_NI_CMXF)/) + (/(f1_II[:]->BC_N_CMXF)/)
 load_II = load_II*1.e12
  else if (plot_type.eq.3) then
 var="EMI_POM"                    ; name of input-variable and plot
 varname="POM emissions"          ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->EMI_POM)/)*1.e12    ; variable to be plotted from I
 else
   load_I=(/(f1_I[:]->SFOM_AI)/) + (/(f1_I[:]->SFOM_AC)/) + (/(f1_I[:]->SFOM_NI)/) +  (/(f1_I[:]->OM_NI_XFRC_COL)/)
   load_I = load_I*1.e12
 end if
 load_II=(/(f1_II[:]->SFOM_AI)/) + (/(f1_II[:]->SFOM_AC)/) + (/(f1_II[:]->SFOM_NI)/) +  (/(f1_II[:]->OM_NI_CMXF)/)
 load_II = load_II*1.e12
 else if (plot_type.eq.4) then
 var="EMI_SS"                     ; name of input-variable and plot
 varname="Sea-salt emissions"     ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->EMI_SS)/)*1.e12     ; variable to be plotted from I
 else
   load_I=(/(f1_I[:]->SFSS_A1)/) + (/(f1_I[:]->SFSS_A2)/) + (/(f1_I[:]->SFSS_A3)/)
   load_I = load_I*1.e12
 end if
 load_II=(/(f1_II[:]->SFSS_A1)/) + (/(f1_II[:]->SFSS_A2)/) + (/(f1_II[:]->SFSS_A3)/)
 load_II = load_II*1.e12
 else if (plot_type.eq.5) then
 var="EMI_DUST"                   ; name of input-variable and plot
 varname="Dust emissions"         ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->EMI_DUST)/)*1.e12   ; variable to be plotted from I
 else 
   load_I=(/(f1_I[:]->SFDST_A2)/) + (/(f1_I[:]->SFDST_A3)/)
   load_I = load_I*1.e12
 end if
 load_II=(/(f1_II[:]->SFDST_A2)/) + (/(f1_II[:]->SFDST_A3)/)
 load_II = load_II*1.e12
 else if (plot_type.eq.6) then
 var="SFisoprene"                   ; name of input-variable and plot
 varname="Isoprene emissions"         ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->EMI_DUST)/)*0.e12   ; variable to be plotted from I
 else 
   load_I=(/(f1_I[:]->SFisoprene)/)
   load_I = load_I*1.e12
 end if
 load_II=(/(f1_II[:]->SFisoprene)/)
 load_II = load_II*1.e12
 else if (plot_type.eq.7) then
 var="SFmonoterp"                   ; name of input-variable and plot
 varname="Monoterpene emissions"         ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->EMI_DUST)/)*0.e12   ; variable to be plotted from I
 else 
   load_I=(/(f1_I[:]->SFmonoterp)/)
   load_I = load_I*1.e12
 end if
 load_II=(/(f1_II[:]->SFmonoterp)/)
 load_II = load_II*1.e12
 end if
 end if
 end if
 end if
 end if
 end if
 end if
 end if
 end if

; Calculating area weighted loads

 load_Ia=load_I              ; initialization of global average variable
 load_IIa=load_II

 xdims_I = dimsizes(gw0_I)
 ;print(xdims_I)
 ydims_I = dimsizes(load_Ia)
 ;print(ydims_I)
 do i=0,dimsizes(gw0_I)-1
 load_Ia(:,i,:)=load_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 end do

 xdims_II = dimsizes(gw0_II)
 ;print(xdims_I)
 ydims_II = dimsizes(load_IIa)
; print(ydims_II)
 do i=0,dimsizes(gw0_II)-1
 load_IIa(:,i,:)=load_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 end do

; Defining color scales for each load variable
if (var .eq. "EMI_SO2" .or. var .eq. "EMI_BC" .or. var .eq. "EMI_DMS") then
 digg=(/0.01,0.05,0.1,0.25,0.5,1,2.5,5,10,25/)                ; EMI_DMS & SO2 & BC
 else if (var .eq. "EMI_SO4") then
  digg=(/0.001,0.005,0.01,0.025,0.05,0.1,0.25,0.5,1,2.5/)     ; EMI_SO4 
  else if (var .eq. "EMI_POM") then
  digg=(/0.1,0.25,0.5,1,2.5,5,10,25,50,100/)                  ; EMI_POM
   else if (var .eq. "EMI_SS") then
    digg=(/5,10,25,50,100,250,500,1000,1500,2000/)            ; EMI_SS
    else if (var .eq. "SFisoprene" .or. var .eq. "SFmonoterp") then
     digg=(/1,2,5,10,25,50,100,250,500,1000/)            ; SFisoprene
    else if (var .eq. "EMI_DUST") then
;     digg=(/50,100,250,500,750,1000,1500,2500,5000,7500/)     ; EMI_DU
     digg=(/250,500,750,1000,1500,2500,5000,7500,15000,25000/)     ; EMI_DU
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
; res@mpShapeMode  = "FreeAspect"
; res@vpWidthF      = 0.8
; res@vpHeightF     = 0.6


;  res@tiMainString   = "CAM4-Oslo"
if (var .eq. "EMI_SO2" .or.  var .eq. "EMI_SO4") then
;err  res@gsnRightString   = "avg = "+sprintf("%6.3f",(sum(dim_avg_n(load_Ia,0))/area1))+" ~F33~m~F21~g S m~S~-2~N~ s~S~-1~N~"
  res@gsnRightString   = "avg = "+sprintf("%6.3f",(sum(dim_avg_n(load_Ia,0))/area1))+" ng S m~S~-2~N~ s~S~-1~N~"
else
;err  res@gsnRightString   = "avg = "+sprintf("%6.3f",(sum(dim_avg_n(load_Ia,0))/area1))+" ~F33~m~F21~g m~S~-2~N~ s~S~-1~N~"
  res@gsnRightString   = "avg = "+sprintf("%6.3f",(sum(dim_avg_n(load_Ia,0))/area1))+" ng m~S~-2~N~ s~S~-1~N~"
end if
  res@gsnLeftString   = varname
  plot(0) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(load_I,0),res)    ; create the plot

;  res@tiMainString   = "CAM5-Oslo"
if (var .eq. "EMI_SO2" .or.  var .eq. "EMI_SO4") then
;err  res@gsnRightString  = "avg = "+sprintf("%6.3f",(sum(dim_avg_n(load_IIa,0))/area1))+" ~F33~m~F21~g S m~S~-2~N~ s~S~-1~N~"
  res@gsnRightString  = "avg = "+sprintf("%6.3f",(sum(dim_avg_n(load_IIa,0))/area1))+" ng S m~S~-2~N~ s~S~-1~N~"
else
;err  res@gsnRightString  = "avg = "+sprintf("%6.3f",(sum(dim_avg_n(load_IIa,0))/area1))+" ~F33~m~F21~g m~S~-2~N~ s~S~-1~N~"
  res@gsnRightString  = "avg = "+sprintf("%6.3f",(sum(dim_avg_n(load_IIa,0))/area1))+" ng m~S~-2~N~ s~S~-1~N~"
end if
  res@gsnLeftString   = varname

  plot(1) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(load_II,0),res)   ; create the plot

  pres                            = True        ; panel plot mods desired
;  pres@gsnMaximize                = True        ; fill the page
;  pres@txString   =   var
  pres@txFontHeightF              =0.015
  pres@txFontThicknessF              =2
  pres@gsnPanelLabelBar = True
  pres@lbLabelFontHeightF = 0.01
;  pres@lbOrientation        ="Vertical"
;  pres@gsnPaperOrientation = "portrait

  gsn_panel(wks,plot,(/1,2/),pres)              ; create panel plot


end
