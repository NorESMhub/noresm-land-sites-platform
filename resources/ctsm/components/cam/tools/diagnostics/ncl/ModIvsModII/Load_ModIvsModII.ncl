  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in aerosol column burdens (loads) from two versions 
; of NorESM/CAM-Oslo and makes global plots of the annually averaged loads,
; including global average as a number in the title line for each figure. 

; Model independent constants
 g=9.80665
 pi=3.1415926
 re=6378.39e3            ; earth radius in m
 coffa=pi*re^2./180.      
 area1=4.*pi*re^2          

; *************************************************************************
; **** To be edited by the user if the ncl script is run interactively ****
; Define plot type and plot output format
 if (.not. isvar("plot_type")) then      ; is plot_type on command line?
  plot_type = 1                      ;-1 => DMS load
                                     ; 0 => SO2 load
                                     ; 1 => SO4 load
                                     ; 2 => BC  load
                                     ; 3 => POM load
                                     ; 4 => SS  load
                                     ; 5 => DU  load
                                     ; 6 => H2O load
                                     ; 7 => Excess look-up table load
                                     ; 8 => COLRBC0 BC0 column budren ratio
                                     ; 9 => COLRBC2 BC2 column budren ratio
                                     ; 10 => COLRBC4 BC4 column budren ratio
                                     ; 11 => COLRBC12 BC12 column budren ratio
                                     ; 12 => COLRBC14 BC14 column budren ratio
                                     ; 13 => COLRBCAC BCAC column budren ratio
                                     ; 14 => COLROC4 OC4 column budren ratio
                                     ; 15 => COLRBC14 BC14 column budren ratio
                                     ; 16 => COLROCAC OCAC column budren ratio
                                     ; 17 => COLRSUL1 SUL1 column budren ratio
                                     ; 18 => COLRSUL5 SUL5 column budren ratio
                                     ; 19 => COLRSULA SULA column budren ratio
; Note: column burden ratio for tracer X = (look-up table burden X)/(life-cycle burden X)  
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
 var="C_DMS"              ; name of input-variable and plot
 varname="DMS"            ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->C_DMS)/)*1.e6     ; variable to be plotted from I
 else
   load_I=(/(f1_I[:]->cb_DMS)/)*1.e6*32/62   ; variable to be plotted from I
 end if
 load_II=(/(f1_II[:]->cb_DMS)/)*1.e6*32/62   ; variable to be plotted from II
 else if (plot_type.eq.0) then
 var="C_SO2"              ; name of input-variable and plot
 varname="SO2"            ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->C_SO2)/)*1.e6     ; variable to be plotted from I
 else
   load_I=(/(f1_I[:]->cb_SO2)/)*1.e6/1.998   ; variable to be plotted from I
 end if
 load_II=(/(f1_II[:]->cb_SO2)/)*1.e6/1.998   ; variable to be plotted from II
 else if (plot_type.eq.1) then
 var="C_SO4"              ; name of input-variable and plot
 varname="SO4"            ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->C_SO4)/)*1.e6     ; variable to be plotted from I
 else
   load_I=(/(f1_I[:]->cb_SO4_A1)/)/3.06 + (/(f1_I[:]->cb_SO4_A2)/)/3.59 + (/(f1_I[:]->cb_SO4_AC)/)/3.06 + (/(f1_I[:]->cb_SO4_NA)/)/3.06 + (/(f1_I[:]->cb_SO4_PR)/)/3.06 + (/(f1_I[:]->cb_SO4_A1_OCW)/)/3.06 + (/(f1_I[:]->cb_SO4_A2_OCW)/)/3.59 + (/(f1_I[:]->cb_SO4_AC_OCW)/)/3.06 + (/(f1_I[:]->cb_SO4_NA_OCW)/)/3.06 + (/(f1_I[:]->cb_SO4_PR_OCW)/)/3.06
   load_I = load_I*1.e6
 end if
 load_II=(/(f1_II[:]->cb_SO4_A1)/)/3.06 + (/(f1_II[:]->cb_SO4_A2)/)/3.59 + (/(f1_II[:]->cb_SO4_AC)/)/3.06 +  (/(f1_II[:]->cb_SO4_NA)/)/3.06 + (/(f1_II[:]->cb_SO4_PR)/)/3.06 + (/(f1_II[:]->cb_SO4_A1_OCW)/)/3.06 + (/(f1_II[:]->cb_SO4_A2_OCW)/)/3.59 + (/(f1_II[:]->cb_SO4_AC_OCW)/)/3.06  + (/(f1_II[:]->cb_SO4_NA_OCW)/)/3.06 + (/(f1_II[:]->cb_SO4_PR_OCW)/)/3.06
 load_II = load_II*1.e6
 else if (plot_type.eq.2) then
 var="C_BC"               ; name of input-variable and plot
 varname="BC"             ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->C_BC)/)*1.e6     ; variable to be plotted from I
 else
   load_I=(/(f1_I[:]->cb_BC)/) + (/(f1_I[:]->cb_BC_A_OCW)/) + (/(f1_I[:]->cb_BC_AC_OCW)/) + (/(f1_I[:]->cb_BC_AI_OCW)/) + (/(f1_I[:]->cb_BC_NI_OCW)/) + (/(f1_I[:]->cb_BC_N_OCW)/)
   load_I = load_I*1.e6
 end if
 load_II=(/(f1_II[:]->cb_BC)/) + (/(f1_II[:]->cb_BC_A_OCW)/) + (/(f1_II[:]->cb_BC_AC_OCW)/) + (/(f1_II[:]->cb_BC_AI_OCW)/) + (/(f1_II[:]->cb_BC_NI_OCW)/) + (/(f1_II[:]->cb_BC_N_OCW)/)
 load_II = load_II*1.e6
 else if (plot_type.eq.3) then
 var="C_POM"              ; name of input-variable and plot
 varname="POM"            ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->C_POM)/)*1.e6    ; variable to be plotted from I
 else
   load_I=(/(f1_I[:]->cb_OM)/) + (/(f1_I[:]->cb_OM_AI_OCW)/) + (/(f1_I[:]->cb_OM_AC_OCW)/) + (/(f1_I[:]->cb_OM_NI_OCW)/) \
          + (/f1_I[:]->cb_SOA_A1_OCW/) + (/f1_I[:]->cb_SOA_NA_OCW/)
   load_I = load_I*1.e6
 end if
 load_II=(/(f1_II[:]->cb_OM)/) + (/(f1_II[:]->cb_OM_AI_OCW)/) + (/(f1_II[:]->cb_OM_AC_OCW)/) + (/(f1_II[:]->cb_OM_NI_OCW)/) \
        + (/f1_II[:]->cb_SOA_A1_OCW/) + (/f1_II[:]->cb_SOA_NA_OCW/)
 load_II = load_II*1.e6
 else if (plot_type.eq.4) then
 var="C_SS"               ; name of input-variable and plot
 varname="Sea-salt"       ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->C_SS)/)*1.e6     ; variable to be plotted from I
 else
   load_I=(/(f1_I[:]->cb_SALT)/) + (/(f1_I[:]->cb_SS_A1_OCW)/) + (/(f1_I[:]->cb_SS_A2_OCW)/) + (/(f1_I[:]->cb_SS_A3_OCW)/) 
   load_I = load_I*1.e6
 end if
 load_II=(/(f1_II[:]->cb_SALT)/) + (/(f1_II[:]->cb_SS_A1_OCW)/) + (/(f1_II[:]->cb_SS_A2_OCW)/) + (/(f1_II[:]->cb_SS_A3_OCW)/) 
 load_II = load_II*1.e6
 else if (plot_type.eq.5) then
 var="C_DUST"             ; name of input-variable and plot
 varname="Dust"           ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
   load_I=(/(f1_I[:]->C_DUST)/)*1.e6   ; variable to be plotted from I
 else
   load_I=(/(f1_I[:]->cb_DUST)/) + (/(f1_I[:]->cb_DST_A2_OCW)/) + (/(f1_I[:]->cb_DST_A3_OCW)/)
   load_I = load_I*1.e6
 end if
 load_II=(/(f1_II[:]->cb_DUST)/) + (/(f1_II[:]->cb_DST_A2_OCW)/) + (/(f1_II[:]->cb_DST_A3_OCW)/)
 load_II = load_II*1.e6
 else if (plot_type.eq.6) then
 var="DAERH2O"             ; name of input-variable and plot
 varname="Condensed H2O"   ; variable name used in text string:
 load_I=(/(f1_I[:]->DAERH2O)/)       ; variable to be plotted from I
 load_II=(/(f1_II[:]->DAERH2O)/)     ; variable to be plotted from II 
 else if (plot_type.eq.7) then
 var="AKCXS"             ; name of input-variable and plot
 varname="AKCXS"   ; variable name used in text string:
 load_I=(/(f1_I[:]->AKCXS)/)       ; variable to be plotted from I
 load_II=(/(f1_II[:]->AKCXS)/)     ; variable to be plotted from II 
 else if (plot_type.eq.8) then
 var="COLRBC0"             ; name of input-variable and plot
 varname="COLRBC0"   ; variable name used in text string:
; load_I=(/(f1_I[:]->COLRBC0)/)       ; variable to be plotted from I
;temporary fix
 load_I=(/(f1_I[:]->AKCXS)/)*0.0       ; variable to be plotted from I
 load_II=(/(f1_II[:]->COLRBC0)/)     ; variable to be plotted from II 
 else if (plot_type.eq.9) then
 var="COLRBC2"             ; name of input-variable and plot
 varname="COLRBC2"   ; variable name used in text string:
; load_I=(/(f1_I[:]->COLRBC2)/)       ; variable to be plotted from I
;temporary fix
 load_I=(/(f1_I[:]->AKCXS)/)*0.0       ; variable to be plotted from I
 load_II=(/(f1_II[:]->COLRBC2)/)     ; variable to be plotted from II 
 else if (plot_type.eq.10) then
 var="COLRBC4"             ; name of input-variable and plot
 varname="COLRBC4"   ; variable name used in text string:
; load_I=(/(f1_I[:]->COLRBC4)/)       ; variable to be plotted from I
;temporary fix
 load_I=(/(f1_I[:]->AKCXS)/)*0.0       ; variable to be plotted from I
 load_II=(/(f1_II[:]->COLRBC4)/)     ; variable to be plotted from II 
 else if (plot_type.eq.11) then
 var="COLRBC12"             ; name of input-variable and plot
 varname="COLRBC12"   ; variable name used in text string:
; load_I=(/(f1_I[:]->COLRBC12)/)       ; variable to be plotted from I
;temporary fix
 load_I=(/(f1_I[:]->AKCXS)/)*0.0       ; variable to be plotted from I
 load_II=(/(f1_II[:]->COLRBC12)/)     ; variable to be plotted from II 
 else if (plot_type.eq.10) then
 var="COLRBC4"             ; name of input-variable and plot
 varname="COLRBC4"   ; variable name used in text string:
; load_I=(/(f1_I[:]->COLRBC4)/)       ; variable to be plotted from I
;temporary fix
 load_I=(/(f1_I[:]->AKCXS)/)*0.0       ; variable to be plotted from I
 load_II=(/(f1_II[:]->COLRBC4)/)     ; variable to be plotted from II 
 else if (plot_type.eq.12) then
 var="COLRBC14"             ; name of input-variable and plot
 varname="COLRBC14"   ; variable name used in text string:
; load_I=(/(f1_I[:]->COLRBC14)/)       ; variable to be plotted from I
;temporary fix
 load_I=(/(f1_I[:]->AKCXS)/)*0.0       ; variable to be plotted from I
 load_II=(/(f1_II[:]->COLRBC14)/)     ; variable to be plotted from II 
 else if (plot_type.eq.13) then
 var="COLRBCAC"             ; name of input-variable and plot
 varname="COLRBCAC"   ; variable name used in text string:
; load_I=(/(f1_I[:]->COLRBCAC)/)       ; variable to be plotted from I
;temporary fix
 load_I=(/(f1_I[:]->AKCXS)/)*0.0       ; variable to be plotted from I
 load_II=(/(f1_II[:]->COLRBCAC)/)     ; variable to be plotted from II 
 else if (plot_type.eq.14) then
 var="COLROC4"             ; name of input-variable and plot
 varname="COLROC4"   ; variable name used in text string:
; load_I=(/(f1_I[:]->COLROC4)/)       ; variable to be plotted from I
;temporary fix
 load_I=(/(f1_I[:]->AKCXS)/)*0.0       ; variable to be plotted from I
 load_II=(/(f1_II[:]->COLROC4)/)     ; variable to be plotted from II 
 else if (plot_type.eq.15) then
 var="COLROC14"             ; name of input-variable and plot
 varname="COLROC14"   ; variable name used in text string:
; load_I=(/(f1_I[:]->COLROC14)/)       ; variable to be plotted from I
;temporary fix
 load_I=(/(f1_I[:]->AKCXS)/)*0.0       ; variable to be plotted from I
 load_II=(/(f1_II[:]->COLROC14)/)     ; variable to be plotted from II 
 else if (plot_type.eq.16) then
 var="COLROCAC"             ; name of input-variable and plot
 varname="COLROCAC"   ; variable name used in text string:
; load_I=(/(f1_I[:]->COLROCAC)/)       ; variable to be plotted from I
;temporary fix
 load_I=(/(f1_I[:]->AKCXS)/)*0.0       ; variable to be plotted from I
 load_II=(/(f1_II[:]->COLROCAC)/)     ; variable to be plotted from II 
 else if (plot_type.eq.17) then
 var="COLRSUL1"             ; name of input-variable and plot
 varname="COLRSUL1"   ; variable name used in text string:
; load_I=(/(f1_I[:]->COLRSUL1)/)       ; variable to be plotted from I
;temporary fix
 load_I=(/(f1_I[:]->AKCXS)/)*0.0       ; variable to be plotted from I
 load_II=(/(f1_II[:]->COLRSUL1)/)     ; variable to be plotted from II 
 else if (plot_type.eq.18) then
 var="COLRSUL5"             ; name of input-variable and plot
 varname="COLRSUL5"   ; variable name used in text string:
; load_I=(/(f1_I[:]->COLRSUL5)/)       ; variable to be plotted from I
;temporary fix
 load_I=(/(f1_I[:]->AKCXS)/)*0.0       ; variable to be plotted from I
 load_II=(/(f1_II[:]->COLRSUL5)/)     ; variable to be plotted from II 
 else if (plot_type.eq.19) then
 var="COLRSULA"             ; name of input-variable and plot
 varname="COLRSULA"   ; variable name used in text string:
; load_I=(/(f1_I[:]->COLRSULA)/)       ; variable to be plotted from I
;temporary fix
 load_I=(/(f1_I[:]->AKCXS)/)*0.0       ; variable to be plotted from I
 load_II=(/(f1_II[:]->COLRSULA)/)     ; variable to be plotted from II 
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
 ;print(ydims_II)
 do i=0,dimsizes(gw0_II)-1
 load_IIa(:,i,:)=load_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 end do

; Defining color scales for each load variable
if (var .eq. "C_SO2" .or.  var .eq. "C_SO4") then
 digg=(/0.1,0.2,0.3,0.5,1,2,3,5,10,20/)                      ; C_SO2 & C_SO4
; else if (var .eq. "C_BC") then
 else if (var .eq. "C_BC" .or.  var .eq. "C_DMS") then
  digg=(/0.03,0.05,0.1,0.2,0.3,0.5,1,2,3,5/)                 ; C_BC & C_DMS
  else if (var .eq. "C_POM" .or. var .eq. "C_SS") then
   digg=(/0.3,0.5,1,2,3,5,10,20,30,50/)                      ; C_POM & C_SS
   else if (var .eq. "C_DUST") then
    digg=(/0.5,1,2.5,5,10,25,50,100,250,500/)                ; C_DUST
    else if (var .eq. "DAERH2O") then
     digg=(/0.5,1,2.5,5,10,25,50,100,250,500/)               ; C_H2O
     else if (var .eq. "AKCXS") then
      digg=(/0.01,0.02,0.03,0.05,.1,.2,.3,.5/)               ; AKCXS
       else 
      digg=(/0.8,0.85,0.95,0.99,0.995,1.0,1.005,1.01,1.025,1.05/) ; COLR*
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
if(plot_type.le.7) then
  res@cnLevels       =   sprintf("%5.2f",digg)             ; min level
else
  res@cnLevels       =   sprintf("%5.3f",digg)             ; min level
end if

;  res@tiMainString   = "ModI"
if (var .eq. "C_SO2" .or.  var .eq. "C_SO4" .or.  var .eq. "C_DMS") then
  res@gsnRightString   = "avg = "+sprintf("%4.2f",(sum(dim_avg_n(load_Ia,0))/area1))+" mg S m~S~-2~N~"
else if (var .eq. "AKCXS") then
  res@gsnRightString  = "avg = "+sprintf("%6.4f",(sum(dim_avg_n(load_Ia,0))/area1))+" mg m~S~-2~N~"
else if (plot_type .ge. 8) then
  res@gsnRightString  = "avg = "+sprintf("%6.4f",(sum(dim_avg_n(load_Ia,0))/area1))
else
  res@gsnRightString   = "avg = "+sprintf("%4.2f",(sum(dim_avg_n(load_Ia,0))/area1))+" mg m~S~-2~N~"
end if
end if
end if
if (plot_type.le.7) then
  res@gsnLeftString   = varname + " column burden"
else
  res@gsnLeftString   = varname + " column burden ratio"
end if
  plot(0) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(load_I,0),res)    ; create the plot

;  res@tiMainString   = "ModII"
if (var .eq. "C_SO2" .or.  var .eq. "C_SO4" .or.  var .eq. "C_DMS") then
  res@gsnRightString  = "avg = "+sprintf("%4.2f",(sum(dim_avg_n(load_IIa,0))/area1))+" mg S m~S~-2~N~"
else if (var .eq. "AKCXS") then
  res@gsnRightString  = "avg = "+sprintf("%6.4f",(sum(dim_avg_n(load_IIa,0))/area1))+" mg m~S~-2~N~"
else if (plot_type .ge. 8) then
  res@gsnRightString  = "avg = "+sprintf("%6.4f",(sum(dim_avg_n(load_IIa,0))/area1))
else
  res@gsnRightString  = "avg = "+sprintf("%4.2f",(sum(dim_avg_n(load_IIa,0))/area1))+" mg m~S~-2~N~"
end if
end if
end if
if (plot_type.le.7) then
  res@gsnLeftString   = varname + " column burden"
else
  res@gsnLeftString   = varname + " column burden ratio"
end if
  plot(1) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(load_II,0),res)   ; create the plot

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
