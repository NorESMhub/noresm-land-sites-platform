  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in aerosol and droplet properties from two versions of
; NorESM / CAM-Oslo and makes global plots of the annually averaged differences 
; between PD and PI, including global average as a number in the title line for 
; each figure.

; Model independent constants
 g=9.80665
 pi=3.1415926
 re=6378.39e3            ; earth radius in m
 coffa=pi*re^2./180.      
 area1=4.*pi*re^2          
 small=1.0e-15

; *************************************************************************
; **** To be edited by the user if the ncl script is run interactively ****
;
; Define plot type and plot output format
 if (.not. isvar("plot_type")) then      ; is plot_type on command line?
  plot_type = 3                      ; 0 => AOD at 550 nm, PD - PI
                                     ; 1 => ABS at 550 nm, PD - PI
                                     ; 2 => CDNCINT, PD - PI
                                     ; 3 => SO4 column burden, PD - PI
                                     ; 4 => POM column burden, PD - PI
                                     ; 5 => BC column burden, PD - PI
                                     ; 6 => Dust column burden, PD - PI
                                     ; 7 => Sea-salt column burden, PD - PI
                                     ; 8 => BC MEC based on PD - PI AOD and Burdens
                                     ; 9 => BC MABS based on PD - PI ABS and Burdens
                                     ;10 => SO4 AOD at 550 nm, PD - PI
                                     ;11 => POM AOD at 550 nm, PD - PI
                                     ;12 => BC AOD at 550 nm, PD - PI
                                     ;13 => DU AOD at 550 nm, PD - PI
                                     ;14 => SS AOD at 550 nm, PD - PI
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
      all_filesPD_I  = systemfunc ("ls " + filepathPD_I + filenamepPD_I + "*")
      all_filesPD_II = systemfunc ("ls " + filepathPD_II + filenamepPD_II + "*")
      f0PD_I      = addfile (filepathPD_I+filenamePD_I, "r")
      f0PD_II     = addfile (filepathPD_II+filenamePD_II, "r")
      f1PD_I      = addfiles (all_filesPD_I, "r")   ; note the "s" of addfile
      f1PD_II     = addfiles (all_filesPD_II, "r")   ; note the "s" of addfile
      all_filesPI_I  = systemfunc ("ls " + filepathPI_I + filenamepPI_I + "*")
      all_filesPI_II = systemfunc ("ls " + filepathPI_II + filenamepPI_II + "*")
      f1PI_I      = addfiles (all_filesPI_I, "r")   ; note the "s" of addfile
      f1PI_II     = addfiles (all_filesPI_II, "r")   ; note the "s" of addfile

; Reading Gaussian weights and other required model variables
 gw0_I=doubletofloat(f0PD_I->gw)
 gw0_II=doubletofloat(f0PD_II->gw)

 lon_I=f0PD_I->lon
 dlon_I=360./dimsizes(lon_I)
 lon_II=f0PD_II->lon
 dlon_II=360./dimsizes(lon_II)
  
; Initialization (and obtain correct variable dimensions)
 tmp_I=f1PD_I[:]->PS
 tmp_II=f1PD_II[:]->PS         
 forc_I=tmp_I
 forc_II=tmp_II
 
 if (plot_type.eq.0) then
 var="dDOD550"                                              ; name of plot
 varname="Anthropogenic AOD at 550nm"                       ; variable name used in text string:
 forc_I=(/(f1PD_I[:]->DOD550)/)-(/(f1PI_I[:]->DOD550)/)     ; variable to be plotted from I
 forc_II=(/(f1PD_II[:]->DOD550)/)-(/(f1PI_II[:]->DOD550)/)
 else if (plot_type.eq.1) then
 var="dABS550" 
 varname="Anthropogenic ABS at 550nm"
 forc_I=(/(f1PD_I[:]->ABS550)/)-(/(f1PI_I[:]->ABS550)/) 
 forc_II=(/(f1PD_II[:]->ABS550)/)-(/(f1PI_II[:]->ABS550)/)
 else if (plot_type.eq.2) then
 var="dCDNUMC2"
 varname="Anthrop. CDNC col." 
 if(ModI.eq."CAM4-Oslo") then
  forc_I=1.e-6*((/(f1PD_I[:]->CLDTOT)/)*(/(f1PD_I[:]->CDNCINT)/)-(/(f1PI_I[:]->CLDTOT)/)*(/(f1PI_I[:]->CDNCINT)/))
 else
  forc_I=1.e-10*((/(f1PD_I[:]->CDNUMC)/)-(/(f1PI_I[:]->CDNUMC)/))
 end if
 forc_II=1.e-10*((/(f1PD_II[:]->CDNUMC)/)-(/(f1PI_II[:]->CDNUMC)/))
 else if (plot_type.eq.3) then
 var="dC_SO4"
 varname="Anthropogenic SO4 column burden"
 if(ModI.eq."CAM4-Oslo") then
   forc_I=((/(f1PD_I[:]->C_SO4)/)-(/(f1PI_I[:]->C_SO4)/))*1.e6
 else
   forc_I=(/(f1PD_I[:]->cb_SO4_A1)/)/3.06 + (/(f1PD_I[:]->cb_SO4_A2)/)/3.59 + (/(f1PD_I[:]->cb_SO4_AC)/)/3.06 + (/(f1PD_I[:]->cb_SO4_NA)/)/3.06 + (/(f1PD_I[:]->cb_SO4_PR)/)/3.06 + (/(f1PD_I[:]->cb_SO4_A1_OCW)/)/3.06 + (/(f1PD_I[:]->cb_SO4_A2_OCW)/)/3.59 + (/(f1PD_I[:]->cb_SO4_AC_OCW)/)/3.06 + (/(f1PD_I[:]->cb_SO4_NA_OCW)/)/3.06 + (/(f1PD_I[:]->cb_SO4_PR_OCW)/)/3.06
   forc_I=forc_I-((/(f1PI_I[:]->cb_SO4_A1)/)/3.06 + (/(f1PI_I[:]->cb_SO4_A2)/)/3.59 + (/(f1PI_I[:]->cb_SO4_AC)/)/3.06  + (/(f1PI_I[:]->cb_SO4_NA)/)/3.06 + (/(f1PI_I[:]->cb_SO4_PR)/)/3.06 + (/(f1PI_I[:]->cb_SO4_A1_OCW)/)/3.06 + (/(f1PI_I[:]->cb_SO4_A2_OCW)/)/3.59 + (/(f1PI_I[:]->cb_SO4_AC_OCW)/)/3.06 + (/(f1PI_I[:]->cb_SO4_NA_OCW)/)/3.06 + (/(f1PI_I[:]->cb_SO4_PR_OCW)/)/3.06)
   forc_I=forc_I*1.e6
 end if
 forc_II=(/(f1PD_II[:]->cb_SO4_A1)/)/3.06 + (/(f1PD_II[:]->cb_SO4_A2)/)/3.59 + (/(f1PD_II[:]->cb_SO4_AC)/)/3.06 + (/(f1PD_II[:]->cb_SO4_NA)/)/3.06 + (/(f1PD_II[:]->cb_SO4_PR)/)/3.06 + (/(f1PD_II[:]->cb_SO4_A1_OCW)/)/3.06 + (/(f1PD_II[:]->cb_SO4_A2_OCW)/)/3.59 + (/(f1PD_II[:]->cb_SO4_AC_OCW)/)/3.06 + (/(f1PD_II[:]->cb_SO4_NA_OCW)/)/3.06 + (/(f1PD_II[:]->cb_SO4_PR_OCW)/)/3.06
 forc_II=forc_II-((/(f1PI_II[:]->cb_SO4_A1)/)/3.06 + (/(f1PI_II[:]->cb_SO4_A2)/)/3.59 + (/(f1PI_II[:]->cb_SO4_AC)/)/3.06 + (/(f1PI_II[:]->cb_SO4_NA)/)/3.06 + (/(f1PI_II[:]->cb_SO4_PR)/)/3.06 + (/(f1PI_II[:]->cb_SO4_A1_OCW)/)/3.06 + (/(f1PI_II[:]->cb_SO4_A2_OCW)/)/3.59 + (/(f1PI_II[:]->cb_SO4_AC_OCW)/)/3.06 + (/(f1PI_II[:]->cb_SO4_NA_OCW)/)/3.06 + (/(f1PI_II[:]->cb_SO4_PR_OCW)/)/3.06)
 forc_II=forc_II*1.e6
 else if (plot_type.eq.4) then
 var="dC_POM"
 varname="Anthropogenic POM column burden"
 if(ModI.eq."CAM4-Oslo") then
   forc_I=((/(f1PD_I[:]->C_POM)/)-(/(f1PI_I[:]->C_POM)/))*1.e6
 else
   forc_I=((/(f1PD_I[:]->cb_OM)/)-(/(f1PI_I[:]->cb_OM)/))*1.e6
 end if
 forc_II=((/(f1PD_II[:]->cb_OM)/)-(/(f1PI_II[:]->cb_OM)/))*1.e6
 else if (plot_type.eq.5) then
 var="dC_BC"
 varname="Anthropogenic BC column burden"
 if(ModI.eq."CAM4-Oslo") then
   forc_I=((/(f1PD_I[:]->C_BC)/)-(/(f1PI_I[:]->C_BC)/))*1.e6
 else
   forc_I=((/(f1PD_I[:]->cb_BC)/)-(/(f1PI_I[:]->cb_BC)/))*1.e6
 end if
 forc_II=((/(f1PD_II[:]->cb_BC)/)-(/(f1PI_II[:]->cb_BC)/))*1.e6
 else if (plot_type.eq.6) then
 var="dC_DUST"
 varname="Anthropogenic dust column burden"
 if(ModI.eq."CAM4-Oslo") then
   forc_I=((/(f1PD_I[:]->C_DUST)/)-(/(f1PI_I[:]->C_DUST)/))*1.e6
 else
   forc_I=((/(f1PD_I[:]->cb_DUST)/)-(/(f1PI_I[:]->cb_DUST)/))*1.e6
 end if
 forc_II=((/(f1PD_II[:]->cb_DUST)/)-(/(f1PI_II[:]->cb_DUST)/))*1.e6
 else if (plot_type.eq.7) then
 var="dC_SS"
 varname="Anthropogenic sea-salt column burden"
 if(ModI.eq."CAM4-Oslo") then
   forc_I=((/(f1PD_I[:]->C_SS)/)-(/(f1PI_I[:]->C_SS)/))*1.e6
 else
   forc_I=((/(f1PD_I[:]->cb_SALT)/)-(/(f1PI_I[:]->cb_SALT)/))*1.e6
 end if
 forc_II=((/(f1PD_II[:]->cb_SALT)/)-(/(f1PI_II[:]->cb_SALT)/))*1.e6
 else if (plot_type.eq.8) then
 var="MECbcant"
 varname="MEC for Anthropogenic BC"
 aod_I=(/f1PD_I[:]->D550_BC/)-(/f1PI_I[:]->D550_BC/)
 aod_II=(/f1PD_II[:]->D550_BC/)-(/f1PI_II[:]->D550_BC/)
 if(ModI.eq."CAM4-Oslo") then
   forc_I=(((/f1PD_I[:]->D550_BC/)-(/f1PI_I[:]->D550_BC/))/((/(f1PD_I[:]->C_BC)/)-(/(f1PI_I[:]->C_BC)/)))*1.e-3
   load_I=(((/(f1PD_I[:]->C_BC)/)-(/(f1PI_I[:]->C_BC)/)))*1.e3                     
 else
   forc_I=(((/f1PD_I[:]->D550_BC/)-(/f1PI_I[:]->D550_BC/))/((/(f1PD_I[:]->cb_BC)/)-(/(f1PI_I[:]->cb_BC)/)))*1.e-3
   load_I=(((/(f1PD_I[:]->cb_BC)/)-(/(f1PI_I[:]->cb_BC)/)))*1.e3                     
 end if
 forc_II=(((/f1PD_II[:]->D550_BC/)-(/f1PI_II[:]->D550_BC/))/((/(f1PD_II[:]->cb_BC)/)-(/(f1PI_II[:]->cb_BC)/)))*1.e-3
 load_II=(((/(f1PD_II[:]->cb_BC)/)-(/(f1PI_II[:]->cb_BC)/)))*1.e3                     
 else if (plot_type.eq.9) then
 var="MACbcant"
 varname="MAC for Anthropogenic BC"
 aod_I=(/f1PD_I[:]->A550_BC/)-(/f1PI_I[:]->A550_BC/)
 aod_II=(/f1PD_II[:]->A550_BC/)-(/f1PI_II[:]->A550_BC/)
 if(ModI.eq."CAM4-Oslo") then
   forc_I=(((/f1PD_I[:]->A550_BC/)-(/f1PI_I[:]->A550_BC/))/((/(f1PD_I[:]->C_BC)/)-(/(f1PI_I[:]->C_BC)/)))*1.e-3
   load_I=(((/(f1PD_I[:]->C_BC)/)-(/(f1PI_I[:]->C_BC)/)))*1.e3                     
 else
   forc_I=(((/f1PD_I[:]->A550_BC/)-(/f1PI_I[:]->A550_BC/))/((/(f1PD_I[:]->cb_BC)/)-(/(f1PI_I[:]->cb_BC)/)))*1.e-3
   load_I=(((/(f1PD_I[:]->cb_BC)/)-(/(f1PI_I[:]->cb_BC)/)))*1.e3                     
 end if
 forc_II=(((/f1PD_II[:]->A550_BC/)-(/f1PI_II[:]->A550_BC/))/((/(f1PD_II[:]->cb_BC)/)-(/(f1PI_II[:]->cb_BC)/)))*1.e-3
 load_II=(((/(f1PD_II[:]->cb_BC)/)-(/(f1PI_II[:]->cb_BC)/)))*1.e3                     

 else if (plot_type.eq.10) then
 var="dD550so4"                                             
 varname="Anthropogenic SO4 AOD at 550nm"                       
 forc_I=(/(f1PD_I[:]->D550_SO4)/)-(/(f1PI_I[:]->D550_SO4)/)     
 forc_II=(/(f1PD_II[:]->D550_SO4)/)-(/(f1PI_II[:]->D550_SO4)/)
 else if (plot_type.eq.11) then
 var="dD550pom"                                             
 varname="Anthropogenic POM AOD at 550nm"                       
 forc_I=(/(f1PD_I[:]->D550_POM)/)-(/(f1PI_I[:]->D550_POM)/)     
 forc_II=(/(f1PD_II[:]->D550_POM)/)-(/(f1PI_II[:]->D550_POM)/)
 else if (plot_type.eq.12) then
 var="dD550bc"                                             
 varname="Anthropogenic BC AOD at 550nm"                       
 forc_I=(/(f1PD_I[:]->D550_BC)/)-(/(f1PI_I[:]->D550_BC)/)     
 forc_II=(/(f1PD_II[:]->D550_BC)/)-(/(f1PI_II[:]->D550_BC)/)
 else if (plot_type.eq.13) then
 var="dD550du"                                             
 varname="Anthropogenic DU AOD at 550nm"                       
 forc_I=(/(f1PD_I[:]->D550_DU)/)-(/(f1PI_I[:]->D550_DU)/)     
 forc_II=(/(f1PD_II[:]->D550_DU)/)-(/(f1PI_II[:]->D550_DU)/)
 else if (plot_type.eq.14) then
 var="dD550ss"                                             
 varname="Anthropogenic SS AOD at 550nm"                       
 forc_I=(/(f1PD_I[:]->D550_SS)/)-(/(f1PI_I[:]->D550_SS)/)     
 forc_II=(/(f1PD_II[:]->D550_SS)/)-(/(f1PI_II[:]->D550_SS)/)
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

; Calculating area weighted forcings

 forc_Ia=forc_I              ; initialization of global average variable
 forc_IIa=forc_II
if (plot_type.ge.8) then
 aod_Ia=forc_I 
 aod_IIa=forc_II
 load_Ia=forc_I
 load_IIa=forc_II
end if

 xdims_I = dimsizes(gw0_I)
 ;print(xdims_I)
 ydims_I = dimsizes(forc_Ia)
 ;print(ydims_I)
 do i=0,dimsizes(gw0_I)-1
 forc_Ia(:,i,:)=forc_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 end do
 forcave_I=sum(dim_avg_n(forc_Ia,0))/area1

if (plot_type.eq.8 .or. plot_type.eq.9) then
 do i=0,dimsizes(gw0_I)-1
 aod_Ia(:,i,:)=aod_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 load_Ia(:,i,:)=load_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 end do
 aodave_I=sum(dim_avg_n(aod_Ia,0))/area1
 loadave_I=sum(dim_avg_n(load_Ia,0))/area1  
end if

 xdims_II = dimsizes(gw0_II)
 ;print(xdims_I)
 ydims_II = dimsizes(forc_IIa)
 ;print(ydims_II)
 do i=0,dimsizes(gw0_II)-1
 forc_IIa(:,i,:)=forc_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 end do
 forcave_II=sum(dim_avg_n(forc_IIa,0))/area1

if (plot_type.eq.8 .or. plot_type.eq.9) then
 do i=0,dimsizes(gw0_II)-1
 aod_IIa(:,i,:)=aod_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 load_IIa(:,i,:)=load_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 end do
 aodave_II=sum(dim_avg_n(aod_IIa,0))/area1
 loadave_II=sum(dim_avg_n(load_IIa,0))/area1  
end if

; Defining color scales for each forcing variable
if (var.eq."dDOD550".or.var.eq."dD550so4".or.var.eq."dD550pom".or.var.eq."dD550bc".or.var.eq."dD550du".or.var.eq."dD550ss") then
 digg=(/-0.2,-.05,0,0.01,0.02,0.03,0.05,0.1,0.2,0.3/)
 else if (var .eq. "dABS550") then
  digg=(/-.01,0,0.001,0.002,0.003,0.005,0.01,0.02,0.03/)
  else if (var .eq. "dCDNUMC2") then
   digg=(/-0.1,0,0.1,0.2,0.3,0.5,1,2,5,10/)
   else if (var .eq. "dC_SO4") then
;    digg=(/0.05,0.1,0.2,0.3,0.5,1,1.5,2,3,5/)
    digg=(/0,0.05,0.1,0.2,0.3,0.5,1,2,3,5/)
    else if (var .eq. "dC_POM") then
     digg=(/-5,-2.5,-1,-0.5,0,0.5,1,2.5,5,10,15/)
      else if (var .eq. "dC_BC") then
       digg=(/-0.2,-0.1,0,0.1,0.2,0.3,0.5,1,1.5,2/)
        else if (var .eq. "dC_DUST".or.var .eq. "dC_SS") then
         digg=(/-0.5,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.5/)
       else  ; MECbcant
         digg=(/1,2,3,4,5,7,10,15,20,30/)
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
if (var.eq."dDOD550".or.var.eq."dABS550".or.var.eq."dD550so4".or.var.eq."dD550pom".or.var.eq."dD550bc".or.var.eq."dD550du".or.var.eq."dD550ss") then
  res@gsnRightString   = "avg = "+sprintf("%6.4f",(sum(dim_avg_n(forc_Ia,0))/area1))
else if (var.eq."dCDNUMC2") then
 if(ModI.eq."CAM4-Oslo") then
  res@gsnRightString   = "(CDNCINT*CLDTOT) avg = "+sprintf("%5.3f",(sum(dim_avg_n(forc_Ia,0))/area1))+" (10~S~6~N~ cm~S~-2~N~)"
 else
  res@gsnRightString  = "(CDNUMC) avg = "+sprintf("%5.3f",(sum(dim_avg_n(forc_Ia,0))/area1))+" (10~S~6~N~ cm~S~-2~N~)" 
 end if
else if (var.eq."dC_SO4") then
  res@gsnRightString   = "avg = "+sprintf("%5.3f",(sum(dim_avg_n(forc_Ia,0))/area1))+" mg S m~S~-2~N~"
else if (var.eq."dC_POM".or.var.eq."dC_BC".or.var.eq."dC_DUST".or.var.eq."dC_SS") then
  res@gsnRightString   = "avg = "+sprintf("%5.3f",(sum(dim_avg_n(forc_Ia,0))/area1))+" mg m~S~-2~N~"
else if (var.eq."MECbcant".or.var.eq."MACbcant") then
  res@gsnRightString   = "avg = "+sprintf("%5.2f",forcave_I)+" ("+sprintf("%4.2f",aodave_I/loadave_I)+") m~S~2~N~ g~S~-1~N~" 
else
  res@gsnRightString   = "avg = "+sprintf("%5.3f",(sum(dim_avg_n(forc_Ia,0))/area1))
end if
end if
end if
end if
end if
  res@gsnLeftString   = varname
  plot(0) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(forc_I,0),res)    ; create the plot

;  res@tiMainString   = "CAM5-Oslo"
if (var.eq."dDOD550".or.var.eq."dABS550".or.var.eq."dD550so4".or.var.eq."dD550pom".or.var.eq."dD550bc".or.var.eq."dD550du".or.var.eq."dD550ss") then
  res@gsnRightString  = "avg = "+sprintf("%6.4f",(sum(dim_avg_n(forc_IIa,0))/area1))
else if (var.eq."dCDNUMC2") then
  res@gsnRightString  = "(CDNUMC) avg = "+sprintf("%5.3f",(sum(dim_avg_n(forc_IIa,0))/area1))+" (10~S~6~N~ cm~S~-2~N~)"
else if (var.eq."dC_SO4") then
  res@gsnRightString  = "avg = "+sprintf("%5.3f",(sum(dim_avg_n(forc_IIa,0))/area1))+" mg S m~S~-2~N~"
else if (var.eq."dC_POM".or.var.eq."dC_BC".or.var.eq."dC_DUST".or.var.eq."dC_SS") then
  res@gsnRightString  = "avg = "+sprintf("%5.3f",(sum(dim_avg_n(forc_IIa,0))/area1))+" mg m~S~-2~N~"
else if (var.eq."MECbcant".or.var.eq."MACbcant") then
  res@gsnRightString   = "avg = "+sprintf("%5.2f",forcave_II)+" ("+sprintf("%4.2f",aodave_II/loadave_II)+") m~S~2~N~ g~S~-1~N~" 
else
  res@gsnRightString  = "avg = "+sprintf("%5.3f",(sum(dim_avg_n(forc_IIa,0))/area1))+"endre! W m~S~-2~N~"
end if
end if
end if
end if
end if
  res@gsnLeftString   = varname
  plot(1) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(forc_II,0),res)   ; create the plot

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
