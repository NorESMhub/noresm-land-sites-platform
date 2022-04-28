  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in effective aerosol forcings/fluxes from two versions 
; of NorESM / CAM-Oslo and makes global plots of the annually averaged forcings 
; (ERF) including global average as a number in the title line for each figure.
; Note: LW forcings/fluxes have been defined positive downwards, as for SW.
; Hence, total forcing = SW + LW (instead of SW-LW).  

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
  plot_type = 3                      ; 0 => TOA SW direct radiative forcing as ERF + dn
                                     ; 1 => TOA SW Cloud radiative forcing as ERF  + dn
                                     ; 2 => TOA SW Surface albedo forcing as ERF   + dn
                                     ; 3 => TOA LW direct radiative forcing as ERF + dn also
                                     ; 4 => TOA LW Cloud radiative forcing as ERF  + dn also
                                     ; 5 => TOA LW Surface albedo forcing as ERF   + dn also
                                     ; 6 => total TOA LW ERF (sun of 0 to 5)
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
 var="direct_ERF"              ; name of plot
 varname="SW Direct radiative forcing at TOA"            ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
  forc_I=(/(f1PD_I[:]->FSNT_DRF)/)-(/(f1PI_I[:]->FSNT_DRF)/)     ; variable to be plotted from I
 else
  forc_I=(/(f1PD_I[:]->FSNT)/)-(/(f1PI_I[:]->FSNT)/)-((/(f1PD_I[:]->FSNT_DRF)/)-(/(f1PI_I[:]->FSNT_DRF)/))
 end if
 forc_II=(/(f1PD_II[:]->FSNT)/)-(/(f1PI_II[:]->FSNT)/)-((/(f1PD_II[:]->FSNT_DRF)/)-(/(f1PI_II[:]->FSNT_DRF)/))
 else if (plot_type.eq.1) then
 var="cloud_ERF"              ; name of input-variable and plot
 varname="SW cloud radiative forcing at TOA"            ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
  forc_I=(/(f1PD_I[:]->FSNT_AIE)/)-(/(f1PI_I[:]->FSNT_AIE)/)     ; variable to be plotted from I
 else
  forc_I=(/(f1PD_I[:]->FSNT_DRF)/)-(/(f1PI_I[:]->FSNT_DRF)/)-((/(f1PD_I[:]->FSNTCDRF)/)-(/(f1PI_I[:]->FSNTCDRF)/))
 end if
 forc_II=(/(f1PD_II[:]->FSNT_DRF)/)-(/(f1PI_II[:]->FSNT_DRF)/)-((/(f1PD_II[:]->FSNTCDRF)/)-(/(f1PI_II[:]->FSNTCDRF)/))
 else if (plot_type.eq.2) then
 var="surfalb_ERF"               ; name of input-variable and plot
 varname="SW surface albedo forcing (TOA)"             ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
  forc_I=(/(f1PD_I[:]->FSNTCDRF)/)*0.0                  ; variable to be plotted from I (no output, set = 0)
 else
  forc_I=(/(f1PD_I[:]->FSNTCDRF)/)-(/(f1PI_I[:]->FSNTCDRF)/)  ; variable to be plotted from I
 end if
 forc_II=(/(f1PD_II[:]->FSNTCDRF)/)-(/(f1PI_II[:]->FSNTCDRF)/)  ; variable to be plotted from II
 else if (plot_type.eq.3) then
 var="direct_ERF_LW"               ; name of input-variable and plot
 varname="LW Direct radiative forcing at TOA"             ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
  forc_I=(/(f1PD_I[:]->FSNT_DRF)/)*0.0
 else
  forc_I=((/(f1PD_I[:]->FLNT_DRF)/)-(/(f1PI_I[:]->FLNT_DRF)/))-((/(f1PD_I[:]->FLNT)/)-(/(f1PI_I[:]->FLNT)/))
 end if
 forc_II=((/(f1PD_II[:]->FLNT_DRF)/)-(/(f1PI_II[:]->FLNT_DRF)/))-((/(f1PD_II[:]->FLNT)/)-(/(f1PI_II[:]->FLNT)/))
 else if (plot_type.eq.4) then
 var="cloud_ERF_LW"               ; name of input-variable and plot
 varname="LW cloud radiative forcing at TOA"             ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
  forc_I=-((/(f1PD_I[:]->FLNT_AIE)/)-(/(f1PI_I[:]->FLNT_AIE)/))
 else
  forc_I=-((/(f1PD_I[:]->FLNT_DRF)/)-(/(f1PI_I[:]->FLNT_DRF)/))+((/(f1PD_I[:]->FLNTCDRF)/)-(/(f1PI_I[:]->FLNTCDRF)/))
 end if
 forc_II=-((/(f1PD_II[:]->FLNT_DRF)/)-(/(f1PI_II[:]->FLNT_DRF)/))+((/(f1PD_II[:]->FLNTCDRF)/)-(/(f1PI_II[:]->FLNTCDRF)/))
 else if (plot_type.eq.5) then
 var="surfalb_ERF_LW"               ; name of input-variable and plot
 varname="LW surface albedo forcing (TOA)"             ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
  forc_I=(/(f1PD_I[:]->FSNTCDRF)/)*0.0                  ; variable to be plotted from I (no output, set = 0)
 else
 forc_I=-((/(f1PD_I[:]->FLNTCDRF)/)-(/(f1PI_I[:]->FLNTCDRF)/))  ; variable to be plotted from I
 end if
 forc_II=-((/(f1PD_II[:]->FLNTCDRF)/)-(/(f1PI_II[:]->FLNTCDRF)/))  ; variable to be plotted from II
 else if (plot_type.eq.6) then
 var="total_ERF"               ; name of input-variable and plot
 varname="Total SW + LW ERF (TOA)"             ; variable name used in text string:
 if(ModI.eq."CAM4-Oslo") then
;    forc_I=(/(f1PD_I[:]->FSNT_DRF)/)-(/(f1PI_I[:]->FSNT_DRF)/)+(/(f1PD_I[:]->FSNT_AIE)/)-(/(f1PI_I[:]->FSNT_AIE)/)-((/(f1PD_I[:]->FLNT_AIE)/)-(/(f1PI_I[:]->FLNT_AIE)/)) 
 forc_I=(/(f1PD_I[:]->FSNT)/)-(/(f1PI_I[:]->FSNT)/)-((/(f1PD_I[:]->FLNT)/)-(/(f1PI_I[:]->FLNT)/))
 else
 forc_I=(/(f1PD_I[:]->FSNT)/)-(/(f1PI_I[:]->FSNT)/)-((/(f1PD_I[:]->FLNT)/)-(/(f1PI_I[:]->FLNT)/))
 end if
 forc_II=(/(f1PD_II[:]->FSNT)/)-(/(f1PI_II[:]->FSNT)/)-((/(f1PD_II[:]->FLNT)/)-(/(f1PI_II[:]->FLNT)/))
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

 xdims_I = dimsizes(gw0_I)
 ;print(xdims_I)
 ydims_I = dimsizes(forc_Ia)
 ;print(ydims_I)
 do i=0,dimsizes(gw0_I)-1
 forc_Ia(:,i,:)=forc_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 end do

 xdims_II = dimsizes(gw0_II)
 ;print(xdims_I)
 ydims_II = dimsizes(forc_IIa)
 ;print(ydims_II)
 do i=0,dimsizes(gw0_II)-1
 forc_IIa(:,i,:)=forc_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 end do

; Defining color scales for each forcing variable
if (var .eq. "direct_ERF") then
 digg=(/-2,-1,-0.5,-0.25,-0.1,0,0.1,0.25,0.5,1,2/)
; digg=(/-10,-5,-3,-1,-0.5,0,0.5,1,3,5,10/)
; else if (var .eq. "cloud_ERF") then
 else if (var .eq. "cloud_ERF" .or. var .eq. "total_ERF") then
;  digg=(/-10,-5,-3,-1,-0.5,0,0.5,1,3,5/)
  digg=(/-10,-5,-3,-1,-0.5,0,0.5,1,3,5,10/)
  else if (var .eq. "surfalb_ERF") then
   digg=(/-2,-1,-0.5,-0.25,-0.1,0,0.1,0.25,0.5,1,2/)
   else if (var .eq. "direct_ERF_LW") then
    digg=(/0.005,0.01,0.02,0.03,0.05,0.07,0.1,0,0.15/)
;    digg=(/-2,-1,-0.5,-0.25,-0.1,0,0.1,0.25,0.5,1,2/)
    else if (var .eq. "cloud_ERF_LW") then
;     digg=(/-2,-1,-0.5,-0.25,-0.1,0,0.1,0.25,0.5,1,2/)
     digg=(/-10,-5,-3,-1,-0.5,0,0.5,1,3,5,10/)
      else if (var .eq. "surfalb_ERF_LW") then
;      digg=(/-2,-1,-0.5,-0.25,-0.1,0,0.1,0.25,0.5,1,2/)
      digg=(/-10,-5,-3,-1,-0.5,0,0.5,1,3,5/)
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
  res@gsnRightString   = "avg = "+sprintf("%5.3f",(sum(dim_avg_n(forc_Ia,0))/area1))+" W m~S~-2~N~"
  res@gsnLeftString   = varname
  plot(0) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(forc_I,0),res)    ; create the plot

;  res@tiMainString   = "CAM5-Oslo"
  res@gsnRightString  = "avg = "+sprintf("%5.3f",(sum(dim_avg_n(forc_IIa,0))/area1))+" W m~S~-2~N~"
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
