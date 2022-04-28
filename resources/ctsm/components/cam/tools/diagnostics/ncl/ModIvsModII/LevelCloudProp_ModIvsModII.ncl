  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; This ncl script reads in 3d cloud cover or ambient relative humidity or liquid
; or ice water content from two versions of NorESM/CAM-Oslo and makes global plots 
; of annually averaged variables at a certain eta-level. 

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
  plot_type = 1                      ; 1 => CDNC    Cloud droplet number concentration
                                     ; 2 => REFFL   Cloud droplet effective radius
                                     ; 3 => ICNC    Ice crystal number concentration
                                     ; 4 => CCN1    CCN at 0.02% supersaturation
                                     ; 5 => CCN2    CCN at 0.05% supersaturation
                                     ; 6 => CCN3    CCN at 0.1% supersaturation
                                     ; 7 => CCN4    CCN at 0.2% supersaturation
                                     ; 8 => CCN5    CCN at 0.5% supersaturation
                                     ; 9 => CCN6    CCN at 1.0% supersaturation
                                     ;10 => N_AER   Aerosol number concentration
                                     ;11 => CDNC_B  Cloud top CDNC for comparison with Bennartz et al. (2017)
                                     ;12 => KLEB_B  Level where CDNC_B is taken out
                                     ;13 => SP REFFL Cloud droplet effective radius with MACv2-SP included
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
varlev_I=f1_I[:]->PS
varlev_II=f1_II[:]->PS

;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
; Note: layers 19 and 20 are at 446 and 525 hPa in CAM5 L32
; Note: layers 25 and 26 are at 860 and 887 hPa in CAM5 L32
; Note: layers 17 and 18 are at 446 and 525 hPa in CAM5 L30
; Note: layers 23 and 24 are at 860 and 887 hPa in CAM5 L30
; Note: layers 21 and 22 are at 787 and 867 hPa in CAM4
; (if 0 is the first model layer, as costomary in ncl)
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

 if(LevModI .eq. "L32") then
  L446_I=19
  L860_I=25
 else
  L446_I=17
  L860_I=23
 end if
 if(LevModII .eq. "L32") then
  L446_II=19
  L860_II=25
 else
  L446_II=17
  L860_II=23
 end if

 if (plot_type.eq.1) then
 var="CDNC"                      ; name of plot
 varname="CDNC"                  ; variable name used in text string
 plot_name="CDNC860hPa"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->CDNC)/)/((/(f1_I[:]->CLDFOC)/)+small)
  varlev_I=(/(f1_I[:]->CDNC(:,22,:,:))/)/((/(f1_I[:]->CLDFOC(:,22,:,:))/)+small)
 else
  var_I=1.e-6*(/(f1_I[:]->AWNC)/)/((/(f1_I[:]->FREQL)/)+small)
  varlev_I=1.e-6*(/(f1_I[:]->AWNC(:,L860_I,:,:))/)/((/(f1_I[:]->FREQL(:,L860_I,:,:))/)+small)
 end if
 var_II=1.e-6*(/(f1_II[:]->AWNC)/)/((/(f1_II[:]->FREQL)/)+small)
 varlev_II=1.e-6*(/(f1_II[:]->AWNC(:,L860_II,:,:))/)/((/(f1_II[:]->FREQL(:,L860_II,:,:))/)+small)
 else if (plot_type.eq.2) then
 var="REFFL"                      ; name of plot
 varname="REFFL"                  ; variable name used in text string
 plot_name="REFFL860hPa"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->REFFL)/)/((/(f1_I[:]->CLDFOC)/)+small)
  varlev_I=(/(f1_I[:]->REFFL(:,22,:,:))/)/((/(f1_I[:]->CLDFOC(:,22,:,:))/)+small)
 else
  var_I=(/(f1_I[:]->AREL)/)/((/(f1_I[:]->FREQL)/)+small)
  varlev_I=(/(f1_I[:]->AREL(:,L860_I,:,:))/)/((/(f1_I[:]->FREQL(:,L860_I,:,:))/)+small)
 end if
 var_II=(/(f1_II[:]->AREL)/)/((/(f1_II[:]->FREQL)/)+small)
 varlev_II=(/(f1_II[:]->AREL(:,L860_II,:,:))/)/((/(f1_II[:]->FREQL(:,L860_II,:,:))/)+small)
 else if (plot_type.eq.3) then
 var="ICNC"                      ; name of plot
 varname="ICNC"                  ; variable name used in text string
 plot_name="ICNC446hPa"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->CDNC)/)*0.0
  varlev_I=1.e-6*(/(f1_I[:]->CDNC(:,17,:,:))/)*0.0
 else
  var_I=1.e-6*(/(f1_I[:]->AWNI)/)/((/(f1_I[:]->FREQI)/)+small)
  varlev_I=1.e-6*(/(f1_I[:]->AWNI(:,L446_I,:,:))/)/((/(f1_I[:]->FREQI(:,L446_I,:,:))/)+small)
 end if
 var_II=1.e-6*(/(f1_II[:]->AWNI)/)/((/(f1_II[:]->FREQI)/)+small)
 varlev_II=1.e-6*(/(f1_II[:]->AWNI(:,L446_II,:,:))/)/((/(f1_II[:]->FREQI(:,L446_II,:,:))/)+small)
 else if (plot_type.eq.4) then
 var="CCN1"                      ; name of plot
 varname="CCN1"                  ; variable name used in text string
 plot_name="CCN_S0.02_860hPa"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->CDNC)/)*0.0
  varlev_I=(/(f1_I[:]->CDNC(:,22,:,:))/)*0.0
 else
  var_I=(/(f1_I[:]->CCN1)/)
  varlev_I=(/(f1_I[:]->CCN1(:,L860_I,:,:))/)
 end if
 var_II=(/(f1_II[:]->CCN1)/)
 varlev_II=(/(f1_II[:]->CCN1(:,L860_II,:,:))/)
 else if (plot_type.eq.5) then
 var="CCN2"                      ; name of plot
 varname="CCN2"                  ; variable name used in text string
 plot_name="CCN_S0.05_860hPa"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->CDNC)/)*0.0
  varlev_I=(/(f1_I[:]->CDNC(:,22,:,:))/)*0.0
 else
  var_I=(/(f1_I[:]->CCN2)/)
  varlev_I=(/(f1_I[:]->CCN2(:,L860_I,:,:))/)
 end if
 var_II=(/(f1_II[:]->CCN2)/)
 varlev_II=(/(f1_II[:]->CCN2(:,L860_II,:,:))/)
 else if (plot_type.eq.6) then
 var="CCN3"                      ; name of plot
 varname="CCN3"                  ; variable name used in text string
 plot_name="CCN_S0.1_860hPa"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->CDNC)/)*0.0
  varlev_I=(/(f1_I[:]->CDNC(:,22,:,:))/)*0.0
 else
  var_I=(/(f1_I[:]->CCN3)/)
  varlev_I=(/(f1_I[:]->CCN3(:,L860_I,:,:))/)
 end if
 var_II=(/(f1_II[:]->CCN3)/)
 varlev_II=(/(f1_II[:]->CCN3(:,L860_II,:,:))/)
 else if (plot_type.eq.7) then
 var="CCN4"                      ; name of plot
 varname="CCN4"                  ; variable name used in text string
 plot_name="CCN_S0.2_860hPa"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->CDNC)/)*0.0
  varlev_I=(/(f1_I[:]->CDNC(:,22,:,:))/)*0.0
 else
  var_I=(/(f1_I[:]->CCN4)/)
  varlev_I=(/(f1_I[:]->CCN4(:,L860_I,:,:))/)
 end if
 var_II=(/(f1_II[:]->CCN4)/)
 varlev_II=(/(f1_II[:]->CCN4(:,L860_II,:,:))/)
 else if (plot_type.eq.8) then
 var="CCN5"                      ; name of plot
 varname="CCN5"                  ; variable name used in text string
 plot_name="CCN_S0.5_860hPa"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->CDNC)/)*0.0
  varlev_I=(/(f1_I[:]->CDNC(:,22,:,:))/)*0.0
 else
  var_I=(/(f1_I[:]->CCN5)/)
  varlev_I=(/(f1_I[:]->CCN5(:,L860_I,:,:))/)
 end if
 var_II=(/(f1_II[:]->CCN5)/)
 varlev_II=(/(f1_II[:]->CCN5(:,L860_II,:,:))/)
 else if (plot_type.eq.9) then
 var="CCN6"                      ; name of plot
 varname="CCN6"                  ; variable name used in text string
 plot_name="CCN_S1.0_860hPa"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->CDNC)/)*0.0
  varlev_I=(/(f1_I[:]->CDNC(:,22,:,:))/)*0.0
 else
  var_I=(/(f1_I[:]->CCN6)/)
  varlev_I=(/(f1_I[:]->CCN6(:,L860_I,:,:))/)
 end if
 var_II=(/(f1_II[:]->CCN6)/)
 varlev_II=(/(f1_II[:]->CCN6(:,L860_II,:,:))/)
 else if (plot_type.eq.10) then
 var="N_AER"                      ; name of plot
 varname="N_AER"                  ; variable name used in text string
 plot_name="N_AER_860hPa"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->N_AER)/)
  varlev_I=(/(f1_I[:]->N_AER(:,22,:,:))/)
 else
  var_I=(/(f1_I[:]->N_AER)/)
  varlev_I=(/(f1_I[:]->N_AER(:,L860_I,:,:))/)
 end if
 var_II=(/(f1_II[:]->N_AER)/)
 varlev_II=(/(f1_II[:]->N_AER(:,L860_II,:,:))/)
 else if (plot_type.eq.11) then
 var="CDNC_B"                      ; name of plot
 varname="CDNCcloudtop"          ; variable name used in text string
 plot_name="CDNC_B"                ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->N_AER)/)*0.0
  varlev_I=(/(f1_I[:]->N_AER(:,22,:,:))/)*0.0
 else
  var_I=1.e-6*(/(f1_I[:]->ACTNL_B)/)/((/(f1_I[:]->FCTL_B)/)+small)
  varlev_I=1.e-6*(/(f1_I[:]->ACTNL_B)/)/((/(f1_I[:]->FCTL_B)/)+small)
 end if
  var_II=1.e-6*(/(f1_II[:]->ACTNL_B)/)/((/(f1_II[:]->FCTL_B)/)+small)
  varlev_II=1.e-6*(/(f1_II[:]->ACTNL_B)/)/((/(f1_II[:]->FCTL_B)/)+small)
 else if (plot_type.eq.12) then
 var="KLEV_B"                      ; name of plot
 varname="CDNC_B KLEV"          ; variable name used in text string
 plot_name="KLEV_B"                ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->N_AER)/)*0.0
  varlev_I=(/(f1_I[:]->N_AER(:,22,:,:))/)*0.0
 else
  var_I=(/(f1_I[:]->KLEV_B)/)/((/(f1_I[:]->FCTL_B)/)+small)
  varlev_I=(/(f1_I[:]->KLEV_B)/)/((/(f1_I[:]->FCTL_B)/)+small)
 end if
 var_II=(/(f1_II[:]->KLEV_B)/)/((/(f1_II[:]->FCTL_B)/)+small)
 varlev_II=(/(f1_II[:]->KLEV_B)/)/((/(f1_II[:]->FCTL_B)/)+small)

 else if (plot_type.eq.13) then
 var="SPREFFL"                      ; name of plot
 varname="SPREFFL"                  ; variable name used in text string
 plot_name="SPREFFL860hPa"          ; name of the plot/figure
 if(ModI.eq."CAM4-Oslo") then
  var_I=(/(f1_I[:]->REFFL)/)/((/(f1_I[:]->CLDFOC)/)+small)
  varlev_I=(/(f1_I[:]->REFFL(:,22,:,:))/)/((/(f1_I[:]->CLDFOC(:,22,:,:))/)+small)
 else
  var_I=(/(f1_I[:]->AREL)/)/((/(f1_I[:]->FREQL)/)+small)
  varlev_I=(/(f1_I[:]->AREL(:,L860_I,:,:))/)/((/(f1_I[:]->FREQL(:,L860_I,:,:))/)+small)
 end if
 var_II=(/(f1_II[:]->SPAREL)/)/((/(f1_II[:]->FREQL)/)+small)
 varlev_II=(/(f1_II[:]->SPAREL(:,L860_II,:,:))/)/((/(f1_II[:]->FREQL(:,L860_II,:,:))/)+small)

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
; printVarSummary(varlev_I)
; printVarSummary(varlev_I)

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

; zave_I = dim_avg_Wrap(var_I)    ; calculate zonal ave
; zave_II = dim_avg_Wrap(var_II)  ; calculate zonal ave
; printVarSummary(zave_I)
; printVarSummary(zave_II)
;************************************************

 varlev_Ia=varlev_I              ; initialization of global average variable
 varlev_IIa=varlev_II

 xdims_I = dimsizes(gw0_I)
 ydims_I = dimsizes(varlev_I)
 do i=0,dimsizes(gw0_I)-1
   varlev_Ia(:,i,:)=varlev_I(:,i,:)*coffa*dlon_I*gw0_I(i)
 end do
 xdims_II = dimsizes(gw0_II)
 ydims_II = dimsizes(varlev_II)
 do i=0,dimsizes(gw0_II)-1
   varlev_IIa(:,i,:)=varlev_II(:,i,:)*coffa*dlon_II*gw0_II(i)
 end do

; Defining color scales for each meteorology variable
if (var .eq. "CDNC") then
;  digg=(/10,15,20,30,40,50,75,100,150,200/)
    digg=(/1,5,10,25,50,75,100,150,200,500/)
   else if (var .eq. "REFFL" .or. var .eq. "SPREFFL") then
    digg=(/0,2,4,5,6,8,10,12,14,16/)
   else if (plot_type .eq. 10) then
    digg=(/25,50,100,500,1000,2500,5000,7500,10000,15000/)
   else if (plot_type .ge. 4 .and. plot_type .le. 9) then
;    digg=(/1,2.5,5,10,25,50,75,100,150,200/)
    digg=(/1,5,10,25,50,75,100,150,200,500/)
    else if (plot_type .eq. 11) then
     digg=(/5,10,20,40,60,80,100,120,140,160/)
    else  ; for ICNC
     digg= (/.0005,.001,.003,.005,.01,.03,.05,.1,.3,.5/)
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

  wks = gsn_open_wks(format,plot_name)

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

;  res@sfYArray              = eta_I                  ; use eta for y axis
;  res@sfXArray              = lat_I                  ; use lat for x axis
;  res@sfYArray              = lat_I
;  res@sfXArray              = lon_I
;  res@tiXAxisString         = "latitude"           ; x-axis label
;  res@tiYAxisString         = "eta x 1000"         ; y-axis label
;  res@trXReverse            = False                ; reverse x-axis
;  res@trYReverse            = True                 ; reverse y-axis
;  res@trYReverse            = False                 ; reverse y-axis
;  res@gsnYAxisIrregular2Log = True                 ; set y-axis to log scale

  res@cnFillColors   = (/3,5,6,8,9,10,11,12,13,14,15,16/) 
  res@cnLevels       =   sprintf("%7.5f",digg)             ; min level


if (var .eq. "CDNC") then
res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_Ia,0))/area1))
 if(ModI.eq."CAM4-Oslo") then
  res@gsnLeftString   = "CDNC/CLDFOC (cm~S~-3~N~) at 867 hPa"
 else
  res@gsnLeftString   = "AWNC/FREQL (cm~S~-3~N~) at 860 hPa"
 end if
else if (var .eq. "REFFL" .or. var .eq. "SPREFFL") then
res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_Ia,0))/area1))
 if(ModI.eq."CAM4-Oslo") then
  res@gsnLeftString   = "REFFL/CLDFOC (~F33~m~F21~m) at 867 hPa"
 else
  res@gsnLeftString   = "AREL/FREQL (~F33~m~F21~m) at 860 hPa"
 end if
else if (var .eq. "ICNC") then
 res@gsnRightString   = "avg = "+sprintf("%6.4f",(sum(dim_avg_n(varlev_Ia,0))/area1))
 if(ModI.eq."CAM4-Oslo") then
  res@gsnLeftString   = "(Not available)"
 else
  res@gsnLeftString   = varname+" (cm~S~-3~N~) at 447 hPa"
 end if
else if (var .eq. "CCN1") then
res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_Ia,0))/area1))
 if(ModI.eq."CAM4-Oslo") then
  res@gsnLeftString   = "CCN(S=0.02%) (cm~S~-3~N~) at 867 hPa"
 else
  res@gsnLeftString   = "CCN(S=0.02%) (cm~S~-3~N~) at 860 hPa"
 end if
else if (var .eq. "CCN2") then
res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_Ia,0))/area1))
 if(ModI.eq."CAM4-Oslo") then
  res@gsnLeftString   = "CCN(S=0.05%) (cm~S~-3~N~) at 867 hPa"
 else
  res@gsnLeftString   = "CCN(S=0.05%) (cm~S~-3~N~) at 860 hPa"
 end if
else if (var .eq. "CCN3") then
res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_Ia,0))/area1))
 if(ModI.eq."CAM4-Oslo") then
  res@gsnLeftString   = "CCN(S=0.1%) (cm~S~-3~N~) at 867 hPa"
 else
  res@gsnLeftString   = "CCN(S=0.1%) (cm~S~-3~N~) at 860 hPa"
 end if
else if (var .eq. "CCN4") then
res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_Ia,0))/area1))
 if(ModI.eq."CAM4-Oslo") then
  res@gsnLeftString   = "CCN(S=0.2%) (cm~S~-3~N~) at 867 hPa"
 else
  res@gsnLeftString   = "CCN(S=0.2%) (cm~S~-3~N~) at 860 hPa"
 end if
else if (var .eq. "CCN5") then
res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_Ia,0))/area1))
 if(ModI.eq."CAM4-Oslo") then
  res@gsnLeftString   = "CCN(S=0.5%) (cm~S~-3~N~) at 867 hPa"
 else
  res@gsnLeftString   = "CCN(S=0.5%) (cm~S~-3~N~) at 860 hPa"
 end if
else if (var .eq. "CCN6") then
res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_Ia,0))/area1))
 if(ModI.eq."CAM4-Oslo") then
  res@gsnLeftString   = "CCN(S=1.0%) (cm~S~-3~N~) at 867 hPa"
 else
  res@gsnLeftString   = "CCN(S=1.0%) (cm~S~-3~N~) at 860 hPa"
 end if
else if (var .eq. "N_AER") then
res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_Ia,0))/area1))
 if(ModI.eq."CAM4-Oslo") then
  res@gsnLeftString   = "N_AER (cm~S~-3~N~) at 867 hPa"
 else
  res@gsnLeftString   = "N_AER (cm~S~-3~N~) at 860 hPa"
 end if
else if (var .eq. "CDNC_B") then
res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_Ia,0))/area1))
 if(ModI.eq."CAM4-Oslo") then
  res@gsnLeftString   = "Not Available "
 else
  res@gsnLeftString   = "CDNCcldtop ACTNL_B/FCTL_B (cm~S~-3~N~)"
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

;  plot(0) = gsn_contour(wks,dim_avg_n_Wrap(zave_I,0),res)    ; create the plot
  plot(0) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(varlev_I,0),res)    ; create the plot

if (var .eq. "CDNC") then
 res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_IIa,0))/area1))
 res@gsnLeftString   = "AWNC/FREQL (cm~S~-3~N~) at 860 hPa"
else if (var .eq. "REFFL") then
 res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_IIa,0))/area1))
 res@gsnLeftString   = "AREL/FREQL (~F33~m~F21~m) at 860 hPa"
else if (var .eq. "ICNC") then
 res@gsnRightString   = "avg = "+sprintf("%6.4f",(sum(dim_avg_n(varlev_IIa,0))/area1))
 res@gsnLeftString   = varname+" (cm~S~-3~N~) at 447 hPa"
else if (var .eq. "CCN1") then
 res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_IIa,0))/area1))
 res@gsnLeftString   = "CCN(S=0.02%) (cm~S~-3~N~) at 860 hPa"
else if (var .eq. "CCN2") then
 res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_IIa,0))/area1))
 res@gsnLeftString   = "CCN(S=0.05%) (cm~S~-3~N~) at 860 hPa"
else if (var .eq. "CCN3") then
 res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_IIa,0))/area1))
 res@gsnLeftString   = "CCN(S=0.1%) (cm~S~-3~N~) at 860 hPa"
else if (var .eq. "CCN4") then
 res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_IIa,0))/area1))
 res@gsnLeftString   = "CCN(S=0.2%) (cm~S~-3~N~) at 860 hPa"
else if (var .eq. "CCN5") then
 res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_IIa,0))/area1))
 res@gsnLeftString   = "CCN(S=0.5%) (cm~S~-3~N~) at 860 hPa"
else if (var .eq. "CCN6") then
 res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_IIa,0))/area1))
 res@gsnLeftString   = "CCN(S=1.0%) (cm~S~-3~N~) at 860 hPa"
else if (var .eq. "N_AER") then
 res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_IIa,0))/area1))
 res@gsnLeftString   = "N_AER (cm~S~-3~N~) at 860 hPa"
else if (var .eq. "CDNC_B") then
 res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_IIa,0))/area1))
 if(ModI.eq."CAM4-Oslo") then
  res@gsnLeftString   = "Not Available "
 else
  res@gsnLeftString   = "CDNCcldtop ACTNL_B/FCTL_B (cm~S~-3~N~)"
 end if
else if (var .eq. "SPREFFL") then
 res@gsnRightString   = "avg = "+sprintf("%5.2f",(sum(dim_avg_n(varlev_IIa,0))/area1))
 res@gsnLeftString   = "SPAREL/FREQL (~F33~m~F21~m) at 860 hPa"
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

;  plot(1) = gsn_contour(wks,dim_avg_n_Wrap(zave_II,0),res)   ; create the plot
  plot(1) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(varlev_II,0),res)    ; create the plot


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

