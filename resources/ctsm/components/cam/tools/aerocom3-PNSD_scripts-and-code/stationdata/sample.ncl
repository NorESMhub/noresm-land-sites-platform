
;;  model history file 

;;    fna = "example_monthly_2D.nc"
;;    fna = "example_monthly_3D.nc"
;;    fna = "aerocom03_CAM5.3-Oslo_global-PNSD_dndlogdaer_2000_monthly.nc"
;;    fna = "aerocom03_CAM5.3-Oslo_global-PNSD_dmdlogdss_2000_monthly.nc"
    fna = "aerocom03_CAM5.3-Oslo_global-PNSD_dmdlogddust_2000_monthly.nc"

;;  site location information 

    fnb = "latlon.nc"

;;  output file 

;;    fno = "OUT_" + fna
;;    fno = "aerocom03_CAM5.3-Oslo_INSITU-PNSD_dndlogdaer_2000_monthly.nc"
;;    fno = "aerocom03_CAM5.3-Oslo_INSITU-PNSD_dmdlogdss_2000_monthly.nc"
    fno = "aerocom03_CAM5.3-Oslo_INSITU-PNSD_dmdlogddust_2000_monthly.nc"

    system("rm " + fno) 

;;...........................................................  
;;  load data
;;...........................................................  

    fla = addfile(fna, "r")   
    flb = addfile(fnb, "r")   
    flo = addfile(fno, "c")   

    lat = flb->lat
    lon = flb->lon

    ns = dimsizes(lat) ; number of stations 

    vn = getfilevarnames(fla)  ; get names of all variables on file

    nv = dimsizes (vn)   ; number of variables on the file

    time = fla->time

    print(vn)

;;...........................................................  
;;  determine if we have levels 
;;...........................................................  
    have_levels = isfilevar(fla,"lev")

    if(have_levels) then 
       hyam = fla->hyam 
       hybm = fla->hybm 
       hyai = fla->hyai
       hybi = fla->hybi
    end if 

;;...........................................................  
;;  loop over variables 
;;...........................................................  

    do n=0,nv-1

       vv = fla->$vn(n)$        ; read the varible to memory
         
       ndim = dimsizes(vv)        ; dimension size of the variable
       nran = dimsizes(ndim)      ; rank [ie: number of dimensions]

       bname = vn(n) 
       wn    = bname

;;...........................................................  
;;     3-D 
;;...........................................................  

       if(nran.eq.3) then 
          va = vv 
          n1 = dimsizes(va(:,0,0)) 
          n2 = dimsizes(va(0,:,0)) 
          n3 = dimsizes(va(0,0,:)) 

          wn = bname

          wa = new((/n1,ns/),"float") 

          wa!1 = "st"
          wa&st = ispan(1,ns,1) 
          wa&st@long_name = "station" 

          wa!0 = "time"
          wa&time = time

          wa@long_name = va@long_name
          wa@units     = va@units

          ;; get grid-box mean value 

          do is = 0, ns-1 
             wa(:,is) = (/ va(:,{lat(is)},{lon(is)}) /)  
          end do 

          print(" varname : " + wn )               ; print all variable names on file
          flo->$wn$=wa

          delete(va) 
          delete(wa) 

       end if 
 
;;...........................................................  
;;     4-D 
;;...........................................................  

       if(nran.eq.4) then 

          va = vv 

          print(dimsizes(vv))

          n1 = dimsizes(va(:,0,0,0)) 
          n2 = dimsizes(va(0,:,0,0)) 
          n3 = dimsizes(va(0,0,:,0)) 
          n4 = dimsizes(va(0,0,0,:))

          bname = vn(n)

          wn = bname

          wa = new((/n1,n2,ns/),"float")      

          wa!2 = "st"
          wa&st = ispan(1,ns,1)
          wa&st@long_name = "station"

          wa!0 = "time"
          wa&time = time

          wa!1 = "particle_diameter" 
          wa&particle_diameter = va&particle_diameter

          wa@long_name = va@long_name
          wa@units     = va@units

          ;; get grid-box mean value 

          do is = 0, ns-1
             wa(:,:,is) = (/ va(:,:,{lat(is)},{lon(is)}) /)
          end do

          print(" varname : " + wn )               ; print all variable names on file
          flo->$wn$=wa

          delete(va) 
          delete(wa) 

       end if 
 
       ;;print(" varname : " + wn )               ; print all variable names on file

       delete(ndim)
       delete(vv)

   end do


;;........................................................................................
;; Coordinate info
;;........................................................................................
   if(have_levels) then 
      flo->hyam=hyam 
      flo->hybm=hybm 
      flo->hyai=hyai 
      flo->hybi=hybi 
   end if 

   stname = asciiread("st.dat",-1,"string")

   wallClock = systemfunc("date")

;;........................................................................................
;; file provenance
;;........................................................................................

   fatt                          = True
   fatt@experiment               = "INSITU"
   fatt@source                   = "https://wiki.met.no/_media/aerocom/INSITU_Station_Inventory.xlsx"
   fatt@creat_time               = wallClock

   fatt@station_name = ""

   do is = ns-1,0,1
      it = is + 1
      myname = "station_name_" + it
      fatt@$myname$         = stname(is)
   end do

   fileattdef( flo, fatt )






