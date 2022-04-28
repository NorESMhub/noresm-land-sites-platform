
   xlat = asciiread("lat.dat",-1,"float") 
   xlon = asciiread("lon.dat",-1,"float") 
   stname = asciiread("st.dat",-1,"string") 

   xlon = where(xlon.lt.0., xlon + 360., xlon) 

   nlon = dimsizes(xlon) 
   nlat = dimsizes(xlat) 

   lat = new((/nlat/),"float")  
   lon = new((/nlon/),"float")  

   lat!0 = "st" 
   lat&st = ispan(1,nlat,1)
   lat = (/ xlat /)

   lon!0 = "st" 
   lon&st = ispan(1,nlon,1)
   lon = (/ xlon /)

   fno = "latlon.nc" 

   system("rm "+fno) 

   fla = addfile(fno,"c") 

   fla->lat=lat 
   fla->lon=lon

   wallClock = systemfunc("date")

;;........................................................................................
;; file provenance
;;........................................................................................

   fatt                          = True
   fatt@experiment               = "INSITU"
   fatt@source                   = "https://wiki.met.no/_media/aerocom/INSITU_Station_Inventory.xlsx"
   fatt@creat_time               = wallClock

   fatt@station_name = ""

   do is = nlat-1,0,1
      it = is + 1 
      myname = "station_name_" + it 
      fatt@$myname$         = stname(is) 
   end do

   fileattdef( fla, fatt )




