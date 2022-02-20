; Create site/regional lightning data for the model platform 
; The origional global file used by default is too large.
; This script is adapated from the original "aerdepregrid.ncl" used in early versions of CLM (see below the original statement)
;
;;#####################################################
;; Regrid CLM Aerosol-Deposition file (aerdep) to a new resolution. 
;; Works for climatology monthly aerdep files.
;; Uses input environment variables to determine resolution and other 
;; parameters to describe set of files to use.
;; Uses bld query to get input filenames. Also uses env variable CSMDATA for location
;; of input files.
;;
;;  Erik Kluzek
;;  Apr/10/2008
;;  $Id: aerdepregrid.ncl 28400 2011-05-16 05:46:46Z erik $
;;  $HeadURL;
;;######################################################

begin
  ; ===========================================================================================================
  ;
  ; IMPORTANT NOTE: EDIT THE FOLLOWING TO CUSTOMIZE
  ; Edit the following as needed to interpolate to a new resolution.
  ; Input and output resolution

;################
;Setup begin
;################

   plot_name=(/"ALP1","ALP2","ALP3","ALP4","SUB1","SUB2","SUB3","SUB4","BOR1","BOR2","BOR3","BOR4","LYG","BUO","HAV","SKO","VIKE","JOAS","LIAH","FINN"/)   ; site name

;SeedClim Sites: "ALP1","ALP2","ALP3","ALP4","SUB1","SUB2","SUB3","SUB4","BOR1","BOR2","BOR3","BOR4"
;Landpress Sites: "LYG","BUO","HAV","SKO"
;Three-D Sites: "VIKE","JOAS","LIAH"
;Finnmark Site: "FINN"

  do p=0,11,1

    filename    = "/cluster/shared/noresm/inputdata/atm/datm7/NASA_LIS/clmforc.Li_2016_climo1995-2013.360x720.lnfm_Total_c160825.nc" ; global lightning data file
    filenameout = "/cluster/shared/noresm/inputdata_fates_platform/atm/datm7/NASA_LIS/clmforc.Li_2016_climo1995-2013.360x720.lnfm_Total_c160825_"+plot_name(p)+".nc" ;output lightning data file for the site 
    gridfile    = "/cluster/shared/noresm/inputdata_fates_platform/share/domains/"+plot_name(p)+"/domain.lnd."+plot_name(p)+".200422.nc"  ;domain file for the site       
;################
;Setup end
;################


  ; Open files
  print( "Interpolate from file: "+filename );
  if ( systemfunc("test -f "+filename+"; echo $?" ) .ne. 0 )then
     print( "Input faerdep file does not exist or not found: "+filename );
     exit
  end if
  nc       = addfile( filename,    "r" );

  print( "Use grid file:         "+gridfile );
  if ( systemfunc("test -f "+gridfile+"; echo $?" ) .ne. 0 )then
     print( "Input gridfile does not exist or not found: "+gridfile );
     exit
  end if
  ncg      = addfile( gridfile,    "r" );
  print( "Output file:           "+filenameout );
  if ( systemfunc("test -f "+filenameout+"; echo $?" ) .eq. 0 )then
     system( "/bin/rm -f "+filenameout );
  end if
  nco      = addfile( filenameout, "c" );
  ;
  ; Define dimensions
  ;
  dimnames  = (/ "time", "lat", "lon", "scalar"/);
  latgg     = tofloat(ncg->yc(:,0));
  longg     = tofloat(ncg->xc(0,:));
  nlat      = dimsizes( latgg );
  nlon      = dimsizes( longg );
  ntime     = getfilevardimsizes(nc,"time");
  dsizes    = (/ ntime, nlat,  nlon, 1 /);
  is_unlim  = (/ True,  False, False, False/);
  filedimdef( nco, dimnames, dsizes, is_unlim );
  print( "Number of times: "+ntime );
  ;
  ; Define vars and add attributes from original file
  ;
  print( "Define vars" );
  vars = getfilevarnames( nc );
  print(vars)
  i = dimsizes(vars) - 1
  do while ( i .ge. 0 )
     print( "Define "+vars(i) );
     filevardef (    nco, vars(i), typeof(nc->$vars(i)$), getfilevardims( nc, vars(i) ) );
     filevarattdef ( nco, vars(i), nc->$vars(i)$ );
     i = i - 1
  end do
  fileattdef ( nco, nc );
  ;
  ; Add global attributes to output file
  ;
  print( "Global attributes" );

  nco@source               = "Regrid from:"+filename;
  nco@gridfile             = gridfile;
  nco@aerdepregridVersion  = "$HeadURL: https://svn-ccsm-models.cgd.ucar.edu/clm2/branch_tags/cesm1_0_rel_tags/cesm1_0_3_n05_clm4_0_32/models/lnd/clm/tools/ncl_scripts/aerdepregrid.ncl $";
  nco@aerdepregridRevision = "$Id: aerdepregrid.ncl 28400 2011-05-16 05:46:46Z erik $";
  ;
  ; Copy coordinate variables over
  ;
  nco->time= (/nc->time/);
  nco->lon   = (/longg/);
  nco->lat   = (/latgg/);
  nco->EDGEW   = tofloat( (/ncg->xv(0,0,0)/) );
  print(ncg->xv(0,0,0))
  nco->EDGES   = tofloat( (/ncg->yv(0,0,0)/) );
  nco->EDGEN   = tofloat( (/ncg->yv(0,0,1)/) );
  nco->EDGEE   = tofloat( (/ncg->xv(0,0,2)/) );
  print(ncg->xv(0,0,2))
  nco->LATIXY = tofloat( (/ncg->yc/) );
  nco->LONGXY = tofloat( (/ncg->xc/) );
  lon  = (/ nc->lon /);
  lat  = (/ nc->lat /);
  lono = (/ nco->lon /);
  lato = (/ nco->lat /);

  ; loop over variables
  ;
  if ( nlon .eq. 1 )then
     Cyclic = False
  else
     dx = lono(1) - lono(0);
     ; The last longitude should equal 360-(dx-first_longitude)
     ; So for example if first_longitude=0,  last will be 360-dx
     ;                If first_longitude=dx, last will be 360
     expLast = 360.0 - (dx - lono(0));
     if ( abs(lono(nlon-1) - expLast) .lt. 1.e-4 )then
        Cyclic = True;
     else
        Cyclic = False
     end if
  end if

  print( "Cyclic grid: "+Cyclic );

  do i = 0, dimsizes( vars )-1

     vardimnames = getfilevardims( nc, vars(i) );
     ;
     ; If variable is not one of the dimensions -- regrid it and write to output file
     ;
     if ( vars(i) .eq. "lnfm" )then
       print( "Write variable: "+vars(i)+" to output file" );
       ;
       ; If time dimension
       ;
       if ( vardimnames(0) .eq. "time" )then
          do t = 0, ntime-1
             vart  = (/nc->$vars(i)$(t,:,:)/);
             varto = linint2 ( lon,  lat,  vart, Cyclic, lono, lato, 0 );
             if ( all(ismissing(varto(nlat-1,:))) )then
                print( "Copy last latitude over" );
                varto(nlat-1,:) = varto(nlat-2,:);
             end if
             if ( any(ismissing(varto)) )then
                print( "There are some missing values on output no="+num(ismissing(varto)) );
                exit
             end if
             nco->$vars(i)$(time|t,lat|:,lon|:) = (/varto/);
          end do
       ;
       ; without time dimension
       ;
       else
          var  = (/nc->$vars(i)$/);
          varo = linint2 ( lon, lat,  var, Cyclic, lono, lato, 0 );
          if ( all(ismissing(varo(nlat-1,:))) )then
             print( "Copy last latitude over" );
             varo(nlat-1,:) = varo(nlat-2,:);
          end if
          if ( any(ismissing(varo)) )then
             print( "There are some missing values on output no="+num(ismissing(varo)) );
             exit
          end if
          nco->$vars(i)$ = (/varo/);
       end if
     end if
     delete( vardimnames );

  end do
  if ( isvar("varto") )then
     delete(varto);
  end if
  if ( isvar("vart") )then
     delete(vart);
  end if
  if ( isvar("varo") )then
     delete(varo);
  end if
  if ( isvar("var") )then
     delete(var)
  end if

  print( "================================================================================================" );
  print( "Successfully created output aerdep file: "+filenameout );
  end do
end
