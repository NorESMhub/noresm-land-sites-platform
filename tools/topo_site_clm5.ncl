;
; Create site elevation file to be read by DATM 
;
;  Hui Tang
;  08/2020
;  

begin
 
;################
;Setup begin
;################
   plot_name=(/"VIKE","JOAS","LIAH"/)          ; site name
   plot_height=(/469,920,1290/)                ; site elevation (m)

;SeedClim Sites: "ALP1","ALP2","ALP3","ALP4","SUB1","SUB2","SUB3","SUB4","BOR1","BOR2","BOR3","BOR4"
;        Height: 1208,1097,1213,1088,815,700,769,797,589,474,431,346
;Landpress Sites: "LYG","BUO","HAV","SKO"
;        Height: 15, 7, 9, 10
;Three-D Sites: "VIKE","JOAS","LIAH"
;        Height: 469,920,1290
;Finnmark Site: "FINN"
;        Height: 352 
 
  do p=0,2,1
 
    filename    = "/cluster/shared/noresm/inputdata/atm/datm7/topo_forcing/topodata_0.9x1.25_USGS_070110_stream_c151201.nc"         ; global file for a template of the netcdf file
    filenameout = "/cluster/shared/noresm/inputdata_fates_platform/atm/datm7/topo_forcing/topodata_"+plot_name(p)+".nc"             ; output topography file for the site
    gridfile    = "/cluster/shared/noresm/inputdata_fates_platform/share/domains/"+plot_name(p)+"/domain.lnd."+plot_name(p)+".200927.nc"    ; domain file for the site    

;################
;Setup end
;################
    ;
    ; Open files
    ;
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
  dimnames  = (/ "time", "lat", "lon" /);
  latgg     = ncg->yc(:,0);
  longg     = ncg->xc(0,:);
  nlat      = dimsizes( latgg );
  nlon      = dimsizes( longg );
  ntime     = getfilevardimsizes(nc,"time");
  dsizes    = (/ ntime, nlat,  nlon /);
  is_unlim  = (/ True,  False, False /);
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

  ;
  ; Copy coordinate variables over
  ;

  if ( isfilevar( nc, "time" ) ) then
     nco->time   = (/nc->time/);
  end if
  if ( isfilevar( nc, "mask" ) ) then
     nco->mask   = (/ncg->mask/);
  end if
  if ( isfilevar( nc, "area" ) ) then
     nco->area   = (/ncg->area/);
  end if
  if ( isfilevar( nc, "NUMLON" ) ) then
     nco->NUMLON = (/nlon/);
  end if
  nco->LONGXY    = (/ncg->xc(:,:)/);
  nco->LATIXY    = (/ncg->yc(:,:)/);
  nco->TOPO      = (/plot_height(p)/);


  print( "================================================================================================" );
  print( "Successfully created output aerdep file: "+filenameout );
  
  end do
  
end
