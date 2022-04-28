.. sectnum::
   :prefix: A.
   :start: 1

.. _new-grids:

********************************
Introducing a new ice sheet grid
********************************

This section describes what is needed when introducing a new ice sheet grid into the
system. Much of the information in :ref:`Generating mapping files` is also relevant when
introducing a new land grid (in which case new lnd-glc mapping files are needed) or a new
ocean grid (in which case new ocn-glc mapping files are needed).

Note that local ice sheet grids must be rectangular; typically they are polar
stereographic projections.

=============
Prerequisites
=============

.. note::

   These instructions were written for the old yellowstone machine. The process on
   cheyenne will be very similar, but batch commands and some other details will differ.

The following instructions assume:

#. You have a SCRIP grid file for your new grid

#. You have the NetCDF operators (nco) in your path. On yellowstone, you can
   accomplish this with ``module load nco``

#. You are using the bash shell (otherwise some instructions will need to be
   modified slightly)

#. You are working on yellowstone

=============================
Modifications needed for CISM
=============================

Add new entries in CISM's xml file(s)
-------------------------------------

NEED TO FILL THIS IN WITH INSTRUCTIONS

For Greenland grids: Submerging land outside of Greenland
---------------------------------------------------------

For Greenland grids, we typically need to submerge land outside of the island of
Greenland. If we don't do this (allowing for Ellesmere Island, Iceland, etc. to
appear as land), we'll run into at least two problems:

A. CISM will try to dictate the land cover in those regions. Even if CLM's
   surface dataset says there is glacier there, they will be overwritten with
   bare land if CISM doesn't have any ice there.

B. We can potentially get an ice sheet growing there if CLM dictates glacial
   inception.

New method, using the 'mask' field
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Joe Kennedy's new files have a 'mask' field on them that can be used for this
purpose. Joe's documentation of this field is:

    * 4 -- floating ice
    * 3 -- grounded ice
    * 2 -- bare land
    * 1 -- ocean
    * 0 -- missing topg data
    * -1 -- shallow paleo ocean
    * -2 -- bare paleo land
    * -3 -- shallow ocean or land outside the paleo domain

    So, for the standard CISM input dataset, every point where mask < 0 should be
    sunk to -200 m a.s.l. For the paleo grid however, mask < -2 should be sunk and
    mask == -2 would become bare land (2) and mask == -1 would become ocean.

However, I also want to submerge the points with mask == 0, because I don't
trust the handling of missing values.

I used the following procedure:

#. Submerge points with something like this (note: it's important to have 'mask'
   in quotes; otherwise ncap2 thinks mask has a special meaning):

   .. code-block:: console

      ncap2 -s "where(('mask' == 0) || ('mask' < 0 && topg > -200)) topg = -200.;" greenland_4km_2017_02_23.epsg3413.nc greenland_4km_epsg3413_c170429.nc

#. Confirm that there are now no missing values for topg, e.g., by loading it
   into python

#. Remove the missing_value attribute from topg and add some metadata with
   something like this:

   .. code-block:: console

      ncatted -a missing_value,topg,d,, greenland_4km_epsg3413_c170429.nc
      ncatted -h -a Note_170429,topg,c,c,"submerged all non-Greenland land to -200m with: ncap2 -s \"where(('mask' == 0) || ('mask' < 0 && topg > -200)) topg = -200.;\"; then removed now-unnecessary missing_value attribute" greenland_4km_epsg3413_c170429.nc
      ncatted -h -a Note_170429,global,c,c,"Same as greenland_4km_2017_02_23.epsg3413.nc (provided by Joe Kennedy), except submerged all non-Greenland land to -200m with: ncap2 -s \"where(('mask' == 0) || ('mask' < 0 && topg > -200)) topg = -200.;\"; then removed now-unnecessary missing_value attribute of topg" greenland_4km_epsg3413_c170429.nc


Old method, using the 'landcover' field
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Note: The method documented below is what I used before we had a 'mask' field
on the input files.**

Determining which grid cells are part of Greenland and which are outside
Greenland is tricky. The easiest thing to do is to take an existing mask, which
we have generated for some other grid, and regrid that to your new
grid. Ideally, the existing mask will be at a resolution as close as possible to
that of your new grid.

In order to regrid this mask from one CISM grid to another, follow this
process. This assumes that you have some "old" CISM input file with the
"landcover" field on it, and that you have a SCRIP grid file corresponding to
that old input file as well as for your new CISM grid.

#. Make a mapping file from the old grid to the new one: Modify the following
   script (see the FIXME note for what to change):

   .. code-block:: bash

      #!/bin/bash
      #
      #
      # Batch script to submit to create ESMF mapping file
      #
      # Set up for yellowstone
      #
      # yellowstone-specific batch commands:
      #BSUB -P P93300601        # project number
      #BSUB -n 8                # number of processors
      #BSUB -R "span[ptile=16]"
      #BSUB -W 1:00             # wall-clock limit
      #BSUB -q caldera          # queue
      #BSUB -o regrid.%J.out    # ouput filename
      #BSUB -e regrid.%J.err    # error filename
      #BSUB -J create_ESMF_map  # job name
      #BSUB -N                  # send email upon job completion

      #----------------------------------------------------------------------

      #----------------------------------------------------------------------
      # Set user-defined parameters here
      #----------------------------------------------------------------------

      # FIXME: Replace the following lines with paths to SCRIP grid files and names of your grids
      filesrc="/glade/p/cesmdata/cseg/inputdata/glc/cism/griddata/SCRIPgrid_gland_4km_c161223.nc"
      filedst="/glade/p/cesmdata/cseg/inputdata/glc/cism/griddata/SCRIPgrid_greenland_4km_epsg3413_c161223.nc"
      namesrc='gland4kmOld'
      namedst='gland4kmNew'

      typesrc='regional'
      typedst='regional'
      maptype='aave'

      #----------------------------------------------------------------------
      # Done setting user-defined parameters
      #----------------------------------------------------------------------

      #----------------------------------------------------------------------
      # Stuff done in a machine-specific way
      #----------------------------------------------------------------------

      # Determine number of processors we're running on
      host_array=($LSB_HOSTS)
      REGRID_PROC=${#host_array[@]}

      #----------------------------------------------------------------------
      # Begin general script
      #----------------------------------------------------------------------

      cmdargs="--filesrc $filesrc --filedst $filedst --namesrc $namesrc --namedst $namedst --typesrc $typesrc --typedst $typedst --maptype $maptype --batch"
      env REGRID_PROC=$REGRID_PROC ./create_ESMF_map.sh $cmdargs

   Put this script in
   ``cime/tools/mapping/gen_mapping_files/gen_ESMF_mapping_file/``, named
   ``regrid_cism_old_to_new.sh``, then submit it with:

   .. code:: bash

      bsub < regrid_cism_old_to_new.sh


#. Extract the landcover field from your old CISM input file

   The landcover field is stored with a degenerate time dimension, but we need
   to remove that degenerate dimension. Run something like this, replacing the
   file path with the actual path to the CISM input file you'll be using

   .. code-block:: console

      cd cime/tools/mapping/map_field
      module load nco
      ncks -v landcover /glade/p/cesmdata/cseg/inputdata/glc/cism/Greenland/glissade/init/greenland_4km_2015_06_03.mcb_trunk_c161025.nc landcover_old_with_time.nc
      ncwa -a time landcover_old_with_time.nc landcover_old.nc

#. Regrid the landcover field from your old CISM input file

   First, build the map_field tool (in ``cime/tools/mapping/map_field``), by
   following the directions there.

   Then, from ``cime/tools/mapping/map_field``, run something like the
   following, though replacing paths with the correct paths to your files. Note
   that, for this to work, you may need to source the env_mach_specific file
   that you sourced when building the map_field tool.

   .. code-block:: console

      ./map_field -m "/glade/p/work/sacks/cime/tools/mapping/gen_mapping_files/gen_ESMF_mapping_file/map_gland4kmOld_TO_gland4kmNew_aave.161223.nc" -if landcover_old.nc -iv landcover -of landcover_new.nc -ov landcover

#. Round landcover to 0 or 1, and fix dimension names

   .. code-block:: console

      ncap2 -s 'landcover_int = int(round(landcover))' landcover_new.nc landcover_new2.nc
      ncrename -d ni,x1 -d nj,y1 landcover_new2.nc
      ncks -x -v landcover landcover_new2.nc landcover_new3.nc
      ncrename -v landcover_int,landcover landcover_new3.nc

#. Append landcover field onto input file

   Change the 'today' variable and file names to point to your actual file in
   the following:

   .. code-block:: console

      export today=161223
      export path_to_input_file=/glade/p/cesmdata/cseg/inputdata/glc/cism/Greenland/glissade/init
      export landcover_origfile=greenland_4km_2015_06_03.mcb_trunk_c161025.nc
      export origfile=greenland_4km_2016_12_19.epsg3413.nc
      export newfile=greenland_4km_epsg3413_c${today}.nc
      cp $path_to_input_file/$origfile $path_to_input_file/$newfile
      ncks -A -v landcover landcover_new3.nc $path_to_input_file/$newfile
      ncatted -h -a no_data,landcover,c,i,0 -a has_data,landcover,c,i,1 -a Note_${today},landcover,c,c,"Regridded landcover from $landcover_origfile using area-conservative remapping then rounding to 0/1" $path_to_input_file/$newfile

#. Submerge non-Greenland land with:

   .. code-block:: console

      export extra_info_on_origfile=" (provided by Joe Kennedy)"
      ncap2 -s 'where(landcover == 0 && topg > -200) topg = -200.;' $path_to_input_file/$newfile tempfile.nc
      mv tempfile.nc $path_to_input_file/$newfile
      ncatted -h -a Note_${today},topg,c,c,"submerged all non-Greenland land to -200m with: ncap2 -s 'where(landcover == 0 && topg > -200) topg = -200.;'" $path_to_input_file/$newfile
      ncatted -h -a Note_${today},global,c,c,"Same as ${origfile}${extra_info_on_origfile}, except (1) Includes landcover field, regridded from $landcover_origfile using area-conservative remapping then rounding to 0/1; (2) Submerged all non-Greenland land to -200m with: ncap2 -s 'where(landcover == 0 && topg > -200) topg = -200.;'" $path_to_input_file/$newfile

#. Optional: Confirm the regridding of landcover.

   This step may not need to be done, but if you want to make sure landcover got
   regridded to the new grid properly, you can do it as follows. This uses
   python, with the NetCDF4 library. Note that dat_old points to the version of
   the dataset prior to modifying topg.

   .. code-block:: python

      dat_old = Dataset('greenland_4km_2016_12_19.epsg3413.nc')
      dat_new = Dataset('greenland_4km_epsg3413_c161223.nc', 'a')
      landcover = np.squeeze(dat_new.variables['landcover'][:])
      topg_orig = np.squeeze(dat_old.variables['topg'][:])
      category = dat_new.createVariable('category', 'i4', ('y1','x1'))
      category_vals = np.zeros(landcover.shape)
      land = np.logical_and(landcover==1, topg_orig>=0)
      ocean = np.logical_and(landcover==1, topg_orig<0)
      category_vals[ocean] = 1
      category_vals[land] = 2
      category[:] = category_vals
      category.landcover_is_0 = 0
      category.landcover_is_1_topg_lt_0 = 1
      category.landcover_is_1_topg_ge_0 = 2
      dat_new.close()

   Then, make sure:

   i. landcover = 0 points only occur off the coast of Greenland - not within or
      near Greenland

      First viewed this with a color scale that spanned 0 - 2 (so different
      colors for 0, 1 and 2), and viewing where the 0s are relative to the 1s
      and 2s. Ideally, there should be some 1 (ocean) between the 2 (land) and 0
      (landcover = 0).

      Also viewed this by setting 0 to blue, 1-2 to white -- making sure blue is
      only on periphery

   ii. no topg > 0, landcover = 1 points outside of Greenland

       Viewed this by setting 2 to blue, 0-1 to white -- making sure there is no
       blue on the periphery

============================
Modifications needed for CLM
============================

You need to ensure that the ``GLACIER_REGION`` field on CLM's surface dataset is set up
consistently with the new CISM grid. You should have a glacier region (or multiple glacier
regions) encompassing the full CISM grid, whose glacier region behaviors are:

- ``glacier_region_behavior = virtual``: This is needed in order to provide downscaled
  forcings for all CISM grid cells.

- ``glacier_region_melt_behavior = replaced_by_ice``: This is needed in order to compute
  SMB throughout the CISM domain.

The value of ``glacier_region_ice_runoff_behavior`` can be whatever makes the most sense
scientifically.

.. _Generating mapping files:

======================================================
Generating the necessary inter-component mapping files
======================================================

Generating lnd <-> glc mapping files for a new CISM grid
--------------------------------------------------------

#. Build the check_maps tool

   This isn't entirely necessary, but allows the maps you generate to be checked
   by this tool. To build this, follow the instructions in
   ``cime/tools/mapping/check_maps/README``.

#. Modify the following script that will create the necessary mapping
   files. Make sure to fill in the correct values for -fglc and -nglc where it
   says 'FIXME':

   .. code-block:: bash

     #!/bin/bash
     #
     #
     # Batch script to submit to create suite of ESMF mapping files
     #
     # Set up for yellowstone
     #
     # yellowstone-specific batch commands:
     #BSUB -P P93300601        # project number
     #BSUB -n 8               # number of processors
     #BSUB -R "span[ptile=16]"
     #BSUB -W 24:00            # wall-clock limit
     #BSUB -q caldera          # queue
     #BSUB -o regrid.%J.out    # ouput filename
     #BSUB -e regrid.%J.err    # error filename
     #BSUB -J gen_cesm_maps    # job name
     #BSUB -N                  # send email upon job completion

     #----------------------------------------------------------------------

     #----------------------------------------------------------------------
     # Set user-defined parameters here
     #----------------------------------------------------------------------

     # CISM grid
     # FIXME: Fill this in with the path to your SCRIP grid file and the name of your grid
     glc_grid=" -fglc /PATH/TO/SCRIPgrid.nc -nglc gland4km "

     # CLM grids
     clm_f09=" -flnd $CESMDATAROOT/inputdata/lnd/clm2/mappingdata/grids/0.9x1.25_c110307.nc -nlnd fv0.9x1.25 "
     clm_f19=" -flnd $CESMDATAROOT/inputdata/lnd/clm2/mappingdata/grids/1.9x2.5_c110308.nc -nlnd fv1.9x2.5 "
     clm_T31=" -flnd $CESMDATAROOT/mapping/grids/T31_040122.nc -nlnd T31 "
     clm_hcru=" -flnd $CESMDATAROOT/inputdata/lnd/clm2/mappingdata/grids/SCRIPgrid_360x720_nomask_c120830.nc -nlnd 360x720 "
     clm_4x5=" -flnd $CESMDATAROOT/inputdata/lnd/clm2/mappingdata/grids/SCRIPgrid_4x5_nomask_c110308.nc -nlnd fv4x5 "
     clm_10x15=" -flnd $CESMDATAROOT/inputdata/lnd/clm2/mappingdata/grids/SCRIPgrid_10x15_nomask_c110308.nc -nlnd fv10x15 "

     # This grid is identical to $CESMDATAROOT/inputdata/lnd/clm2/mappingdata/grids/SCRIPgrid_ne120np4_nomask_c101123.nc
     clm_ne120=" -flnd /glade/p/cesmdata/cseg/mapping/grids/ne120np4_pentagons_100310.nc -nlnd ne120np4 "

     # This grid is identical to $CESMDATAROOT/inputdata/lnd/clm2/mappingdata/grids/SCRIPgrid_ne30np4_nomask_c101123.nc
     clm_ne30=" -flnd /glade/p/cesmdata/cseg/mapping/grids/ne30np4_091226_pentagons.nc -nlnd ne30np4 "

     # This grid is identical to $CESMDATAROOT/inputdata/lnd/clm2/mappingdata/grids/SCRIPgrid_ne16np4_nomask_c110512.nc
     clm_ne16=" -flnd /glade/p/cesmdata/cseg/mapping/grids/ne16np4_110512_pentagons.nc -nlnd ne16np4 "

     ### Not bothering with this one: seems to not be used any more
     ### clm_f02=" -flnd /glade/p/cesmdata/cseg/mapping/grids/fv0.23x0.31_071004.nc -nlnd fv0.23x0.31 "

     #----------------------------------------------------------------------
     # Done setting user-defined parameters
     #----------------------------------------------------------------------

     #----------------------------------------------------------------------
     # Stuff done in a machine-specific way
     #----------------------------------------------------------------------

     # Determine number of processors we're running on
     host_array=($LSB_HOSTS)
     REGRID_PROC=${#host_array[@]}

     #----------------------------------------------------------------------
     # Begin general script
     #----------------------------------------------------------------------

     for lnd_grid in "$clm_f09" "$clm_f19" "$clm_T31" "$clm_hcru" "$clm_4x5" "$clm_10x15" "$clm_ne120" "$clm_ne30" "$clm_ne16"; do
	 cmdargs="$glc_grid $lnd_grid --batch"
	 echo "=============================================================================="
	 echo "About to execute gen_cesm_maps with: $cmdargs"
	 env REGRID_PROC=$REGRID_PROC ./gen_cesm_maps.sh $cmdargs
     done

#. Name the script cism.regridbatch.sh, and put it in
   cime/tools/mapping/gen_mapping_files

#. Run:

   .. code-block:: console

      bsub < cism.regridbatch.sh

   You can ignore errors in the .err file that look like this:

   .. code-block:: console

      ATTENTION: 0031-408  8 tasks allocated by Resource Manager, continuing...
      ATTENTION: 0031-408  8 tasks allocated by Resource Manager, continuing...
      Abort(0) on node 0 (rank 0 in comm -2080374782): application called MPI_Abort(comm=0x84000002, 0) - process 0
      ERROR: 0031-300  Forcing all remote tasks to exit due to exit code 1 in task 0
      forrtl: error (78): process killed (SIGTERM)
      Image              PC                Routine            Line        Source
      libpthread.so.0    0000003F7240F4B5  Unknown               Unknown  Unknown
      libpoe.so          00002B1CF8267AE2  Unknown               Unknown  Unknown
      libpthread.so.0    0000003F724079D1  Unknown               Unknown  Unknown
      libc.so.6          0000003F718E88FD  Unknown               Unknown  Unknown

#. Look through output in the .out file telling you about the results of running
   check_maps on all of your new mapping files.

   Ideally, you'll see a lot of output that looks like this:

   .. code-block:: console

      1: map_gland4km_TO_fv0.9x1.25_aave.161222.nc
       All           21  tests passed!
      -----
      2: map_fv0.9x1.25_TO_gland4km_aave.161222.nc
       All           21  tests passed!
      -----
      3: map_fv0.9x1.25_TO_gland4km_blin.161222.nc
       All           14  tests passed!
      -----

   However, you should expect to see errors when checking the very
   coarse-resolution fv10x15 grid, like this:

   .. code-block:: console

      1: map_gland4km_TO_fv10x15_aave.161222.nc
       ERROR: the test did not successfully map any values
       from the source grid to the destination grid
                 0  of            0  tests failed. See above for details.
      -----
      2: map_fv10x15_TO_gland4km_aave.161222.nc
       FAILED: L1 error =   9.028874246726999E-003  in test            1
       FAILED: L1 error =   2.228991274441720E-002  in test            3
                 2  of           21  tests failed. See above for details.
      -----

   In addition, you *may* see additional errors like that for other CLM grids,
   particularly if you have a higher-resolution CISM grid: The tolerances in
   check_maps are set such that errors can be expected when checking mappings
   between regional grids and relatively coarse-resolution global grids.

#. Put mapping files in correct directories in the inputdata space

   The mapping files should go in ``$CESMDATAROOT/inputdata/cpl/gridmaps/RES``
   where ``RES`` is the *from* resolution. e.g.,
   ``map_fv0.9x1.25_TO_gland4km_aave.161223.nc`` goes in
   ``$CESMDATAROOT/inputdata/cpl/gridmaps/fv0.9x1.25``, whereas
   ``map_gland4km_TO_fv0.9x1.25_aave.161223.nc`` goes in
   ``$CESMDATAROOT/inputdata/cpl/gridmaps/gland4km``. You can accomplish this
   with the following code in bash:

   .. code-block:: bash

      for fl in map_*.nc; do
          IFS='_' read -ra fname_split <<< "$fl"
          from_res=${fname_split[1]}
          mv -v $fl $CESMDATAROOT/inputdata/cpl/gridmaps/${from_res}/
      done

Generating glc -> ocn mapping files
-----------------------------------

See also https://github.com/NCAR/cism_misc-runoff_mapping_inputs

#. Build the runoff_map tool in
   ``cime/tools/mapping/gen_mapping_files/runoff_to_ocn`` by following the
   directions there

#. Create a namelist file like the following, but changing the details to match
   your new grid:

   .. code-block:: console

      &input_nml
         gridtype     = 'scrip'
         file_roff    = '/glade/p/cesmdata/cseg/inputdata/glc/cism/griddata/SCRIPgrid_greenland_4km_epsg3413_c161223.nc'
         file_ocn     = '/glade/p/cesm/cseg/mapping/grids/gx3v7_120309.nc'
         file_ocn_coastal_mask = '/glade/p/cesm/cseg/mapping/grids/gx3v7_coast_180430.nc'
         file_nn      = 'map_gland4km_epsg3413_to_gx3v7_nn.nc '
         file_smooth  = 'map_gx3v7_coast_to_gx3v7_sm.nc '
         file_new     = 'map_gland4km_to_gx3v7_nnsm_e1000r500_171024.nc'
         title        = 'runoff map: gland4km -> gx3v7, nearest neighbor and smoothed '
         eFold        = 1000000.0
         rMax         =  500000.0
         restrict_smooth_src_to_nn_dest = .true.
         step1 = .true.
         step2 = .true.
         step3 = .true.
        /

   Name this file ``runoff_map.nml``

#. Run

   .. code-block:: console

      ./runoff_map < runoff_map.nml

   Note that it may be necessary to have the same environment that you used for
   building (e.g., via sourcing src/.env_mach_specific.sh before running this
   executable).

#. Run

   .. code-block:: console

      ./run_merge_mapping_files.sh \
      --map_in_oo map_gland4km_epsg3413_to_gx3v7_nn.nc \
      --map_in_ms map_gland4km_to_gx3v7_nnsm_e1000r500_171024.nc \
      --region_mask /glade/p/cesmdata/cseg/inputdata/ocn/pop/gx3v7/grid/region_mask_20090831.ieeei4 \
      --map_out map_gland4km_to_gx3v7_nn_open_ocean_nnsm_e1000r500_marginal_sea_171024.nc

#. Repeat the above process with the gx1v6 grid, changing the input
   namelist appropriately.

   * For ``rMax``, use ``300000.0``

   * As of 2017-10-24, for ``file_ocn_coastal_mask``, use ``gx1v6_coast_170503.nc``

   * As of 2017-10-24, for ``--region_mask``, use ``/glade/p/cesmdata/cseg/inputdata/ocn/pop/gx1v6/grid/region_mask_20090205.ieeei4``

#. Repeat the above process with the gx1v7 grid, changing the input
   namelist appropriately.

   * For ``rMax``, use ``300000.0``

   * As of 2017-10-24, for ``file_ocn_coastal_mask``, use ``gx1v7_coast_170322.nc``

   * As of 2017-10-24, for ``--region_mask``, use ``/glade/p/cesmdata/cseg/inputdata/ocn/pop/gx1v7/grid/region_mask_20151008.ieeei4``

#. Run the check_maps tool on each of the resulting final mapping files
   (the files with the date stamp at the end)

   * To build and run this, follow the instructions in
     ``cime/tools/mapping/check_maps/README``

   * Examine the output from this tool to make sure there are no major
     mapping errors

     * For these runoff mapping files, messages about "L1 error" and "L2
       error" can be ignored.

     * Until https://github.com/ESMCI/cime/issues/2014 is resolved, you
       need to add dimensions to the merged files with something like
       ``ncap2 -s
       'defdim("ni_a",416);defdim("nj_a",704);defdim("ni_b",320);defdim("nj_b",384)'
       YOUR_MAP_NAME.nc`` (where you can find the correct dimensions on
       the non-merged (i.e., nnsm) mapping files).

   * If you'd like, you can also visually examine the mapped
     fields. Open the file named ``test_YOUR_MAP_NAME.nc``; a useful
     field to view is ``dst02``, which is the result of mapping a
     uniform field (with value 2) from the glc grid to the ocn grid.

#. Put mapping files in correct directories in the inputdata space

   The mapping files should go in ``$CESMDATAROOT/inputdata/cpl/gridmaps/RES``
   where ``RES`` is your new CISM resolution.

   There are two final mapping files that need to be kept for each
   glc-ocn grid combination: The two files with the date stamp at the
   end.

Adding new grid and mapping files in config_grids.xml
-----------------------------------------------------

In order for your new grid and mapping files to be recognized by the CESM
scripts, you need to add entries in config_grids.xml
(``cime/cime_config/cesm/config_grids.xml``).

#. Add new grid definition

   You'll need to add a section like this:

   .. code-block:: xml

      <domain name="gland4">
        <nx>376</nx> <ny>701</ny>
        <desc>4-km Greenland grid, for use with the glissade dycore</desc>
      </domain>

#. Point to new mapping files: lnd <-> glc

   You'll need to add a section like this for each land grid, in the section
   "lnd to glc and glc to lnd mapping":

   .. code-block:: xml

      <gridmap lnd_grid="0.9x1.25" glc_grid="gland4" >
        <map name="LND2GLC_FMAPNAME">cpl/gridmaps/fv0.9x1.25/map_fv0.9x1.25_TO_gland4km_aave.161223.nc</map>
        <map name="LND2GLC_SMAPNAME">cpl/gridmaps/fv0.9x1.25/map_fv0.9x1.25_TO_gland4km_blin.161223.nc</map>
        <map name="GLC2LND_FMAPNAME">cpl/gridmaps/gland4km/map_gland4km_TO_fv0.9x1.25_aave.161223.nc</map>
        <map name="GLC2LND_SMAPNAME">cpl/gridmaps/gland4km/map_gland4km_TO_fv0.9x1.25_aave.161223.nc</map>
      </gridmap>

#. Point to new mapping files: glc -> ocn

   In the section "GRIDS: glc to ocn mapping", add a section like this:

   .. code-block:: xml

      <gridmap ocn_grid="gx1v6" glc_grid="gland4" >
        <map name="GLC2OCN_LIQ_RMAPNAME">cpl/gridmaps/gland4km/map_gland4km_to_gx1v6_nn_open_ocean_nnsm_e1000r300_marginal_sea_171105.nc</map>
        <map name="GLC2OCN_ICE_RMAPNAME">cpl/gridmaps/gland4km/map_gland4km_to_gx1v6_nnsm_e1000r300_171105.nc</map>
      </gridmap>
      <gridmap ocn_grid="gx1v7" glc_grid="gland4" >
        <map name="GLC2OCN_LIQ_RMAPNAME">cpl/gridmaps/gland4km/map_gland4km_to_gx1v7_nn_open_ocean_nnsm_e1000r300_marginal_sea_171105.nc</map>
        <map name="GLC2OCN_ICE_RMAPNAME">cpl/gridmaps/gland4km/map_gland4km_to_gx1v7_nnsm_e1000r300_171105.nc</map>
      </gridmap>
      <!-- POP's estuary box model is currently not active for gx3v7, so
           we need nnsm maps for liquid as well as ice. -->
      <gridmap ocn_grid="gx3v7" glc_grid="gland4" >
        <map name="GLC2OCN_LIQ_RMAPNAME">cpl/gridmaps/gland4km/map_gland4km_to_gx3v7_nnsm_e1000r500_171105.nc</map>
        <map name="GLC2OCN_ICE_RMAPNAME">cpl/gridmaps/gland4km/map_gland4km_to_gx3v7_nnsm_e1000r500_171105.nc</map>
      </gridmap>
      
   * **Important note for gx3v7 grid:** The estuary box model is not
     active for the gx3v7 grid for now, so point to the nnsm file for
     both ice and liquid runoff. (However, to follow what's done for the
     rof2ocn mapping files, you can still put the merged maps in the
     inputdata repository, so that they can be used if the estuary box
     model is ever activated for gx3v7.)

