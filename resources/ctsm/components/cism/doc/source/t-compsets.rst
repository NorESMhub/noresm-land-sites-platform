.. _t-compsets:

**************************************************************
Running the standalone ice sheet model within CESM: T compsets
**************************************************************

============
 Background
============

The ice sheet model requires much less computational time per simulated
year than other CESM components, but often needs to be run for many more
model years (e.g., tens of thousands of years rather than a century).
Thus, it can be desirable to run CISM in standalone mode, forced by
output from a previous CESM run. In this way, you can cycle many times
through the forcing data from a previous run (e.g., to spin up the ice
sheet model), or run parameter sensitivity analyses much faster than you
could within the coupled system.

A run with standalone CISM in the CESM context is known as a T compset. (In the past this
was referred to as a TG compset. Now the G appears near the end of the compset name, so
compsets have names like T1850G.)  This compset uses the active ice sheet model forced by
a data land model; all other components are stubs. Before running a T compset, you must
have coupler history files from a previous run that included CLM (version 4.5 or
later). You can run with either existing forcing data (see :numref:`t-with-existing-data`)
or with your own forcing data (see :numref:`t-with-your-own-data`).

.. _t-with-existing-data:

====================================
 Running with existing forcing data
====================================

There is currently just a single set of forcing data available for running T
compsets. These forcing data were created with software testing rather than scientific
validity in mind. They were created from 30 years of an ``I1850Clm50Sp`` compset (CLM
forced by a data atmosphere with GSWP3 forcing, starting from an already-near-spun-up
state, with nominally year-1850 forcings and satellite phenology). The resolution was
``f09_g17``. The code base was close to the final CESM2.0 release. For more details, see
`<https://svn-ccsm-inputdata.cgd.ucar.edu/trunk/inputdata/lnd/dlnd7/CPLHIST_SNO/i.e20.I1850Clm50Sp.f09_g17.001_c180502/README>`__.

There are two out-of-the-box T compsets that use these forcing data: T1850G, which uses
CISM2, and T1850G1, which uses CISM1. **You should run these compsets at f09_g17
resolution --- i.e., with the same land resolution and ocean mask used to create the
forcing data.** You can use any CISM resolution, although the current forcing data only
have complete forcings for Greenland, not Antarctica.

So a typical ``create_newcase`` command when running with CISM2, with the standard 4-km
Greenland grid, would look like:

.. code-block:: console

   ./create_newcase --case my_t_case --compset T1850G --res f09_g17_gl4

.. _t-with-your-own-data:

=================================================
 Creating and running with your own forcing data
=================================================

Currently, T compsets are only able to handle forcing data from a previous CESM run
(although, in theory, it should be possible to "fake" CESM output by creating files with
the same format as existing T compset forcing files). Thus, performing a T compset run
with your own forcing data is a two-step process: (1) Perform a CESM run that includes an
active land model (CLM), saving the necessary forcing files, and (2) perform a T compset
run using these new forcing data.

Performing a run to create forcing data
=======================================

To create the necessary forcing data (surface mass balance and surface temperature in each
glacier elevation class, along with surface elevation for the coupler's vertical
downscaling), you need to perform a CESM run using a compset that includes an active land
model (CLM). It does not matter whether the glc component is fully-active
(``CISM2%EVOLVE``), active but non-evolving (``CISM2%NOEVOLVE``) or a stub model
(``SGLC``). If running with CISM, CISM's domain and resolution also do not matter (because
forcing data are saved prior to downscaling to the CISM grid).

However, CLM's ``GLACIER_REGION`` field (on the surface dataset) *does* matter: Surface
mass balance and temperature forcing fields will only be valid in regions whose glacier
region behaviors are:

- ``glacier_region_behavior = virtual``: This is needed in order to provide downscaled
  forcings for all CISM grid cells.

- ``glacier_region_melt_behavior = replaced_by_ice``: This is needed in order to compute
  SMB throughout the CISM domain.

Thus, you must ensure that there are one or more CLM glacier regions encompassing the full
CISM grid(s) that you intend to use in the T compset run, whose glacier region behaviors
are those given above.

In order to save the necessary forcing data, you need to set the driver namelist variable,
``histaux_l2x1yrg``. (This variable name should be read as: coupler auxiliary history file
("histaux") for fields sent from land to coupler ("l2x") with a 1-year frequency ("1yr"),
just focused on glacier-related fields ("g").) This can be set by adding the following
line to ``user_nl_cpl`` in your case directory:

.. code-block:: console

   histaux_l2x1yrg = .true.

This will cause the coupler to write out annual averages of the forcing fields sent from
CLM to CISM (on the CLM grid, separated by elevation class). The files containing these
averages will appear in the cpl/hist directory within your archive space, with names like:
``$CASE.cpl.hl2x1yr_glc.0001-01-01.nc``. (For a typical 1-degree land resolution, these
files are about 7 MB per year.)

A T compset run that later uses these coupler history files as forcing should give
*nearly* identical CISM results as the original run, **as long as you ensure that SMB
renormalization is done (or not done) in both cases, as described in**
:numref:`clm-cism-coupling`. **(For example, if the coupler history files were generated
from a fully-coupled case with an evolving, two-way-coupled ice sheet, then in the T case
you should set** ``glc_renormalize_smb = 'on'`` **in** ``user_nl_cpl`` **in order to
reproduce the results of the fully-coupled case.)** Small differences arise because these
forcing files are written with single precision, leading to roundoff error on the order of
10\ :sup:`-7`. If you need more precision, you can add the following to ``user_nl_cpl``:

.. code-block:: console

   histaux_double_precision = .true.

This will give you double precision output, at the expense of roughly doubling the output
volume. Short tests have shown that this change allows the T compset run to match the
original within double-precision roundoff-level.

Performing a T compset run using your own forcing data
======================================================

To perform a standalone CISM run forced by your newly-created forcing data, first create a
new case using one of the existing T compsets (e.g., T1850G). The land and ocean
resolutions of the T compset run (as specified by the ``--res`` flag to
``create_newcase``) should match the resolution of the run used to create the forcing
data. You *can* run with a different glc resolution than the one used to create the
forcing data. So, for example, if you created the forcing data from an I or B compset with
resolution ``f09_g17_gl4``, the T compset run should use resolution ``f09_g17_xxx``, where
any value of ``xxx`` is acceptable.

The following variables in ``env_run.xml`` should be modified appropriately for your
forcing data:

- ``DLND_CPLHIST_DIR``: Directory in which your ``cpl.hl2x1yr_glc`` files can be found

- ``DLND_CPLHIST_CASE``: Name of the case used to create the ``cpl.hl2x1yr_glc`` files
  (files are assumed to be named ``$DLND_CPLHIST_CASE.cpl.hl2x1yr_glc.yyyy-01-01.nc``)

- ``DLND_CPLHIST_YR_START``: First year of forcing data

  .. note::

     ``DLND_CPLHIST_YR_START`` can be set later than the first existing year of data if
     you want to use a subset of the available years. However, the data land model expects
     to find domain information on the first year's file; typically, this domain
     information (variable names beginning with ``doml``) only appears on the first
     coupler history file. So, to start with something later than the first year, you will
     need to copy all of these ``doml_xxx`` fields from the first file onto the file
     corresponding to ``DLND_CPLHIST_YR_START``. **Make sure you copy ALL of the**
     ``doml_xxx`` **fields (otherwise you may get garbage results).**

- ``DLND_CPLHIST_YR_END``: Last year of forcing data (can be set earlier
  than the last existing year of data if you want to use a subset of the
  available years)

- ``RUN_STARTDATE``: Determines the model year in which the run starts. This can be set to
  anything you want, but a good convention is:

  - For transient T compset runs forced by output from a transient CESM run, set to the
    first year of forcing data (this corresponds to the real-world year, in some sense)

  - For non-transient T compset runs (forced either by output from a non-transient run, or
    by cycling through the available forcing data multiple times), set to 0001-01-01 (in
    this case, there is no real-world meaning to the start year)

- ``DLND_CPLHIST_YR_ALIGN``: The simulation year corresponding to
  ``DLND_CPLHIST_YR_START``. This will usually be the same as the year in
  ``RUN_STARTDATE``, but it can be set to a different year to start the simulation with a
  different year of forcing data.

To confirm you have set up the paths and file names correctly, run:

.. code-block:: console

   ./preview_namelists

and examine the generated file, ``CaseDocs/dlnd.streams.txt.sno.cplhist``.

==============================================
 Changes to some CESM defaults for T compsets
==============================================

T compsets have much lower computational expense per simulation year and much greater
typical run lengths compared to most CESM configurations. Thus, a number of settings are
changed automatically when running with a T compset. These include:

- Default run length: 5 years (rather than 5 days)

- Default coupling frequency: annual (rather than daily or more frequent)

