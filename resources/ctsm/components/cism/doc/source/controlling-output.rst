.. _controlling-output:

************************************
Controlling output from CISM and CLM
************************************

=========================
 Controlling CISM output
=========================

The default CISM output frequency is yearly. Most of the CISM output fields are
instantaneous, so this default frequency means that you get a snapshot of the current
state at the end of each year. You can change the output to be less frequent via the
namelist variable ``history_frequency``, which can be set in ``user_nl_cism``. For
example, to give output every 10 years, set:

.. code-block:: Fortran

   history_frequency = 10

The set of variables written to each history file is controlled by the namelist variable
``cesm_history_vars``. To see the variables output by default, examine
``CaseDocs/cism_in`` after running ``preview_namelists`` for your case. Among the standard
fields written to the history file are the ice thickness (``thk``), upper surface
elevation (``usurf``), bedrock elevation (``topg``), and the surface mass balance
(``smb``) and surface temperature (``artm``) downscaled to the ice sheet grid.

To modify the list of history fields, set ``cesm_history_vars`` in ``user_nl_cism``. Files
with names ending in ``vars.def`` in the source tree specify the fields that can be
written out. You can add new variables to one of these ``vars.def`` files and rebuild the
model in order to enable new output fields. (Note: These ``vars.def`` files can NOT be
placed in SourceMods: they need to be changed in-place in the source tree.)

Model restart frequency is coordinated by the CESM coupler. The restart file contains all
the fields required for exact restart.  However, the restart will be exact only if the
file is written immediately after an ice dynamics time step. This will normally be the
case for restart files written at the end of any model year.

=============================================
 Producing land ice-specific output from CLM
=============================================

Outputting forcing fields sent from CLM to CISM
===============================================

CLM sends two sets of fields to CISM for each elevation class: ``qice`` and ``tsrf``. In
addition, it sends a third set of fields to the coupler for downscaling purposes:
``topo``. It can often be useful to view the values of these forcing fields for each
elevation class within each grid cell. To do this, you can use the three CLM history
variables, ``QICE_FORC``, ``TSRF_FORC`` and ``TOPO_FORC``. These history variables are
inactive by default, but can be added to any of CLM's history files using the
``hist_fincl`` CLM namelist variables. For example, to add ``QICE_FORC`` and ``TSRF_FORC``
to CLM's default (monthly) history file, you would add the following in ``user_nl_clm``:

.. code-block:: Fortran

   hist_fincl1 = 'QICE_FORC','TSRF_FORC'

As with other CLM history variables, additional history files can be created with
different time frequencies. See the `CLM User's Guide
<https://escomp.github.io/ctsm-docs>`__ for details on how to do this.

Adding a CLM history field that provides averages only over ice
===============================================================

In general, CLM history fields give weighted averages over the entire grid cell. If you
are interested in diagnostics just over ice landunits for certain history fields, you can
make a source code modification for each field of interest.

CLM history field definitions are distributed throughout the CLM source code. The first
step is to find the ``hist_addfld1d`` or ``hist_addfld2d`` subroutine call for the history
field(s) of interest, by searching the code for the history field name. Then add a new
``hist_addfld1d`` or ``hist_addfld2d`` call that is mostly the same as the original, but:

1. Change the field name (``fname``), for example, changing ``TSOI`` to ``TSOI_ICE``

2. Change ``long_name`` to call out that this field just applies over ice landunits

3. Add a new argument to the call: ``l2g_scale_type='ice'``.

4. Possibly add ``default='inactive'`` so that this field only appears if explicitly
   requested in ``user_nl_clm``.

Here are some examples of currently-defined ``_ICE`` fields and their non-``_ICE``
counterparts:

.. code-block:: Fortran

   call hist_addfld2d (fname='TSOI',  units='K', type2d='levgrnd', &
        avgflag='A', long_name='soil temperature (vegetated landunits only)', &
        ptr_col=this%t_soisno_col, l2g_scale_type='veg')

   call hist_addfld2d (fname='TSOI_ICE',  units='K', type2d='levgrnd', &
        avgflag='A', long_name='soil temperature (ice landunits only)', &
        ptr_col=this%t_soisno_col, l2g_scale_type='ice')

.. code-block:: Fortran

   call hist_addfld1d (fname='FSH', units='W/m^2',  &
        avgflag='A', long_name='sensible heat not including correction for land use change and rain/snow conversion', &
        ptr_patch=this%eflx_sh_tot_patch, c2l_scale_type='urbanf')

   call hist_addfld1d (fname='FSH_ICE', units='W/m^2',  &
        avgflag='A', &
        long_name='sensible heat not including correction for land use change and rain/snow conversion (ice landunits only)', &
        ptr_patch=this%eflx_sh_tot_patch, c2l_scale_type='urbanf', l2g_scale_type='ice', &
        default='inactive')

Note that there are already many ``_ICE`` fields defined in the code, but many of these
are inactive by default (meaning that they need to be explicitly added to history field
lists via CLM's ``hist_fincl`` mechanism).
