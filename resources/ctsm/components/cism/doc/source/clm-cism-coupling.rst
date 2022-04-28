.. _clm-cism-coupling:

************************
Coupling CLM and CISM
************************

==============================
Modes of coupling CLM and CISM
==============================

Non-evolving ice sheet
----------------------

In typical CESM runs, CISM is not evolving; CLM computes the ice sheet surface mass
balance and sends it to CISM, but CISM's ice sheet geometry remains fixed over the course
of the run, and CISM does not send any fluxes to other CESM components. These
configurations use compsets with ``CISM2%NOEVOLVE`` in their long name (or this can be set
after setting up a case via the xml variable, ``CISM_EVOLVE``). In these runs, CISM serves
two roles in the system:

#. Over the CISM domain (typically Greenland in CESM2), CISM dictates
   glacier areas and topographic elevations, overriding the values on
   CLM's surface dataset. CISM also dictates the elevation of
   non-glacier land units in its domain, and only in this domain are
   atmospheric fields downscaled to non-glacier land units.

#. CISM provides the grid onto which SMB is downscaled.


Evolving ice sheet with two-way (interactive) coupling
------------------------------------------------------

Dynamic ice sheet evolution can be turned on by using a compset with ``CISM2%EVOLVE`` in
its long name, or by setting the xml variable ``CISM_EVOLVE`` after setting up a case. In
this configuration, CISM sends updated glacier areas and topographic elevations to CLM at
the end of each year. In addition, CISM sends fluxes of ice and liquid water to the
ocean. CLM responds to these changes by adjusting the areas of the glacier land unit and
each elevation class within this land unit, as well as the mean topographic heights of
each elevation class. Thus, CLM's glacier areas and elevations remain in sync with
CISM's.

When running with two-way coupling, conservation of water and energy in the CLM-CISM
coupling becomes important. There are still some unhandled edge cases in this regard (one
significant case being when CLM dictates an amount of glacial melt that exceeds the
available ice in a CISM grid cell), but we do take pains to achieve conservation in most
cases. An important mechanism to achieve this conservation is via a global renormalization
step done when mapping SMB from CLM to CISM, as described in :numref:`remapping_smb`. **By
default, this renormalization is done when running with a two-way-coupled ice sheet that
sends fluxes to other components (which is typically true in this case of an evolving ice
sheet), but is turned off in other cases. This leads to small differences in the remapped
SMB field in runs with an evolving vs. non-evolving ice sheet. This default can be
overridden via the driver namelist variable,** ``glc_renormalize_smb``, **keeping in mind
that setting this to** ``off`` **will break conservation for a configuration with an
evolving, two-way-coupled ice sheet.**

Evolving ice sheet with one-way (diagnostic) coupling
-----------------------------------------------------

A hybrid mode is also possible, in which CISM evolves dynamically but does not feed back
to the rest of the system. This configuration is enabled by turning on CISM evolution (via
using a ``CISM2%EVOLVE`` compset or changing the ``CISM_EVOLVE`` xml variable to
``TRUE``), but then changing the xml variable ``GLC_TWO_WAY_COUPLING`` to ``FALSE``. This
change results in changes to CLM and CISM:

- CLM will not respond to changes in CISM's glacier areas and topographic elevations. In
  addition, even at initialization, CLM is not affected by CISM's glacier areas and
  topographic elevations, instead specifying these from its initial conditions file, or
  from its surface dataset in a cold start or interpolated-start run (as would be the case
  when running with a stub GLC model, as described below). (In the future, we may want to
  allow the option that CLM synchronizes its glacier areas and topographic elevations at
  initialization, but then does not respond to any changes throughout the run.)

- CISM does not send fluxes of ice or liquid water to the ocean. Instead, fluxes from CLM
  are treated the same as in the non-evolving ice sheet case. Note that this behavior of
  whether or not CISM sends fluxes to the ocean can also be controlled independently from
  ``GLC_TWO_WAY_COUPLING``, via the CISM namelist variable, ``zero_gcm_fluxes``.

Stub GLC model (CISM absent)
----------------------------

It is also possible to run CESM with a stub glacier model rather than CISM by using
compsets with ``SGLC`` in place of ``CISM2%NOEVOLVE``. These configurations are similar to
those with a non-evolving ice sheet, with the following differences:

#. CLM's glacier areas and elevations will be taken entirely from CLM's initial conditions
   file, or from its surface dataset in a cold start or interpolated-start run.

#. In CLM, no downscaling of atmospheric forcings is done over non-glacier land units (but
   atmospheric forcings are still downscaled over glacier land units in areas with
   multiple glacier elevation classes).

#. There is no downscaling of CLM's surface mass balance and temperature fields to a
   higher-resolution glacier grid.

This configuration is useful for single-point or regional CLM simulations, or for
configurations not supported by CISM (such as runs with a Gregorian, rather than no-leap,
calendar), or simply to avoid needing to make CLM-CISM mapping files when running with a
new land/atmosphere grid.

===================================
Brief overview of CLM-CISM coupling
===================================

This section provides a brief overview of the two-way coupling between CLM and
CISM. Details on the coupling fields and their remapping are given in the following
sections.

CLM passes two fields to CISM: surface temperature and surface mass balance (SMB). Surface
temperature is provided only for glacier land units, whereas SMB is provided for both
glacier and vegetated land units. Over the CISM domain, CLM runs with "virtual" vegetated
and glacier land units, so that it is able to provide forcing fields even for grid cells
where a given land unit has zero area. SMB over glacier land units can be positive (ice
accumulation), negative (ice melt) or zero; SMB over vegetated land units can only be
positive or zero. A positive SMB over vegetated land units will trigger glacial inception
in any underlying CISM cell that is currently unglaciated.

These forcing fields are sent from CLM to the CESM coupler every time step (typically 1/2
hour). The coupler creates annual averages, remaps and downscales these annual averages to
the CISM grid, then calls CISM at the end of the year. CISM then uses these forcings to
drive its dynamics for the year, likely resulting in changes to glacier area and
topographic elevations. These new areas and elevations are sent back to CLM (via the
coupler); CLM updates its own areas and elevations over the CISM domain to match these for
the following year's computations.

There is an important but subtle point here: Although CLM is responsible for computing
SMB, including glacial inception, its glacier areas and elevations do not change until it
receives a signal to do so from CISM.

=====================================================
Resetting CLM's baseline states for dynamic landunits
=====================================================

.. important::

   The information in this section is subtle, but it is important for a fully-coupled run
   with an evolving ice sheet. In particular, it is important that you give some thought
   to CLM's ``reset_dynbal_baselines`` namelist flag in order to minimize the fictitious
   sensible heat fluxes generated by CLM's dynamic landunits code while still conserving
   water and energy. So please read this section carefully and ask for clarifications if
   you are still unsure when to set this flag.

   The information presented here applies to CLM development tags starting with
   ``ctsm1.0.dev031`` and to the CLM tag used for the ISMIP6 runs
   (``ismip6.n01_release-clm5.0.15``). It does **not** apply to other CLM versions used
   in ``CESM2.1.z`` releases. You should strive to use a version of CLM where this applies
   (which can be determined based on the availability of the ``reset_dynbal_baselines``
   namelist flag) in order to avoid very large dynbal ice and energy fluxes.

Overview of CLM's dynbal fluxes
-------------------------------

When subgrid column or landunit areas change in CLM --- as occurs with transient glaciers
--- the water and energy states of each column remain constant on a per-area basis. In
general, this results in a change in the grid cell-integrated water and energy. In order
to conserve water and energy in the coupled system, CLM generates adjustment
fluxes. Runoff fluxes (either positive or negative) are generated to conserve liquid water
and ice, and sensible heat fluxes are generated to conserve energy. Although needed for
conservation, these "dynbal" fluxes do not have a physical meaning. (See also the
"Transient Land Use and Land Cover Change" chapter of the `CLM Technical Note`_, and in
particular the "Mass and Energy Conservation" section in that chapter.)

CLM's glacier columns have a different state representation from soil columns: glacier
columns include nearly 50 m of ice that is (in some sense) "virtual", yet they do not
represent the soil under this ice. These two differences work in opposite directions, but
the first dominates because there is much more mass in the 50 m of glacial ice than there
is in a typical 50 m soil column. A naïve accounting would therefore generate large dynbal
fluxes in the transition between glacier and bare soil.

To reduce these fictitious dynbal fluxes, we subtract baseline values from glacier
columns, accounting for the two issues mentioned above: (1) we subtract the water and
energy in the glacier ice, because this is a virtual state in CLM, and (2) we add the
water and energy from the vegetated column, to account for the fact that we don't have an
explicit representation of soil-under-glacier. (This carries the assumption that the
soil-under-glacier has the same state as the initial vegetated state in that grid cell.)
We set these baselines in initialization, so they begin equal to the cold start
state. Water and ice in the glacial ice stay fixed over the course of a simulation, so the
cold start values should be the same as the current values at any point in time. The heat
content of the glacial ice does change over time, however, so these default baselines do
not sufficiently reduce the dynbal sensible heat fluxes. (In addition, the water and ice
contents in the soil column change over time, although this is a secondary concern.) The
resolution of this issue is discussed in the following sub-section.

Note that these baseline values do **not** include aboveground mass and energy --- that
is, any mass and energy in the snow pack or associated with surface water or vegetation.

Further reducing dynbal fluxes via ``reset_dynbal_baselines``
-------------------------------------------------------------

As mentioned above, the use of baselines set based on the cold start state is not
sufficient to reduce the dynbal sensible heat fluxes. For a grid cell that undergoes full
glaciation or deglaciation in a single year (the first of which can often happen in
practice in the model), CLM can generate dynbal sensible heat fluxes on the order of 10s
of W m\ :sup:`-2` every time step for the following year. To reduce these dynbal sensible
heat fluxes, CLM provides the namelist flag, ``reset_dynbal_baselines``. Use of this flag
can also further reduce the runoff fluxes, since water and ice contents in the soil column
can change over time.

Setting ``reset_dynbal_baselines = .true.`` in ``user_nl_clm`` at the start of a
simulation resets the baselines for glacier columns to values based on the states in CLM's
initial conditions file for that simulation. This can be done, for example, when
transitioning from an offline spinup to a fully-coupled run. The baseline values are saved
to CLM's restart file, so the same baselines will then persist for the remainder of this
simulation, as well as for any new cases branched off of this one. (This setting only
impacts startup and hybrid runs. It has no effect in a continue run, so it is safe to keep
this flag set to ``.true.`` for resubmissions of the case. It is an error for this to be
set in a branch run. Furthermore, this setting has no effect in a cold start run.) If the
states haven't changed much from the reset point to the point when glacier dynamics occur
(because the system was close to equilibrium when you reset baselines), then the dynbal
fluxes arising from glacier dynamics should be very small.

**Setting** ``reset_dynbal_baselines`` **to true in the midst of a series of simulations
has the potential to break water and energy conservation, so care is needed regarding
exactly when to set this flag.** Specifically, any water and energy that has previously
been added to or removed from states that contribute to these baselines (currently, (a)
glacier ice and (b) soil water and energy in the vegetated landunit in the same grid cell
as glaciers) will effectively be ignored when computing conservation corrections due to
land cover change. Instead, only the change in states from this point forward will be
considered.

**Here are guidelines for when this flag should and should not be set:**

1. If you are starting a fully-coupled (``B`` compset) case with an evolving,
   two-way-coupled ice sheet, using initial conditions from a case without a full ocean
   (``I`` or ``F`` compset): You should set ``reset_dynbal_baselines = .true.`` at the
   start of this fully-coupled case.

2. If you are transitioning from one coupled run with an evolving ice sheet to another
   (e.g., from a historical to a future transient run): Do **not** set
   ``reset_dynbal_baselines``, as this will break conservation.

3. What about the situation where you are starting a fully-coupled case with an evolving,
   two-way-coupled ice sheet, using initial conditions from a fully-coupled case with a
   non-evolving ice sheet? For example, you may be doing a series of (a) offline spinup
   (via an ``I`` compset), (b) further coupled spinup with a non-evolving ice sheet, (c)
   coupled run with an evolving ice sheet; should you reset the baselines at the start
   of (b) or at the start of (c)? Doing so at the start of (b) is safe (as for case (1),
   above), but what about doing so at the start of (c)? It's unclear whether resetting the
   dynbal baselines at this time is the "right" thing to do. Doing so would likely result
   in smaller dynbal fluxes, but may result in some loss of conservation. Referencing the
   two ways to think about the dynbal fluxes (in section :numref:`Ways to think about
   subtraction of baselines`): If we think of the baselines as being arbitrary, then it
   seems safe to reset them at this time, because the dynbal baselines aren't invoked
   until the onset of transient glacier areas, so it seems safe to reset them up until
   that transient behavior begins. However, if we think of the baselines as being more
   physically-based, then it seems wrong to reset them at this time, because there may
   (for example) have truly been some energy absorbed by CLM's glacier ice since the start
   of the coupled run, and this energy should be released back to the system when the ice
   sheet retreats.

Note that the value of this flag has no significant impact on cases with a non-evolving
ice sheet.

Confirming that the dynbal fluxes are small in your simulation
--------------------------------------------------------------

When running a coupled simulation with an evolving ice sheet, it is a good idea to
periodically check CLM's dynbal fluxes to ensure that they remain relatively small. The
three relevant fluxes are ``EFLX_DYNBAL``, ``QFLX_LIQ_DYNBAL``, and
``QFLX_ICE_DYNBAL``. It is a good idea to check these fluxes for the first few years of
your simulation, and then periodically spot-check them at various other points throughout
the run.

The point of this is to ensure that CLM's fluxes to the ocean and atmosphere aren't being
dominated by these fictitious, conservation-correction fluxes. These fluxes remain
constant throughout a given year, so it is sufficient to check a single monthly average
for a given year, or to only output annual averages of these fields.

More details and thoughts on these dynbal fluxes
------------------------------------------------

It is not necessary that you read this sub-section, but we provide it in case you would
like more details and thoughts on these dynbal fluxes.

.. _Ways to think about subtraction of baselines:

Two ways to think about the subtraction of baselines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It seems that there are two ways to think about this subtraction of baselines for the sake
of computing dynbal fluxes:

1. More physically-based: we choose which states to subtract and add via baselines in
   order to have a state representation that more closely matches reality. For glaciers,
   we subtract the virtual ice column, and add the missing soil-under-glacier.

2. Choose baselines in order to minimize dynbal fluxes. We are free to choose whatever
   baselines we want in order to minimize fluxes (as long as these baselines are constant
   in time --- though I think it is fine for them to vary for different columns within or
   between grid cells). We can think of counting the water and energy contents relative to
   some arbitrary "zero" state (where the baseline values give this "zero" state), or
   roughly equivalently, counting the change in water and energy contents over time
   relative to some starting point. One way to think about this is that we have some
   unknown states (e.g., the soil under glacier); we are free to keep these values in an
   "unknown" state (rather than assigning them some arbitrary value) until the last
   possible moment.

I’m not sure if (2) is always acceptable. For glaciers, it turns out that the two methods
lead us to the same place for mass, though not necessarily for energy. For cases where the
two ways of thinking lead us to different places, I’m not sure if (2) is an acceptable way
to think about these baselines, in terms of conservation.

Other resources
~~~~~~~~~~~~~~~

See also the "Transient Land Use and Land Cover Change" chapter of the `CLM Technical
Note`_, and in particular the "Mass and Energy Conservation" section in that chapter.

For more details and diagrams of water and energy conservation with dynamic landunits, see
the `Dynamic landunits water and energy conservation presentation`_.

=====================================
Fields exchanged between CLM and CISM
=====================================

CLM to CISM
-----------

Overview
~~~~~~~~

CLM passes three fields to the coupler for the sake of CLM-CISM coupling: surface mass
balance (SMB), surface temperature, and surface topographic height. The first two are
remapped/downscaled and sent to CISM, whereas surface topographic height is just used by
the coupler itself in the downscaling routine. Each CLM grid cell sends :math:`N+1` copies
of each of these fields, where :math:`N` is the number of elevation classes, and the
additional :math:`1` is for the bare/vegetated portion of the grid cell. (However, surface
temperature and topographic height are irrelevant for the bare/vegetated portion.) CLM
sends values of these fields every time step (typically 1/2 hour). The coupler creates
annual averages of the fields before remapping and downscaling them to the CISM grid.

Details of CLM's glacier treatment, including the surface mass balance calculation, are
given in the "Glaciers" chapter of the `CLM Technical Note`_.

Note that the CLM-CISM coupling does *not* currently have the capability to couple using a
positive degree day (PDD) scheme.

Surface mass balance (SMB)
~~~~~~~~~~~~~~~~~~~~~~~~~~

The SMB calculation is described in detail in the "Glaciers" chapter of the `CLM Technical
Note`_. Here we just summarize a few important points.

CLM's SMB currently only considers changes in the ice column, *not* changes in the snow
pack. A positive SMB (ice accumulation) is generated when the snow pack grows beyond its
prescribed limit (snow capping). A negative SMB (ice melt) is generated when CLM's ice
column experiences melt. A positive (but *not* negative) SMB can be generated over CLM's
vegetated land unit; this condition triggers glacial inception in CISM.

Surface temperature
~~~~~~~~~~~~~~~~~~~

CLM sends surface temperature to provide an upper boundary condition for CISM's
temperature calculations. In CLM, this is the temperature of the top ice layer.

Surface topographic height
~~~~~~~~~~~~~~~~~~~~~~~~~~

The average topographic height of each glacier elevation class is needed for the
downscaling, as described below. When running two-way-coupled, CLM's topographic heights
are obtained via averages of the underlying CISM grid cells. However, CLM sends these
heights back to the coupler so that the downscaling routine has access to these values
regardless of whether we are running one-way or two-way coupled.

CISM to CLM
-----------

Mask of ice-covered vs. ice-free points
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each grid cell in CISM is classified as either ice-covered or ice-free (there are no
partially-ice-covered cells). CISM uses different definitions of ice-covered for different
purposes; for the purposes of this coupling, any cell with ice thickness greater than zero
is considered to be ice-covered. This field is used in conjunction with surface height to
determine the total glacier fraction in each CLM grid cell, as well as the fractional
cover of each CLM glacier elevation class.

This field is needed even when running one-way-coupled, because it is used in the
CLM-to-CISM downscaling (to determine which CISM grid cells should receive SMB from
glacier land units vs. vegetated land units).

Surface height
~~~~~~~~~~~~~~

CISM sends the surface height of each grid cell. For glaciers, this is the height of the
ice surface. For ice-free points, this is the topographic height. This field is used to
determine the fractional cover and mean elevation of each CLM glacier elevation class, as
well as the mean elevation of the vegetated land unit in each CLM grid cell within the
CISM domain.

This field is needed even when running one-way-coupled, because it is used in the
CLM-to-CISM downscaling.

.. _ice_sheet_grid_mask:

Ice sheet grid mask
~~~~~~~~~~~~~~~~~~~

CLM needs a way to know where CISM is sending valid data, and thus knowing where it should
update its glacier areas and elevations. This is provided via the "ice sheet grid
mask". CISM sets this field to 1 for all points that are either bare land or ice-covered
(including floating ice), and 0 for open ocean (this is determined based on the criterion,
``usrf > 0``; in principle, this criterion could cause problems if there were a grid cell
with ``usrf <= 0`` despite having non-zero ice thickness). This mask is important so that
CLM maintains the values specified by its surface dataset outside the CISM domain, as well
as in areas that CISM considers to be open ocean but CLM considers to be at least
partially land-covered.

This mask is also used in the coupler to determine the ice sheet region over which SMB
must be conserved in the SMB remapping process (see :numref:`remapping_smb`). We assume
that we can use the same mask for these two purposes (i.e., both for defining where CISM
is sending valid data and for defining where CISM can receive SMB). (This use of the ice
sheet grid mask more closely aligns with the use of the mask where we are potentially
sending non-zero fluxes, described in :numref:`mask_for_nonzero_fluxes`. However, we can't
use that mask for the remapping, because we then could only perform renormalization if we
were running with two-way coupling. For this reason, it is important that these two masks
are defined in the same way.)

One subtlety regards the treatment of land points that fall within CISM's rectangular grid
but are outside of Greenland - chiefly, Ellesmere Island. We do not want CISM to handle
these points, and we want CLM to maintain the glacier cover from its surface dataset
there. To accomplish this, all land points outside of Greenland are artificially submerged
to below sea level in a preprocessing step applied to CISM's input file. Thus, these
points are not included in the ice sheet grid mask.

This mask is (slightly) dynamic in time, both because of its inclusion of ice shelves and
because (with isostasy) CISM's land-ocean boundary can change in time.

This mask is regridded to the CLM grid using simple area-conservative
remapping. (Elevation classes are irrelevant here.)

.. _mask_for_nonzero_fluxes:

Ice sheet mask where we are potentially sending non-zero fluxes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CLM also needs to know where CISM is a fully-coupled part of the climate system - i.e.,
where it is potentially sending non-zero runoff fluxes to the ocean. CLM uses this
information to determine how to route its positive and negative SMB terms in order to
conserve water. This is described in detail in the "Glaciers" chapter of the `CLM
Technical Note`_. In particular, see the discussion of the dependence on
*glc\_dyn\_runoff\_routing* in that chapter: CLM's *glc\_dyn\_runoff\_routing* is true
within this mask and false outside of it.

This mask is currently a subset of the ice sheet grid mask. Currently, it is identical to
the ice sheet grid mask if we are running with an evolving, two-way-coupled ice sheet, and
otherwise is zero everywhere (and, as described in :numref:`ice_sheet_grid_mask`, this
relationship should remain true, because the ice sheet grid mask is used in the coupler in
a way that closely matches the use of this second mask). In the future, when we allow
multiple ice sheets in CESM (e.g., Greenland and Antarctica), it is possible that one ice
sheet will operate two-way-coupled while another is one-way-coupled. In this case, this
mask would match the ice sheet grid mask for the two-way-coupled ice sheet and would be
zero for the other.

Note that, like the ice sheet grid mask, this mask excludes CISM's open ocean grid
cells. CISM does not currently have code in place to handle inputs of SMB over open ocean
(e.g., routing this SMB directly to the ocean), so CLM needs to treat these open ocean
areas the same as points completely outside CISM's domain for conservation reasons.

This mask, like the ice sheet grid mask, is regridded to the CLM grid using simple
area-conservative remapping. (Elevation classes are irrelevant here.)

Heat flux
~~~~~~~~~

Hooks are in place for CISM to send the heat flux from the ice interior to the surface to
each CLM elevation class. However, this is not yet fully implemented, leading to a small
loss of energy conservation.

This flux is only applicable when running with an evolving, two-way-coupled ice sheet.

Other fields sent from CISM
---------------------------

Ice runoff (calving)
~~~~~~~~~~~~~~~~~~~~

CISM sends an ice runoff - i.e., calving - flux directly to the ocean (POP). When this flux
reaches the ocean, POP immediately melts the ice, so this ice flux is equivalent to a
negative salinity flux together with a negative heat flux. Hooks are in place to instead
direct this flux to the sea ice model, but CESM's sea ice model is not yet capable of
simulating icebergs.

This flux is only applicable when running with an evolving, two-way-coupled ice sheet.

Liquid runoff (basal melting)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CISM sends a liquid runoff flux directly to the ocean; this is generated from basal
melting. Note that this term does *not* include surface melting: the surface melt term is
sent from CLM to the ocean via the runoff routing model.

This flux is only applicable when running with an evolving, two-way-coupled ice sheet.

======================================
Remapping fields sent from CLM to CISM
======================================

.. _remapping_smb:

Remapping surface mass balance from CLM to CISM
-----------------------------------------------

As described above, the surface mass balance (SMB) of ice sheets is computed by CLM
for each column (i.e., elevation class) of each glaciated landunit in each grid cell on the land grid.
The SMB is then remapped by the coupler to the finer ice sheet grid and passed to CISM.
When CESM is run with two-way, interactive coupling between glaciers and ice sheets, we want to conserve
the total amount of water in the system, while also mapping SMB smoothly and accurately between grids.

Specifically, we would like the SMB remapping to satisfy the following requirements:

1. ``Conservation``: For any ice sheet defined by a CISM domain, the sum over CLM grid cells of the SMB sent to the coupler
   is equal (within machine roundoff) to the sum over CISM grid cells of the SMB received from the coupler.
   Note that this is a global (i.e., whole-ice-sheet) rather than a local requirement.

2. ``Smoothness``: The remapping is smooth and continuous on the CISM grid, without obvious imprinting of the coarser CLM grid.

3. ``Accuracy``: The SMB applied in CISM at a given location is close to the value computed by CLM at that location
   and elevation.

4. ``Sign preservation``: Any positive SMB in CLM maps to a positive SMB in CISM, and likewise for negative SMB.

Here we describe the algorithm used by the coupler to satisfy these requirements.  First we introduce some notation:

- ``lfrac`` is the fraction of a CLM grid cell that does not overlap the ocean grid and is treated as land.
  Since the ocean and land grids are non-conforming, we can have ``0 < lfrac < 1`` in CLM cells near the ocean boundary.

- ``Sg_icemask_g`` is a binary mask on the CISM grid that identifies cells which are ice-covered and/or land-covered,
  and therefore are eligible to apply a nonzero SMB from CLM.  (Ice-free land cells can have a positive SMB,
  and ice-covered cells can have an SMB of either sign.)  CISM cells that are ice- and/or land-covered have
  ``Sg_icemask_g = 1``, and ice-free ocean cells have ``Sg_icemask_g = 0``.

- ``Sg_icemask_l`` is obtained by mapping ``Sg_icemask_g`` from the CISM grid to the CLM grid.
  Since the grids are different, this mask is not binary; we can have ``0 < Sg_icemask_l < 1``.

- ``g = min(lfrac, Sg_icemask_l)`` is the fraction of CLM-computed SMB that is sent to CISM via the coupler.
  The remaining SMB is not sent to CISM.  A fraction ``lfrac - g`` is sent by the coupler to the runoff model;
  this is the fraction of the cell that is land-covered but does not overlap the CISM grid.  The remaining
  fraction, ``1 - lfrac``, is not sent to either CISM or the runoff model, because any precipitation in
  the non-land part of a CLM cell has already fallen into the ocean.

- :math:`A_i` is the area of a CLM grid cell.  CLM and the coupler agree on the grid cell area.

- :math:`A_j` is the area of a CISM grid cell according to CISM, and :math:`A_j^c` is the area according to the coupler.
  These two areas differ because CISM's stereographic projection does not conserve area.

- :math:`f_{ik}` is the fraction of CLM grid cell *i* occupied by glacier ice in elevation class *k*.

- :math:`q_{ik}` is the SMB of CLM grid cell *i* in elevation class *k*.

- :math:`q_j` is the SMB remapped to CISM grid cell *j*.

Using this notation, we can express the conservation requirement (1):

.. math::
   :label: conservation

   \sum_i{g_i A_i \sum_k{f_{ik} q_{ik}}} = \sum_j{A_j q_j},

where the sum on the LHS is taken over grid cells *i* and columns *k* on the CLM grid, and
the sum on the RHS is taken over grid cells *j* on the CISM grid.

To additionally satisfy sign preservation (4), Eq. :eq:`conservation` is replaced by two equations:
one for the accumulation zone (limited to cells and columns with :math:`q > 0`),
and one for the ablation zone (limited to cells and columns with :math:`q < 0`).

Requirements (2) and (3) are ensured by bilinear remapping in the horizontal plane combined
with linear interpolation in the vertical. These operations are smooth but not conservative.
Thus, in order to satisfy all four requirements, bilinear remapping and vertical interpolation
are followed by a normalization step that guarantees conservation in both the accumulation and ablation zones.

The algorithm proceeds as follows:

1. In CLM, compute the SMB for each grid cell and elevation class (EC) that has nonzero overlap (:math:`g > 0`)
   with the CISM domain, and send to the coupler.

2. Accumulate and average the SMB for each EC over the CLM-CISM coupling interval
   (typically 1 year).

3. At the end of the coupling interval, compute the total SMB in the accumulation and ablation zones of CLM.

4. For each EC, do a bilinear remapping of SMB from the CLM grid to the CISM grid.

5. For each CISM grid cell, do a linear interpolation in elevation space between adjacent ECs, to compute
   the SMB at the CISM cell elevation.  If a cell lies above or below the range of elevations in the
   various ECs, values from the highest and lowest ECs are extrapolated.  *Note: State whether this 
   is a linear extrapolation from the two highest and lowest ECs, or simply an extension of the highest and lowest values.*

6. Compute the total (uncorrected) SMB in the accumulation and ablation zones of CISM.

7. Apply a normalization correction for conservation.  For example, suppose
   :math:`Q_{\text{acc}}^{\text{clm}} = 1.05 \, Q_{\text{acc}}^{\text{cism}}`,
   where :math:`Q_{\text{acc}}` is the total SMB in the accumulation zone of a given model.
   Then in every CISM cell that lies in the accumulation zone, we would multiply the SMB by
   :math:`Q_{\text{acc}}^{\text{clm}}\, / \, Q_{\text{acc}}^{\text{cism}} = 1.05` (and similarly for the ablation zone).

8. Send the normalized SMB on the CISM grid to CISM.

Step 1 is done in CLM at every time step.  The other steps are done in the coupler, with steps 3-8
carried out at the end of the coupling interval.

In practice, normalization factors usually fall between 0.9 and 1.1 at typical CESM global grid resolutions
of :math:`\sim 1^\circ`.  Thus, if an SMB of 1 m/yr is computed in CLM, the downscaled SMB in CISM might differ
by up to 10%.  If we used conservative rather than bilinear remapping, differences also would be up to about 10%,
because of area distortions on CISM's polar stereographic grid.
Thus the local errors for bilinear remapping and renormalization are similar to the local errors for conservative remapping.
Bilinear remapping, however, is far smoother; smoothness is obtained at the cost of local conservation.

Remapping surface temperature from CLM to CISM
----------------------------------------------

Surface temperature is remapped similarly to surface mass balance (see
:numref:`remapping_smb`), but without the renormalization and without separation into
accumulation vs. ablation zones:

1. CLM computes surface temperature for each grid cell and elevation class (EC).

2. The coupler accumulates and averages surface temperature for each EC over the CLM-CISM
   coupling interval (typically 1 year).

3. For each EC, the coupler does a bilinear remapping of surface temperature from the CLM
   grid to the CISM grid.

4. For each CISM grid cell, the coupler does a linear interpolation in elevation space
   between adjacent ECs, to compute the surface temperature at the CISM cell elevation.

=========================================
CLM's glacier regions and their behaviors
=========================================

CLM divides the world's glaciers and ice sheets into multiple regions that differ in
various respects. For a detailed description of these different glacier behaviors, see the
"Glaciers" chapter of the `CLM Technical Note`_. Here we focus on the user interface for
controlling these behaviors.

Two sets of CLM inputs work together to determine glacier physics in each grid cell: the
``GLACIER_REGION`` field on the surface dataset and a set of namelist options (whose names
begin with ``glacier_region``; see the `CLM Namelist Definitions`_ for details). The
``GLACIER_REGION`` field is an integer from 1 through the number of glacier regions, as
well as 0 for all grid cells that are not part of a distinct other region. The various
``glacier_region`` namelist options then specify the behavior for each of these
regions. The first element in each namelist array specifies the behavior of
``GLACIER_REGION`` 0, the second element specifies the behavior of ``GLACIER_REGION`` 1,
etc.

(We rely on CLM's surface dataset rather than making behaviors dependent on CISM's ice
sheet grid mask because we don't want CLM physics to change just because CISM is using a
different grid.)

.. important::

   If you want ice sheet forcings (SMB and surface temperature) for regions other than the
   standard Greenland CISM domain, it is **critical** that you give some thought to this
   ``GLACIER_REGION`` field and the associated namelist options: You will need to ensure
   that your glacier regions are set up to have virtual elevation classes
   (``glacier_region_behavior = 'virtual'``), and that glaciers produce a valid SMB field
   (``glacier_region_melt_behavior = 'replaced_by_ice'``) wherever you want forcings for
   CISM.

.. _CLM Technical Note: https://escomp.github.io/ctsm-docs

.. _CLM Namelist Definitions: http://www.cesm.ucar.edu/models/cesm2/settings/current/clm5_0_nml.html

.. _Dynamic landunits water and energy conservation presentation: https://drive.google.com/open?id=1PJNIEfKTIxFeDZ2rTrwXw555PrxTBpb2
