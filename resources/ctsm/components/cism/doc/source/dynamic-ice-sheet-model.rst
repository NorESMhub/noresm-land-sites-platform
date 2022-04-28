.. _dynamic-ice-sheet-model:

*****************************
 The dynamic ice sheet model
*****************************

This section gives a brief overview of ice flow modeling and of
CISM, the dynamic ice sheet model in CESM. For more details,
including a technical description of the model, please see Rutt et al. (2009)
for the Glide shallow-ice dycore and Lipscomb et al. (2018) for the 
Glissade higher-order dycore.
(The latter paper will be submitted to *Geoscientific Model Development*
in June 2018, soon after the release of CESM2.0).
See also the CISM documentation, which is slightly out of date but
still provides a useful description of the Glissade dycore
and the climate model interface (Glad).

===============================
 The Community Ice Sheet Model
===============================

CISM is a thermomechanical ice sheet model that solves the
equations of ice flow, given suitable approximations and boundary
conditions. The source code is written primarily in Fortran 90 and 95.
The model resides on the github repository
(https://github.com/ESCOMP/cism), where it is under active development.
CISM2.1 is currently the default ice sheet model in CESM2.0.

CISM2.1 introduces a new dynamical core, Glissade, which runs in parallel
and solves equations for the conservation of mass, momentum, and
internal energy. Glissade supports several approximations of the
Stokes equations for ice flow: the shallow-ice approximation
(SIA), the shallow-shelf approximation (SSA), a depth-integrated viscosity
approximation (DIVA), and the Blatter-Pattyn (BP) approximation.
DIVA is the default solver for CISM in the CESM framework.

In previous versions of CESM with CISM coupling capabilities, the
dynamical core of the model was known as Glide and only solved the
shallow-ice approximation in serial. While this option is still available,
it is no longer supported.

The surface boundary conditions (e.g., the surface temperature and
surface mass balance) are supplied by a climate driver. When
CISM is run in CESM, the climate driver is Glad, which receives
the temperature and SMB from the coupler and passes them to the
ice-sheet grid. The lower boundary conditions are given
by a geothermal dataset, which supplies heat fluxes at the lower boundary,
and by a basal topography dataset.
Optionally, the basal topography can be modified at runtime by an isostasy model;
isostasy is turned off by default.

The new version of CISM adds a set of basal sliding and calving law
options of varying complexity. The default options and parameters 
were chosen based on standalone Greenland simulations (with SMB forcing
from the RACMO2 regional climate model) to support a
stable, reasonably accurate simulation of the Greenland Ice Sheet;
see Lipscomb et al. (2018) for details.
The default basal sliding option is pseudo-plastic sliding,
with parameters based on Aschwanden et al. (2016).
The default calving law is "no-float", with all floating ice
calving immediately.

The model currently has simple treatments of basal hydrology, including
a local till model used in conjunction with pseudo-plastic sliding.
More complex schemes for subglacial water hydrology and
evolution of basal till strength are being developed.


Configuring and running the model
---------------------------------

**Timesteps:** There are several kinds of timesteps in CISM.

1. The *forcing timestep* is the interval in hours between calls to
   Glad. Currently, the forcing timestep is the same as the *coupling
   interval* at which information is passed from the coupler to GLC. The
   forcing timestep is determined by the CISM namelist variables
   *dt\_option* and *dt\_count*. It is 24 hours by default for most
   compsets, but 1 year for T compsets. Note that these are the only
   values that have been tested extensively; results should be checked
   carefully if the forcing timestep is changed from these defaults.

2. The *mass balance timestep* is the interval over which
   accumulation/ablation forcing data is summed and averaged. This
   timestep is set in subroutine *glad\_mbc\_init* in module
   *glad\_mbal\_coupling.F90*. The current default is one year. With the
   default settings of the forcing timestep and mass balance timestep, Glad
   will accumulate forcing data from the coupler over 365 daily forcing
   timesteps and average the data. The mass balance timestep must be an
   integer multiple of the forcing timestep.

3. The *ice sheet timestep* is the interval in years between calls to
   the dynamic ice sheet model, Glissade (or Glide). The ice sheet timestep should
   divide evenly into the mass balance timestep. The current default is
   0.1 year for 4-km, and 0.5 year for 20-km.

Two optional runtime parameters can be used to make the time-stepping
more intricate:

1. The mass balance accumulation time, *mbal\_accum\_time* (in years),
   is the period over which mass balance information is accumulated
   before calling Glissade. By default, the mass balance accumulation time
   is equal to either the ice sheet timestep or the mass balance
   timestep, whichever is larger. (For current defaults, this means that
   *mbal\_accum\_time* is set equal to the mass balance timestep: 1
   year.) But suppose, for example, that the ice sheet timestep is 5
   years. If we set mbal\_accum\_time = 1.0, we accumulate mass balance
   information for 1 year and use this mass balance to force the ice
   sheet model, thus avoiding 4 additional years of accumulating mass
   balance data. **Note that this parameter cannot currently be modified
   via *user\_nl\_cism*, because it is not recommended that users change
   it.**

2. The timestep multiplier\ *, ice\_tstep\_multiply*, is equal to the
   number of ice sheet timesteps executed for each accumulated mass
   balance field. Suppose that the mass balance timestep is 1 year, the
   ice sheet timestep is 1 year, and ice\_tstep\_multiply = 10. Glad
   will accumulate and average mass balance information for 1 year, then
   execute 10 ice sheet model timesteps of 1 year each. In other words,
   the ice sheet dynamics is accelerated relative to the land and
   atmosphere. This option may be useful in CESM for multi-millennial
   ice-sheet simulations where it is impractical to run the atmosphere
   and ocean models for more than a few centuries.

These time options (apart from the forcing timestep) are set in
*cism.config*. This file contains (or may contain) the following
timestep information:

1. The ice sheet timestep *dt* (in years) is set in the section
   [*time*\ ] in the ice config file.

2. The mass balance time step is not set directly in the config file,
   but is set to the number of hours in a year (i.e., 8760 hours
   for a 365-day year).

3. The values of *ice\_tstep\_multiply* and *mbal\_accum\_time*, if
   present, are listed in the section [*GLAD climate*\ ].

Note that the total length of the simulation is not determined by
CISM, but is set in the file *env\_run.xml* in the case directory.

