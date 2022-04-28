import os
import sys
import gc
import pylab as p
from pylab import *
import numpy as np
import netCDF4
from scipy import stats
from mpl_toolkits.basemap import cm,Basemap,maskoceans,shiftgrid,addcyclic
from matplotlib.colors import SymLogNorm
from matplotlib.ticker import LogFormatter
from matplotlib.ticker import SymmetricalLogLocator
import argparse
import os.path

##############################################################################################
# READ DATA
##############################################################################################
#
# This is a script for plotting the the tendency terms that contribute to the total tendencies
# of the cloud microphysics scheme by Morrison and Gettelman in NorESM2/CAM5. 
#
# Please edit this part of the script to read you input files and choose the budget you want
# to plot. 
#
# The input file should be 3D output from NorESM2 with history_budget=.true.
#
# Currently the script reads only 1 time step and reads all variables from the same file.
#
# You can compare two files with diff=True. If diff is not PD-PI plaese edit the title section
# below or set a custom title. You can also turn off title by setting title=False.
#
# Projections
# map    will give you lat-lon plots of column tendencies sorted by global average contribution to 
#        the total budget.
# zm     will give you zonal mean projection of the zonal average tendencies sorted by global average
#        contribution to the total budget.
# global will give you histogram of global average tendencies sorted by contribution to the total
#        budget (Not so pretty yet).
#
# Written by Anna Lewinschal anna@misu.su.se
##############################################################################################

stateName={'cldliq':'liquid', 'cldice':'ice'}
diffTitle={'normal':'', 'diff':'PD-PI'}
mgVersion={'MG10':10, 'MG15':15, 'MG20':20}

if __name__ == '__main__':

   parser = argparse.ArgumentParser()
   parser.add_argument('--version',choices=['MG10','MG15','MG20'],default='MG10',help='MG-version')
   parser.add_argument('--plottype',choices=['normal','diff'],default='normal')
   parser.add_argument('--projection',choices=['map','zm','global'],default='global',help='desired plot type')
   parser.add_argument('--state',choices=['cldliq','cldice'],default='cldliq')
   parser.add_argument('--budget',choices=['mass','number'],default='mass')
   parser.add_argument('--inputFile', default="/lustre/storeB/users/alfg/CESM1_5/MGRuns/MG10PDb2d4Free/atm/hist/MG10PDb2d4Free_YA_0003_0004.nc")
   parser.add_argument('--diffInputFile', default="")
   parser.add_argument('--outputFile',default="result.png")
   parser.add_argument('--icenucleation',choices=['cam53',"classnuc","bestguess"],default="bestguess")
   args = parser.parse_args()

   # Choose budget

   # Mass or number?
   budget= args.budget

   # Liquid or ice?
   state = args.state

   # Plot projection
   proj = args.projection

   print( "YOUR CONFIG: "+budget  + "  " +  state +" " +  proj + "  " + args.version + " " + args.inputFile)
   # Diffplot?
   diff = (args.plottype == 'diff')

   # Number of panels to show
   # 8 panels works best for map
   # 10 works for zm
   panels = 10
   pan_row = 2
   pan_col = panels/pan_row

   # For histogram: number of bars:
   nbars = 9
 
   # colours
   bar_colour =(0.2,0.3,0.9)       # a blue colour
   bar_colour_bdgt = (0.9,0.4,0.5) # a pink colour
   
   # Set y-axis limits
   ######################
   #y-limits for ice mass
   qi_ymin=-1.0e-9
   qi_ymax=1.0e-9

   # Poster: y-limits for liquid mass
   qc_ymin=-2.1e-9
   qc_ymax=0.5e-9

   # Poster: y-limits for ice number
   ni_ymin=-3.5
   ni_ymax=2.5

   # Poster: y-limits for liquid number
   nc_ymin=-150
   nc_ymax=650

   # font sizes
   ###############
   # axis label
   ax_fsz = 14
   # tick label
   tk_fsz = 12
   # title 
   tt_fsz = 16

   # figure size
   fg_sz_x=9
   fg_sz_y=7

   # Logscale for plotting?
   logscale = False
   logscale = True

   # Output from MG1.0 or MG1.5 
   usedMGVersion=mgVersion[args.version]

   #ice nucleation scheme uses classnuc
   use_classnuc=False
   if(args.icenucleation == "bestguess"):
      if usedMGVersion > 15:
         use_classnuc=True
   if(args.icenucleation == "classnuc"):
      use_classnuc=True

   plottitle=stateName[args.state] + " " + budget+ " budget " + args.version  
   plottitle+= "  " +diffTitle[args.plottype] 


   # Custom title
   #plottitle = 'Your title'

   # Don't want title?
   #title=False
   title=True

   if not (os.path.exists(args.inputFile)):
      print("inputfile : " + args.inputFile + "does not exist")
      sys.exit(1)

   ##############################################################################################
   # Read netcdf file
   id_nc = netCDF4.Dataset(args.inputFile)

   if (os.path.exists(args.diffInputFile)):
      id_nc2 = netCDF4.Dataset(args.diffInputFile)


   ##############################################################################################
   # No need to edit anything in the sections below
   ##############################################################################################
   ##############################################################################################
   ##############################################################################################
   # Define variables
   n_var = list()
   n_var_long  = list()

   # Number budget
   if budget == 'number':
      if state == 'cldliq':

         n_var.append('MPDNLIQ'),      n_var_long.append('CLDNLIQ tendency - Morrison microphysics')
         n_var.append('NPSACWSO'),     n_var_long.append('NC tendency accretion by snow')
         n_var.append('NSUBCO'),       n_var_long.append('NC tendency evaporation of droplet')  # this is zero!
         n_var.append('NPRAO'),        n_var_long.append('NC tendency accretion')
         n_var.append('NPRC1O'),       n_var_long.append('NC tendency autoconversion')
         n_var.append('NQCSEDTEN'),    n_var_long.append('NC tendency sedimentation')
         n_var.append('NMELTO'),       n_var_long.append('NC tendency melting')
         n_var.append('NHOMOO'),       n_var_long.append('NC tendency homogeneous freezing')
         n_var.append('NPCCNO'),       n_var_long.append('NC activation')
         n_var.append('NCTNNCLD'),     n_var_long.append('NC removal no cloud water')
         n_var.append('NCTNSZMN'),     n_var_long.append('NC $\gamma$-distribution adjustment min slope')
         n_var.append('NCTNSZMX'),     n_var_long.append('NC $\gamma$-distribution adjustment max slope')

         if usedMGVersion == 10:
            n_var.append('NCTNCONS'),  n_var_long.append('correction for conservation of NC')
            n_var.append('NCTNNBMN'),  n_var_long.append('NC correction min number')

      elif state == 'cldice':
         n_var.append('MPDNICE'),      n_var_long.append('CLDNICE tendency - Morrison microphysics')
         n_var.append('NQISEDTEN'),    n_var_long.append('NI tendency sedimentation')
         n_var.append('NIMELTO'),      n_var_long.append('NI tendency melting')
         n_var.append('NIHOMOO'),      n_var_long.append('NI tendency homogeneous freezing')
         n_var.append('NSACWIO'),      n_var_long.append('NI tendency from HM')
         n_var.append('NSUBIO'),       n_var_long.append('NI tendency evaporation')
         n_var.append('NPRCIO'),       n_var_long.append('NI tendency autoconversion snow')
         n_var.append('NPRAIO'),       n_var_long.append('NI tendency accretion snow')
         n_var.append('NNUDEPO'),      n_var_long.append('NI deposition')
         n_var.append('NNUCCDO'),      n_var_long.append('NI nucleation')
         n_var.append('NITNCONS'),     n_var_long.append('correction for conservation of NI')
         n_var.append('NITNNCLD'),     n_var_long.append('NI removal no cloud ice')
         n_var.append('NITNSZMN'),  n_var_long.append('NI $\gamma$-distribution adjustment min slope')
         n_var.append('NITNSZMX'),  n_var_long.append('NI $\gamma$-distribution adjustment max slope')

         if usedMGVersion==20:
            n_var.append('NFRZR'),  n_var_long.append('Freezing of rain droplets to snow')
            n_var.append('NNUCCRI'),n_var_long.append('Freezing of rain droplets to ice')
                        
      #Immersion freezing is only included in ice budget if "classnuc_ice" is used
      if (state=='cldice') and (not use_classnuc):
         #don't include immersion freezing in budget
         dontDoAnyThingHere = True
      else:
         #immersion freezing is included in all budgets
         n_var.append('NNUCCCO'),   n_var_long.append('NC/NI tendency immersion freezing')
      # contact freezing In both cloud liquid and ice states:
      n_var.append('NNUCCTO'),   n_var_long.append('NC/NI tendency contact freezing')

   # Mass budget
   elif budget == 'mass':
      if state == 'cldliq':

         n_var.append('MPDLIQ'),   n_var_long.append('CLDLIQ tendency - Morrison microphysics')
         n_var.append('BERGSO'),   n_var_long.append('Conv. of cloud water to snow from bergeron')
         n_var.append('QCRESO'),   n_var_long.append('Residual condensation term for cloud water ')
         n_var.append('PRCO'),     n_var_long.append('Autoconversion of cloud water')
         n_var.append('PRAO'),     n_var_long.append('Accretion of cloud water by rain')
         n_var.append('PSACWSO'),  n_var_long.append('Accretion of cloud water by snow')
         n_var.append('QCSEDTEN'), n_var_long.append('CLDLIQ tend. from sedimentation')


      elif state == 'cldice':
         n_var.append('MPDICE'),   n_var_long.append('CLDICE tendency - Morrison microphysics')
         n_var.append('CMEIOUT'),  n_var_long.append('Rate of dep-subl of ice within the cloud')
         n_var.append('QIRESO'),   n_var_long.append('Residual dep term for cloud ice ')
         n_var.append('PRCIO'),    n_var_long.append('Autoconversion of cloud ice')
         n_var.append('PRAIO'),    n_var_long.append('Accretion of cloud ice by snow')
         n_var.append('QISEDTEN'), n_var_long.append('CLDICE tendency from sedimentation')
         n_var.append('MNUDEPO'),  n_var_long.append('deposition')

         if usedMGVersion==20:
            n_var.append('FRZR'),  n_var_long.append('Freezing of rain droplets to snow')
            n_var.append('MNUCCRI'),n_var_long.append('Freezing of rain droplets to ice')

      # In both cloud liquid and ice budgets:
      n_var.append('BERGO'),   n_var_long.append('Conv. of cloud water to ice from bergeron')
      n_var.append('MNUCCCO'), n_var_long.append('Immersion freezing of cloud water')
      n_var.append('MNUCCTO'), n_var_long.append('Contact freezing of cloud water')
      n_var.append('HOMOO'),   n_var_long.append('Homogeneous freezing of cloud water')
      n_var.append('MELTO'),   n_var_long.append('Melting of cloud ice')
      n_var.append('MSACWIO'), n_var_long.append('Conv. of cloud water from rime-splintering')





   # Read dimensions
   lat   = id_nc.variables['lat'][:]
   lon   = id_nc.variables['lon'][:]
   lev   = id_nc.variables['lev'][:]
   hyai  = id_nc.variables['hyai'][:]
   hybi  = id_nc.variables['hybi'][:]
   ps    = id_nc.variables['PS'][:]
   if diff:
      ps2    = id_nc2.variables['PS'][:]

   varnb = len(n_var)
   varnb2 = varnb+1   
   # Read variables

   var = empty((varnb,len(lev),len(lat),len(lon)), dtype=float32)
   if diff:
      var2 = empty((varnb,len(lev),len(lat),len(lon)), dtype=float32)

   for i in range(0,varnb):
      var[i]  = id_nc.variables[n_var[i]][:,:,:,:] # time, lev, lat, lon
      if diff:
         var2[i]  = id_nc2.variables[n_var[i]][:,:,:,:] # time, lev, lat, lon



      # change sign
      if budget == 'number':
         if state == 'cldliq':
            if   n_var[i] == 'NNUCCCO' \
              or n_var[i] == 'NNUCCTO' \
              or n_var[i] == 'NPSACWSO'\
              or n_var[i] == 'NPRAO'   \
              or n_var[i] == 'NPRC1O'  \
              or n_var[i] == 'NCRES'   \
              or n_var[i] == 'NHOMOO'  \
              or n_var[i] == 'NCTNCONS'\
              or n_var[i] == 'NCTNNBMN'\
              or n_var[i] == 'NCTNSZMN'\
              or n_var[i] == 'NCTNSZMX'\
              or n_var[i] == 'NCTNNCLD':
               var[i]=-var[i]
               if diff:
                  var2[i]=-var2[i]

         elif state == 'cldice':
            if   n_var[i] == 'NPRCIO'  \
              or n_var[i] == 'NPRAIO'  \
              or n_var[i] == 'NIRES'   \
              or n_var[i] == 'NITNCONS'\
              or n_var[i] == 'NITNNBMN'\
              or n_var[i] == 'NITNSZMN'\
              or n_var[i] == 'NITNSZMX'\
              or n_var[i] == 'NITNNCLD'\
              or n_var[i] == 'NIMELTO' :
               var[i]=-var[i]
               if diff:
                  var2[i]=-var2[i]

      elif budget == 'mass':
         if state == 'cldliq':
            if   n_var[i] == 'BERGSO'  \
              or n_var[i] == 'PRCO'    \
              or n_var[i] == 'PRAO'    \
              or n_var[i] == 'PSACWSO' \
              or n_var[i] == 'BERGO'   \
              or n_var[i] == 'MNUCCCO' \
              or n_var[i] == 'MNUCCTO' \
              or n_var[i] == 'HOMOO'   \
              or n_var[i] == 'MSACWIO' :
               var[i]=-var[i]
               if diff:
                  var2[i]=-var2[i]
         elif state == 'cldice':
            if   n_var[i] == 'PRCIO'   \
              or n_var[i] == 'PRAIO'   \
              or n_var[i] == 'MELTO':
               var[i]=-var[i]
               if diff:
                  var2[i]=-var2[i]
   # Close file
   id_nc.close()
   if diff:
      id_nc2.close()

   ##############################################################################################


   # vertical pressure coordinate
   dp = zeros_like(var[0,:,:,:])
   if diff:
      dp2 = zeros_like(var[0,:,:,:])

   for i in range(0,len(hyai)-1):
      p1 = hyai[i]*100000.+hybi[i]*ps
      p2 = hyai[i+1]*100000.+hybi[i+1]*ps
      dp[i,:,:] = p2 -p1 
      if diff:
         p12 = hyai[i]*100000.+hybi[i]*ps2
         p22 = hyai[i+1]*100000.+hybi[i+1]*ps2
         dp2[i,:,:] = p22 -p12 


   # horizontal grid etc.
   lat_d = (lat[10]-lat[9])*np.pi/180.
   lon_d = (lon[10]-lon[9])*np.pi/180.
   R=6371000.
   lon2d,lat2d = np.meshgrid(lon,lat)


   # Shift function
   def shiftcyclic(in_var):
      var_s,     lons  = shiftgrid(180., in_var, lon, start=False,cyclic=360.0)
      var_sc,    lonscout = addcyclic(var_s,lons)
      return var_sc,lonscout


   # Zonal mean
   #########################################################
   if proj == 'zm':
      var_zm = zeros((varnb,len(lev),len(lat)), dtype=float32)
      if diff:
         var_zm2 = zeros((varnb,len(lev),len(lat)), dtype=float32)

      for i in range(0,varnb):
         var_zm[i,:,:] = np.average(var[i,:,:,:],2)
         if diff:
            var_zm2[i,:,:] = np.average(var2[i,:,:,:],2)

      budg_zm = sum(var_zm[1:varnb],0)
      if diff:
         budg_zm2 = sum(var_zm2[1:varnb],0)

      n_var.append(n_var[0]+'-BDGT'),    n_var_long.append(n_var[0]+'-Sum of all terms')
      budg_zm = np.expand_dims(budg_zm,0)
      var_zm = np.append(var_zm,budg_zm,axis=0)
      if diff:
         n_var.append(n_var[0]+'-BDGT'),    n_var_long.append(n_var[0]+'-Sum of all terms')
         budg_zm2 = np.expand_dims(budg_zm2,0)
         var_zm2 = np.append(var_zm2,budg_zm2,axis=0)
         
         var_zm = var_zm - var_zm2

      # MG tendency-budget in last position
      var_zm[varnb2-1] = var_zm[0]-var_zm[varnb2-1]



   # 2D map projection and global average (which is always calculated for sorting)
   ##############################################################

   var_v  = zeros((varnb,len(lat),len(lon)),   dtype=float32)
   var_vs = zeros((varnb,len(lat),len(lon)+1), dtype=float32)
   var_g  = zeros((varnb), dtype=float32)
   atm = zeros((len(lat),len(lon)))
   if diff:
      var_v2  = zeros((varnb,len(lat),len(lon)),   dtype=float32)
      var_vs2 = zeros((varnb,len(lat),len(lon)+1), dtype=float32)
      var_g2  = zeros((varnb), dtype=float32)
      atm2 = zeros((len(lat),len(lon)))


   # Vertical integration of cloud variables
   for i in range(0,varnb):
      for k in range(0, len(lev)):
         var_v[i,:,:] = var_v[i,:,:] + var[i,k,:,:]*dp[k,:,:]/9.81
         if diff:
            var_v2[i,:,:] = var_v2[i,:,:] + var2[i,k,:,:]*dp2[k,:,:]/9.81
      # global sum
      var_g[i] = np.sum(np.multiply(lat_d*lon_d*R*R*cos(np.pi*lat2d/180.),var_v[i,:,:]))
      var_vs[i,:,:],lonsc = shiftcyclic(var_v[i,:,:])
      if diff:
         var_g2[i] = np.sum(np.multiply(lat_d*lon_d*R*R*cos(np.pi*lat2d/180.),var_v2[i,:,:]))
         var_vs2[i,:,:],lonsc = shiftcyclic(var_v2[i,:,:])

   # Vertical integration of the atmosphere
   for k in range(0, len(lev)):
      atm = atm + dp[k,:,:]/9.81
      if diff:
         atm2 = atm2 + dp2[k,:,:]/9.81

   # Mass of the atmosphere
   atm_g = np.sum(np.multiply(lat_d*lon_d*R*R*cos(np.pi*lat2d/180.),atm))

   # MG budget (sum of all terms)
   budg_vs = sum(var_vs[1:varnb],0)

   # Global sum of budget
   budg_g  = np.sum(np.multiply(lat_d*lon_d*R*R*cos(np.pi*lat2d/180.),budg_vs[:,0:len(lon)]))


   if proj == 'map':
      n_var.append(n_var[0]+'-BDGT'),    n_var_long.append(n_var[0]+'-Sum of all terms')
   elif proj == 'global':
      n_var.append('BDGT'),    n_var_long.append('Sum of all terms')

   budg_vs = np.expand_dims(budg_vs,0)

   if diff:
      atm_g2 = np.sum(np.multiply(lat_d*lon_d*R*R*cos(np.pi*lat2d/180.),atm2))
      budg_vs2 = sum(var_vs2[1:varnb],0)
      budg_g2  = np.sum(np.multiply(lat_d*lon_d*R*R*cos(np.pi*lat2d/180.),budg_vs2[:,0:len(lon)]))
      budg_vs2 = np.expand_dims(budg_vs2,0)



   # Global sum of cloud variables divided by mass of atmosphere
   var_g = np.append(var_g,budg_g)/atm_g
   var_vs = np.append(var_vs,budg_vs,axis=0)
   if diff:
      var_g2 = np.append(var_g2,budg_g2)/atm_g2
      var_vs2 = np.append(var_vs2,budg_vs2,axis=0)

      var_vs = var_vs-var_vs2
      var_g  = var_g-var_g2


   # Sort variables according to global mean
   var_g_sorted = zeros(varnb2,dtype=int8)

   var_g_sorted[1:varnb] = np.argsort(-abs(var_g[1:varnb]))+1


   # Difference btw MG tend and bdg in last position
   if proj != 'global':

      var_vs[varnb2-1] = var_vs[0]-var_vs[varnb2-1]
      var_g[varnb2-1]  = var_g[0]-var_g[varnb2-1]



   ##############################################################################################
   # PLOT FUNCTIONS
   ##############################################################################################
   #
   # X Axis Information (Longitude)
   ##################################
   x_min                = -180
   x_max                = 180
   #delta_x              = 5
   majorLocator_x       = MultipleLocator(60)
   minorLocator_x       = MultipleLocator(10)
   minorLocator_x_south = MultipleLocator(1)
   xlab                 = 'longitude'

   #
   # Y Axis Information (Latitude)
   ##################################
   y_min          = -90
   y_max          = 90
   delta_y        = 2
   majorLocator_y = MultipleLocator(30)
   minorLocator_y = MultipleLocator(10)
   ylab           = 'latitude'

   #
   # Z Axis Information (Pressure/height)
   ##################################
   z_min          = max(lev)  # Reverse Y-axis
   z_max          = min(lev)
   #delta_x        = 2
   #majorLocator_x  = MultipleLocator(30)
   #minorLocator_x = MultipleLocator(10)
   zlab           = 'level'

   #
   # Defined DEGREE X AXIS
   ##################################
   def degree(x, pos):
   #    'The two args are the value and tick position'
       if x < 0:
           return "%1.f$^{\circ}$W" % abs(x)
       if x > 0:
           return "%1.f$^{\circ}$E" % (x)
       if x == 0:
           return "%1.f$^{\circ}$" % (x)
   formatter = FuncFormatter(degree)

   #
   # Defined DEGREE Y AXIS
   ##################################
   def degreey(y, pos):
   #    'The two args are the value and tick position'
       if y < 0:
           return "%1.f$^{\circ}$S" % abs(y)
       if y > 0:
           return "%1.f$^{\circ}$N" % (y)
       if y == 0:
           return "EQ" % (y)
   formatter2 = FuncFormatter(degreey)

   #
   # Define PLOTS: 
   #####################################################################################
   #####################################################################################
   def plot_bar():


       np_var = np.array(n_var)
       
       var_g_sorted_hist = zeros(varnb2,dtype=int8)
       var_g_sorted_hist[2:varnb2] = var_g_sorted[1:varnb]
       var_g_sorted_hist[1]=varnb2-1


       #var_g_s = var_g[:10].copy()
       #print var_g_s

       var_g_h = var_g.copy()
       var_g_h[:varnb2-1]=0
       var_g_h[0]=var_g[0]

       #print var_g_sorted_hist


       var_g_s = var_g[var_g_sorted_hist]
       var_g_hs = var_g_h[var_g_sorted_hist]



       # Poster: font side for axis label here.
       if budget == 'number':
          ax.set_ylabel('Number [1/kg/s]', fontsize=ax_fsz)
       elif budget == 'mass':
          ax.set_ylabel('Mass [kg/kg/s]', fontsize=ax_fsz)

       #xpos=arange(1,varnb2+1)
       xpos=arange(1,nbars+1)

       c = ax.bar(xpos,var_g_s[:nbars],color=bar_colour)
       c = ax.bar(xpos,var_g_hs[:nbars],color=bar_colour_bdgt)

       fig.subplots_adjust(hspace=None)
       xticks(arange(1,nbars+1)+0.4,np_var[var_g_sorted_hist], fontsize=tk_fsz, rotation =30)

       ax.hlines(0,0.4, nbars+1+0.4, colors='k', linestyles='solid' )

       if budget == 'mass':
          if state == 'cldice':
             if diff:
                p_min = -1*1e-10
             else:
                ax.set_ylim(qi_ymin,qi_ymax)
          else: #liq
             ax.set_ylim(qc_ymin,qi_ymax)
       else: # number
          if state == 'cldice':
             if diff:
                p_min = -1*1e-10
             else:
                ax.set_ylim(nc_ymin,nc_ymax)
          else: #liq
             ax.set_ylim(nc_ymin,nc_ymax) 
       ax.set_title('Global average '+plottitle)
   #####################################################################################
   #####################################################################################
   def plot_zm():

       tfs = '12' # title font size
       lfs = '12' # label font size
       cfs = '10'


       # linear scale 
       if budget == 'number':
          if state == 'cldice':
             if diff:
                p_min = -1*1e1
             else:
                p_min = -1*1e2
          else: #liquid
             if diff:
                p_min = -1*1e3
             else:
                p_min = -1*1e3
             #p_min = -5*1e1
       if budget == 'mass':
          if state == 'cldice':
             if diff:
                p_min = -1*1e-10
             else:
                p_min = -5*1e-9
          else: #liquid
             if diff:
                p_min = -1*1e-10
             else:
                p_min = -5*1e-9

       p_max = -p_min
       lvls1 = linspace(p_min,p_max,21)

       cmap = 'seismic'
       if logscale:
          cmap = 'RdBu_r'


       # Log scale
       if budget == 'number':
          if state == 'cldliq':
             max_log = 5
             min_log = -2
          else : # ice
             max_log = 5
             min_log = -2
       elif budget == 'mass':
          if state == 'cldliq':
             max_log = -8
             min_log = -15
          else : # ice
             max_log = -8
             min_log = -15

       thresh = 10**min_log
       num_log = (max_log-min_log+1)*1
       lvls_lp = logspace(min_log, max_log, num=num_log, endpoint=True, base=10.0)
       lvls_ln = -lvls_lp
       lvls_lpn = zeros((2*num_log+1))
       lvls_lpn[0:num_log]=lvls_ln[::-1]
       lvls_lpn[num_log]=0
       lvls_lpn[num_log+1:2*num_log+1]=lvls_lp


       i = 0
       for m in range(0,pan_row):
          for n in range(0,pan_col):

             if i == 0:
                j = var_g_sorted[i] # variable index
             elif i ==1:
                j = varnb2-1
             else:
                j = var_g_sorted[i-1] # variable index

             ax[m,n].set_ylabel(zlab, fontsize=lfs)

             #c1 = ax[m,n].contourf(lat,lev,var_zm[j],levels=lvls1)

             # Plotting
             if logscale:
                c1 = ax[m,n].contourf(lat,lev,var_zm[j,:,:],levels=lvls_lpn,norm=SymLogNorm(thresh),extend='both')
             else:
                c1 = ax[m,n].contourf(lat,lev,var_zm[j],levels=lvls1,extend='both')

             # Set title 
             if budget == 'number':
                unit_str = ' #kg$^{-1}$s$^{-1}$'
             elif budget == 'mass':
                unit_str = ' kgkg$^{-1}$s$^{-1}$'
             ax[m,n].set_title(n_var[j]+unit_str, fontsize=tfs)

             # Axis ticks
             ax[m,n].set_ylim(z_min,z_max)
             ax[m,n].xaxis.set_major_formatter(formatter2)
             ax[m,n].xaxis.set_major_locator(majorLocator_y)
             ax[m,n].xaxis.set_minor_locator(minorLocator_y)
             ax[m,n].set_xlim(y_min,y_max)

             # Colourbar
             if logscale:
                #l_f = LogFormatter(10, labelOnlyBase=False)
                l_f = LogFormatterMathtext(base=10, labelOnlyBase=True)
                cb1 = colorbar(c1,ax=ax[m,n],format=l_f)
                #cb1 = colorbar(c1,ax=ax[m,n])
             else: 
                cb1 = colorbar(c1,ax=ax[m,n])
             c1.set_cmap(cmap)

             i=i+1     


   ########################################################################################
   ########################################################################################
   def plot_map():

       if pan_row > pan_col:
          tfs = '11' # title font size
          lfs = '10' # label font size
          cfs = '8'
       else:
          tfs = '12' # title font size
          lfs = '12' # label font size
          cfs = '10'
     
       # linear scale 
       if budget == 'number':
          if state == 'cldice':
             if diff:
                p_min = -1*1e4
             else:
                p_min = -1*1e5
          else: #liquid
             if diff:
                p_min = -5*1e6
             else:
                p_min = -1*1e7
             #p_min = -5*1e1
       if budget == 'mass':
          if state == 'cldice':
             if diff:
                p_min = -1*1e-6
             else:
                p_min = -1*1e-5
          else: #liquid
             if diff:
                p_min = -1*1e-6
             else:
                p_min = -5*1e-5
             #p_min = -5*1e1
       p_max = -p_min
       lvls = linspace(p_min,p_max,21)
       lvls2 = linspace(-5*1e-10,5*1e-10,21)
       lvls3 = linspace(-5*1e-11,5*1e-11,21)
       lvls1 = linspace(-2*1e-8,2*1e-8,21)

       # Log scale
       if budget == 'number':
          if state == 'cldliq':
             max_log = 7
             min_log = 0
          else : # ice
             max_log = 7
             min_log = 0
       elif budget == 'mass':   
          if state == 'cldliq':
             max_log = -4
             min_log = -11
          else : # ice
             max_log = -4
             min_log = -11
    
       thresh = 10**min_log
       num_log = (max_log-min_log+1)*1
       lvls_lp = logspace(min_log, max_log, num=num_log, endpoint=True, base=10.0)
       lvls_ln = -lvls_lp
       lvls_lpn = zeros((2*num_log+1))
       lvls_lpn[0:num_log]=lvls_ln[::-1]
       lvls_lpn[num_log]=0
       lvls_lpn[num_log+1:2*num_log+1]=lvls_lp

    
       cmap = 'seismic'
       if logscale:
          cmap = 'RdBu_r'

       i=0
       for m in range(0,pan_row):
          for n in range(0,pan_col):

             if i == 0:
                j = var_g_sorted[i] # variable index
             elif i ==1:
                j = varnb2-1
             else:
                j = var_g_sorted[i-1] # variable index

    
             #   ax[m,n].set_xlabel(xlab, fontsize=lfs)
             #   ax[m,n].set_ylabel(ylab, fontsize=lfs)

             m1 = Basemap(projection='cyl',llcrnrlat=-90,urcrnrlat=90,\
                 llcrnrlon=-180,urcrnrlon=180,resolution='c',ax=ax[m,n])

             if logscale:
                c1 = ax[m,n].contourf(lonsc,lat,var_vs[j,:,:],levels=lvls_lpn,norm=SymLogNorm(thresh),extend='both')
             else:
                c1 = ax[m,n].contourf(lonsc,lat,var_vs[j,:,:],levels=lvls,extend='both')
        
             m1.drawcoastlines()

             # Set title
             if budget == 'number':
                unit_str = ' #m$^{-2}$s$^{-1}$ Average: '
                unit_str2 = ' #kg$^{-1}$s$^{-1}$'
             elif budget == 'mass':
                unit_str = ' kgm$^{-2}$s$^{-1}$ Average: '
                unit_str2 = ' kgkg$^{-1}$s$^{-1}$'
             ax[m,n].set_title(n_var[j] +unit_str+ str("%.2e" % var_g[j])+unit_str2, fontsize=tfs)

             ax[m,n].xaxis.set_major_formatter(formatter)
             ax[m,n].xaxis.set_major_locator(majorLocator_x)
             ax[m,n].xaxis.set_minor_locator(minorLocator_x)
             ax[m,n].yaxis.set_major_formatter(formatter2)
             ax[m,n].yaxis.set_major_locator(majorLocator_y)
             ax[m,n].yaxis.set_minor_locator(minorLocator_y)
             ax[m,n].set_xlim(x_min,x_max)
             ax[m,n].set_ylim(y_min,y_max)

             ax[m,n].tick_params(axis='both', which='major', labelsize=10)

             #cb1 = colorbar(c1,ax=ax[m,n],orientation='horizontal',label=n_var_long[j], format='%.4e')
             if logscale:
                #l_f = LogFormatter(base=10, labelOnlyBase=True)
                #l_f = LogFormatterExponent(base=10, labelOnlyBase=True)
                l_f = LogFormatterMathtext(base=10, labelOnlyBase=True)
             else:
                #cb1 = colorbar(c1,ax=ax[m,n],orientation='horizontal',label=n_var_long[j], format='%.1e')
             #   cb1.set_label(n_var_long[j],labelpad=-50)
                cb1.formatter.set_powerlimits((0, 0))
             c1.set_cmap(cmap)
      
             i=i+1     

       cbar_ax = fig.add_axes([0.1, 0.025, 0.8, 0.02])
       fig.colorbar(c1, cax=cbar_ax,orientation='horizontal', format=l_f,)      

   ##########################################################################################
   ##########################################################################################
   # Plotting

   # Create panels
   if proj == 'map' or proj == 'zm':
      fig, ax = subplots(pan_row,pan_col)
      if pan_row > pan_col:
         fig.set_size_inches(9.2,14, forward=True)
      else:
         fig.set_size_inches(23,10, forward=True)
   else:
      fig, ax = subplots(1)
      #fig, ax = subplots(1)
      #fig = plt.figure()
      #ax =fig.add_subplot(1,1,1)
      title=False

   # Poster: Set figure size here for histogram
   fig.set_size_inches(fg_sz_x,fg_sz_y, forward=True)

   # Plot   
   if proj == 'zm':
      plot_zm()
   elif proj == 'map':
      plot_map()
   elif proj == 'global':
      plot_bar()

   if proj=='global':
      fig.tight_layout(rect=[0,0.05,1,1])

   if proj == 'zm':
      subplots_adjust(top=0.92)

   if title:
      fig.suptitle(plottitle, fontsize=tt_fsz)
   
   #Only show figure if no outputfile is given
   if(len(args.outputFile)>0):
      fig.savefig(args.outputFile)
   else:
      show()


   sys.exit(0)


