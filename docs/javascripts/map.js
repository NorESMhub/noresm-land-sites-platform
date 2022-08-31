
// Site GEOJSON - update manually!

var nlpSites = {
  "type": "FeatureCollection",
  "features": [
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          8.12343,
          61.0243
        ]
      },
      "properties": {
        "name": "ALP1",
        "description": "Ulvehaugen: Alpine vegetation at 1208 m elevation. Mean summer temperature is 7 degrees C, and mean annual precipitation is 1226 mm.",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_ALP1",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_ALP1.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/ALP1/surfdata_ALP1_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "SeedClim"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          7.27596,
          60.8231
        ]
      },
      "properties": {
        "name": "ALP2",
        "description": "Låvisdalen: Alpine vegetation at 1097 m elevation. Mean summer temperature is 7 degrees C, and mean annual precipitation is 1561 mm.",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_ALP2",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_ALP2.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/ALP2/surfdata_ALP2_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "SeedClim"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          7.17561,
          60.8328
        ]
      },
      "properties": {
        "name": "ALP3",
        "description": "Gudmedalen: Alpine vegetation at 1213 m elevation. Mean summer temperature is 6.5 degrees C, and mean annual precipitation is 2130 mm.",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_ALP3",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_ALP3.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/ALP3/surfdata_ALP3_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "SeedClim"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          6.41504,
          60.9335
        ]
      },
      "properties": {
        "name": "ALP4",
        "description": "Skjellingahaugen: Alpine vegetation at 1088 m elevation. Mean summer temperature is 7 degrees C, and mean annual precipitation is 3402 mm.",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_ALP4",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_ALP4.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/ALP4/surfdata_ALP4_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "SeedClim"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          8.70466,
          60.8203
        ]
      },
      "properties": {
        "name": "SUB1",
        "description": "Ålrust: Sub-alpine vegetation at 815 m elevation. Mean summer temperature is 9 degrees C and mean annual precipitation is 789 mm.",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_SUB1",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_SUB1.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/SUB1/surfdata_SUB1_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "SeedClim"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          7.17666,
          60.8760
        ]
      },
      "properties": {
        "name": "SUB2",
        "description": "Høgsete: Sub-alpine vegetation at 700 m elevation. Mean summer temperature is 9 degrees C and mean annual precipitation is 1356 mm.",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_SUB2",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_SUB2.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/SUB2/surfdata_SUB2_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "SeedClim"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          6.63028,
          61.0866
        ]
      },
      "properties": {
        "name": "SUB3",
        "description": "Rambæra: Sub-alpine vegetation at 769 m elevation. Mean summer temperature is 8.7 degrees C and mean annual precipitation is 1848 mm.",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_SUB3",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_SUB3.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/SUB3/surfdata_SUB3_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "SeedClim"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          6.51468,
          60.5445
        ]
      },
      "properties": {
        "name": "SUB4",
        "description": "Veskre: Sub-alpine vegetation at 797 m elevation. Mean summer temperature is 8.6 degrees C and mean annual precipitation is 3029 mm.",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_SUB4",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_SUB4.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/SUB4/surfdata_SUB4_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "SeedClim"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          9.07876,
          61.0355
        ]
      },
      "properties": {
        "name": "BOR1",
        "description": "Fauske: North-boreal vegetation at 589 m elevation. Mean summer temperature is 10.3 degrees C and mean annual precipitation is 600 mm.",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_BOR1",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_BOR1.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/BOR1/surfdata_BOR1_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "SeedClim"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          7.16982,
          60.8803
        ]
      },
      "properties": {
        "name": "BOR2",
        "description": "Vikesland: North-boreal vegetation at 474 m elevation. Mean summer temperature is 10.5 degrees C and mean annual precipitation is 1161 mm.",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_BOR2",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_BOR2.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/BOR2/surfdata_BOR2_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "SeedClim"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          6.33738,
          60.6652
        ]
      },
      "properties": {
        "name": "BOR3",
        "description": "Arhelleren: North-boreal vegetation at 431 m elevation. Mean summer temperature is 10.6 degrees C and mean annual precipitation is 2044 mm.",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_BOR3",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_BOR3.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/BOR3/surfdata_BOR3_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "SeedClim"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          5.96487,
          60.6901
        ]
      },
      "properties": {
        "name": "BOR4",
        "description": "Øvstedal: North-boreal vegetation at 346 m elevation. Mean summer temperature is 10.8 degrees C and mean annual precipitation is 2923 mm.",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_BOR4",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_BOR4.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/BOR4/surfdata_BOR4_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "SeedClim"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          25.29547425,
          69.3408715
        ]
      },
      "properties": {
        "name": "ISK",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_ISK",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_ISK.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/ISK/surfdata_ISK_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "LATICE-MIP"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          12.25481033,
          61.10516357
        ]
      },
      "properties": {
        "name": "HIS1",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_HIS1",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_HIS1.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/HIS1/surfdata_HIS1_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "LATICE-MIP"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          12.25089836,
          61.1115036
        ]
      },
      "properties": {
        "name": "HIS2",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_HIS2",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_HIS2.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/HIS2/surfdata_HIS2_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "LATICE-MIP"
      }
    },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          11.078142,
          60.372387
        ]
      },
      "properties": {
        "name": "HUR",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_HUR",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_HUR.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/HUR/surfdata_HUR_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "LATICE-MIP"
      }
      },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          7.527008533,
          60.59383774
        ]
      },
      "properties": {
        "name": "FNS",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_FNS",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_FNS.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/FNS/surfdata_FNS_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "LATICE-MIP"
      }
      },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          11.83334,
          78.92094
        ]
      },
      "properties": {
        "name": "BYV",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_BYV",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_BYV.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/BYV/surfdata_BYV_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "LATICE-MIP"
      }
      },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          15.91667,
          78.18333
        ]
      },
      "properties": {
        "name": "ADV",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_ADV",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_ADV.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/ADV/surfdata_ADV_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "LATICE-MIP"
      }
      },
    {
      "type": "Feature",
      "geometry": {
        "type": "Point",
        "coordinates": [
          10.781667,
          59.660278
        ]
      },
      "properties": {
        "name": "AAS",
        "compset": "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
        "res": "1x1_AAS",
        "url": "https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/v1.0.0/default/inputdata_version1.0.0_AAS.tar",
        "config": [
          {
            "name": "DIN_LOC_ROOT_CLMFORC",
            "value": "inputdata/atm/datm7/GSWP3v1"
          },
          {
            "name": "fsurdat",
            "value": "inputdata/lnd/clm2/surfdata_map/AAS/surfdata_AAS_simyr2000.nc"
          },
          {
            "name": "fates_paramfile",
            "value": "inputdata/lnd/clm2/paramdata/fates_params_api.22.1.0_12pft_c220307.nc"
          },
          {
            "name": "pft_index_count",
            "value": 12
          },
          {
            "name": "included_pft_indices",
            "value": [
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "11",
              "12"
            ]
          },
          {
            "name": "fates_c2b",
            "value": [
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2
            ]
          },
          {
            "name": "fates_grperc",
            "value": [
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11,
              0.11
            ]
          },
          {
            "name": "fates_leaf_long",
            "value": [
              1.5,
              4,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_leaf_stor_priority",
            "value": [
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8,
              0.8
            ]
          },
          {
            "name": "fates_leaf_slatop",
            "value": [
              0.012,
              0.01,
              0.024,
              0.012,
              0.03,
              0.03,
              0.012,
              0.03,
              0.03,
              0.03,
              0.03,
              0.03
            ]
          },
          {
            "name": "fates_leaf_vcmax25top",
            "value": [
              50,
              65,
              39,
              62,
              41,
              58,
              62,
              54,
              54,
              78,
              78,
              78
            ]
          },
          {
            "name": "fates_leaf_xl",
            "value": [
              0.1,
              0.01,
              0.01,
              0.1,
              0.01,
              0.25,
              0.01,
              0.25,
              0.25,
              -0.3,
              -0.3,
              -0.3
            ]
          },
          {
            "name": "fates_mort_bmort",
            "value": [
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014,
              0.014
            ]
          },
          {
            "name": "fates_mort_freezetol",
            "value": [
              2.5,
              -55,
              -80,
              -30,
              2.5,
              -30,
              -60,
              -10,
              -80,
              -80,
              -20,
              2.5
            ]
          },
          {
            "name": "fates_mort_hf_sm_threshold",
            "value": [
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06,
              1e-06
            ]
          },
          {
            "name": "fates_phen_evergreen",
            "value": [
              1,
              1,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              0,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_season_decid",
            "value": [
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1,
              0,
              0
            ]
          },
          {
            "name": "fates_phen_stress_decid",
            "value": [
              0,
              0,
              0,
              0,
              1,
              0,
              0,
              1,
              0,
              0,
              1,
              1
            ]
          },
          {
            "name": "fates_root_long",
            "value": [
              1,
              2,
              1,
              1.5,
              1,
              1,
              1.5,
              1,
              1,
              1,
              1,
              1
            ]
          },
          {
            "name": "fates_woody",
            "value": [
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              0,
              0,
              0
            ]
          }
        ],
        "group": "LATICE-MIP"
      }
    }
  ]
};

// Map object
var map = L.map('map').setView([64.616667, 16.65], 2);

// Base map
L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
    attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
}).addTo(map);

// Site marker display options
var geojsonMarkerOptions = {
    radius: 6,
    fillColor: "#ff7800",
    color: "#000",
    weight: 1,
    opacity: 1,
    fillOpacity: 0.9
};


// Functions for hovering and clicking sites

function highlightFeature(e) {
    var layer = e.target;

    layer.setStyle({
        weight: 3,
        color: '#000',
		fillColor: '#DCDCDC',
        dashArray: '',
        fillOpacity: 0.7
    });

    if (!L.Browser.ie && !L.Browser.opera && !L.Browser.edge) {
        layer.bringToFront();
    }
	info.update(layer.feature.properties);
}

var geojson;

function resetHighlight(e) {
    geojson.resetStyle(e.target);
	info.update();
}

//function zoomToFeature(e) {
//   map.setView(e.latlng, 7);
//}


function onEachFeature(feature, layer) {
	layer.on({
        mouseover: highlightFeature,
        mouseout: resetHighlight//,
        //click: zoomToFeature
    });
    // Display box with site group name on click
    if (feature.properties && feature.properties.name) {
        layer.bindPopup(
          '<b>' + feature.properties.name +
          '</b><br><span>' + feature.properties.description +'</span>'
          );
    }
}

// Apply to geojson

geojson = L.geoJSON(nlpSites, {
    pointToLayer: function (feature, latlng) {
        return L.circleMarker(latlng, geojsonMarkerOptions);
    },
	style: function(feature) {
        switch (feature.properties.group) {
            case 'SeedClim': return {fillColor: "#50c878"};
            case 'LATICE-MIP': return {fillColor: "#4169E1"};
        }
    },
	onEachFeature: onEachFeature
}).addTo(map);


// Add a legend

function getColor(d) {
        return d === 'SeedClim'  ? "#50c878" :
               d === 'LATICE-MIP'  ? "#4169E1" :
                            "#ff7f00"; //Other
    }

var legend = L.control({position: 'bottomleft'});

legend.onAdd = function(map) {
    var div = L.DomUtil.create('div', 'info legend');
    labels = ['<strong>Site family</strong>'],
    categories = ['SeedClim', 'LATICE-MIP'];

    for (var i = 0; i < categories.length; i++) {

            div.innerHTML +=
            labels.push(
                '<i class="fa fa-circle" style="color:' + getColor(categories[i]) + '"></i> ' +
            (categories[i] ? categories[i] : '+')
			);

        }
        div.innerHTML = labels.join('<br>');
    return div;
    };
legend.addTo(map);

// Add info box
var info = L.control();

info.onAdd = function (map) {
    this._div = L.DomUtil.create('div', 'info site-info'); // create a div with a class "info"
    this.update();
    return this._div;
};

// method that we will use to update the control based on feature properties passed
info.update = function (props) {
    this._div.innerHTML = '<h4>Site info</h4>' +  (props ?
        '<b>' + props.name + '</b><br />' + 'Click for more info.'
        : 'Hover over a site to display its name here.');
};

info.addTo(map);

// Reset zoom button
(function() {
	var control = new L.Control({position:'topleft'});
	control.onAdd = function(map) {
			var azoom = L.DomUtil.create('a','resetzoom leaflet-bar');
			azoom.innerHTML += '<i class="fa fa-refresh" aria-hidden="true"></i>';
			L.DomEvent
				.disableClickPropagation(azoom)
				.addListener(azoom, 'click', function() {
					map.setView([64.616667, 16.65], 4);
				},azoom);
			return azoom;
		};
	return control;
}())
.addTo(map);
