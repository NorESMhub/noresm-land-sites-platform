# NorESM land sites

We currently support 18 sites: 12 [Vestland climate grid](https://betweenthefjords.w.uib.no/vestland-climate-grid/) sites and 6 climate station sites of interest to the [LATICE](https://www.mn.uio.no/geo/english/research/groups/latice/) group. These are locations where there are climate stations and ecological field experiments that make these sites of particular interest. 

<link rel="stylesheet" href="https://unpkg.com/leaflet@1.7.1/dist/leaflet.css"
   integrity="sha512-xodZBNTC5n17Xt2atTPuE1HxjVMSvLVW9ocqUKLsCC5CXdbqCmblAshOMAS6/keqq/sMZMZ19scR4PsZChSR7A=="
   crossorigin=""/>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<div id="map"></div>
<script src="https://unpkg.com/leaflet@1.7.1/dist/leaflet.js"
   integrity="sha512-XQoYMqMTK8LvdxXYG3nZ448hOEQiglfqkJs1NOQV44cWnUrBc8PkAOcXy20w0vlaXaVUearIOBhiXZ5V3ynxwA=="
   crossorigin=""></script>
   
   

To plot site locations over a background map in R, you can also look at [this repo](https://github.com/evalieungh/map_scripts)
 
-------------------------------


## [Vestland climate grid](https://betweenthefjords.w.uib.no/vestland-climate-grid/) (SeedClim) sites


| Site name | Name             | Lon (X) | Lat (Y) | Elevation (Z) | Data years | Data types | Description      |
| --------- | ---------------  | ------- | ------- | ------------- | ---------- | ---------- | ---------------- |   
| ALP1      | Ulvehaugen       | 8.12343 | 61.0243 | 1208          | 2008-2022  | Vegetation surveys, vegetation demography, experimental treatments, weather data: [to be added] | Above current climatic forest line. Semi-natural grassland, heather, ridges and snowbeds |
| ALP2      | Laavisdalen      | 7.27596 | 60.8231 | 1097          |   |  |
| ALP3      | Gudmedalen       | 7.17561 | 60.8328 | 1213          |   |  |
| ALP4      | Skjellingahaugen | 6.41504 | 60.9335 | 1088          |   |  |
| SUB1      | Aalrust          | 8.70466 | 60.8203 | 815           |   |  |Just below current climatic forest line. |
| SUB2      | Hoegsete         | 7.17666 | 60.8760 | 700           |   |  |
| SUB3      | Rambaera         | 6.63028 | 61.0866 | 769           |   |  |
| SUB4      | Veskre           | 6.51468 | 60.5445 | 797           |   |  |
| BOR1      | Fauske           | 9.07876 | 61.0355 | 589           |   |  | Well below current climatic forest line. |
| BOR2      | Vikesland        | 7.16982 | 60.8803 | 474           |   |  |
| BOR3      | Arhelleren       | 6.33738 | 60.6652 | 431           |   |  |
| BOR4      | Oevstedal        | 5.96487 | 60.6901 | 346           |   |  |

The Vestland climate grid, often called the SeedClim grid after the first major project that set it up, is a set of calcareous grassland sites across gradients of precipitation and temperature. The sites have been, and still are, host to many ecological reseach projects that study vegetation dynamics, plant interactions, functional traits and climate responses. Each site also has loggers for temperature and other climatic variables: ...

![Vestland climate grid](https://betweenthefjords.w.uib.no/files/2020/08/grid.png)

*Illustration from the SeedClim project: In the first project, turf squares (top soil, roots and whole plants) were cut out and physically transplanted to other sites, as indicated by the arrows in the top left corner. The vegetation in these turfs was carefully monitored over several years in their new locations, to see how and how quickly the plants adapted to a new environment.*

**Selected papers using the sites:**

- coming soon


![Field work at Ulvehaugen = ALP1 site](https://tinyimg.io/i/q5EA7X5.jpg)

*******************************************************

## LATICE-MIP sites 🚧 NB! Which sites to include in published release is uncertain (2022 June 30)


| Site name | Station name | Lon (X)     |	Lat (Y)     |	Elevation (Z) | Data years | Data types | Description      |
| --------- |------------------ | ----------  | ---------   | -------------- | ---- | -------- | ------------ |
| FIN       | finseflux         |	7.527008533 | 60.59383774 | 1210  | Nov 2016 (?) - present | Long wave in (LWin), Long wave out (LWout), Short wave in (SWin), Short wave out (SWout), turbulent heat fluxes, precipitation, temperature, humidity, pressure, wind speed and direction, snow depths (surveys and single point), H2O, CO2, soil temperature, soil vol. water, soil conductivity, drone images | Above current climatic tree- and forest lines. Alpine ridges, wetlands, lakes, heather and snowbed vegetation.
|  | hisaasen_upper    |	12.25481033 | 61.10516357 | 680   |
|  | hisaasen_lower    |	12.25089836 | 61.1115036  | 640   |
|  | iskoras           |	25.29547425 | 69.3408715  | 360   |
|  | aas               |	10.781667   | 59.660278   | 93.2  |
|  | hurdal            | 11.078142   | 60.372387   | NA    |
