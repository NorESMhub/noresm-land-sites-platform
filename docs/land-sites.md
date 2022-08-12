# NorESM land sites

We currently support 20 sites: 8 climate station sites of interest to the [LATICE](https://www.mn.uio.no/geo/english/research/groups/latice/) group and the 12 [Vestland climate grid](https://betweenthefjords.w.uib.no/vestland-climate-grid/) sites. These locations are established study sites with climatic, and/or ecological data available. It is possible to add custom-made sites for somewhat experienced users.

<link rel="stylesheet" href="https://unpkg.com/leaflet@1.7.1/dist/leaflet.css"
   integrity="sha512-xodZBNTC5n17Xt2atTPuE1HxjVMSvLVW9ocqUKLsCC5CXdbqCmblAshOMAS6/keqq/sMZMZ19scR4PsZChSR7A=="
   crossorigin=""/>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<div id="map"></div>
<script src="https://unpkg.com/leaflet@1.7.1/dist/leaflet.js"
   integrity="sha512-XQoYMqMTK8LvdxXYG3nZ448hOEQiglfqkJs1NOQV44cWnUrBc8PkAOcXy20w0vlaXaVUearIOBhiXZ5V3ynxwA=="
   crossorigin=""></script>
   
   
To plot site locations over a background map in R, you can also look at [this repo](https://github.com/evalieungh/map_scripts)
 
-----------------------------------------------------------------------

## LATICE-MIP sites

LATICE-MIP is a coordinated effort to utilise the field measurements at different sites for land surface modelling activities in Norway. Currently, eight sites provide flux tower and surface data representing the latitudinal gradient (between 60° and 78° N) in Norway including Svalbard. The most prominent site is the Finse Eco-Hydrological Observatory (EcHO; http://mn.uio.no/latice/infrastructure/ [accessed 2022-06-21]) (Pirk et al. soon submitted), located at 1200 m above sea level. Lakes and rivers, alpine heathland, open fens, and snowbeds dominate the land surface around the observatory (Bryn and Horvath 2020). Finse has a long research history in alpine biology, glaciology, and geology, and high quality and -resolution data for solving and tracking surface energy balance, CO2, and H2O fluxes between the land and the atmosphere.

| Site name | Station name | Lon (X)     |   Lat (Y)     |  Elevation (Z) | Data years | Data types | Description      |
| --------- |------------------ | ----------  | ---------   | -------------- | ---- | -------- | ------------ |
| FNS       | Finse_fluxtower | 7.527008533 | 60.59383774 | 1210  | Feb 2018 - present | Long wave in (LWin), Long wave out (LWout), Short wave in (SWin), Short wave out (SWout), turbulent heat fluxes, 10 m air temperature, air pressure, 4.4 m wind speed and direction, snow depths (surveys and single point), H2O, CO2, soil temperature, soil vol. water, soil conductivity, drone images | Above current climatic tree- and forest lines. Alpine ridges, wetlands, lakes, heather and snowbed vegetation. mn.uio.no/latice/infrastructure 
| HIS1 | Hisaasen_up |  12.25481033 | 61.10516357 | 681 | June 2019 - present | Long wave in (LWin), Long wave out (LWout), Short wave in (SWin), Short wave out (SWout), turbulent heat fluxes, 2 m air temperature, air pressure, 2.8 m wind speed and direction, rain
| HIS2 | hisaasen_low | 12.25089836 | 61.1115036  | 642 | " | "
| ISK | Iskoras_EC | 25.29547425 | 69.3408715  | 357 | March 2019 | "
| AAS  | Aas | 10.781667 | 59.660278   | 93.2  | NA | Precipitation, temperature, wind, air pressure, snow depth, EC measurements | meteorological data is from MET Norway
| HUR | Hurdal | 11.078142 | 60.372387   | NA    |
| BYV  | Bayelva | 11.83334 | 78.92094 | 56  | 1998–2017 | permafrost, active layer and meteorological conditions | https://doi.org/10.5194/essd-10-355-2018
| ADV | Adventdalen | 15.91667 | 78.18333 | 21 |

-----------------------------------------------------------------------


## [Vestland climate grid](https://betweenthefjords.w.uib.no/vestland-climate-grid/) (SeedClim) sites

The Vestland climate grid, often called the SeedClim grid after the first major project that set it up, is a set of calcareous grassland sites across gradients of precipitation and temperature. The sites have been, and still are, host to many ecological reseach projects that study vegetation dynamics, plant interactions, functional traits and climate responses. Each site also has loggers for temperature and other climatic variables.

Central data papers:

- [FunCaB](https://zenodo.org/record/6520022)
- Coming soon: [INCLINE]()
- Coming soon: [SeedClim]()

| Site name | Name             | Lon (X) | Lat (Y) | Elevation (Z) | Data years | Data types | Description      |
| --------- | ---------------  | ------- | ------- | ------------- | ---------- | ---------- | ---------------- |   
| ALP1      | Ulvehaugen       | 8.12343 | 61.0243 | 1208          | 2008-2022  | Vegetation surveys, vegetation demography, experimental treatments, weather data: [to be added] | Above current climatic forest line. Semi-natural grassland, heather, ridges and snowbeds |
| ALP2      | Laavisdalen      | 7.27596 | 60.8231 | 1097          |  " | " |
| ALP3      | Gudmedalen       | 7.17561 | 60.8328 | 1213          |  " | " |
| ALP4      | Skjellingahaugen | 6.41504 | 60.9335 | 1088          |  " | " |
| SUB1      | Aalrust          | 8.70466 | 60.8203 | 815           |   |  | Just below current climatic forest line. |
| SUB2      | Hoegsete         | 7.17666 | 60.8760 | 700           |  " | " |
| SUB3      | Rambaera         | 6.63028 | 61.0866 | 769           |  " | " |
| SUB4      | Veskre           | 6.51468 | 60.5445 | 797           |  " | " |
| BOR1      | Fauske           | 9.07876 | 61.0355 | 589           |   |  | Well below current climatic forest line. |
| BOR2      | Vikesland        | 7.16982 | 60.8803 | 474           | "  | " |
| BOR3      | Arhelleren       | 6.33738 | 60.6652 | 431           | "  | " |
| BOR4      | Oevstedal        | 5.96487 | 60.6901 | 346           | "  | " |


![Vestland climate grid](https://betweenthefjords.w.uib.no/files/2020/08/grid.png)

*Figure 1: Illustration from the first project using the Vestland Climate Grid (SeedClim). Turf squares (top soil, roots and whole plants) were cut out and physically transplanted to other sites, as indicated by the arrows in the top left corner. The vegetation in these turfs was carefully monitored over several years in their new locations, to see how and how quickly the plants adapted to a new environment.*


![Field work at Ulvehaugen = ALP1 site](https://tinyimg.io/i/q5EA7X5.jpg)

*Figure 2: Field work at Ulvehaugen (ALP1). Ragnhild Gya and Joachim Töpper doing vegetation analysis in a 25x35cm metal frame. The small, white ([Tomst](https://tomst.com/web/en/systems/tms/tms-4/)) loggers track temperature and soil moisture, and the small weather station (wooden pole to the right) tracks additional weather data. The fence keeps sheep and other animals away from the plots, but the vegetation is semi-natural after decades or centuries of grazing. The clear, hexagonal Open-Top Chambers experimentally heat up the vegetation.*

**Selected papers using the sites:**

- Töpper JP., Meineri E, Olsen SL, Rydgren K, Skarpaas O., & Vandvik V. 2018. The devil is in the detail: non-additive and context-dependent plant population responses to increasing temperature and precipitation. Global Change Biology 24: 4657–4666. doi:10.1111/gcb.14336
- Althuizen IHJ, Lee H, Sarneel J, & Vandvik V. 2018. Long-term climate regime modulates the impact of short-term climate variability on decomposition in alpine grassland soils. Ecosystems xx: xx-xx. https://doi.org/10.1007/s10021-018-0241-5
- Delnevo N, Petraglia A, Carbognani M, Vandvik V, & Halbritter AH. 2018. Plastic and genetic responses to change in snowmelt time in reproductive phenology and growth of Ranunculus acris. Perspectives in Plant Ecology, Evolution and Systematics 30: 62-70
- Klanderud K, Meineri E, Töpper, J, Michel P, Vandvik V. 2017. Biotic interaction effects on seedling recruitment along bioclimatic gradients: testing the stress gradient hypothesis. Journal of Vegetation Science 28: 347-356
- Münzbergová Z, Hadincová V, Skálová H, & Vandvik V. 2017. Genetic differentiation and plasticity interact along temperature and precipitation gradients to determine plant performance under climate change. Journal of Ecology 105: 1358–1373. DOI : 10.1111/1365-2745.12762
- Graae BJ, Vandvik V, Armbruster WS, Eiserhardt WL, Svenning JC, Hylander K, Ehrlén J, Speed JM, Klanderud K, Bråthen KA, Milbau A, Opedal ØH, Alsos IG, Ejrnæs R, Bruun HH, Birks HJB, Westergaard KB, Birks HH, & Lenoir J. 2018. Stay or go – how topographic complexity influences alpine plant population and community responses to climate change. Perspectives in Plant Ecology, Evolution and Systematics 30: 41-50. DOI: 10.1016/j.ppees.2017.09.008 
- Vandvik V, Elven R, & Töpper J. 2017. Seedling recruitment in subalpine grassland forbs: Predicting field regeneration behaviour from lab germination responses. Botany 95: 73–88.  dx.doi.org/10.1139/cjb-2016-0022.
- Guittar J, Goldberg DE, Klanderud K, Telford RJ, & Vandvik V. 2016. Can trait patterns along gradients predict plant community responses to climate change? Ecology 97: 2791-2801.
- Olsen S. L., J. P. Töpper, O. Skarpaas, V. Vandvik, & K. Klanderud. 2016. From facilitation to competition: temperature-driven shift in dominant plant interactions affects population dynamics in semi-natural grasslands. Global Change Biology 22: 1915-1926. doi: 10.1111/gcb.13241
- Skarpaas O., Meineri E.P., Bargmann T., Pötsch C., Spindelböck J.P., & Vandvik V. 2016. Biomass partitioning in grassland plants along independent gradients in temperature and precipitation. Perspectives in Plant Ecology, Evolution and Systematics 19:1-11.
- Vandvik V., Klanderud K., Meineri E.P., Måren I.E., & Spindelböck J.P. 2016. Seed banks are biodiversity reservoirs: Species-area relationships above vs. below ground. Oikos 125: 218-228. doi: 10.1111/oik.02022
- Klanderud K., Vandvik V., & Goldberg D.E. 2015. The importance of biotic vs. abiotic drivers of local plant community composition along regional bioclimatic gradients. PLoS One 10(6): e0130205. doi:10.1371/journal.pone.0130205
- Tingstad L., Olsen S.L., Klanderud K., Vandvik V., & Ohlson M.O. 2015. Temperature, precipitation and biotic interactions as determinants of tree seedling recruitment across the tree line ecotone. Oecologia 179: 599-608. doi:10.1007/s00442-015-3360-0
- Wasof S., Lenoir J., Aarrestad P.A., Alsos I.G., Armbruster W. S., Austrheim G., Bakkestuen V., Birks H.J.B., Bråthen K. A., Broennimann O., Brunet J., Bruun H. H., Dahlberg C. J., Diekmann M., Dullinger S., Dynesius M., Ejrnæs R., Gégout J-C., Graae B. J., Grytnes J-A., Guisan A., Hylander K., Jónsdóttir I. S., Kapfer J., Klanderud K., Luoto M., Milbau A., Moora M., Nygaard B., Odland A., Pauli H., Ravolainen V., Reinhardt S., Sandvik S. M., Schei F. H., Speed J. D. M., Svenning J-C., Thuiller W., Tveraabak L. U., Vandvik V., Velle L. G., Virtanen R., Vittoz P., Willner W., Wohlgemuth T., Zimmermann N. E., Zobel M., & Decocq G. 2015. Disjoint populations of European vascular plant species keep the same climatic niches. Global Change Biology 24: 1401-1412. 10.1111/geb.12375
- Meineri E., Skarpaas O., Spindelböck J.S., Bargmann T., and Vandvik V. 2014. Direct and size-dependent climate effect on flowering performance in alpine and lowland herbaceous species. Journal of Vegetation Science 25: 275-278. DOI: 10.1111/jvs.12082
- Lenoir J., Graae B.J., Aarrestad, P.A., Alsos I.G., Armbruster S., Austrheim G., Bergendorff C., Birks H.J.B., Bråthen K.A., Brunet J., Bruun H.H., Dahlberg C., Decocq G., Diekmann M., Dynesius M., Ejrnæs R., Grytnes J.-A., Hylander K., Klanderud K., Luoto M., Milbau A., Moora M., Nygaard B., Odland A., Ravolainen V., Reinhardt S., Sandvik S., Schei F.H., Speed J., Tveraabak L., Vandvik V., Velle L.G., Virtanen R., Zobel M., Svenning J.-C. 2013. Strong local spatial buffering of climate-change impacts on species across Northern Europe. Global Change Biology 19: 1470-1481. DOI: 10.1111/gcb.12129. Highlighted by the editors in GCB.
- Meineri E., Spindelböck J.S., & Vandvik V. 2013. Seedling emergence responds to the climate of both recruitment site and seed source - A climate change experiment combining transplant and gradient approaches. Plant Ecology 214:607-619. DOI: 10.1007/s11258-013-0193-y
- Spindelböck J.P., Cook Z., Daws M.I., Heegaard E., Måren I.E. & Vandvik V. 2013. Conditional cold avoidance drives variation in germination behaviour in Calluna vulgaris. Annals of Botany 112 (5): 801-810.
- Meineri E., Skarpaas O., & Vandvik V. 2012. Modeling alpine plant distributions at the landscape scale: Do biotic interactions matter? Ecological Modeling 231:1-10.
