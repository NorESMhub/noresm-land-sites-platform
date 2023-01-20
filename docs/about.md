[![NorESM](img/NORESM-logo.png "the Norwegian Earth System Model")](https://www.noresm.org/)
[![EMERALD](img/Emerald_darktext_whiteBG_small.png "EMERALD project")](https://www.mn.uio.no/geo/english/research/projects/emerald/)
[![LATICE](img/UiO_LATICE_logo_black_small.png "Land-ATmosphere Interactions in Cold Environments research group")](https://www.mn.uio.no/geo/english/research/groups/latice/)

# Platform for NorESM site-level simulation over land

doi: ***under construction***

The NorESM-LSP (LSP for short) aims to facilitate site-level simulations over land using [NorESM](https://github.com/NorESMhub/NorESM) and its land component [CTSM](https://github.com/NorESMhub/CTSM) with the developing vegetation demographic module [FATES](https://github.com/NGEET/fates). 

The platform aims to lower the technical barriers to vegetation demographic modelling with NorESM-CLM-FATES. In doing so, we aim to help bridge the gaps between observations and modelling, promote the usage of site-level observations to test, validate and improve the models and its land component, and to promote the usage of NorESM-CLM-FATES to new audiences and research settings.

The observation sites currently included in the platform are described in the [Sites](https://noresmhub.github.io/noresm-land-sites-platform/land-sites/) page. 

## Development team
* [Kaveh Karimi](https://github.com/ka7eh)
* [Lasse Torben Keetz](https://github.com/lasseke)
* [Eva Lieungh](https://github.com/evalieungh)
* [Hui Tang](https://github.com/huitang-earth)
* [Emiliano Gelati](https://github.com/emiliano-gelati)
* [Stefanie Falk](https://github.com/ziu1986)
* [Sunniva Indrehus](https://github.com/sunnivin)
* [Anne Fouilloux](https://github.com/annefou)
* [Yeliz Yilmaz](https://github.com/yelizy)
* [Elin Aas](https://github.com/ecaas)
* [Peter Horvath](https://github.com/peterhor)
* [Kjetil Aas](https://github.com/kjetilaas)
* [Devaraju Narayanappa](https://github.com/devarajun)
* [Sonya Geange](https://github.com/srg101)
* [Inge Althuizen](https://github.com/ingealthuizen)
* Anders Bryn
* Hanna Lee
* Frans-Jan Parmentier
* Norbert Pirk
* Vigdis Vandvik
* Ane Vollsnes
* Olav Skarpaas
* Frode Stordal
* Lena M. Tallaksen

## Supporting projects
* [EMERALD](https://www.mn.uio.no/geo/english/research/projects/emerald/)
* [LATICE](https://www.mn.uio.no/geo/english/research/groups/latice/) and LATICE-X
* [SeedClim](https://www.uib.no/en/rg/EECRG/55395/seedclim)
* [LandPress](https://www.uib.no/en/rg/EECRG/95156/landpress)
* [THREE-D](https://www.uib.no/en/rg/EECRG/126712/three-d)
* [INES](https://www.ines.noresm.org/)
* [EOSC-Nordic](https://www.eosc-nordic.eu/)

## Acknowledgements
The NorESM-LSP has evolved over several years from the needs and visions of a large group of researchers and students connected through the EMERALD and LATICE projects. Funding has come from the projects listed above. Thanks are due to everyone who participated along the way in discussions, feedback, and testing of early versions.
The NorESM-LSP software wraps around the NorESM-CLM-FATES model framework. The Norwegian Earth System Model is developed by a [consortium](https://www.noresm.org/consortium/), and is closely related to the Community Earth System Model (CESM). The CESM and Community Land Model (CLM) are supported primarily by the U.S. National Science Foundation. The FATES model is supported primarily by the U.S. Department of Energy’s Next Generation Ecosystem Experiment - Tropics (NGEE-T) project.

## How to cite
If you publish something based on simulations through the LSP, please cite the software in your publication. For now, cite the complete NorESM Land Sites Platform as: **NorESM-LSP development team. 2022. The NorESM Land Sites Platform (Version v1) [Computer software]**. The webpage with technical documentation can be cited as: *The NorESM-LSP development team. 2022, [month day]. NorESM Land Sites Platform technical documentation. https://noresmhub.github.io/noresm-land-sites-platform/*. [Each individual repository](https://noresmhub.github.io/noresm-land-sites-platform/documentation/#links-to-noresm-lsp-github-repositories) has Zenodo records with DOIs and citation instructions, in case you need to refer to them separately. 
Look for our upcoming technical description paper, which will provide a better overall citation for the whole LSP.

NB! The NorESM-LSP software wraps around the model framework. When you publish model experiments, you also need to properly cite and acknowledge the models: See the licences of [NorESM](https://github.com/NorESMhub/NorESM/blob/master/LICENSE.txt), [CLM](https://github.com/ESCOMP/CTSM/blob/master/LICENSE), [FATES](https://github.com/NGEET/fates/blob/master/LICENSE.txt). In the Acknowledgements section, it is common practice to add funding information of the models that you use (e.g. the U.S. National Science Foundation for CLM, NGEE-Tropics for FATES). The main publications for each model should also be cited in the main text.

NB! If you use observational data, e.g. from the Vestland Climate Grid sites or another integrated site, make sure to look at the licences, rights and conditions attached to those data as well! Following international standards for research ethics, authorship invitations should be given based on 'substantial contributions to conception and design, acquisition of data, or analysis and interpretation of data', among other things. 

See for instance the [ICMJE authorship guidelines (Vancouver convention)](https://www.icmje.org/recommendations/browse/roles-and-responsibilities/defining-the-role-of-authors-and-contributors.html) to help you decide which kind of acknowledgement is appropriate. Similarly, if an LSP developer helped you substantially, please consider adding that person to the Acknowledgements section of your publication or invite them to co-author your publication if the contribution is substantial enough. 

## Contact

For questions about the platform, please [open an issue](https://github.com/NorESMhub/noresm-land-sites-platform/issues/) on GitHub or get in touch with one of the developers directly.
