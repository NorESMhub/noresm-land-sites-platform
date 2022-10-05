# The NorESM land sites platform - technical description paper results

Workflow and data for the example CLM-FATES simulation described in Lieungh and Keetz et al. (2022). We used the NorESM-LSP for default model experiments with the BOR1 ("Fauske") site coordinates. We then relate the outputs to available observational data from the Vestland Climate Grid (citation below) in simplified ways for demonstration purposes. See the publications and documentation links therein for details.

**Data citation**:

Telford, R. J., Vandvik, V., Halbritter, A. H., Skarpaas, O., Olsen, S. L., Lynn, J. S., … Zernichow, C. (2022, September 7). Vestland Climate Grid (VCG). https://doi.org/10.17605/OSF.IO/NPFA9.

Vandvik, V., Telford, R. J., Halbritter, A. H., Jaroszynska, F., Lynn, J. S., Geange, S. R., … Rüthers, J. (2022, June 26). FunCaB - The role of functional group interactions in mediating climate change impacts on the Carbon dynamics and biodiversity of alpine ecosystems. https://doi.org/10.17605/OSF.IO/4C5V2.

## Folder structure

| Folder                | File(s) or subfolder              | Description  |
|:---------------------:|:---------------------------------:|:------------:|
| `./`                  | `gswp3_vs_vcg_temperature.ipynb`  | Plotting GSWP3 default forcing temperatures against available VCG temperature logger observations. |
|                       | `model_output_and_observed.ipynb` | Create the following plots: 1: Aboveground biomass (AGB) over full simulation period; 2: NorESM-LSP AGB against FunCaB observations from vegetation removals; 3: Modelled ecosystem carbon fluxes (GPP, RH, RA, NEP). |
| `./model_data/`       | `input/`                          | Provided NorESM-LSP GSWP3 default climate forcing for BOR1. |
|                       | `history_output_nc/`              | Concatenated CLM-FATES history output NetCDF files for the two 1000-year BOR1 simulations performed with the NorESM-LSP: 1. all default PFTs enabled; 2: only default grass PFTs enabled. |
| `./observation_data/` | _empty_                           | Target directory for automated downloading of VCG observational tabular data. |
| `./output_plots/`     | _PNG image files_                 | Output folder for the plots created by the notebooks for the publication. |
