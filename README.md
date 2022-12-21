# Global vulnerability of anurans to drought and warming

[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](https://choosealicense.com/)
![Open Source
Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)

This repository contains code and data needed to reproduce the article:

**Wu N. C., Enriquez-Urzelai U., Navas C. A., & Kong J. D.** (In Prep) Global vulnerability of anurans to drought and warming. *Journal name*, **xx**, xxx-xxx, DOI: 

**Data**
- anuran_species_list.csv - Species-specific data from the [IUCN Red List](https://www.iucnredlist.org/).
- raw_data.csv  - Raw hydrological data obtained from the literature used for the analysis.

**Analysis workflow**
- supplementary_information.html - Supplementary information which contains the R workflow for processing and analysing the raw data, creating figures, and supplementary material for statistical outcomes, additional figures, and descriptions from the main document.

**External files**
- ne_50m_land.shp - World land polygon shape file (1:50m) from [Natural Earth](https://www.naturalearthdata.com/downloads/50m-physical-vectors/).
- ANURA.shp - Shape file of anuran native distribution downloaded on the 08/01/2020 from the International Union for Conservation of Nature Red List of Threatened Species ([IUCN Red List](https://www.iucnredlist.org/resources/spatial-data-download)).
- TerraClimate19812010_ppt.nc - A NetCDF file of yearly global precipitation (1981-2010) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- TerraClimate19812010_pet.nc - A NetCDF file of yearly global evapotranspiration (1981-2010) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- TerraClimate4C_ppt.nc - A NetCDF file of yearly global precipitation (+4°C) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- TerraClimate4C_pet.nc - A NetCDF file of yearly global evapotranspiration (+4°C) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- pdsi-2100-ssp5-85.nc - A NetCDF file of yearly self-calibrated Palmer drought severity index with Penman–Monteith potential evapotranspiration from 25 CMIP6 models in [Zhao and Dai (2022)](https://journals.ametsoc.org/view/journals/clim/35/3/JCLI-D-21-0442.1.xml).
  

## Abstract
Maintaining water balance is a critical attribute influencing the resilience of organisms to climate extremes such as drought events. The risk of drought may also be mediated by ecological types, or ecotype that differ in their microhabitat preference and dependence on water, and thus their physiological niche. Using anurans, a data-rich taxonomic group, we identified regions of exposure risk to increase aridity and drought, and examined which ecotypes were most sensitive to desiccation using biophysical simulations to quantify changes in activity in relation to climate change. Under a business-as-usual scenario, we show that ~24% of areas occupied by anurans will increase to arid-like conditions, and 38% of areas will be at risk of drought...

**Keywords:** amphibian decline, climate change, desiccation, hydroregulation, thermoregulation, macrophysiology

## License
This repository is provided by the authors under the MIT License ([MIT](http://opensource.org/licenses/MIT)).
