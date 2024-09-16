# Global vulnerability of anurans to drought and warming

[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](https://choosealicense.com/)
![Open Source
Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)

This repository contains code and data needed to reproduce the article:

**Wu N. C., Bovo, R. P., Enriquez-Urzelai U., Clusella-Trullas S., Kearney, M. R., Navas C. A., & Kong J. D.** (Accepted) Global exposure risk of frogs to increasing environmental dryness. *Nature Climate Change*, **xx**, xxx-xxx, DOI: 

**When using the data or code from this project, please cite it as:**

**Wu N. C., Bovo, R. P., Enriquez-Urzelai U., Clusella-Trullas S., Kearney, M. R., Navas C. A., & Kong J. D.** (2024) Accepted version of paper data and code of manuscript: Global exposure risk of frogs to increasing environmental dryness. *Zenodo*. DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13743578.svg)](https://doi.org/10.5281/zenodo.13743578)

**Code**
- `behav_functions.R` - Modified ectotherm function to estimate hydration, body temperature, activity, burrow depth, tree climbing height, thermal tolerance, hydration tolerance, and activity hydration limit.
- `sim_behavior.R` - Example workflow testing the 'behav_functions' and 'micro_global_drought' functions

**Data**
- `anuran_species_list.csv` - Species-specific ecotype data from the [IUCN Red List](https://www.iucnredlist.org/).
- `raw_data.csv` - Raw hydrological data obtained from the literature used for the analysis.
- `obs_rainfall.csv` - Rainfall data from weather station near Karawatha, QLD.

**Analysis workflow**
- [`supplementary_information.html`](https://nicholaswunz.github.io/global-frog-drought/supplementary_information.html) - Supplementary information which contains the R workflow for processing and analysing the raw data, creating figures, and supplementary material for statistical outcomes, additional figures, and descriptions from the main document.

**External files**
- `ne_50m_land.shp` - World land polygon shape file (1:50m) from [Natural Earth](https://www.naturalearthdata.com/downloads/50m-physical-vectors/).
- `ANURA.shp` - Shape file of anuran native distribution downloaded on the 08/01/2020 from the International Union for Conservation of Nature Red List of Threatened Species ([IUCN Red List](https://www.iucnredlist.org/resources/spatial-data-download)).
- `TerraClimate19812010_ppt.nc` - A NetCDF file of yearly global precipitation (1981-2010) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- `TerraClimate19812010_pet.nc` - A NetCDF file of yearly global evapotranspiration (1981-2010) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- `TerraClimate19812010_vpd.nc` - A NetCDF file of yearly global vapour pressure deficiet (1981-2010) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- `TerraClimate19812010_ws.nc` - A NetCDF file of yearly global wind speed (1981-2010) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- `TerraClimate2C_ppt.nc` - A NetCDF file of yearly global precipitation (+2°C) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- `TerraClimate2C_pet.nc` - A NetCDF file of yearly global evapotranspiration (+2°C) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- `TerraClimate2C_ppt.nc` - A NetCDF file of yearly global precipitation (+4°C) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- `TerraClimate4C_pet.nc` - A NetCDF file of yearly global evapotranspiration (+4°C) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- `pdsisc.monthly.1900-2100.r2.5x2.5.EnsAvg25Models.TP2.ipe-2.ssp245.nc` - A NetCDF file of yearly self-calibrated Palmer drought severity index with Penman–Monteith potential evapotranspiration from 25 CMIP6 models under a SSP2-4.5 scenario in [Zhao and Dai (2022)](https://journals.ametsoc.org/view/journals/clim/35/3/JCLI-D-21-0442.1.xml).
- `pdsisc.monthly.1900-2100.r2.5x2.5.EnsAvg25Models.TP2.ipe-2.ssp585.nc` - A NetCDF file of yearly self-calibrated Palmer drought severity index with Penman–Monteith potential evapotranspiration from 25 CMIP6 models under a SSP5-8.5 scenario in [Zhao and Dai (2022)](https://journals.ametsoc.org/view/journals/clim/35/3/JCLI-D-21-0442.1.xml).

## Abstract
Species exposed to prolonged drying are at risk of population declines or extinctions. Understanding species' sensitivity to water loss and microhabitat preference, or ecotype, is therefore vital for assessing climate change risks. Here, we mapped global areas where water-sensitive vertebrates, i.e., anurans, will face increasing aridity and drought, analysed ecotype sensitivity to water loss, and modelled behavioural activity impacts under future drought and warming scenarios. Predictions indicate 6.6% to 33.6% of anuran habitats will become arid-like by 2080–2100, with 15.4% to 36.1% exposed to worsening drought, under an intermediate to high emission scenario, respectively. Critically, arid conditions are expected to double water loss rates. Biophysical models demonstrated a 11.45 ± 8.95% reduction in anuran activity under combined drought and warming, compared to the 6.74 ± 3.95% reduction from warming alone in the warmest quarter. These findings underscore the pervasive synergistic threat of warming and environmental drying to anurans.


**Keywords:** amphibian decline, climate change, dehydration, desiccation, hydroregulation, macrophysiology, thermoregulation


## License
This repository is provided by the authors under the MIT License ([MIT](http://opensource.org/licenses/MIT)).
