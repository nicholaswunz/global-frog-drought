# Global vulnerability of anurans to drought and warming

[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](https://choosealicense.com/)
![Open Source
Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)

This repository contains code and data needed to reproduce the article:

**Wu N. C., Bovo, R. P., Enriquez-Urzelai U., Clusella-Trullas S., Kearney, M. R., Navas C. A., & Kong J. D.** (In Prep) Widespread exposure risk of anurans to rising global dryness. *Journal name*, **xx**, xxx-xxx, DOI: 

**Data**
- anuran_species_list.csv - Species-specific ecotype data from the [IUCN Red List](https://www.iucnredlist.org/).
- raw_data.csv  - Raw hydrological data obtained from the literature used for the analysis.

**Analysis workflow**
- supplementary_information.html - Supplementary information which contains the R workflow for processing and analysing the raw data, creating figures, and supplementary material for statistical outcomes, additional figures, and descriptions from the main document.

**External files**
- ne_50m_land.shp - World land polygon shape file (1:50m) from [Natural Earth](https://www.naturalearthdata.com/downloads/50m-physical-vectors/).
- ANURA.shp - Shape file of anuran native distribution downloaded on the 08/01/2020 from the International Union for Conservation of Nature Red List of Threatened Species ([IUCN Red List](https://www.iucnredlist.org/resources/spatial-data-download)).
- TerraClimate19812010_ppt.nc - A NetCDF file of yearly global precipitation (1981-2010) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- TerraClimate19812010_pet.nc - A NetCDF file of yearly global evapotranspiration (1981-2010) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- TerraClimate19812010_vpd.nc - A NetCDF file of yearly global vapour pressure deficiet (1981-2010) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- TerraClimate19812010_ws.nc - A NetCDF file of yearly global wind speed (1981-2010) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- TerraClimate2C_ppt.nc - A NetCDF file of yearly global precipitation (+2°C) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- TerraClimate2C_pet.nc - A NetCDF file of yearly global evapotranspiration (+2°C) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- TerraClimate2C_ppt.nc - A NetCDF file of yearly global precipitation (+4°C) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- TerraClimate4C_pet.nc - A NetCDF file of yearly global evapotranspiration (+4°C) from [TerraClimate](https://www.climatologylab.org/terraclimate.html).
- pdsisc.monthly.1900-2100.r2.5x2.5.EnsAvg25Models.TP2.ipe-2.ssp245.nc - A NetCDF file of yearly self-calibrated Palmer drought severity index with Penman–Monteith potential evapotranspiration from 25 CMIP6 models under a SSP2-4.5 scenario in [Zhao and Dai (2022)](https://journals.ametsoc.org/view/journals/clim/35/3/JCLI-D-21-0442.1.xml).
- pdsisc.monthly.1900-2100.r2.5x2.5.EnsAvg25Models.TP2.ipe-2.ssp585.nc - A NetCDF file of yearly self-calibrated Palmer drought severity index with Penman–Monteith potential evapotranspiration from 25 CMIP6 models under a SSP5-8.5 scenario in [Zhao and Dai (2022)](https://journals.ametsoc.org/view/journals/clim/35/3/JCLI-D-21-0442.1.xml).

## Abstract
Species exposed to prolonged drying are at risk to population declines or extinctions. A key missing element for assessments of climate risk is the sensitivity of species to water loss and their microhabitat preference, or ecotype, as both dictate the risk of environmental drying. Here, we identified globally where water-sensitive ectotherms, i.e. anurans, are at risk to increasing aridity and drought, examined which ecotypes are more sensitive to water loss from 238 species, and showed how behavioural activity is impacted by future drought and warming scenarios through biophysical models. Under an intermediate and high emission scenario, 6.6 and 33.5% of areas occupied by anurans will increase to arid-like conditions, and 15.4 and 36.1% are at risk of exposure to a combination of increasing drought intensity, frequency, and duration by 2080¬–2100, respectively. Critically, ecotypes did not differ in the rate of water loss, and biophysical models showed that for a ground-dwelling frog during the warmest quarter, the combination of drought and warming reduced potential activity by 17.9% relative to the current conditions compared to warming alone which reduced potential activity by 8%. Our results exemplify the widespread exposure risk of environmental drying for anurans, posing a serious challenge for the lives of water-sensitive species beyond the effects of temperature alone.


**Keywords:** amphibian decline, climate change, dehydration, hydroregulation, thermoregulation, macrophysiology

## License
This repository is provided by the authors under the MIT License ([MIT](http://opensource.org/licenses/MIT)).
