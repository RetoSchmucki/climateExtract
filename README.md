### climateExtract <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->
[![Build Status](https://travis-ci.com/RetoSchmucki/climateExtract.png?branch=master)](https://travis-ci.com/RetoSchmucki/climateExtract)
<!-- badges: end -->

### R functions to extract and manipulate ECAD climate data
* ECAD at [Copernicus Climate](https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles)
* Package URL: [https://retoschmucki.github.io/climateExtract/](https://retoschmucki.github.io/climateExtract)

> Before extracting any data, please read carefully the description of the datasets and the different grid size available (eg. 0.25 deg. regular grid, "TG" average temperature).
> Note shorter time-series are also available [Copernicus Climate](https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles)

#### News
* 29/11/2021 ("Climbing-Rose")
  - added Growing Degree Day functions
    1. compute gdd from average temperature method
    2. compute gdd using Baskerville-Emin method
    3. compute cumulative sum over RasterBrick layers

* 14/07/2021
  - added functionality for three new variables
    1. daily averaged sea level pressure PP
    2. daily averaged relative humidity HU 
    3. daily mean global radiation QQ

* 07/04/2021 ("Wild-Rose")
  - Updated to E-OBS v23.1 (March 2021) 
  - Optimized several functions
  - Added option to write data to a raster brick
  - Better used of memory
  - Extract data within a bounding box of polygons or points
  - Option to manually select the E-OBS version

#### Installation

GitHub installation (CRAN not available)

```
install.packages("devtools")
devtools::install_github("RetoSchmucki/climateExtract")
```

Dependancies: `ncdf4`, `data.table`, `sf`, `stars`, `sp`, `raster`, `terra`, `zoo`, `methods`

#### Usage

Check default ECAD version (can be changed with `ecad_v = xx.x`)

```R
library(climateExtract)
ecad_version
```

Define specific regions and/or points. If not provided, data will be extracted from the entire ECAD extent.

```R
# use set.seed() for reproducibility
set.seed(42876) 
fr_border = sf::st_as_sf(raster::getData("GADM", country = "FRA", level = 0))
sf_point = sf::st_sf(sf::st_sample(x = fr_border, size = 25, type = "random"))
```

Extract data
- Retrieve climate data from E-OBS server, or locally if available.
- Crop to the extent of the spatial object provided.
- Specify years and data of interest; see details help(extract_nc_value). 
- Write output to local disk with `write_raster=TRUE` 
- If `return_data = TRUE`, an R object (a list) with the climate values is created.  

```R
climate_data = extract_nc_value(first_year = 2012, 
                                last_year = 2015,
                                local_file = FALSE,
                                file_path = NULL,
                                sml_chunk = "2011-2020",
                                spatial_extent = fr_border,
                                clim_variable = "mean temp",
                                statistic = "mean",
                                grid_size = 0.25,
                                ecad_v = NULL,
                                write_raster = TRUE,
                                out = "raster_mean_temp.grd",
                                return_data = TRUE)
```

Connect to the raster brick created on local disk. If too big to be stored in Memory, raster will set pointer to the file.

```R
rbk = raster::brick("raster_mean_temp.grd")
format(object.size(climate_data), "MB")
format(object.size(rbk), "MB")
```

Aggregate the data over time, "annual", "monthly" or by using a rolling "window". The function `mean` could be replaced by `sum`, `sd` or other functions the can be computed along a vector. If "daily" is selected as time_step, no aggregation is conducted as ECAD data are provided on daily scale. 

```R
# monthly mean
monthly_avg_temp_R = temporal_aggregate(x = rbk,
                                        agg_function = "mean",
                                        variable_name = "average temp",
                                        time_step = "monthly")
# annual mean
annual_avg_temp_R = temporal_aggregate(x = rbk,
                                       agg_function = "mean",
                                       variable_name = "average temp",
                                       time_step = "annual")
# 7-day rolling mean
window_7d_avg_temp_R = temporal_aggregate(x = rbk,
                                          agg_function = "mean",
                                          variable_name = "average temp",
                                          time_step = "window",
                                          win_length = 7)
```

>Note: Argument in `x` could also be the climate_data object that resulted from the `extract_nc_value()` function.

Aggregation for points, if a spatial point object is provided in `y` argument (e.g. `y = sf_point`).

```R
# annual mean per point
annual_avg_temp_pnts = temporal_aggregate(x = rbk,
                                          y = sf_point,
                                          agg_function = "mean",
                                          variable_name = "average temp",
                                          time_step = "annual")
```
>Note: Here, `site_1` is located along the coast and falls outside the raster grid cell with a value, the value returned correspond to the nearest cell with a value. Distance_from_pnt indicates distance to cell centroid, when within a cell with value, this is NA.

Map of the aggregated layer. Here the average temperature for Sept 2012, `"2012.09"`.

```R
raster::plot(monthly_avg_temp_R[["2012.09"]])
```

Add points to the map, here with a circle around the point 1 to show the point near the coast of Corse (bottom right corner).

```R
raster::plot(monthly_avg_temp_R[["2012.09"]])
plot(sf_point, col = 'magenta', pch = 17, add = TRUE)
plot(sf_point[1,], col = 'blue', border= 1.5, cex= 2, pch = 1, add = TRUE)
```
#### Meta

* See citation and ECA&D/E-OBS data policy [https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php\#datafiles](https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php\#datafiles)
* Register as an E-OBS user at <https://surfobs.climate.copernicus.eu/dataaccess/registration.php>
* Package URL: [https://retoschmucki.github.io/climateExtract/](https://retoschmucki.github.io/climateExtract)
* Raise issues: [https://github.com/RetoSchmucki/climateExtract/issues](https://github.com/RetoSchmucki/climateExtract/issues)
* Get citation information for `climateExtract` in R doing `citation(package = 'climateExtract')`

* Suggested citation:
  * Schmucki R. (2021) climateExtract: Extract and manipulate daily gridded observational dataset of European climate (E-OBS) provided by ECA&D. R package version 1.23.0. https://github.com/RetoSchmucki/climateExtract
 
