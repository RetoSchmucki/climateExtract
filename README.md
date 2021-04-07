# climateExtract <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->
[![Build Status](https://travis-ci.com/RetoSchmucki/climateExtract.png?branch=master)](https://travis-ci.com/RetoSchmucki/climateExtract)
<!-- badges: end -->

R functions to extract climate data from local NETCDF file that you can download from the
ECAD at [Copernicus Climate](https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles)

Package URL: [https://retoschmucki.github.io/climateExtract/](https://retoschmucki.github.io/climateExtract)

**NEWS (07/04/2021):** 
- Updated to E-OBS v23.1 (March 2021) 
- Optimized several functions
- Added option to write data to a raster brick
- Better used of memory
- Extract data within a bounding box of polygons or points
- Option to manually select the E-OBS version

**NEWS (06/02/2021):**
- Updated to E-OBS v22 (December 2020)
- Added option for accessing smaller chunk (15 years) with the argument `sml_chunk` in the function `extract_nc_value()`
- Changing argument names to underscore format to avoid possible confusion with generic method functions


#### Suggested citation for the climateExtract package

Schmucki R. (2021) climateExtract: Extract and manipulate daily gridded observational dataset of European climate (E-OBS) provided by ECA&D. R package version 1.23.0. https://github.com/RetoSchmucki/climateExtract


#### Installation
You will need the package `devtools` (or `remotes`) and then use the function install_github()
```
install.packages("devtools")
devtools::install_github("RetoSchmucki/climateExtract")
```

climateExtract depends on 
`ncdf4`
`data.table`
`sf`
`stars`
`raster`
`terra`
`zoo`
`methods`


#### Usage

Load the library and check the version of the ECAD database

```R
library(climateExtract) 
ecad_version
```

To limit the size of the object, we can limit the extract to a specific region of the data set. Here we will extract climate data for France and for 25 points (random) within the country. We use the package raster to retrieve a spatial object of France's country borders and sf to generate 25 random point within the country. Because points are generated through a random number process, we set.seed for repeatability.

```R
set.seed(42876)
fr_border = sf::st_as_sf(raster::getData("GADM", country = "FRA", level = 0))
sf_point = sf::st_sf(sf::st_sample(x = fr_border, size = 25, type = "random"))
```

Use the function `extract_nc_value` to retrieve the climate data from the online E-OBS server and crop the data within the extent of the spatial object provided. We specify the first and last year of interest, use `sml_chunk` to reduce the size of the file to download, see details help(extract_nc_value). The argument `write_raster=TRUE` will write a raster brick object in your directory of the region defined by the spatial extent. The argument `return_data = TRUE` returns an R object (a list) with the climate values stored in a numeric array.  

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

From here, we can use the raster package to connect to the raster brick created. If this object is too big for being stored in Memory, raster will set pointer to the raster file on the disk.

```R
rbk = raster::brick("raster_mean_temp.grd")

format(object.size(climate_data), "MB")
format(object.size(rbk), "MB")
```

We can now aggregate the data over time, "annual", "monthly" or using a rolling "window". Here we show a monthly average, using the function `mean`, this could be set to `sum`, `sd` or many others. 

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

>Note: the `x` object can be the climate_data object that resulted from the `extract_nc_value()` function.

Aggregation can also be computed for specific points, if a spatial point object is provided as `y` argument (e.g. `y = sf_point`). This will return a data.table object.

```R
# annual mean per point
annual_avg_temp_pnts = temporal_aggregate(x = rbk,
                                          y = sf_point,
                                          agg_function = "mean",
                                          variable_name = "average temp",
                                          time_step = "annual")
```
>Note: Because the point that is referring to `site_1` falls outside of the raster cell having a value, along the coast, the function extract the value of the nearest cell with a value. The distance_from_pnt indicate how far the centroid of this cell is from the point, when the point falls in a cell, the distance_from_pnt is NA.

We can produce a map of the averaged layer calculated. For example the average temperature for Sept 2012, `"2012.09"`.

```R
raster::plot(monthly_avg_temp_R[["2012.09"]])
```
You can plot the points on this same map, with a circle around the point 1, falling in the see near the coast of Corse (bottom right corner). This is only true if the seed was set to `set.seed(42876)` for the random sampling. 

```R
raster::plot(monthly_avg_temp_R[["2012.09"]])
plot(sf_point, col = 'magenta', pch = 17, add = TRUE)
plot(sf_point[1,], col = 'blue', border= 1.5, cex= 2, pch = 1, add = TRUE)
```

### Citation and term of use from ECAD Data

See citation and ECA&D/E-OBS data policy at [https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php\#datafiles](https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php\#datafiles)
You can register as an E-OBS user at [https://surfobs.climate.copernicus.eu/dataaccess/registration.php](https://surfobs.climate.copernicus.eu/dataaccess/registration.php)

**Before extracting any data, please read carefully the description of the datasets and the different grid size available (eg. 0.25 deg. regular grid, "TG" average temperature).** **Note** that shorter time-series are also available [Copernicus Climate](https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles)

** Data available at 0.1 deg regular grid are ca. 5GB and can be challenging to handle when extracting the entire continent, consider using specific areas defined by spatial object (e.g. set of points or polygons).**

**You can download the high-resolution grid and subset the area of interest to build a manageable dataset as demonstrated above.**

*This is a work in progress that is good for some tasks, but this comes with no guarantee. Suggestions and contributions for improvement are welcome.*

**Raise issues :raised_hand:: ** [https://github.com/RetoSchmucki/climateExtract/issues](https://github.com/RetoSchmucki/climateExtract/issues)
#### TO DO
- [ ] update and improve documentation
