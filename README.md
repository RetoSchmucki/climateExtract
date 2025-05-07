### climateExtract

<!-- badges: start -->

[![R-CMD-check](https://github.com/RetoSchmucki/climateExtract/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RetoSchmucki/climateExtract/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

`<img style="float: right;" src="man/figures/logo.png" hspace="20" width="120" />`

### R functions to extract and manipulate ECAD climate data

* ECAD at [Copernicus Climate](https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles)
* Package URL: [https://retoschmucki.github.io/climateExtract/](https://retoschmucki.github.io/climateExtract)

> Before extracting any data, please read carefully the description of the datasets and the different grid sizes available (eg. 0.25 deg. regular grid, "TG" average temperature).
> Note shorter time-series are also available [Copernicus Climate](https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles)

#### News

07/05/2025 ("Pacific-Rose")

* Updated to E-OBS v31.0 (March 2025)
  1. extending data from January 1950 to December 2024

14/04/2024 ("Coastal-Rose")

- Updated to E-OBS v30.0 (September 2024)
  1. extending data from January 1950 to June 2024
  2. fixed spatial misalignment in output raster

24/04/2024 ("Roman-Rose")

- Updated to E-OBS v29.0 (March 2024)
  1. extending data from January 1950 to December 2023
  2. fixed bug small negative values in ggd "be" method

07/11/2023 ("Mountain-Rose")

- Updated to E-OBS v28.0 (Oct 2023)
  1. extending data from January 1950 to June 2023
  2. fully update to the `terra` package
  3. included the `geodata` package
  4. removed former dependency on the `zoo` package

#### Installation

GitHub installation (CRAN not available)

```
install.packages("devtools")
devtools::install_github("RetoSchmucki/climateExtract")
```

Dependencies: `ncdf4`, `data.table`, `sf`, `terra`,`lubridate`, and `methods`

#### Usage

Check the default ECAD version (can be changed with `ecad_v = xx.x`)

```R
library(climateExtract)
ecad_version
```

Define specific regions and points. If not provided,  the function will extract the data for the entire extent of ECAD. Note that we need to install the package `geodata` to retrieve the border of France.

```R
# use set.seed() for reproducibility
set.seed(42876) 
fr_border = sf::st_as_sf(geodata::gadm("GADM", country = "FRA", level = 0))
sf_point = sf::st_sf(sf::st_sample(x = fr_border, size = 25, type = "random"))
```

Extract data

- Retrieve climate data from E-OBS server or a local file if available.
- Crop to the extent of the spatial object provided.
- Specify years and data of interest; see details help(extract_nc_value).
- Write output to local disk with `write_raster=TRUE`
- If `return_data = TRUE`, an R object (a list) with the climate values is created.

```R
climate_data = extract_nc_value(first_year = 2012, 
                                last_year = 2015,
                                local_file = FALSE,
                                file_path = NULL,
                                sml_chunk = "2011-2024",
                                spatial_extent = fr_border,
                                clim_variable = "mean temp",
                                statistic = "mean",
                                grid_size = 0.25,
                                ecad_v = NULL,
                                write_raster = TRUE,
                                out = "raster_mean_temp.tiff",
                                return_data = TRUE)
```

Connect to the raster brick created on the local disk. If it is too big to be stored in Memory, the terra package will set a pointer to the file.

```R
rbk = terra::rast("raster_mean_temp.tiff")
format(object.size(climate_data), "MB")
format(object.size(rbk), "MB")
```

Aggregate the data over time; `annual`, `monthly`, or by using a rolling `window`. The function `mean` could be replaced by `sum`, `sd`, or other functions that can be computed along a vector. If you select "daily" for the time_step, the function will return the daily ECAD data without aggregation.

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
                                          agg_function = "sum",
                                          variable_name = "average temp",
                                          time_step = "window",                                          ,
                                          win_length = 7)
```

> Note: Argument in `x` could also be the climate_data object that resulted from the `extract_nc_value()` function.

## Extract location-specific value

You can provide a set of spatial locations (e.g., points) in the `y` argument to extract the values for these locations. By providing geographic locations in `y`, the function return the results, the temporal aggregates, specific to these points.

```R
# annual mean per point
annual_avg_temp_pnts = temporal_aggregate(x = rbk,
                                          y = sf_point,
                                          agg_function = "mean",
                                          variable_name = "average temp",
                                          time_step = "annual")
```

> Note: If a site is located along the coast and falls outside the terrestrial part of the raster grid cell with a value, the function return the value of the nearest cell with a value. This will also return the Distance_from_pnt to inform the distance between the location and the centroid of the cell from which the value was collected. For points located on grid cell with value, the Distance_from_pnt is NA.

Map of the aggregated layer. For a map of the average temperature for Sept 2012, use layer `"2012-09"`.

```R
terra::plot(monthly_avg_temp_R[["2012-09"]])
```

Add points to the map.

```R
terra::plot(monthly_avg_temp_R[["2012-09"]])
plot(sf_point, col = 'magenta', pch = 17, add = TRUE)
```

#### Growing Degree Day (GDD)

ClimateExtract enables users to calculate growing degree day values with specific base and maximum temperatures. You can calculate the GDD from the daily average temperature or use the Baskerville-Emin method (method = 'be'), which fits a sine function to the minimum and maximum daily temperature to account for daily fluctuations. With these methods, you can calculate daily GDD values and compute the total or the cumulative sum (accumulated) GDD over a specific period (e.g., year, month, week, or a moving window).

```R
# get minimum temperature 

climate_data_min = extract_nc_value(first_year = 2012, 
                                    last_year = 2015,
                                    local_file = FALSE,
                                    file_path = NULL,
                                    sml_chunk = "2011-2024",
                                    spatial_extent = fr_border,
                                    clim_variable = "min temp",
                                    statistic = "mean",
                                    grid_size = 0.25,
                                    ecad_v = NULL,
                                    write_raster = TRUE,
                                    out = "raster_min_temp.tiff",
                                    return_data = TRUE)

climate_data_max = extract_nc_value(first_year = 2012,
                                    last_year = 2015,
                                    local_file = FALSE,
                                    file_path = NULL,
                                    sml_chunk = "2011-2024",
                                    spatial_extent = fr_border,
                                    clim_variable = "max temp",
                                    statistic = "mean",
                                    grid_size = 0.25,
                                    ecad_v = NULL,
                                    write_raster = TRUE,
                                    out = "raster_max_temp.tiff",
                                    return_data = TRUE)

rbk_min = terra::rast("raster_min_temp.tiff")
rbk_max = terra::rast("raster_max_temp.tiff")
```

With the `gdd_extract()` function, we can calculate the GDD with a specific base temperature (7 degrees C) and a preferred method. Here, we used the Baskerville-Emin method (method = 'be'), which requires the minimum and maximum daily temperatures. You can also use the average daily temperature by using the method to `avg` instead of `be`. Note that for the average method, you can specify the mean_temp or provide the min_temp and max_temp from which the function calculates the mean temperature. When using the `be` method, you must provide the minimum and maximum temperature to enable the fit of a daily sine function.

```R
be_gdd_france <- gdd_extract(base_temp = 7,
                            min_temp = rbk_min,
                            max_temp = rbk_max,
                            gdd_method = 'be')

# visualise the GDD-base7, for June 16th 2012
terra::plot(be_gdd_france[["2012-06-16"]])
```

The output is a multilayer raster that you can use with the function `temporal_aggregate()` to calculate the sum, mean, or rolling window mean. In general, however, we will be most interested in the cumulative sum (i.e., the accumulation of GDD over a specific period). You can use the function `cumsum_rb()` (cumulative sum on multilayer raster). This function uses the GDD Raster and a vector that indexes the layers to inform the specific period (e.g., monthly, yearly, weekly, or X-day windows). The indices are returned with the function `get_layer_indice()` (see below). You can also provide a vector of index that will be used to aggregate the layers.

```R
tp_index <- get_layer_indice(x = be_gdd_france,
                             date_format = "%Y-%m-%d",
                             indice_level = "year")

year_cumsum_gdd_france <- cumsum_rb(be_gdd_france, indices = tp_index)

# visualise the GDD base7 accumulated between January 1st until June 16th 2012.
terra::plot(year_cumsum_gdd_france[["2012-06-16"]])
```

To get the cumulative GDD at the end of each month, you can calculate the cumulative total monthly and extract the last day of each month. The following script generates a RasterBrick of the total GDD accumulated at the end of each month.

```R
tp_index <- get_layer_indice(x = be_gdd_france,
                             date_format = "%Y-%m-%d",
                             indice_level = "month")

month_cumsum_gdd_france <- cumsum_rb(be_gdd_france, indices = tp_index)

# get the indices for the last day of each month - calculate the number of day in each month along the time-series
last_day_index <- as.numeric(table(as.numeric(factor(lubridate::floor_date(as.Date(names(month_cumsum_gdd_france), "%Y-%m-%d"), "month"))))) 

month_cumsum_gdd_france[[cumsum(last_day_index)]]
# plot the accumulated gdd on January 31 and Feburary 29, 2012
plot(month_cumsum_gdd_france[[cumsum(last_day_index)]][[1:2]])
```

#### Meta

* See citation and ECA&D/E-OBS data policy [https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php\#datafiles](https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php\#datafiles)
* Register as an E-OBS user at [https://surfobs.climate.copernicus.eu/dataaccess/registration.php](https://surfobs.climate.copernicus.eu/dataaccess/registration.php)
* Package URL: [https://retoschmucki.github.io/climateExtract/](https://retoschmucki.github.io/climateExtract)
* Raise issues: [https://github.com/RetoSchmucki/climateExtract/issues](https://github.com/RetoSchmucki/climateExtract/issues)
* Get citation information for `climateExtract` in R doing `citation(package = 'climateExtract')`
* Suggested citation:

  * Schmucki R. (2024) climateExtract: Extract and manipulate daily gridded observational dataset of European climate (E-OBS) provided by ECA&D. R package version 1.29. https://github.com/RetoSchmucki/climateExtract
