# example
library(climateExtract)
source("R/growingDegreeDay.r")

set.seed(42876) 
fr_border = sf::st_as_sf(raster::getData("GADM", country = "FRA", level = 0))
sf_point = sf::st_sf(sf::st_sample(x = fr_border, size = 25, type = "random"))

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

rbk_mean = raster::brick("raster_mean_temp.grd")

climate_data_min = extract_nc_value(first_year = 2012, 
                                last_year = 2015,
                                local_file = FALSE,
                                file_path = NULL,
                                sml_chunk = "2011-2020",
                                spatial_extent = fr_border,
                                clim_variable = "min temp",
                                statistic = "mean",
                                grid_size = 0.25,
                                ecad_v = NULL,
                                write_raster = TRUE,
                                out = "raster_min_temp.grd",
                                return_data = TRUE)

rbk_min = raster::brick("raster_min_temp.grd")

climate_data_max = extract_nc_value(first_year = 2012, 
                                last_year = 2015,
                                local_file = FALSE,
                                file_path = NULL,
                                sml_chunk = "2011-2020",
                                spatial_extent = fr_border,
                                clim_variable = "max temp",
                                statistic = "mean",
                                grid_size = 0.25,
                                ecad_v = NULL,
                                write_raster = TRUE,
                                out = "raster_max_temp.grd",
                                return_data = TRUE)

rbk_max = raster::brick("raster_max_temp.grd")

## GDD
be_gdd_france <- gdd_extract(base_temp = 7, min_temp = rbk_min, max_temp = rbk_max, gdd_method = 'be')
raster::plot(be_gdd_france[[1]])

month_cumsum_gdd_france <- cumsum_rb(be_gdd_france, indices = get_layer_indice(be_gdd_france, indice_level = "month"))
year_cumsum_gdd_france <- cumsum_rb(be_gdd_france, indices = get_layer_indice(be_gdd_france, indice_level = "year"))

raster::plot(year_cumsum_gdd_france[[150]])

terra::extract(terra::rast(year_cumsum_gdd_france[[150]]), terra::vect(as(sf_point, "Spatial")))
# raster::extract(year_cumsum_gdd_france[[150]], as(sf_point, "Spatial"))
fr_gdd <- terra::extract(terra::rast(year_cumsum_gdd_france[[150]]), terra::vect(as(fr_border, "Spatial")))
fr_gdd[,2]

last_day_index <- as.numeric(table(as.numeric(factor(lubridate::floor_date(as.Date(gsub("X", "", names(month_cumsum_gdd_france)), "%Y.%m.%d"), "month")))))

month_cumsum_gdd_france[[cumsum(last_day_index)]]

fr_gdd_lastDayMonth <- terra::extract(terra::rast(month_cumsum_gdd_france[[cumsum(last_day_index)]]), terra::vect(as(fr_border, "Spatial")))


plot(sf_point, add = TRUE)
plot(fr_border, add = TRUE)
raster::plot(year_cumsum_gdd_france[[367]])

sd(terra::values(year_cumsum_gdd_france[[150]]), na.rm=TRUE)