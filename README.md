# climateExtract

R functions to extract climate data from local NETCDF file that you can download from the
ECAD at [http://www.ecad.eu/download/ensembles/download.php#datafiles](http://www.ecad.eu/download/ensembles/download.php#datafiles)

**NEWS:** Updated for - ECAD Version 18.0 -

#### Installation
You will need the package `devtools` and then use the function install_github()
```
install.packages("devtools")
devtools::install_github("RetoSchmucki/climateExtract")
```

This package depends on the `ncdf4` package. For *Linux* or *MacOS* users, the `ncdf4` can be installed directly from CRAN. *Windows* users should refer to the instructions available at http://cirrus.ucsd.edu/~pierce/ncdf/ and install the `ncdf4` package manually from the appropriate `.zip` file.

**Windows users** also need to install a tool to unzip the file from your command prompt. So to make it easy and cross-platform, I rely on Rtools that is available for download from [here] (https://cran.r-project.org/bin/windows/Rtools/index.html). The Rtools installer should install it in "C:\Rtools\bin". This need to be added to your PATH environment variable (if you forgot how to do this, follow the [instruction here](http://www.computerhope.com/issues/ch000549.htm)). Once you installed and set the PATH in your environment variable, relaunch your R instance and test it with this function system("gzip -h"). This should print the help documentation for the gzip function. Now with Rtools on board, you are ready to  go and extract some climate data! Well, almost... you might encounter some issues related to R's memory limit under Windows. This is partly my fault as I did not pay much attention to this while coding under UNIX systems (Linux or Mac). But slowly, I am working on this issue (among others) by revisiting and restructuring the source code. Anyway, there is a workaround the memory issue under Windows and this is by extracting smaller chunk of data at the time (see point no.5 below).


**Before extracting any data, please read carefully the description of the datasets and the different grid size available (eg. 0.25 deg. regular grid, "TG" average temperature).**
**Note** that shorter time-series are also available [http://www.ecad.eu/download/ensembles/downloadchunks.php](http://www.ecad.eu/download/ensembles/downloadchunks.php)


#### Example

You can get your climate data from the web repository http://www.ecad.eu/download/ensembles/download.php#datafiles and decompress the data to extract the `.nc` file.

Or you can use the function `extract_nc_value()` to download the data directly by setting the parameter local_file to FALSE and adding the details of the data you want to be extracted.

**1.** To extract climate values for a specific time period, use the function `extract_nc_value()`. By default this function will open an interactive window asking you to select a local `.nc` file from which you want the data to extract from, in this case you just have to specify the firs and the last years of the time period you are interested.
```R
library(climateExtract)
climate_data <- extract_nc_value(2012,2015)
```
**2.** If you don't have a local .nc file, you can ask the function to download the desired data directly from the web repository.

```R
climate_data <- extract_nc_value(2012, 2015, local_file = FALSE, clim_variable = 'precipitation', grid_size = 0.25)
```

*where clim_variable set to:*
* "mean temp" extract the daily mean temperature
* "mim temp" extract the daily minimum temperature
* "max temp extract the daily maximum temperature
* "precipitation" extract the daily precipitation

*where grid_size set to:*
* 0.25 extract a grid with a 0.25-degree resolution
* 0.10 extract a grid with a 0.10-degree resolution

**3.** To compute summary value of the daily values, use the function `temporal_mean()` for temperature or `temporal_sum()` for precipitation . This function computes the mean for a specified time period, monthly or annual or for specified window computing a rolling average over a specific number of days. **NOTE** This function use the data extracted with the function `extract_nc_value`.

```
annual_mean <- temporal_mean(climate_data,"annual")
monthly_sum <- temporal_sum(climate_data,"monthly")
```
**4.** To extract the weather data for a set of specific locations (points), use the function `point_grid_extract()`. With this function, you can extracts either the original or the summary values corresponding to the points, depending on the data object provided in the first argument. The second argument is a `data.frame` with the coordinates of the points in a degree decimal format and using the epsg projection 4326 - **wgs 84**

```
point_coord <- data.frame(site_id=c("site1","site2","site3","site4","site5"), longitude=c(28.620000,6.401499,4.359062,-3.579906,-2.590392), latitude=c(61.29000,52.73953,52.06530,50.43031,52.02951))

point.ann_mean <- point_grid_extract(annual_mean,point_coord)
point.month_sum <- point_grid_extract(monthly_sum,point_coord)
```

**5.** To extract long series one small chunk at the time (A quick and dirty workaround memory limit under Windows).
```
# This is a workaround when facing memory issues under Windows while extracting a long series on a computer
# with limited RAM.

library(climateExtract)

climate_data <- extract_nc_value(1950, 1960, local_file=FALSE, clim_variable='mean temp', grid_size=0.25)
point_coord <- data.frame(site_id = c("site1"), longitude = c(-1.3177988), latitude = c(51.7503954))

annual_mean <- temporal_mean(climate_data, "annual")
point.ann_mean <- point_grid_extract(annual_mean, point_coord)

climate_data <- extract_nc_value(1961, 1970)
annual_mean <- temporal_mean(climate_data, "annual")
point.ann_mean <- rbind(point.ann_mean, point_grid_extract(annual_mean, point_coord))

climate_data <- extract_nc_value(1971,1980)
annual_mean <- temporal_mean(climate_data,"annual")
point.ann_mean <- rbind(point.ann_mean,point_grid_extract(annual_mean,point_coord))

climate_data <- extract_nc_value(1981, 1990)
annual_mean <- temporal_mean(climate_data, "annual")
point.ann_mean <- rbind(point.ann_mean, point_grid_extract(annual_mean, point_coord))

climate_data <- extract_nc_value(1991, 2000)
annual_mean <- temporal_mean(climate_data, "annual")
point.ann_mean <- rbind(point.ann_mean, point_grid_extract(annual_mean, point_coord))

climate_data <- extract_nc_value(2001, 2012)
annual_mean <- temporal_mean(climate_data, "annual")
point.ann_mean <- rbind(point.ann_mean, point_grid_extract(annual_mean, point_coord))


names(point.ann_mean) <- c("year", "mean_temp") # I really need to fix this

plot(point.ann_mean$year, point.ann_mean$mean_temp, type = 'l')
abline(h = mean(point.ann_mean$mean_temp), col = 'red')
```

**6.** If you want a raster of the mean temperature across Europe for a specific year (e.g. 1988).
```
# get a raster

library(climateExtract)
library(raster)
climate_data <- extract_nc_value(1988, 1988, local_file = FALSE, clim_variable = 'mean temp', grid_size = 0.25)

# compute the annual mean temperature
annual_mean <- temporal_mean(climate_data, "annual")

# get a XY data.frame for the 0.25deg grid
grid.e <- expand.grid(annual_mean$longitude, annual_mean$latitude)

# extract the value for 1988 in one vector
y1988 <- as.vector(annual_mean$value_array[ , , 1])

# build a XYZ object
xyz_value <- cbind(grid.e, y1988)

# build a raster from the XYZ object with the library raster
r <- rasterFromXYZ(xyz_value, crs = CRS("+init=epsg:4326"))
names(r) <- annual_mean$date_extract[1]

# voila!
plot(r, main("Mean temperature in 1988")

# you could use the stack() function to build a raster stack (multiple bands) with multiple years, months or days

```

*This is a work in progress that is good for some tasks, but this comes with no guarantee. Suggestions and contributions for improvement are welcome.*

#### TO DO
* optimize the script to avoid (limit) memory issues under Windows
* implement data.table approach to compute summary statistics to speed up computation
* update and improve documentation
