R --vanilla

devtools::install_github("hypertidy/tidync", dependencies = TRUE)

library(tidync)
data_nc <- tidync('tg_ens_mean_0.1deg_reg_v18.0e.nc')
system.time(
a <- tidync('tg_ens_mean_0.1deg_reg_v18.0e.nc') %>% activate("tg") %>%
     hyper_filter(longitude = longitude >= -11 & longitude <= 31, latitude = latitude >= 36 &
                  latitude <= 71, time = time %in% 1:365) %>% hyper_array()
)

print(object.size(a), units='Gb')


system.time(
a <- tidync('tg_ens_mean_0.1deg_reg_v18.0e.nc') %>% activate("tg") %>%
     hyper_filter(longitude = longitude >= -11 & longitude <= 31, latitude = latitude >= 36 &
                  latitude <= 71, time = time %in% 1:1000) %>% hyper_array()
)

print(object.size(a), units='Gb')

r_nc <- raster::brick('tg_ens_mean_0.25deg_reg_v18.0e.nc')
dev.new()
system.time(
raster::plot(raster::stackApply(raster::subset(r_nc, 1:100), 1, mean))
)
dev.new()
system.time(
image(apply(tidync('tg_ens_mean_0.1deg_reg_v18.0e.nc') %>% activate("tg") %>%
     hyper_filter(longitude = longitude >= -12 & longitude <= 32, latitude = latitude >= 35 &
                  latitude <= 72, time = time %in% c(1:100)) %>% hyper_array() %>% .$tg,
c(1, 2), matrixStats::mean2, na.rm = T))
)

lon_lat <- tidync('tg_ens_mean_0.1deg_reg_v18.0e.nc') %>% activate("tg") %>%
                  hyper_filter(longitude = longitude >= -12 & longitude <= 32,
                  latitude = latitude >= 35 & latitude <= 72, time = time %in%
                  c(1)) %>% hyper_array()

lon <- attr(lon_lat, 'transforms')$longitude$longitude[attr(lon_lat, 'transforms')$longitude$selected]
lat <- attr(lon_lat, 'transforms')$latitude$latitude[attr(lon_lat, 'transforms')$latitude$selected]
grid.e <- expand.grid(lon, lat)

v <- apply(tidync('tg_ens_mean_0.1deg_reg_v18.0e.nc') %>% activate("tg") %>%
     hyper_filter(longitude = longitude >= -12 & longitude <= 32,
     latitude = latitude >= 35 & latitude <= 72, time = time %in% c(0:180)) %>%
     hyper_array() %>% .$tg, c(1, 2), mean, na.rm = TRUE)

xyz_value <- cbind(grid.e, as.vector(v))
r <- raster::rasterFromXYZ(xyz_value, crs = sp::CRS("+init=epsg:4326"))
raster::plot(r, col = rev(heat.colors(50)))


nc.ncdf <- ncdf4::nc_open('tg_ens_mean_0.25deg_reg_v18.0e.nc')

# retrieve dimensions
lon <- ncdf4::ncvar_get(nc.ncdf,"longitude")
lat <- ncdf4::ncvar_get(nc.ncdf,"latitude")
nlon <- dim(lon)
nlat <- dim(lat)
nc_var <- names(nc.ncdf$var)
nc_varname <- ncdf4::ncatt_get(nc.ncdf, nc_var,"long_name")$value

# set day since in the data
day_since <- ncdf4::ncatt_get(nc.ncdf,"time")$units
timeserie_length <- length(ncdf4::ncvar_get(nc.ncdf,"time"))
fillvalue <- ncdf4::ncatt_get(nc.ncdf, nc_var, "_FillValue")
day_vals <- ncdf4::ncvar_get(nc.ncdf,"time")

date_seq <- seq(lubridate::ymd(unlist(strsplit(gsub('days since ', '', day_since)," ",))[1]),
                                by = "day",  length.out = timeserie_length)

dt_iso_dwmy <- data.table::data.table(
                date = data.table::as.IDate(date_seq),
                day_vals = day_vals,
                year = data.table::year(date_seq),
                month = data.table::month(date_seq),
                day = data.table::mday(date_seq),
                week = data.table::isoweek(date_seq),
                week_day = c(7,1:6)[data.table::wday(date_seq)])


indx <- dt_iso_dwmy[year == unique(dt_iso_dwmy[ , year])[2], day_vals]

image(apply(tidync('tg_ens_mean_0.1deg_reg_v18.0e.nc') %>% activate("tg") %>%
                  hyper_filter(longitude = longitude >= -11 & longitude <= 31, latitude = latitude >= 36 &
                  latitude <= 71, time = time %in% dt_iso_dwmy[year == unique(dt_iso_dwmy[ , .(year, month)])[, year][2] &
                  month == unique(dt_iso_dwmy[ , .(year, month)])[, month][2], day_vals]) %>% hyper_tibble() %>% .$tg, c(1, 2), matrixStats::mean2, na.rm = T))
