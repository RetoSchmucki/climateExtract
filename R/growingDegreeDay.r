#' gdd_avg_r
#'
#' Compute the growing degree day (GDD) using a base temperature against the mean daily temperature.
#' @param base_temp value for the base temperature to use for the growing degree day, default = 4⁰C
#' @param min_tem raster, rasterbrick or vector of the minimum daily temperature
#' @param max_temp raster, rasterbrick or vector of the maximum daily temperature
#' @param avg_temp raster, rasterbrick or vector of the mean daily temperature
#' @details This function compute a rough estimate of GDD from the mean temperature without accounting from variation during the day.
#' For a more refined method using a daily curve, use the BE method (Baskerville-Emin method) available with
#' `gdd_be_r``
#' @author Reto Schmucki
#' @export
#'
# example
# avg_ggd_france <- gdd_avg_r(base_temp = 7, avg_temp = rbk)

gdd_avg_r <- function(base_temp = NULL, min_temp = NULL, max_temp = NULL, avg_temp = NULL) {
    if (!is.null(base_temp)) {
        b.t <- base_temp
    } else {
        b.t <- 4
    }
    if(is.null(avg_temp) & (is.null(min_temp) | is.null(max_temp))){
        stop('values for the average temperature or the minimum and maximum temperature (min_temp and max_temp) must be provided for estimating GDD')
    }
    mn.t <- min_temp
    mx.t <- max_temp
    if (is.null(avg_temp)) {
        avg.t <- (mn.t + mx.t) / 2
    } else {
        avg.t <- avg_temp
    }
    GDD <- avg.t - base_temp
    GDD[GDD < 0] = 0
    return(GDD)
}

#' gdd_be_r
#'
#' Compute the growing degree day (GDD) using the BE method (Baskerville-Emin method) with base temperature.
#' @param base_temp value for the base temperature to use for the growing degree day, default = 4⁰C
#' @param min_tem raster, rasterbrick or vector of the minimum daily temperature
#' @param max_temp raster, rasterbrick or vector of the maximum daily temperature
#' @param avg_temp raster, rasterbrick or vector of the mean daily temperature
#' @details This function compute an estimate of GDD fitting a sine curve on the minimum and the maximum to account for
#' daily variation in temperature.
#' @author Reto Schmucki
#' @export
#'


gdd_be_r <- function(base_temp = NULL, min_temp = NULL, max_temp = NULL, avg_temp = NULL) {
    if (!is.null(base_temp)) {
        b.t <- base_temp
    } else {
        b.t <- 4
    }
   if(is.null(min_temp) | is.null(max_temp)){
            stop('values for minimum and maximum temperature (min_temp and max_temp) must be provided for the BE method')
        }
    mn.t <- min_temp
    mx.t <- max_temp
    if (is.null(avg_temp)) {
        avg.t <- (mn.t + mx.t) / 2
    } else {
        avg.t <- avg_temp
    }
    fv = 3.14 / 2
    W = (mx.t-mn.t) / 2
    A = (b.t - avg.t) / W
    A[A < -1] = -1
    A[A > 1] = 1
    A = asin(A)
    GDD = round(((W*cos(A)) - ((b.t - avg.t) * (fv - A))) / 3.14, digits = 2)

    GDD[GDD < 0] <- 0
    names(GDD) <- names(mn.t)

    return(GDD)
}

#' ggd_extract
#' Compute the growing degree day (GDD) using either the mean temperature or the Baskerville-Emin method,
#' with base temperature.
#' @param base_temp value for the base temperature to use for the growing degree day, default = 4⁰C
#' @param min_tem raster, rasterbrick or vector of the minimum daily temperature
#' @param max_temp raster, rasterbrick or vector of the maximum daily temperature
#' @param avg_temp raster, rasterbrick or vector of the mean daily temperature
#' @param gdd_method string defining the method to use, "avg" for mean daily temperature or "be" for
#' the Baskerville-Emin method that fit a sine curve on the minimum and the maximum to account for daily 
#' variation in temperature.
#' @details This function is a wrapper around the internal functions gdd_avg_r and gdd_be_r where each method is coded.
#' @author Reto Schmucki
#' @export
#'

gdd_extract <- function(base_temp = NULL, min_temp = NULL, max_temp = NULL, avg_temp = NULL,
                         gdd_method = "avg"){
    if(gdd_method == "avg"){
        GDD <- gdd_avg_r(base_temp = base_temp, min_temp = min_temp, max_temp = max_temp, avg_temp = avg_temp)
    }
    if(gdd_method == "be"){
        GDD <- gdd_be_r(base_temp = base_temp, min_temp = min_temp, max_temp = max_temp, avg_temp = avg_temp)
    }
    return(GDD)
}


cumsum_rb <- function(x, indices = NULL){
    
    if(is.null(indices)){
            indices <- rep(1, dim(x)[3])
    }
    x_agg <- stars::st_as_stars(x)

    x_agg[[1]] <- as.integer(x_agg[[1]] * 100)
    for (i in seq_along(unique(indices))) {
        j <- unique(indices)[i]
        if (i == 1) {
            x_agg_cumsum_r <- stars::st_apply(x_agg[, , , which(indices == j)], c(1, 2), cumsum)
        } else {
            x_agg_cumsum_i <- stars::st_apply(x_agg[, , , which(indices == j)], c(1, 2), cumsum)
            x_agg_cumsum_r <- c(x_agg_cumsum_r, x_agg_cumsum_i, along = 1)
        }
    }
    x_agg_cumsum_r[[1]] <- x_agg_cumsum_r[[1]] / 100
    x_agg_cumsum_r <- as(x_agg_cumsum_r, "Raster")
    names(x_agg_cumsum_r) <- names(x)
return(x_agg_cumsum_r)
}


get_date <- function(x=x, pattern = pattern, date_format = date_format){
    date_vect <- as.Date(gsub(pattern, "", names(x), fixed= TRUE), date_format)
    return(date_vect)
}

get_layer_indice <- function(x=NULL, pattern = "X", date_format = "%Y.%m.%d", indice_level = "year"){
    layer_indices <- as.numeric(factor(lubridate::floor_date(get_date(x=x, pattern=pattern, date_format = date_format), indice_level)))
    return(layer_indices)
}

