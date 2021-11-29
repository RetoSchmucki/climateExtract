#' ggd_extract
#' Compute the growing degree day (GDD) using either the mean temperature or the Baskerville-Emin method,
#' with base temperature.
#' @param base_temp value for the base temperature to use for the growing degree day, default = 4°C
#' @param min_temp raster, rasterbrick or vector of the minimum daily temperature
#' @param max_temp raster, rasterbrick or vector of the maximum daily temperature
#' @param avg_temp raster, rasterbrick or vector of the mean daily temperature
#' @param gdd_method string defining the method to use, "avg" for mean daily temperature or "be" for
#' the Baskerville-Emin method that fit a sine curve on the minimum and the maximum to account for daily
#' variation in temperature
#' @param top_temp value for the maximum temperature beyond growing degree day are not accumulating, default = NULL
#' @details This function computes the Growing Degree Day on a specific base temperature and method (i.e., `avg` or `be`). The funciton
#' is build on two internal functions, gdd_avg_r() and gdd_be_r() to apply the specific gdd calculation method. The function
#' works on raster, rasterbrick (multiple layers), vector or matrix.
#' @author Reto Schmucki
#' @export
#'

gdd_extract <- function(base_temp = NULL, min_temp = NULL, max_temp = NULL, avg_temp = NULL,
                        gdd_method = "avg", top_temp = NULL) {
    if (!is.null(top_temp)) {
        if (top_temp <= base_temp) stop("base_temp must be lower than top_temp")
    }
    if (gdd_method == "avg") {
        GDD <- gdd_avg_r(base_temp, min_temp, max_temp, avg_temp, top_temp)
    }
    if (gdd_method == "be") {
        GDD <- gdd_be_r(base_temp, min_temp, max_temp, avg_temp, top_temp)
    }
    return(GDD)
}

#' gdd_avg_r
#'
#' Compute the growing degree day (GDD) using a base temperature against the mean daily temperature.
#' @param base_temp value for the base temperature to use for the growing degree day, default = 4°C
#' @param min_temp raster, rasterbrick or vector of the minimum daily temperature
#' @param max_temp raster, rasterbrick or vector of the maximum daily temperature
#' @param avg_temp raster, rasterbrick or vector of the mean daily temperature
#' @param top_temp value for the maximum temperature beyond growing degree day are not accumulating, default = NULL
#' @details This function computes a rough estimate of GDD from the mean temperature without accounting from variation
#' during the day. For a more refined method using a daily curve, use the BE method (Baskerville-Emin method) available
#' with `gdd_be_r()`.
#' @author Reto Schmucki
#' @export
#'

gdd_avg_r <- function(base_temp, min_temp, max_temp, avg_temp, top_temp) {
    if (!is.null(base_temp)) {
        b.t <- base_temp
    } else {
        b.t <- 4
    }
    if (is.null(avg_temp) & (is.null(min_temp) | is.null(max_temp))) {
        stop("values for the average temperature or the minimum and maximum temperature (min_temp and max_temp) must be provided for estimating GDD")
    }
    mn.t <- min_temp
    mx.t <- max_temp
    if (is.null(avg_temp)) {
        avg.t <- (mn.t + mx.t) / 2
    } else {
        avg.t <- avg_temp
    }
    GDD <- avg.t - base_temp
    GDD[GDD < 0] <- 0

    if (is.null(top_temp)) {
        GDD <- GDD
    } else {
        GDD_top <- avg.t - top_temp
        GDD_top[GDD_top < 0] <- 0
        GDD <- GDD - GDD_top
    }

    if (is.null(avg_temp)) {
        names(GDD) <- names(mn.t)
    } else {
        names(GDD) <- names(avg_temp)
    }

    return(GDD)
}

#' gdd_be_r
#'
#' Compute the growing degree day (GDD) using the BE method (Baskerville-Emin method) with base temperature.
#' @param base_temp value for the base temperature to use for the growing degree day, default = 4°C
#' @param min_temp raster, rasterbrick or vector of the minimum daily temperature
#' @param max_temp raster, rasterbrick or vector of the maximum daily temperature
#' @param avg_temp raster, rasterbrick or vector of the mean daily temperature
#' @param top_temp value for the maximum temperature beyond growing degree day are not accumulating, default = NULL
#' @details This function computes an estimate of GDD fitting a sine curve on the minimum and the maximum to account for
#' daily variation in temperature.
#' @author Reto Schmucki
#' @export
#'

gdd_be_r <- function(base_temp, min_temp, max_temp, avg_temp, top_temp) {
    if (!is.null(base_temp)) {
        b.t <- base_temp
    } else {
        b.t <- 4
    }
    if (is.null(min_temp) | is.null(max_temp)) {
        stop("values for minimum and maximum temperature (min_temp and max_temp) must be provided for the BE method")
    }
    mn.t <- min_temp
    mx.t <- max_temp
    if (is.null(avg_temp)) {
        avg.t <- (mn.t + mx.t) / 2
    } else {
        avg.t <- avg_temp
    }
    fv <- 3.14 / 2
    W <- (mx.t - mn.t) / 2
    A <- (b.t - avg.t) / W
    A[A < -1] <- -1
    A[A > 1] <- 1
    A <- asin(A)
    GDD <- round(((W * cos(A)) - ((b.t - avg.t) * (fv - A))) / 3.14, digits = 2)
    GDD[GDD < 0] <- 0

    if (is.null(top_temp)) {
        GDD <- GDD
    } else {
        At <- (top_temp - avg.t) / W
        At[At < -1] <- -1
        At[At > 1] <- 1
        At <- asin(At)
        GDD_top <- round(((W * cos(At)) - ((top_temp - avg.t) * (fv - At))) / 3.14, digits = 2)
        GDD_top[GDD_top < 0] <- 0
        GDD <- GDD - GDD_top
    }

    names(GDD) <- names(mn.t)

    return(GDD)
}

#' cumsum_rb
#'
#' Compute the cumulative sum on a multilayered raster (rasterbrick), using indices spaning over specific time periods.
#' @param x rasterbrick object on which cumulative sum should be computed
#' @param indices vector with indices over which the sum is to be cumulated.
#' @details This function computes the cumulative sum over specific periods defined by the a vector of incides (e.g. 
#' two five-day cummulative sum c(1,1,1,1,1,2,2,2,2,2), restarting at zero on the first day of each series defined by
#' the indices).
#' @author Reto Schmucki
#' @export
#'

cumsum_rb <- function(x, indices = NULL) {
    if (is.null(indices)) {
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


#' get_date
#'
#' Extract a series of dates from the layers named in a rasterBrick or a vector of strings containing dates.
#' @param x rasterBrick or a vector of dates, from which to extract the dates of the time-series.
#' @param pattern string or characters to remove from the date name (remove the character "X" from "X2001.07.15")
#' @param date_format format of the date
#' @details internal function to retrieve data from names.
#' @author Reto Schmucki
#' @export
#'

get_date <- function(x, pattern, date_format) {
    if (class(x) == "RasterBrick") {
        xn <- names(x)
    } else {
        xn <- x
    }
    date_vect <- as.Date(gsub(pattern, "", xn, fixed = TRUE), date_format)
    return(date_vect)
}


#' get_layer_indice
#'
#' Compute a vector of indices of "year" or "month" based on the date extracted from the rasterBrick or a vector of
#' strings over the time period.
#' @param x rasterBrick or a vector of dates, from which to extract the dates of the time-series.
#' @param pattern string or characters to remove from the date name (e.g. remove the character "X" from "X2001.07.15")
#' @param date_format format of the date
#' @param indice_level string to define the time-period to use to define the indices, "year" or "month".
#' @details This function generate a vector of indices dividing the time-series in years or months. This indices are use
#' for the calculating the cummulative sum over years or month (see function cumsum_rb()).
#' @author Reto Schmucki
#' @export
#'

get_layer_indice <- function(x = NULL, pattern = "X", date_format = "%Y.%m.%d", indice_level = "year") {
    layer_indices <- as.numeric(factor(lubridate::floor_date(get_date(x = x, pattern = pattern, date_format = date_format), indice_level)))
    return(layer_indices)
}