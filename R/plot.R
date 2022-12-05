
#' Plot terrain
#'
#' @param x a terrain object
#' @param ... additional arguments passed on to plot method for spatRaster
#'
#' @details
#' See terra's plot function for additional arguments.
#'
#' @export
#'
#' @examples
#' library(terra)
#'
#' fn <- system.file("extdata/red_butte_dem.tif", package = "hiker")
#' red_butte_dem <- rast(fn)
#'
#' terrain <- hf_terrain(red_butte_dem)
#'
#' # plot(terrain, main = "Travel Cost")
plot.terrain <- function(x, ...){

  terra::plot(hf_rasterize(x), ...)

}
