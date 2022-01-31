
#' Cost surface
#'
#' Create a `terrain` representing the cost of travel. For a list of available
#' hiking functions and their parameters, see
#' `vignette("hiking functions", package = "hiker")`.
#'
#' The `SpatRaster` must have a projected CRS, with both distance units and elevation
#' values in meters.
#'
#' The range shown when printing excludes zero and infinite values
#' for min and max, respectively.
#'
#' @param x a `SpatRaster` with elevation values.
#' @param hf a character string specifying the preferred hiking function. Options
#'   include `tobler` and `campbell` (default).
#' @param max_slope a numeric value specifying the maximum allowed hiking slope
#'   _in degrees_ (default is 45). Suppose you ask: Would a person really hike
#'   _that_? This is the shallowest slope where the answer to that question is No.
#' @param neighbors numeric, one of 4, 8, or 16 (default) specifying the number of
#'   adjacent cells between which to calculate movement cost. These are passed on
#'   to `terra::adjacent()` and represent standard neighborhoods (rook, queen, and
#'   knight + queen).
#' @param ... Arguments passed on to the hiking function specified by `hf`.
#'
#' @return a `terrain` representing cost of travel.
#'
#' @references
#' Campbell, Michael J., Philip E. Dennison, Bret W. Butler, and Wesley G. Page
#' (2019). "Using crowdsourced fitness tracker data to model the relationship
#' between slope and travel rates. _Applied Geography_ 106, 93-107.
#' \doi{https://doi.org/10.1016/j.apgeog.2019.03.008}
#'
#' Tobler, Waldo R. (1993). "Three Presentations on Geographical Analysis
#' and Modeling: Non-Isotropic Geographic Modeling, Speculations
#' on the Geometry of Geography, and Global Spatial Analysis."
#' _National Center for Geographic Information and Analysis, Technical Report_ 93-1.
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
#' # convert to raster to visualize
#' # plot(hf_rasterize(terrain), main = "Travel Cost")
#'
#' # tobler on path
#' # terrain <- hf_terrain(red_butte_dem, hf = "tobler", off.path = FALSE)
#'
#' # max slope
#' # terrain <- hf_terrain(red_butte_dem, max_slope = 25)
#'
#' # neighbors
#' # terrain <- hf_terrain(red_butte_dem, neighbors = 16)
#'
hf_terrain <- function(x,
                       hf = "campbell",
                       max_slope = 45,
                       neighbors = 8,
                       ...) {

  stop_if_not_SpatRaster(x)

  # find adjacent cells in the raster
  # make sure to ignore all na values
  n_cells <- terra::ncell(x)

  na_cells <- which(is.na(terra::values(x)))

  cells <- setdiff(1:n_cells, na_cells)

  adj <- terra::adjacent(x,
                         cells = cells,
                         directions = neighbors,
                         pairs = TRUE)

  adj <- adj[!adj[, 2] %in% na_cells, ]


  ### slope ###
  # change in y between adjacent pixels
  heights <- terra::values(x)[, 1]

  rise <- (heights[adj[, 2]] - heights[adj[, 1]])

  # change in x between adjacent pixels
  run <- terra::distance(terra::xyFromCell(x, adj[, 1]),
                         terra::xyFromCell(x, adj[, 2]),
                         lonlat = FALSE,
                         pairwise = TRUE)

  slope <- rise/run


  ### speed ###
  speed_fun <- switch(hf,
                      "campbell" = campbell,
                      "tobler"   = tobler,
                      stop(paste0("hiker does not implement the ",
                                  hf,
                                  " hiking function.")))

  speed <- speed_fun(slope, ...)


  ### conductance ###
  conductance <- speed/run

  # what is a realistic slope people would try to hike?
  max_slope <- tan(max_slope * pi / 180) # degrees -> radians -> rise-over-run

  i <- which(abs(slope) >= max_slope)

  conductance[i] <- 0

  # build matrix
  cm <- Matrix::Matrix(0, nrow = n_cells, ncol = n_cells)

  cm[adj] <- conductance


  # include some spatial and other information to make
  # manipulating the network faster and easier
  # when passed to other functions
  # storing bounding box as vector to keep the size down
  bb8 <- terra::ext(x)
  bb8 <- c(bb8$xmin, bb8$xmax, bb8$ymin, bb8$ymax)

  xcrs <- terra::crs(x, describe = TRUE)
  xcrs <- paste0(xcrs$authority, ":", xcrs$code)

  tmin <- 1/max(conductance)
  tmax <- 1/min(conductance[conductance > 0])

  terrain <-
    list(
      "conductance" = cm,
      "neighbors"   = neighbors,
      "nrow"        = terra::nrow(x),
      "ncol"        = terra::ncol(x),
      "bb8"         = bb8,
      "crs"         = xcrs,
      "range"       = c("min" = tmin, "max" = tmax)
    )

  class(terrain) <- "terrain"

  return(terrain)

}



#' @name hf_terrain
#' @export
#'
print.terrain <- function(x, ...){

  ncell <- x$nrow * x$ncol

  dx <- (x$bb8[["xmax"]] - x$bb8[["xmin"]]) / x$ncol
  dy <- (x$bb8[["ymax"]] - x$bb8[["ymin"]]) / x$nrow

  dx <- round(dx, digits = 2)
  dy <- round(dy, digits = 2)

  tmin <- round(x$range[["min"]], digits = 2)
  tmax <- round(x$range[["max"]], digits = 2)

  cat("class       :", class(x), "\n")
  cat("dimensions  :", paste(x$nrow, x$ncol, ncell, sep = ", "), "(nrow, ncol, ncell)\n")
  cat("resolution  :", paste(dx, dy, sep = ", "), "(x, y)\n")
  cat("extent      :", x$bb8, "(xmin, xmax, ymin, ymax)\n")
  cat("coord.ref   :", gsub(":", " ", x$crs), "\n")
  cat("min cost    :", tmin, "\n")
  cat("max cost    :", tmax, "\n")

}



#' @noRd
#'
update_range <- function(x) {

  new_values <- x$conductance@x

  cmin <- min(new_values)
  cmax <- max(new_values[is.finite(new_values)])

  # invert conductance to get min and max travel cost
  x$range[["min"]] <- 1/cmax
  x$range[["max"]] <- 1/cmin

  invisible(x)

}
