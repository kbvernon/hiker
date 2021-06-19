
#' Cost surface
#'
#' Create a `terrain` representing the cost of travel. No assumption is
#' made about existing trails or paths.
#'
#' The `SpatRaster` must have a projected CRS, with both distance units and elevation
#' values in meters.
#'
#' For a list of available hiking functions and their parameters, see
#' `vignette("hiking functions", package = "hiker")`.
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

  n_cells <- terra::ncell(x)

  # find adjacent cells in the raster
  # make sure to ignore all na values
  na_cells <- which(is.na(terra::values(x)))

  cells <- setdiff(1:n_cells, na_cells)

  # this bit on adjacency will be greatly simplified
  # once terra 1.2-18 is available
  # see https://github.com/rspatial/terra/issues/239
  neighbors <- switch(as.character(neighbors)[1],
                      "4" = "rook",
                      "8" = "queen",
                      "16"= "16",
                      stop(paste0("hiker does not support ",
                                  neighbors[1],
                                  " neighbors, only 4, 8, or 16.")))

  adj <- terra::adjacent(x,
                         cells = cells,
                         directions = neighbors)

  adj <- cbind("from" = rep(as.integer(rownames(adj)), each = ncol(adj)),
               "to" = as.vector(t(adj)))

  adj <- adj[!is.na(adj[, 2]), ]

  adj <- adj[!(adj[, 2] %in% na_cells), ]


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

  cm <- Matrix::Matrix(0, nrow = n_cells, ncol = n_cells)

  cm[adj] <- conductance


  # include some spatial and other information to make
  # manipulating the network faster and easier
  # when passed to other functions
  # storing bounding box as vector to keep the size down
  bb8 <- terra::ext(x)
  bb8 <- c(bb8$xmin, bb8$xmax, bb8$ymin, bb8$ymax)

  xcrs <- terra::crs(x, describe = TRUE)
  epsg <- as.integer(xcrs["EPSG"])
  epsg <- paste0("epsg:", epsg)

  terrain <- list("conductance" = cm,
                  "neighbors"   = neighbors,
                  "nrow"        = terra::nrow(x),
                  "ncol"        = terra::ncol(x),
                  "bb8"         = bb8,
                  "epsg"        = epsg)

  class(terrain) <- "terrain"

  return(terrain)

}




#' @name hf_terrain
#' @export
#'
print.terrain <- function(x, ...){

  cat("class       :", class(x), "\n")

  ncell <- x$nrow * x$ncol

  cat("dimensions  : ", x$nrow, ", ", x$ncol, ", ", ncell, " (nrow, ncol, ncell)\n", sep = "")

  dx <- (x$bb8[["xmax"]] - x$bb8[["xmin"]])/x$ncol
  dy <- (x$bb8[["ymax"]] - x$bb8[["ymin"]])/x$nrow

  dx <- round(dx, digits = 2)
  dy <- round(dy, digits = 2)

  cat("resolution  : ", dx, ", ", dy, " (x, y)\n", sep = "")
  cat("epsg        :", gsub("epsg:", "", x$epsg), "\n")

  # invert conductance to get cost
  rmin <- round(1/min(x$conductance), digits = 2)

  if (is.infinite(rmin)) rmin <- 0

  rmax <- round(1/max(x$conductance), digits = 2)

  cat("cost        : ", rmin, ", ", rmax, " (min, max)", sep = "")

}