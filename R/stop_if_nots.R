
#' Just so I don't have to retype these checks every time.
#'
#' @noRd
#'
stop_if_not_point <- function(...) {

  dots <- list(...)

  types <- lapply(dots, sf::st_geometry_type)
  types <- unlist(types)
  types <- as.character(unique(types))

  set_difference <- setdiff(types, c("POINT", "MULTIPOINT"))

  any_geometry_is_not_point <- length(set_difference) > 0

  if ( any_geometry_is_not_point ) {

    stop(
      paste0(
        "Only POINT and MULTIPOINT geometries are supported.\n\n",
        "For all other simple feature geometries,\n",
        "consider sampling points along their edges."
      ),
      call. = FALSE
    )

  }

}



#' @noRd
#'
stop_if_not_terrain <- function(x) {

  if (!inherits(x, "terrain")) {

    stop(
      paste0(
        class(x)[1], " objects not supported. ",
        "x must be a terrain object.\n",
        "Use hf_terrain() first."),
      call. = FALSE
    )

  }

}



#' @noRd
#'
stop_if_not_sf <- function(...) {

  dots <- list(...)

  not_sf <- lapply(dots, function(x) {

    !inherits(x, "sf") && !inherits(x, "sfc")

  })

  not_sf <- unlist(not_sf)

  if ( any(not_sf) ) {

    x <- dots[[which(not_sf)]]

    stop(
      paste0(
        class(x)[1], " objects not supported. Shape must be an sf or sfc."
      ),
      call. = FALSE
    )

  }

}



#' @noRd
#'
stop_if_not_SpatRaster <- function(x) {

  if (!inherits(x, "RasterLayer") && !inherits(x, "SpatRaster")) {

    stop("x must be a RasterLayer or SpatRaster", call. = FALSE)

  }

  if (terra::is.lonlat(x)) {

    stop(
      paste0(
        "Projected CRS required.\n",
        "Note: elevation must be in same units as CRS."
      ),
      call. = FALSE
    )

  }

}


#' @noRd
#'
stop_if_not_crs_equal <- function(x, ...) {

  comparison <- list(...)

  crs_not_equal <- lapply(comparison, function(z) {

    sf::st_crs(x) != sf::st_crs(z)

  })

  crs_not_equal <- unlist(crs_not_equal)

  if( any(crs_not_equal) ) {

    stop("arguments have different crs", call. = FALSE)

  }

}

