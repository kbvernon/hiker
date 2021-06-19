
#' @noRd
#'
tobler <- function(x, off.path = TRUE) {

  # tobler on- or off-path
  w <- ifelse(off.path, 0.6, 1.0)

  speed <- 6 * exp(-3.5 * abs(x + 0.05)) * w

  # convert to m/s
  speed * (1000 / 3600)

}


#' @noRd
#'
campbell <- function(x, decile = 50L) {

  if (length(decile) != 1) {

    stop("campbell accepts only one decile at a time.")

  }

  # campbell function wants degrees, not rise-over-run as in tobler
  # but, we're working with rr, so remember to convert radians to degrees with 180/pi!
  slope <- atan(x) * 180 / pi

  # values provided in Campbell 2019, Supplement, Table S1
  # for simplicity, I excluded all but the deciles
  deciles <-
    data.frame(
      decile = c(10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L),
      a = c(-1.568, -1.71,  -1.858, -1.958, -2.171, -2.459, -2.823,  -3.371,  -3.06),
      b = c(13.328, 10.154,  8.412,  8.96,  10.064, 11.311, 12.784,  15.395,  16.653),
      c = c(38.892, 36.905, 39.994, 50.34,  63.66,  79.287, 98.697, 134.409, 138.875),
      d = c( 0.404,  0.557,  0.645,  0.649,  0.628,  0.599,  0.566,   0.443,   0.823),
      e = c(-0.003, -0.004, -0.004, -0.005, -0.005, -0.005, -0.005,  -0.005,  -0.0139)
    )

  ind <- which(deciles$decile == as.integer(decile))

  deciles <- deciles[ind, ]

  a <- deciles$a
  b <- deciles$b
  c <- deciles$c
  d <- deciles$d
  e <- deciles$e

  # lorentz distribution
  lorentz <- (1 / ((pi * b) * (1 + ((slope - a) / b)^2)))

  # modified lorentz
  (c * lorentz) + d + (e * slope)

}
