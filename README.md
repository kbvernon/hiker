
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hiker <img src='man/figures/logo.png' align="right" height="160" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/kbvernon/hiker/workflows/R-CMD-check/badge.svg)](https://github.com/kbvernon/hiker/actions)
<!-- badges: end -->

The **hiker** package aids in the modeling of specifically human
movement across a landscape. Much of its code was inspired by Jacob van
Etten’s (2017) [`gdistance`](https://github.com/AgrDataSci/gdistance)
package, though it works with more modern spatial tools, specifically
[`sf`](https://r-spatial.github.io/sf/) and
[`terra`](https://rspatial.github.io/terra/index.html).

## Installation

The hiker package is not currently on
[CRAN](https://CRAN.R-project.org), though that will hopefully change
soon enough. In the meantime, you can download and install the
development version using:

``` r
install.packages("remotes")
remotes::install_github("kbvernon/hiker")
```

## Example

Consider the [Red Butte Canyon Research Natural
Area](https://redbuttecanyon.net/) in the Wasatch Foothills above the
University of Utah. If I wanted to hike up this canyon in the shortest
time possible, what route should I take?

``` r

library(hiker)
library(sf)
library(terra)
library(viridis)
```

``` r

fn <- system.file("extdata/red_butte_dem.tif", package = "hiker")
red_butte_dem <- rast(fn)

from <- st_sf(
  geometry = st_sfc(st_point(c(432000, 4513100))),
  crs = 26912
)

to <- st_sf(
  geometry = st_sfc(st_point(c(436750, 4518500))),
  crs = 26912
)
```

To figure out the “shortest” path from start to end, we need to have
some sense of the time it would take to move through any particular
location in the canyon. This *cost surface* is the terrain we are hiking
in R.

``` r

(terrain <- hf_terrain(red_butte_dem))
#> class       : terrain 
#> dimensions  : 268, 260, 69680 (nrow, ncol, ncell)
#> resolution  : 26.48, 26.48 (x, y)
#> extent      : 430567.2 437451.2 4512415 4519511 (xmin, xmax, ymin, ymax)
#> coord.ref   : EPSG 26912 
#> min cost    : 9.98 
#> max cost    : 75.86

# to adjust legend (plg) and axis (pax) text size
pax <- list(cex.axis = 0.65)
plg <- list(cex = 0.65)

plot(terrain, 
     col = viridis(50), 
     pax = pax, 
     plg = plg)

title(main = "Travel Cost (sec)", adj = 0, line = 0.3)
```

<img src="man/figures/README-cost_surface-1.png" width="100%" />

The next step is to simply pass the start and end point, along with the
cost surface, to one of our three path functions. For instance,
`hf_hike()` computes shortest paths and returns them as `sf` objects.

``` r

short_path <- hf_hike(terrain, from, to)

short_path
#> Simple feature collection with 1 feature and 2 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 432010.2 ymin: 4513090 xmax: 436749.6 ymax: 4518492
#> Projected CRS: NAD83 / UTM zone 12N
#>   from to                       geometry
#> 1    1  1 LINESTRING (432010.2 451309...
```

Now we can visualize this `short_path`.

``` r

plot(red_butte_dem, 
     col = viridis(50),
     pax = pax, 
     plg = plg)

title(main = "Path over Elevation (m)", adj = 0, line = 0.3)

plot(st_geometry(short_path),
     col = "white",
     lwd = 1.5,
     add = TRUE)

plot(st_geometry(from), 
     pch = 21, 
     cex = 1.3, 
     bg = "red2", 
     col = "white", 
     add = TRUE)

plot(st_geometry(to), 
     pch = 21, 
     cex = 1.3, 
     bg = "dodgerblue3", 
     col = "white", 
     add = TRUE)
```

<img src="man/figures/README-short_path_plot-1.png" width="100%" />

## Alternative packages

I am aware of three packages that offer similar functionality:

- [`leastcostpath`](https://github.com/josephlewis/leastcostpath),
- [`movecost`](https://github.com/cran/movecost), and
- [`marmap`](https://github.com/ericpante/marmap)

The hiker package differs from these packages in the following ways:

1.  hiker is simple. The package is somewhat singular in purpose and
    many arguably reasonable assumptions come baked in. This makes for
    much simpler code and a more user-friendly API.  
2.  hiker is tidy (or tries to be). As much as possible, code is written
    to comport with the `tidyverse` [style
    guide](https://style.tidyverse.org/), mostly thanks to
    `usethis::use_tidy_style()`, and development largely follows the
    guidelines described in Wickham and Bryan’s [*R
    Packages*](https://r-pkgs.org/index.html).  
3.  hiker uses [`sf`](https://r-spatial.github.io/sf/) rather than its
    predecessor sp.  
4.  hiker uses [`terra`](https://rspatial.github.io/terra/index.html)
    instead of its predecessor `raster`.

## References

Campbell, Michael J., Philip E. Dennison, Bret W. Butler, and Wesley G.
Page (2019). “Using crowdsourced fitness tracker data to model the
relationship between slope and travel rates. *Applied Geography* 106,
93-107. <https://doi.org/10.1016/j.apgeog.2019.03.008>

Tobler, Waldo R. (1993). “Three Presentations on Geographical Analysis
and Modeling: Non-Isotropic Geographic Modeling, Speculations on the
Geometry of Geography, and Global Spatial Analysis.” *National Center
for Geographic Information and Analysis, Technical Report* 93-1.

van Etten, Jacob (2017). “R package gdistance: Distances and routes on
geographical grids.” *Journal of Statistical Software*, 76(1), 1–21.
<https://doi.org/10.18637/jss.v076.i13>

## Code of Conduct

Please note that the hiker project is released with a [Contributor Code
of
Conduct](https://github.com/kbvernon/hiker/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
