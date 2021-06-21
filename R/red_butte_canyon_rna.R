
#' Red Butte Canyon Research Natural Area
#'
#' A spatial dataset for the Red Butte Canyon (RBC) Research Natural Area (RNA)
#' including a Digital Elevation Model (DEM), a polygon of the RNA boundary, a
#' a polygon of the RBC Reservoir, and a linestring of the RBC Road.
#'
#' The vectors are all saved as external data rather than being exported by the package.
#' This is to avoid CRAN protesting about non-ASCII characters in their CRS.
#'
#' @format The DEM raster is saved as a TIF file, with elevation in meters.
#'   The three vectors are saved as GEOJSON. All three vectors have a name
#'   attribute and geometry. All spatial data share the same CRS,
#'   _NAD83 / UTM zone 12N_ (EPSG: 26912).
#'
#' @source Data for the DEM was downloaded from the US National Elevation Dataset
#'   using the [`FedData`](https://docs.ropensci.org/FedData/) package. The RNA
#'   boundary polygon was downloaded from the
#'   [US Forest Service Geodata Clearinhouse](https://data.fs.usda.gov/geodata/edw/datasets.php?dsetCategory=boundaries).
#'   The RBC Reservoir polygon was (sadly) digitized by hand using Google Earth. And
#'   the road linestring was derived from the
#'   [Utah AGRC Roads and Highways](https://gis.utah.gov/data/transportation/roads-system/)
#'   dataset.
#'
#' @docType data
#' @keywords datasets
#' @name rbc
#' @aliases red butte canyon, red butte, rna
#'
NULL
