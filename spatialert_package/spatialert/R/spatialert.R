#' spatialert: Interactive Spatial Hotspot Analysis for Public Health
#'
#' @description
#' spatiAlert provides a point-and-click Shiny interface for identifying spatial
#' hotspots in public health data using the Getis-Ord Gi* statistic and
#' Local Moran's I. All analysis runs locally — no data ever leaves your machine.
#'
#' @section Main function:
#' - [hotspot_app()]: Launch the spatiAlert Shiny application
#'
#' @section Core analysis functions (also usable in scripts):
#' - [compute_gi_star()]: Compute Getis-Ord Gi* statistic for an sf object
#' - [compute_local_moran()]: Compute Local Moran's I for an sf object
#' - [build_weights()]: Build spatial weights matrix
#' - [classify_hotspots()]: Classify areas into hot/cold/not significant
#'
#' @section Data helpers:
#' - [validate_upload()]: Validate and parse an uploaded data file
#' - [join_to_geography()]: Spatially join facility-level data to areal units
#' - [fetch_geography()]: Fetch census geography via tigris
#'
#' @docType package
#' @name spatialert-package
"_PACKAGE"
