#' Create patches for a specific feature
#'
#' @description This function takes a feature and splits it into multiple "patches" of the feature. This is ideal for features that you want to protect x% of, but want to make sure that entire patches (as opposed to pieces of patches) are protected to meet that target.
#'
#' @param feature a raster or sf object with the feature of interest present (value = 1) or absent (value = NA)
#' @param spatial_grid a raster or sf template with the desired resolution and coordinate reference system generated by `spatialgridr::get_grid()`; values in areas of interest are 1, while all other values are NA (only required if feature is a sf object)
#'
#' @return A raster or sf object with independent layers (raster)/columns (sf) designating the location of each patch
#' @export
#'
#' @examples
#'# Start with a little housekeeping to get the data from oceandatr
#'# Choose area of interest (Bermuda EEZ)
#'area <- oceandatr::get_area(area_name = "Bermuda",  mregions_column = "territory1")
#'projection <-'+proj=laea +lon_0=-64.8108333 +lat_0=32.3571917 +datum=WGS84 +units=m +no_defs'
#'# Create a planning grid
#'planning_raster <- spatialgridr::get_grid(area, projection = projection)
#'# Grab all relevant data
#'features_raster <- oceandatr::get_features(spatial_grid = planning_raster)
#'# Separate seamount data - we want to protect entire patches
#'seamounts_raster <- features_raster[["seamounts"]]
#'features_raster <- features_raster[[names(features_raster)[names(features_raster) != "seamounts"]]]
#'# Create a "cost" to protecting a cell - just a uniform cost for this example
#'cost_raster <- stats::setNames(planning_raster, "cost")
#'# Create patches from layer
#'patches_raster <- create_patches(seamounts_raster)

create_patches <- function(feature, spatial_grid = NULL) {

  # Add error for incorrect format of feature
  if(!check_raster(feature) & !check_sf(feature)) { stop("feature must be a raster or sf object")}

  if(class(feature)[1] %in% c("RasterLayer", "SpatRaster")) {
    feature <- feature %>%
      methods::as("SpatRaster") %>%
      terra::patches() %>%
      terra::segregate(other = NA) %>%
      stats::setNames(paste0("patches_", seq_len(terra::nlyr(.))))
  } else {

    if(is.null(spatial_grid)) {
      stop("spatial_grid required if feature is an sf object")
    }
    feature <- feature %>%
      dplyr::rename(value = 1) %>%
      dplyr::filter(value == 1)

    feature_matrix <- sf::st_touches(feature, sparse = F)
    hc <- stats::hclust(stats::as.dist(!feature_matrix), method = "single")
    feature_groups <- stats::cutree(hc, h = 0.5)

    suppressWarnings({
      feature <- feature %>%
        dplyr::mutate(patches = paste0("patches_", feature_groups)) %>%
        dplyr::mutate(value = 1) %>%
        tidyr::pivot_wider(names_from = "patches", values_from = "value") %>%
        sf::st_join(spatial_grid %>%
                      sf::st_drop_geometry() %>%
                      dplyr::mutate(geometry = sf::st_geometry(spatial_grid)) %>%
                      sf::st_set_geometry(., "geometry") %>%
                      dplyr::select(geometry), ., largest = TRUE) %>%
        dplyr::select(colnames(.)[grepl("patches", colnames(.))])
    })
  }
  return(feature)
}
