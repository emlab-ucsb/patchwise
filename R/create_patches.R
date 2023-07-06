create_patches <- function(feature, planning_grid) {
  # Add error for incorrect format of feature
  if(!is.null(feature) & !(class(feature)[1] %in% c("RasterLayer", "SpatRaster", "sf"))) {
    stop("feature must be a raster or sf object")}

  if(class(feature)[1] %in% c("RasterLayer", "SpatRaster")) {
    feature <- feature %>%
      as("SpatRaster") %>%
      terra::patches() %>%
      terra::segregate(other = NA) %>%
      setNames(paste0("patches_", seq_len(terra::nlyr(.))))
  } else {
    feature <- feature %>%
      sf::st_cast("POLYGON", warn = FALSE) %>%
      dplyr::mutate(value = 1,
                    patches = paste0("patches_", 1:nrow(.))) %>%
      tidyr::pivot_wider(names_from = "patches", values_from = "value") %>%
      dplyr::select(colnames(.)[grepl("patches", colnames(.))]) %>%
      sf::st_join(planning_grid, .)}

  return(feature)
}
