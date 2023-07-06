create_boundary_matrix <- function(planning_grid, seamount_pu_ids){

  pu_grid_sf <- rasterToPolygons(planning_raster_data, na.rm=FALSE) %>%
    sf::st_as_sf() %>%
    dplyr::select(-1)

  pu_sf <-
    pu_grid_sf %>%
    bind_rows(
      lapply(seamount_pu_ids, function(i) {
        st_sf(layer = 1, geometry = st_union(pu_grid_sf[i, , drop = FALSE]))
      }) %>%
        do.call(what = bind_rows) %>%
        dplyr::select(-layer)
    )

  return(boundary_matrix(pu_sf, str_tree = TRUE))
}
