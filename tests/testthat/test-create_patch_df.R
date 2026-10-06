test_that("convert seamount data to df - raster", {
  expect_s3_class(
    # Choose area of interest (Bermuda EEZ)
    {pu_raster <- terra::rast(system.file("extdata/pu_raster.tif", package = "patchwise"))

     species_distributions <- terra::rast(system.file("extdata/spp_distributions.tif", package = "patchwise"))

     seamounts <- terra::rast(system.file("extdata/seamounts.tif", package = "patchwise"))

     patches_raster <- create_patches(seamounts)

     suppressMessages(create_patch_df(spatial_grid = pu_raster, features = species_distributions,
                                      patches = patches_raster, costs = pu_raster))},
    class = "tbl_df")
})

test_that("convert seamount data to df - sf", {
  expect_s3_class(
    {pu_raster <- terra::rast(system.file("extdata/pu_raster.tif", package = "patchwise"))

    species_distributions <- terra::rast(system.file("extdata/spp_distributions.tif", package = "patchwise"))

    seamounts <- terra::rast(system.file("extdata/seamounts.tif", package = "patchwise"))

      pu_sf <- terra::as.polygons(pu_raster, aggregate = FALSE) |>
        sf::st_as_sf()

      features_sf <- terra::as.polygons(species_distributions, aggregate = FALSE) |>
        sf::st_as_sf()

      seamounts_sf <- terra::as.polygons(seamounts, aggregate = FALSE, na.rm = FALSE) |>
        sf::st_as_sf()

      seamounts_sf[is.na(seamounts_sf$Seamounts), "Seamounts"] <- 0

      patches_sf <- create_patches(seamounts_sf, spatial_grid = pu_sf)

      suppressMessages(create_patch_df(spatial_grid = pu_sf, features = features_sf,
                                       patches = patches_sf, costs = pu_sf))},
    class = "tbl_df")
})
