test_that("create boundary matrix - raster", {
  expect_s4_class(
    {pu_raster <- terra::rast(system.file("extdata/pu_raster.tif", package = "patchwise"))

    species_distributions <- terra::rast(system.file("extdata/spp_distributions.tif", package = "patchwise"))

    seamounts <- terra::rast(system.file("extdata/seamounts.tif", package = "patchwise"))

    patches_raster <- create_patches(seamounts)

    patches_df <- suppressMessages(create_patch_df(spatial_grid = pu_raster, features = species_distributions,
                                                   patches = patches_raster, costs = pu_raster))

    create_boundary_matrix(spatial_grid = pu_raster, patches = patches_raster, patch_df = patches_df)},
    class = "Matrix")
})

test_that("create boundary matrix - sf", {
  expect_s4_class(
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

    patches_df <- suppressMessages(create_patch_df(spatial_grid = pu_sf, features = features_sf,
                                                   patches = patches_sf, costs = pu_sf))

    create_boundary_matrix(spatial_grid = pu_sf, patches = patches_sf, patch_df = patches_df)},
    class = "Matrix")
})
