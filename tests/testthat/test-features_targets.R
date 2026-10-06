test_that("create features targets - raster", {
  expect_s3_class(
    {species_distributions <- terra::rast(system.file("extdata/spp_distributions.tif", package = "patchwise"))

    seamounts <- terra::rast(system.file("extdata/seamounts.tif", package = "patchwise"))

    features_targets(targets = rep(0.2, (terra::nlyr(species_distributions) + 1)),
                     features = species_distributions, pre_patches = seamounts)},
    class = "tbl_df")
})

test_that("create features targets - sf", {
  expect_s3_class(
    {species_distributions <- terra::rast(system.file("extdata/spp_distributions.tif", package = "patchwise"))

    seamounts <- terra::rast(system.file("extdata/seamounts.tif", package = "patchwise"))

    features_sf <- terra::as.polygons(species_distributions, aggregate = FALSE) |>
      sf::st_as_sf()

    seamounts_sf <- terra::as.polygons(seamounts, aggregate = FALSE, na.rm = FALSE) |>
      sf::st_as_sf()

    seamounts_sf[is.na(seamounts_sf$Seamounts), "Seamounts"] <- 0

    features_targets(targets = rep(0.2, ncol(features_sf)), features = features_sf, pre_patches = seamounts_sf)},
    class = "tbl_df")
})

