test_that("create patches from seamount data - raster", {
  expect_s4_class(
    {terra::rast(system.file("extdata/seamounts.tif", package = "patchwise")) |>
        create_patches(seamounts)},
    class = "SpatRaster")
})

test_that("create patches from seamount data - sf", {
  expect_s3_class(
    {pu_sf <- terra::rast(system.file("extdata/pu_raster.tif", package = "patchwise"))|>
      terra::as.polygons(aggregate = FALSE) |>
      sf::st_as_sf()

      seamounts_sf <- terra::rast(system.file("extdata/seamounts.tif", package = "patchwise")) |>
        terra::as.polygons(aggregate = FALSE, na.rm = FALSE) |>
        sf::st_as_sf()

    #replace NAs with zeroes
    seamounts_sf[is.na(seamounts_sf$Seamounts), "Seamounts"] <- 0

    create_patches(seamounts_sf, spatial_grid = pu_sf)},
    class = "sf")
})
