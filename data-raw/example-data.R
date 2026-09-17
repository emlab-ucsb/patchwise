# Creation of a raster planning grid and some features for use in package examples

# create a 40 x 40 planning grid, with cost values all equal to 1

max_x_and_y <- 40
pu_raster <- terra::rast(nrows = max_x_and_y, ncols = max_x_and_y, xmin= 0, xmax= max_x_and_y, ymin= 0, ymax= max_x_and_y, resolution = 1 , vals = 1) |>
  setNames("cost")

#create a 4 layer raster of random, binary (0 or 1) features
# these could represent distributions of species; for this example they are named fish

number_species <- 4

fish_distributions <- lapply(seq_len(number_species), function(x) {
  terra::setValues(pu_raster, round(runif(terra::ncell(pu_raster)) > 0.5))
}) |>
  setNames(paste0("fish_", seq_len(number_species))) |>
  terra::rast()

# create patches; for this example, they are named seamounts
seamounts <- data.frame(x = c(5, 22, 32),
                        y = c(5, 18, 29)) |>
  terra::vect() |>
  terra::buffer(4) |>
  terra::rasterize(pu_raster, field = 1) |>
  setNames("Seamounts")

terra::writeRaster(pu_raster, "inst/extdata/pu_raster.tif")

terra::writeRaster(fish_distributions, "inst/extdata/spp_distributions.tif")

terra::writeRaster(seamounts, "inst/extdata/seamounts.tif")
