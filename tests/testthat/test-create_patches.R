test_that("create patches from seamount data - raster", {
  expect_s4_class(
    # Choose area of interest (Bermuda EEZ)
    {area <- offshoredatr::get_area(area_name = "Bermuda")
    projection <- 'PROJCS["ProjWiz_Custom_Lambert_Azimuthal", GEOGCS["GCS_WGS_1984", DATUM["D_WGS_1984", SPHEROID["WGS_1984",6378137.0,298.257223563]], PRIMEM["Greenwich",0.0], UNIT["Degree",0.0174532925199433]], PROJECTION["Lambert_Azimuthal_Equal_Area"], PARAMETER["False_Easting",0.0], PARAMETER["False_Northing",0.0], PARAMETER["Central_Meridian",-64.5], PARAMETER["Latitude_Of_Origin",32], UNIT["Meter",1.0]]'

    # Create a planning grid
    planning_rast <- offshoredatr::get_planning_grid(area, projection = projection)

    # Grab all relevant data
    features_rast <- suppressMessages(offshoredatr::get_features(area, planning_rast))

    # Separate seamount data - we want to protect entire patches
    seamounts_rast <- features_rast[["seamounts"]]
    patchwise::create_patches(seamounts_rast)
    },
    class = "SpatRaster")
})

test_that("create patches from seamount data - sf", {
  expect_s3_class(
    # Choose area of interest (Bermuda EEZ)
    {area <- offshoredatr::get_area(area_name = "Bermuda")
    projection <- 'PROJCS["ProjWiz_Custom_Lambert_Azimuthal", GEOGCS["GCS_WGS_1984", DATUM["D_WGS_1984", SPHEROID["WGS_1984",6378137.0,298.257223563]], PRIMEM["Greenwich",0.0], UNIT["Degree",0.0174532925199433]], PROJECTION["Lambert_Azimuthal_Equal_Area"], PARAMETER["False_Easting",0.0], PARAMETER["False_Northing",0.0], PARAMETER["Central_Meridian",-64.5], PARAMETER["Latitude_Of_Origin",32], UNIT["Meter",1.0]]'

    # Create a planning grid
    planning_sf <- offshoredatr::get_planning_grid(area, projection = projection, option = "sf_square")

    # Grab all relevant data
    features_sf <- suppressMessages(offshoredatr::get_features(area, planning_sf))

    # Separate seamount data - we want to protect entire patches
    seamounts_sf <- features_sf %>% dplyr::select(seamounts)

    patchwise::create_patches(seamounts_sf, planning_grid = planning_sf %>% dplyr::rename(geometry = x))
    },
    class = "sf")
})
