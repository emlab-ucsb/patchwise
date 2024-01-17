test_that("convert seamount data to df - raster", {
  expect_s3_class(
    # Choose area of interest (Bermuda EEZ)
    {area <- oceandatr::get_area(area_name = "Bermuda")
    projection <- 'PROJCS["ProjWiz_Custom_Lambert_Azimuthal", GEOGCS["GCS_WGS_1984", DATUM["D_WGS_1984", SPHEROID["WGS_1984",6378137.0,298.257223563]], PRIMEM["Greenwich",0.0], UNIT["Degree",0.0174532925199433]], PROJECTION["Lambert_Azimuthal_Equal_Area"], PARAMETER["False_Easting",0.0], PARAMETER["False_Northing",0.0], PARAMETER["Central_Meridian",-64.5], PARAMETER["Latitude_Of_Origin",32], UNIT["Meter",1.0]]'

    # Create a planning grid
    planning_rast <- oceandatr::get_planning_grid(area, projection = projection)

    # Grab all relevant data
    features_rast <- suppressMessages(oceandatr::get_features(area, planning_rast))

    # Separate seamount data - we want to protect entire patches
    seamounts_rast <- features_rast[["seamounts"]]
    features_rast <- features_rast[[names(features_rast)[names(features_rast) != "seamounts"]]]

    # Create seamount patches - seamount areas that touch are considered the same patch
    patches_rast <- patchwise::create_patches(seamounts_rast)

    # Create a "cost" to protecting a cell - just a uniform cost for this example
    cost_rast <- setNames(planning_rast, "cost")

    # Create patches dataframe - this creates several constraints so that entire seamount units are protected together
    patchwise::create_patch_df(planning_grid = planning_rast, features = features_rast, patches = patches_rast, costs = cost_rast)
    },
    class = "tbl_df")
})

test_that("convert seamount data to df - sf", {
  expect_s3_class(
    # Choose area of interest (Bermuda EEZ)
    {area <- oceandatr::get_area(area_name = "Bermuda")
    projection <- 'PROJCS["ProjWiz_Custom_Lambert_Azimuthal", GEOGCS["GCS_WGS_1984", DATUM["D_WGS_1984", SPHEROID["WGS_1984",6378137.0,298.257223563]], PRIMEM["Greenwich",0.0], UNIT["Degree",0.0174532925199433]], PROJECTION["Lambert_Azimuthal_Equal_Area"], PARAMETER["False_Easting",0.0], PARAMETER["False_Northing",0.0], PARAMETER["Central_Meridian",-64.5], PARAMETER["Latitude_Of_Origin",32], UNIT["Meter",1.0]]'

    # Create a planning grid
    planning_sf <- oceandatr::get_planning_grid(area, projection = projection, option = "sf_square")

    # Grab all relevant data
    features_sf <- suppressMessages(oceandatr::get_features(area, planning_sf))

    # Separate seamount data - we want to protect entire patches
    seamounts_sf <- features_sf %>% dplyr::select(seamounts)
    features_sf <- features_sf %>% dplyr::select(-seamounts)

    # Create seamount patches - seamount areas that touch are considered the same patch
    patches_sf <- patchwise::create_patches(seamounts_sf, planning_grid = planning_sf %>% dplyr::rename(geometry = x))

    # Add cost layer
    cost_sf <- features_sf %>%
      dplyr::mutate(cost = 1) %>%
      dplyr::select(cost)

    # Create patches dataframe - this creates several constraints so that entire seamount units are protected together
    patchwise::create_patch_df(planning_grid = planning_sf, features = features_sf, patches = patches_sf, costs = cost_sf)
    },
    class = "tbl_df")
})