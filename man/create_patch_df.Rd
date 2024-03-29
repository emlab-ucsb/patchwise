% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_patch_df.R
\name{create_patch_df}
\alias{create_patch_df}
\title{Create a dataframe that counts each patch independently for the prioritizr input}
\usage{
create_patch_df(
  spatial_grid,
  features,
  patches,
  costs = NULL,
  locked_out = NULL,
  locked_in = NULL
)
}
\arguments{
\item{spatial_grid}{a raster or sf template with the desired resolution and coordinate reference system generated by \code{spatialgridr::get_grid()}; values in areas of interest are 1, while all other values are NA}

\item{features}{a raster or sf object that includes all relevant features to be used in the prioritization; each layer of the raster or each column of the sf object identifies the location of each feature}

\item{patches}{a raster or sf object generated by \code{create_patches()}; a single feature split into spatially distinct groups; each layer of the raster or each column of the sf object identifies the location of each patch}

\item{costs}{a raster or sf object of the costs for protecting each cell in the planning grid}

\item{locked_out}{a raster or sf object for areas to be locked out (absolutely not protected) in the prioritization}

\item{locked_in}{a raster or sf object for areas to be locked in (absolutely protected) in the prioritization}
}
\value{
A data frame to be used as an input for \code{create_boundary_matrix()}
}
\description{
This function takes all of the inputs of prioritizr and creates a dataframe precursor for prioritizr. This specific function includes the use of "patches" which have been already generated by \code{create_patches()}. "Patches" are spatial groups of a particular feature, such as seamounts. Prioritizr will protect entire patches to meet the protection goal for that feature. The output of this function should be passed to \code{create_boundary_matrix()} before being passed to prioritizr.
}
\examples{
# Start with a little housekeeping to get the data from oceandatr
# Choose area of interest (Bermuda EEZ)
area <- oceandatr::get_area(area_name = "Bermuda",  mregions_column = "territory1")
projection <-'+proj=laea +lon_0=-64.8108333 +lat_0=32.3571917 +datum=WGS84 +units=m +no_defs'
# Create a planning grid
planning_raster <- spatialgridr::get_grid(area, projection = projection)
# Grab all relevant data
features_raster <- oceandatr::get_features(spatial_grid = planning_raster)
# Separate seamount data - we want to protect entire patches
seamounts_raster <- features_raster[["seamounts"]]
features_raster <- features_raster[[names(features_raster)[names(features_raster) != "seamounts"]]]
# Create a "cost" to protecting a cell - just a uniform cost for this example
cost_raster <- stats::setNames(planning_raster, "cost")
# Create patches from layer
patches_raster <- create_patches(seamounts_raster)
# Create patch dataframe
patches_raster_df <- create_patch_df(spatial_grid = planning_raster, features = features_raster,
  patches = patches_raster, costs = cost_raster)
}
