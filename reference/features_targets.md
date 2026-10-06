# Create a dataframe for the relative and absolute targets for features

This function creates a dataframe of targets for each of the features to
be used in the prioritization

## Usage

``` r
features_targets(
  targets,
  features,
  pre_patches,
  locked_out = NULL,
  locked_in = NULL
)
```

## Arguments

- targets:

  a vector of targets for protection (range between 0 and 1); must be
  the same length as the number of features + the pre-patches variable

- features:

  a raster or sf object that includes all relevant features to be used
  in the prioritization; each layer of the raster or each column of the
  sf object identifies the location of each feature

- pre_patches:

  a raster or sf object that includes the feature that is to be split
  into patches (not the layer that is already split into patches)

- locked_out:

  a raster or sf object for areas to be locked out (absolutely not
  protected) in the prioritization

- locked_in:

  a raster or sf object for areas to be locked in (absolutely protected)
  in the prioritization

## Value

A data frame to be used to specify targets for features. Will need to be
plugged into `constraint_targets()` before being implemented into
prioritizr

## Examples

``` r
# Import some planning data

#import species distributions - feature data for planning
species_distributions <- terra::rast(system.file("extdata/spp_distributions.tif", package = "patchwise"))

#import seamounts
seamounts <- terra::rast(system.file("extdata/seamounts.tif", package = "patchwise"))


# Create targets for protection - use 20% for each feature (including 20% of whole seamounts) in this example
targets_df <- features_targets(targets = rep(0.2, (terra::nlyr(species_distributions) + 1)), features = species_distributions, pre_patches = seamounts)
head(targets_df)
#> # A tibble: 5 × 5
#>      id name   total relative_target absolute_target
#>   <int> <chr>  <dbl>           <dbl>           <dbl>
#> 1     1 fish_1   782             0.2           156. 
#> 2     2 fish_2   790             0.2           158  
#> 3     3 fish_3   758             0.2           152. 
#> 4     4 fish_4   794             0.2           159. 
#> 5     5 patch    156             0.2            31.2
```
