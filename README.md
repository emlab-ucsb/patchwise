# patchwise

`patchwise` is intended to be used as a supplementary package to `offshoredatr` for instances in which users wish to protect entire "chunks" of areas using `prioritizr`. 
One example is when a user wishes to include seamounts as a feature to protect in `prioritizr` with a target of 20%. Instead of protecting a little bit of each seamount until the 20% is reached, `patchwise` makes it easy to ensure that entire seamounts are protected sequentially to meet the protection target.

## Installation
You can install the development version of `patchwise` from GitHub with:

```
if (!require(devtools)) install.packages("devtools")
devtools::install_github("emlab-ucsb/patchwise")
```

You can install `offshoredatr` from GitHuv with: 

```
if (!require(devtools)) install.packages("devtools")
devtools::install_github("emlab-ucsb/offshoredatr")
```

For our examples, we use the prioritization optimizer `gurobi`, which can be found [here](https://www.gurobi.com/solutions/gurobi-optimizer/?campaignid=193283256&adgroupid=138872523240&creative=596136082788&keyword=gurobi%20optimization&matchtype=e&gclid=CjwKCAjwtuOlBhBREiwA7agf1oUW5qsO9aXGpfbjy04XRAw0DRpVGdSlrnEYRyC2q-B9EafXdArQUhoCDxQQAvD_BwE). Gurobi must be installed on your device and the `gurobi` R package must also be installed in R before you can run `prioritizr`.

## Example of usage

Since this package is intended to be used in combination with `offshoredatr`, there are several housekeeping steps that need to be completed first. Note that even though the example uses rasters, the functions can work with `sf` objects as well.

```
# Load libraries
library(offshoredatr) # not entirely needed here, since we call to the package directly
library(patchwise) # also not entirely needed here, since we call to the package directly
library(prioritizr) # required due to dependencies with gurobi

# Choose area of interest (Bermuda EEZ)
area <- offshoredatr::get_area(area_name = "Bermuda")
projection <- 'PROJCS["ProjWiz_Custom_Lambert_Azimuthal", GEOGCS["GCS_WGS_1984", DATUM["D_WGS_1984", SPHEROID["WGS_1984",6378137.0,298.257223563]], PRIMEM["Greenwich",0.0], UNIT["Degree",0.0174532925199433]], PROJECTION["Lambert_Azimuthal_Equal_Area"], PARAMETER["False_Easting",0.0], PARAMETER["False_Northing",0.0], PARAMETER["Central_Meridian",-64.5], PARAMETER["Latitude_Of_Origin",32], UNIT["Meter",1.0]]'

# Create a planning grid
planning_rast <- offshoredatr::get_planning_grid(area, projection = projection)

# Grab all relevant data
features_rast <- offshoredatr::get_features(area, planning_rast)

# Create a "cost" to protecting a cell - just a uniform cost for this example
cost_rast <- setNames(planning_rast, "cost")

# Separate seamount data - we want to protect entire patches
seamounts_rast <- features_rast[["seamounts"]]
features_rast <- features_rast[[names(features_rast)[names(features_rast) != "seamounts"]]]

# Show what seamounts look like... 
terra::plot(seamounts_rast) # there are 7 seamount areas (seamounts that are touching)
```

![image](https://github.com/echelleburns/patchwise/assets/40546424/7e717d95-4673-4dac-8f59-dbb4865398ea)

```
# Create seamount patches - seamount areas that touch are considered the same patch
patches_rast <- patchwise::create_patches(seamounts_rast)

# Create patches dataframe - this creates several constraints so that entire seamount units are protected together
patches_df_rast <- patchwise::create_patch_df(planning_grid = planning_rast, features = features_rast, patches = patches_rast, costs = cost_rast)

# Create boundary matrix for prioritizr
boundary_matrix_rast <- patchwise::create_boundary_matrix(planning_grid = planning_rast, patches = patches_rast, patch_df = patches_df_rast)

# Create targets for protection - let's just do 20% for each feature (including 20% of whole seamounts)
targets_rast <- patchwise::features_targets(targets = rep(0.2, (terra::nlyr(features_rast) + 1)), features = features_rast, pre_patches = seamounts_rast)

# Add these targets to targets for protection for the "constraints" we introduced to protect entire seamount units
constraints_rast <- patchwise::constraints_targets(feature_targets = targets_rast, patch_df = patches_df_rast)

# Run the prioritization
problem_rast <- prioritizr::problem(x = patches_df_rast, features = constraints_rast$feature, cost_column = "cost") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_manual_targets(constraints_rast) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_boundary_penalties(penalty = 0.000002, data = boundary_matrix_rast) %>%
  prioritizr::add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1)

# Solve the prioritization
solution <- solve(problem_rast)

# Convert the prioritization into a more digestible format
result <- patchwise::convert_solution(solution = solution, patch_df = patches_df_rast, planning_grid = planning_rast)

# Show the results
terra::plot(result)
```

![image](https://github.com/echelleburns/patchwise/assets/40546424/8d64a8a0-375e-420a-9bb6-f5b4158e068c)

Areas in green were identified by `prioritizr` as areas worth protecting. We can see that entire seamounts were used to meet the target objective of protecting 20% of seamounts. We can compare this result to a `prioritizr` run that does not protect whole seamounts: 

```
# Grab all relevant data
features_rast <- offshoredatr::get_features(area, planning_rast)

# Run the prioritization
problem_rast <- prioritizr::problem(x = cost_rast, features = features_rast) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(rep(0.2, terra::nlyr(features_rast))) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_boundary_penalties(penalty = 0.000002) %>%
  prioritizr::add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1)

# Solve the prioritization
solution <- solve(problem_rast)

# Show the results
terra::plot(result)
```
![image](https://github.com/echelleburns/patchwise/assets/40546424/a77660ee-aa90-437e-9714-49243d5c7950)

Only portions of seamount units are protected here.
