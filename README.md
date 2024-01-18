# patchwise

`patchwise` is intended to be used as a supplementary package to `oceandatr` for instances in which users wish to protect entire "chunks" of areas using `prioritizr`. 
One example is when a user wishes to include seamounts as a feature to protect in `prioritizr` with a target of 20% protection. Instead of protecting a little bit of each seamount until the 20% is reached, `patchwise` makes it easy to ensure that entire seamounts are protected sequentially to meet the protection target.

## Installation
You can install the development version of `patchwise` from GitHub with:

```
if (!require(devtools)) install.packages("devtools")
devtools::install_github("emlab-ucsb/patchwise")
```

You can install `oceandatr` from GitHub with: 

```
if (!require(devtools)) install.packages("devtools")
devtools::install_github("emlab-ucsb/oceandatr")
```

For our examples, we use the prioritization optimizer `gurobi`, which can be found [here](https://www.gurobi.com/solutions/gurobi-optimizer/?campaignid=193283256&adgroupid=138872523240&creative=596136082788&keyword=gurobi%20optimization&matchtype=e&gclid=CjwKCAjwtuOlBhBREiwA7agf1oUW5qsO9aXGpfbjy04XRAw0DRpVGdSlrnEYRyC2q-B9EafXdArQUhoCDxQQAvD_BwE). Gurobi must be installed on your device and the `gurobi` R package must also be installed in R before you can run `prioritizr`.

## Examples of usage

### Using `raster` objects as inputs

Since this package is intended to be used in combination with `oceandatr`, there are several housekeeping steps that need to be completed first. 

```
# Choose area of interest (Bermuda EEZ)
area <- oceandatr::get_area(area_name = "Bermuda")
projection <- '+proj=laea +lon_0=-64.8108333 +lat_0=32.3571917 +datum=WGS84 +units=m +no_defs'

# Create a planning grid
planning_rast <- oceandatr::get_planning_grid(area, projection = projection)

# Grab all relevant data
features_rast <- oceandatr::get_features(planning_grid = planning_rast)

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
solution_rast <- solve(problem_rast)

# Convert the prioritization into a more digestible format
result_rast <- patchwise::convert_solution(solution = solution_rast, patch_df = patches_df_rast, planning_grid = planning_rast)

# Show the results
terra::plot(result_rast)
```

![image](https://github.com/emlab-ucsb/patchwise/assets/40546424/82d88030-e915-4fde-a3ec-a99b62ff596d)


Areas in green were identified by `prioritizr` as areas worth protecting. We can see that entire seamounts were used to meet the target objective of protecting 20% of seamounts. We can compare this result to a `prioritizr` run that does not protect whole seamounts: 

```
# Grab all relevant data
features_rast_nopatch <- oceandatr::get_features(planning_grid = planning_rast)

# Run the prioritization
problem_rast_nopatch <- prioritizr::problem(x = cost_rast, features = features_rast_nopatch) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(rep(0.2, terra::nlyr(features_rast_nopatch))) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_boundary_penalties(penalty = 0.000002) %>%
  prioritizr::add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1)

# Solve the prioritization
solution_nopatch <- solve(problem_rast_nopatch)

# Show the results
terra::plot(solution_nopatch)
```

![image](https://github.com/emlab-ucsb/patchwise/assets/40546424/0d27f6df-32e4-49ff-abbc-720b930c26d4)

Only portions of seamount units are protected here.

### Using `sf` objects as inputs

Since this package is intended to be used in combination with `oceandatr`, there are several housekeeping steps that need to be completed first. 

```
# Choose area of interest (Bermuda EEZ)
area <- oceandatr::get_area(area_name = "Bermuda")
projection <- '+proj=laea +lon_0=-64.8108333 +lat_0=32.3571917 +datum=WGS84 +units=m +no_defs'

# Create a planning grid
planning_sf <- oceandatr::get_planning_grid(area, projection = projection, option = "sf_square")

# Grab all relevant data
features_sf <- oceandatr::get_features(planning_grid = planning_sf)

# Create a "cost" to protecting a cell - just a uniform cost for this example
cost_sf <- features_sf %>%
  dplyr::mutate(cost = 1) %>%
  dplyr::select(cost)

# Separate seamount data - we want to protect entire patches
seamounts_sf <- features_sf %>% 
  dplyr::select(seamounts)

features_sf <- features_sf %>% 
  dplyr::select(-seamounts)

# Show what seamounts look like... 
plot(seamounts_sf, border = F) # there are 7 seamount areas (seamounts that are touching)
```

![image](https://github.com/emlab-ucsb/patchwise/assets/40546424/892e0ee5-6791-4fb7-acac-6e3fcb8718b0)

```
# Create seamount patches - seamount areas that touch are considered the same patch
patches_sf <- patchwise::create_patches(seamounts_sf, planning_grid = planning_sf)

# Create patches dataframe - this creates several constraints so that entire seamount units are protected together
patches_df_sf <- patchwise::create_patch_df(planning_grid = planning_sf, features = features_sf, patches = patches_sf, costs = cost_sf)

# Create boundary matrix for prioritizr
boundary_matrix_sf <- patchwise::create_boundary_matrix(planning_grid = planning_sf, patches = patches_sf, patch_df = patches_df_sf)

# Create targets for protection - let's just do 20% for each feature (including 20% of whole seamounts)
targets_sf <- patchwise::features_targets(targets = rep(0.2, ncol(features_sf)), features = features_sf, pre_patches = seamounts_sf)

# Add these targets to targets for protection for the "constraints" we introduced to protect entire seamount units
constraints_sf <- patchwise::constraints_targets(feature_targets = targets_sf, patch_df = patches_df_sf)

# Run the prioritization
problem_sf <- prioritizr::problem(x = patches_df_sf, features = constraints_sf$feature, cost_column = "cost") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_manual_targets(constraints_sf) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_boundary_penalties(penalty = 0.000002, data = boundary_matrix_sf) %>%
  prioritizr::add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1)

# Solve the prioritization
solution_sf <- solve(problem_sf)

# Convert the prioritization into a more digestible format
result_sf <- patchwise::convert_solution(solution = solution_sf, patch_df = patches_df_sf, planning_grid = planning_sf)

# Show the results
plot(result_sf, border = F)
```

![image](https://github.com/emlab-ucsb/patchwise/assets/40546424/4fe72641-1693-43f5-a317-b86d30e7c54c)

Areas in yellow were identified by `prioritizr` as areas worth protecting. We can see that entire seamounts were used to meet the target objective of protecting 20% of seamounts. We can compare this result to a `prioritizr` run that does not protect whole seamounts: 

```
# Grab all relevant data
features_sf_nopatch <- oceandatr::get_features(planning_grid = planning_sf) %>% 
  dplyr::mutate(cost = 1) %>% # create a cost column
  dplyr::relocate(cost, .before = x) # make sure cost column is before geometry column

# Run the prioritization
problem_sf_nopatch <- prioritizr::problem(x = features_sf_nopatch, features = names(features_sf_nopatch)[1:(ncol(features_sf_nopatch)-1)], cost_column = "cost") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(rep(0.2, ncol(features_sf_nopatch)-1)) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_boundary_penalties(penalty = 0.000002) %>%
  prioritizr::add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1)

# Solve the prioritization
solution_sf_nopatch <- solve(problem_sf_nopatch)

# Show the results
plot(solution_sf_nopatch %>% dplyr::select(solution_1), border = F)
```

![image](https://github.com/emlab-ucsb/patchwise/assets/40546424/a4fee456-4206-4668-bd61-76e22b8b0222)

Only portions of seamount units are protected here.
