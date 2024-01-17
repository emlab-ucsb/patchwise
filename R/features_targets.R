#' Create a dataframe for the relative and absolute targets for features
#'
#' @description This function creates a dataframe of targets for each of the features to be used in the prioritization
#'
#' @param targets a vector of targets for protection (range between 0 and 1); must be the same length as the number of features + the pre-patches variable
#' @param features a raster or sf object that includes all relevant features to be used in the prioritization; each layer of the raster or each column of the sf object identifies the location of each feature
#' @param pre_patches a raster or sf object that includes the feature that is to be split into patches (not the layer that is already split into patches)
#' @param locked_out a raster or sf object for areas to be locked out (absolutely not protected) in the prioritization
#' @param locked_in a raster or sf object for areas to be locked in (absolutely protected) in the prioritization
#'
#' @return A data frame to be used to specify targets for features. Will need to be plugged into `constraint_targets()` before being implemented into prioritizr
#' @export
#'
#' @examples
#' ## To be updated later
#' # Create target features - using just 20% for every feature
#' features_targets <- features_targets(targets = rep(0.2, (terra::nlyr(features_raster)) + 1), features = features_raster, pre_patches = seamounts_raster)

features_targets <- function(targets, features, pre_patches, locked_out = NULL, locked_in = NULL){

  if(class(features)[1] %in% c("RasterLayer", "SpatRaster")){
    feature_targets_df <- c(features, setNames(pre_patches, "patch")) %>%
      terra::values(.)
    } else {
      feature_targets_df <- features %>%
        sf::st_drop_geometry() %>%
        dplyr::bind_cols(pre_patches %>% sf::st_drop_geometry() %>%
                           dplyr::rename(patch = 1))
    }

  feature_targets_df <- feature_targets_df %>%
    colSums(., na.rm = TRUE) %>%
    {tibble::tibble(id = seq_along(.), name = names(.), total = unname(.))} %>%
    dplyr::mutate(relative_target = targets) %>%
    dplyr::mutate(absolute_target = relative_target * total)

  # Adding locked in/out separately so that we can adjust absolute targets
   if(!is.null(locked_out)) {
     feature_targets_df <- feature_targets_df %>%
     dplyr::bind_rows(tibble::tibble(id = nrow(.)+1,
                      name = "locked_out",
                      total = ifelse(class(locked_out)[1] %in% c("RasterLayer", "SpatRaster"),
                                     sum(terra::values(locked_out), na.rm = TRUE),
                                     sum((locked_out %>% sf::st_drop_geometry())[,1], na.rm = TRUE)),
                      relative_target = NA_real_,
                      absolute_target = 0.5))
   }

   if(!is.null(locked_in)) {
     feature_targets_df <- feature_targets_df %>%
       dplyr::bind_rows(tibble::tibble(id = nrow(.)+1,
                                       name = "locked_in",
                                       total = ifelse(class(locked_in)[1] %in% c("RasterLayer", "SpatRaster"),
                                         sum(terra::values(locked_in), na.rm = TRUE),
                                         sum((locked_in %>% sf::st_drop_geometry())[,1], na.rm = TRUE)),
                                       relative_target = 1) %>%
                          dplyr::mutate(absolute_target = total))
   }

  return(feature_targets_df)

}