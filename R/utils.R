# A file of little functions that we use across the board.

#' Check if data is a raster
#'
#' @param dat
#'
#' @return `logical` TRUE if raster, else FALSE
#' @noRd
check_raster <- function(dat){
  class(dat)[1] %in% c("RasterLayer", "SpatRaster")
}

#' Check if data is sf
#'
#' @param dat
#'
#' @return TRUE if sf, else FALSE
#' @noRd
check_sf <- function(dat){
  class(dat)[1] == "sf"
}

#' Check if data is df
#'
#' @param dat
#'
#' @return TRUE if df, else FALSE
#' @noRd
check_df <- function(dat){
  is.data.frame(dat)
}

#' Check if the inputs have the same crs
#'
#' @param object1
#' @param object2
#'
#' @return `logical` TRUE crs' match, FALSE if they don't
#' @noRd
check_matching_crs <- function(object1, object2){
  sf::st_crs(object1) == sf::st_crs(object2)
}

#' Check if the inputs have the same type
#'
#' @param object1
#' @param object2
#'
#' @return `logical` TRUE crs' match, FALSE if they don't
#' @noRd
check_matching_type <- function(object1, object2){
  class(object1)[1] == class(object2)[1]
}

