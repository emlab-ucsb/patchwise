create_df <- function(planning_grid, features, patches, costs, locked_out, locked_in){

  # Add repeated errors for planning_grid (present in nearly all functions)
  if(!is.null(planning_grid) & !(class(planning_grid)[1] %in% c("RasterLayer", "SpatRaster", "sf"))) {
    stop("planning_grid must be a raster or sf object")}

  # Make sure all the features are in the same format
  if(!(identical(class(planning_grid), class(features)) &
       identical(class(features), class(patches)))) {
    stop("features, patches, costs, locked out, and locked in objects must be of the same class")
  }

  # Find which optional objects were supplied, an make sure they are in the right format
  supplied <- NULL
  for(options in c("costs", "locked_out", "locked_in")) {
    if(!is.null(eval(as.name(options)))) {
      supplied <- c(supplied, options)
      if(!(identical(class(planning_grid), class(eval(as.name(options)))))) {
        stop("features, patches, costs, locked out, and locked in objects must be of the same class")
        }
      }
  }
  rm(options)

  if (class(planning_grid)[1] %in% c("RasterLayer", "SpatRaster")) {
    # Initialize
    pu_grid_data <- tibble::tibble(id = as.list(seq_len(nrow(terra::as.data.frame(planning_grid))))) %>%
      dplyr::bind_cols(tibble::tibble(terra::as.data.frame(features))) %>%
      dplyr::mutate(patch = 0)

    if(!is.null(supplied)) {
      for(options in supplied) {
        pu_grid_data <- pu_grid_data %>%
          dplyr::bind_cols(tibble::tibble(terra::as.data.frame(eval(as.name(options)))))
        }
      }

    # Create planning units at the patch level
    pu_sm_data <- lapply(names(patches), function(i) {
      curr_sm_pu <-
        tibble::tibble(
          id = list(as.numeric(row.names(terra::as.data.frame(patches[[i]]))[which(terra::as.data.frame(patches[[i]]) > 0.5)]))) %>%
        # dplyr::bind_cols(
        #   terra::as.data.frame(costs * patches[[i]]) %>%
        #     setNames(names(costs)) %>%
        #     dplyr::summarize_all(sum, na.rm=TRUE)
        # ) %>%
        dplyr::bind_cols(
          terra::as.data.frame(features * patches[[i]]) %>%
            setNames(names(features)) %>%
            dplyr::summarize_all(sum, na.rm = TRUE)
        )

      if(!is.null(supplied)) {
        for(options in supplied) {
          if(grepl("locked", options)) {
            curr_sm_pu <- curr_sm_pu %>%
              dplyr::bind_cols(
                terra::as.data.frame(eval(as.name(options)) * patches[[i]]) %>%
                  sum(na.rm = TRUE) %>%
                  {data.frame(options = ifelse(. > 0, 1, 0))})
          } else {
          curr_sm_pu <- curr_sm_pu %>%
            dplyr::bind_cols(
              terra::as.data.frame(eval(as.name(options)) * patches[[i]]) %>%
                setNames(names(eval(as.name(options)))) %>%
                dplyr::summarize_all(sum, na.rm=TRUE))
          }
        }
      }

        # dplyr::bind_cols(
        #   terra::as.data.frame(locked_out * patches[[i]]) %>%
        #     sum(na.rm = TRUE) %>%
        #     {data.frame(locked_out = ifelse(. > 0, 1, 0))}
        # ) %>%
        # dplyr::bind_cols(
        #   terra::as.data.frame(locked_in * patches[[i]]) %>%
        #     sum(na.rm = TRUE) %>%
        #     {data.frame(locked_in = ifelse(. > 0, 1, 0))}

      curr_sm_pu <- curr_sm_pu %>%
        dplyr::mutate(
          patch = sum(terra::values(patches[[i]]), na.rm = T)
        )
      curr_sm_pu
    }
    ) %>%
      do.call(what = dplyr::bind_rows) %>% tibble::as_tibble()

    if("locked_out" %in% supplied) {
      pu_sm_data <-  pu_sm_data %>%
        dplyr::relocate(locked_out, .after = last_col())
    }
    if("locked_in" %in% supplied) {
      pu_sm_data <-  pu_sm_data %>%
        dplyr::relocate(locked_in, .after = last_col())
    }

  } else {
    # Drop all geometries
    costs <- costs %>% sf::st_drop_geometry()
    features <- features %>% sf::st_drop_geometry()
    locked_out <- locked_out %>% sf::st_drop_geometry()
    locked_in <- locked_in %>% sf::st_drop_geometry()

    # Initialize
    pu_grid_data <- tibble::tibble(id = as.list(seq_len(nrow(planning_grid)))) %>%
      dplyr::bind_cols(costs) %>%
      dplyr::bind_cols(features) %>%
      dplyr::mutate(patch = 0) %>%
      dplyr::bind_cols(locked_out) %>%
      dplyr::bind_cols(locked_in)

    patches <- patches %>% sf::st_drop_geometry()

    # Create planning units at the patch level
    pu_sm_data <- lapply(names(patches), function(i) {
      curr_sm_pu <-
        tibble::tibble(
          id = list(as.numeric(row.names(patches[which(patches[,i] > 0.5),])))) %>%
        dplyr::bind_cols(data.frame(t(costs %>% dplyr::mutate_all(., ~(.*patches[,i])) %>%
                                        colSums(., na.rm = T)))) %>%
        dplyr::bind_cols(data.frame(t(features %>% dplyr::mutate_all(., ~(.*patches[,i])) %>%
                                        colSums(., na.rm = T)))) %>%
        dplyr::bind_cols(data.frame(t(locked_out %>% dplyr::mutate_all(., ~(.*patches[,i])) %>%
                                        colSums(., na.rm = T)))) %>%
                    # dplyr::mutate_all(., ~(ifelse(. > 0, 1, 0)))) %>%
        dplyr::bind_cols(data.frame(t(locked_in %>% dplyr::mutate_all(., ~(.*patches[,i])) %>%
                                        colSums(., na.rm = T)))) %>%
                    # dplyr::mutate_all(., ~(ifelse(. > 0, 1, 0)))) %>%
        dplyr::mutate(
          patch = sum(patches[,i], na.rm = T)
        )
      curr_sm_pu
    }
    ) %>%
      do.call(what = dplyr::bind_rows) %>% tibble::as_tibble() %>%
      dplyr::relocate(locked_out, .after = last_col()) %>%
      dplyr::relocate(locked_in, .after = last_col())
  }

  # Merge patch-level data and grid-cell level data together
  pu_data <-  dplyr::bind_rows(pu_grid_data, pu_sm_data)
  # Constraints so the solution doesn't select overlapping grid cell and patch level planning units
  constraints <- data.frame(test  = rep(0,nrow(pu_data)))

  index <- 1

  #Create a vector with a '1' for each combination of patch-level and grid-cell level planning unit that overlaps with that patch.
  for (i in seq_len(nrow(pu_sm_data))) {
    print(paste0("Processing patch ", i, " of ", nrow(pu_sm_data)))

    for (j in pu_sm_data$id[[i]]) {

      ### specify planning unit indices for constraints
      v <- rep(0, nrow(pu_data))      # initialize with zeros
      v[nrow(pu_grid_data) + i] <- 1  # specify seamount-level planning unit
      v[as.numeric(j)] <- 1                       # specify grid cell-level planning unit

      #store the constraints vectors in a list
      #constraints_list[[index]] <- v
      constraints <- cbind(constraints, v)
      index <- index+1
    }
  }

  #make tibble with one columns for each constraint vector
  #constraints <- bind_cols(constraints_list)

  #remove dummy row
  constraints <- constraints[, -1]

  #################################################################

  #add the constraints as features to the planning data

  pu_data_final <- constraints %>%
    setNames(sprintf("constraint_%d", seq.int(1:ncol(.)))) %>%
    dplyr::bind_cols(pu_data, .)

  return(pu_data_final)

  ################################################################
}
