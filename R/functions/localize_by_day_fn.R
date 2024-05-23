## Update localizations using multilateration
localize_by_day_fn <- function(file_f = as.character(),
                               output_folder = as.character(),
                               log_dist_RSSI_mdl = as.character(),
                               grid_points_folder = as.character(),
                               failure_date_file = NA,
                               tz = as.character(),
                               crs = as.numeric(),
                               reps = as.numeric()){
  
  
  ## Get grid points
  grid_points <- get_grid_points_fn_mousebird(grid_points_folder,
                                              crs)
  
  ## Then localize
  ml_localize_dets_error_fn(file_f = file_f,
                            output_folder = output_folder,
                            grid_points = grid_points,
                            log_dist_RSSI_mdl = log_dist_RSSI_mdl,
                            failure_date_file = failure_date_file,
                            tz = tz,
                            crs = crs,
                            rep = reps,
                            dist_cutoff = 200)
  
  
}
