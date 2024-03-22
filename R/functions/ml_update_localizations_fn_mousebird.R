## Update localizations using multilateration
ml_update_localizations_fn_mousebird <- function(band_f = as.character(),
                                                 db_name = as.character(),
                                                 db_password = as.character(),
                                                 tag_folder = as.character(),
                                                 node_folder = as.character(),
                                                 output_folder = as.character(),
                                                 log_dist_RSSI_mdl = as.character(),
                                                 grid_points_folder = as.character(), 
                                                 tz = "UTC",
                                                 ss_filter,
                                                 crs = as.numeric(),
                                                 reps = as.numeric()){
  
  
  # # Collect data for  each tag
  # collect_raw_data_fn_mousebird(band_f = band_f,
  #                               db_name = db_name,
  #                               db_password = db_password,
  #                               tag_folder = tag_folder,
  #                               node_folder = node_folder,
  #                               output_folder = output_folder,
  #                               ss_filter = ss_filter,
  #                               tz = tz)
  # 
  ## Read in raw data
  dets_t <- readRDS(paste0(output_folder,"/raw_detections/data/",band_f,".RDS"))

  ## Get most recently prepared data file (if it exists)
  mrdf <- rev(list.files(paste0(output_folder,"/ml_prepared/",band_f,""),full.names = TRUE, pattern = ".csv.gz"))[1]

  ## Get date time to filter by
  if(!is.na(mrdf)){
    mrd <- suppressWarnings(readr::read_csv(mrdf,show_col_types = FALSE) %>%
                              dplyr::mutate(dt_r = lubridate::force_tz(dt_r, tz =tz)) %>% 
                              dplyr::pull(dt_r) %>%
                              max())
    mrd <- lubridate::with_tz(mrd, tz = tz)
  } else{
    mrd <- as.Date("2021-08-01")
  }

  ## Filter detections based on prepared data
  dets_t <- dets_t %>%
    dplyr::filter(date_time >= mrd)
  
  ## Get grid points
  grid_points <- get_grid_points_fn_mousebird(grid_points_folder,
                                              crs)

  ## If new data to prepare:
  if(nrow(dets_t) > 0){

    ## Prepare tag
    ml_prepare_dets_error_fn(band_f = band_f,
                             dets_t = dets_t,
                             grid_points = grid_points,
                             output_folder = output_folder,
                             tz = tz)

  } 
  # else{
  #   
  #   cat("############ \n",
  #       "No new data for tag: ", band_f, "\n",
  #       "############ \n", sep = "")
  #   
  # }
  # 
  # 
  
  # ## Then localize
  # ml_localize_dets_error_fn_mousebird(band_f = band_f,
  #                                     output_folder = output_folder,
  #                                     grid_points = grid_points,
  #                                     log_dist_RSSI_mdl = log_dist_RSSI_mdl,
  #                                     tz = tz,
  #                                     crs = crs,
  #                                     rep = reps,
  #                                     dist_cutoff = 200)
  # 
  
}
