#' Prepare detection data for localizing. This function outputs a csv file for each day for each tag. If a day has already been prepared, these days will be skipped.
#' The output csv has one row for each specified window and columns for each grid point. The window is the total time over which the average is calculated. The lag determines how to shift the window. For example, a lag of 0 and a window of 1 minute would give the average RSSI for the preceeding minute. With a lag of -30 seconds and a 1 minute window, the window is shifted 30 seconds forward and so the average is calculated as based on the 30 seconds before and 30 seconds after the start of the interval.
#' The value in each cell is the moving average (based on the window and lag) of the RSSI for that window and grid point.
#' The dist_filter argument controls the distance filter that is used to retain nodes around the node with the strongest RSSI value within each window. (Larger values increase the number of nodes retained.)
#' 
#' @param processed_dets_file The processed detection file 
#' @param tags Tags to prepare. If NULL, then any tag with detections will be prepared.
#' @param project 
#' @param output_folder The location where the prepared data should go. A new subdirectory called 'prepared_detections' will be created here if necessary as well as subdirectories for each tag.
#' @param grid_point_file The grid point file with coordinates for each grid point.
#' @param window The desired window within which to calculate the average tag RSSI.
#' @param lag The increment over which to calculate the moving window given as a negative value, e.g. '-30 seconds'.
#' @param dist_filter The distance filter (in meters) for retaining nodes around the node with the strongest value in each window.

ml_prepare_dets_error_fn <- function(tag_f, 
                                     dets_t, 
                                     grid_points,
                                     output_folder, 
                                     tz,
                                     tags = NULL,
                                     project = as.character(),
                                     window = "30 secs",
                                     lag = "-15 secs",
                                     dist_filter = 300){
  
  ## Get unique days 
  days <- as.character(unique(dets_t$date))
  
  # ## Set progress bar for preparing tags
  # pb <- txtProgressBar(min = 0, max = length(days), style = 3)
  # 
  ## Files that still need to be processed
  if(length(days) > 0){
    
    cat("\n Tag:", tag_f, "- days to prepare:", length(days), "\n")
    
    for(day_f in days){
      
      # ## Progress bar
      # Sys.sleep(0.1)
      # setTxtProgressBar(pb, which(days == day_f))
      # 
      # day_f = days[1]
      
      day_f_f <- as.Date(day_f, tz = tz)
      
      dets_2_prepare <- dets_t %>% 
        dplyr::filter(date == day_f_f)
      
      ## If any to prepare
      if(nrow(dets_2_prepare) > 0){
        
        cat("\n Starting tag:", tag_f, "- day:", day_f, "- detections to prepare:", nrow(dets_2_prepare), "\n")
        
        ## Prepare filtered records
        fdets_prep <- dets_2_prepare %>%
          dplyr::arrange(date_time) %>% 
          dplyr::group_by(grid_point) %>% 
          dplyr::mutate(dt_r = lubridate::floor_date(date_time,
                                                     unit = lag),
                        mean_RSSI_gp = runner::runner(x = .,
                                                      k = window,
                                                      lag = lag,
                                                      idx = "date_time",
                                                      f = function(x) mean(x$rssi),
                                                      na_pad = FALSE)) %>% 
          dplyr::ungroup() %>% 
          dplyr::select(tag,
                        grid_point,
                        date_time, 
                        dt_r,
                        mean_RSSI_gp)
        
        ## Summarize
        fdets_prep_sum <- fdets_prep %>% 
          dplyr::group_by(grid_point,
                          dt_r) %>% 
          na.omit() %>%
          dplyr::mutate(mean_RSSI_gp = round(mean(mean_RSSI_gp,
                                                  na.rm = T), 0)) %>% 
          dplyr::group_by(dt_r) %>% 
          dplyr::mutate(dets = dplyr::n(),
                        n_gp = dplyr::n_distinct(grid_point)) %>% 
          dplyr::distinct(grid_point,
                          dt_r,
                          .keep_all = T) %>% 
          
          ## Create time index
          dplyr::ungroup() %>%
          dplyr::mutate(t_ind = cumsum(!duplicated(dt_r))) %>% 
          
          ## Rearrange
          dplyr::select(tag, dt_r, t_ind, grid_point, mean_RSSI_gp,dets,n_gp)
        
        ## Grid point with max RSSI for each window
        gp_max_RSSI <- fdets_prep_sum %>% 
          
          ## Get max RSSI values within each window
          dplyr::group_by(t_ind) %>%
          dplyr::mutate(max_RSSI = max(mean_RSSI_gp)) %>%
          dplyr::filter(mean_RSSI_gp == max_RSSI) %>% 
          dplyr::distinct(t_ind,
                          .keep_all = T)
        
        ## Empty data frame
        dt_r_dets_all <- data.frame()
       
        ## For each interval
        for(interval in unique(gp_max_RSSI$t_ind)){
          
          # ## Progress bar
          # Sys.sleep(0.1)
          # setTxtProgressBar(pb2, which(unique(gp_max_RSSI$t_ind) == interval))
          # 
          ## Get grid_points with detections during the interval
          interval_gps <- fdets_prep_sum %>% 
            dplyr::filter(t_ind == interval) %>%
            dplyr::pull(grid_point) %>% 
            unique() 
          
          ## If possible:
          if( length(interval_gps) >= 3  ){
            
            ## Filter node pts based on those with detections
            grid_points_df_f <- grid_points %>% 
              dplyr::filter(grid_point %in% interval_gps) 
            
            ## Get distance between nodes with detections during the point
            n_dist <- raster::pointDistance(grid_points_df_f[,c("x", "y")], 
                                            grid_points_df_f[,c("x", "y")], 
                                            lonlat = F,
                                            allpairs = T)
            
            # Make matrix into a dataframe with a row for NodeId
            n_dist_df <- data.frame(n_dist)
            colnames(n_dist_df) <- grid_points_df_f$grid_point
            n_dist_df$gp <- colnames(n_dist_df)
          }
          
          ## Keep nodes within specified distance filter
          nodes_dist_f <- n_dist_df %>%
            dplyr::filter(gp == gp_max_RSSI[gp_max_RSSI$t_ind == interval,]$grid_point) %>% 
            tidyr::gather(key = "gp_", 
                          value = "distance") %>%
            dplyr::filter(distance <= dist_filter) 
          
          ## If there are at least 3 nodes
          if( nrow(nodes_dist_f) >= 3 ){
            
            ## Filter calibration detections based on nodes within distance filter
            dt_r_dets_f <- fdets_prep_sum %>% 
              na.omit() %>% 
              dplyr::filter(grid_point %in% nodes_dist_f$gp,
                            t_ind == interval) 
            
            ## Bind to other cp dets
            dt_r_dets_all <- dplyr::bind_rows(dt_r_dets_f,
                                              dt_r_dets_all)
            
          }
          
        }
        

        ## If any to process, make wide:
        if(nrow(dt_r_dets_all)>0){
          
          ## Process and make wide
          dt_r_dets_w <- dt_r_dets_all %>%
            
            ## Get mean of filtered values
            dplyr::group_by(dt_r) %>% 
            # dplyr::select(-node) %>% 
            data.frame() %>% 
            tidyr::pivot_wider(names_from = "grid_point",
                               values_from  = "mean_RSSI_gp") %>% 
            dplyr::arrange(dt_r) %>% 
            dplyr::mutate(date_round = lubridate::floor_date(dt_r, unit = "day"))
          
          ## Create directory if needed
          if(!dir.exists(paste0(output_folder, "/ml_prepared/w_error/15s/", tag_f))){
            
            dir.create(paste0(output_folder, "/ml_prepared/w_error/15s/", tag_f))  
          }
          
          ## Split based on date round and save (not neecessary, but keeping for now)
          dt_r_dets_w %>% 
            dplyr::group_by(date_round) %>% 
            dplyr::group_walk(~ write.csv(.x, paste0(output_folder,
                                                     "/ml_prepared/w_error/15s/",
                                                     tag_f,
                                                     "/",
                                                     .y$date_round,
                                                     ".csv.gz"),
                                          row.names = F))
          
        }
        
        
        cat("\n Finished tag:", tag_f, "- day:", day_f,"\n")
        
      } else{
        
        cat("\n No records, skipped:", tag_f, "- day:", day_f,"\n")
        
      }
    }
  }
  # ## End progress bar
  # close(pb)
  # 
}
