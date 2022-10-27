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

prepare_dets <- function(processed_dets_file = as.character(),
                         tags = NULL,
                         project = as.charcter(),
                         output_folder = as.character(),
                         grid_point_file = as.character(),
                         window = "60 secs",
                         lag = "-30 secs",
                         dist_filter = 200){
  
  cat("Starting to prepare detection data\n")
  
  ## Read in processed and filtered detection file
  dets <- readRDS(here::here(processed_dets_file))
  
  ## Filter detections to only retain 'official' grid points
  dets_f <- dets %>% 
    dplyr::filter(grepl(pattern = "Gp", dets$grid_point)) %>% 
    dplyr::filter(grid_point != "Gp0")
  
  ## Node data
  grid_points <- readRDS(here::here(grid_point_file))
  
  grid_points_df <- grid_points %>% 
    sf::st_coordinates() %>% 
    as.data.frame() %>% 
    dplyr::mutate(grid_point = as.character(grid_points$grid_point)) %>% 
    dplyr::select(grid_point,
                  x = X,
                  y = Y) %>% 
    na.omit()
  
  ## Tags to process
  if(is.null(tags)){
    
    tags <- unique(dets_f$tag)
    
  }
  
  ## Set progress bar
  pb <- txtProgressBar(min = 0, max = length(tags), style = 3)
  
  ## Process each tag
  for(tag_f in tags){
    
    cat("\n Starting:", tag_f, "\n")
    
    ## Progress bar
    Sys.sleep(0.1)
    setTxtProgressBar(pb, which(tags == tag_f))
    
    ## Subset by tag
    dets_t <- dets_f %>% 
      dplyr::filter(tag == tag_f) %>% 
      dplyr::mutate(day = lubridate::round_date(date_time, unit = "day")) %>% 
      
      ## Remove sensor station detections
      dplyr::filter(grid_point != "Gp0")
    
    ## Get unique days 
    days <- unique(dets_t$day)
    
    ## Get days that have already been prepared for the tag
    prepared_files <- gsub(x = list.files(paste0(output_folder,
                                                 tag_f),
                                          pattern = ".csv"),
                           pattern = ".csv",
                           replacement = "")
    
    
    ## Days to prepare
    days_2_prepare <- days[!(days %in% prepared_files)]
    
    ## Files that still need to be processed
    if(length(days_2_prepare) > 0){
      
      ## Keep days to prepare
      dets_2_prepare <- dets_t %>% 
        dplyr::filter(day %in% days_2_prepare)
      
      cat("\n Days to prepare:", length(days_2_prepare), "\n")
      
      ## If any to prepare
      if(nrow(dets_2_prepare) > 0){
        
        cat("\n Records:", nrow(dets_2_prepare), "\n")
        
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
                                                      f = function(x) round(mean(x$rssi),0),
                                                      na_pad = TRUE)) %>% 
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
          dplyr::select(tag,
                        grid_point,
                        dt_r,
                        mean_RSSI_gp,
                        dets,
                        n_gp) %>% 
          
          ## Create time index
          dplyr::ungroup() %>%
          dplyr::mutate(t_ind = cumsum(!duplicated(dt_r)))
        
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
        
        ## Set progress bar
        pb2 <- txtProgressBar(min = 0, max = length(unique(gp_max_RSSI$t_ind)), style = 3)
        
        cat("\n Windows:", length(unique(gp_max_RSSI$t_ind)), "\n")
        
        ## For each interval
        for(interval in unique(gp_max_RSSI$t_ind)){
          
          ## Progress bar
          Sys.sleep(0.1)
          setTxtProgressBar(pb2, which(unique(gp_max_RSSI$t_ind) == interval))
          
          ## Get grid_points with detections during the interval
          interval_gps <- fdets_prep_sum %>% 
            dplyr::filter(t_ind == interval) %>%
            dplyr::pull(grid_point) %>% 
            unique() 
          
          ## If possible:
          if( length(interval_gps) > 0 ){
            
            ## Filter node pts based on those with detections
            grid_points_df_f <- grid_points_df %>% 
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
            tidyr::gather(key = "gp", 
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
        
        ## End progress bar
        close(pb2)
        
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
            dplyr::mutate(date_round = lubridate::round_date(dt_r, unit = "day"))
          
          ## Create directory if needed
          if(!dir.exists(paste0(output_folder, tag_f))){
            
            dir.create(paste0(output_folder, tag_f))  
          }
          
          ## Split based on date round and save
          dt_r_dets_w %>% 
            dplyr::group_by(date_round) %>% 
            dplyr::group_walk(~ write.csv(.x, paste0(output_folder,
                                                     tag_f,
                                                     "/",
                                                     .y$date_round,
                                                     ".csv"),
                                          row.names = F))
          
        }
        
        
        cat("\n Finished:", tag_f, "\n")
        
        
      } else{
        
        cat("\n No records, finished:", tag_f, "\n")
        
      }
    }
  }
  ## End progress bar
  close(pb)
  
}
