ml_prepare_dets_error_fn <- function(band_f,
                                     tag_log_folder,
                                     grid_points_folder,
                                     output_folder, 
                                     tz,
                                     crs = crs,
                                     window = "30 secs",
                                     lag = "-15 secs",
                                     dist_filter = 305){
  
  ## Read in tag log and reformat
  tag_log_mr <- sort(list.files(paste0(tag_log_folder),
                                full.names = TRUE,
                                pattern = "tag_log"),
                     decreasing = TRUE)[1]
  
  ##  Process most recent tag log
  tag_log_all  <- suppressWarnings(readxl::read_excel(tag_log_mr) %>% 
                                     janitor::clean_names() %>% 
                                     transmute(species,
                                               tag = gsub("NA",NA, tag),
                                               bird_band,
                                               # date,
                                               # time,
                                               tag_start_time = lubridate::parse_date_time(paste(date, time),
                                                                                           tz = tz,
                                                                                           orders = c("%d.%m.%Y %H:%M:%S",
                                                                                                      "%d.%m.%Y %H:%M")),
                                               # end_date,
                                               # end_time,
                                               tag_removal_time = lubridate::parse_date_time(paste(end_date, end_time),
                                                                                             tz = tz,
                                                                                             orders = c("%d.%m.%Y %H:%M:%S",
                                                                                                        "%d.%m.%Y %H:%M")),
                                               year = format(tag_start_time, "%Y"))  %>% 
                                     dplyr::filter(!is.na(tag)) %>% 
                                     dplyr::select(year,
                                                   bird_band,
                                                   tag,
                                                   tag_start_time,
                                                   tag_removal_time) %>% 
                                     dplyr::mutate(tag_removal_time = if_else(is.na(tag_removal_time), Sys.time(), tag_removal_time))) %>% 
    
    ## Keep focal band
    dplyr::filter(bird_band == band_f)
  
  ## Get year bird was tagged
  year = min(tag_log_all$year)
  
  ## Get grid points
  grid_points <- get_grid_points_fn_mousebird(grid_points_folder,
                                              crs)
  
  ## Read in raw data
  dets_t <- readRDS(paste0(output_folder, "/raw_detections", "/", year, "/data/",band_f,".RDS"))
  
  ## Get most recently prepared data file (if it exists)
  mrdf <- rev(list.files(paste0(output_folder,  "/ml_prepared/", year, "/",band_f,""),full.names = TRUE, pattern = ".csv.gz"))[1]
  
  ## Get date time to filter by
  if(!is.na(mrdf)){
    mrd <- suppressWarnings(readr::read_csv(mrdf,show_col_types = FALSE) %>%
                              dplyr::pull(dt_r) %>%
                              max())
    mrd <- lubridate::with_tz(mrd, tz = tz)
  } else{
    
    ## If no data is prepared, then use the start raw detection data
    mrd <- min(dets_t$date_time)
  }
  
  ## Filter detections based on prepared data
  dets_t <- dets_t %>%
    dplyr::filter(date_time >= mrd)
  
  
  ## If new data to prepare:
  if(nrow(dets_t) > 0){
    
    ## Get unique days in raw data
    raw_days <- unique(dets_t$date)
    
    ## Most recent day
    mrd <- rev(sort(gsub(".csv.gz",
                         "",
                         list.files(paste0(output_folder,  "/ml_prepared/", year, "/", band_f),
                                    recursive = T,
                                    pattern = ".csv.gz"))))[1]
    
    ## If NA, then set to first raw day
    if(is.na(mrd)){
      mrd<-min(raw_days)
    }
    
    ## Keep any days greater than or equal to last day prepared
    days2prepare <- as.character(raw_days[raw_days >= mrd])
    
    ## Files that still need to be prepared
    if(length(days2prepare) > 0){
      
      cat("\n Starting tag:", band_f, "- days to prepare:", length(days2prepare), "\n")
    
      ## Set progress bar for preparing tags
      pb <- txtProgressBar(min = 0, max = length(days2prepare), style = 3)
      
      for(day_f in days2prepare){
        
        ## Progress bar
        Sys.sleep(0.1)
        setTxtProgressBar(pb, which(days2prepare == day_f))
        # 
        # day_f = days2prepare[1]
        
        day_f_f <- as.Date(day_f, tz = tz)
        
        dets_2_prepare <- dets_t %>% 
          dplyr::filter(date == day_f_f)
        
        # ## Summarize detections per day
        # dets_sum <- dets_t %>% 
        #   group_by(date) %>% 
        #   summarise(dets= n())
        # 
        ## If any to prepare
        if(nrow(dets_2_prepare) > 0){
          
          cat("\n Starting tag:", band_f, "- day:", day_f, "- detections:", nrow(dets_2_prepare), "\n")
          
          ## Prepare filtered records
          fdets_prep <- dets_2_prepare %>%
            dplyr::arrange(date_time) %>% 
            dplyr::group_by(grid_point) %>% 
            dplyr::mutate(dt_r = lubridate::floor_date(date_time,
                                                       unit = lag),
                          mean_rssi_gp = runner::runner(x = .,
                                                        k = window,
                                                        lag = lag,
                                                        idx = "date_time",
                                                        f = function(x) mean(x$rssi),
                                                        na_pad = FALSE)) %>% 
            dplyr::ungroup() %>% 
            dplyr::select(bird_band,
                          tag,
                          grid_point,
                          date_time, 
                          dt_r,
                          mean_rssi_gp)
          
          ## Summarize
          fdets_prep_sum <- fdets_prep %>% 
            dplyr::group_by(grid_point,
                            dt_r) %>% 
            na.omit() %>%
            dplyr::mutate(mean_rssi_gp = round(mean(mean_rssi_gp,
                                                    na.rm = T), 0),
                          dets = dplyr::n()) %>% 
            dplyr::group_by(dt_r) %>% 
            dplyr::mutate(n_gp = dplyr::n_distinct(grid_point)) %>% 
            dplyr::distinct(grid_point,
                            dt_r,
                            .keep_all = T) %>% 
            
            ## Create time index
            dplyr::ungroup() %>%
            dplyr::mutate(t_ind = cumsum(!duplicated(dt_r))) %>% 
            
            ## Rearrange
            dplyr::select(bird_band,tag, dt_r, t_ind, grid_point, mean_rssi_gp,dets,n_gp)
          
          ## Grid point with max rssi for each window
          gp_max_rssi <- fdets_prep_sum %>% 
            
            ## Get max rssi values within each window
            dplyr::group_by(t_ind) %>%
            dplyr::mutate(max_rssi = max(mean_rssi_gp)) %>%
            dplyr::filter(mean_rssi_gp == max_rssi) %>% 
            dplyr::distinct(t_ind,
                            .keep_all = T)
          
          ## Empty data frame
          dt_r_dets_all <- data.frame()
          
          ## For each interval
          for(interval in unique(gp_max_rssi$t_ind)){
            
            # ## Progress bar
            # Sys.sleep(0.1)
            # setTxtProgressBar(pb2, which(unique(gp_max_rssi$t_ind) == interval))
            # 
            
            # interval = unique(gp_max_rssi$t_ind)[1]
            
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
              
              ## If any:
              if(nrow(grid_points_df_f)>0){
                
                ## Get distance between nodes with detections during the point
                n_dist <- raster::pointDistance(grid_points_df_f[,c("gp_x", "gp_y")], 
                                                grid_points_df_f[,c("gp_x", "gp_y")], 
                                                lonlat = F,
                                                allpairs = T)
                
                # Make matrix into a dataframe with a row for NodeId
                n_dist_df <- data.frame(n_dist)
                colnames(n_dist_df) <- grid_points_df_f$grid_point
                n_dist_df$gp <- colnames(n_dist_df)
                
                ## Keep nodes within specified distance filter
                nodes_dist_f <- n_dist_df %>%
                  dplyr::filter(gp == gp_max_rssi[gp_max_rssi$t_ind == interval,]$grid_point) %>% 
                  tidyr::gather(key = "gp_", 
                                value = "distance") %>%
                  dplyr::filter(distance <= dist_filter) 
                
              }
              
              
              
              ## If there are at least 3 nodes
              if( nrow(nodes_dist_f) >= 3 ){
                
                ## Filter based on nodes within distance filter
                dt_r_dets_f <- fdets_prep_sum %>% 
                  na.omit() %>% 
                  dplyr::filter(grid_point %in% nodes_dist_f$gp,
                                t_ind == interval) 
                
                ## Bind to other cp dets
                dt_r_dets_all <- dplyr::bind_rows(dt_r_dets_f,
                                                  dt_r_dets_all)
                
              }
            }
            
          }
          
          ## If any to process, make wide:
          if(nrow(dt_r_dets_all)>0){
            
            ## Process and make wide
            dt_r_dets_w <- dt_r_dets_all %>%
              
              ## Get mean of filtered values
              dplyr::group_by(dt_r) %>% 
              dplyr::select(-dets) %>%
              data.frame() %>% 
              tidyr::pivot_wider(names_from = "grid_point",
                                 values_from  = "mean_rssi_gp") %>% 
              dplyr::arrange(dt_r) %>% 
              dplyr::mutate(date_round = lubridate::floor_date(dt_r, unit = "day"))
            
            ## Create directory if needed
            if(!dir.exists(paste0(output_folder,"/ml_prepared/", year, "/", band_f))){
              
              dir.create(paste0(output_folder,"/ml_prepared/", year, "/", band_f), recursive = T)
            }  
            
            ## Split based on date round and save
            write.csv(dt_r_dets_w, paste0(output_folder,
                                          "/ml_prepared/",
                                          year,
                                          "/", 
                                          band_f,
                                          "/",
                                          day_f,
                                          ".csv.gz"),
                      row.names = F)
            cat("\n Finished tag:", band_f, "- day:", day_f,"\n")
            
            
          } else{
            
            cat("\n No data to save for tag:", band_f, "- day:", day_f,"\n")
            
          }
        } 
        
      }
      
      # ## End progress bar
      close(pb)
    }

    
    cat("############ \n",
        "Finished preparing tag: ", band_f," - days prepared: ", "\n",
        "############ \n", sep = "")
    
  } else{
    
    cat("############ \n",
        "No new data for tag: ", band_f, "\n",
        "############ \n", sep = "")
    
  }
  
}


