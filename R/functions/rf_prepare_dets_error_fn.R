
## Process each tag
rf_prepare_dets_error <- function(tag_f, dets_t, output_folder, tz){
  
  # ## Subset by tag
  # dets_t <- dets_t %>% 
  #   dplyr::filter(tag == tag_f) %>% 
  #   dplyr::filter(date >= last_date) 
  # 
  ## Get unique days 
  days <- as.character(unique(dets_t$date))
  
  ## Set progress bar for preparing tags
  pb <- txtProgressBar(min = 0, max = length(days), style = 3)
  
  ## Files that still need to be processed
  if(length(days) > 0){
    
    cat("\n Tag:", tag_f, "- days to prepare:", length(days), "\n")
    
    for(day_f in days){
      
      ## Progress bar
      Sys.sleep(0.1)
      setTxtProgressBar(pb, which(days == day_f))
      
      # day_f = days[26]
      
      day_f_f <- as.Date(day_f, tz = tz)
      
      dets_2_prepare <- dets_t %>% 
        filter(date == day_f_f)
      
      ## If any to prepare
      if(nrow(dets_2_prepare) > 0){
        
        cat("\n Tag:", tag_f, "- day:", day_f, nrow(dets_2_prepare), "detections to prepare \n")
        
        ## Prepare filtered records
        fdets_prep <- dets_2_prepare %>%
          dplyr::arrange(grid_point, date_time) %>% 
          dplyr::group_by(grid_point) %>% 
          dplyr::mutate(dt_r = lubridate::floor_date(date_time,
                                                     unit = "30 seconds"),
                        running_gp_RSSI = runner::runner(x = .,
                                                         k = "60 secs",
                                                         lag = "-30 secs",
                                                         idx = "date_time",
                                                         f = function(x) mean(x$rssi),
                                                         na_pad = FALSE))  %>% 
          dplyr::group_by(grid_point, dt_r) %>% 
          dplyr::mutate(mean_rssi = mean(running_gp_RSSI, na.rm = TRUE),
                        sd_rssi = sd(running_gp_RSSI, na.rm = TRUE)) %>% 
          dplyr::ungroup() %>% 
          dplyr::distinct(tag,
                          grid_point,
                          dt_r,
                          .keep_all = TRUE) %>% 
          dplyr::group_by(dt_r) %>% 
          dplyr::mutate(gps = n_distinct(grid_point),
                        rssi_str = sum(mean_rssi > -100)) %>% 
          dplyr::select(tag,
                        grid_point,
                        dt_r,
                        mean_rssi,
                        sd_rssi,
                        gps,
                        rssi_str) %>% 
          arrange(dt_r)
        
        ## Get average SD for each mean RSSI to use for imputing
        fdets_prep_sum <- fdets_prep %>% 
          mutate(mean_rssi_r = round(mean_rssi)) %>% 
          group_by(mean_rssi_r) %>% 
          summarise(mean_sd = mean(sd_rssi,na.rm  = TRUE))
        
        ## Fill in missing values if possible
        fdets_prep_j <- fdets_prep %>% 
          mutate(mean_rssi_r = round(mean_rssi)) %>% 
          left_join(fdets_prep_sum, by = "mean_rssi_r") %>% 
          
          ## Fill in with mean SD if missing
          mutate(sd_rssi = ifelse(is.na(sd_rssi), mean_sd, sd_rssi))
        
        
        ## Resample 500 times
        fdets_wide <- data.frame()
        for(i in 1:500){
          
          ## Sample from distribution 
          fdets_wide_resampled <- fdets_prep_j %>% 
            # slice(1:30) %>% 
            rowwise() %>% 
            mutate(rssi_sample = suppressWarnings(rnorm(n= 1,
                                                        mean = mean_rssi,
                                                        sd = sd_rssi))) %>% 
            group_by(dt_r) %>% 
            dplyr::transmute(tag, dt_r, rep = i, grid_point, rssi_sample) %>%
            tidyr::pivot_wider(names_from = "grid_point",
                               values_from = "rssi_sample",
                               names_prefix = "Gp_")
          
          fdets_wide <- bind_rows(fdets_wide, fdets_wide_resampled)
          
        }
        
        ## Create directory
        if(!dir.exists(paste0(output_folder,"/data/processed_detections/prepared/w_error/60s/", tag_f))){
          dir.create(paste0(output_folder,"/data/processed_detections/prepared/w_error/60s/", tag_f))
        }
        
        
        ## Save
        readr::write_csv(fdets_wide,
                         paste0(output_folder,
                                "/data/processed_detections/prepared/w_error/60s/", 
                                tag_f,
                                "/", 
                                as.character(day_f),".csv.gz"),
                         progress = FALSE)
        
        cat("\n Saved prepared detections from tag:", tag_f, "- day:", day_f, "\n")
      }
    }
  }
  
  close(pb)
}
