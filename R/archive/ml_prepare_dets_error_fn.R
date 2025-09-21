ml_prepare_dets_error_fn <- function(band_f,
                                     output_folder, 
                                     grid_points_folder,
                                     tz,
                                     crs,
                                     sumFun = "mean",
                                     window = "30 secs",
                                     lag = "0 secs",
                                     dist_filter = 175){
  
  ## Get raw file path
  path = list.files(paste0(output_folder, "raw_detections", "/"),pattern = band_f,recursive = T,full.names = T)[grep("data",list.files(paste0(output_folder, "raw_detections", "/"),pattern = band_f,recursive = T))]
  
  ## Read in raw data
  dets_t <- readRDS(path)

  year = format(min(dets_t$date_time),"%Y")
  
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
    
    ## Get grid points
    grid_points <- get_grid_points_fn_zebby(grid_points_folder,
                                            crs = crs)
    
    
    ## Files that still need to be prepared
    if(length(days2prepare) > 0){
      
      cat("\n Starting tag:", band_f, "- days to prepare:", length(days2prepare), "\n")
      
      ## Set progress bar for preparing tags
      pb <- txtProgressBar(min = 0, max = length(days2prepare), style = 3)
      
      ## Prepare each day separately
      for(day_f in days2prepare){
        
        ## Progress bar
        Sys.sleep(0.1)
        setTxtProgressBar(pb, which(days2prepare == day_f))
        # 
        # day_f = days2prepare[1]
        
        day_f_f <- as.Date(day_f, tz = tz)
        
        ## Retain detections on that day
        dets_2_prepare <- dets_t %>% 
          dplyr::filter(date == day_f_f)
        
        ## If any to prepare
        if(nrow(dets_2_prepare) > 0){
          
          cat("\n Starting tag:", band_f, "- day:", day_f, "- detections:", nrow(dets_2_prepare), "\n")
          
          ## Process
          fdets_prep <- dets_2_prepare %>%
            dplyr::arrange(date_time) %>% 
            dplyr::group_by(grid_point) %>% 
            dplyr::mutate(dt_r = lubridate::round_date(date_time,
                                                       unit = window),
                          rssi_gp = runner::runner(x = .,
                                                   k = window,
                                                   lag = lag,
                                                   idx = "date_time",
                                                   f = function(x) match.fun(sumFun)(x$rssi),
                                                   na_pad = FALSE)) %>% 
            dplyr::ungroup() %>% 
            dplyr::select(bird_band,
                          tag,
                          date_time, 
                          dt_r,
                          grid_point,
                          rssi_gp)
          
          ## Keep nodes within filtered distance
          fdets_prep_sum <- fdets_prep %>% 
            dplyr::group_by(bird_band,
                            tag,
                            dt_r,
                            grid_point)  %>%
            
            ## Summarize within rounded interval using specified summary function
            dplyr::summarise(rssi_gp_r = round(match.fun(sumFun)(rssi_gp, na.rm = T), 0),
                             .groups = "keep") %>% 
            
            ## Change groups
            dplyr::group_by(dt_r) %>% 
            
            ## Number of grid points in the interval
            dplyr::mutate(n_gp = n()) %>% 
            
            ## Arrange by dt_r, rssi_gp and then shuffle any ties
            dplyr::arrange(dt_r, desc(rssi_gp_r), runif(nrow(.))) %>% 
            
            ## Remove any intervals without at least 3 grid points
            dplyr::filter(n_gp >= 3) %>% 
            
            ## Join coordinates for grid points
            dplyr::left_join(grid_points, 
                             by = dplyr::join_by(grid_point)) %>% 
            
            ## Within each interval, distance from the first (strongest node) to the remainder
            dplyr::mutate(dist = sqrt((gp_x - dplyr::first(gp_x))^2 + (gp_y - dplyr::first(gp_y))^2)) %>% 
            
            ## Remove nodes farther away than specified filter
            dplyr::filter(dist <= dist_filter) %>% 
            
            ## Calculate remaining nodes within interval and remove intervals without at least 3
            dplyr::group_by(dt_r) %>% 
            dplyr::mutate(n_gp = n()) %>% 
            dplyr::ungroup() %>% 
            dplyr::filter(n_gp >= 3) %>% 
            dplyr::as_tibble()  %>% 
            
            ## Group by:
            dplyr::group_by(tag, dt_r) %>% 
            
            ## Create interval id
            dplyr::mutate(int_id = dplyr::cur_group_id()) %>% 
            dplyr::arrange(int_id) %>% 
            dplyr::select(int_id, dplyr::everything())
          
          ## Create directory if needed
          if(!dir.exists(paste0(output_folder,"/ml_prepared/", year, "/", band_f))){
            
            dir.create(paste0(output_folder,"/ml_prepared/", year, "/", band_f), recursive = T)
          }  
          
          ## Split based on date round and save
          readr::write_csv(x= fdets_prep_sum, 
                           file = paste0(output_folder,
                                         "/ml_prepared/",
                                         year,
                                         "/", 
                                         band_f,
                                         "/",
                                         day_f,
                                         ".csv.gz"))
          
          cat("\n Finished tag:", band_f, "- day:", day_f,"\n")
          
          
        } else{
          
          cat("\n No data to save for tag:", band_f, "- day:", day_f,"\n")
          
        }
      } 
    }
    ## End progress bar
    close(pb)
  }
  
  cat("############ \n",
      "Finished preparing tag: ", band_f," - days prepared: ", n_distinct(raw_days), "\n",
      "############ \n", sep = "")
  
}


