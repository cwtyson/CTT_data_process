## Get HRs in parallel script
library(foreach)
library(magrittr)

cl <- parallel::makeForkCluster(9, outfile = "")
doParallel::registerDoParallel(cl)

## Localize using RF map ############

## Read in RF map file
walking_dets_w <- readr::read_csv(here::here("./data/detections_processed/rf_maps/rf_map_grid_points_wide_combined.csv"),
                           show_col_types = FALSE)

## For each tag, localize
tags <- list.files(here::here("./data/detections_processed/prepared/w_error/"))

# ## Group tags
# tags <- readxl::read_excel("./data/field_data/logs/TaggedBirdList_AUS.xlsx") %>% 
#   janitor::clean_names() %>% 
#   dplyr::filter(!is.na(caught_together)) %>% 
#   dplyr::filter(tag != "NA") %>% 
#   dplyr::pull(tag)


# tags <- c("33334C07","4B192D2A","66781961")
# tags <- "07195533"

## For each tag:
rf_localizing_fn <- function(tag_f){
  
  # tag_f <- "072A2A4B"
  
  cat("Tag", tag_f, "\n")
  
  ## Get files to localize
  prepared <- lubridate::ymd(gsub(".csv",
                                  "",
                                  list.files(paste0("./data/detections_processed/prepared/w_error/", tag_f, "/"))),
                             tz = "Australia/Broken_Hill")
  
  ## Get last date of files that have been localized
  if(length(list.files(paste0("./data/detections_processed/rf_localized/w_error/", tag_f))) > 0){
    
    ## Get list of dates of detections that have already been prepared
    last_date <- suppressWarnings(max(lubridate::ymd(gsub(".csv",
                                                          "",
                                                          list.files(paste0("./data/detections_processed/rf_localized/w_error/", tag_f))),
                                                     tz = "Australia/Broken_Hill")))
    
  } else(last_date <- ymd("2022-09-13", tz = "Australia/Broken_Hill"))
  
  ## Keep dates including and after last localized
  files_2_localize <- paste0("./data/detections_processed/prepared/w_error/", 
                             tag_f,
                             "/",
                             prepared[prepared >= last_date],
                             ".csv.gz")
  
  ## Get 
  for(f in files_2_localize){
    
    # f = files_2_localize[1]
    
    ## Detections from day
    tag_dets_d <- readr::read_csv(f,
                                  show_col_types = FALSE,
                                  progress = FALSE) %>% 
      mutate(dt_r=  lubridate::with_tz(dt_r, tz = "Australia/Broken_Hill")) %>% 
      dplyr::mutate_at(vars(contains("Gp_")), ~round(.,0))
    
    
    ## Summarize
    tag_dets_d_sum <- tag_dets_d %>% 
      dplyr::mutate_at(vars(contains("Gp_")), ~replace(., . == -115, NA)) %>%
      pivot_longer(cols = contains("Gp_")) %>% 
      na.omit() %>% 
      group_by(dt_r) %>% 
      summarise(gps = round(n()/500),
                rssi_str = round(mean(sum(value > -100)/500)))
    
    day_f <- as.character(round_date(tag_dets_d$dt_r[1],"day"))
    
    cat("Starting tag:", tag_f, "- day:", day_f, "\n")
    
    
    ## Join and then subset
    rf_data <- bind_rows(walking_dets_w %>% 
                           mutate(data = "rf"),
                         tag_dets_d %>% 
                           mutate(data = "dets")) %>% 
      dplyr::mutate_at(vars(contains("Gp_")), ~replace(., is.na(.), -115)) %>% 
      dplyr::select(x,y,dt_r,data, everything())
    
    
    ## Calibration points to use for training
    x_tr <- rf_data %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(data == "rf") %>% 
      dplyr::select(contains("Gp"))
    
    
    y_tr <- rf_data %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(data == "rf") %>% 
      dplyr::select(x,
                    y) 
    
    ## Calibration point to use for testing
    x_ts <- rf_data %>% 
      dplyr::ungroup()%>% 
      dplyr::filter(data == "dets") %>% 
      dplyr::select(contains("Gp"))
    
    ## Define model
    model <- ipft::ipfKnn(train_fgp = x_tr, 
                          train_pos = y_tr, 
                          # Non-deault KNN settings to use
                          k = 5,
                          method = "PLGD",
                          alpha = 4.16,
                          sd = 10,
                          threshold = -40)
    
    
    ## Apply estimation
    knnEstimation <- ipft::ipfEstimate(ipfmodel = model, 
                                       test_fgp = x_ts)
    
    ## Neigbors
    neighbors = apply(data.frame(knnEstimation$neighbors) , 1, function(x) paste(x, collapse = "-"))
    
    ## Estimated locations 
    est_locs <- data.frame(tag = tag_f,
                           dt_r = rf_data  %>% 
                             dplyr::filter(data == "dets")  %>% 
                             pull(dt_r),
                           x_est = round(knnEstimation$location$x),
                           y_est = round(knnEstimation$location$y),
                           neighbors = neighbors) 
    
    est_locs_ellipse_coords <- suppressWarnings(est_locs %>%
                                                  group_by(tag, dt_r) %>%
                                                  do(tryCatch(expr = {car::dataEllipse(.data$x_est,
                                                                                       .data$y_est,
                                                                                       levels = 1-exp(-1), 
                                                                                       segments = 100,
                                                                                       draw = FALSE) %>%
                                                      data.frame()},
                                                      error = function(e){ 
                                                        data.frame()
                                                      })))
    
    # est_locs_ellipse_coords_reproj <- est_locs_ellipse_coords %>%
    #   sf::st_as_sf(coords = c("x", "y")) %>%
    #   sf::st_set_crs(3308) %>%
    #   sf::st_transform(4326) %>%
    #   dplyr::transmute(tag,
    #                    x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
    #                    y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>%
    #   sf::st_drop_geometry()
    
    # ggplot(est_locs_ellipse_coords_reproj) +
    #   geom_point(aes(x=x,y=y, color = dt_r))
    
    est_locs_ellipse_features <- est_locs_ellipse_coords %>%
      ungroup() %>% 
      group_by(tag, dt_r) %>%
      do(ell.info = cov.wt(data.frame(.data$x,.data$y)))
    
    ## Error ellipse features
    est_locs_ellipse_features$center_x <- unlist(lapply(est_locs_ellipse_features$ell.info, function(x) as.numeric(x$center[1])))
    est_locs_ellipse_features$center_y <- unlist(lapply(est_locs_ellipse_features$ell.info, function(x) as.numeric(x$center[2])))
    est_locs_ellipse_features$cov <- lapply(est_locs_ellipse_features$ell.info, function(x) x$cov)
    est_locs_ellipse_features$eigen.info <- lapply(est_locs_ellipse_features$ell.info, function(x) eigen(x[[1]]))
    est_locs_ellipse_features$e <- lapply(est_locs_ellipse_features$eigen.info, function(x) sqrt(x$values))
    est_locs_ellipse_features$a <- unlist(lapply(est_locs_ellipse_features$e, function(x) sqrt(x[1]/2)))
    est_locs_ellipse_features$b <- unlist(lapply(est_locs_ellipse_features$e, function(x) sqrt(x[2]/2)))
    est_locs_ellipse_features$u <- lapply(est_locs_ellipse_features$eigen.info, function(x) x$vectors[,1])
    est_locs_ellipse_features$theta <- unlist(lapply(est_locs_ellipse_features$u, function(x) (atan2(x[2],x[1])*360/(2*pi)) - 90))
    
    ## Create data frame to save
    ee_features <- est_locs_ellipse_features %>% 
      dplyr::select(tag, dt_r, center_x, center_y, a,b,theta) %>% 
      data.frame() %>% 
      
      ## Join summary information
      left_join(tag_dets_d_sum, by = "dt_r")
    
    ## Create directory
    if(!dir.exists(paste0(here::here("./data/detections_processed/rf_localized/w_error/", tag_f)))){
      dir.create(paste0(here::here("./data/detections_processed/rf_localized/w_error/", tag_f)))
    }
    
    
    ## Save
    readr::write_csv(ee_features,
                     paste0(here::here("./data/detections_processed/rf_localized/w_error/",
                                       tag_f,
                                       "/"),
                            day_f,
                            ".csv"))
    
    cat("Finished tag:", tag_f, "- day:", day_f, "\n")
  }
  
}


foreach(tag_f=tags,.packages=c("tidyverse","lubridate","readr"), 
                .verbose = TRUE) %dopar% 
  { rf_localizing_fn(tag_f) }