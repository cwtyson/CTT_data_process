#' Localize prepared detections. This function takes files from prepare_dets.R.
#'
#' @param input_folder Folder of prepared detection data
#' @param output_folder Folder location to save data 
#' @param grid_point_file Grid point location file
#' @param RSSI_model_file Model of RSSI and distance.
#' @param reps Number of times to repeat each localization estimate to draw error ellipses
#' 
#' 
localize <- function(input_folder = as.character(),
                     output_folder = as.character(),
                     grid_point_file = "./Eswatini/data/field_data/processed_field_data/grid_point_locations.RData",
                     RSSI_model_file = "./R/data/RSSI_log_distance_lm.RDS",
                     reps = 100){

  ## Read in model
  log_dist_RSSI_mdl <- readRDS(RSSI_model_file)
  
  ## Model table
  mdl_tab <- broom::augment(log_dist_RSSI_mdl,
                            newdata = data.frame(RSSI = seq(-25, -115, by = -1)),
                            se_fit = TRUE) %>% 
    dplyr::select(mean_RSSI = RSSI,
                  mean = .fitted,
                  sd = .se.fit) %>% 
    dplyr::distinct(mean_RSSI,.keep_all = T)
  
  
  ## Grid point locations
  node_pts <- readRDS(grid_point_file) %>%
    sf::st_transform(4326) %>% 
    dplyr::transmute(gp = stringr::str_extract(grid_point, pattern = "^Gp[:digit:]{1,2}"),
                     gp_lon = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                     gp_lat = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>% 
    sf::st_drop_geometry()
  
  ## Tags prepared 
  tags_2_localize <- list.files(path = input_folder)
  
  ## Set progress bar
  pb <- txtProgressBar(min = 0, max = length(tags_2_localize), style = 3)
  
  ## Process each tag that hasn't already been processed  ######
  for(tag_f in tags_2_localize){
    
    ## Create directory if needed
    if(!dir.exists(paste0(output_folder, tag_f))){
      
      dir.create(paste0(output_folder, tag_f))  
    }
    
    # tag_f <- tags_2_localize[2]
    
    ## Progress bar
    Sys.sleep(0.1)
    setTxtProgressBar(pb, which(tag_f == tags_2_localize))
    
    ## Prepared files
    files_prep <- list.files(path = paste0(input_folder,
                                           tag_f))
    
    ## Get files that have been localized
    files_localized <- list.files(path = paste0(output_folder,
                                                tag_f))
    
    start_time <- Sys.time()
    
    ## If any files
    if(length(files_prep[!(files_prep %in% files_localized)]) > 0){
      
      ## Files to localize
      files_2_localize <- paste0(input_folder,
                                 tag_f,
                                 "/",
                                 files_prep[!(files_prep %in% files_localized)])
      
      cat("\n Starting tag:", tag_f)
      
      ## Get all files to process
      tag_f_files <- list.files(paste0("/Users/tyson/Documents/academia/institutions/WUR/research/eswatini/data/detections/prepared/",
                                       tag_f),
                                full.names = T)
      
      ## Set progress bar
      pb2 <- txtProgressBar(min = 0, max = length(files_2_localize), style = 3)
      
      ## Process each file for each tag separately ######
      for (file in files_2_localize){
        
        # file <- files_2_localize[6]
        
        ## Get current date
        tag_f_date <- stringi::stri_sub(gsub(".csv","",file),-10)
        
        cat("\n Starting date:", tag_f_date)
        
        start_time <- Sys.time()
        
        ## Progress bar
        Sys.sleep(0.1)
        setTxtProgressBar(pb2, which(files_2_localize == file))
        
        ## Get detections
        dets <- readr::read_csv(file,
                                show_col_types = FALSE,
                                guess_max = 10000)
        
        ## Process for nls 
        dets_p <- dets %>% 
          
          dplyr::select(-n_gp,
                        -dets,
                        -t_ind) %>% 
          
          dplyr::group_by(dt_r) %>% 
          tidyr::pivot_longer(cols = contains("Gp"),
                              names_to = "gp",
                              values_to = "RSSI") %>% 
          na.omit() %>% 
          dplyr::group_by(dt_r,
                          gp) %>% 
          dplyr::summarise(mean_RSSI = round(mean(RSSI),0),
                           .groups = "keep")  %>% 
          
          ## Join model interval
          dplyr::left_join(mdl_tab,
                           by = "mean_RSSI")
        
        ## Retain periods with at least 3 nodes
        dets_p <- dets_p %>% 
          na.omit() %>% 
          dplyr::group_by(dt_r) %>% 
          dplyr::mutate(n_gp = n()) %>% 
          
          ## Remove any periods with fewer than 3 gps
          dplyr::filter(n_gp >= 3) %>%
          dplyr::ungroup() %>% 
          dplyr::mutate(t_ind = cumsum(!duplicated(dt_r))) %>% 
          
          ## Join node pts
          dplyr::left_join(node_pts,
                           by = "gp") 
        
        ## If any:
        if(nrow(dets_p) > 0){
          
          cat("\n Intervals to localize:", max(dets_p$t_ind)) 
          
          ## Empty df
          tag_loc_est <- data.frame()
          
          ## For each interval
          for(int in unique(dets_p$t_ind)){
            
            # int = unique(dets_p$t_ind)[10]
            
            tryCatch(
              expr = {
                
                ## Subset detections
                dets_p_int = dets_p[dets_p$t_ind == int,]
                
                ## Ellipse info
                ellipse_center_est <- list()
                ellipse_cov_est <- list()
                
                for(i in 1:reps){
                  
                  ## Sample within interval
                  dets_p_int <- dets_p_int %>% 
                    dplyr::rowwise() %>% 
                    dplyr::mutate(dist_est_samp = round(exp(sample(rnorm(n = 100, 
                                                                         mean = mean,
                                                                         sd = sd),
                                                                   size = 1)),0))
                  
                  # Determine the node with the strongest avg.RSSI value to be used as starting values
                  max_RSSI <- dets_p_int[which.max(dets_p_int$mean_RSSI),]
                  
                  # Non-linear test to optimize the location of unknown signal by looking at the radius around each Node based on RSSI values (distance) and the pairwise distance between all nodes
                  nls_mod <- suppressWarnings(nls(dist_est_samp ~ geosphere::distm(data.frame(gp_lon, gp_lat), 
                                                                                   c(lng_solution, lat_solution), 
                                                                                   fun = distHaversine), # distm - matrix of pairwise distances between lat/longs
                                                  data = dets_p_int,
                                                  start = list(lng_solution = max_RSSI$gp_lon,
                                                               lat_solution = max_RSSI$gp_lat),
                                                  control = nls.control(warnOnly = T,
                                                                        minFactor=1/30000)))
                  
                  ## Determine error around the point location estimate
                  ellipse <- car::confidenceEllipse(nls_mod, 
                                                    levels = 1-exp(-1), 
                                                    segments = 1000, 
                                                    draw = F) 
                  
                  ## Project
                  ellipse_coords_proj <- ellipse %>%
                    data.frame() %>%
                    sf::st_as_sf(coords = c("x", "y")) %>%
                    sf::st_set_crs(4326) %>%
                    sf::st_transform(22291) %>%
                    dplyr::transmute(x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                                     y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>%
                    sf::st_drop_geometry()
                  
                  ## Ellipse error features
                  ell.info <- cov.wt(ellipse_coords_proj)
                  
                  ## Add ellipse info to list
                  ellipse_center_est[[i]] <- ell.info$center
                  ellipse_cov_est[[i]] <- ell.info$cov
                  
                }
                
                ## Get average point
                pts <- do.call(rbind, ellipse_center_est) %>% 
                  as.data.frame()
                mean_pt_est <- data.frame(x = mean(pts$x),
                                          y = mean(pts$y))
                
                ## Get mean covariance matrix
                mean_cov <- apply(simplify2array(ellipse_cov_est), c(1,2), mean)
                eigen.info <- eigen(mean_cov)
                
                ## Axes
                e <- sqrt(eigen.info$values)
                a <- sqrt(e[1]/2)  # semi-major axis
                b <- sqrt(e[2]/2)  # semi-minor axis
                
                ## Orientation
                u <- eigen.info$vectors[,1] # major axis eigenvector
                theta <- atan2(u[2],u[1]) # angle from x-axis in radians
                theta <- theta *360/(2*pi) # angle from x-axis in degrees
                theta <- theta - 90 # angle from y-axis in degrees
                
                ## Combine estimated locations with summary information
                tag_int_loc_est <- data.frame(tag = tag_f,
                                              dt_r = dets_p_int$dt_r[1],
                                              n_gp = dets_p_int$n_gp[1],
                                              mean_RSSI = mean(dets_p_int$mean_RSSI),
                                              max_RSSI = max(dets_p_int$mean_RSSI),
                                              x_est = mean_pt_est$x,
                                              y_est = mean_pt_est$y,
                                              semi_major = a,
                                              semi_minor = b,
                                              orientation = theta)
                
                ## Combine
                tag_loc_est <- rbind(tag_loc_est, tag_int_loc_est)
              },
              error = function(e){ 
                "Too few detections to trilaterate location"
              }
              
            )
            
          }
          
          ## Save as separate file
          readr::write_csv(tag_loc_est,
                           paste0(output_folder, 
                                  tag_f,
                                  "/",
                                  tag_f_date,
                                  ".csv"))
          
          cat("\n Finished localizing after:", round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1), "minutes")
          
        } else {
          
          cat("\n No intervals to localize, skipped file")
          
        }
        
      } 
      
    } else{
      
      cat("\n No new files to localize, skipped tag")
      
      
    }
    
    ## End progress bar for files
    close(pb2)
    
  }
  
  ## End progress bar for all tags
  close(pb)
  
  
}
