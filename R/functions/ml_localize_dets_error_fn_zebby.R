ml_localize_dets_error_fn_zebby <- function(band_f,
                                      output_folder,
                                      grid_points,
                                      log_dist_RSSI_mdl,
                                      tz = tz,
                                      crs = crs,
                                      reps = reps,
                                      dist_cutoff = 200){
  
  cat("############ \n",
      "Starting localizing tag: ", band_f, "\n",
      "############ \n", sep = "")
  
  ## Read in RSSI~distance model
  mdl <- readRDS(log_dist_RSSI_mdl)
  
  ## Predict
  mdl_tab <- mdl %>% 
    broom::augment(newdata = data.frame(rssi = seq(-25, -115, by = -1)),
                   se_fit = TRUE) %>% 
    dplyr::select(mean_rssi = rssi,
                  mean = .fitted) %>% 
    dplyr::distinct(mean_rssi,.keep_all = T)
  
  ## Get model sigma
  mdl_sigma <- sigma(mdl)
  
  ## Convert grid points to lat/long  ########
  grid_points_ll <- suppressWarnings(grid_points %>%
                                       sf::st_as_sf(coords = c("gp_x","gp_y"),
                                                    crs = crs) %>% 
                                       sf::st_transform(4326) %>% 
                                       dplyr::transmute(grid_point,
                                                        gp_lon = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                                                        gp_lat = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>% 
                                       sf::st_drop_geometry())
  
  ## Create directory if needed
  if(!dir.exists(paste0(output_folder,"/ml_localized/", band_f))){
    
    suppressWarnings(dir.create(paste0(output_folder,"/ml_localized")))
    dir.create(paste0(output_folder,"/ml_localized/", band_f))  
  }
  
  ## Prepared files
  files_prep <- list.files(path = paste0(output_folder,
                                         "/ml_prepared/",
                                         band_f,
                                         "/"),
                           ".csv.gz",
                           full.names = T)
  
  ## Get files that have been localized
  files_localized <- list.files(path = paste0(output_folder,
                                              "/ml_localized/",
                                              band_f,
                                              "/"),
                                ".csv.gz",
                                full.names = T)
  
  ## Files to localize
  files_2_localize <-  files_prep[!(files_prep %in% files_localized)]
  
  
  ## If any files
  if(length(files_2_localize) > 0){
    
    
    ## Set progress bar
    pb2 <- txtProgressBar(min = 0, max = length(files_2_localize), style = 3)
    
    ## Process each file for each tag separately ######
    for (file in files_2_localize){
      
      # file <- files_2_localize[1]
      
      ## Get current date
      band_f_date <- stringi::stri_sub(gsub(".csv.gz","",file),-10)
      
      ## Progress bar
      Sys.sleep(0.1)
      setTxtProgressBar(pb2, which(files_2_localize == file))
      
      ## Get detections
      dets <- readr::read_csv(file,
                              col_types = paste0("ccT", paste(rep('d', 200), collapse='')),
                              show_col_types = FALSE)
      
      ## Process for nls 
      dets_p <- dets %>% 
        
        dplyr::group_by(dt_r) %>% 
        tidyr::pivot_longer(cols = c(6:ncol(.)),
                            names_to = "grid_point",
                            values_to = "mean_rssi") %>% 
        na.omit() %>% 
        
        ## Remove any periods with fewer than 3 gps
        dplyr::filter(n_gp >= 3) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(t_ind = cumsum(!duplicated(dt_r))) %>% 
        
        ## Join node pts
        dplyr::left_join(grid_points_ll,
                         by = "grid_point") %>% 
        ## Join model interval
        dplyr::left_join(mdl_tab,
                         by = "mean_rssi")
      
      ## If any:
      if(nrow(dets_p) > 0){
        
        cat("\n Starting tag:", band_f, "- date:", band_f_date, "- intervals to localize:", length(unique(dets_p$t_ind)), "\n")
        
        ## Starting time
        start_time <- Sys.time()
        
        ## Empty df
        tag_loc_est <- data.frame()
        
        # ## Set progress bar
        # pb_ints <- txtProgressBar(min = 0, max = length(unique(dets_p$t_ind)), style = 3)
        # 
        ## Empty lists
        tag_loc_est_list <- list()
        cov_list <- list()
        pe_df_list <- list()
        
        ## For each interval
        for(int in unique(dets_p$t_ind)){
          
          # int = unique(dets$t_ind)[1]
          
          # ## Progress bar
          # Sys.sleep(0.1)
          # setTxtProgressBar(pb_ints, which(unique(dets_p$t_ind) == int))
          # 
          tryCatch(
            expr = {
              
              ## Subset detections
              dets_p_int = dets_p[dets_p$t_ind == int,]
              
              ## Point estimates
              pe <- list()
              
              ## Repeat X times
              for(i in 1:reps){
                
                ## Sample within interval
                dets_p_int_sample <- dets_p_int %>% 
                  mutate(dist_est_mean = predict(mdl, newdata =  data.frame(rssi = mean_rssi))) %>% 
                  dplyr::rowwise() %>% 
                  dplyr::mutate(dist_est_samp = round(10^(sample(rnorm(n = 100,
                                                                       mean = dist_est_mean,
                                                                       sd = mdl_sigma),size = 1))),
                                
                                ## Cutoff estimates 
                                dist_est_samp = ifelse(dist_est_samp > dist_cutoff, dist_cutoff, dist_est_samp))
                
                # Determine the node with the strongest avg.RSSI value to be used as starting values
                max_RSSI <- dets_p_int[which.max(dets_p_int$mean_rssi),]
                
                # Non-linear test to optimize the location of unknown signal by looking at the radius around each Node based on RSSI values (distance) and the pairwise distance between all nodes
                nls_mod <- suppressWarnings(nls(dist_est_samp ~ geosphere::distm(data.frame(gp_lon, gp_lat), 
                                                                                 c(lng_solution, lat_solution), 
                                                                                 fun = distHaversine), # distm - matrix of pairwise distances between lat/longs
                                                data = dets_p_int_sample,
                                                start = list(lng_solution = max_RSSI$gp_lon,
                                                             lat_solution = max_RSSI$gp_lat),
                                                control = nls.control(warnOnly = T,
                                                                      minFactor=1/30000)))
                
                ## Combine point estimates
                pe[[i]] <- c(coef(nls_mod)[1],
                             coef(nls_mod)[2])
                
              }
              
              ## Combine point estimates
              pe_df <- do.call(rbind,pe) %>% 
                data.frame()
              
              ## Project
              ellipse_coords_proj <- pe_df %>%
                sf::st_as_sf(coords = c("lng_solution", "lat_solution")) %>%
                sf::st_set_crs(4326) %>%
                sf::st_transform(crs = crs) %>%
                dplyr::transmute(x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                                 y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>%
                sf::st_drop_geometry()
              
              ## Ellipse info
              ell.info <- cov.wt(cbind(ellipse_coords_proj$x, ellipse_coords_proj$y))
              x_est <- ell.info$center[1]
              y_est <- ell.info$center[2]
              eigen.info <- eigen(ell.info$cov)
              lengths <- sqrt(eigen.info$values * 2 * qf(.95, 2, reps-1))
              a = lengths[1]
              b = lengths[2]
              
              ## Orientation
              u <- eigen.info$vectors[,1] # major axis eigenvector
              theta <- atan2(u[2],u[1]) # angle from x-axis in radians
              theta <- theta * 360/(2*pi) # angle from x-axis in degrees
              theta <- theta - 90 # angle from y-axis in degrees
              
              ## Nodes to UTM
              nodes_int <- dets_p_int %>% 
                sf::st_as_sf(coords = c("gp_lon", "gp_lat")) %>%
                sf::st_set_crs(4326) %>%
                sf::st_transform(crs = crs) %>%
                dplyr::transmute(x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                                 y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2])
              
              ## Combine estimated locations with summary information
              tag_int_loc_est <- data.frame(bird_band = dets_p_int$bird_band[1],
                                            tag = dets_p_int$tag[1],
                                            dt_r = dets_p_int$dt_r[1],
                                            t_ind = dets_p_int$t_ind[1],
                                            n_gp = dets_p_int$n_gp[1],
                                            mean_RSSI = mean(dets_p_int$mean_rssi),
                                            max_RSSI = max(dets_p_int$mean_rssi),
                                            x_est = x_est,
                                            y_est = y_est,
                                            semi_major = a,
                                            semi_minor = b,
                                            orientation = theta)
              
              ##  Get raw points
              pe_df_p <- pe_df %>% 
                mutate(bird_band = dets_p_int$bird_band[1],
                       tag = dets_p_int$tag[1],
                       dt_r = dets_p_int$dt_r[1],
                       t_ind = dets_p_int$t_ind[1])
              
              
              # ## Plot
              # print(ggplot() +
              #         geom_point(data = ellipse_coords_proj, aes(x,y)) +
              #         # stat_ellipse(data = ellipse_coords_proj, aes(x,y),type = "norm") +
              #         ggforce::geom_ellipse(aes(x0=x_est,
              #                                   y0=y_est,
              #                                   a=semi_major,
              #                                   b=semi_minor,
              #                                   angle=orientation),
              #                               data = tag_int_loc_est,
              #                               color = "red") +
              #         theme_minimal() +
              #         geom_point(data=nodes_int,aes(x,y),color = "gold"))
              
              pe_df_list[int] <- list(pe_df_p)
              tag_loc_est_list[int] <- list(tag_int_loc_est)
              cov_list[int] <- list(ell.info$cov)
              
              tag_loc_est <- list(pe_df_list,
                                    tag_loc_est_list,
                                    cov_list)
              
              
            },
            error = function(e){ 
              "Too few detections to trilaterate location"
            }
            
          )
          
        }
        
        # ## Save as separate file
        # readr::write_csv(tag_loc_est,
        #                  paste0(output_folder,
        #                         "/ml_localized/",
        #                         band_f,
        #                         "/",
        #                         band_f_date,
        #                         ".csv.gz"))
        
        ## Save lists as R data file
        saveRDS(tag_loc_est,
                         paste0(output_folder,
                                "/ml_localized/",
                                band_f,
                                "/",
                                band_f_date,
                                ".RDS"))
        
        cat("\n Finished tag:", band_f, "- date:", band_f_date, "-", length(unique(dets_p$t_ind)), "intervals localized", 
            "after", round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1), "minutes \n")
        
        
      } else {
        
        cat("\n No intervals to localize, skipped file")
        
      }
      
    } 
    
  } else{
    
    cat("############ \n",
        "Finished localizing tag: ", band_f, "\n",
        "############ \n", sep = "")
    
  }
  
  ## End progress bar for files
  close(pb2)
  
  cat("############ \n",
      "Finished localizing tag: ", band_f, "\n",
      "############ \n", sep = "")
  
  
}