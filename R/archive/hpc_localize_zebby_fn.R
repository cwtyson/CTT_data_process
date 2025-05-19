## HPC localize
hpc_localize_zebby_fn <- function(file_f,
                                  rssi_dist_model_file,
                                  model_scale = c("log", "exp"),
                                  reps = 100){
  
  ## Shortened file name for printing
  file_f_ab <- paste(tail(unlist(strsplit(file_f,"/")),2),collapse="/")
  
  cat("############ \n",
      "Opening file: ", file_f_ab, "\n",
      "############ \n", sep = "")
  
  start_time = Sys.time()
  
  ## Read in prepared data
  dets_p <- readr::read_csv(file_f)
  
  # sample_ids = sample(dets_p$int_id, 20,replace = FALSE)
  # 
  # ## Sample
  # dets_p <- dets_p %>% 
  #   filter(int_id %in% sample_ids)
  
  ## New data to predict distance
  new_data = data.frame(rssi = seq(max(dets_p$rssi_gp_r), 
                                   min(dets_p$rssi_gp_r), 
                                   by = -1))
  
  ## Augment
  if(model_scale == "log"){
    
    ## Model
    rssi_dist_mdl <- readRDS(rssi_dist_model_file) 
    
    ## Model table
    mdl_tab <- broom::augment(rssi_dist_mdl,
                              newdata = new_data,
                              se_fit = TRUE) %>% 
      dplyr::select(rssi_gp_r = rssi,
                    mean = .fitted,
                    se = .se.fit)
    
    ## Add model data
    dets_p <- dets_p %>% 
      
      ## Join modeled rssi values
      left_join(mdl_tab, by = join_by(rssi_gp_r))
    
    
  } else{
    
    # ## Exp model file
    # rssi_dist_model_file = "./data/rssi_dist_models/RSSI_nls_dist_model_2023.RDS"
    # 
    ## Model
    rssi_dist_mdl <- readRDS(rssi_dist_model_file) 
    
    ## NLS model
    mdl_data =  broom::augment(rssi_dist_mdl) %>% 
      distinct(distance, rssi) %>% 
      arrange(desc(distance))
    
    ## Model parameters
    a = as.numeric(-coef(rssi_dist_mdl)[2])
    S = as.numeric(exp(coef(rssi_dist_mdl)[3]))
    K = as.numeric(coef(rssi_dist_mdl)[1])
    
    ## Set reps to 1
    reps = 1
    
    # tr <- invest(rssi_dist_mdl,
    #              data = mdl_data,
    #              mean.response = FALSE,
    #              y0 = -70,
    #              upper = 1000000,
    #              interval="Wald") 
    # 
    # ## Create data frame of predicted values
    # pred_df = map(.x = rev(sort(unique(mdl_data$rssi))),
    #               .f = function(.x)   tryCatch(expr = {invest(rssi_dist_mdl,
    #                                                           data = mdl_data,
    #                                                           mean.response = FALSE,
    #                                                           y0 = .x,
    #                                                           upper = 1000000,
    #                                                           interval="Wald") },
    #                                            
    #                                            ## If not defined, use mean value
    #                                            error = function(error)  { .x %>% 
    #                                                data.frame(estimate = ((log(.x - K) - log(a)) / -S))
    #                                              
    #                                              
    #                                              
    #                                            }))
  }
  
  
  ## Empty df
  tag_loc_est <- data.frame()
  
  ## Empty lists
  tag_loc_est_list <- list()
  cov_list <- list()
  pe_df_list <- list()
  
  cat("\n Intervals to localize:", max(dets_p$int_id),  "with", reps, "resamples") 
  
  ## Set progress bar
  pb <- txtProgressBar(min = 0, max = max(dets_p$int_id), style = 3)
  
  ## For each interval, localize X times
  for(int in unique(dets_p$int_id)){
    
    ## Progress bar
    Sys.sleep(0.1)
    setTxtProgressBar(pb, which(int == 1:unique(max(dets_p$int_id))))
    
    ## Subset detections
    dets_p_int = dets_p[dets_p$int_id == int,]
    
    ## Point estimates
    pe <- list()
    
    for(i in 1:reps){
      
      ## Estimate distance
      if(model_scale == "log"){
        
        ## Sample within interval
        dets_p_int <- dets_p_int %>% 
          dplyr::rowwise() %>%
          dplyr::mutate(dist_est_samp = round(10^(rnorm(n = 1, 
                                                        mean = mean,
                                                        sd = se)),
                                              0)) %>%
          dplyr::ungroup()
        
      } else{
        
        ## Sample within interval
        dets_p_int <- dets_p_int %>% 
          dplyr::rowwise() %>%
          dplyr::mutate(dist_est_samp = suppressWarnings(round((log(rssi_gp_r - K) - log(a)) / -S)))  %>%
          
          ## If less than 0, 0. If greater than 200 or NaN, 200
          dplyr::mutate(dist_est_samp = dplyr::case_when(dist_est_samp < 0 ~ 0,
                                                         dist_est_samp >= 200 ~ 200,
                                                         is.nan(dist_est_samp) ~ 200,
                                                         TRUE ~ dist_est_samp))
      }
      
      tryCatch(
        expr = {
          
          # Non-linear test to optimize the location estimate given pairwise distances
          nls_mod <- suppressWarnings(nls(dist_est_samp ~ raster::pointDistance(data.frame(gp_x, gp_y), 
                                                                                c(x_solution, y_solution),
                                                                                lonlat = F, 
                                                                                allpairs = T), 
                                          data = dets_p_int,
                                          start = list(x_solution = dets_p_int[1,]$gp_x,
                                                       y_solution = dets_p_int[1,]$gp_y),
                                          control = nls.control(warnOnly = T,
                                                                maxiter=20,
                                                                minFactor=1/3000)))
          
          ## Combine point estimates
          pe[[i]] <- c(coef(nls_mod)[1],
                       coef(nls_mod)[2])
          
        },
        
        ## Print error message
        error = function(e)  {cat("ERROR :",conditionMessage(e), "\n")})
      
    }
    
    
    ## Combine point estimates
    pe_df <- do.call(bind_rows,pe) %>% 
      data.frame()
    
    ## Ellipse info from X repeated multilateration estimates
    ell.info <- cov.wt(cbind(pe_df$x_solution, pe_df$y_solution))
    
    ## Center
    x_est <- ell.info$center[1]
    y_est <- ell.info$center[2]
    
    ## Axes
    eigen.info <- eigen(ell.info$cov)
    lengths <- sqrt(eigen.info$values * 2 * qf(.95, 2, reps-1))
    a = lengths[1]
    b = lengths[2]
    
    ## Orientation
    u <- eigen.info$vectors[,1] # major axis eigenvector
    theta <- atan2(u[2],u[1]) # angle from x-axis in radians
    theta <- theta * 360/(2*pi) # angle from x-axis in degrees
    theta <- theta-90 # angle from y-axis in degrees
    
    ## Combine estimated locations with summary information
    tag_int_loc_est <- dets_p_int %>% 
      select(where(~ n_distinct(.) == n_distinct(dets_p_int$int_id))) %>%     
      distinct() %>% 
      mutate(mean_RSSI = mean(dets_p_int$rssi_gp_r),
             max_RSSI = max(dets_p_int$rssi_gp_r),
             x_est = x_est,
             y_est = y_est,
             semi_major = a,
             semi_minor = b,
             orientation = theta)
    
    ##  Get raw points
    pe_df_p <- tag_int_loc_est %>% 
      left_join(pe_df %>% 
                  mutate(int_id = tag_int_loc_est$int_id),
                by = join_by(int_id))
    
    ## Add elements to list
    tag_loc_est_list[int] <- list(tag_int_loc_est)
    pe_df_list[int] <- list(pe_df_p)
    cov_list[int] <- list(ell.info$cov)
    
  }
  
  ## Combine estimated locations, raw point estimates, and covariance matrices
  tag_loc_est <- list(tag_loc_est_list %>% 
                        do.call(bind_rows,.),
                      pe_df_list %>% 
                        do.call(bind_rows,.),
                      cov_list)
  
  ## Name of outfile
  outfile = gsub(".csv.gz",
                 ".RDS",
                 gsub("/ml_prepared/",
                      "/ml_localized/",
                      file_f))
  
  ## Create tag directory if needed
  dir <- paste(head(unlist(strsplit(outfile,"/")),-1),collapse="/")
  if (!dir.exists(dir)) dir.create(dir,recursive = T)
  
  ## Save lists as R data file
  saveRDS(tag_loc_est,
          outfile)
  
  cat("\n Finished file:", file_f_ab, "-", length(unique(dets_p$int_id)), "intervals localized", 
      "after", round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1), "minutes \n")
  
} else {
  
  cat("\n No intervals to localize, skipped file:", file_f_ab)
  
}

