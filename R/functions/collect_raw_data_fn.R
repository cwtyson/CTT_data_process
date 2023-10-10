## Collect raw data
collect_raw_data_fn <- function(db_name = db_name,
                                db_password = db_password,
                                output_folder = output_folder,
                                tag_folder = tag_folder,
                                node_folder = node_folder,
                                grid_points_folder = grid_points_folder,
                                band_f = band_f,
                                tz = tz,
                                crs=crs){
  
  cat("############ \n",
      "Collecting raw data for band: ", band_f, "\n",
      "############ \n", 
      sep = "")
  
  ## Connect to data base back end
  conn <- DBI::dbConnect(RPostgres::Postgres(),
                         dbname = db_name,
                         password = db_password)
  
  ## Get most recently prepared data file (if it exists)
  mrdf <- rev(list.files(paste0(output_folder,"/ml_prepared/",band_f,""),full.names = TRUE, pattern = ".csv.gz"))[1]
  
  ## Get date time to filter by
  if(!is.na(mrdf)){
    mrd <- suppressWarnings(readr::read_csv(mrdf,show_col_types = FALSE) %>%
                              dplyr::pull(dt_r) %>%
                              max())
    mrd <- lubridate::with_tz(mrd, tz = tz)
  } else{
    mrd <- as.Date("2021-08-01")
  }
  
  ## Read in tag log and reformat
  tag_log_mr <- sort(list.files(paste0(tag_folder),
                                full.names = TRUE,
                                pattern = "tag_log"),
                     decreasing = TRUE)[1]
  
  ## Read in most recent tag log 
  tag_log <- suppressWarnings(readxl::read_excel(paste0(tag_log_mr))  %>%
                                janitor::clean_names() %>%
                                dplyr::transmute(tag = gsub("NA",NA, tag),
                                                 bird_band,
                                                 tag_start_time = lubridate::parse_date_time(paste(date, time), 
                                                                                             orders = c("%d-%m-%y %H:%M",
                                                                                                        "%Y-%m-%d %H:%M"),
                                                                                             tz = tz),
                                                 tag_removal_time = lubridate::parse_date_time(paste(tag_removal_date, tag_removal_time), 
                                                                                               orders = c("%d-%m-%y %H:%M",
                                                                                                          "%Y-%m-%d %H:%M"),
                                                                                               tz = tz))  %>%
                                dplyr::filter(bird_band == band_f) %>%
                                dplyr::select(bird_band,
                                              tag,
                                              tag_start_time,
                                              tag_removal_time) %>% 
                                dplyr::mutate(tag_removal_time = if_else(is.na(tag_removal_time), Sys.time(), tag_removal_time)))
  
  ## Time filters
  tag_start_dt <- min(tag_log$tag_start_time)
  tag_end_dt <- max(tag_log$tag_removal_time)
  
  ## Tag(s) based on focal band
  tags_f <- unique(tag_log$tag)
  
  ## Read in detections from database
  dets_raw <- dplyr::tbl(conn, from = "raw") %>%
    
    ## Keep focal tag(s)
    dplyr::filter(tag_id %in% tags_f) %>%
    
    ## Keep only data on or after most recently processed day
    dplyr::filter(time >= mrd) %>%
    
    ## Remove detections before/after removal times 
    ## TODO: Update removal times to interval to account for period between deployments
    dplyr::filter(time > tag_start_dt) %>%
    dplyr::filter(time  <= tag_end_dt) %>%
    
    dplyr::collect() %>%
    dplyr::transmute(tag = tag_id,
                     date_time = lubridate::with_tz(time, tz = tz),
                     node = toupper(node_id),
                     rssi = tag_rssi) %>%
    dplyr::distinct(tag,
                    node,
                    date_time,
                    .keep_all = T)
  
  cat("############ \n",
      "Finished collecting raw data for band: ", band_f, "\n",
      "############ \n", 
      sep = "")
  
  
  if(nrow(dets_f) > 0){
    
    cat("############ \n",
        "Cleaning raw data for band: ", band_f, "\n",
        "############ \n", 
        sep = "")
    
    
    ## Read in tag log and reformat
    node_codes_mr <- sort(list.files(node_folder,
                                     full.names = TRUE,
                                     pattern = "node_codes"),
                          decreasing = TRUE)[1]
    
    
    ## Read in node codes
    node_codes <- readxl::read_xlsx(node_codes_mr) %>%
      dplyr::mutate(node_number = as.character(node_number))
    
    ## Read in node log and reformat
    node_log_mr <- sort(list.files(paste0(node_folder),
                                   full.names = TRUE,
                                   pattern = "node_deployment_log"),
                        decreasing = TRUE)[1]
    
    node_log <- suppressWarnings(readxl::read_excel(path = node_log_mr) %>%
                                   dplyr::mutate(deployment_time = lubridate::parse_date_time(paste(start_date, start_time), "dmy HM", tz = tz),
                                                 removal_time = lubridate::parse_date_time(paste(end_date, end_time), "dmy HM", tz = tz)) %>%
                                   ## Join node node
                                   dplyr::left_join(node_codes,
                                                    by  = "node_number") %>%
                                   
                                   dplyr::select(node,
                                                 grid_point,
                                                 date_time = deployment_time,
                                                 removal_time))
    ## Convert to data.table
    nodes <- data.table::data.table(node_log, key = c("node", "date_time"))
    dets_f <- data.table::data.table(dets_raw, key = c("node", "date_time"))
    
    ## Rolling join node log to node records
    dets_f <- nodes[dets_f, roll = Inf]
    
    ## Remove locations without a grid point and where date time is after removal time
    dets_f1 <- dets_f %>%
      dplyr::filter(!is.na(grid_point),
                    (date_time < removal_time | is.na(removal_time))) %>%
      
      ## Remove impossible RSSI values
      dplyr::filter(rssi < 0) %>%
      
      dplyr::arrange(tag,
                     date_time) %>%
      dplyr::select(grid_point,
                    tag,
                    date_time,
                    rssi) %>%
      data.frame()
    
    ## Associate tag with correct band Convert to data.table and do a roiling join.
    tag_log <- tag_log %>% 
      dplyr::rename(date_time = tag_start_time) %>% 
      dplyr::select(bird_band,
                    tag,
                    date_time,
                    tag_removal_time)
    tag_log <- data.table::data.table(tag_log, key = c("tag", "date_time"))
    dets_f1 <- data.table::data.table(dets_f1, key = c("tag", "date_time"))
    
    ## Rolling join node log to node records
    dets_f1 <- tag_log[dets_f1, roll = Inf]
    
    ## Remove tags without a band and where detection time is after removal time
    dets_f2 <- dets_f1 %>%
      dplyr::filter(!is.na(bird_band),
                    (date_time < tag_removal_time | is.na(tag_removal_time))) %>%
      dplyr::arrange(tag,
                     date_time) %>%
      dplyr::select(grid_point,
                    bird_band,
                    tag,
                    date_time,
                    rssi) %>%
      data.frame()
    
    ## Add day column
    dets_t <- dets_f2 %>%
      dplyr::mutate(date = lubridate::floor_date(date_time, unit = "day"),
                    grid_point = grid_point)
    
    ggplot(dets_t) +
      geom_point(aes(x=date,
                     y=grid_point))
    
    cat("############ \n",
        "Finished cleaning raw data for band: ", band_f, "\n",
        "############ \n", 
        sep = "")
    
    return(dets_t)
    

    
  } else{
    
    
    cat("############ \n",
        "No raw data for band: ", band_f, "\n",
        "############ \n", 
        sep = "")
    
  }
  
}
