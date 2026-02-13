## Collect raw data
collect_fn <- function(band_f,
                       db_name,
                       tag_log,
                       station_ids,
                       node_folder,
                       output_folder,
                       tz = tz){
  
  cat("############ \n",
      "Collecting raw data for band: ", band_f, "\n",
      "############ \n", 
      sep = "")
  
  ## Filter tag log
  tag_log = tag_log[tag_log$bird_band == band_f,]
  
  ## Time filters
  tag_start_dt <- min(tag_log$tag_start_time)
  tag_end_dt <- max(tag_log$tag_removal_time)
  year = format(tag_start_dt, "%Y")
  
  ## Tag(s) based on focal band
  tags_f <- unique(tag_log$tag)
  
  ## Connection to database
  conn <- DBI::dbConnect(drv = duckdb::duckdb(dbdir = db_name,config = list("access_mode" = "READ_ONLY")))
  
  ## Read in detections from database
  dets_raw_og <- dplyr::tbl(conn, "raw") %>%
    
    ## Keep only station ids matching the specified filter
    dplyr::filter(station_id %in% station_ids) %>%
    
    ## Keep focal tag(s)
    dplyr::filter(tag_id %in% tags_f) %>%
    
    ## Keep detections after deployment time and before removal time
    dplyr::filter(time > tag_start_dt) %>%
    dplyr::filter(time  <= tag_end_dt) %>%
    
    ## Distinct
    dplyr::distinct(tag_id,
                    node_id,
                    time,
                    .keep_all = T) %>%
    
    dplyr::collect() %>%
    
    ## Select and rename
    dplyr::transmute(node = toupper(node_id),
                     date_time = lubridate::with_tz(time, tz = tz),
                     tag = tag_id,
                     rssi = tag_rssi) %>%
    
    dplyr::arrange(date_time)
  
  ## Read in detections from postgres database
  dets_raw_blu <- dplyr::tbl(conn, "blu") %>%
    
    ## Keep only station ids matching the specified filter
    dplyr::filter(station_id %in% station_ids) %>%
    
    ## Keep focal tag(s)
    dplyr::filter(tag_id %in% tags_f) %>%
    
    ## Keep detections after deployment time and before removal time
    dplyr::filter(time > tag_start_dt) %>%
    dplyr::filter(time  <= tag_end_dt) %>%
    
    ## Distinct
    dplyr::distinct(tag_id,
                    node_id,
                    time,
                    .keep_all = T) %>%
    
    dplyr::collect() %>%
    
    ## Select and rename
    dplyr::transmute(node = toupper(node_id),
                     date_time = lubridate::with_tz(time, tz = tz),
                     tag = tag_id,
                     rssi = tag_rssi) %>%
    
    dplyr::arrange(date_time)
  
  ## Combine
  dets_raw <- bind_rows(dets_raw_og, dets_raw_blu)
  
  # ## Check raw data
  # ggplot2::ggplot(dets_raw) +
  #   ggplot2::geom_point(ggplot2::aes(x=date_time,
  #                                    shape=tag,
  #                                    y=node,
  #                                    color = rssi)) +
  #   ggplot2::theme_minimal()
  # 
  cat("############ \n",
      "Finished collecting raw data for band: ", band_f, "\n",
      "############ \n", 
      sep = "")
  
  
  if(nrow(dets_raw) > 0){
    
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
                                   dplyr::select(grid_point, node_number, start_date, start_time, end_date, end_time) %>% 
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
    
    ## Associate tag with correct band 
    tag_log <- tag_log %>% 
      dplyr::rename(date_time = tag_start_time) %>% 
      dplyr::select(bird_band,
                    tag,
                    date_time,
                    tag_removal_time)
    tag_log <- data.table::data.table(tag_log, key = c("tag", "date_time"))
    dets_f1 <- data.table::data.table(dets_f1, key = c("tag", "date_time"))
    
    ## Rolling join
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
      dplyr::mutate(date = lubridate::floor_date(date_time, unit = "1 day"),
                    grid_point = grid_point)
    
    # Summarize filtered data by day by grid point
    dets_sum <- dets_t %>% 
      dplyr::group_by(tag, date, grid_point) %>% 
      dplyr::summarise(dets = n(),
                       mean_rssi = mean(rssi),
                       .groups = "keep")
    
    ## Check filtered data
    dets_sum_plot <- ggplot2::ggplot(dets_sum) +
      ggplot2::geom_point(ggplot2::aes(x=date,
                                       y=grid_point,
                                       shape=tag,
                                       color = mean_rssi,
                                       size = dets)) +
      ggplot2::scale_colour_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"), name = "rssi") +
      ggplot2::theme_minimal()
    
    
    ## Create directory if missing
    if(!dir.exists(paste0(output_folder,
                          "/raw_detections/", 
                          year,
                          "/plots/"))){dir.create(paste0(output_folder,
                                                         "/raw_detections/", 
                                                         year,
                                                         "/plots/"), recursive = T)}
    
    ## Create directory if missing
    if(!dir.exists(paste0(output_folder,
                          "/raw_detections/", 
                          year,
                          "/data/"))){dir.create(paste0(output_folder,
                                                        "/raw_detections/", 
                                                        year,
                                                        "/data/"), recursive = T)}
    
    
    ## Plot
    ggplot2::ggsave(plot = dets_sum_plot,
                    filename = paste0(output_folder,
                                      "/raw_detections/",
                                      year,
                                      "/plots/", 
                                      band_f,
                                      "_detection_summmary.jpg"),
                    scale = 2,
                    width = 7,
                    height = 7,
                    create.dir = TRUE)
    
    
    ## Save raw data
    saveRDS(dets_t,
            paste0(output_folder,
                   "/raw_detections/", 
                   year, 
                   "/data/",
                   band_f,
                   ".RDS"))
    
    ## Garbage cleanup
    gc()
    rm(dets_t)
    
    cat("############ \n",
        "Finished cleaning raw data for band: ", band_f, "\n",
        "############ \n", 
        sep = "")
    
  } else{
    
    
    cat("############ \n",
        "No raw data for band: ", band_f, "\n",
        "############ \n", 
        sep = "")
    
  }
  
}
