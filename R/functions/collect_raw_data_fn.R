## Collect raw data
collect_raw_data_fn <- function(band_f = band_f,
                                db_name = db_name,
                                station_ids = station_ids,
                                tag_log = tag_log,
                                node_folder = node_folder,
                                output_folder = output_folder,
                                tz = tz){
  
  cat("############ \n",
      "Collecting raw data for band: ", band_f, "\n",
      "############ \n", 
      sep = "")
  
  ## Tag log for focal band
  tag_log_f <- tag_log_all %>% 
    dplyr::filter(bird_band == band_f)
  
  ## Time filters
  tag_start_dt <- min(tag_log_f$tag_start_time)
  tag_end_dt <- max(tag_log_f$tag_removal_time)
  
  ## Tag(s) based on focal band
  tags_f <- unique(tag_log_f$tag)
  
  ## Connection to database
  conn <- DBI::dbConnect(duckdb::duckdb((dir = db_name)))
  
  # ## Check connection
  # DBI::dbListTables(conn)
  # 
  ## Read in detections from database
  dets_raw <- dplyr::tbl(conn, "raw") %>%
    
    ## Keep only station ids matching the specified filter
    duckplyr::filter(station_id %in% station_ids) %>%
    
    ## Keep focal tag(s)
    duckplyr::filter(tag_id %in% tags_f) %>%
    
    ## Keep detections after deployment time and before removal time
    duckplyr::filter(time > tag_start_dt) %>%
    duckplyr::filter(time <= tag_end_dt) %>%
    
    ## Distinct
    duckplyr::distinct(tag_id,
                       node_id,
                       time,
                       .keep_all = T) %>%
    
    dplyr::collect() %>%
    
    ## Select and rename
    dplyr::transmute(node = toupper(node_id),
                     date_time = lubridate::with_tz(time, tz = tz),
                     tag = tag_id,
                     rssi = tag_rssi) %>%
    duckplyr::arrange(date_time)
  
  
  # ## Check raw data
  # ggplot(dets_raw) +
  #   geom_point(aes(x=date_time,
  #                  y=node,
  #                  color = rssi))
  
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
                                   dplyr::mutate(deployment_time = lubridate::parse_date_time(paste(start_date, start_time), "dmy HM", tz = tz),
                                                 removal_time = lubridate::parse_date_time(paste(end_date, end_time), "dmy HM", tz = tz)) %>%
                                   dplyr::filter(location=="gp") %>% 
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
    tag_log <- tag_log_f %>% 
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
    
    ## Summarize filtered data by day by grid point
    dets_sum <- dets_t %>% 
      dplyr::group_by(date, grid_point) %>% 
      dplyr::summarise(dets = n(),
                       mean_rssi = mean(rssi),
                       .groups = "keep")
    
    ## Check filtered data
    dets_sum_plot <- ggplot2::ggplot(dets_sum) +
      ggplot2::geom_point(ggplot2::aes(x=date,
                              y=grid_point,
                              color = mean_rssi,
                              size = dets)) +
      ggplot2::scale_colour_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"), name = "rssi") +
      ggplot2::theme_minimal()
    
    
    ggplot2::ggsave(plot = dets_sum_plot,
                    filename = paste0(output_folder,
                                      "/raw_detections/plots/", 
                                      band_f,
                                      "_detection_summary.jpg"),
                    scale = 2,
                    create.dir = TRUE)
    
    ## Save raw data
    saveRDS(dets_t,
            paste0(output_folder,"/raw_detections/data/",band_f,".RDS")) 
    
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
