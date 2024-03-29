## Update localizations using range-free method
rf_update_localization <- function(tag_f = as.character(),
                                   db_name = as.character(),
                                   db_user = as.character(),
                                   db_password = as.character(),
                                   project = as.character(),
                                   tag_folder = as.character(),
                                   node_folder = as.character(),
                                   output_folder = as.character(),
                                   tz = "UTC"){
  
  cat("\n Starting to get new data for tag:", tag_f, "\n")
  

  ## Connect to data base back end
  conn <- DBI::dbConnect(RPostgres::Postgres(),
                         dbname = db_name,
                         password = db_password)
  
  ## Get most recent date file (if it exists)
  mrdf <- rev(list.files(paste0(output_folder,"data/processed_detections/rf/rf_prepared/w_error/60s/",tag_f,"/"),full.names = TRUE))[1]
  
  ## Get date time to filter by 
  if(length(mrdf)==1){
    mrd <- suppressWarnings(readr::read_csv(mrdf,show_col_types = FALSE) %>% 
                              dplyr::pull(dt_r) %>% 
                              max())
    mrd <- lubridate::with_tz(mrd, tz = tz)
  } else{
    mrd <- as.Date("2021-08-01")
  }
  
  
  ## Read in detections from database 
  dets_f <- dplyr::tbl(conn, from = "raw") %>%
    dplyr::filter(tag_id %in% tag_f) %>% 
    dplyr::filter(time > mrd) %>% 
    dplyr::collect() %>% 
    dplyr::transmute(tag = tag_id,
                     date_time = lubridate::with_tz(time, tz = tz),
                     node = toupper(node_id),
                     rssi = tag_rssi) %>% 
    dplyr::distinct(tag,
                    node,
                    date_time,
                    .keep_all = T)
  
  ## Read in node codes
  node_codes <- readxl::read_excel(paste0(node_folder,"node_codes.xlsx")) %>% 
    dplyr::mutate(node_number = as.character(node_number))
  
  node_codes <- readxl::read_excel("/Users/tyson/Documents/academia/institutions/WUR/research/australia/zebby_tracking/data/field_data/logs/node_master_list.xlsx") %>% 
    dplyr::mutate(node_number = as.character(node_number))
                                   
  
  ## Get most recent node log
  node_log_mr <- sort(list.files(paste0(node_folder),
                                 full.names = TRUE,
                                 pattern = "node_deployment_log"),
                      decreasing = TRUE)[1]
  
  ## Read in node log and reformat
  node_log <- suppressWarnings(readxl::read_excel(path = node_log_mr) %>%
                                 dplyr::mutate(deployment_time = lubridate::parse_date_time(paste(start_date, start_time), "dmy HM", tz = tz),
                                               removal_time = lubridate::parse_date_time(paste(end_date, end_time), "dmy HM", tz = tz)) %>%
                                 ## Join node node
                                 dplyr::left_join(node_codes,
                                                  by  = "node_number") %>% 
                                 dplyr::filter(!grepl("d",grid_point)) %>% 
                                 dplyr::select(node,
                                               grid_point,
                                               date_time = deployment_time,
                                               removal_time))
  
  ## Convert to data.table
  nodes <- data.table::data.table(node_log, key = c("node", "date_time"))
  dets_f <- data.table::data.table(dets_f, key = c("node", "date_time"))
  
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
  
  ## Add day column
  dets_t <- dets_f1 %>% 
    dplyr::mutate(date = lubridate::floor_date(date_time, unit = "day")) 
  
  cat("\n Finished getting new data for tag", tag_f, "\n")
  
  # ## Prepare each tag
  # rf_prepare_dets_error(tag_f = tag_f, 
  #                       dets_t = dets_t,
  #                       output_folder = output_folder,
  #                       tz = tz)
  # 
  ## Then localize
  rf_localizing_fn(tag_f = tag_f,
                   output_folder = output_folder,
                   tz = tz)
  
}
