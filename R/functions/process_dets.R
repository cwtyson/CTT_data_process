#' Process detection data
#'
#' @param db_name Name of postgres database
#' @param db_user User name for postgres database
#' @param db_password Password for db user 
#' @param project Location of project. This will folder must have a subdirectory 'data/field' with the appropriate field logs.
#' @param sensor_station_code Code of sensor stations to use. If not specified, all sensor stations will be used. Multiple sensor station codes can be accepted as a vector.
#' @param tz Time zone. Time zone where the sensor station is located. Should be one of OlsonNames().

process_dets <- function(db_name = as.character(),
                         db_user = as.character(),
                         db_password = as.character(),
                         project = as.character(),
                         sensor_station_code = NULL,
                         node_log_file = as.character(),
                         tz = "UTC"){
  
  cat("Starting to process detection data\n")
  
  ## Connect to data base back end
  conn <- DBI::dbConnect(RPostgres::Postgres(),
                         dbname = db_name,
                         user = db_user,
                         password = db_password) 
  
  ## Station id is null, use all
  if(is.null(sensor_station_code)){
    
    sensor_station_code <- dplyr::tbl(conn, "raw") %>%
      dplyr::distinct(station_id) %>% 
      dplyr::collect() %>% 
      dplyr::pull(station_id)
    
  }
  
  ## Read in detections from postgres database
  dets_r <- dplyr::tbl(conn, "raw") %>%
    
    ## Keep only station ids matching the specified filter
    dplyr::filter(station_id %in% sensor_station_code) %>% 
    dplyr::distinct(tag_id,
                    node_id,
                    time,
                    .keep_all = T) %>% 
    dplyr::collect()
    
  ## Process
  dets_p <- dets_r %>%
    
    ## Select and rename
    dplyr::transmute(node = toupper(node_id),
                     date_time = lubridate::with_tz(time, tz = tz),
                     tag = tag_id,
                     rssi = tag_rssi) %>%
    
    ## Set NAs in node to '000000' - The Sensor Station code
    dplyr::mutate(node = ifelse(is.na(node), '000000', node)) 
  
  ## Save as RData
  saveRDS(dets_p,
          file = here::here("project",
                            project,
                            "data/processed/raw/dets_all.Rdata"))
  
  cat("Saved raw detection data\n")
  
  ## Read in tag data, keeping deployed tags and reformating
  tags <- readxl::read_excel(path = here::here("project", project, "data/field/tags/tag_log.xlsx")) %>%
    dplyr::filter(!is.na(bird_band)) %>%
    dplyr::mutate(deployment_time = lubridate::parse_date_time(paste(date, time),
                                                               orders = "%d-%m-%Y %H:%M",
                                                               tz = tz)) %>%
    dplyr::select(tag,
                  deployment_time) %>% 
    dplyr::filter(tag != "removed")
  
  ## Filter out detections from before the tag was deployed
  dets_f <- dets_p %>%
    dplyr::left_join(tags,
                     by = "tag") %>%
    
    dplyr::filter(date_time > deployment_time) %>%
    na.omit() %>%
    dplyr::select(-deployment_time)
  
  ## Read in node codes
  node_codes <- readxl::read_excel(path = here::here("project", project, "data/field/nodes/node_codes.xlsx"))
  
  ## Read in node log and reformat
  node_log_mr <- sort(list.files(here::here("project", project, "data/field/nodes/"),
                                 full.names = TRUE,
                                 pattern = "node_deployment_log"),
                      decreasing = TRUE)[1]
  
  
  ## Read in node data and reformat
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
  
  ## Save as RData
  saveRDS(dets_f1,
          file = here::here("project",
                            project,
                            "data/processed/raw/dets_filtered.Rdata"))
  
  cat("Saved filtered detection data\n")
  
}
