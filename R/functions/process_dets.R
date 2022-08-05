#' Process detection data
#'
#' @param db_name Name of postgres database
#' @param db_user User name for postgres database
#' @param db_password Password for db user 
#' @param tag_log_file Tag log file (.xlsx). This file has the deployed tags and information about when the tag was deployed, the species, etc.
#' @param node_log_file Node log file (.xlsx). This file has the times when a node was associated with a grid point.
#' @param sensor_station_code Code of sensor stations to use.  Multiple sensor station codes can be accepted as a vector.
#' @param tz Time zone. Time zone where the sensor station is located. Should be one of OlsonNames().
#' @param output_folder Location to save RData file

process_dets <- function(db_name = as.character(),
                         db_user = as.character(),
                         db_password = as.character(),
                         tag_log_file = as.character(),
                         sensor_station_code = as.character(),
                         node_log_file = as.character(),
                         tz = "UTC",
                         output_folder = as.character()){
  
  ## Connect to data base back end
  dets_db <- DBI::dbConnect(RPostgres::Postgres(),
                            dbname = db_name,
                            user = db_user,
                            password = db_password) %>%
    dplyr::tbl("raw")
  
  cat("Starting to process detection data\n")
  
  ## Read in detections from postgres database
  dets_r <- dets_db %>%
    
    ## Keep only station ids matching the specified filter
    dplyr::filter(station_id %in% sensor_station_code) %>% 
    dplyr::collect() %>% 
    dplyr::distinct(tag_id,
                    node_id,
                    time,
                    .keep_all = T) 
  
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
          file = here::here(output_folder,
                            "dets_all.Rdata"))
  
  cat("Saved raw detection data\n")
  
  ## Read in tag data, keeping deployed tags and reformating
  tags <- readxl::read_excel(path = tag_log_file) %>%
    dplyr::filter(!is.na(bird_band)) %>%
    dplyr::mutate(deployment_time = lubridate::parse_date_time(paste(date, time),
                                                               orders = "%d-%m-%Y %H:%M",
                                                               tz = tz)) %>%
    dplyr::select(tag,
                  deployment_time)
  
  ## Filter out detections from before the tag was deployed
  dets_f <- dets_p %>%
    dplyr::left_join(tags,
                     by = "tag") %>%
    
    dplyr::filter(date_time > deployment_time) %>%
    na.omit() %>%
    dplyr::select(-deployment_time)
  
  ## Read in node data and reformat
  node_log <- readxl::read_excel(path = node_log_file) %>%
    dplyr::mutate(deployment_time = lubridate::parse_date_time(paste(date_on, time_on), "dmy HM", tz = tz),
                  removal_time = lubridate::parse_date_time(paste(date_off, time_off), "dmy HM", tz = tz)) %>%
    dplyr::select(node = node_code,
                  grid_point,
                  date_time = deployment_time,
                  removal_time)
  
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
          file = here::here(output_folder,
                            "dets_filtered.Rdata"))

  cat("Saved filtered detection data\n")
  
}
