#' Process node health files
#'
#' @param db_name Name of postgres database
#' @param db_user User name for postgres database
#' @param db_password Password for db user 
#' @param node_log_file Node log file (.xlsx). This file says when a node was associated with a grid point.
#' @param sensor_station_code Code of sensor stations to use. If not specified, all sensor stations will be used. Multiple sensor station codes can be accepted as a vector.
#' @param tz Time zone. Time zone where the sensor station is located. Should be one of OlsonNames().
#' @param output_folder Location to save RData file

process_nodes <- function(db_name = as.character(),
                          db_user = as.character(),
                          db_password = as.character(),
                          node_log_file = as.character(),
                          sensor_station_code = NULL,
                          tz = "UTC",
                          output_folder = as.character()){
  
  cat("Starting to process node data\n")
  
  ## Connect to data base back end
  conn <- DBI::dbConnect(RPostgres::Postgres(),
                         dbname = db_name,
                         user = db_user,
                         password = db_password)
  
  ## Station id is null, use all
  if(is.null(sensor_station_code)){
    
    sensor_station_code <- dplyr::tbl(conn, "node_health") %>%
      dplyr::distinct(station_id) %>% 
      dplyr::collect() %>% 
      dplyr::pull(station_id)
    
  }
  
  ## Read in node health data base
  nodes_db <- dplyr::tbl(conn, "node_health") %>%
    
    ## Keep only station ids matching the specified filter
    dplyr::filter(station_id %in% sensor_station_code) %>% 
    dplyr::collect() %>%
    
    ## Select (and rename) relevant columns
    dplyr::transmute(node = toupper(node_id),
                     date_time = lubridate::with_tz(time, tz = tz),
                     node_rssi = node_rssi,
                     battery,
                     lat = latitude,
                     lon = longitude,
                     solar_volts,
                     solar_current) %>%
    
    ## Arrange
    dplyr::arrange(node,
                   date_time)
  
  ## Read in node log and reformat
  node_log <- suppressWarnings(readxl::read_excel(path = node_log_file) %>%
                                 dplyr::mutate(deployment_time = lubridate::parse_date_time(paste(date_on, time_on), "dmy HM", tz = tz),
                                               removal_time = lubridate::parse_date_time(paste(date_off, time_off), "dmy HM"), tz = tz) %>%
                                 dplyr::select(node = node_code,
                                               grid_point,
                                               date_time = deployment_time,
                                               removal_time))
  
  ## Convert to data.table
  nodes_db <- data.table::data.table(nodes_db, key = c("node", "date_time"))
  node_log <- data.table::data.table(node_log, key = c("node", "date_time"))
  
  ## Rolling join node log to node records
  nodes_db <- node_log[nodes_db, roll = Inf]
  
  ## Process
  nodes_db_p <- nodes_db %>%
    
    ## Convert to tibble
    tibble::as_tibble() %>%
    
    ## Filter out node records before they were deployed
    dplyr::filter(!is.na(grid_point),
                  (date_time < removal_time | is.na(removal_time))) %>%
    
    ## Drop unnecessary variables
    dplyr::select(-removal_time) %>% 
    
    ## Distinct
    dplyr::distinct(grid_point,
                    date_time,
                    .keep_all = T)
  
  ## Save as RData
  saveRDS(nodes_db_p,
          file = paste0(output_folder,
                        "/node_health.Rdata"))
  
  cat("Saved updated node data\n")
  
}
