#' Fetch field data from Google Drive and save a local copy
#'
#' @param output_folder Where should the field data be saved?
#' @param project What is the name of the project?
#' @param overwrite Should any local data be overwritten? Default is FALSE
#' @param update Should other derived files be updated? If TRUE, then this will update these files.

fetch_field_data <- function(google_drive_folder = as.character(),
                             project,
                             overwrite = FALSE,
                             update = TRUE){
  
  googledrive::drive_ls(path = google_drive_folder) %>%
    drive_reveal("permissions")
  
  ## Download all files and save to relevant project folder
  googledrive::drive_ls(path = google_drive_folder) %>% 
    split(.$id) %>% 
    purrr::walk(~googledrive::drive_download(.$id, 
                                             path = here::here("project", 
                                                               project, 
                                                               "data/field", .$name), 
                                             overwrite = overwrite))
  
  if(update == TRUE){
    
    ## Update node log file
    process_node_log(node_log_file = list.files(here::here("project", 
                                                           project,
                                                           "data/field"), 
                                                pattern = "grid_points",
                                                recursive = TRUE,
                                                full.names = TRUE),
                     output_folder = here::here(paste0("project/",project,"/data/processed/field_data/")))
    
  }
  
}