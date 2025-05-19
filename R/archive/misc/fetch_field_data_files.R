#' Fetch field data from Google Drive and save a local copy
#'
#' @param output_folder Where should the field data be saved?
#' @param project What is the name of the project?
#' @param overwrite Should any local data be overwritten? Default is FALSE. If TRUE, a new sub-folder will be created to archive the current data files.
#' @param update Should other derived files be updated? If TRUE, then this will update these files.

fetch_field_data <- function(google_drive_folder = as.character(),
                             project,
                             overwrite = FALSE,
                             update = TRUE){
  
  ## If overwrite TRUE, first archive files
  if(overwrite == TRUE){
    
    ## Archive directory to create
    archive_dir <- here::here("project", 
                              project, 
                              "data/field/archive",
                              gsub(" ","_", as.character(Sys.time())))
    
    ## Get current data files
    current_data_files <- list.files(here::here("project", 
                                                project, 
                                                "data/field"),
                                     pattern = ".csv|.xlsx",
                                     include.dirs = FALSE)
    
    ## Create new archive directory
    dir.create(archive_dir,
               recursive = TRUE)
    
    ## Move files to archive folder
    file.rename(from = list.files(here::here("project", 
                                             project, 
                                             "data/field"),
                                  pattern = ".csv|.xlsx",
                                  full.names = TRUE),
                to = paste0(archive_dir,
                            "/",
                            current_data_files))
    
  }
  
  ## Get files
  googledrive::drive_ls(path = google_drive_folder) %>%
    googledrive::drive_reveal("permissions")
  
  ## Download all files and save to relevant project folder
  googledrive::drive_ls(path = google_drive_folder) %>% 
    split(.$id) %>% 
    purrr::walk(~googledrive::drive_download(.$id, 
                                             path = here::here("project", 
                                                               project, 
                                                               "data/field", .$name), 
                                             overwrite = overwrite))

  ## If update is TRUE, process files
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