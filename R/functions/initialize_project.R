
initalize_project <- function(project = NULL,
                              google_drive_folder = NULL){
  
  ## Subdirectories to create
  subdirs <- c("data/field", 
               "data/processed/detections", 
               "data/processed/field_data",
               "data/processed/raw",
               "plots",
               "summaries")
  
  cat("Creating project subdirectories\n")
  
  ## Create subdirectories
  for(subdir in subdirs){
    
    ## Create necessary directories
    dir.create(path = here::here("project", 
                                 project, 
                                 subdir),
               recursive = TRUE)
    
  }
  
  ## If google drive folder is specified, get data
  if(!is.null(google_drive_folder)){
    
    cat("Fetching field data from Google Drive\n")
    
    fetch_field_data(google_drive_folder = google_drive_folder,
                     project = project,
                     overwrite = TRUE,
                     update = FALSE)
  }
}
