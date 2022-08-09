#' Fetch field data from Google Drive and save a local copy
#'
#' @param output_folder Where should the field data be saved?
#' @param overwrite Should any local data be overwritten? Default is FALSE

fetch_field_data <- function(output_folder = as.character(),
                             overwrite = FALSE){
  
  ## Download all files and save to specified output folder
  googledrive::drive_ls(path='Eswatini_field_data/') %>% 
    split(dir$id) %>% 
    purrr::walk(~googledrive::drive_download(.$id, path = file.path(output_folder, .$name), 
                                             overwrite = overwrite))
  
}





