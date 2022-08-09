## This script will process the raw field data files (e.g. node locations) to create new files for use in various scripts


## Get grid point locations from node location log ######
process_node_log <- function(node_log_file = as.character(),
                             output_folder = as.character()){
  
  grid_pts <- readr::read_delim(node_log_file,
                                delim = ";",
                                show_col_types = FALSE) %>% 
    dplyr::rename(grid_point = name) %>% 
    dplyr::mutate(grid_point = stringr::str_extract(grid_point, pattern = "^Gp[:digit:]{1,2}")) %>% 
    sf::st_as_sf(coords = c("lon", "lat")) %>% 
    sf::st_set_crs(4326) %>% 
    sf::st_transform(22291)
  
  saveRDS(grid_pts, paste0(output_folder,"processed_grid_point_locations.RData"))
  
  
}
