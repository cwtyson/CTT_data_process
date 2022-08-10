#' Process grid point file to  
#'
#' @param grid_point_file Location of the grid point file with the names (written as 'Gp' followed by the number) and the coordinates (lat/lon) of the grid point.
#' @param output_folder Folder to save the processed data
#' @param projected_crs Projected CRS to use

process_grid_points <- function(grid_point_file = as.character(),
                             output_folder = as.character(),
                             projected_crs = 22291){
  
  grid_pts <- readr::read_delim(grid_point_file,
                                delim = ";",
                                show_col_types = FALSE) %>% 
    dplyr::rename(grid_point = name) %>% 
    dplyr::mutate(grid_point = stringr::str_extract(grid_point, pattern = "^Gp[:digit:]{1,2}")) %>% 
    sf::st_as_sf(coords = c("lon", "lat")) %>% 
    sf::st_set_crs(4326) %>% 
    sf::st_transform(projected_crs)
  
  saveRDS(grid_pts, paste0(output_folder,"processed_grid_point_locations.RData"))
  
  
}
