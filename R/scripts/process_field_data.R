## This script will process the raw field data files (e.g. node locations) to create new files for use in various scripts


## Get grid point locations from node location log ######
grid_pts <- readr::read_delim("/Users/tyson/Documents/academia/institutions/WUR/research/eswatini/field/Nodes/node_locations_Feb2022.csv",
                              delim = ";",
                              show_col_types = FALSE) %>% 
  dplyr::rename(grid_point = name) %>% 
  dplyr::mutate(grid_point = stringr::str_extract(grid_point, pattern = "^Gp[:digit:]{1,2}")) %>% 
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(22291)


saveRDS(grid_pts, here::here("./Eswatini/data/field_data/processed_field_data/grid_point_locations.RData"))
