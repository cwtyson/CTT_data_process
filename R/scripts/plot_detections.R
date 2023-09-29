library(tidyverse)

locs <- vroom::vroom(list.files("/Users/tyson/Documents/academia/research/zebby_tracking/data/2023/localizations/ml_localized/",
                                recursive = T,
                                full.names = T,
                                ".csv"))

## Get grid point coordinates
grid_points <- suppressWarnings(sf::read_sf(paste0(grid_points_folder, "grid_points.kml")) %>% 
                                  sf::st_transform(crs) %>% 
                                  dplyr::transmute(grid_point = gsub("Gp ", "gp_", Name),
                                                   gp_x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                                                   gp_y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>% 
                                  sf::st_drop_geometry())


ggplot() +
  geom_point(aes(x=x_est,
                 y=y_est,
                 color = tag),
             data = locs) +
  geom_point(aes(x=gp_x,
                 y=gp_y),
             grid_points) +
  facet_wrap(~tag,scales= "free") +
  theme_minimal()



## Convert grid points to lat/long  ########
grid_points_ll <- suppressWarnings(grid_points %>%
                                     sf::st_as_sf(coords = c("gp_x","gp_y"),
                                                  crs = crs) %>% 
                                     sf::st_transform(4326) %>% 
                                     dplyr::transmute(grid_point,
                                                      gp_lon = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                                                      gp_lat = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>% 
                                     sf::st_drop_geometry())
