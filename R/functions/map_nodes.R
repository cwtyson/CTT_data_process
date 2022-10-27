## Map nodes
library(tidyverse)
library(sf)
library(ggmap)

## Read in node data
nodes <- readRDS(here::here("project",
                            project,
                            "data/processed/raw/node_health.Rdata")) %>%
  dplyr::filter(grepl("Pp", grid_point))

## Read in node summary
nodes_sum <- nodes %>%
  dplyr::rename(pole_point = grid_point) %>%
  # dplyr::mutate(day = format(date_time, "%D")) %>%
  dplyr::group_by(pole_point) %>%
  dplyr::summarise(check_ins = n(),
                   mean_rssi = mean(node_rssi))

## Read in node codes
node_codes <- readxl::read_excel(path = here::here("project", project, "data/field/nodes/node_codes.xlsx"))

## Read in node log and reformat
node_log_mr <- sort(list.files(here::here("project", project, "data/field/nodes"),
                               full.names = TRUE,
                               pattern = "node_deployment_log"),
                    decreasing = TRUE)[1]

## Get node deployment log
node_log <- suppressWarnings(readxl::read_excel(path = node_log_mr) %>%
                               dplyr::mutate(deployment_time = lubridate::parse_date_time(paste(start_date, start_time), "dmy HM", tz = tz),
                                             removal_time = lubridate::parse_date_time(paste(end_date, end_time), "dmy HM", tz = tz)) %>%
                               ## Join node node
                               dplyr::left_join(node_codes,
                                                by  = "node_number") %>%
                               
                               dplyr::select(node,
                                             grid_point,
                                             deployment_time,
                                             removal_time)) %>%
  dplyr::filter(grepl("Pp", grid_point)) %>%
  dplyr::rename(pole_point = grid_point)

## Get most recent grid point
grid_points_mr <- sort(list.files(here::here("project", project, "data/field/nodes"),
                                  full.names = TRUE,
                                  pattern = "points"),
                       decreasing = TRUE)[1]

## Get node coordinates
gps <- suppressWarnings(sf::read_sf(grid_points_mr)  %>%
                          janitor::clean_names() %>%
                          dplyr::transmute(pole_point = name) %>%
                          dplyr::mutate(x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                                        y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]))


nodes_j <- nodes_sum %>%
  
  data.frame() %>%
  ## Join node log
  dplyr::left_join(node_log,
                   by = "pole_point") %>%
  dplyr::left_join(gps,
                   by = "pole_point") %>%
  sf::st_as_sf(coords=c("x","y"), crs= 4326) %>%
  dplyr::mutate(field_time = as.numeric(difftime(Sys.time(),
                                                 deployment_time,
                                                 units = "mins"))/5,
                check_in_rate = round(check_ins/field_time, 3))


## Map
ggmap::register_google(key = "AIzaSyCjJjtwV1nnlkRyq_dAUACftUqGdApvchg")
map <- ggmap::get_map(location = as.numeric(sf::st_centroid(nodes_j) %>%
                                              sf::st_coordinates() %>%
                                              data.frame() %>%
                                              summarise(mean_x = mean(X),
                                                        mean_y = mean(Y))),
                      zoom = 15,
                      maptype = "satellite")

## Plot node health
(node_map <- ggmap(map) +
    
    geom_sf_text(data = gps %>%
                   filter(!(pole_point %in% nodes_j$pole_point)),
                 aes(label = gsub("Pp","",pole_point)),
                 size= 3,
                 inherit.aes = FALSE,
                 color = "white") +
    geom_sf_text(aes(color = check_in_rate,
                     label = gsub("Pp","",pole_point)),
                 data = nodes_j,
                 size= 3,
                 inherit.aes = FALSE) +
    theme_minimal() +
    ggplot2::scale_colour_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous")))

## Save node map
suppressMessages(ggplot2::ggsave(here::here("project",
                                            project,
                                            paste0("plots/nodes/node_map_plot.jpg")),
                                 plot = node_map))
