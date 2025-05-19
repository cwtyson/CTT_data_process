
#' @param project 
#' @param tags Tags to prepare. If NULL, then any tag with detections will be prepared.

plot_localized_dets <- function(project = "Eswatini",
                                tags = NULL){
  
  
  ## Set up folders and files based on project
  input_folder = here::here("project",
                            project,
                            "data/processed/detections/localized/")
  output_folder = here::here("project",
                             project,
                             "plots/detections/localized/")
  
  ## Grid points
  grid_point_file =  here::here("project",
                                project,
                                "data/processed/field_data/grid_point_locations.RData")
  
  ## Tags to process
  if(is.null(tags)){
    
    ## If not specified, then use all tags from input folder
    tags <- list.files(path = input_folder)
    
  }
  
  ## Grid point locations, transformed to lat/lon
  grid_pts <- readRDS(grid_point_file) %>%
    sf::st_transform(22291) %>% 
    dplyr::transmute(gp = stringr::str_extract(grid_point, pattern = "^Gp[:digit:]{1,2}"),
                     x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                     y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>% 
    sf::st_drop_geometry()
  
  
  ## For each tag
  for(tag_f in tags){
    
    ## Read in all localized detections
    tag_dets <- vroom::vroom(list.files(paste0(input_folder, tag_f), full.names = TRUE),
                             show_col_types = FALSE) %>% 
      
      ## Process
      dplyr::mutate(hour = format(lubridate::round_date(dt_r, unit = "hour"), "%H")) %>% 
      dplyr::arrange(desc(max_RSSI))
    
    ## Plot
    tag_plot <- ggplot(tag_dets) +
      
      geom_point(aes(x  = x,
                     y = y),
                 data = grid_pts,
                 color = color_pal[90],
                 size = 1) +
      
      geom_point(aes(x  = x_est,
                     y = y_est),
                 alpha = 0.3) +
      
      theme_minimal() +
      facet_wrap(hour ~ .) +
      theme(axis.text = element_blank()) +
      labs(x = NULL,
           y = NULL)
    
    ## Create if necessary
    if(!dir.exists(paste0(output_folder))){
      dir.create(paste0(output_folder))
    }
    
    cat("Plotting tag: ", tag_f, "\n")
    
    ## Save plots
    suppressMessages(ggplot2::ggsave(paste0(output_folder,
                                            "/",
                                            tag_f, 
                                            "_localized_dets.jpg"),
                                     plot = tag_plot))
    
    
  }
}
