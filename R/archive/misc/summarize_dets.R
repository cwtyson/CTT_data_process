#' Make summary table of tag detections
#'
#' @param project Name of project
#' @param plot_type Either "summary", "individual", "both", or "none". Summary will make one plot with a row for each node. Individual will create separate plots for each node
#' @param interval Interval to summarize the data for plotting. Default is one day

summarize_dets <- function(project = as.character(),
                           plot_type = as.character(),
                           interval = as.character()){
  
  cat("Starting to summarize detections\n")
  
  ## Read in detections
  dets <- readRDS(here::here("project", 
                             project,
                             "data/processed/raw/dets_filtered.Rdata"))
  
  ## Summarize detections
  dets_sum <- dets %>% 
    dplyr::group_by(tag) %>% 
    dplyr::summarize(dets_n = dplyr::n(),
                     max_RSSI = max(rssi, na.rm = T),
                     max_gp = names(which.max(table(grid_point))),
                     first_det = min(date_time),
                     last_det = max(date_time)) %>% 
    dplyr::arrange(dets_n)
  
  ## Save summary table
  readr::write_csv(dets_sum,
                   here::here("project", 
                              project,
                              "summaries/detection_summary.csv"))
  
  cat("Saved detection summary\n")
  
  if(plot_type %in% c("summary","both")){
    
    cat("Creating overall detection summary plot\n")
    
    ## Summarize to plot
    dets_sum_2plot <- dets %>% 
      dplyr::mutate(date_r = lubridate::round_date(date_time, unit = interval)) %>%
      dplyr::group_by(tag, date_r) %>%
      dplyr::mutate(max_RSSI = max(rssi, na.rm = T),
                    dets = dplyr::n()) %>%
      dplyr::distinct(tag, date_r, .keep_all = T)
    
    ## Make plot
    dets_sum_plot <- ggplot2::ggplot(dets_sum_2plot) +
      ggplot2::geom_point(ggplot2::aes(x = date_r,
                                       y = tag,
                                       color = max_RSSI,
                                       size = dets)) +
      ggplot2::labs(x = NULL) +
      ggplot2::scale_size(name = "Detections") +
      ggplot2::scale_colour_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"), name = "Max RSSI") +
      ggplot2::theme_minimal()
    
    ## Save
    suppressMessages(ggplot2::ggsave(here::here("project", 
                                       project,
                                       "plots/detections/detections_summary_plot.jpg"),
                            plot = dets_sum_plot))
    
    cat("Finished plotting overall detection summary\n")
    
  }
  
  if(plot_type %in% c("individual","both")){
    
    cat("Creating detection summary plots for all tags\n")
    
    ## Make plot
    for(tag_f in unique(dets$tag)){
      
      # tag_f = unique(dets$tag)[1]
      
      cat("Plotting tag: ", tag_f, "\n")
      
      ## Summarize to plot
      dets_sum_2plot <- dets %>% 
        
        ## Keep only focal tag
        dplyr::filter(tag == tag_f) %>% 
        dplyr::mutate(date_r = lubridate::round_date(date_time, unit = interval)) %>%
        dplyr::group_by(tag, grid_point, date_r) %>%
        dplyr::mutate(max_RSSI = max(rssi, na.rm = T),
                      dets = dplyr::n()) %>%
        dplyr::distinct(tag,grid_point, date_r, .keep_all = T)
      
      
      dets_ind_plot <- ggplot2::ggplot(dets_sum_2plot) +
        ggplot2::geom_point(ggplot2::aes(x = date_r,
                                         y = grid_point,
                                         color = max_RSSI,
                                         size = dets)) +
        ggplot2::labs(x = NULL,title = tag_f, y = "Grid point") +
        ggplot2::scale_size(name = "Detections") +
        ggplot2::scale_colour_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"), name = "Max RSSI") +
        ggplot2::theme_minimal()
      
      ## Save
      suppressMessages(ggplot2::ggsave(here::here("project", 
                                         project,
                                         "plots/detections/individual", 
                                         paste0(tag_f, "_detections_plot.jpg")),
                              plot = dets_ind_plot))
    }
    
    cat("Finished detection summary plots for all tags\n")
    
    
  }
  
  if(!(plot_type  %in% c("summary", "individual"," both"))){
    
    cat("Not creating summary plots\n")
    
  }
  
}
