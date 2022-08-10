#' Make summary table and plots of nodes
#'
#' @param project Name of project.
#' @param plot_type Either "summary", "individual", "both", "none". Summary will make one plot with a row for each node. Individual will create separate plots for each node. 
#' @param interval Interval to summarize the data for plotting. Default is one day

summarize_nodes <- function(project = as.character(),
                            interval = "day",
                            plot_type = "both"){
  
  cat("Starting to summarize nodes\n")
  
  ## Read in node data
  nodes <- readRDS(here::here("project", 
                              project,
                              "data/processed/raw/node_health.Rdata"))
  
  ## Read in detections data
  dets <- readRDS(here::here("project",
                             project,
                             "data/processed/raw/dets_filtered.Rdata"))
  
  ## Summarize node file
  nodes_sum <- nodes %>% 
    dplyr::mutate(date_r = lubridate::round_date(date_time, unit = interval)) %>%
    dplyr::group_by(grid_point, date_r) %>% 
    dplyr::summarize(mean_solar_volt = mean(solar_volts, na.rm = T),
                     mean_solar_current = round(mean(solar_current, na.rm = T),0),
                     mean_battery = round(mean(battery, na.rm = T),0),
                     mean_rssi = round(mean(node_rssi, na.rm = T),0),
                     mean_check_in_time = round(mean(as.numeric(difftime(dplyr::lead(date_time), date_time, units = 'min')), na.rm = T),0),
                     .groups = "keep") %>% 
    
    ## Summarize detections and join
    dplyr::left_join(dets %>% 
                       dplyr::mutate(date_r = lubridate::round_date(date_time, unit = interval)) %>%
                       dplyr::group_by(grid_point, date_r) %>% 
                       dplyr::summarise(dets = dplyr::n(),
                                        most_det_tag = names(which.max(table(tag))),
                                        .groups = "keep"),
                     by = c("grid_point", "date_r"))
  
  
  ## Save summary table
  readr::write_csv(nodes_sum,
                   here::here("project",
                              project,
                              "/summaries/node_summary.csv"))
  
  cat("Saved node summary\n")
  
  ## Depending on plot type:
  if(plot_type == "summary|both"){
    
    ## Make plot from summary
    dets_sum_plot <- ggplot2::ggplot(nodes_sum) +
      ggplot2::geom_point(ggplot2::aes(x = date_r,
                                       y = grid_point,
                                       color = log10(mean_check_in_time),
                                       size = ifelse(is.na(dets), 1, dets))) +
      ggplot2::labs(x = NULL, y = "Grid point") +
      ggplot2::scale_size(name = "Detections") +
      ggplot2::scale_colour_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"), name = "Log health\ninterval") +
      ggplot2::theme_minimal()
    
    ## Save
    suppressMessages(ggplot2::ggsave(here::here("project",
                                                project,
                                                "plots/nodes/node_summary_plot.jpg"),
                                     plot = dets_sum_plot))
    
    cat("Finished plotting overall node summary\n")
    
  }
  
  if(plot_type == "individual|both"){
    
    cat("Plotting individual node summaries\n")
    
    ## Make plot
    for(gp_f in unique(nodes_sum$grid_point)){
      
      # gp_f = unique(nodes_sum$grid_point)[1]
      
      cat("Plotting node summary: ", gp_f, "\n")
      
      ## Summarize to plot
      dets_sum_2plot <- nodes_sum %>% 
        
        ## Keep only focal tag
        dplyr::filter(grid_point == gp_f) %>% 
        dplyr::select(-most_det_tag) %>% 
        tidyr::pivot_longer(cols = c("mean_solar_volt", "mean_solar_current", "mean_battery", "mean_rssi", "mean_check_in_time","dets"),
                            names_to = "metric",
                            values_to = "value")
      
      gp_ind_plot <- ggplot2::ggplot(dets_sum_2plot) +
        ggplot2::geom_point(ggplot2::aes(x = date_r,
                                         y = value),
                            color = wesanderson::wes_palette("Zissou1", 100, type = "continuous")[80]) +
        ggplot2::labs(x = NULL,title = gp_f, y = "Value") +
        ggplot2::facet_grid(metric~., scales = "free") +
        ggplot2::theme_minimal()
      
      ## Save
      suppressMessages(ggplot2::ggsave(here::here("project",
                                                  project,
                                                  "plots/nodes/individual/", 
                                                  gp_f, 
                                                  "_health_plot.jpg"),
                                       plot = gp_ind_plot))
      
    }
    
    cat("Finished plotting individual node summaries\n")
    
    
  }
  
}
