#' Make summary table of node
#'
#' @param node_file Detection file (either absolute or relative path)
#' @param summary_folder Location to save summary
#' @param plot_type Either "summary", "individual" or "none". Summary will make one plot with a row for each node. Individual will create separate plots for each node
#' @param interval Interval to summarize the data for plotting. Default is one day
#' @param plot_folder Location to save plot

summarize_nodes <- function(node_file = as.character(),
                           plot = TRUE,
                           interval = "day",
                           plot_folder = as.character()){
  
  ## Read in detections
  dets <- readRDS(here::here(det_file))
  
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
                   here::here(summary_folder))
  
  ## Depending on plot type:
  if(plot == "summary"){
    
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
    suppressMessages(ggsave(here::here(plot_folder, "detections_summary_plot.jpg"),
           plot = dets_sum_plot))
    
  }
}
