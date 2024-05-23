## Get end dates based on raw data
library(tidyverse)

## Get raw data files
rdfs <- list.files("/Volumes/data_bases/bulbuls/processed_detections/raw_detections/data/",
                   pattern = ".RDS",
                   full.names = T)


## Empty data frame
fail_dates <- data.frame()

## For each, read in and find day with large drop in detections
for(rd in rdfs){
  
  ## Detections
  dets <- readRDS(rd)
  
  cat(unique(dets$bird_band) ,"\n")
  
  ## Add day column
  dets_t <- dets %>%
    dplyr::mutate(dt_r = lubridate::floor_date(date_time, unit = "day"),
                  grid_point = grid_point) %>% 
    
    ## Filter by max date of 90 days after start
    mutate(min_date = min(dt_r)) %>% 
    filter(dt_r < min_date + 3600*24*60) %>% 
    mutate(day = as.numeric(format(dt_r, "%j"))) 
  
  dates_df <- data.frame(date = seq(from = min(dets_t$date),
                                    to = max(dets_t$date),
                                    by = "day")) %>% 
    mutate(day = as.numeric(format(date, "%j"))) 
  
  ## Summarize filtered data by day by grid point
  dets_sum <- dets_t %>% 
    dplyr::group_by(day, grid_point) %>% 
    dplyr::summarise(dets = n(),
                     mean_rssi = mean(rssi),
                     .groups = "keep")
  
  # ## Check filtered data
  # (dets_sum_plot <- ggplot2::ggplot(dets_sum) +
  #     ggplot2::geom_point(ggplot2::aes(x=day,
  #                                      y=grid_point,
  #                                      color = mean_rssi,
  #                                      size = dets)) +
  #     ggplot2::scale_colour_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"), name = "rssi") +
  #     ggplot2::theme_minimal())
  
  ## Summarize filtered data by day
  dets_sum_daily <- dets_t %>% 
    dplyr::group_by(day) %>% 
    dplyr::summarise(dets = n(),
                     mean_rssi = mean(rssi),
                     .groups = "keep")
  
  
  ## Moving window of count of detections
  window = 10
  dets_sum_daily <- dets_t %>% 
    dplyr::group_by(day) %>% 
    dplyr::summarise(dets = n(),
                     mean_rssi = mean(rssi),
                     .groups = "keep") %>% 
    data.frame() %>% 
    ungroup() %>% 
    mutate(mean_dets_7_days = runner::runner(x = .,
                                             k = window,
                                             lag = 0,
                                             idx = day,
                                             f = function(x) round(mean(x$dets),0),
                                             na_pad = FALSE)) %>% 
    mutate(cs = cumsum(dets))
  # 
  ## Cumulative dets
  (dets_sum_plot <- ggplot2::ggplot(dets_sum_daily) +
      ggplot2::geom_point(ggplot2::aes(x=day,
                                       y=cs,
                                       fill = cs)) +
      ggplot2::scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"), name = "rssi") +
      ggplot2::theme_minimal())
  
  
  fit.lm <- lm(cs ~ day, dets_sum_daily)
  seg.lm <- segmented::segmented(fit.lm, seg.Z = ~ day, npsi=1)
  
  fail_date <- round(data.frame(list(seg.lm[['psi']]))$Est.)
  
  ## Moving window dets
  (sum_point <- ggplot2::ggplot(dets_sum) +
      ggplot2::geom_point(ggplot2::aes(x=day,
                                       y=grid_point,
                                       size = dets,
                                       color = mean_rssi)) +
      
      
      # geom_point(aes(x = day,
      #                y = cs)) +
      # 
      
      geom_vline(xintercept = fail_date) +
      
      ggplot2::scale_color_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"), name = "rssi") +
      ggplot2::theme_minimal())
  
  ## Cumulative detections
  (dets_sum_plot <- ggplot2::ggplot(dets_sum_daily) +
      labs(title = unique(dets$bird_band)) +
      ggplot2::geom_point(ggplot2::aes(x=day,
                                       y=cs,
                                       color = cs)) +
      
      
      # geom_point(aes(x = day,
      #                y = cs)) +
      #
      
      geom_vline(xintercept = fail_date) +
      
      ggplot2::scale_color_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"), name = "rssi") +
      ggplot2::theme_minimal(base_size = 32))
  
  ## Save failure date plot
  ggsave(plot = sum_point, 
         filename = paste0("/Volumes/data_bases/bulbuls/misc/failure_date_plots/",
                           unique(dets$bird_band),
                           ".jpg"),
         scale = 1.5)
  
  failure_date_d <- dates_df %>% 
    filter(day == fail_date) %>% 
    distinct(date) %>% 
    pull(date)
  
  ## Failure dates
  fdf <- data.frame(tag = unique(dets$bird_band),
                    fail_date = failure_date_d,
                    fail_date_jd = fail_date)
  fail_dates <- bind_rows(fdf,
                          fail_dates)
  
}

  write_csv(fail_dates, 
          "/Volumes/data_bases/bulbuls/misc/failure_date_plots/bulbul_2021_failure_dates.csv")
