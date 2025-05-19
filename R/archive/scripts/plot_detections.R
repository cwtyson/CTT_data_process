library(tidyverse)

locs <- readRDS("/Volumes/data_bases/zebby/processed_detections/ml_localized/43951/2023-03-06.RDS")


dets <- do.call(rbind, locs[[2]])

ggplot(dets) +
  geom_point(aes(x=x_est,
                 y=y_est,
                 color=t_ind)) +
  theme(legend.position = "none")



cov_ests <- do.call(rbind, locs[[3]])
