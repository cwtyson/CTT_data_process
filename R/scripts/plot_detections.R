library(tidyverse)

file_locs <- list.files("/Volumes/data_bases/bulbuls/processed_detections/ml_localized/",pattern = "RDS", recursive = T, full.names = T)

rds <- lapply(file_locs, readRDS)



dets <- do.call(rbind, 
                lapply(rds, function(x) do.call(rbind, x[[2]])))



ggplot(dets) +
  geom_point(aes(x=x_est,
                 y=y_est,
                 color=tag)) +
  facet_wrap(tag~.)
  theme(legend.position = "none")

