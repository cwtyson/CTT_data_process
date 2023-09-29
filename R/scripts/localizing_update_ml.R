## Update localizaitons using multilateration method
library(foreach)
library(dplyr)

## Source functions
source("./R/functions/collect_raw_data_fn.R")
source("./R/functions/ml_update_localizations_fn.R")
source("./R/functions/ml_prepare_dets_error_fn.R")
source("./R/functions/ml_localize_dets_error_fn.R")

cl <- parallel::makeForkCluster(8, outfile = "")
doParallel::registerDoParallel(cl)

tags <- readxl::read_xlsx("/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/tags/zebby_tag_log_20230831.xlsx") %>% 
  mutate(year = format(Date,"%Y")) %>% 
  filter(year == "2023") %>% 
  select(tag = Tag) %>% 
  filter(tag != "NA") %>% 
  pull(tag)

foreach(tag_f=tags,.packages=c("tidyverse","lubridate","readr","geosphere"),
        .verbose = TRUE) %dopar%
  { ml_update_localizations_fn(db_name = "tyson",
                               db_password = "time00",
                               tag_f = tag_f,
                               node_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/nodes/",
                               tag_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/tags/",
                               grid_points_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/grid_points/",
                               output_folder =   "/Users/tyson/Documents/academia/research/zebby_tracking/data/2023/localizations/",
                               log_dist_RSSI_mdl = "./R/data/RSSI_log_dist_model_zebby.RDS",
                               tz = "Australia/Broken_Hill",
                               crs = 3308,
                               reps = 100) }