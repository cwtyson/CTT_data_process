## Update localizaitons using multilateration method
library(foreach)
library(dplyr)

## Source functions
source("./R/functions/ml_update_localizations_fn.R")
source("./R/functions/collect_raw_data_fn.R")
source("./R/functions/ml_prepare_dets_error_fn.R")
source("./R/functions/ml_localize_dets_error_fn.R")

## Set up parallelization options
cl <- parallel::makeForkCluster(8, outfile = "")
doParallel::registerDoParallel(cl)

## Define the tags to be processed- should be a vector
tags <- readxl::read_xlsx("/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/tags/zebby_tag_log_20230917.xlsx") %>% 
  # select(tag = Tag) %>% 
  filter(Species == "CW") %>% 
  pull(Tag)

foreach(tag_f=tags,.packages=c("tidyverse","lubridate","readr","geosphere"),
        .verbose = TRUE) %dopar%
  { ml_update_localizations_fn(
    
    ## Database credentials
    db_name = "tyson",
    
    ## Tag value is defined in foreach function
    tag_f = tag_f,
    
    ## Folder where node logs are saved
    node_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/nodes/",
    
    ## Folder where tag logs are saved
    tag_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/tags/",
    
    ## Folder where grid point files are saved
    grid_points_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/grid_points/",
    
    ## Folder where the data should be saved. A new folder will be created for each tag
    output_folder =   "/Volumes/mac_mini_storage/academia/research/wedgebill_tracking/data/localizations/",
    
    ## Location of log-linear model RSSI~distance output
    log_dist_RSSI_mdl = "./R/data/RSSI_log_dist_model_zebby.RDS",
    
    ## Time zone
    tz = "Australia/Broken_Hill",
    
    ## Projected CRS to use
    crs = 3308,
    
    ## Number of repetitions for resampling localization to estimate error
    reps = 100) }