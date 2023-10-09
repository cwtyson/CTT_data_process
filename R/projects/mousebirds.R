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

## Define the bird bands to be processed- should be a vector
bird_bands <- readxl::read_xlsx("/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2023/tag_logs/tag_log_20230909.xlsx") %>% 
  filter(bird_band %in% c("4A94985")) %>% 
  pull(bird_band) %>% 
  unique()


foreach(band_f = bird_bands,.packages=c("tidyverse","lubridate","readr","geosphere"),
        .verbose = TRUE) %dopar%
  { ml_update_localizations_fn(
    
    ## Database credentials
    db_name = "postgres",
    db_password = "time00",
    
    ## Tag value is defined in foreach function
    band_f = band_f,
    
    ## Folder where node logs are saved
    node_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2023/node_logs/",
    
    ## Folder where tag logs are saved
    tag_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2023/tag_logs/",
    
    ## Folder where grid point files are saved
    grid_points_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2023/grid_points/",
    
    ## Folder where the data should be saved. A new folder will be created for each tag
    output_folder =   "/Users/tyson/Documents/academia/research/Eswatini/data/processed_detections/",
    
    ## Location of log-linear model RSSI~distance output
    log_dist_RSSI_mdl = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/RSSI_log_distance_lm_Eswatini.RDS",
    
    ## Time zone
    tz = "Africa/Mbabane",
    
    ## Projected CRS to use
    crs = 22291,
    
    ## Number of repetitions for resampling localization to estimate error
    reps = 100) }