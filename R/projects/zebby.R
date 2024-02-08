## Update localizaitons using multilateration method
library(foreach)
library(dplyr)
library(geosphere)

## Source functions
source("./R/functions/collect_raw_data_fn_zebby.R")
source("./R/functions/ml_update_localizations_fn_zebby.R")
source("./R/functions/ml_prepare_dets_error_fn.R")
source("./R/functions/ml_localize_dets_error_fn_zebby.R")
source("./R/functions/get_grid_points_fn_zebby.R")

cl <- parallel::makeForkCluster(8, outfile = "")
doParallel::registerDoParallel(cl)

## Define the bird bands to be processed- should be a vector
bird_bands <- readxl::read_xlsx("/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/tags/zebby_tag_log_20240205.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(year = format(lubridate::mdy(date), "%Y")) %>% 
  filter(year == "2023") %>% 
  filter(antanne_type == "Steel") %>% 
  filter(species == "ZF" ) %>% 
  pull(bird_band) %>% 
  unique()

## Run in terminal as:
# OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES Rscript ./R/projects/zebby.R

foreach(band_f = bird_bands,
        # .packages=c("dplyr","lubridate","readr","geosphere"),
        .verbose = TRUE) %dopar%
  { ml_update_localizations_fn_zebby(
    
    ## Database credentials
    db_name = "zebbie",
    
    db_password = "time00",
    
    ## Tag value is defined in foreach function
    band_f = band_f,
    
    ## Folder where node logs are saved
    node_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/nodes/",
    
    ## Folder where tag logs are saved
    tag_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/tags/",
    
    ## Folder where grid point files are saved
    grid_points_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/grid_points/",
    
    ## Folder where the data should be saved. A new folder will be created for each tag
    output_folder =   "/Volumes/data_bases/zebby/processed_detections/",
    
    ## Location of log-linear model RSSI~distance output
    log_dist_RSSI_mdl = "/Volumes/data_bases/zebby/RSSI_log_dist_model_2023.RDS",
    
    ## Time zone
    tz = "Australia/Broken_Hill",
    
    ## Projected CRS to use
    crs = 3308,
    
    ## Number of repetitions for resampling localization to estimate error
    reps = 100) }

