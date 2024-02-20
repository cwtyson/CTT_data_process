## Update localizaitons using multilateration method
library(foreach)
library(dplyr)
library(geosphere)

## Source functions
source("./R/functions/ml_update_localizations_fn_mousebird.R")
source("./R/functions/collect_raw_data_fn_mousebird.R")
source("./R/functions/ml_prepare_dets_error_fn.R")
source("./R/functions/ml_localize_dets_error_fn_mousebird.R")
source("./R/functions/get_grid_points_fn_mousebird.R")

cl <- parallel::makeForkCluster(11, outfile = "")
doParallel::registerDoParallel(cl)

## Define the bird bands to be processed- should be a vector
bird_bands <- readxl::read_xlsx("/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2023/tag_logs/tag_log_20230909.xlsx") %>% 
  mutate(year = format(lubridate::dmy(date), "%Y")) %>% 
  # filter(year == "2023") %>% 
  filter(species == "SPMO" ) %>% 
  pull(bird_band) %>% 
  unique()

## Get maximum date for preparing data to account for sensor station uploads

## Active sensor stations
ss_ids <- c("39C9F709EC64","31517E791AAE", "98B773B8FE7C")

max_dates <- data.frame()
suppressWarnings(
  for(ss_id in ss_ids){
    
    files <- list.files(paste0("/Volumes/ctt_data/files/Mouse Bird/",ss_id,"/raw"))
    dates <- lapply(files, function(x) unlist(strsplit(x,split = "[.]")))
    dates_df <- do.call(rbind, dates) %>% 
      data.frame() %>% 
      transmute(date= lubridate::ymd_hms(X2, tz="Africa/Mbabane"),
                ss = ss_id,
                max_date = max(date)) %>% 
      filter(date == max_date)
    max_dates <- bind_rows(max_dates, dates_df)
    
  }
)
## Keep oldest download date as filter
ss_date_filter = min(max_dates$max_date)

## Run in termainal
## OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES Rscript ./R/projects/mousebirds.R


foreach(band_f = bird_bands,
        .packages=c("tidyverse","lubridate","readr","geosphere"),
        .verbose = FALSE) %dopar%
  { ml_update_localizations_fn_mousebird(
    
    ## Database credentials
    db_name = "mousebird",
    
    ## Tag value is defined in foreach function
    band_f = band_f,
    
    ## Folder where node logs are saved
    node_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2023/node_logs/",
    
    ## Folder where tag logs are saved
    tag_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2023/tag_logs/",
    
    ## Folder where grid point files are saved
    grid_points_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2023/grid_points/",
    
    ## Folder where the data should be saved. A new folder will be created for each tag
    output_folder =   "/Volumes/data_bases/mousebird/processed_detections/",
    
    ## Location of log-linear model RSSI~distance output
    log_dist_RSSI_mdl = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2023/RSSI_log_distance_lm_Eswatini.RDS",
    
    ## Time zone
    tz = "Africa/Mbabane",
    
    ## Sensor station date filter
    ss_filter = ss_date_filter,
    
    ## Projected CRS to use
    crs = 22291,
    
    ## Number of repetitions for resampling localization to estimate error
    reps = 100) }
