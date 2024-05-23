## Update localizaitons using multilateration method
library(foreach)
library(dplyr)
library(geosphere)

## Source functions
source("./R/functions/localize_by_day_fn.R")
source("./R/functions/ml_localize_dets_error_fn.R")
source("./R/functions/get_grid_points_fn_mousebird.R")

cores<-parallel::detectCores()-1
cl <- parallel::makeForkCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

## Run in termainal
## OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES Rscript ./R/localize/bulbuls_localize.R

## Folder with processed detections
output_folder =  "/Volumes/data_bases/mousebird/processed_detections/"

## Get files that have been prepared and not yet localized

## Prepared file names
files_prep <- list.files(path = paste0(output_folder,
                                       "/ml_prepared"),
                         recursive = T,
                         pattern = ".csv.gz",
                         full.names = T)


## Prepared file names shortened
files_prep_abs <- list.files(path =  paste0(output_folder,
                                            "/ml_prepared"),
                             recursive = T,
                             ".csv.gz",
                             full.names = F)

## Get files that have been localized
files_localized <- list.files(path = paste0(output_folder,
                                            "/ml_localized"),
                              recursive = T,
                              ".csv.gz",
                              full.names = F)

## Files to localize
files_2_localize <-  files_prep[!(files_prep_abs %in% files_localized)]

foreach(file_f = files_2_localize,
        .packages=c("tidyverse","lubridate","readr","geosphere"),
        .verbose = FALSE) %dopar%
  { localize_by_day_fn(
    
    ## File
    file_f = file_f,
    
    ## Folder where grid point files are saved
    grid_points_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2023/grid_points/",
    
    ## Folder where the data should be saved. A new folder will be created for each tag
    output_folder =   output_folder,
    
    ## Location of log-linear RSSI~distance model
    log_dist_RSSI_mdl = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2023/RSSI_log_distance_lm_Eswatini.RDS",
    
    ## File with dates to exclude
    failure_date_file = NA,
    
    ## Time zone
    tz = "Africa/Mbabane",
    
    ## Projected CRS to use
    crs = 22291,
    
    ## Number of repetitions for resampling localization to estimate error
    reps = 100) }
