## Update localizaitons using multilateration method
library(foreach)
library(dplyr)
library(geosphere)

## Source functions
source("/Users/tracking/git/CTT_data_process/R/functions/get_grid_points_fn_zebby.R")
source("/Users/tracking/git/CTT_data_process/R/functions/ml_prepare_dets_error_fn.R")

## Set parallel options
cores = parallel::detectCores()
cl <- parallel::makeForkCluster(cores-1, outfile = "")
doParallel::registerDoParallel(cl)

## Set time zone
tz = "Australia/Broken_Hill"

## Tag folder
tag_log_folder = "/Users/tracking/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/tags/"

## Read in tag log and reformat
tag_log_mr <- sort(list.files(paste0(tag_log_folder),
                              full.names = TRUE,
                              pattern = "tag_log"),
                   decreasing = TRUE)[1]

##  Process most recent tag log
tag_log_all  <- suppressWarnings(readxl::read_excel(tag_log_mr) %>% 
                                   janitor::clean_names() %>% 
                                   transmute(species,
                                             tag = gsub("NA",NA, tag),
                                             bird_band,
                                             # date,
                                             # time,
                                             tag_start_time = lubridate::parse_date_time(paste(date, time),
                                                                                         tz = tz,
                                                                                         orders = c("%d.%m.%Y %H:%M:%S",
                                                                                                    "%d.%m.%Y %H:%M")),
                                             # end_date,
                                             # end_time,
                                             tag_removal_time = lubridate::parse_date_time(paste(end_date, end_time),
                                                                                           tz = tz,
                                                                                           orders = c("%d.%m.%Y %H:%M:%S",
                                                                                                      "%d.%m.%Y %H:%M")),
                                             year = format(tag_start_time, "%Y"))  %>% 
                                   dplyr::filter(!is.na(tag)) %>% 
                                   dplyr::select(year,
                                                 bird_band,
                                                 tag,
                                                 tag_start_time,
                                                 tag_removal_time) %>% 
                                   dplyr::mutate(tag_removal_time = if_else(is.na(tag_removal_time), Sys.time(), tag_removal_time))) %>% 
  
  ## Keep birds from 2024
  dplyr::filter(year == "2024")

## Get grid points
grid_points <- get_grid_points_fn_zebby(grid_points_folder = "/Users/tracking/Google Drive/My Drive/Zebby_tracking_field_data/grid_points/",
                                        crs = 3308)


## Bird bands from most recent log
bird_bands = unique(tag_log_all$bird_band)

# band_f = bird_bands[1]

## Run in terminal as:
# OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES Rscript ./R/projects/prepare/prepare_zebby.R

foreach(band_f = bird_bands,
        # .packages=c("dplyr","lubridate","readr","geosphere"),
        .verbose = TRUE) %dopar%
  { 
    
    ## Prepare each tag
    ml_prepare_dets_error_fn(band_f = band_f,
                             tag_log_folder = "/Users/tracking/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/tags/",
                             grid_points = grid_points,
                             output_folder = "/Users/tracking/Documents/research/processed_data/zebby/processed_detections/",
                             window = "30 secs",
                             lag = "0 secs",
                             crs = 3308,
                             dist_filter = 175,
                             tz = tz)
  }
