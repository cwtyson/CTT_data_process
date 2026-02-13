## Update localizaitons using multilateration method
library(foreach)
library(dplyr)
library(geosphere)

## Source functions
source("/Users/tyson/Documents/git/CTT_data_process/R/functions/get_grid_points_fn_gt.R")
source("/Users/tyson/Documents/git/CTT_data_process/R/functions/prepare.R")

## Set parallel options
cores = parallel::detectCores()
cl <- parallel::makeForkCluster(cores-1, outfile = "")
doParallel::registerDoParallel(cl)

## Set time zone
tz = "Europe/Amsterdam"

## Set CRS
crs = 32631

## Tag folder
tag_log_folder = '/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/GT_tracking_field_data/tags/'

## Read in tag log and reformat
tag_log_mr <- sort(list.files(paste0(tag_log_folder),
                              full.names = TRUE,
                              pattern = "tag_log"),
                   decreasing = TRUE)[1]

##  Process most recent tag log
tag_log_all  <- suppressWarnings(readxl::read_excel(tag_log_mr) %>% 
                                   janitor::clean_names() %>% 
                                   transmute(tag = gsub("NA",NA, tag),
                                             bird_band = ring_nr,
                                             # date,
                                             # time,
                                             tag_start_time = lubridate::parse_date_time(paste(start_date, start_time),
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
  na.omit()

## Bird bands from most recent log
bird_bands = unique(tag_log_all$bird_band)

# band_f = bird_bands[1]

## Run in terminal as:
# OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES Rscript ./R/projects/prepare/zebby.R

foreach(band_f = bird_bands,
        # .packages=c("dplyr","lubridate","readr","geosphere"),
        .verbose = TRUE) %dopar%
  { 
    
    ## Prepare each tag
    prepare(band_f = band_f,
            output_folder = '/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/Other computers/My Mac/processed_data/gt/processed_detections',
            grid_points_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/GT_tracking_field_data/grid_points/",
            tz = tz,
            crs = crs,
            sumFun = "mean",
            window = "15 secs",
            lag = "0 secs",
            dist_filter = 175)
  }
