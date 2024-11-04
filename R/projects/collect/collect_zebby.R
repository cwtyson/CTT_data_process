## Update localizaitons using multilateration method
library(foreach)
library(dplyr)
library(geosphere)

## Source functions
source("./R/functions/collect_raw_data_fn.R")

cl <- parallel::makeForkCluster(8, outfile = "")
doParallel::registerDoParallel(cl)

## Set time zone
tz = "Australia/Broken_Hill"

## Read in tag log and reformat
tag_log_mr <- sort(list.files(paste0("/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/tags"),
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

## Bird bands from most recent log
bird_bands = unique(tag_log_all$bird_band)

band_f = bird_bands[1]

## Run in terminal as:
# OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES Rscript ./R/projects/zebby.R

foreach(band_f = bird_bands,
        # .packages=c("dplyr","lubridate","readr","geosphere"),
        .verbose = TRUE) %dopar%
  { 
    
    ## Prepare each tag
    collect_raw_data_fn(band_f = band_f,
                        db_name = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/Other computers/My Mac (1)/databases/australia.duckdb",
                        tag_log = tag_log_all, ## Processed tag log
                        station_ids = c("D82AA0A12259", "4BA80216EAEB"),
                        node_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/nodes/",
                        output_folder = "/Users/tyson/Downloads/",
                        tz = tz)
    
  }