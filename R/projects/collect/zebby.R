## Update localizaitons using multilateration method
library(foreach)
library(dplyr)
library(geosphere)

## Source functions
## Set parallel options
cores = parallel::detectCores()
cl <- parallel::makeForkCluster(cores-3, outfile = "")
source("/Users/tyson/Documents/git/CTT_data_process/R/functions/collect.R")

## Set time zone
tz = "Australia/Broken_Hill"

## Tag folder
tag_log_folder = '/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/tags'

## Read in tag log and reformat
tag_log_mr <- sort(list.files(paste0(tag_log_folder),
                              full.names = TRUE,
                              pattern = "tag_log"),
                   decreasing = TRUE)[1]

## Tag log
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
  na.omit() %>% 
  filter(year == 2025)

## Bird bands from most recent log
bird_bands = unique(tag_log_all$bird_band)

## Run in terminal as:
# OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES Rscript ./R/projects/collect/collect_zebby.R

# band_f = bird_bands[10]

foreach(band_f = bird_bands,
        # .packages=c("dplyr","lubridate","readr","geosphere"),
        .verbose = FALSE) %dopar%
  { 
    
    ## Prepare each tag
    collect_fn(band_f = band_f,
               db_name = '/Users/tyson/Documents/academia/institutions/WUR/research/CTT_data/Zebra Finches in Australia/zebby_database.duckdb',
               tag_log = tag_log_all,
               station_ids = c("D82AA0A12259", "4BA80216EAEB"),
               node_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Zebby_tracking_field_data/nodes",
               output_folder = '/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/Other computers/My Mac/processed_data/zebby/processed_detections',
               tz = tz)
  }