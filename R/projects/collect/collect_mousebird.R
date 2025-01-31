## Update localizaitons using multilateration method
library(foreach)
library(dplyr)
library(geosphere)

## Source functions
source("/Users/tracking/git/CTT_data_process/R/functions/collect_raw_data_fn.R")

## Set parallel options
cores = parallel::detectCores()
cl <- parallel::makeForkCluster(cores-8, outfile = "")
doParallel::registerDoParallel(cl)

## Set time zone
tz = "Africa/Mbabane"

## Tag folder
tag_log_folder = "/Users/tracking/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2024/tag_logs"

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
                                                 species,
                                                 bird_band,
                                                 tag,
                                                 tag_start_time,
                                                 tag_removal_time) %>% 
                                   dplyr::mutate(tag_removal_time = if_else(is.na(tag_removal_time), Sys.time(), tag_removal_time)) %>% 
                                   
                                   ## Keep only mousebirds
                                   dplyr::filter(species == "SPMO") %>% 
                                   
                                   ## Keep 2023 & 2024 birds
                                   dplyr::filter(year %in% c(2023,2024)))


## Bird bands from most recent log
bird_bands = unique(tag_log_all$bird_band)

# band_f = bird_bands[50]

## Run in terminal as:
# OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES Rscript ./R/projects/collect/collect_mousebird.R

foreach(band_f = bird_bands,
        # .packages=c("dplyr","lubridate","readr","geosphere"),
        .verbose = TRUE) %dopar%
  { 
    
    ## Collect data for each tag
    collect_raw_data_fn(band_f = band_f,
                        db_name = "/Users/tracking/Documents/research/ctt_data/databases/eswatini.duckdb",
                        tag_log = tag_log_all, ## Processed tag log
                        station_ids = c("3DDBDADF9153", "39C9F709EC64","98B773B8FE7C","31517E791AAE","31556FCE4EEA"),
                        node_folder = "/Users/tracking/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2024/node_logs/",
                        output_folder = "/Users/tracking/Documents/research/processed_data/mousebird/processed_detections/",
                        tz = tz)
  }
