## Housekeeping
library(foreach)
library(dplyr)
library(geosphere)

## Source functions
## Set parallel options
cores = parallel::detectCores()
cl <- parallel::makeForkCluster(cores-3, outfile = "")
source("/Users/tyson/Documents/git/CTT_data_process/R/functions/collect.R")

## Set time zone
tz = "Africa/Mbabane"

## Tag folder
tag_log_folder = '/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2025/tag_logs/'

## Read in tag log and reformat
tag_log_mr <- sort(list.files(paste0(tag_log_folder),
                              full.names = TRUE,
                              pattern = "tag_log"),
                   decreasing = TRUE)[1]

## Tag log
tag_log_all  <- suppressWarnings(readxl::read_excel(tag_log_mr, .name_repair = "unique_quiet") %>% 
                                   janitor::clean_names() %>% 
                                   filter(!(tag %in% c("no tag","removed"))) %>% 
                                   transmute(tag = gsub("NA",NA, tag),
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
                                             year = format(tag_start_time, "%Y"),
                                             species)  %>% 
                                   dplyr::filter(!is.na(tag)) %>% 
                                   filter(species == "DCBU") %>% 
                                   dplyr::select(year,
                                                 bird_band,
                                                 tag,
                                                 tag_start_time,
                                                 tag_removal_time) %>% 
                                   dplyr::mutate(tag_removal_time = if_else(is.na(tag_removal_time), Sys.time(), tag_removal_time))) %>% 
  na.omit() %>% 
  arrange(bird_band) 

## Bird bands from most recent log
bird_bands = unique(tag_log_all$bird_band)

## Run in terminal as:
# OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES Rscript ./R/projects/collect/gt.R

# band_f = bird_bands[[1]]

foreach(band_f = bird_bands,
        # .packages=c("dplyr","lubridate","readr","geosphere"),
        .verbose = FALSE) %dopar%
  { 
    
    ## Prepare each tag
    collect_fn(band_f = band_f,
               db_name = '/Users/tyson/Documents/academia/institutions/WUR/research/CTT_data/Mouse Bird/',
               tag_log = tag_log_all,
               station_ids = c("31517E791AAE", "31556FCE4EEA", "39C9F709EC64", "3DDBDADF9153", "98B773B8FE7C"),
               node_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2025/node_logs/",
               output_folder = '/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/Other computers/My Mac/processed_data/bulbuls/processed_detections',
               tz = tz)
  }