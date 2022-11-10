## Update localizaitons using RF method
library(foreach)
## Source functions
source("./R/functions/rf_update_localizations_fn.R")
source("./R/functions/rf_localizing_w_error_fn.R")
source("./R/functions/rf_prepare_dets_error_fn.R")

cl <- parallel::makeForkCluster(7, outfile = "")
doParallel::registerDoParallel(cl)

tags <- list.files("/Users/tyson/Documents/academia/research/zebby_tracking/data/ml")

foreach(tag_f=tags,.packages=c("tidyverse","lubridate","readr"), 
        .verbose = TRUE) %dopar% 
  { rf_update_localization(db_name = "tyson",
                           db_user = "tyson",
                           db_password = "time00",
                           project = "zebby_tracking",
                           tag_f = tag_f,
                           node_folder = "/Users/tyson/Documents/academia/research/zebby_tracking/data/field/nodes/",
                           tag_folder = "/Users/tyson/Documents/academia/research/zebby_tracking/data/field/tag/",
                           output_folder =  "/Users/tyson/Documents/academia/research/zebby_tracking/",
                           tz = "Australia/Broken_Hill") }

