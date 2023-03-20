## Update localizaitons using multilateration method
library(foreach)
## Source functions
source("./R/functions/ml_update_localizations_fn.R")
source("./R/functions/ml_prepare_dets_error_fn.R")
source("./R/functions/ml_localize_dets_error_fn.R")

cl <- parallel::makeForkCluster(8, outfile = "")
doParallel::registerDoParallel(cl)

tags <- list.files("/Users/tyson/Google Drive/Other computers/My Mac mini/processed_detections/ml/ml_prepared//")

# foreach(tag_f=tags,.packages=c("tidyverse","lubridate","readr","geosphere"), 
#         .verbose = TRUE) %dopar% 
#   { ml_update_localizations_fn(db_name = "tyson",
#                                db_user = "tyson",
#                                db_password = "time00",
#                                project = "zebby_tracking",
#                                tag_f = tag_f,
#                                node_folder = "/Users/tyson/Documents/academia/research/zebby_tracking/data/field/nodes/",
#                                tag_folder = "/Users/tyson/Documents/academia/research/zebby_tracking/data/field/tag/",
#                                output_folder =   "/Users/tyson/Documents/academia/research/zebby_tracking/data/processed_detections/ml/",
#                                log_dist_RSSI_mdl = "./R/data/RSSI_log_dist_model_zebby.RDS",
#                                tz = "Australia/Broken_Hill") }

foreach(tag_f=tags,.packages=c("tidyverse","lubridate","readr","geosphere"), 
        .verbose = TRUE) %dopar% 
  { ml_localizing_fn(tag_f = tag_f,
                     output_folder = "/Users/tyson/Google Drive/Other computers/My Mac mini/zebby_tracking/data/processed_detections/ml/",
                     node_folder = "/Users/tyson/Google Drive/Other computers/My Mac mini/zebby_tracking/data/field/nodes/",
                     # grid_points = grid_points,
                     log_dist_RSSI_mdl = "./R/data/RSSI_log_dist_model_zebby.RDS",
                     tz = "Australia/Broken_Hill")
  }
