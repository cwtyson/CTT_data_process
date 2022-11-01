## Housekeeping
library(magrittr)
library(geosphere)
library(ggplot2)
library(sf)
library(ggmap)
color_pal = wesanderson::wes_palette("Zissou1", 100, type = "continuous")

## Set up profile with personal information
## Source functions
funs <- list.files(here::here("./R/functions"), full.names = TRUE)
for(fun in funs){
  source(fun)
}
profile_info <- source("./R/profile_info.R")

# data_update <- function(update_type = c("update_field_data",
#                                         "process_dets",
#                                         "process_nodes",
#                                         "summarize_dets",
#                                         "summarize_nodes",
#                                         "prepare_dets",
#                                         "localize",
#                                         "all"))


# ## Set up new project
# initalize_project(project = "Eswatini",
#                   google_drive_folder = "Eswatini_field_data")

# ## Fetch and update field data
# fetch_field_data(project = "Eswatini",
#                  google_drive_folder = "Eswatini_field_data",
#                  overwrite = TRUE,
#                  update = FALSE)

# # Process detection data
# process_dets(db_name = db_name,
#              db_user = db_user,
#              db_password = db_password,
#              project = "Eswatini",
#              sensor_station_code = c("31517E791AAE","31556FCE4EEA"),
#              tz = "Africa/Mbabane")
# 
# # ## Process detection data
# process_nodes(db_name = db_name,
#               db_user = db_user,
#               db_password = db_password,
#               project = "Eswatini",
#               sensor_station_code = c("31517E791AAE","31556FCE4EEA"),
#               tz = "Africa/Mbabane")
# 
# # ## Summarize detections and plot
# summarize_dets(project = "Eswatini",
#                plot_type = "both",
#                interval = "day")
# 
# # Summarize detections and plot
# summarize_nodes(project = "Eswatini",
#                 plot_type = "both",
#                 interval = "6 hours")

map_nodes(project = "Eswatini",
          tz = "Africa/Mbabane")

# ## Prepare detection data to localize
# prepare_dets(processed_dets_file = "./project/Eswatini/data/processed/raw/dets_filtered.Rdata",
#              tags = c("07343366","78524B34", "19071934"),
#              project = "Eswatini",
#              grid_point_file = "./project/Eswatini/data/processed/field_data/grid_point_locations.RData",
#              output_folder = "./project/Eswatini/data/processed/detections/prepared/",
#              window = "30 secs",
#              lag = "-15 secs",
#              dist_filter = 200)
# 
# ## Localize detections
# localize(project = "Eswatini",
#          RSSI_model_file = "R/data/RSSI_log_distance_lm.RDS",
#          reps = 5)
# 
# 
# ## Localize detections
# plot_localized_dets(project = "Eswatini")
# 

## Home range analysis
