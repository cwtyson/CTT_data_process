## Housekeeping
library(magrittr)
library(geosphere)
library(ggplot2)
library(sf)
library(ggmap)
color_pal = wesanderson::wes_palette("Zissou1", 100, type = "continuous")

## Set up profile with personal information
profile_info <- source("./R/profile_info.R")

## Source functions for updating data
source("./R/functions/process_dets.R")
source("./R/functions/process_nodes.R")
source("./R/functions/summarize_dets.R")
source("./R/functions/summarize_nodes.R")
source("./R/functions/map_nodes.R")

# Process detection data
process_dets(db_name = db_name,
             db_user = db_user,
             db_password = db_password,
             project = "Mousebird",
             sensor_station_code = c("D82AA0A12259"),
             tz = "AAustralia/Broken_Hill")

# ## Process detection data
process_nodes(db_name = db_name,
              db_user = db_user,
              db_password = db_password,
              project = "Mousebird",
              sensor_station_code = c("D82AA0A12259"),
              tz = "Australia/Broken_Hill")

# ## Summarize detections and plot
summarize_dets(project = "zebby",
               plot_type = "both",
               interval = "day")

# Summarize detections and plot
summarize_nodes(project = "zebby",
                plot_type = "both",
                interval = "6 hours")

map_nodes(project = "zebby",
          tz = "Australia/Broken_Hill")
