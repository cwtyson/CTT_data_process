## Housekeeping
library(magrittr)

## If necessary, set here to the CTT_data_process folder location and then restart R
here::set_here(path = "CTT_data_process folder with CTT_data_process.Rproj")

## Set up profile with personal information
## Source functions
funs <- list.files(here::here("./R/functions"), full.names = TRUE)
for(fun in funs){
  source(fun)
}

## Set up new project
initalize_project(project = "Eswatini",
                  google_drive_folder = "Eswatini_field_data")

## Fetch and update field data
fetch_field_data(project = "Eswatini",
                 google_drive_folder = "Eswatini_field_data",
                 overwrite = TRUE,
                 update = FALSE)

## Process detection data
process_dets(db_name = db_name,
             db_user = db_user,
             db_password = db_password,
             project = "Eswatini",
             sensor_station_code = NULL,
             tz = "Africa/Mbabane")

## Process detection data
process_nodes(db_name = db_name,
              db_user = db_user,
              db_password = db_password,
              project = "Eswatini",
              sensor_station_code = NULL,
              tz = "Africa/Mbabane")

## Summarize detections and plot
summarize_dets(project = "Eswatini",
               plot_type = "both",
               interval = "day")

## Summarize detections and plot
summarize_nodes(project = "Eswatini",
                plot_type = "both",
                interval = "day")

## Prepare detection data to localize
prepare_dets(processed_dets_file = "./Eswatini/data/processed/dets_filtered.Rdata",
             grid_point_file = "./Eswatini/data/field_data/processed_field_data/grid_point_locations.RData",
             output_folder = "./Eswatini/data/processed/prepared/",
             window = "30 secs",
             lag = "-15 secs",
             dist_filter = 200)

## Localize detections
localize(input_folder = "./Eswatini/data/processed/prepared/",
         output_folder = "./Eswatini/data/processed/localized/",
         grid_point_file = "./Eswatini/data/field_data/processed_field_data/grid_point_locations.RData",
         RSSI_model_file = "R/data/RSSI_log_distance_lm.RDS",
         reps = 100)

## Home range analysis
