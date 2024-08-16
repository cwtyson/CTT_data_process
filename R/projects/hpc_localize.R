## Script to localize prepared files on HPC

## Initial libraries to load
library(foreach)
library(dplyr)
library(geosphere)

## Source functions
source("./R/functions/hpc_localize_fn.R")
source("./R/functions/get_grid_points_fn_mousebird.R")

## Prepared files
files <- list.files( "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/Other computers/My Mac/mousebird/processed_detections/ml_prepared",
                     recursive = T,
                     full.names = T)

file_f = files[1]

## Localize
hpc_localize_fn(
  
  ## Focal prepared file
  file_f = file_f,
  
  ## Folder where grid point files are saved
  grid_points_folder = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2024/grid_points/",
  
  ## Folder where the data should be saved. A new folder will be created for each tag
  output_folder =   "/Users/tyson/Downloads/",
  
  ## Location of log-linear model RSSI~distance output
  log_dist_RSSI_mdl = "/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/My Drive/Eswatini_field_data/2024/RSSI_log_distance_lm_Eswatini.RDS",
  
  ## Time zone
  tz = "Africa/Mbabane",
  
  ## Projected CRS to use
  crs = 22291,
  
  ## Number of repetitions for resampling localization to estimate error
  reps = 100
  
)

