#!/usr/bin/env Rscript
## Script to localize prepared files on HPC

suppressMessages(library(docopt))
suppressMessages(library(stringr))

doc <- ("
Usage: 
  hpc_localize_w_docstring.R <file> [-h]

Options:
  -h --help             show this help text
")

opt <- docopt(doc) # will not work in interactive session because no <arguments> are given, can be given via e.g. docopt(doc, args=c('a', 'b'))
file_f <- str_trim(opt$file)

## Initial libraries to load
library(dplyr)
library(geosphere)

## Source functions
source("/lustre/nobackup/SHARED/BHE/mousebird_test/localisation_script/R/functions/hpc_localize_fn.R")
source("/lustre/nobackup/SHARED/BHE/mousebird_test/localisation_script/R/functions/get_grid_points_fn_mousebird.R")


## Localize
hpc_localize_fn(
  
  ## Focal prepared file
  file_f = file_f,
  
  ## Folder where grid point files are saved
  grid_points_folder = "/lustre/nobackup/SHARED/BHE/mousebird_test/field_data/2024/grid_points",
  
  ## Location of log-linear model RSSI~distance output
  log_dist_RSSI_mdl = "/lustre/nobackup/SHARED/BHE/mousebird_test/field_data/2024/RSSI_log_distance_lm_Eswatini.RDS",
  
  ## Time zone
  tz = "Africa/Mbabane",
  
  ## Projected CRS to use
  crs = 22291,
  
  ## Number of repetitions for resampling localization to estimate error
  reps = 100
  
)

