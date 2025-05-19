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
source("/lustre/nobackup/SHARED/BHE/zebby/hpc_localize_zebby_fn.R")

## Localize
hpc_localize_zebby_fn(
  
  ## Focal prepared file
  file_f = file_f,

  ## Location of log-linear model RSSI~distance output
  rssi_dist_model_file = "/lustre/nobackup/SHARED/BHE/mousebird_test/field_data/2024/RSSI_log_distance_lm_Eswatini.RDS",
  
  model_scale = "log",
  
  ## Number of repetitions for resampling localization to estimate error
  reps = 100
  
)

