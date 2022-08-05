suppressMessages(library(tidyverse))

## Source files
source(here::here("./R/functions/process_dets.R"))
source(here::here("./R/functions/process_nodes.R"))
source(here::here("./R/functions/summarize_dets.R"))

process_dets(db_name = "postgres",
             db_user = "tyson",
             db_password = "time00",
             tag_log_file = "/Users/tyson/Documents/academia/institutions/WUR/research/eswatini/field/Tags/LifeTags_log21.xlsx",
             node_log_file = "/Users/tyson/Documents/academia/institutions/WUR/research/eswatini/field/Nodes/Node_deployment_log_Feb2022.xlsx",
             sensor_station_code = "31556FCE4EEA",
             tz = "Africa/Mbabane",
             output_folder = )

## Summarize detections and plot
summarize_dets(det_file = "./Eswatini/data/processed/dets_filtered.Rdata",
               plot_type = "individual",
               interval = "day",
               summary_folder = "./Eswatini/data_summaries/",
               plot_folder = "./Eswatini/plots/")



