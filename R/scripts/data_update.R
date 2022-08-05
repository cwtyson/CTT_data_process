suppressMessages(library(tidyverse))

## Source files
source(here::here("./R/functions/process_dets.R"))
source(here::here("./R/functions/process_nodes.R"))
source(here::here("./R/functions/summarize_dets.R"))
source(here::here("./R/functions/summarize_nodes.R"))

## Process detection data
process_dets(db_name = "postgres",
             db_user = "tyson",
             db_password = "time00",
             sensor_station_code = NULL,
             tag_log_file = "/Users/tyson/Documents/academia/institutions/WUR/research/eswatini/field/Tags/LifeTags_log21.xlsx",
             node_log_file = "/Users/tyson/Documents/academia/institutions/WUR/research/eswatini/field/Nodes/Node_deployment_log_Feb2022.xlsx",
             tz = "Africa/Mbabane",
             output_folder = "./Eswatini/data/processed")

## Process detection data
process_nodes(db_name = "postgres",
             db_user = "tyson",
             db_password = "time00",
             sensor_station_code = NULL,
             node_log_file = "/Users/tyson/Documents/academia/institutions/WUR/research/eswatini/field/Nodes/Node_deployment_log_Feb2022.xlsx",
             tz = "Africa/Mbabane",
             output_folder = "./Eswatini/data/processed")

## Summarize detections and plot
summarize_dets(dets_data_file = "./Eswatini/data/processed/dets_filtered.Rdata",
               plot_type = "summary",
               interval = "day",
               summary_folder = "./Eswatini/data_summaries",
               plot_folder = "./Eswatini/plots")

## Summarize detections and plot
summarize_nodes(node_data_file = "./Eswatini/data/processed/node_health.Rdata",
                dets_data_file = "./Eswatini/data/processed/dets_filtered.Rdata",
                plot_type = "summary",
                interval = "day",
                summary_folder = "./Eswatini/data_summaries",
                plot_folder = "./Eswatini/plots")
