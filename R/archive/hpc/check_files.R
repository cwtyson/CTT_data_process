## Check prepared file


ptf = list.files("/Users/tracking/Documents/research/processed_data/zebby/processed_detections/ml_prepared/2024/02745374/",
                 pattern = ".csv",
                 full.names = T)

tcsv = readr::read_csv(ptf[1], col_types = "dccTddddd")
