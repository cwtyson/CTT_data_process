## Create test file

library(tidyverse)





full = readr::read_csv('/Users/tyson/Library/CloudStorage/GoogleDrive-cwtyson@gmail.com/Other computers/My Mac/processed_data/zebby/processed_detections/ml_prepared/2025/02749635/2025-09-21.csv.gz')

test = full %>% 
  filter(int_id < 50)

write.csv(test, "/Users/tyson/Downloads/test.csv.gz")

put /Users/tyson/Downloads/test.csv.gz /lustre/nobackup/SHARED/BHE/zebby/ml_prepared/2025/