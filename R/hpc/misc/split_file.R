## Split files

file2split <- readr::read_csv("/Users/tyson/Downloads/2023-09-14.csv")

rows <- seq(1,5000, by=20)
rows <- rows[-max(length(rows))]

for(row in 1:length(rows)){
  
  # row = rows[1]
  
  test_split <- file2split[rows[row]:rows[row+1],]

  cat(names(test_split))
  
  readr::write_csv(x = test_split,
                   file = paste0("/Users/tyson/Downloads/test_files/test_",row,"_2023-09-14.csv.gz"))
                 
}
