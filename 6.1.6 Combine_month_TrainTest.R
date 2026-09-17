library(data.table)
library(fst)
# library(lubridate)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ML_Final_Input_001/Train_Inputs")
pmf_path = getwd()

pmf_source_list <-
  list(
    "traffic", 
    "sulfate",
    "nitrate", 
    "dust",  
    "bbsoa",
    "pm"
  )

for (pmf_src in pmf_source_list){
  # pmf_src = pmf_source_list[1]
  # Source name pattern & file list
  pattern <- 
    paste0("ML_TrainTest_001_", pmf_src, "_[0-9]{6}\\.fst$")
  files <- 
    list.files(path = pmf_path, 
               pattern = pattern, full.names = TRUE)
  
  # Combine and write
  combined <- rbindlist(lapply(files, read_fst), fill = TRUE)
  write_fst(combined, 
            file.path(pmf_path, 
                      sprintf("ML_TrainTest_001_%s_2011-20.fst", pmf_src)))
}
