# allYear_ML_HD_4day_001.R

library(fst)
library(data.table)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ML_Final_Input_001/Annual_combine")

# Source and 4 days
sources   <- c("bbsoa", "sulfate", "nitrate", "dust", "traffic", "pm")
selected_4days  <- c("20200117", "20200417", "20200717", "20201017")

data_path <- getwd()

# Grid coordinates
us_point_coord =
  read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Long_lat_CONUS_0.01_degree.fst")
# n_points <- nrow(us_point_coord)
setDT(us_point_coord)

for (source.name in sources) {
  cat("Processing source:", source.name, "\n")
  
  # source.name = sources[5]
  
  # Read only the four selected days
  pred_4d <- 
    read_fst(
      file.path(data_path, sprintf("%s_US_RF_Daily_Pred_2020.fst", source.name)),
      columns = paste0("Pred_", selected_4days), as.data.table = TRUE)
  unc_4d  <- 
    read_fst(
      file.path(data_path, sprintf("%s_US_RF_Daily_Unc_2020.fst", source.name)),
      columns = paste0("Unc_", selected_4days), as.data.table = TRUE)

  # Check if the row number match CONUS grid number  
  stopifnot(nrow(pred_4d) == nrow(unc_4d), nrow(pred_4d) == nrow(us_point_coord))
  
  # Combine coordinates, Pred and Unc
  out <- cbind(us_point_coord, pred_4d, unc_4d)
  
  write_fst(out,
            file.path(data_path, sprintf("%s_US_RF_Pred_Unc_001_4days_in_2020.fst", source.name)),
            compress = 75)
  
  rm(pred_4d, unc_4d, out); gc()
}