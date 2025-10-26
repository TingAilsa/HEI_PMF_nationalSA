library(dplyr)
library(fst)
library(data.table)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/Annual_combine/")
getwd()

# Define single source
source.tests <- c("Sulfate", "Biomass", "Dust", "Traffic")  # Change as needed

# Define columns to keep
use_var_list <- c("Date", "Longitude", "Latitude", "CMAQ_conc", "PMF_conc", 
                  "Predictions", "Pred.sd", "PM25_TOT_EGU", "PM25_TOT_OTA",
                  "PM25_TOT_ONR", "PM25_TOT_NRD", "PM25_TOT_ACM",
                  "PM25_TOT_ASEA", "PM25_TOT_ARS", "PM25_TOT_DUST", 
                  "PM25_TOT_BIOG", "PM25_TOT_AFI",
                  "O3", "NH3", "SO2", "NO2")

for(source.test in source.tests){ # source.test = "Sulfate"
  # Input and output file paths
  full_var_prediction_file <- 
    paste0("RF_Pred_HD_US_01_",
                       source.test, "_2011-2020.fst")
  
  pmf_cmaq_file <- 
    paste0("RF_Pred_HD_US_01_allDaily_PMF_CMAQ_",
                        source.test, "_2011-2020.fst")
  
  # Read, select, and save
  cat("Reading:", basename(full_var_prediction_file), "\n")
  full_var_prediction <- read_fst(full_var_prediction_file)
  
  # Select columns (only those that exist)
  available_cols <- intersect(use_var_list, names(full_var_prediction))
  pmf_cmaq_pred_subset <- full_var_prediction[, available_cols, drop = FALSE]
  
  cat("Selected", ncol(pmf_cmaq_pred_subset), "columns from", ncol(data), "total\n")
  
  write_fst(pmf_cmaq_pred_subset, pmf_cmaq_file)
  
  rm(pmf_cmaq_pred_subset)
  gc()
}
