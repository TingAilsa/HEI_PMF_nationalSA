# AY_average_daily_outputs.R

# Temp dir
tmpdir <- file.path(Sys.getenv("SCRATCH"), "rtmp", Sys.getenv("SLURM_ARRAY_TASK_ID"))
dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TMPDIR = tmpdir, TMP = tmpdir, TEMP = tmpdir)

library(dplyr)
library(fst)
library(stringr)
library(lubridate)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/Annual_combine")
getwd()

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
source_name <- args[1]

# File name pattern that matches only this source for daily predictions
source_conc_name_pattern <- paste0(
  "^.+_Pred_US_01_Daily_mean_Predictions_", source_name, "_2011-2020\\.fst$"
)

source_files <- list.files(
  pattern = source_conc_name_pattern,
  full.names = TRUE,
  ignore.case = FALSE
)
print("source_files: \n"); source_files

## Get averages
for (source_conc_file in source_files){ # source_conc_file = source_files[2]
  
  # Get basenames, and model, and source names
  s_base_name = basename(source_conc_file)
  model_name <- sub("^([^_]+)_.*", "\\1", s_base_name)
  # source_name <- sub(".*_mean_Predictions_(.*)_2011-2020\\.fst$", "\\1", s_base_name)
  
  # Get the dataset
  if(model_name == "CSN") { 
    dataset = "CSN" 
    used_model = "RF"
  } else { 
    dataset = "Both"
    used_model = model_name
  }
  cat("Model,", used_model, "; Data,", dataset, "; Source,", source_name, "\n")
  
  # Read as data.table
  daily_source_conc = read_fst(source_conc_file, as.data.table = TRUE)
  
  # Add metadata and time indexes, and estimate uncertainty
  daily_source_conc[, `:=`(
    Model = used_model,
    Source = source_name,
    Dataset = dataset,
    Year = year(Date),
    Month = month(Date),
    Uncertainty = Pred.sd / Predictions * 100
  )]  
  # head(daily_source_conc)
  
  # Get averages
  # Native data.table Aggregations (faster & uses less RAM than dplyr)
  per_source_conc <-
    daily_source_conc[, .(Pred.sd = mean(Pred.sd, na.rm = TRUE), 
                          Predictions = mean(Predictions, na.rm = TRUE),
                          Predictions_max = max(Predictions, na.rm = TRUE),
                          Predictions_min = min(Predictions, na.rm = TRUE),
                          Predictions_median = median(Predictions, na.rm = TRUE),
                          Predictions_995 = quantile(Predictions, 0.995, na.rm = TRUE),
                          Predictions_005 = quantile(Predictions, 0.005, na.rm = TRUE),
                          Predictions_975 = quantile(Predictions, 0.975, na.rm = TRUE),
                          Predictions_025 = quantile(Predictions, 0.025, na.rm = TRUE),
                          Uncertainty = mean(Uncertainty, na.rm = TRUE),
                          Uncertainty_min = min(Uncertainty, na.rm = TRUE),
                          Uncertainty_median = median(Uncertainty, na.rm = TRUE),
                          Uncertainty_995 = quantile(Uncertainty, 0.995, na.rm = TRUE),
                          Uncertainty_005 = quantile(Uncertainty, 0.005, na.rm = TRUE),
                          Uncertainty_975 = quantile(Uncertainty, 0.975, na.rm = TRUE),
                          Uncertainty_025 = quantile(Uncertainty, 0.025, na.rm = TRUE)), 
                      by = .(Model, Source, Dataset)]
  
  overall_source_conc <-
    daily_source_conc[, .(Pred.sd = mean(Pred.sd, na.rm = TRUE), 
                          Predictions = mean(Predictions, na.rm = TRUE), 
                          Uncertainty = mean(Uncertainty, na.rm = TRUE)), 
                      by = .(Model, Source, Dataset, Longitude, Latitude)]
  
  annual_source_conc <-
    daily_source_conc[, .(Pred.sd = mean(Pred.sd, na.rm = TRUE), 
                          Predictions = mean(Predictions, na.rm = TRUE), 
                          Uncertainty = mean(Uncertainty, na.rm = TRUE)), 
                      by = .(Model, Source, Dataset, Year, Longitude, Latitude)]
  
  year_month_source_conc <-
    daily_source_conc[, .(Pred.sd = mean(Pred.sd, na.rm = TRUE), 
                          Predictions = mean(Predictions, na.rm = TRUE), 
                          Uncertainty = mean(Uncertainty, na.rm = TRUE)), 
                      by = .(Model, Source, Dataset, Year, Month, Longitude, Latitude)]
  
  month_source_conc <-
    daily_source_conc[, .(Pred.sd = mean(Pred.sd, na.rm = TRUE), 
                          Predictions = mean(Predictions, na.rm = TRUE), 
                          Uncertainty = mean(Uncertainty, na.rm = TRUE)), 
                      by = .(Model, Source, Dataset, Month, Longitude, Latitude)]
  
  # Output files
  source_name_prefix = paste0(used_model, "_", dataset, "_", source_name)
  write_fst(per_source_conc, 
            paste0(source_name_prefix, "_Pred_US_01_allinOne.fst"))
  write_fst(overall_source_conc, 
            paste0(source_name_prefix, "_Pred_US_01_overall.fst"))
  write_fst(annual_source_conc, 
            paste0(source_name_prefix, "_Pred_US_01_annual.fst"))
  write_fst(year_month_source_conc, 
            paste0(source_name_prefix, "_Pred_US_01_year-month.fst"))
  write_fst(month_source_conc, 
            paste0(source_name_prefix, "_Pred_US_01_month.fst"))
  
  # CRITICAL: Clear memory before next file
  rm(daily_source_conc, per_source_conc, overall_source_conc, 
     annual_source_conc, year_month_source_conc, month_source_conc)
  gc() 
}
