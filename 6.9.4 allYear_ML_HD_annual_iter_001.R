# allYear_ML_HD_annual_001.R

library(fst)
library(data.table)
library(matrixStats)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ML_Final_Input_001")
getwd()

# Grid coordinates
us_point_coord =
  read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Long_lat_CONUS_0.01_degree.fst")
n_points <- nrow(us_point_coord)

# Number of iteration
n_iter = 50


# # Get command line arguments
# args <- commandArgs(trailingOnly = TRUE)
# source.test <- args[1]
# time_year <- args[2]

# source.test = "sulfate"; time_year = 2013

# source.test = "bbsoa"

cat("Processing source:", source.test, "\n")

# Paths for read and save files
combine_path <- "Annual_combine/"
pred_path_itr <- paste0("Prediction_results/", source.test)

for (time_year in 2015:2020) {
  # time_year = 2011
  cat("Processing iterations 1 to 50 for year", time_year, "\n")
  
  # All dates of the year & day_tags
  dates <-
    seq(as.Date(paste0(time_year, "-01-01")), 
        as.Date(paste0(time_year, "-12-31")), by = "day")
  day_tags <- 
    gsub("-", "", as.character(dates))   # "20110101" -> valid column names
  month_tags <- format(dates, "%Y%m")
  
  # Files to save daily predictions & uncertainties
  pred_list <- vector("list", length(dates))
  names(pred_list) <- paste0("Pred_", day_tags)

  unc_list <- vector("list", length(dates))
  names(unc_list) <- paste0("Unc_",   day_tags)
  
  has_data  <- logical(length(dates))
  
  ###### Get the daily mean Predictions & Uncertainty from 50 iterations ###### 
  for (i in seq_along(dates)) { # seq_along(dates) # 1:5
    # i = 3
    
    # Day to process
    d_use <- as.character(dates[i])
    
    # File of the day from all iterations
    day_files <- 
      file.path(
        paste0(pred_path_itr, sprintf("/Iteration_%s", 1:n_iter)),
        pattern = sprintf("%s_RF_HD_Daily_pred_001_%s.fst", source.test, d_use))
    day_files <- 
      day_files[file.exists(day_files)]
    cat("Number of detected daily predictions from 50 iterations:", 
        source.test, length(day_files), d_use, "\n")
    
    # Keep a placeholder column if a day has no files, hopefully no such thing...
    if (length(day_files) == 0) {
      pred_list[[i]] <- rep(NA_real_, n_points)
      unc_list[[i]]   <- rep(NA_real_, n_points)
      next
    }
    
    # Grid x iterations matrix; vapply stops if any file has the wrong row count
    pred_mat <- 
      vapply(day_files,
             function(f) read_fst(f, columns = "Predictions")[[1]],
             numeric(n_points)) # not n_points directly, otherwise, vapply will expect every file to return one integer value
    
    # Get uncertainty, and set NA in Unc if needed
    day_pred <- rowMeans(pred_mat)
    day_unc  <- rowSds(pred_mat) / day_pred * 100
    day_unc[!is.finite(day_unc)] <- NA_real_   # prediction = 0 gives Inf/NaN
    
    # Save into annual list
    pred_list[[i]] <- day_pred
    unc_list[[i]]  <- day_unc
    has_data[i]    <- TRUE
    
    # rm(pred_mat, day_pred, day_unc)
  }
  
  ###### Year-Month means: Predictions and Uncertainty in one file ######
  ym_out  <- list()
  months  <- unique(month_tags)
  
  # Year-Month means
  for (m in months) {
    cols <- which(month_tags == m)
    ym_out[[paste0("Pred_", m)]] <- 
      rowMeans(do.call(cbind, pred_list[cols]), na.rm = TRUE)
    ym_out[[paste0("Unc_",  m)]] <- 
      rowMeans(do.call(cbind, unc_list[cols]),  na.rm = TRUE)
  }
  ym_dt <- setDT(ym_out)
  
  # Days with data per Year-Month (weights for later annual/overall averages)
  days_in_mo <-
    data.table(Source = source.test, 
               YearMonth = months,
               N_days = 
                 sapply(months, 
                        function(m) 
                          sum(has_data[month_tags == m])))
  
  # In case of NA exists
  for (col in names(ym_dt)) set(ym_dt, which(is.nan(ym_dt[[col]])), col, NA_real_)
  setcolorder(ym_dt, 
              c(grep("^Pred_", names(ym_dt)),
                grep("^Unc_", names(ym_dt))))
  
  # Output 
  write_fst(ym_dt,
            file.path(combine_path, 
                      sprintf("%s_US_RF_YearMonth_Pred_Unc_%s.fst", source.test, time_year)),
            compress = 75)
  rm(ym_dt, ym_out); gc()
  
  # Days with data per Year-Month (weights for later annual/overall averages)
  fwrite(days_in_mo,
         file.path(combine_path, 
                   sprintf("%s_US_RF_YearMonth_NDays_%s.csv", source.test, time_year)))
  
  
  ###### Save annual files ######
  # rows = grid points (same order as grid file), columns = days
  annual_Pred <- setDT(pred_list)
  write_fst(annual_Pred,
            file.path(combine_path, sprintf("%s_US_RF_Daily_Pred_%s.fst", source.test, time_year)),
            compress = 75)
  rm(annual_Pred, pred_list); gc()
  
  annual_Unc <- setDT(unc_list)
  write_fst(annual_Unc,
            file.path(combine_path, sprintf("%s_US_RF_Daily_Unc_%s.fst", source.test, time_year)),
            compress = 75)
  rm(annual_Unc, unc_list); gc()
}

