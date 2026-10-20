# allYear_ML_HD_annual_monthly_001.R

library(fst)
library(data.table)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ML_Final_Input_001/Annual_combine")
getwd()

sources   <- c("bbsoa", "sulfate", "nitrate", "dust", "traffic", "pm")
years     <- 2011:2020
vars      <- c("Pred", "Unc")
data_path <- getwd()

# Grid coordinates (same row order as the prediction files)
coords   <- as.data.table(us_point_coord)[, .(Longitude, Latitude)]
n_points <- nrow(coords)

safe_div <- function(s, w) ifelse(w > 0, s / w, NA_real_)

for (source.name in sources) {
  cat("Processing source:", source.name, "\n")
  
  # Accumulators: whole period and calendar months (Jan-Dec)
  all_sum <- setNames(lapply(vars, function(v) numeric(n_points)), vars)
  all_w   <- all_sum
  mon_sum <- setNames(lapply(vars, function(v) lapply(1:12, function(m) numeric(n_points))), vars)
  mon_w   <- mon_sum
  
  annual_res <- list()
  
  for (yr in years) {
    ym <- read_fst(file.path(data_path,
                             sprintf("%s_US_RF_YearMonth_Pred_Unc_%s.fst", source.name, yr)),
                   as.data.table = TRUE)
    stopifnot(nrow(ym) == n_points)
    
    nd <- fread(file.path(data_path,
                          sprintf("%s_US_RF_YearMonth_NDays_%s.csv", source.name, yr)))
    nd[, YearMonth := as.character(YearMonth)]
    
    yr_sum <- setNames(lapply(vars, function(v) numeric(n_points)), vars)
    yr_w   <- yr_sum
    
    for (k in seq_len(nrow(nd))) {
      ym_tag <- nd$YearMonth[k]
      w      <- nd$N_days[k]
      if (w == 0) next
      mm <- as.integer(substr(ym_tag, 5, 6))
      
      for (v in vars) {
        x  <- ym[[paste0(v, "_", ym_tag)]]
        ok <- !is.na(x)
        xw <- x[ok] * w
        
        yr_sum[[v]][ok]        <- yr_sum[[v]][ok] + xw
        yr_w[[v]][ok]          <- yr_w[[v]][ok] + w
        mon_sum[[v]][[mm]][ok] <- mon_sum[[v]][[mm]][ok] + xw
        mon_w[[v]][[mm]][ok]   <- mon_w[[v]][[mm]][ok] + w
        all_sum[[v]][ok]       <- all_sum[[v]][ok] + xw
        all_w[[v]][ok]         <- all_w[[v]][ok] + w
      }
    }
    
    # Annual averages (day-weighted)
    for (v in vars) annual_res[[paste0(v, "_", yr)]] <- safe_div(yr_sum[[v]], yr_w[[v]])
    rm(ym, yr_sum, yr_w); gc()
  }
  
  # Calendar-month averages across 2011-2020 (day-weighted)
  monthly_res <- list()
  for (v in vars) for (mm in 1:12) {
    monthly_res[[paste0(v, "_", month.abb[mm])]] <- safe_div(mon_sum[[v]][[mm]], mon_w[[v]][[mm]])
  }
  
  
  # Overall study period
  overall_dt <- data.table(Pred_2011_2020 = safe_div(all_sum$Pred, all_w$Pred),
                           Unc_2011_2020  = safe_div(all_sum$Unc,  all_w$Unc))
  write_fst(cbind(coords, overall_dt),
            file.path(data_path, sprintf("%s_US_RF_Overall_Avg_Pred_Unc_2011-2020.fst", source.name)),
            compress = 75)
  
  # Annual
  annual_dt <- setDT(annual_res)
  setcolorder(annual_dt, c(grep("^Pred_", names(annual_dt)), grep("^Unc_", names(annual_dt))))
  write_fst(cbind(coords, annual_dt),
            file.path(data_path, sprintf("%s_US_RF_Annual_Avg_Pred_Unc_2011-2020.fst", source.name)),
            compress = 75)
  
  # Monthly 
  monthly_dt <- setDT(monthly_res)
  write_fst(cbind(coords, monthly_dt),
            file.path(data_path, sprintf("%s_US_RF_Monthly_Avg_Pred_Unc_2011-2020.fst", source.name)),
            compress = 75)

  rm(annual_res, monthly_res, annual_dt, monthly_dt, mon_sum, mon_w, all_sum, all_w, overall_dt); gc()
}