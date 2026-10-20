# AY_Pred_Unc_summary_001.R

# US-wide summary statistics of every map produced by RF_001_Map_Plot.R
#   Annual  : each year 2011-2020
#   Monthly : each calendar month (2011-2020 average)
#   Overall : 2011-2020
#   Daily   : the four selected days in 2020
# Statistics are across grid points (spatial), for Pred and Unc, by source and PM2.5.
#
# Usage: Rscript RF_001_Map_Summary.R

library(fst)
library(data.table)
library(writexl)

# ---- Directory ----
setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ML_Final_Input_001/Annual_combine")
getwd()
data_path <- getwd()
out_file  <- "US_RF_Pred_Unc_Summary_001.xlsx"

# ---- Sources: file name -> label ----
src_lab <- c(traffic = "Traffic Exhaust", nitrate = "Secondary Nitrate", sulfate = "Sulfate",
             bbsoa   = "Biomass Burning/OA", dust = "Dust", pm = "PM2.5")

years    <- 2011:2020
sel_days <- c("20200117", "20200417", "20200717", "20201017")

# ---- Statistics for one map (one column of values) ----
map_stats <- function(x, prefix) {
  x <- x[is.finite(x)]
  q <- quantile(x, c(0.05, 0.5, 0.95), names = FALSE)
  setNames(list(mean(x), sd(x), q[2], q[1], q[3]),
           paste0(prefix, c("_Mean", "_SD", "_Median", "_P05", "_P95")))
}

# Summarise one pair of Pred/Unc columns in a file
summarise_cols <- function(f, pred_col, unc_col) {
  dt <- read_fst(f, columns = c(pred_col, unc_col), as.data.table = TRUE)
  pred <- dt[[1]]
  unc  <- abs(dt[[2]])   # same as the maps: negative predictions give negative Unc
  c(list(N_grid = sum(is.finite(pred))),
    map_stats(pred, "Pred"),
    map_stats(unc,  "Unc"))
}

annual_rows <- list(); monthly_rows <- list(); overall_rows <- list(); daily_rows <- list()

for (s in names(src_lab)) {
  cat("Processing:", s, "\n")
  
  # ---- Annual ----
  f <- file.path(data_path, sprintf("%s_US_RF_Annual_Avg_Pred_Unc_2011-2020.fst", s))
  for (yr in years) {
    annual_rows[[length(annual_rows) + 1]] <-
      c(list(Source = src_lab[[s]], Year = yr),
        summarise_cols(f, paste0("Pred_", yr), paste0("Unc_", yr)))
  }
  
  # ---- Monthly ----
  f <- file.path(data_path, sprintf("%s_US_RF_Monthly_Avg_Pred_Unc_2011-2020.fst", s))
  for (mm in 1:12) {
    monthly_rows[[length(monthly_rows) + 1]] <-
      c(list(Source = src_lab[[s]], Month = month.name[mm]),
        summarise_cols(f, paste0("Pred_", month.abb[mm]), paste0("Unc_", month.abb[mm])))
  }
  
  # ---- Overall ----
  f <- file.path(data_path, sprintf("%s_US_RF_Overall_Avg_Pred_Unc_2011-2020.fst", s))
  overall_rows[[length(overall_rows) + 1]] <-
    c(list(Source = src_lab[[s]], Period = "2011-2020"),
      summarise_cols(f, "Pred_2011_2020", "Unc_2011_2020"))
  
  # ---- Daily (four selected days) ----
  f <- file.path(data_path, sprintf("%s_US_RF_Pred_Unc_001_4days_in_2020.fst", s))
  for (d in sel_days) {
    daily_rows[[length(daily_rows) + 1]] <-
      c(list(Source = src_lab[[s]], Date = format(as.Date(d, "%Y%m%d"), "%Y-%m-%d")),
        summarise_cols(f, paste0("Pred_", d), paste0("Unc_", d)))
  }
  gc()
}

# ---- Combine and round ----
tidy <- function(rows) {
  dt <- rbindlist(rows)
  num <- setdiff(names(dt)[sapply(dt, is.double)], "N_grid")
  dt[, (num) := lapply(.SD, round, 4), .SDcols = num]
  dt[, Source := factor(Source, levels = unname(src_lab))]
  setorder(dt, Source)
  dt
}

notes <- data.table(
  Item = c("Statistics", "Pred", "Unc", "Annual", "Monthly", "Overall", "Daily",
           "N_grid", "Pred units"),
  Description = c(
    "Computed across all grid points of each map (spatial), not over time.",
    "Mean prediction from 50 iterations.",
    "Uncertainty (%) = SD across 50 iterations / mean prediction x 100, absolute value, as in the maps.",
    "Day-weighted annual average for each year.",
    "Day-weighted average of each calendar month over 2011-2020.",
    "Day-weighted average over 2011-2020.",
    "Daily values on the four selected days in 2020.",
    "Number of grid points with a valid prediction.",
    "ug/m3 (source impacts; PM2.5 concentration)."))

write_xlsx(list(Annual  = tidy(annual_rows),
                Monthly = tidy(monthly_rows),
                Overall = tidy(overall_rows),
                Daily   = tidy(daily_rows),
                Notes   = notes),
           out_file)

cat("Saved:", out_file, "\n")