# AY_source_pred_inOne.R

# Temp dir
tmpdir <- file.path(Sys.getenv("SCRATCH"), "rtmp", Sys.getenv("SLURM_ARRAY_TASK_ID"))
dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TMPDIR = tmpdir, TMP = tmpdir, TEMP = tmpdir)

library(fst)
library(data.table)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/Annual_combine")
getwd()

# #### Quickly check if the Date & GPS sequence are the same from three predictions ####
# sources <- c("Traffic", "Dust", "Sulfate", "Biomass", "Nitrate", "PM25")
# years   <- "2011-2020"
# cols    <- c("Date", "Longitude", "Latitude")
# 
# for (s in sources) {
#   dt_RF  <- read_fst(paste0("RF_Pred_US_01_Daily_mean_Predictions_",  s, "_", years, ".fst"), columns = cols, as.data.table = TRUE)
#   dt_GBM <- read_fst(paste0("GBM_Pred_US_01_Daily_mean_Predictions_", s, "_", years, ".fst"), columns = cols, as.data.table = TRUE)
#   dt_CSN <- read_fst(paste0("CSN_Pred_US_01_Daily_mean_Predictions_", s, "_", years, ".fst"), columns = cols, as.data.table = TRUE)
#   
#   cat(s, "| rows:", nrow(dt_RF), nrow(dt_GBM), nrow(dt_CSN),
#       "| RF==GBM:", identical(dt_RF, dt_GBM),
#       "| RF==CSN:", identical(dt_RF, dt_CSN), "\n")
#   
#   rm(dt_RF, dt_GBM, dt_CSN); gc()
# }
# ## Yes, they are the same!

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
source_name <- args[1]

#### Read each file, drop Date/Lon/Lat from 2nd and 3rd, just keep prediction columns ####
# source_name = "Biomass"
base_name_pattern = 
  paste0("_Pred_US_01_Daily_mean_Predictions_", source_name, "_2011-2020.fst")

source_dt_base <- read_fst(paste0("RF", base_name_pattern), as.data.table = TRUE)
setnames(source_dt_base, "Predictions", "Pred_RF")
setnames(source_dt_base, "Pred.sd",     "Pred.sd_RF")

for (m in c("GBM", "CSN")) {
  source_dt_tmp <- read_fst(paste0(m, base_name_pattern), as.data.table = TRUE)
  source_dt_base[, paste0("Pred_", m)    := source_dt_tmp$Predictions]
  source_dt_base[, paste0("Pred.sd_", m) := source_dt_tmp$Pred.sd]
  rm(source_dt_tmp); gc()
}

#### Model result comparison ####
model_stats <- function(x, y) {
  fit <- lm(y ~ x)
  s   <- summary(fit)
  list(
    R2    = s$r.squared,
    pval  = coef(s)[2, 4],   # p-value for slope
    slope = coef(fit)[2],     # a
    inter = coef(fit)[1]      # b
  )
}

# Pairwise stats
pairs <- list(
  c("RF", "GBM"),
  c("RF", "CSN"),
  c("GBM", "CSN")
)

model_comp <- 
  rbindlist(lapply(pairs, function(p) {
    stats <- model_stats(source_dt_base[[paste0("Pred_", p[1])]], 
                         source_dt_base[[paste0("Pred_", p[2])]])
    data.table(
      Source = source_name,
      Pair   = paste0(p[1], "_vs_", p[2]),
      R2     = stats$R2,
      Pvalue = stats$pval,
      Slope  = stats$slope,
      Intercept = stats$inter
    )
  }))

print(model_comp)
fwrite(model_comp, paste0("Between-model_", source_name, "_2011-2020.csv"))


##### Between_model_compare on local ####

library(dplyr)
library(fst)
library(stringr)
library(lubridate)

setwd("/Users/ztttttt/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nationwide_SA/data/outputs/Aim3_inputs_predictions_10year_model")
getwd()

# # Get command line arguments
# args <- commandArgs(trailingOnly = TRUE)
# source_name <- args[1]

###### Combine single source averages to that of all sources ######
pattern <- "^Between-model_.*_2011-2020\\.csv$"
between_m_files <- list.files(pattern = pattern, full.names = TRUE)

between_m <- 
  rbindlist(lapply(between_m_files, read.csv))
View(between_m)

for (cat in categories) { # cat = categories[1]
  # Find all files matching the current category
  # This looks for files ending in e.g., "_overall.fst"
  
  if (length(cat_files) > 0) {
    message("Combining ", length(cat_files), " files for category: ", cat)
    
    # Read all files in the category and stack them
    # as.data.table = TRUE ensures we keep the speed advantage
    combined_dt <- rbindlist(lapply(cat_files, read_fst, as.data.table = TRUE), use.names = TRUE)
    
    # Save the master file
    output_name <- paste0("Combined_US_01_", cat, ".fst")
    write_fst(combined_dt, output_name) #
    
    message("Saved: ", output_name)
    
    # Clean up memory before next category
    rm(combined_dt)
    gc()
  }
}

