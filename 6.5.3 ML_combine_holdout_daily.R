# ML_combine_holdout_daily.R

library(dplyr)
library(fst)
library(data.table)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout")
getwd()

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
source.test <- args[1] 
cmaq_period <- args[2]
# iteration <- as.integer(args(3)) # Ensure numeric format

# source.test = "Traffic"; cmaq_period = "2013-01_2013-12"

# Extract study year
cmaq_year = substring(cmaq_period, 1, 4)

print(paste("Processing source:", source.test))
print(paste("Period:", cmaq_period))
print(paste("Year:", cmaq_year))
# print(paste("Iteration:", iteration))

# Directory of annual combined file from all iterations
combine_path <- "Annual_combine/"

# Initiate a list to store predictions from all iterations
iteration_predictions_list_1 <- vector("list", 25)
iteration_predictions_list_2 <- vector("list", 25)

###### First half of combination ###### 
for(iteration in 1:25) { # 25, iteration number
  print(paste("Processing iteration:", iteration))
  
  # Get all prediction files for this source and period
  prediction_dir <- 
    paste0(source.test, "/", cmaq_year, "/Iteration_", iteration)
  prediction_files <-
    list.files(prediction_dir,
               pattern = "_holdout_pred_\\d{4}-\\d{2}-\\d{2}\\.fst$",
               full.names = TRUE)
  
  print("The first five files from the current Iteration")
  prediction_files[1:5]
  print("Number of files found: for each Iteration")
  length(prediction_files)
  
  # Store combined predictions for this iteration, use data.table for more storage-efficient process
  iteration_predictions_list_1[[iteration]] <- 
    rbindlist(lapply(prediction_files, function(x) {
      as.data.table(read_fst(x))
    }))[, Iteration := iteration]
  
  # Clean up memory
  gc()
}

print("Start to combine all results and get the averages")

# Combine all iterations efficiently using bind_rows
# For scripts include large datasets, clean memory from those no longer needed
all_combined_iteration_pred_1 <- rbindlist(iteration_predictions_list_1)
rm(iteration_predictions_list_1)
gc()

# Save combined results
write_fst(all_combined_iteration_pred_1,
          paste0(combine_path,
                 "RF_Pred_HD_US_01_grids_noUnc_noCoords_allDaily_", 
                 source.test, "_", cmaq_period, "_1.fst"))
rm(all_combined_iteration_pred_1)
gc()

###### Second half of combination ###### 
for(iteration in 26:50) { # 25, iteration number
  print(paste("Processing iteration:", iteration))
  
  # Get all prediction files for this source and period
  prediction_dir <- 
    paste0(source.test, "/", cmaq_year, "/Iteration_", iteration)
  prediction_files <-
    list.files(prediction_dir,
               pattern = "_holdout_pred_\\d{4}-\\d{2}-\\d{2}\\.fst$",
               full.names = TRUE)
  
  print("The first five files from the current Iteration")
  prediction_files[1:5]
  print("Number of files found: for each Iteration")
  length(prediction_files)
  
  # Store combined predictions for this iteration, use data.table for more storage-efficient process
  iteration_predictions_list_2[[iteration]] <- 
    rbindlist(lapply(prediction_files, function(x) {
      as.data.table(read_fst(x))
    }))[, Iteration := iteration]
  
  # Clean up memory
  gc()
}

print("Start to combine all results and get the averages")

# Combine all iterations efficiently using bind_rows
# For scripts include large datasets, clean memory from those no longer needed
all_combined_iteration_pred_2 <- rbindlist(iteration_predictions_list_2)
rm(iteration_predictions_list_2)
gc()

# Save combined results
write_fst(all_combined_iteration_pred_2,
          paste0(combine_path,
                 "RF_Pred_HD_US_01_grids_noUnc_noCoords_allDaily_", 
                 source.test, "_", cmaq_period, "_2.fst"))

rm(all_combined_iteration_pred_2)
gc()


###### Read, combine daily outputs, and get overall estimation #####

# Read and estimate the mean prediction of the first group
all_combined_iteration_pred_1 =
  read_fst(paste0(combine_path,
                  "RF_Pred_HD_US_01_grids_noUnc_noCoords_allDaily_", 
                  source.test, "_", cmaq_period, "_1.fst"))
setDT(all_combined_iteration_pred_1)

avg_all_iteration_pred_1 <- 
  all_combined_iteration_pred_1[
    , .(Predictions = mean(Predictions, na.rm = TRUE)),
    by = setdiff(names(all_combined_iteration_pred_1), 
                 c("Predictions", "Iteration"))
  ]

write_fst(avg_all_iteration_pred_1, 
          paste0(combine_path,
                 "RF_Pred_HD_US_01_grids_noUnc_noCoords_", 
                 source.test, "_", cmaq_period, "_1.fst"))

rm(all_combined_iteration_pred_1)
gc()


# Read and estimate the mean prediction of the second group
all_combined_iteration_pred_2 =
  read_fst(paste0(combine_path,
                  "RF_Pred_HD_US_01_grids_noUnc_noCoords_allDaily_", 
                  source.test, "_", cmaq_period, "_2.fst"))

setDT(all_combined_iteration_pred_2)

avg_all_iteration_pred_2 <- 
  all_combined_iteration_pred_2[
    , .(Predictions = mean(Predictions, na.rm = TRUE)),
    by = setdiff(names(all_combined_iteration_pred_2), 
                 c("Predictions", "Iteration"))
  ]

write_fst(avg_all_iteration_pred_2, 
          paste0(combine_path,
                 "RF_Pred_HD_US_01_grids_noUnc_noCoords_", 
                 source.test, "_", cmaq_period, "_2.fst"))

rm(all_combined_iteration_pred_2)
gc()


## Combine the file

# # Only for Traffic
# rm(avg_all_iteration_pred_1)
# rm(avg_all_iteration_pred_2)
# 
# avg_all_iteration_pred_1 = 
#   read_fst(paste0(combine_path,
#                   "RF_Pred_HD_US_01_grids_noUnc_noCoords_", 
#                   source.test, "_", cmaq_period, "_1.fst"))
# 
# avg_all_iteration_pred_2 = 
#   read_fst(paste0(combine_path,
#                   "RF_Pred_HD_US_01_grids_noUnc_noCoords_", 
#                   source.test, "_", cmaq_period, "_2.fst"))

avg_all_iteration_pred =
  rbind(avg_all_iteration_pred_1, avg_all_iteration_pred_2
        )

# Get the averages
avg_all_iteration_pred = 
  avg_all_iteration_pred %>%
  dplyr::group_by(Longitude, Latitude, Date) %>%
  dplyr::summarize(
    dplyr::across(-Predictions, first),
    Predictions = mean(Predictions),
    .groups = "drop"
  )

print("Check the summary of the averaged predictions")
summary(avg_all_iteration_pred); dim(avg_all_iteration_pred)

# Save combined results
write_fst(avg_all_iteration_pred,
          paste0(combine_path,
                 "RF_Pred_HD_US_01_grids_noUnc_noCoords_", 
                 source.test, "_", cmaq_period, ".fst"))

# rm(all_combined_iteration_pred)
# rm(avg_all_iteration_pred)
# gc()

