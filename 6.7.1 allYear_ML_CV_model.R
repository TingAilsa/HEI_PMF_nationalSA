# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran
# 6.7.1 allYear_ML_CV_model.R

library(base)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(timeDate) #holidayNYSE{}
library(fst)
library(terra)
library(caret) # trainControl
library(randomForest)
library(ggplot2)
library(patchwork)
library(randomForest)
library(doParallel) # parallel backend
# library(pdp) # partial dependence in RF
#library(USAboundaries)

# Detect the number of cores available on the server
num_cores <- detectCores() - 1  # Use all but one core
num_cores

# Create a parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/MLresult_RF/")

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
getwd()
base_dir = getwd()

#### Extract info from bash script and read model input ####

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
source.test <- args[1]
included_years <- args[2]

#### 1. Modeling Input ####

# included_years = "2011-2017"

model_input_ini = read_fst(paste0(source.test, "_ML_input_only_PMF_sites_", included_years, ".fst"))
# model_input_all_grid = read_fst(paste0(source.test, "_ML_input_mainlandUS_", included_years, ".fst"))

# dim(model_input_mainland_more); dim(model_input_all_grid)

# Site number
site_count = length(unique(model_input_ini$SiteCode))

cat("Study period: ", included_years)
cat("Modeled source: ", source.test)
cat("Initial site number: ", site_count)

print("Intial input file check")
dim(model_input_ini); head(model_input_ini)
# dim(model_input_all_grid); head(model_input_all_grid)

# head(data.frame(table(model_input_ini$Longitude)))
# head(data.frame(table(model_input_ini$Latitude)))

model.method = "Random_Forest"

#### Use dataset WITHOUT Longitude or Latitude!!!!!
model_input_use = model_input_ini
model_input_use$Longitude = model_input_use$Latitude = NULL
# model_input_all_grid$Longitude = model_input_all_grid$Latitude = NULL

###### Random Forest modeling ###### 

unique_sites <- unique(model_input_use$grid_ID)

# Set up 5-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Fit the Random Forest model with and without grid_ID
rf_model_noGrid_cv <- 
  caret::train(
    PMF_conc ~ ., 
    data = dplyr::select(model_input_use, 
                         -Dataset, -Date, -Source_aftermanual, -grid_ID, -SiteCode), 
    method = "rf",
    trControl = train_control,
    importance = TRUE
  )

# Save model
saveRDS(rf_model_noGrid_cv,
        paste0("RF_AllYear_", source.test, "_", included_years, ".rds"))

#### Plot the relative variable importance of each predictor #### 

# Variable importance info
rf_cv_var_imp <- varImp(rf_model_noGrid_cv, scale = TRUE)
rf_cv_var_imp_df <- rf_cv_var_imp$importance
rf_cv_var_imp_df$Variable <- rownames(rf_cv_var_imp_df)
rownames(rf_cv_var_imp_df) <- NULL
names(rf_cv_var_imp_df) <- c("Relative_Influence", "Variable")

# Create plot
rf_var_influence_p <- ggplot(rf_cv_var_imp_df,
                             aes(x = reorder(Variable, Relative_Influence),
                                 y = Relative_Influence)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  coord_flip() +
  labs(title = "Variable Performance",
       x = "Predictor",
       y = "Relative Influence (%)") +
  theme_minimal(base_size = 16)

# Save plot
ggsave(plot = rf_var_influence_p,
       paste0("RF_AllYear_variable_performance_", source.test, "_", included_years, ".pdf"),
       width = 9, height = 10.5)



