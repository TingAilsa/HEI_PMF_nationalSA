# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran

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
cmaq_period <- args[2]

## Read model input, use a pattern (to avoid day_count in the name)
# model_input_ini <- read_fst(
#   paste0(source.test, "_ML_input_only_PMF_sites_", day_count, "_days_", cmaq_period, ".fst"))
file_name_pattern <- 
  paste0(source.test, "_ML_input_only_PMF_sites_.*_days_", cmaq_period, "\\.fst$")
file_path <- list.files(path = ".", pattern = file_name_pattern, full.names = TRUE)[1]
model_input_ini <- read_fst(file_path)

# Site number
site_count = length(unique(model_input_ini$SiteCode))

# Print info
print(paste0("Modeling method: Random_Forest"))
print(paste0("Study period: ", cmaq_period))
print(paste0("Modeled source: ", source.test))
print(paste0("Input data: ", source.test, "-no_Longitude_Latitude"))
print(paste0("Total site number: ", site_count))

# Remove longitude and latitude
model_input_ini$Longitude = model_input_ini$Latitude = NULL

#### Random Forest modeling #### 

# Set up cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Fit Random Forest model
rf_model_noGrid_cv <- caret::train(
  PMF_conc ~ ., 
  data = dplyr::select(model_input_ini, 
                       -Dataset, -Date, -Source_aftermanual, -year, -grid_ID, -SiteCode), 
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

# Save model
saveRDS(rf_model_noGrid_cv, 
        paste0("RF_No_coords_", source.test, "_", cmaq_period, ".rds"))

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
       paste0("RF_No_coords_variable_performance_", source.test, "_", cmaq_period, ".pdf"),
       width = 9, height = 10.5)
