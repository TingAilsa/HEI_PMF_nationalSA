# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran

library(base)
library(dplyr)
library(data.table)
library(fst)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
getwd()
base_dir = getwd()

#### 0. Extract long & points with the continental US ####
#us_states = USAboundaries::us_states()
#us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

#### 1. Modeling Input ####

cmaq_period = "2011-01_2011-12"; cmaq_year = 2011
# cmaq_period = "2017-01_2017-12"; cmaq_year = 2017

# All date related predictors 
date_use = read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Date_DOW_Holiday_2011-20.fst")
us_point_coord = read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Long_lat_Mainland_US_0.1_degree.fst")
# dim(us_point_coord)
# head(date_use)

###### 1.3 Traffic input ######
##### Traffic
# model_input_all_grid_imputed = read_fst(paste0("Traffic_all_CMAQ_points_input_", cmaq_period, "_commute_mice_interpolated.fst"))
model_input_all_grid = read_fst(paste0("Traffic_all_CMAQ_points_input_", cmaq_period, ".fst"))
# model_input_all_grid$PM25_TOT_Traf = 
#   model_input_all_grid$PM25_TOT_NRD + model_input_all_grid$PM25_TOT_ONR
source_name = "Traffic"

##### Dust
model_input_all_grid = read_fst(paste0("Dust_all_CMAQ_points_input_", cmaq_period, ".fst"))
source_name = "Dust"

##### Sulfate
model_input_all_grid = read_fst(paste0("Sulfate_all_CMAQ_points_input_", cmaq_period, ".fst"))
source_name = "Sulfate"


#### Shared Process for CMAQ variable extraction ####
# Change name
model_input_all_grid <-
  plyr::rename(model_input_all_grid,
               c("Concentration" = "PMF_conc"))
head(model_input_all_grid)

# Select columns related to CMAQ
model_input_all_grid_cmaq = 
  dplyr::select(model_input_all_grid, Longitude:SiteCode)
model_input_all_grid_cmaq$SiteCode = NULL

# Add PMF results
model_input_all_grid_cmaq$PMF_conc = model_input_all_grid$PMF_conc
summary(model_input_all_grid_cmaq)

# Filter points within mainland US
model_cmaq_all_grid_us <- 
  merge(model_input_all_grid_cmaq, us_point_coord, all.y = TRUE)
summary(model_cmaq_all_grid_us); dim(model_cmaq_all_grid_us)

# Update the year month day of week info
# date_model_use = subset(date_use, Date %in% model_cmaq_all_grid_us$Date)
# model_cmaq_all_grid_us =
#   merge(model_cmaq_all_grid_us, date_model_use, all.x = TRUE)
# summary(model_cmaq_all_grid_us); dim(model_cmaq_all_grid_us)

write_fst(model_cmaq_all_grid_us, 
          paste0(source_name, "_CMAQ_mainlandUS_", cmaq_period, ".fst"))



