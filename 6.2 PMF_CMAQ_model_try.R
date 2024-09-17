library(base)
library(dplyr)
library(data.table)
library(raster) 
library(ncdf4) 
library(sf)
library(lubridate)
library(timeDate) #holidayNYSE{}

setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data")
getwd()

#### 1. etract CMAQ source info ####

cmaq_path <- "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya"
# file.exists(cmaq_path)
cmaq_rds_list <- list.files(cmaq_path, pattern = "\\.rds$", full.names = TRUE)
cmaq_rds_list

# initialize an empty list for faster binding of files with geometry
cmaq_pm_source_list = list()

# sources to use for now
source_to_use = c("PM25_TOT_EGU", "PM25_TOT_DUST")

for (cmaq_single_path in cmaq_rds_list){
  
  cmaq_pm = readRDS(cmaq_single_path)
  
  cmaq_pm_month = 
    subset(cmaq_pm, variable %in% source_to_use)
  
  cmaq_pm_source_list[[length(cmaq_pm_source_list) + 1]] <-
    cmaq_pm_month
  # head(cmaq_pm_source_list); sapply(cmaq_pm_source_list, class)
}

cmaq_pm_source_all = do.call(rbind, cmaq_pm_source_list)
head(cmaq_pm_source_all); dim(cmaq_pm_source_all)
class(cmaq_pm_source_all); sapply(cmaq_pm_source_all, class)
unique(cmaq_pm_source_all$variable)

cmaq_pm_source_all$Date = day_of_year_toDate(cmaq_pm_source_all$day)
# cmaq_pm_source_all$day = NULL
# saveRDS(cmaq_pm_source_all, "CMAQ_Sumaiya/CMAQ_extracted_PM_dust_egu_201701-03.rds")

#### 2. etract PMF source info ####
csn_source = fread("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/CSN_Site_15t1mdl0unc_source_daily_contribution.csv")
imp_source = fread("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/IMPROVE_Site_15t1mdlVNi_DN_source_daily_contribution.csv")
head(csn_source); dim(csn_source)
head(imp_source); dim(imp_source)
summary(names(imp_source) == names(csn_source))

# combine and only keep columns to be used for modeling
pmf_source = rbind(csn_source, imp_source)
pmf_source = pmf_source[, .(Date, Latitude, Longitude, Source_aftermanual, Concentration)]
write.csv(pmf_source, 
          "PMF_results/CSN_IMPROVE_source_daily_contribution.csv", 
          row.names=FALSE)


#### 3. Gradient Boosting Machines (GBM) modeling ####

###### 3.1 GBM, merging data ###### 
# load datasets to use 
# CMAQ source
cmaq_source_all = readRDS("CMAQ_Sumaiya/CMAQ_extracted_PM_dust_egu_201701-03.rds") 
# PMF source
pmf_source = fread("PMF_results/CSN_IMPROVE_source_daily_contribution.csv") 
# NCLD land use type
ncld_year <- raster("NLCD_landcover/nlcd_2016_land_cover_l48_20210604/nlcd_2016_land_cover_l48_20210604.img") 

# Extract year, month, day of week, and is_holiday (whether a day is a US national holiday)
pmf_source$Date = as.Date(pmf_source$Date)
pmf_source[, `:=`(
  year = year(Date),
  month = month(Date),
  day_of_week = wday(Date, label = TRUE),
  is_holiday = Date %in% as.Date(holidayNYSE(year(Date)))  # US holiday (can adjust for local holidays)
)]


# add coordinate reference system to pmf_source and combine with land use
pmf_source_crs = 
  st_as_sf(pmf_source, coords = c("Longitude", "Latitude"),
           crs = st_crs(ncld_year))

pmf_source$land_use_type = extra(ncld_year, pmf_source_crs)


# extract single source file
pmf_single_source = subset(pmf_source, Source_aftermanual == "F2-Secondary Sulfate")
cmaq_single_source = subset(cmaq_source_all, variable = "PM_TOT_PEGU")

# merge with CMAQ file 
pmf_cmaq_single_source = 
  st_join(st_as_sf(pmf_single_source, 
                   coords = c("Longitude", "Latitude"),
                   crs = st_crs(cmaq_single_source)),
                   cmaq_single_source,
                   join = st_nearest_feature)

###### 3.2 GBM, modeling ###### 

# Prepare data for modeling
sa_model_input = 
  pmf_cmaq_single_source[, .(Concentration, value, land_use_type, year, month, day_of_week, is_holiday)]

# Convert categorical variables to factors
sa_model_input[, land_use_type := as.factor(land_use_type)]
sa_model_input[, day_of_week := as.factor(day_of_week)]
sa_model_input[, is_holiday := as.factor(is_holiday)]

# Set up 5-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Fit the GBM model
gbm_model <- train(
  Concentration ~ 
    value + land_use_type + month + day_of_week + is_holiday, #  + year
  data = sa_model_input,
  method = "gbm",
  trControl = train_control,
  verbose = FALSE
)

# View model results
summary(gbm_model)

# Predict on the training data
predictions <- predict(gbm_model, sa_model_input)

# Evaluate the model performance
performance <- 
  postResample(predictions, sa_model_input$Concentration)
print(performance)





