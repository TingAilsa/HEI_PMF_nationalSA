library(base)
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
library(gbm)
library(randomForest)
# library(torch)
library(ggplot2)
library(patchwork)
library(randomForest)
library(iml)
library(doParallel) # parallel backend

# Detect the number of cores available on the server
num_cores <- detectCores() - 1  # Use all but one core
num_cores

# Create a parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)


setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data")
getwd()
base_dir = getwd()

#### 1. CMAQ source data #### 
CMAQ_path = paste0(base_dir, "/CMAQ_Sumaiya/var_combined_rds/")

# read CMAQ & add crs
cmaq_period = "2011-02_2011-08"

# NH3
cmaq_nh3_rds = readRDS(paste0(CMAQ_path, "NH3_cmaq_", cmaq_period, ".rds"))
class(cmaq_nh3_rds); head(cmaq_nh3_rds)
summary(cmaq_nh3_rds)

# SO2
cmaq_so2_rds <- readRDS(paste0(CMAQ_path, "SO2_cmaq_", cmaq_period, ".rds"))
summary(cmaq_so2_rds)

# DUST
cmaq_dust_rds <- readRDS(paste0(CMAQ_path, "PM25_TOT_DUST_cmaq_", cmaq_period, ".rds"))
summary(cmaq_dust_rds)

# EGU
cmaq_egu_rds <- readRDS(paste0(CMAQ_path, "PM25_TOT_EGU_cmaq_", cmaq_period, ".rds"))
summary(cmaq_egu_rds)

# NRD
cmaq_nrd_rds <- readRDS(paste0(CMAQ_path, "PM25_TOT_NRD_cmaq_", cmaq_period, ".rds"))
summary(cmaq_nrd_rds)

# ORD
cmaq_ord_rds <- readRDS(paste0(CMAQ_path, "PM25_TOT_ONR_cmaq_", cmaq_period, ".rds"))
summary(cmaq_nrd_rds)

### merge rds for each source
cmaq_sulfate_rds = merge(cmaq_nh3_rds, cmaq_so2_rds)
cmaq_sulfate_rds = merge(cmaq_sulfate_rds, cmaq_egu_rds)
cmaq_traffic_rds = merge(cmaq_nrd_rds, cmaq_ord_rds)
dim(cmaq_traffic_rds); dim(cmaq_ord_rds)

#### REMOVE 2011.06 DATA, only for now
cmaq_sulfate_rds = 
  subset(cmaq_sulfate_rds, Date < as.Date("2011-06-01") | Date > as.Date("2011-06-30"))
cmaq_traffic_rds = 
  subset(cmaq_traffic_rds, Date < as.Date("2011-06-01") | Date > as.Date("2011-06-30"))
cmaq_dust_rds = 
  subset(cmaq_dust_rds, Date < as.Date("2011-06-01") | Date > as.Date("2011-06-30"))

### CMAQ crs construction
# Open the .nc file
nc_cmaq_file <- nc_open("CMAQ_Sumaiya/hr2day_SA_v54_gcc_CMAQ_ISAM_201106.nc")

# Extract grid information
xorig <- ncatt_get(nc_cmaq_file, varid = 0, 'XORIG')$value
yorig <- ncatt_get(nc_cmaq_file, varid = 0, 'YORIG')$value
ncols <- ncatt_get(nc_cmaq_file, varid = 0, 'NCOLS')$value
nrows <- ncatt_get(nc_cmaq_file, varid = 0, 'NROWS')$value
xcell <- ncatt_get(nc_cmaq_file, varid = 0, 'XCELL')$value
ycell <- ncatt_get(nc_cmaq_file, varid = 0, 'YCELL')$value
p_alp <- ncatt_get(nc_cmaq_file, varid = 0, 'P_ALP')$value
p_bet <- ncatt_get(nc_cmaq_file, varid = 0, 'P_BET')$value
xcent <- ncatt_get(nc_cmaq_file, varid = 0, 'XCENT')$value
ycent <- ncatt_get(nc_cmaq_file, varid = 0, 'YCENT')$value

# create p4s CRS
#https://forum.cmascenter.org/t/equates-grid-coordinates/3018/3
# p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
p4s <- paste( '+proj=lcc', # projection type: Lambert Conformal Conic
              paste0( 'lat_1=', p_alp),
              paste0( 'lat_2=', p_bet),
              paste0( 'lat_0=', ycent),
              paste0( 'lon_0=', xcent),
              'a=6370000 +b=6370000', # Ellipsoid parameters (radius of Earth)
              sep = ' +')

cmaq_crs = p4s

## Add crs
# cmaq for each source category, Sulfate
cmaq_sulfate = st_as_sf(cmaq_sulfate_rds, coords = c("x", "y"), crs = cmaq_crs)

# cmaq for each source category, Dust
cmaq_dust = st_as_sf(cmaq_dust_rds, coords = c("x", "y"), crs = cmaq_crs)

# cmaq for each source category, Traffic
cmaq_traffic = st_as_sf(cmaq_traffic_rds, coords = c("x", "y"), crs = cmaq_crs)

st_write(cmaq_sulfate, paste0("CMAQ_Sulfate_2011.gpkg"))
st_write(cmaq_dust, paste0("CMAQ_Dust_2011.gpkg"))
st_write(cmaq_traffic, paste0("CMAQ_Traffic_2011.gpkg"))

#### 2. PMF, census, & NCLD data ####
year = 2011
pmf_census_ncld_grid = st_read(paste0("PMF_NCLD_Census_by_year/PMF_source_NCLD_land_ACS_Census_", year, ".gpkg"))

pmf_census_nlcd_Sulfate = subset(pmf_census_ncld_grid, Source_aftermanual == "F3-Secondary Sulfate")
pmf_census_nlcd_Dust = subset(pmf_census_ncld_grid, Source_aftermanual == "F9-Soil/Dust")
pmf_census_nlcd_Traffic = subset(pmf_census_ncld_grid, Source_aftermanual == "F1-Traffic")

# convert pmf_nlcd crs to CMAQ
pmf_census_nlcd_Sulfate <- st_transform(pmf_census_nlcd_Sulfate, crs = st_crs(cmaq_sulfate))
pmf_census_nlcd_Dust <- st_transform(pmf_census_nlcd_Dust, crs = st_crs(cmaq_dust))
pmf_census_nlcd_Traffic <- st_transform(pmf_census_nlcd_Sulfate, crs = st_crs(cmaq_traffic))

st_write(pmf_census_nlcd_Sulfate, paste0("PMF_Sulfate_NCLD_land_ACS_Census_2011.gpkg"))
st_write(pmf_census_nlcd_Dust, paste0("PMF_Dust_NCLD_land_ACS_Census_2011.gpkg"))
st_write(pmf_census_nlcd_Traffic, paste0("PMF_Traffic_NCLD_land_ACS_Census_2011.gpkg"))

#### 3. data combine and model input preparation ####
###### 3.1 CMAQ, PMF, ect. combine and model input preparation ####
pmf_census_nlcd_Sulfate = st_read(paste0("PMF_Sulfate_NCLD_land_ACS_Census_2011.gpkg"))
pmf_census_nlcd_Dust = st_read(paste0("PMF_Dust_NCLD_land_ACS_Census_2011.gpkg"))
pmf_census_nlcd_Traffic = st_read(paste0("PMF_Traffic_NCLD_land_ACS_Census_2011.gpkg"))

cmaq_sulfate = st_read(paste0("CMAQ_Sulfate_2011.gpkg"))
cmaq_dust = st_read(paste0("CMAQ_Dust_2011.gpkg"))
cmaq_traffic = st_read(paste0("CMAQ_Traffic_2011.gpkg"))

# combine pmf_nlcd with CMAQ for each source category, & remove not matched Date, nearest point
# sulfate
nearest_index_sulfate <- st_nearest_feature(pmf_census_nlcd_Sulfate, cmaq_sulfate)
pmf_cmaq_sulfate <- cbind(pmf_census_nlcd_Sulfate, cmaq_sulfate[nearest_index_sulfate, ])
dim(pmf_census_nlcd_Sulfate); dim(pmf_cmaq_sulfate)

# pmf_cmaq_sulfate_date = 
#   dplyr::select(pmf_cmaq_sulfate, Date, Date.1, SO2, NH3, GEOID) 
# 
# pmf_cmaq_sulfate_date = 
#   subset(pmf_cmaq_sulfate_date,
#          Date == Date.1)
# head(pmf_cmaq_sulfate_date)

pmf_cmaq_sulfate <- 
  pmf_cmaq_sulfate %>%
  filter(Date == Date.1)
dim(pmf_census_nlcd_Sulfate); dim(pmf_cmaq_sulfate); head(pmf_cmaq_sulfate)
pmf_cmaq_sulfate$Date.1 = pmf_cmaq_sulfate$geometry.1 = pmf_cmaq_sulfate$geometry = NULL
summary(pmf_cmaq_sulfate)

# dust
nearest_index_dust <- st_nearest_feature(pmf_census_nlcd_Dust, cmaq_dust)
pmf_cmaq_dust <- cbind(pmf_census_nlcd_Dust, cmaq_dust[nearest_index_dust, ])
dim(pmf_census_nlcd_Dust); dim(pmf_cmaq_dust)
pmf_cmaq_dust <- 
  pmf_cmaq_dust %>%
  filter(Date == Date.1)
dim(pmf_census_nlcd_Dust); dim(pmf_cmaq_dust); head(pmf_cmaq_dust)
pmf_cmaq_dust$Date.1 = pmf_cmaq_dust$geometry.1 = pmf_cmaq_dust$geometry = NULL
summary(pmf_cmaq_dust)

# traffic
nearest_index_traffic <- st_nearest_feature(pmf_census_nlcd_Traffic, cmaq_traffic)
pmf_cmaq_traffic <- cbind(pmf_census_nlcd_Traffic, cmaq_traffic[nearest_index_traffic, ])
dim(pmf_census_nlcd_Traffic); dim(pmf_cmaq_traffic)
pmf_cmaq_traffic <- 
  pmf_cmaq_traffic %>%
  filter(Date == Date.1)
dim(pmf_census_nlcd_Traffic); dim(nearest_index_dust); head(pmf_cmaq_traffic)
pmf_cmaq_traffic$Date.1 = pmf_cmaq_traffic$geometry.1 = pmf_cmaq_traffic$geometry = NULL
summary(pmf_cmaq_traffic)

## check if the three nearest_index are the same
length(nearest_index_sulfate); length(nearest_index_dust); length(nearest_index_traffic)
summary(nearest_index_sulfate); summary(nearest_index_dust); summary(nearest_index_traffic)
summary(nearest_index_sulfate %in% nearest_index_dust)
summary(nearest_index_sulfate %in% nearest_index_traffic)
summary(nearest_index_dust %in% nearest_index_sulfate)
summary(nearest_index_dust %in% nearest_index_traffic)
summary(nearest_index_traffic %in% nearest_index_dust)
summary(nearest_index_traffic %in% nearest_index_sulfate)

class(pmf_cmaq_traffic)
write_sf(pmf_cmaq_sulfate, paste0("PMF_CMAQ_NLCD_Census_", cmaq_period, "_Sulfate.gpkg"))
write_sf(pmf_cmaq_dust, paste0("PMF_CMAQ_NLCD_Census_", cmaq_period, "_Dust.gpkg"))
write_sf(pmf_cmaq_traffic, paste0("PMF_CMAQ_NLCD_Census_", cmaq_period, "_Traffic.gpkg"))

#### 4 Model common input prepare ####

###### 4.1 traffic info ######
traffic_vol = read_fst("Traffic_volume/FHWA_Traffic_Volume_2011-20.fst")
setDT(traffic_vol)
traffic_vol_year = subset(traffic_vol, Date < as.Date("2012-01-01"))

roadiness = read_fst("Roadiness/us_roadiness_longlat.fst")
setDT(roadiness)

# convert to sf
traffic_vol_sf <-
  st_as_sf(traffic_vol, 
           coords = c("Longitude", "Latitude"), 
           crs = 4326) # EPSG:4326 (WGS 84) is for longitude/latitude
roadiness_sf <-
  st_as_sf(roadiness, 
           coords = c("Longitude", "Latitude"), 
           crs = 4326) # EPSG:4326 (WGS 84) is for longitude/latitude

###### 4.2 Land Use ######

##### Land cover
landcover_labels <- c(
  "Unclassified" = "Unclassified",
  "11" = "Open Water", 
  "12" = "Perennial Ice/Snow", 
  "21" = "Developed, Open Space", 
  "22" = "Developed, Low Intensity", 
  "23" = "Developed, Medium Intensity", 
  "24" = "Developed, High Intensity", 
  "31" = "Barren Land", 
  "41" = "Deciduous Forest", 
  "42" = "Evergreen Forest", 
  "43" = "Mixed Forest", 
  "52" = "Shrub/Scrub", 
  "71" = "Grassland/Herbaceous", 
  "81" = "Pasture/Hay", 
  "82" = "Cultivated Crops", 
  "90" = "Woody Wetlands", 
  "95" = "Emergent Herbaceous Wetlands"
)

# Convert the named vector into a data.frame
landcover_df <- data.frame(
  NLCD.Land.Cover.Class = names(landcover_labels),
  land_type = landcover_labels,
  stringsAsFactors = FALSE # Keep the values as characters
)
rownames(landcover_df) = 1:nrow(landcover_df)


###### 4.3 PMF CMAQ ######
cmaq_period = "2011-02_2011-08"

# Sulfate
pmf_cmaq_sulfate = read_sf(paste0("PMF_CMAQ_NLCD_Census_", cmaq_period, "_Sulfate.gpkg"))
pmf_cmaq_sulfate = 
  dplyr::select(pmf_cmaq_sulfate, 
                -GEOID, -Source_aftermanual, 
                -car_truck_van, -public_transport, -bike, 
                -walk, -taxi_moto_etc, -work_home, -commute_time) 
names(pmf_cmaq_sulfate)[c(6, 10)]
names(pmf_cmaq_sulfate)[c(6, 10)] = c("PMF_conc", "CMAQ_conc")
length(unique(pmf_cmaq_sulfate$Date))
length(unique(pmf_cmaq_sulfate$geom))

# Soil/Dust 
pmf_cmaq_dust = read_sf(paste0("PMF_CMAQ_NLCD_Census", cmaq_period, "_Dust.gpkg"))
pmf_cmaq_dust = 
  dplyr::select(pmf_cmaq_dust, 
                -GEOID, -Source_aftermanual) 
names(pmf_cmaq_dust)[c(6, 15)]
names(pmf_cmaq_dust)[c(6, 15)] = c("PMF_conc", "CMAQ_conc")
length(unique(pmf_cmaq_dust$Date))
length(unique(pmf_cmaq_dust$geom))

# Traffic
pmf_cmaq_traffic = read_sf(paste0("PMF_CMAQ_NLCD_Census_", cmaq_period, "_Traffic.gpkg"))
pmf_cmaq_traffic = 
  dplyr::select(pmf_cmaq_traffic, 
                -GEOID, -Source_aftermanual) 
names(pmf_cmaq_traffic)[c(6, 15)]
names(pmf_cmaq_traffic)[c(6, 15)] = c("PMF_conc", "CMAQ_conc")
length(unique(pmf_cmaq_traffic$Date))
length(unique(pmf_cmaq_traffic$geom))

###### choose the source to model
##### Sulfate
pmf_cmaq_for_model = pmf_cmaq_sulfate
source.test = "Secondary Sulfate"

##### Soil/Dust
# replace the NAs in commute_time by 0
pmf_cmaq_dust$commute_time[is.na(pmf_cmaq_dust$commute_time)] = 0

pmf_cmaq_for_model = pmf_cmaq_dust
source.test = "Soil/Dust"

##### Traffic
# prepare traffic volume and roadiness data
traffic_vol_sf = st_transform(traffic_vol_sf, crs = st_crs(pmf_cmaq_traffic))
roadiness_sf = st_transform(roadiness_sf, crs = st_crs(pmf_cmaq_traffic))
traffic_vol_sf = subset(traffic_vol_sf, 
                        Date %in% pmf_cmaq_traffic$Date)
summary(traffic_vol_sf$Date %in% pmf_cmaq_traffic$Date)
summary(pmf_cmaq_traffic$Date %in% traffic_vol_sf$Date)

# st_join traffic volume and roadiness data via st_nearest_feature
nearest_index_road <- st_nearest_feature(pmf_cmaq_traffic, roadiness_sf)
pmf_cmaq_traffic_road <- cbind(pmf_cmaq_traffic, roadiness_sf[nearest_index_road, ])
pmf_cmaq_traffic_road$geometry.1 = NULL
summary(pmf_cmaq_traffic_road)
length(unique(pmf_cmaq_traffic_road$Date))
length(unique(pmf_cmaq_traffic_road$geom))

nearest_index_road_vol <- st_nearest_feature(pmf_cmaq_traffic_road, traffic_vol_sf)
pmf_cmaq_traffic_volume_road <- cbind(pmf_cmaq_traffic_road, traffic_vol_sf[nearest_index_road_vol, ])
pmf_cmaq_traffic_volume_road <-
  pmf_cmaq_traffic_volume_road %>%
  filter(Date == Date.1)
summary(pmf_cmaq_traffic_volume_road)
length(unique(pmf_cmaq_traffic_volume_road$Date))
length(unique(pmf_cmaq_traffic_volume_road$geom))

pmf_cmaq_traffic_volume_road$Date.1 = pmf_cmaq_traffic_volume_road$Station_ID = 
  pmf_cmaq_traffic_volume_road$geometry.1 = NULL

summary(pmf_cmaq_traffic_volume_road)
# replace the NAs in commute_time by 0
pmf_cmaq_traffic_volume_road$commute_time[is.na(pmf_cmaq_traffic_volume_road$commute_time)] = 0

pmf_cmaq_for_model = pmf_cmaq_traffic_volume_road
source.test = "Traffic"

######## common data preparation steps
# set geometry name as geometry
st_geometry(pmf_cmaq_for_model) <- "geometry"

# extract coordinates from the sf objects, X & Y
coords = 
  as.data.frame(st_coordinates(pmf_cmaq_for_model)) %>%
  dplyr::select(X, Y)
head(coords); summary(coords)

# drop geometry and combine with coordinates
pmf_cmaq_for_model_coords <-
  pmf_cmaq_for_model %>%
  st_drop_geometry() %>%
  cbind(coords)
head(pmf_cmaq_for_model_coords) 
sapply(pmf_cmaq_for_model_coords, class)

# data for modeling
model_input = 
  dplyr::select(pmf_cmaq_for_model_coords, -year)
# Convert day_of_week to factor (unordered)
model_input$day_of_week <- factor(model_input$day_of_week, ordered = FALSE)
model_input$month <- factor(model_input$month, ordered = FALSE)
model_input$is_holiday <- factor(model_input$is_holiday, ordered = FALSE)

# set NA as Unclassified
model_input$NLCD.Land.Cover.Class[is.na(model_input$NLCD.Land.Cover.Class)] = "Unclassified"

# merge with land use type
model_input =
  merge(model_input, landcover_df, all.x = TRUE)
model_input$NLCD.Land.Cover.Class = NULL
# caret only deal with factor and numeric, not characters
model_input$land_type <- factor(model_input$land_type, ordered = FALSE)

sapply(model_input, class)
dim(model_input)
summary(is.na(model_input$land_type))
summary(is.na(model_input$is_holiday))
class(model_input)

# check model_input to exclude any factor class variable with < 2 levels
summary(model_input)
model_input <- 
  model_input %>%
  dplyr::select_if(~ !(is.factor(.) && length(levels(.)) == 1))
summary(model_input)

write_fst(model_input, paste0("PMF_CMAQ_NLCD_Census_", cmaq_period, "_Sulfate.fst"))
write_fst(model_input, paste0("PMF_CMAQ_NLCD_Census_", cmaq_period, "_Dust.fst"))
write_fst(model_input, paste0("PMF_CMAQ_NLCD_Census_", cmaq_period, "_Traffic.fst"))

######################################################
############### Modeling ###################
######################################################
cmaq_period = "2011-02_2011-08"

# Sulfate
model_input = read_fst(paste0("PMF_CMAQ_NLCD_Census_", cmaq_period, "_Sulfate.fst"))
source.test = "Secondary Sulfate"
summary(model_input); dim(model_input)
mtry_use = 15

# Dust
model_input = read_fst(paste0("PMF_CMAQ_NLCD_Census_", cmaq_period, "_Dust.fst"))
source.test = "Dust"
summary(model_input); dim(model_input)
mtry_use = 26

# Traffic
model_input = read_fst(paste0("PMF_CMAQ_NLCD_Census_", cmaq_period, "_Traffic.fst"))
source.test = "Traffic"
summary(model_input); dim(model_input)

# original data
conc_plot_data <- data.frame(
  Date = model_input$Date,
  PMF_conc = model_input$PMF_conc,
  CMAQ_conc = model_input$CMAQ_conc
) 

summary(is.na(model_input))

#### 5. Random Forest modeling #### 

model.method = "Random Forest"

###### 5.1 Random Forest, cross validation ###### 

# Set up 5-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Fit the Random Forest model
rf_model_cv <- caret::train(
  PMF_conc ~ ., #  + year if needed
  data = dplyr::select(model_input, -Date),
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

summary(rf_model_cv)
print(rf_model_cv)

# Check the hyperparameter setting
caret.rf.mtry = rf_model_cv$bestTune$mtry
caret.rf.ntree = rf_model_cv$finalModel$ntree
summary(rf_model_cv$finalModel)
rf_model_cv$bestTune

print(caret.rf.mtry); print(caret.rf.ntree)

# Predict on the training data
rf_cv_predictions <- predict(rf_model_cv, model_input)

# Evaluate the model performance
rf_cv_performance <- 
  postResample(rf_cv_predictions, model_input$PMF_conc)
print(rf_performance)

# Extract performance metrics
rmse_rf <- round(rf_cv_performance["RMSE"], 3)
rsq_rf <- round(rf_cv_performance["Rsquared"], 2)
mae_rf <- round(rf_cv_performance["MAE"], 3)
rmse_rf; rsq_rf; mae_rf

###### Plot the relative variable importance of each predictor
# Extract variable importance
rf_cv_var_imp <- varImp(rf_model_cv, scale = TRUE)

# Convert variable importance to a data frame
rf_cv_var_imp_df <- rf_cv_var_imp$importance

# Add Variable names as a column
rf_cv_var_imp_df$Variable <- rownames(rf_cv_var_imp_df)
rownames(rf_cv_var_imp_df) <- NULL
head(rf_cv_var_imp_df)
names(rf_cv_var_imp_df) = c("Relative_Influence", "Variable")

# # Sort the data frame in decreasing order of importance
# rf_cv_var_imp_df <- rf_cv_var_imp_df[order(rf_cv_var_imp_df$Overall, decreasing = TRUE), ]

### Predictor performance plot
rf_var_influence_p <-
  ggplot(rf_cv_var_imp_df, 
         aes(x = reorder(Variable, 
                         Relative_Influence), 
             y = Relative_Influence)) +
  geom_bar(stat = "identity",
           fill = "steelblue",
           width = 0.5) +
  coord_flip() +  # Flip coordinates to make a horizontal bar plot
  labs(title = "Variable Performance",
       x = "Predictor",
       y = "Relative Influence (%)") +
  theme_minimal(base_size = 16)
# theme(plot.title = element_text(hjust = 2.5))

###### Plot the absolute variable importance of each predictor
# Create an iml Predictor object for the model
predictor_rf_cv <- 
  Predictor$new(rf_model_cv, 
                data = dplyr::select(model_input, -Date), 
                y = model_input$PMF_conc)

# Compute Shapley values for a specific observation (or loop over multiple observations)
shapley_rf_cv <- 
  Shapley$new(predictor_rf_cv, 
              x.interest = dplyr::select(model_input, -Date)[1,])  # Example for first observation

# View contributions of all variables
# phi, quantifies the contribution of that feature
#      negative phi values indicate that the feature is decreasing the prediction values
# phi.var, the variance of the Shapley value (phi) for the feature
shapley_rf_cv$results

# Sort by absolute value of phi (most influential to least)
shapley_rf_cv_sorted <- 
  shapley_rf_cv$results %>%
  arrange(desc(abs(phi)))

# Plot the results
shapley_varImp_rf_cv <- 
  ggplot(shapley_rf_cv_sorted, 
         aes(x = phi, y = reorder(feature, abs(phi)))) +
  geom_point(color = "blue", size = 3) +  # Shapley values (phi)
  geom_errorbarh(aes(xmin = phi - sqrt(phi.var), xmax = phi + sqrt(phi.var)), 
                 color = "gray", height = 0.2) +  # Error bars for phi.var
  labs(title = "Shapley Values with Variance for Features",
       x = "Shapley Value (phi)",
       y = "Feature") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

### Prediction vs. measurement
conc_plot_rf_cv = conc_plot_data
conc_plot_rf_cv$RF_Prediction = rf_cv_predictions

# calculate the limits based on the range of the data
max_rf <- 
  max(c(conc_plot_data$PMF_conc, 
        conc_plot_data$CMAQ_conc, 
        conc_plot_rf_cv$RF_Prediction), 
      na.rm = TRUE)
min_rf <-
  min(c(conc_plot_data$PMF_conc, 
        conc_plot_data$CMAQ_conc, 
        conc_plot_rf_cv$RF_Prediction), 
      na.rm = TRUE)

pmf_vs_cmaq_rf <-
  ggplot(conc_plot_data, aes(x = PMF_conc, y = CMAQ_conc)) +
  geom_point(color = "darkgreen", alpha = 0.15) +
  coord_equal() + # enforces an equal scaling between the axes
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste0("PMF vs. CMAQ"),
       x = format_variable("PMF conc µg/m3"),
       y = format_variable("CMAQ conc µg/m3")) +
  theme_minimal(base_size = 16) +
  xlim(min_rf, max_rf) +
  ylim(min_rf, max_rf)

pmf_vs_predicted_rf <-
  ggplot(conc_plot_rf_cv, aes(x = PMF_conc, y = RF_Prediction)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  coord_equal() + # enforces an equal scaling between the axes
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste0("PMF vs. ML Predictions"),
       x = format_variable("PMF conc µg/m3"),
       y = format_variable("ML Predictions µg/m3")) +
  theme_minimal(base_size = 16) +
  xlim(min_rf, max_rf) +
  ylim(min_rf, max_rf)


##### time series

# Combine data reshaping and summarization using pivot_longer
rf_cv_daily <- 
  conc_plot_rf_cv %>%
  dplyr::select(Date, PMF_conc, RF_Prediction) %>%
  pivot_longer(
    cols = c(PMF_conc, RF_Prediction),
    names_to = "Variable",
    values_to = "Concentration"
  ) %>%
  group_by(Date, Variable) %>%
  summarize(
    median_Conc = median(Concentration, na.rm = TRUE),
    lower_CI = quantile(Concentration, probs = 0.025, na.rm = TRUE),
    upper_CI = quantile(Concentration, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )

pmf_rf_daily_plot <-
  ggplot(rf_cv_daily, 
         aes(x = Date, y = median_Conc, 
             color = Variable, fill = Variable)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), 
              alpha = 0.2, color = NA) +
  labs(
    title =  paste0("Daily Median with 95% CI"),
    x = "Date",
    y = "Concentration"
  ) +
  theme_minimal(base_size = 16) +
  theme(legend.position = c(0.15, 0.9), 
        legend.background = element_blank(),
        legend.title = element_text(size = 0))

title_text <- paste0(model.method, " for ", 
                     source.test, 
                     ": RMSE, ", rmse_rf,
                     "; MAE, ", mae_rf,
                     "; R-squared, ", rsq_rf)

combined_rf_cv_plot <- 
  rf_var_influence_p + ((pmf_vs_cmaq_rf + pmf_vs_predicted_rf) / pmf_rf_daily_plot) +
  plot_layout(widths = c(2, 5)) +
  plot_annotation(title = title_text,
                  theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
combined_rf_cv_plot

###### 5.2 Random Forest, Holdout Analyses ###### 

site_info = dplyr::select(model_input, X, Y)
site_info = site_info[!duplicated(site_info), ]
dim(site_info)
site_info$Station_ID = 1:nrow(site_info)
site_info$Station_ID = as.factor(site_info$Station_ID)

model_input = merge(model_input, site_info)
head(model_input); dim(model_input)

# Create an empty list to store results from 50 iterations
all_rf_predictions <- list()
rf_rmse_list <- c()
rf_rsq_list <- c()
rf_mae_list <- c()

# Lists to store variable importance from each iteration
rf_rfp_var_imp_list <- list()
shapley_results_list <- list()

# Number of iterations
n_iterations <- 50 # 3

# Loop for 50 iterations
for (i in 1:n_iterations) {
  
  # Split data into 80% train and 20% test based on unique Station_ID
  set.seed(i)  # for reproducibility
  unique_sites <- unique(model_input$Station_ID)
  
  # Randomly select 80% of sites for training
  train_sites <- sample(unique_sites, size = floor(0.8 * length(unique_sites)))
  # length(unique_sites); length(train_sites)
  
  # Create training and test datasets
  train_data <- model_input %>% filter(Station_ID %in% train_sites)
  test_data <- model_input %>% filter(!Station_ID %in% train_sites)
  
  # Set up training input for modeling
  model_input_train <- dplyr::select(train_data, -Station_ID, -Date)
  
  # Set up test input for prediction
  model_input_test <- dplyr::select(test_data, -Station_ID, -Date)
  
  # Fit the Random Forest model on training data
  rf_model_rfp <- randomForest(
    PMF_conc ~ . ,
    data = model_input_train,
    ntree = 1000,  # Number of trees in the forest
    mtry = mtry_use, # with mtry = 15, very high R-square
    importance = TRUE  # To track variable importance
  )
  
  # Store variable importance for this iteration
  rf_rfp_var_imp <- data.frame(Importance = rf_model_rfp$importance[, "IncNodePurity"],
                               Variable = rownames(rf_model_rfp$importance))
  rf_rfp_var_imp$Iteration <- i
  rf_rfp_var_imp_list[[i]] <- rf_rfp_var_imp
  
  # Extract Shapley values for one observation (you can loop over multiple observations)
  predictor_rf_rfp <- iml::Predictor$new(rf_model_rfp, data = model_input_train)
  shapley_rf_rfp <- Shapley$new(predictor_rf_rfp, x.interest = model_input_train[1,])  # First observation
  
  # Store Shapley results
  shapley_results_rfp <- shapley_rf_rfp$results
  shapley_results_rfp$Iteration <- i  # Add iteration info
  shapley_results_list[[i]] <- shapley_results_rfp  # Store results
  
  # Predict on the test data
  rf_test_predictions <- predict(rf_model_rfp, model_input_test)
  
  # Store the predictions and actual values
  all_rf_predictions[[i]] <- data.frame(Station_ID = test_data$Station_ID,
                                        Date = test_data$Date,
                                        PMF_conc = test_data$PMF_conc,
                                        RF_Prediction = rf_test_predictions)
  
  # Evaluate performance metrics for this iteration
  rf_performance <- postResample(rf_test_predictions, model_input_test$PMF_conc)
  rf_rmse_list[i] <- rf_performance["RMSE"]
  rf_rsq_list[i] <- rf_performance["Rsquared"]
  rf_mae_list[i] <- rf_performance["MAE"]
}

# Combine all predictions from 50 iterations
combined_rf_predictions <- bind_rows(all_rf_predictions)

# Calculate the average predictions per site
final_rf_predictions <- combined_rf_predictions %>%
  group_by(Station_ID, Date) %>%
  summarize(Mean_RF_Prediction = mean(RF_Prediction, na.rm = TRUE),
            Actual_PMF = mean(PMF_conc, na.rm = TRUE))

# Calculate overall RMSE, MAE, R-squared using the averaged predictions
final_rf_performance <- 
  postResample(final_rf_predictions$Mean_RF_Prediction, final_rf_predictions$Actual_PMF)

# Output performance metrics
final_rf_rmse <- round(final_rf_performance["RMSE"], 3)
final_rf_rsq <- round(final_rf_performance["Rsquared"], 2)
final_rf_mae <- round(final_rf_performance["MAE"], 3)
final_rf_rmse; final_rf_rsq; final_rf_mae

# Plotting the averaged results

# Plot actual vs predicted for final predictions
rf_predictions_vs_pmf <- 
  ggplot(final_rf_predictions, aes(x = Actual_PMF, y = Mean_RF_Prediction)) +
  geom_point(color = "blue", alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "PMF vs. Averaged RF Predictions",
       x = "Actual PMF Concentration (µg/m3)",
       y = "Mean Predicted PMF Concentration (µg/m3)") +
  theme_minimal(base_size = 16)


##### Relative variable importance
# Combine all variable importance results into one dataframe
combined_var_imp <- bind_rows(rf_rfp_var_imp_list)

# Calculate the mean and standard deviation of variable importance across iterations
final_var_imp_summary <- combined_var_imp %>%
  group_by(Variable) %>%
  summarize(Mean_Importance = mean(Importance),
            SD_Importance = sd(Importance))

relative_var_imp_rf <-
  ggplot(final_var_imp_summary, 
         aes(x = reorder(Variable, Mean_Importance), 
             y = Mean_Importance)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  geom_errorbar(aes(ymin = Mean_Importance - SD_Importance, 
                    ymax = Mean_Importance + SD_Importance), 
                width = 0.2, color = "gray") +
  coord_flip() +  # Flip coordinates to make a horizontal bar plot
  labs(title = "Variable Importance",
       x = "Predictor",
       y = "Mean Importance (IncNodePurity)") +
  theme_minimal(base_size = 16)

######### Shapley quantitative variable importance
# Combine Shapley values from all iterations
combined_shapley_results <- bind_rows(shapley_results_list)

# Summary of Shapley results for all variables
shapley_summary_rf_rfp <- 
  combined_shapley_results %>%
  group_by(feature) %>%
  summarize(Mean_Contribution = mean(phi),
            SD_Contribution = sd(phi),
            .groups = 'drop')

# Plot the average Shapley values (quantitative contribution) with variance
shapley_summary_rf_rfp_imp<-
  ggplot(shapley_summary_rf_rfp, 
         aes(x = reorder(feature, Mean_Contribution), 
             y = Mean_Contribution)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  geom_errorbar(aes(ymin = Mean_Contribution - SD_Contribution, 
                    ymax = Mean_Contribution + SD_Contribution), 
                width = 0.2, color = "gray") +
  coord_flip() +  # Flip coordinates to make a horizontal bar plot
  labs(title = "Shapley Values",
       x = "Predictor",
       y = "Mean Shapley Value (phi)") +
  theme_minimal(base_size = 16)

# the combined figure
overall_title =
  title = paste0(source.test, ": PMF vs. Averaged RF Predictions (50 Holdout Iterations)",
                "\nRMSE:", final_rf_rmse, 
                " , MAE:", final_rf_mae, 
                " , R-squared:", final_rf_rsq)
combined_plot_rf_rfp =
  rf_predictions_vs_pmf + relative_var_imp_rf + shapley_summary_rf_rfp_imp +
  plot_annotation(title = overall_title,
                  theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
combined_plot_rf_rfp

# output the photo
ggsave(paste0("RF holdout_", source.test, "_", cmaq_period, ".pdf"), 
       plot = combined_plot_rf_rfp, 
       width = 14.5, height = 8)
