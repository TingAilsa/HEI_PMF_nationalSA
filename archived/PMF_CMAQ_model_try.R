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
library(torch)
library(ggplot2)
library(patchwork)

# library(stars) # st_rasterize

setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data")
getwd()

# The Keras package in R relies on Python's Keras and TensorFlow as its backend
# It interfaces with Python to perform the computations
# use the reticulate package to specify the Conda environment, for Convention NN
library(reticulate)

# Use the newly created environment
use_condaenv("r-reticulate", required = TRUE)
# Verify Python configuration
py_config()

library(keras) # python relied
library(tensorflow)

# Test TensorFlow (optional)
tf$constant("Hello, TensorFlow!")

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
pmf_source = pmf_source[, .(SiteCode, Date, Latitude, Longitude, Source_aftermanual, Concentration)]
write.csv(pmf_source, 
          "PMF_results/CSN_IMPROVE_source_daily_contribution.csv", 
          row.names=FALSE)

#### 2.2 Generate the date info ####

##### create a date file, and get the year, month, day of week, and is_holiday (whether a day is a US national holiday)
date_use = 
  data.frame(Date = seq(as.Date("2011-01-01"), as.Date("2020-12-31"), by = "day"))
setDT(date_use)
date_use[, `:=`(
  year = year(Date),
  month = month(Date),
  day_of_week = wday(Date, label = TRUE),
  is_holiday = Date %in% as.Date(holidayNYSE(year(Date)))  # US holiday (can adjust for local holidays)
)]

# convert categorical variables to factors
# date_use[, land_use_type := as.factor(land_use_type)]
date_use[, day_of_week := as.factor(day_of_week)]
date_use[, is_holiday := as.factor(is_holiday)]

write.fst(date_use, "Date_DOW_Holiday_2011-20.fst")


#### 3.1 Merge CMAQ & PMF & other predictors, when CMAS is RDS ####

# load datasets to use 
# CMAQ source
cmaq_source_all = readRDS("CMAQ_Sumaiya/CMAQ_extracted_PM_dust_egu_201701-03.rds") 
# PMF source
pmf_source = fread("PMF_results/CSN_IMPROVE_source_daily_contribution.csv") 
# NCLD land use type
ncld_year <- terra::rast("NLCD_landcover/nlcd_2013_land_cover_l48_20210604/nlcd_2013_land_cover_l48_20210604.img")
plot(ncld_year)
print(ncld_year)
summary(ncld_year)
res(ncld_year); crs(ncld_year); nlyr(ncld_year)

date_use = read.fst("Date_DOW_Holiday_2011-20.fst")
sapply(date_use, class)

# Merge date info with source info
pmf_source$Date = as.Date(pmf_source$Date)
pmf_source = base::merge(pmf_source, date_use, all.x = TRUE)

# add coordinate reference system to pmf_source and combine with land use
# pmf_source_crs =
#   st_as_sf(pmf_source, coords = c("Longitude", "Latitude"),
#            crs = st_crs(ncld_year))

# pmf_source$land_use_type = extract(ncld_year, pmf_source_crs)
# head(pmf_source); unique(pmf_source$land_use_type)

# extract single source file
pmf_single_source = 
  subset(pmf_source, 
         Source_aftermanual == "F3-Secondary Sulfate" |
           Source_aftermanual == "F9-Soil/Dust" &
           Date < as.Date("2017-04-01") & Date > as.Date("2016-12-31"))
cmaq_single_source = subset(cmaq_source_all, variable = "PM25_TOT_EGU")

# write_fst(pmf_single_source, "CMAQ_Sumaiya/pmf_single_source_201701-03.fst")
# saveRDS(cmaq_single_source, "CMAQ_Sumaiya/cmaq_single_source201701-03.rds")

pmf_single_source = read_fst("CMAQ_Sumaiya/pmf_single_source_201701-03.fst") # pmf_single_source_201701.fst
setDT(pmf_single_source)
nrow(table(pmf_single_source$Latitude, pmf_single_source$Longitude)) # 263
cmaq_single_source = readRDS("CMAQ_Sumaiya/cmaq_single_source201701-03.rds") # cmaq_single_source201701.rds

# pmf_single_source = 
#   subset(pmf_single_source, Date < as.Date("2017-02-01"))
# cmaq_single_source = 
#   subset(cmaq_single_source, Date < as.Date("2017-02-01"))

# write_fst(pmf_single_source, "CMAQ_Sumaiya/pmf_single_source_201701.fst")
# saveRDS(cmaq_single_source, "CMAQ_Sumaiya/cmaq_single_source201701.rds")

head(pmf_single_source); head(cmaq_single_source)
dim(pmf_single_source); dim(cmaq_single_source)
class(pmf_single_source); class(cmaq_single_source)
crs(cmaq_single_source)

# convert cmaq_single_source to sf
cmaq_single_source_sf <- st_as_sf(cmaq_single_source)

########### Extraction after rasterization
## the rasterization takes forever
# crs(cmaq_single_source_sf)
# # Create a Raster Template with crs
# raster_template <- raster(extent(cmaq_single_source_sf), 
#                           resolution = c(0.1, 0.1)) # 12 km, ~ 0.1 degree
# crs(raster_template) <- st_crs(cmaq_single_source_sf)$proj4string
# 
# # Rasterize the polygon data using the 'value' column
# cmaq_single_source_rst <- rasterize(cmaq_single_source_sf, 
#                                     raster_template, 
#                                     field = "value", 
#                                     fun = mean)  # Choose 'mean', 'max', or other aggregation as needed


# cmaq_single_source_res = rast(vect(cmaq_single_source), res = 0.1)
# cmaq_single_source_rst = st_rasterize(cmaq_single_source)

# convert pmf_single_source to SpatialPointsDataFrame 
# coordinates(pmf_single_source) <- ~Longitude + Latitude
# 
# # Extract & add values from the raster at the point locations from pmf_single_source to pmf_single_source
# extracted_values <- extract(cmaq_single_source_rst, pmf_single_source)
# pmf_single_source$CMAQ_values <- extracted_values

# View the merged data
# head(pmf_single_source)
########### Extraction after rasterization

########### st_join with sf file
# Convert pmf_single_source to sf 
# here, cannot use crs = st_crs(cmaq_single_source_sf), 
#       which will treat long and lat as the projected CRS in CMAQ dataset, 
#       causing the coordinates to appear incorrectly in a much smaller area.
pmf_single_source_sf <- 
  st_as_sf(pmf_single_source, 
           coords = c("Longitude", "Latitude"), 
           crs = 4326) # EPSG:4326 (WGS 84) is for longitude/latitude

# transform pmf_single_source_sf to match the CRS of cmaq_single_source_sf
pmf_single_source_sf <- 
  st_transform(pmf_single_source_sf, crs = st_crs(cmaq_single_source_sf))

ggplot(pmf_single_source,
       aes(x = Longitude, y = Latitude)) +
  geom_point(alpha = 0.6)
ggplot(pmf_single_source_sf) + 
  geom_sf(aes(fill = Concentration))

head(pmf_single_source_sf); head(cmaq_single_source_sf)
dim(pmf_single_source_sf); dim(cmaq_single_source_sf)
class(pmf_single_source_sf); class(cmaq_single_source_sf)
st_crs(pmf_single_source_sf); st_crs(cmaq_single_source_sf)

### patial join based on geometry and Date
# st_intersects is more flexible than st_within, which will exclude points on boundaries
# however, after compare with results from st_within, it turns no points was on the boundary.
# so use st_within, which is more efficient
pmf_cmaq_combine <-
  st_join(pmf_single_source_sf,
          cmaq_single_source_sf,
          join = st_within,
          left = TRUE)
# plot(st_geometry(pmf_single_source_sf), add = TRUE)
dim(pmf_cmaq_combine)

# Filter the join by Date
pmf_cmaq_combine_filter <-
  pmf_cmaq_combine[pmf_cmaq_combine$Date.x == pmf_cmaq_combine$Date.y, ]

pmf_cmaq_combine_filter$Date.y = pmf_cmaq_combine_filter$day = NULL
head(pmf_cmaq_combine_filter); dim(pmf_cmaq_combine_filter)
names(pmf_cmaq_combine_filter)[1] = "Date"
unique(pmf_cmaq_combine_filter$Source_aftermanual)
unique(pmf_cmaq_combine_filter$Date)
unique(pmf_cmaq_combine_filter$variable)
length(unique(pmf_cmaq_combine_filter$geometry))
class(pmf_cmaq_combine_filter)
# saveRDS(pmf_cmaq_combine_filter, "cmaq_pmf_dust_sulfate_source201701-03.rds")
# pmf_cmaq_combine_filter = readRDS("cmaq_pmf_dust_sulfate_source201701-03.rds")

######### Create grid-like data, something can be done later ########
# Define the approximate bounding box for mainland U.S.
us_bbox <- c(xmin = -125, xmax = -66, ymin = 24, ymax = 50)

# Convert the bounding box to an extent object for the raster package
us_extent <- extent(us_bbox["xmin"], us_bbox["xmax"], us_bbox["ymin"], us_bbox["ymax"])

# Create the raster grid with a desired resolution (e.g., 0.1 degrees)
us_grid_raster <- raster(ext = us_extent, resolution = 0.1)

# convert the grid to an sf object
us_grid_sf <- st_as_sf(as(us_grid_raster, "SpatialPolygons"))

# aggregate the point data into the grid
pmf_cmaq_grid_data <- st_join(us_grid_sf, pmf_cmaq_combine_filter)




# transform sf object to match NLCD raster CRS
pmf_cmaq_nlcd <-
  st_transform(pmf_cmaq_combine_filter, crs(ncld_year))
length(unique(pmf_cmaq_nlcd$geometry))

# Extract NCDL values at the locations of the sf object
print(ext(ncld_year))
extracted_ncdl_lut <- terra::extract(ncld_year, pmf_cmaq_nlcd)

# Get land use categories from the raster
land_use_cats <- terra::cats(ncld_year)[[1]]

# Add land use codes to the sf object
pmf_cmaq_nlcd$land_use <- 
  extracted_ncdl_lut[[2]]
unique(pmf_cmaq_nlcd$land_use)

names(pmf_cmaq_nlcd)[8] = "CMAQ_conc"
names(pmf_cmaq_nlcd)[3] = "PMF_conc"
length(unique(pmf_cmaq_nlcd$geometry))

pmf_cmaq_nlcd_median <- 
  pmf_cmaq_nlcd %>%
  group_by(geometry = st_geometry(.), Source_aftermanual) %>%
  summarize(PMF_conc_med = median(PMF_conc),
            CMAQ_conc_med = median(CMAQ_conc),
            .groups = "drop")

ggplot(data = pmf_cmaq_nlcd_median) +
  geom_sf(aes(fill = PMF_conc_med)) +
  scale_fill_viridis_c() +  # or another color scale of your choice
  theme_minimal(base_size = 16) +
  labs(title = "Mapping AFTER NCLD data",
       fill = "PMF_conc_med")


# remove unmatch PMF & CMAQ variables
pmf_cmaq_combine_use = 
  subset(pmf_cmaq_nlcd,
         (Source_aftermanual == "F3-Secondary Sulfate" & variable == "PM25_TOT_EGU") |
           (Source_aftermanual == "F9-Soil/Dust" & variable == "PM25_TOT_DUST"))


# saveRDS(pmf_cmaq_combine_use, "cmaq_pmf_ncld_dust_sulfate_source201701-03.rds")

#### 3.2 Merge CMAQ & PMF & other predictors, when CMAS is RASTER ####

###### 3.2.1 PMF process & merge with date info ######
# read files
pmf_source = fread("PMF_results/CSN_IMPROVE_source_daily_contribution.csv") 
date_use = read.fst("Date_DOW_Holiday_2011-20.fst")
sapply(date_use, class)

# Merge date info with source info
pmf_source$Date = as.Date(pmf_source$Date)
pmf_source = base::merge(pmf_source, date_use, all.x = TRUE)

# Convert pmf_single_source to sf with crs 4326
# here, cannot use crs = st_crs(cmaq_single_source_sf), 
#       which will treat long and lat as the projected CRS in CMAQ dataset, 
#       causing the coordinates to appear incorrectly in a much smaller area.
pmf_source_sf <- 
  st_as_sf(pmf_source, 
           coords = c("Longitude", "Latitude"), 
           crs = 4326) # EPSG:4326 (WGS 84) is for longitude/latitude

# Merge date info with source info
pmf_source_date = base::merge(pmf_source_sf, date_use, all.x = TRUE)

###### 3.2.2 merge PMF with Census related to source ######
# Geometry of each GEOID at the included census tract level
census_tract_geo =
  st_read(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS",
                    "ACS_census_tract_geoid_geometry_4326.gpkg"))
st_geometry(census_tract_geo) <- "geometry"
head(census_tract_geo)

# Census tract level info
census_tract_acs =
  fread(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS",
          "US_Census_ACS_tract_2011-2020.csv"))

# extract ACS Census variables to use for fine-scale modeling
census_acs_use =
  subset(census_tract_acs,
         variable %in%
           c(c("B08006_002", "B08006_008", paste0("B08006_", sprintf("%03d", 14:17))), # commute method
             paste0("B08303_", sprintf("%03d", 2:13)))) # commute time

# convert GEOID to 11 digits character, probably starting with 0
census_acs_use$GEOID <- ifelse(
  nchar(as.character(census_acs_use$GEOID)) < 11,
  sprintf("%011s", as.character(census_acs_use$GEOID)),
  as.character(census_acs_use$GEOID)
)
head(census_tract_acs)
head(census_acs_use)

### only for 2011 data for now!!!
# census_acs_use = subset(census_acs_use, year == 2011)

# merge ACS census with geometry
census_acs_geom = base::merge(census_tract_geo, census_acs_use, all.y = TRUE)
head(census_acs_geom)


# create dataset to list what census variables represent
census_variable = data.frame(
  variable = 
    c(c("B08006_002", "B08006_008", paste0("B08006_", sprintf("%03d", 14:17))), # commute method
      paste0("B08303_", sprintf("%03d", 2:13))),
  Census_group = 
    c(rep("Commute_method", 6), rep("commute_time", 12)),
  Census_sub = 
    c("car_truck_van", "public_transport", "bike", "walk", "taxi_moto_etc", "work_home",
      "min_0-4", "min_5-9", "min_10-14", "min_15-19", "min_20-24", "min_25-29", 
      "min_30-34", "min_35-39", "min_40-44", "min_45-59", "min_60-89", "min_90-more"))

# merge PMF with ACS census, 
crs(pmf_source_date); crs(census_acs_geom) # check if crs match


# pmf_source_date_use = 
#   subset(pmf_source_date, 
#          (grepl("Traffic", Source_aftermanual, fixed = T) |
#          Source_aftermanual == "F3-Secondary Sulfate" |
#            Source_aftermanual == "F9-Soil/Dust") & 
#            Date < as.Date("2011-03-01") & Date > as.Date("2011-01-31"))
# unique(pmf_source_date_use$month); unique(pmf_source_date_use$year)
# 
# # Modify the 'Source_aftermanual' column based on the condition
# pmf_source_date_use <- 
#   pmf_source_date_use %>%
#   mutate(Source_aftermanual = 
#            ifelse(grepl("Traffic", Source_aftermanual, fixed = TRUE), 
#                   "F1-Traffic", 
#                   Source_aftermanual))
# unique(pmf_source_date_use$Source_aftermanual)

pmf_cmaq_combine <- # pmf_census
  st_join(pmf_source_date,
          census_acs_geom,
          join = st_within,
          left = TRUE)

st_write(pmf_census_date, "PMF_ACS_Census_2011-20_aim3_data.geojson", delete_dsn = TRUE)

###### 3.2.3 merge PMF with NLCD Land use ###### 

# NCLD land use type, combining two year data failed after multiple tries, use the year in the middle instead
# ncld_year <- terra::rast("NLCD_landcover/nlcd_2016_land_cover_l48_20210604/nlcd_2016_land_cover_l48_20210604.img")
ncld_year <- terra::rast("NLCD_landcover/nlcd_2011_fact30_landcover_resampled.tif")
names(ncld_year) = "nlcd_Code"
plot(ncld_year)
res(ncld_year); crs(ncld_year); nlyr(ncld_year)

# pmf_census = st_read("PMF_ACS_Census_2011-20_aim3_data.geojson")

## Define the land cover classes based on NLCD classification
# if running unique(ncld_2016_org$`NLCD Land Cover Class`) after the as.factor(ncld_2016_org), then got the numbers on the left side
# The labels for the NLCD dataset are predefined
landcover_labels <- c(
  "0" = "Unclassified",
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
  land_use = names(landcover_labels),
  land_type = landcover_labels,
  stringsAsFactors = FALSE # Keep the values as characters
)

# transform sf object to match NLCD raster CRS
pmf_nlcd <-
  st_transform(pmf_source_date_use, crs(ncld_year))

# Extract NCDL values at the locations of the sf object
print(ext(ncld_year))
extracted_ncdl_lut <- terra::extract(ncld_year, pmf_nlcd)

pmf_nlcd$land_use <- 
  extracted_ncdl_lut[[2]]

pmf_nlcd = merge(pmf_nlcd, landcover_df)
pmf_nlcd$land_use = NULL
head(pmf_nlcd)

###### 3.2.4 merge with CMAQ variables to use ###### 
cmaq_so2 = terra::rast("CMAQ_Sumaiya/SO2_combined_raster.tif")
cmaq_nh3 = terra::rast("CMAQ_Sumaiya/NH3_combined_raster.tif")
cmaq_dust = terra::rast("CMAQ_Sumaiya/PM25_TOT_DUST_combined_raster.tif")
cmaq_egu = terra::rast("CMAQ_Sumaiya/PM25_TOT_EGU_combined_raster.tif")
cmaq_nrd = terra::rast("CMAQ_Sumaiya/PM25_TOT_NRD_combined_raster.tif")
cmaq_ord = terra::rast("CMAQ_Sumaiya/PM25_TOT_ONR_combined_raster.tif")

plot(cmaq_ord)

# Stack the rasters
cmaq_stack <- c(cmaq_so2, cmaq_nh3, cmaq_dust, cmaq_egu, cmaq_nrd, cmaq_ord)

# Reproject pmf_nlcd to match the CRS of the raster stack
pmf_nlcd_reprojected <- st_transform(pmf_nlcd, crs(cmaq_stack))

# Extract raster values to points, ensuring CRS alignment
cmaq_values <- extract(cmaq_stack, vect(pmf_nlcd_reprojected))

# Combine the extracted values with the original pmf_nlcd data
pmf_nlcd_with_cmaq <- 
  st_as_sf(cbind(st_drop_geometry(pmf_nlcd_reprojected), cmaq_values), 
           geometry = st_geometry(pmf_nlcd_reprojected))






pmf_single_source_sf <- 
  st_transform(pmf_single_source_sf, crs = st_crs(cmaq_single_source_sf))

pmf_cmaq_combine <-
  st_join(pmf_single_source_sf,
          cmaq_single_source_sf,
          join = st_within,
          left = TRUE)
# plot(st_geometry(pmf_single_source_sf), add = TRUE)
dim(pmf_cmaq_combine)

# Filter the join by Date
pmf_cmaq_combine_filter <-
  pmf_cmaq_combine[pmf_cmaq_combine$Date.x == pmf_cmaq_combine$Date.y, ]

pmf_cmaq_combine_filter$Date.y = pmf_cmaq_combine_filter$day = NULL




#### 4 Gradient Boosting Machines & Random Forest ####

pmf_cmaq_combine_use = readRDS("cmaq_pmf_ncld_dust_sulfate_source201701-03.rds")

source.test = "F3-Secondary Sulfate"
source.test = "F9-Soil/Dust"

pmf_cmaq_for_model = subset(pmf_cmaq_combine_use,
                            Source_aftermanual == source.test)
head(pmf_cmaq_for_model)
class(pmf_cmaq_for_model)

# extract coordinates from the sf objects, X & Y
coords = as.data.frame(st_coordinates(pmf_cmaq_for_model))

# # Transform coordinates to WGS84 & extract longitude and latitude
# pmf_cmaq_for_model_wgs84 <- st_transform(pmf_cmaq_for_model, crs = 4326)
# pmf_cmaq_for_model_wgs84 <- 
#   pmf_cmaq_for_model_wgs84 %>%
#   mutate(
#     Longitude = st_coordinates(.)[,1],
#     Latitude = st_coordinates(.)[,2]
#   ) 

# drop geometry and combine with coordinates
pmf_cmaq_for_model_coords <-
  pmf_cmaq_for_model %>%
  st_drop_geometry() %>%
  cbind(coords)
head(pmf_cmaq_for_model_coords); sapply(pmf_cmaq_for_model_coords, class)

# # drop geometry and combine with coordinates
# pmf_cmaq_for_model_coords <- 
#   pmf_cmaq_for_model_wgs84 %>%
#   st_drop_geometry()

# one_site = 
#   subset(pmf_cmaq_for_model, 
#          geometry = unique(pmf_cmaq_for_model$geometry)[10])
# head(one_site)
# 
# ggplot(one_site, aes(x = Date, y = CMAQ_conc)) +
#   geom_point()

# data for modeling
model_input = 
  dplyr::select(pmf_cmaq_for_model_coords, 
                PMF_conc, CMAQ_conc, month, day_of_week, is_holiday, land_use, X, Y) # year, 
# Convert day_of_week to factor (unordered)
model_input$day_of_week <- factor(model_input$day_of_week, ordered = FALSE)
model_input$month <- factor(model_input$month, ordered = FALSE)
sapply(model_input, class)

# Set up 5-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)

# 
conc_plot_data <- data.frame(
  Date = pmf_cmaq_for_model_coords$Date,
  PMF_conc = pmf_cmaq_for_model_coords$PMF_conc,
  CMAQ_conc = pmf_cmaq_for_model_coords$CMAQ_conc
)

###### 4.1 GBM modeling ###### 
model.method = "Gradient Boosting Machines"

# Define the hyperparameter grid 
gbm_grid <- 
  expand.grid(
  n.trees = seq(100, 1000, by = 100),
  interaction.depth = c(3, 5, 7),
  shrinkage = c(0.01, 0.1),
  n.minobsinnode = c(10, 20)
)

# Fit the GBM model
gbm_model <- caret::train(
  PMF_conc ~ 
    CMAQ_conc + month + day_of_week + is_holiday +X + Y + land_use, #   ,  + year
  data = model_input,
  method = "gbm",
  trControl = train_control,
  # tuneGrid = gbm_grid,
  verbose = FALSE
)

# View model results
summary(gbm_model)

# Predict on the training data
gbm_predictions <- predict(gbm_model, model_input)

# Evaluate the model performance
gbm_performance <- 
  postResample(gbm_predictions, model_input$PMF_conc)
print(gbm_performance)

rmse_gbm <- round(gbm_performance["RMSE"], 3)
rsq_gbm <- round(gbm_performance["Rsquared"], 2)
mae_gbm <- round(gbm_performance["MAE"], 3)
rmse_gbm; rsq_gbm; mae_gbm

###### plot the relative influence of each predictor
# extract variable importance
gbm_var_imp <- as.data.frame(summary(gbm_model$finalModel, plotit = FALSE))
colnames(gbm_var_imp) <- c("Variable", "Relative_Influence")
rownames(gbm_var_imp) <- NULL

gbm_var_imp <- 
  gbm_var_imp[order(gbm_var_imp$Relative_Influence, decreasing = TRUE), ]

### Predictor performance plot
gbm_var_influence_p <-
  ggplot(gbm_var_imp, 
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

### Prediction vs. measurement
conc_plot_gbm = conc_plot_data
conc_plot_gbm$GBM_Prediction = gbm_predictions

# calculate the limits based on the range of the data
max_gbm <- 
  max(c(conc_plot_data$PMF_conc, 
        conc_plot_data$CMAQ_conc, 
        conc_plot_gbm$GBM_Prediction), 
      na.rm = TRUE)
min_gbm <-
  min(c(conc_plot_data$PMF_conc, 
        conc_plot_data$CMAQ_conc, 
        conc_plot_gbm$GBM_Prediction), 
      na.rm = TRUE)

pmf_vs_cmaq_gbm <-
  ggplot(conc_plot_data, aes(x = PMF_conc, y = CMAQ_conc)) +
  geom_point(color = "darkgreen", alpha = 0.15) +
  coord_equal() + # enforces an equal scaling between the axes
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste0("PMF vs. CMAQ"),
       x = format_variable("PMF conc µg/m3"),
       y = format_variable("CMAQ conc µg/m3")) +
  theme_minimal(base_size = 16) +
  xlim(min_gbm, max_gbm) +
  ylim(min_gbm, max_gbm)


pmf_vs_predicted_gbm <-
  ggplot(conc_plot_gbm, aes(x = PMF_conc, y = GBM_Prediction)) +
  geom_point(color = "darkgreen", alpha = 0.25) +
  coord_equal() + # enforces an equal scaling between the axes
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste0("PMF vs. ML Predictions"),
       x = format_variable("PMF conc µg/m3"),
       y = format_variable("ML Predictions µg/m3")) +
  theme_minimal(base_size = 16) +
  xlim(min_gbm, max_gbm) +
  ylim(min_gbm, max_gbm)


##### time series

# Combine data reshaping and summarization using pivot_longer
gbm_daily <- 
  conc_plot_gbm %>%
  dplyr::select(Date, PMF_conc, GBM_Prediction) %>%
  pivot_longer(
    cols = c(PMF_conc, GBM_Prediction),
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

pmf_gbm_daily_plot <-
  ggplot(gbm_daily, 
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

# pmf_gbm_daily_plot

title_text <- paste0(model.method, " for ", 
                     source.test, 
                     ": RMSE, ", rmse_gbm,
                     "; MAE, ", mae_gbm,
                     "; R-squared, ", rsq_gbm)

combined_gbm_plot <- 
  gbm_var_influence_p + ((pmf_vs_cmaq_gbm + pmf_vs_predicted_gbm) / pmf_gbm_daily_plot) +
  plot_layout(widths = c(2, 5)) +
  plot_annotation(title = title_text,
                  theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
combined_gbm_plot

###### 4.2 Random Forest modeling ###### 

model.method = "Random Forest"

# Fit the Random Forest model
rf_model <- caret::train(
  PMF_conc ~ 
    CMAQ_conc + month + day_of_week + is_holiday + X + Y + land_use, #  + year if needed
  data = model_input,
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

summary(rf_model)
print(rf_model)

# Predict on the training data
rf_predictions <- predict(rf_model, model_input)

# Evaluate the model performance
rf_performance <- 
  postResample(rf_predictions, model_input$PMF_conc)
print(rf_performance)

# Extract performance metrics
rmse_rf <- round(rf_performance["RMSE"], 3)
rsq_rf <- round(rf_performance["Rsquared"], 2)
mae_rf <- round(rf_performance["MAE"], 3)
rmse_rf; rsq_rf; mae_rf

###### Plot the variable importance of each predictor
# Extract variable importance
rf_var_imp <- varImp(rf_model, scale = TRUE)

# Convert variable importance to a data frame
rf_var_imp_df <- rf_var_imp$importance

# Add Variable names as a column
rf_var_imp_df$Variable <- rownames(rf_var_imp_df)
rownames(rf_var_imp_df) <- NULL
head(rf_var_imp_df)
names(rf_var_imp_df) = c("Relative_Influence", "Variable")

# # Sort the data frame in decreasing order of importance
# rf_var_imp_df <- rf_var_imp_df[order(rf_var_imp_df$Overall, decreasing = TRUE), ]

### Predictor performance plot
rf_var_influence_p <-
  ggplot(rf_var_imp_df, 
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

### Prediction vs. measurement
conc_plot_rf = conc_plot_data
conc_plot_rf$RF_Prediction = rf_predictions

# calculate the limits based on the range of the data
max_rf <- 
  max(c(conc_plot_data$PMF_conc, 
        conc_plot_data$CMAQ_conc, 
        conc_plot_rf$RF_Prediction), 
      na.rm = TRUE)
min_rf <-
  min(c(conc_plot_data$PMF_conc, 
        conc_plot_data$CMAQ_conc, 
        conc_plot_rf$RF_Prediction), 
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
  ggplot(conc_plot_rf, aes(x = PMF_conc, y = RF_Prediction)) +
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
rf_daily <- 
  conc_plot_rf %>%
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
  ggplot(rf_daily, 
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

combined_rf_plot <- 
  rf_var_influence_p + ((pmf_vs_cmaq_rf + pmf_vs_predicted_rf) / pmf_rf_daily_plot) +
  plot_layout(widths = c(2, 5)) +
  plot_annotation(title = title_text,
                  theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
combined_rf_plot


#### 5. convention Neural Network ####

pmf_cmaq_combine_use = readRDS("cmaq_pmf_ncld_dust_sulfate_source201701-03.rds")

# extract coordinates from the sf objects
coords = as.data.frame(st_coordinates(pmf_cmaq_combine_use))

# drop geometry and combine with coordinates
pmf_cmaq_coords <- 
  pmf_cmaq_combine_use %>%
  st_drop_geometry() %>%
  cbind(coords)

# Convert categorical variables to factors
pmf_cmaq_coords$month <- as.factor(pmf_cmaq_coords$month)
pmf_cmaq_coords$day_of_week <- as.factor(pmf_cmaq_coords$day_of_week)
pmf_cmaq_coords$is_holiday <- as.factor(pmf_cmaq_coords$is_holiday)
pmf_cmaq_coords$land_use <- as.factor(pmf_cmaq_coords$land_use)
pmf_cmaq_coords$Source_aftermanual <- as.factor(pmf_cmaq_coords$Source_aftermanual)
sapply(pmf_cmaq_coords, class)
length(unique(pmf_cmaq_coords$Date))
nrow(table(pmf_cmaq_coords$X, pmf_cmaq_coords$Y))

# Use one-hot encoding for categorical variables
fnn_dummy_vars <- 
  dummyVars(" ~ month + day_of_week + is_holiday + land_use + Source_aftermanual",
            data = pmf_cmaq_coords)
fnn_categorical_var <- 
  data.frame(predict(fnn_dummy_vars, newdata = pmf_cmaq_coords))
dim(fnn_categorical_var)

# Replace those <=0 in concentration by 0.0000001
fnn_conc_var <-
  pmf_cmaq_coords %>% 
  dplyr::select(PMF_conc, CMAQ_conc)
fnn_conc_var <- 
  lapply(fnn_conc_var, 
         function(x) {
           x[x <= 0] <- 0.000001
           return(x)
         })
fnn_conc_var = as.data.frame(fnn_conc_var)
summary(fnn_conc_var)

# combine all numeric and conduct normalization
fnn_numeric_var <-
  cbind(  
    pmf_cmaq_coords %>% 
      dplyr::select(X, Y),
    fnn_conc_var)
pre_proc_values <- preProcess(fnn_numeric_var, method = c("center", "scale"))
fnn_numeric_var <- predict(pre_proc_values, fnn_numeric_var)

# Combine numerical and categorical data
fnn_input <- cbind(fnn_numeric_var, fnn_categorical_var)
dim(fnn_input)

# Split data into training and testing sets
set.seed(123)
fnn_train_index <- 
  createDataPartition(fnn_input$PMF_conc, p = 0.8, list = FALSE)
fnn_train_data <- fnn_input[fnn_train_index, ]
fnn_test_data <- fnn_input[-fnn_train_index, ]

# Prepare data for Keras
fnn_train_x <- 
  as.matrix(fnn_train_data %>% 
              dplyr::select(-PMF_conc))
fnn_train_y <- fnn_train_data$PMF_conc

fnn_test_x <- 
  as.matrix(fnn_test_data %>% 
              dplyr::select(-PMF_conc))
fnn_test_y <- fnn_test_data$PMF_conc

# Install torch's dependencies 
# install_torch() # installs the necessary backend dependencies required by the torch package, including LibTorch

# Convert to torch tensors
fnn_train_x_tensor <- torch_tensor(fnn_train_x, dtype = torch_float())
fnn_train_y_tensor <- torch_tensor(fnn_train_y, dtype = torch_float())

fnn_test_x_tensor <- torch_tensor(fnn_test_x, dtype = torch_float())
fnn_test_y_tensor <- torch_tensor(fnn_test_y, dtype = torch_float())

# Define a dataset
fnn_train_dataset <- tensor_dataset(fnn_train_x_tensor, fnn_train_y_tensor)
fnn_test_dataset <- tensor_dataset(fnn_test_x_tensor, fnn_test_y_tensor)

# Create dataloaders
fnn_train_dataloader <- dataloader(fnn_train_dataset, batch_size = 32, shuffle = TRUE)
fnn_test_dataloader <- dataloader(fnn_test_dataset, batch_size = 32, shuffle = FALSE)

# Define a Feedforward Neural Network model, CNN is based on grid
ffn_model <- 
  nn_module( # create a custom neural network model
  "FFN_Model",
  
  initialize = function() { # define the layers of the neural network
    
    # defines a fully connected (linear) layer, the dense layer
    self$fc1 <- 
      nn_linear( 
        in_features = ncol(fnn_train_x), 
        out_features = 128) #  the number of neurons in this layer
    # ReLU (Rectified Linear Unit) activation function, introduces non-linearity into the model
    self$relu1 <- nn_relu() 
    # Dropout is a regularization technique used to prevent overfitting, 0.3 is 30%
    self$dropout1 <- nn_dropout(p = 0.3) 

    self$fc2 <- 
      nn_linear(in_features = 128, out_features = 64)
    self$relu2 <- nn_relu()
    self$dropout2 <- nn_dropout(p = 0.3)
    
    self$fc3 <- 
      nn_linear(in_features = 64, out_features = 32)
    self$relu3 <- nn_relu()
    
    self$fc4 <- nn_linear(in_features = 32, out_features = 1)
  },
  
  # apply the connected layers, ReLU activation, and dropout
  forward = function(x) {
    x <- self$fc1(x)
    x <- self$relu1(x)
    x <- self$dropout1(x)
    x <- self$fc2(x)
    x <- self$relu2(x)
    x <- self$dropout2(x)
    x <- self$fc3(x)
    x <- self$relu3(x)
    x <- self$fc4(x)
    x
  }
)

# Initialize the model
fnn_model_apply <- ffn_model()

# Define loss and optimizer
criterion <- nn_mse_loss()
optimizer <- optim_adam(fnn_model_apply$parameters, lr = 0.001)

# Training loop
epochs <- 50

for (epoch in 1:epochs) {
  fnn_model_apply$train()
  total_loss <- 0
  
  coro::loop(for (batch in fnn_train_dataloader) {
    optimizer$zero_grad()
    
    inputs <- batch[[1]]$unsqueeze(2)  # Add channel dimension
    targets <- batch[[2]]
    
    outputs <- fnn_model_apply(inputs)
    loss <- criterion(outputs, targets)
    
    loss$backward()
    optimizer$step()
    
    total_loss <- total_loss + loss$item()
  })
  
  # Print epoch loss
  cat(sprintf("Epoch %d/%d - Loss: %.4f\n", epoch, epochs, total_loss / length(fnn_train_dataloader)))
}

# Evaluation
fnn_model_apply$eval()
with_no_grad({
  predictions <- fnn_model_apply(fnn_test_x_tensor$unsqueeze(2))$squeeze()
})

# Convert predictions and actuals to R vectors
fnn_predictions <- as.numeric(predictions$cpu())
fnn_actuals <- as.numeric(fnn_test_y_tensor$cpu())

# Calculate performance metrics
fnn_rmse <- sqrt(mean((fnn_predictions - fnn_actuals)^2))
fnn_r_squared <- 1 - sum((fnn_predictions - fnn_actuals)^2) / sum((fnn_actuals - mean(fnn_actuals))^2)

cat(sprintf("Test RMSE: %.4f\n", fnn_rmse))
cat(sprintf("Test R²: %.4f\n", fnn_r_squared))

# Plot Actual vs Predicted
fnn_results <- data.frame(
  FNN_Actual = fnn_actuals,
  FNN_Predicted = fnn_predictions
)
dim(fnn_results); summary(fnn_results)

ggplot(fnn_results, aes(x = FNN_Actual, y = FNN_Predicted)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "PMF vs FNN predicted",
       x = "PMF_conc",
       y = "FNN Prediction") +
  theme_minimal(base_size = 16)

#### SHAP
library(shapper)
# library(SHAPforxgboost) #shap.prep
library(fastshap)

# Define the prediction function compatible with fastshap
# include the ... parameter, ensures that any additional arguments passed by fastshap::explain are gracefully ignored.
predict_function <- function(model, newdata, ...) {
  # Convert newdata to a Torch tensor with the correct dtype and move it to the device
  newdata_tensor <- 
    torch_tensor(as.matrix(newdata), 
                 dtype = torch_float()) # $to(device)
  
  # Set the model to evaluation mode
  model$eval()
  
  # Make predictions without tracking gradients
  with_no_grad({
    predictions <- model(newdata_tensor)$squeeze()
  })
  
  # Convert predictions to a numeric vector
  return(as.numeric(predictions$cpu()))
}

# ensure training and test are correctely formatted
str(fnn_train_x)
str(fnn_test_x)
class(fnn_train_x); class(fnn_test_x)

# Create a sample of test data
sample_test <- fnn_test_x[1:100, ]

# Define device
device <- if (cuda_is_available()) torch_device("cuda") else torch_device("cpu")

# test the prediction function
test_sample <- fnn_test_x[50, , drop = FALSE]
prediction <- predict_function(fnn_model_apply, test_sample)
print(prediction)

# Calculate SHAP values based on explain{fastshap}
shap_values <- 
  fastshap::explain(
  object = fnn_model_apply,      
  X = sample_test,               
  pred_wrapper = predict_function, # Prediction function
  nsim = 100,                    # Number of Monte Carlo samples
  parallel = FALSE               
)

shap.plot.summary(as.data.frame(shap_values))

