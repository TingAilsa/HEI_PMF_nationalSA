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
# library(randomForest)
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

# library(stars) # st_rasterize

setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/")
getwd()

#### 0. functions to use ####
###### convert day or year'YYYYDDD' format to date YYYY-MM-DD ###### 
day_of_year_toDate <- function(day_of_year) {
  year <- as.numeric(substr(day_of_year, 1, 4))  # Extract the year
  day <- as.numeric(substr(day_of_year, 5, 7))   # Extract the day of the year
  
  # Convert to date using the year and day of year
  dates = as.Date(day - 1, origin = paste0(year, "-01-01"))  # Subtract 1 since day 001 is January 1st
  
  return(dates)
}

###### Function to get NetCDF grid info and create CRS ###### 
get_nc_grid_create_crs <- function(nc_file) {
  # Open the .nc file
  nc_cmaq_file <- nc_open(nc_file)
  
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
  
  # Describe grids from grid description
  #define lat & lon in meters based on origin and cell size
  lon <- seq( from = xorig, by = xcell, length.out = ncols)
  lat <- seq( from = yorig, by = ycell, length.out = nrows)
  # min(lon); max(lon); min(lat); max(lat); length(lon); length(lat)
  
  # Close the .nc file after extracting information
  nc_close(nc_cmaq_file)
  
  # Return a list of grid info
  return(list(lon = lon, lat = lat, p4s = p4s))
}

###### Daily, extract selected variable from .nc file, add crs and date and return the corresponding raster brick ###### 
daily_cmaq_var_add_date <- 
  function(nc_file, cmaq_var, lon, lat, p4s) {
    
    # Open the .nc file
    nc_cmaq_file <- nc_open(nc_file)
    
    # Extract the variable from the NetCDF file (dimensions: 442, 265, 30)
    pollu_var_array <- ncvar_get(nc_cmaq_file, cmaq_var)
    
    # get the number of days in the file and the start day (day of the year)
    n_time <- dim(ncvar_get(nc_cmaq_file, "TFLAG"))[3]
    start_day_intg <- ncatt_get(nc_cmaq_file, varid = 0)$SDATE
    
    # convert the start day to date, create a sequence of all dates
    start_day = day_of_year_toDate(start_day_intg)
    day_seq <- seq.Date(start_day, by = "day", length.out = n_time)
    
    # Convert the 3D array to a raster brick (30 layers representing 30 days)
    pollu_var_brick <- brick(pollu_var_array, 
                             xmn = min(lon), xmx = max(lon), 
                             ymn = min(lat), ymx = max(lat), 
                             crs = p4s)
    
    # Only use the day information to rename the layers
    names(pollu_var_brick) <- paste0("Date_", day_seq)
    
    # Close the .nc file after processing
    nc_close(nc_cmaq_file)
    
    # Return the raster brick
    return(pollu_var_brick)
  }

###### Hourly, extract selected variable from .nc file, add crs and date and return the corresponding raster brick ###### 
hourly_daily_cmaq_var_add_date <-
  function(nc_file, cmaq_var, lon, lat, p4s) {
    
    # Open the .nc file
    nc_cmaq_file <- nc_open(nc_file)
    
    # Extract the variable from the NetCDF file (dimensions: 442, 265, 24*nday)
    pollu_var_array <- ncvar_get(nc_cmaq_file, cmaq_var)
    
    # get the number of days in the file and the start day (day of the year)
    n_time <- dim(ncvar_get(nc_cmaq_file, "TFLAG"))[3]
    hours_per_day <- 24
    nday <- n_time / hours_per_day
    start_day_intg <- ncatt_get(nc_cmaq_file, varid = 0)$SDATE
    
    # convert the start day to date, create a sequence of all dates
    start_day = day_of_year_toDate(start_day_intg)
    day_seq <- seq.Date(start_day, by = "day", length.out = nday)
    
    # Convert the 3D array to a raster brick (24*nday layers representing 24*nday hours)
    pollu_var_brick <- brick(pollu_var_array, 
                             xmn = min(lon), xmx = max(lon), 
                             ymn = min(lat), ymx = max(lat), 
                             crs = p4s)
    
    # Aggregate the hourly data to daily data by averaging each 24-hour block
    pollu_var_daily_brick <- 
      stackApply(pollu_var_brick, 
                 indices = rep(1:nday, each = hours_per_day), 
                 fun = mean)
    
    # Only use the day information to rename the layers
    names(pollu_var_daily_brick) <- paste0("Date_", day_seq)
    
    # Close the .nc file after processing
    nc_close(nc_cmaq_file)
    
    # Return the raster brick
    return(pollu_var_daily_brick)
  }

#### 1. convert Daily data ####

# Define the list of all .nc files in the getwd()
# meteorology
nc_files <- list.files(pattern = "hr2day_v54_gcc_CMAQ_ISAM_20\\d{4}.nc")

# nc_files <- c("hr2day_SA_v54_gcc_CMAQ_ISAM_201102.nc")
# nc_files <- ("hr2day_SA_v54_gcc_CMAQ_ISAM_201106.nc")
nc_files <- c("hr2day_v54_gcc_CMAQ_ISAM_201102.nc")

# Define the cmaq_var_use_list based on variables of interest
nc_cmaq_month_check = nc_open(nc_files[1])
cmaq_var_names <- names(nc_cmaq_month_check$var) 
print(cmaq_var_names)

cmaq_var_use_list = cmaq_var_names[c(2:5)] # air pollutant
print(cmaq_var_use_list)

# Initialize a list to store combined raster bricks for each variable
meteo_combined_rasters <- vector("list", length(cmaq_var_use_list))
names(meteo_combined_rasters) <- cmaq_var_use_list

# Process each .nc file
for (nc_file in nc_files) {
  # Get grid information and CRS
  grid_info <- get_nc_grid_create_crs(nc_file)
  
  # Process each variable
  for (i in seq_along(cmaq_var_use_list)) {
    cmaq_var <- cmaq_var_use_list[i]
    
    # Process the variable to create the raster brick
    var_raster <- 
      daily_cmaq_var_add_date(
        nc_file, 
        cmaq_var, 
        grid_info$lon, grid_info$lat, grid_info$p4s)
    
    # Stack or combine the raster bricks across files
    if (is.null(meteo_combined_rasters[[cmaq_var]])) {
      meteo_combined_rasters[[cmaq_var]] <- var_raster
    } else {
      meteo_combined_rasters[[cmaq_var]] <- 
        stack(meteo_combined_rasters[[cmaq_var]], var_raster)
    }
  }
}

# # Save each variable's raster stack to a GeoTIFF file
# for (cmaq_var in names(meteo_combined_rasters)) {
#   
#   # extract date and layer names
#   layer_names <- names(meteo_combined_rasters[[cmaq_var]])
#   date <- gsub("Date_(\\d{4}\\.\\d{2})\\.\\d{2}", "\\1", layer_names[1])
#   
#   # Define the output filename
#   output_file <- paste0(cmaq_var, "_cmaq_", date, ".tif")
#   
#   # Save the raster stack to a GeoTIFF file
#   writeRaster(meteo_combined_rasters[[cmaq_var]], 
#               filename = output_file, 
#               format = "GTiff", 
#               overwrite = TRUE)
# }

# Save each variable's raster stack to a .fst file
for (cmaq_var in names(meteo_combined_rasters)) {
  
  # extract date and layer names
  layer_names <- names(meteo_combined_rasters[[cmaq_var]])
  start_date <- gsub("Date_(\\d{4}\\.\\d{2})\\.\\d{2}", "\\1", layer_names[1])
  dates <- as.Date(gsub("Date_", "", layer_names), format = "%Y.%m.%d")
  
  # Extract spatial information and values
  cmaq_dt = 
    as.data.frame(meteo_combined_rasters[[cmaq_var]], # can only as.data.frame and the set to DT
                  xy = TRUE, na.rm = TRUE) # xy = TRUE adds x, y coordinates
  setDT(cmaq_dt)
  
  # Reshape data from wide to long format (each variable in a single column)
  cmaq_var_long <- 
    melt(cmaq_dt, id.vars = c("x", "y"), 
         variable.name = "Layer", value.name = "cmaq_values")
  
  # Add Date and CMAQ variable columns
  cmaq_var_long[, Date := dates[match(Layer, layer_names)]]
  cmaq_var_long[, cmaq_variable := cmaq_var]
  
  # Remove unnecessary columns
  cmaq_var_long[, Layer := NULL]
  
  # Define the output filename
  output_file <- paste0(cmaq_var, "_cmaq_", start_date, ".rds")
  
  # Save the raster stack to a .rds file
  saveRDS(cmaq_var_long, file = output_file)
}

#### 2. convert Hourly data ####
# combined PM sources
nc_files <- list.files(pattern = "COMBINE_ACONC_v54_gcc_CMAQ_ISAM_20\\d{4}.nc")


nc_files <- c("COMBINE_SA_ACONC_v54_gcc_CMAQ_ISAM_201102.nc")

# Define the cmaq_var_use_list based on variables of interest
nc_cmaq_month_check = nc_open(nc_files[1])
cmaq_var_names <- names(nc_cmaq_month_check$var) 
print(cmaq_var_names)

# cmaq_var_use_list = cmaq_var_names[c(2:5, 19)] # air pollutant
cmaq_var_use_list = cmaq_var_names[c(22:24, 29:30)] # PM sources
print(cmaq_var_use_list)

# Initialize a list to store combined raster bricks for each variable
source_combined_rasters <- vector("list", length(cmaq_var_use_list))
names(source_combined_rasters) <- cmaq_var_use_list

# Process each .nc file
for (nc_file in nc_files) {
  # Get grid information and CRS
  grid_info <- get_nc_grid_create_crs(nc_file)
  
  # Process each variable
  for (i in seq_along(cmaq_var_use_list)) {
    cmaq_var <- cmaq_var_use_list[i]
    
    # Process the variable to create the raster brick
    var_raster <- 
      hourly_daily_cmaq_var_add_date(
        nc_file, 
        cmaq_var, 
        grid_info$lon, grid_info$lat, grid_info$p4s)
    
    # Stack or combine the raster bricks across files
    if (is.null(source_combined_rasters[[cmaq_var]])) {
      source_combined_rasters[[cmaq_var]] <- var_raster
    } else {
      source_combined_rasters[[cmaq_var]] <- 
        stack(source_combined_rasters[[cmaq_var]], var_raster)
    }
  }
}

# # Save each variable's raster stack to a GeoTIFF file
# for (cmaq_var in names(source_combined_rasters)) {
#   
#   # extract date and layer names
#   layer_names <- names(source_combined_rasters[[cmaq_var]])
#   date <- gsub("Date_(\\d{4}\\.\\d{2})\\.\\d{2}", "\\1", layer_names[1])
#   
#   # Define the output filename
#   output_file <- paste0(cmaq_var, "_cmaq_", date, ".tif")
#   
#   # Save the raster stack to a GeoTIFF file
#   writeRaster(source_combined_rasters[[cmaq_var]], 
#               filename = output_file, 
#               format = "GTiff", 
#               overwrite = TRUE)
#   
#   # After writing, save the layer names in a separate file for future use
#   write.csv(layer_names, paste0("cmaq_layer_names_", date, ".csv"), row.names = FALSE)
# }


# Save each variable's raster stack to a .fst file
for (cmaq_var in names(source_combined_rasters)) {
  
  # extract date and layer names
  layer_names <- names(source_combined_rasters[[cmaq_var]])
  start_date <- gsub("Date_(\\d{4}\\.\\d{2})\\.\\d{2}", "\\1", layer_names[1])
  dates <- as.Date(gsub("Date_", "", layer_names), format = "%Y.%m.%d")
  
  # Extract spatial information and values
  cmaq_dt = 
    as.data.frame(source_combined_rasters[[cmaq_var]], # can only as.data.frame and the set to DT
                  xy = TRUE, na.rm = TRUE) # xy = TRUE adds x, y coordinates
  setDT(cmaq_dt)
  
  # Reshape data from wide to long format (each variable in a single column)
  cmaq_var_long <- 
    melt(cmaq_dt, id.vars = c("x", "y"), 
         variable.name = "Layer", value.name = "cmaq_values")
  
  # Add Date and CMAQ variable columns
  cmaq_var_long[, Date := dates[match(Layer, layer_names)]]
  cmaq_var_long[, cmaq_variable := cmaq_var]
  
  # Remove unnecessary columns
  cmaq_var_long[, Layer := NULL]
  
  # Define the output filename
  # compared with fst, rds use less storage space, and keep crs
  output_file <- paste0(cmaq_var, "_cmaq_", start_date, ".rds")
  
  # Save the raster stack to a .rds file
  saveRDS(cmaq_var_long, output_file)
}


#### 3. prepare other data #### 

###### 3.1 Land use type NLCD ###### 

# NCLD land use type, combining two year data failed after multiple tries, use the year in the middle instead
# ncld_year <- terra::rast("NLCD_landcover/nlcd_2016_land_cover_l48_20210604/nlcd_2016_land_cover_l48_20210604.img")
ncld_year <- 
  terra::rast(
    file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data", 
              "NLCD_landcover/nlcd_2011_fact30_landcover_resampled.tif"))
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


###### 3.2 PMF ###### 

# read files
pmf_source = 
  fread(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data", 
                  "PMF_results/CSN_IMPROVE_source_daily_contribution.csv")) 
date_use = 
  read.fst(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data", 
                     "Date_DOW_Holiday_2011-20.fst"))
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

pmf_source_date_use =
  subset(pmf_source_date,
         (grepl("Traffic", Source_aftermanual, fixed = T) |
            Source_aftermanual == "F3-Secondary Sulfate" |
            Source_aftermanual == "F9-Soil/Dust") &
           Date < as.Date("2011-03-01") & Date > as.Date("2011-01-31"))
unique(pmf_source_date_use$month); unique(pmf_source_date_use$year)

# Modify the 'Source_aftermanual' column based on the condition
pmf_source_date_use <-
  pmf_source_date_use %>%
  mutate(Source_aftermanual =
           ifelse(grepl("Traffic", Source_aftermanual, fixed = TRUE),
                  "F1-Traffic",
                  Source_aftermanual))
unique(pmf_source_date_use$Source_aftermanual)
class(pmf_source_date_use)

###### 3.3 PMF & NCLD merge ###### 

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
head(pmf_nlcd); class(pmf_nlcd)

###### 3.4 CMAQ prepare ###### 

# Open the .nc file
nc_cmaq_file <- nc_open("hr2day_SA_v54_gcc_CMAQ_ISAM_201106.nc")

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

# Describe grids from grid description
#define lat & lon in meters based on origin and cell size
lon <- seq( from = xorig, by = xcell, length.out = ncols)
lat <- seq( from = yorig, by = ycell, length.out = nrows)

# read CMAQ & add crs
cmaq_crs = p4s
start_month = "2011.02"

# NH3
cmaq_nh3_rds = readRDS(paste0("NH3_cmaq_", start_month, ".rds"))
class(cmaq_nh3_rds); head(cmaq_nh3_rds)
names(cmaq_nh3_rds)[3]
names(cmaq_nh3_rds)[3] = "cmaq_NH3"
cmaq_nh3_rds$cmaq_variable = NULL

# SO2
cmaq_so2_rds <- readRDS(paste0("SO2_cmaq_", start_month, ".rds"))
names(cmaq_so2_rds)[3] <- "cmaq_SO2"
cmaq_so2_rds$cmaq_variable <- NULL

# DUST
cmaq_dust_rds <- readRDS(paste0("PM25_TOT_DUST_cmaq_", start_month, ".rds"))
names(cmaq_dust_rds)[3] <- "cmaq_DUST"
cmaq_dust_rds$cmaq_variable <- NULL

# EGU
cmaq_egu_rds <- readRDS(paste0("PM25_TOT_EGU_cmaq_", start_month, ".rds"))
names(cmaq_egu_rds)[3] <- "cmaq_EGU"
cmaq_egu_rds$cmaq_variable <- NULL

# NRD
cmaq_nrd_rds <- readRDS(paste0("PM25_TOT_NRD_cmaq_", start_month, ".rds"))
names(cmaq_nrd_rds)[3] <- "cmaq_NRD"
cmaq_nrd_rds$cmaq_variable <- NULL

# ORD
cmaq_ord_rds <- readRDS(paste0("PM25_TOT_ONR_cmaq_", start_month, ".rds"))
names(cmaq_ord_rds)[3] <- "cmaq_ORD"
cmaq_ord_rds$cmaq_variable <- NULL

# cmaq for each source category, Secondary Sulfate
summary(cmaq_nh3_rds$x == cmaq_so2_rds$x); summary(cmaq_nh3_rds$y == cmaq_so2_rds$y); summary(cmaq_nh3_rds$Date == cmaq_so2_rds$Date)
cmaq_sulfate_rds = merge(cmaq_nh3_rds, cmaq_so2_rds)
cmaq_sulfate_rds = merge(cmaq_sulfate_rds, cmaq_egu_rds)
dim(cmaq_sulfate_rds); dim(cmaq_nh3_rds)

cmaq_sulfate = st_as_sf(cmaq_sulfate_rds, coords = c("x", "y"), crs = cmaq_crs)

# cmaq for each source category, Dust
cmaq_dust = st_as_sf(cmaq_dust_rds, coords = c("x", "y"), crs = cmaq_crs)

# cmaq for each source category, Traffic
cmaq_traffic_rds = merge(cmaq_nrd_rds, cmaq_ord_rds)
dim(cmaq_traffic_rds); dim(cmaq_ord_rds)

cmaq_traffic = st_as_sf(cmaq_traffic_rds, coords = c("x", "y"), crs = cmaq_crs)

###### 3.5 PMF_NCLD file with CMAQ for each source ###### 

# prepare pmf_nlcd for each source category
pmf_nlcd_sulfate = subset(pmf_nlcd, Source_aftermanual == "F3-Secondary Sulfate")
pmf_nlcd_dust = subset(pmf_nlcd, Source_aftermanual == "F9-Soil/Dust")
pmf_nlcd_traffic = subset(pmf_nlcd, Source_aftermanual == "F1-Traffic")

# convert pmf_nlcd crs to CMAQ
pmf_nlcd_sulfate <- st_transform(pmf_nlcd_sulfate, crs = st_crs(cmaq_sulfate))
pmf_nlcd_dust <- st_transform(pmf_nlcd_dust, crs = st_crs(cmaq_dust))
pmf_nlcd_traffic <- st_transform(pmf_nlcd_sulfate, crs = st_crs(cmaq_traffic))

# combine pmf_nlcd with CMAQ for each source category, & remove not matched Date, nearest point
# sulfate
nearest_index <- st_nearest_feature(pmf_nlcd_sulfate, cmaq_sulfate)
pmf_cmaq_sulfate <- cbind(pmf_nlcd_sulfate, cmaq_sulfate[nearest_index, ])
dim(pmf_nlcd_sulfate); dim(pmf_cmaq_sulfate)
pmf_cmaq_sulfate <- 
  pmf_cmaq_sulfate %>%
  filter(Date != Date.1)
dim(pmf_nlcd_sulfate); dim(pmf_cmaq_sulfate); head(pmf_cmaq_sulfate)

pmf_cmaq_sulfate$Date.1 = pmf_cmaq_sulfate$geometry.1 = NULL

# dust
nearest_index <- st_nearest_feature(pmf_nlcd_dust, cmaq_dust)
pmf_cmaq_dust <- cbind(pmf_nlcd_dust, cmaq_dust[nearest_index, ])
dim(pmf_nlcd_dust); dim(pmf_cmaq_dust)
pmf_cmaq_dust <- 
  pmf_cmaq_dust %>%
  filter(Date != Date.1)
dim(pmf_nlcd_dust); dim(pmf_cmaq_dust); head(pmf_cmaq_dust)

pmf_cmaq_dust$Date.1 = pmf_cmaq_dust$geometry.1 = NULL

# traffic
nearest_index <- st_nearest_feature(pmf_nlcd_traffic, cmaq_traffic)
pmf_cmaq_traffic <- cbind(pmf_nlcd_traffic, cmaq_traffic[nearest_index, ])
dim(pmf_nlcd_traffic); dim(pmf_cmaq_traffic)
pmf_cmaq_traffic <- 
  pmf_cmaq_traffic %>%
  filter(Date != Date.1)
dim(pmf_nlcd_traffic); dim(pmf_cmaq_traffic); head(pmf_cmaq_traffic)

pmf_cmaq_traffic$Date.1 = pmf_cmaq_traffic$geometry.1 = NULL

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

# drop geometry and combine with coordinates
pmf_cmaq_for_model_coords <-
  pmf_cmaq_for_model %>%
  st_drop_geometry() %>%
  cbind(coords)
head(pmf_cmaq_for_model_coords); sapply(pmf_cmaq_for_model_coords, class)

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
  PMF_conc ~ ., #   ,  + year
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
  Predictor$new(rf_model_cv, data = model_input, y = model_input$PMF_conc)

# Compute Shapley values for a specific observation (or loop over multiple observations)
shapley_rf_cv <- 
  Shapley$new(predictor_rf, x.interest = model_input[1,])  # Example for first observation

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

###### 4.2.2 Random Forest modeling - Holdout analyses ###### 

model.method = "Random Forest"

site_info = dplyr::select(model_input, X, Y)
site_info = site_info[!duplicated(site_info), ]
dim(site_info)
site_info$Station_ID = 1:nrow(site_info)
site_info$Station_ID = as.factor(site_info$Station_ID)

model_input = merge(model_input, site_info)
dim(model_input); head(model_input)

# Create an empty list to store results from 50 iterations
all_rf_predictions <- list()
rf_rmse_list <- c()
rf_rsq_list <- c()
rf_mae_list <- c()

# Lists to store variable importance from each iteration
rf_var_imp_list <- list()
shapley_results_list <- list()

# Number of iterations
n_iterations <- 50

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
    mtry = 15, # with mtry = 15, very high R-square
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
ggplot(final_rf_predictions, aes(x = Actual_PMF, y = Mean_RF_Prediction)) +
  geom_point(color = "blue", alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste("PMF vs. Averaged RF Predictions (50 Holdout Iterations)",
                     "\nRMSE:", final_rf_rmse, 
                     ", MAE:", final_rf_mae, 
                     ", R-squared:", final_rf_rsq),
       x = "Actual PMF Concentration (µg/m3)",
       y = "Mean Predicted PMF Concentration (µg/m3)") +
  theme_minimal(base_size = 16)


##### Relative variable importance
# Combine all variable importance results into one dataframe
combined_var_imp <- bind_rows(rf_var_imp_list)

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
  labs(title = "Variable Importance with Variability across 50 Iterations",
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
  labs(title = "Average Shapley Values with Variability across 50 Iterations",
       x = "Predictor",
       y = "Mean Shapley Value (phi)") +
  theme_minimal(base_size = 16)

