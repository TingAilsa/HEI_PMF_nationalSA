library(dplyr)
library(data.table)
library(raster) # .img
library(ncdf4) # .nc
library(sf)

setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya")
getwd()

#### First batch, 2017 01-03, hourly to daily, not use raster ####

###### 1.1 explore data and create crs ###### 
# nc_cmaq_month_nc = "COMBINE_ACONC_v54_gcc_CMAQ_ISAM_2017_Feb_201702.nc"
# nc_cmaq_month_nc = "COMBINE_ACONC_v54_gcc_CMAQ_ISAM_2017_Jan_201701.nc"
nc_cmaq_month_nc = "COMBINE_ACONC_v54_gcc_CMAQ_ISAM_2017_Mar_201703.nc"
nc_cmaq_month = nc_open(nc_cmaq_month_nc)
# nc_cmaq_month = nc_open("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/COMBINE_ACONC_v54_gcc_CMAQ_ISAM_2017_Feb_201702.nc")
print(nc_cmaq_month)

cmaq_var_names <- names(nc_cmaq_month$var) 
length(cmaq_var_names)

# print(cmaq_var_names)   
# [1] "TFLAG"         "ATOTI_EGU"     "ATOTJ_EGU"     "ATOTI_ONR"     "ATOTJ_ONR"     "ATOTI_NRD"     "ATOTJ_NRD"    
# [8] "ATOTI_BCO"     "ATOTJ_BCO"     "ATOTI_ICO"     "ATOTJ_ICO"     "ATOTI_OTH"     "ATOTJ_OTH"     "ATOTI_BIOG"   
# [15] "ATOTJ_BIOG"    "ATOTI_DUST"    "ATOTJ_DUST"    "ATOTI_ASEA"    "ATOTJ_ASEA"    "ATOTI_LTNG"    "ATOTJ_LTNG"   
# [22] "PM25_TOT_EGU"  "PM25_TOT_ONR"  "PM25_TOT_NRD"  "PM25_TOT_BCO"  "PM25_TOT_ICO"  "PM25_TOT_OTH"  "PM25_TOT_BIOG"
# [29] "PM25_TOT_DUST" "PM25_TOT_ASEA" "PM25_TOT_LTNG"

# extract variables from the .nc file
# dim(ncvar_get(nc_cmaq_month, "TFLAG")) # 2 30 672
# dim(ncvar_get(nc_cmaq_month, "ATOTJ_EGU")) # 442 265 672
# dim(ncvar_get(nc_cmaq_month, "PM25_TOT_EGU")) # 442 265 672

# # get all global attributes
# ncatt_get(nc_cmaq_month, varid = 0)

# create crs from file information
xorig <- ncatt_get(nc_cmaq_month, varid = 0, 'XORIG')$value # X, Y-coordinate origin of the grid
yorig <- ncatt_get(nc_cmaq_month, varid = 0, 'YORIG')$value  
xcent <- ncatt_get(nc_cmaq_month, varid = 0, 'XCENT')$value # X, Y-coordinate center of the projection
ycent <- ncatt_get(nc_cmaq_month, varid = 0, 'YCENT')$value
ncols <- ncatt_get(nc_cmaq_month, varid = 0, 'NCOLS')$value # Number of columns & rows in the grid
nrows <- ncatt_get(nc_cmaq_month, varid = 0, 'NROWS')$value # X, Y-cell size
xcell <- ncatt_get(nc_cmaq_month, varid = 0, 'XCELL')$value
ycell <- ncatt_get(nc_cmaq_month, varid = 0, 'YCELL')$value
p_alp <- ncatt_get(nc_cmaq_month, varid = 0, 'P_ALP')$value # Projection parameter: latitude of first standard parallel
p_bet <- ncatt_get(nc_cmaq_month, varid = 0, 'P_BET')$value # Projection parameter: latitude of second standard parallel

ntime <- dim(ncvar_get(nc_cmaq_month, "TFLAG"))[3]
nday <- ntime/24
start_day_intg <- ncatt_get(nc_cmaq_month, varid = 0)$SDATE

# more about grids
gdtyp <- ncatt_get(nc_cmaq_month, varid = 0)$GDTYP # grid type, (e.g., 1 = Lambert Conformal, 2 = Polar Stereographic).
gdnam <- ncatt_get(nc_cmaq_month, varid = 0)$GDNAM 
upnam <- ncatt_get(nc_cmaq_month, varid = 0)$UPNAM

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

###### 1.2 round selected variables to daily and match with crs ###### 

cmaq_var_use_list = cmaq_var_names[c(22, 26, 29, 30)]
cmaq_var_use_list

# Initialize an empty list to store all data
final_data_list <- list()

# Initialize a list to store daily averages
pollu_var_daily <- list()
hours_per_day = 24

# Loop through each variable in cmaq_var_use_list
for (cmaq_var in cmaq_var_use_list) {
  
  # Extract the variable from the NetCDF file
  pollu_var_array <- 
    ncvar_get(nc_cmaq_month, cmaq_var)
  pollu_var_brick <- 
    brick(pollu_var_array, 
          xmn=min(lat), xmx=max(lat), 
          ymn=min(lon), ymx=max(lon), crs=p4s)
  
  # Correct the orientation by flipping and transposing
  # flip is applied cause in NetCDF file, often have latitudes stored from north to south.
  pollu_var_brick <- 
    flip(t(pollu_var_brick), direction='y')

  # Initialize a list to store daily averages
  pollu_var_daily <- list()
  
  # Loop through each day (24 hour layers) and calculate the daily average
  for (day in 1:nday) {
    # Extract 24 layers corresponding to the current day
    single_day_layer <- 
      pollu_var_brick[[((day - 1) * hours_per_day + 1):(day * hours_per_day)]]
    
    # Calculate the daily mean
    single_daily_mean <- calc(single_day_layer, fun = mean, na.rm = TRUE)
    
    # Store the daily mean
    pollu_var_daily[[day]] <- single_daily_mean
  }
  
  # Combine the daily averages into a raster brick
  pollu_var_daily_brik <- stack(pollu_var_daily)
  # dim(pollu_var_daily_brik)
  
  # Reset the extent and CRS, the spatial layout may have changed after flip() and t()
  extent_longlat <- extent(min(lon), max(lon), min(lat), max(lat))
  extent(pollu_var_daily_brik) <- extent_longlat
  crs(pollu_var_daily_brik) <- p4s
  # head(pollu_var_daily_brik); dim(pollu_var_daily_brik); class(pollu_var_daily_brik)
  
  # Loop through each day to convert daily rasters to sf and data.table
  for (day in 1:nday) {
    pollu_var_day_raster <- pollu_var_daily_brik[[day]]
    
    # Convert raster to SpatialPolygonsDataFrame, then to sf, then to data.table
    pollu_var_day_sf.dt <- 
      pollu_var_day_raster %>%
      as('SpatialPolygonsDataFrame') %>% # more efficient than rasterToPolygons{}
      st_as_sf() %>%
      as.data.table()
    names(pollu_var_day_sf.dt)[1] = "value"
    
    # Add the variable name and day as new columns
    pollu_var_day_sf.dt[, `:=`(variable = cmaq_var, day = day + start_day_intg - 1)]
    # head(pollu_var_day_sf.dt); dim(pollu_var_day_sf.dt); class(pollu_var_day_sf.dt)
    # unique(pollu_var_day_sf.dt$variable); unique(pollu_var_day_sf.dt$day)
    
    # Append the result to the final list
    final_data_list[[length(final_data_list) + 1]] <- pollu_var_day_sf.dt
    # head(final_data_list); dim(final_data_list[[2]]); head(final_data_list[[2]]); class(final_data_list[[2]])
    # View(final_data_list)
  }
}


# Combine all the daily data into a single data.table
final_data <- rbindlist(final_data_list, use.names = TRUE)

# saveRDS(final_data, "CMAQ_extracted_PM_201701.rds")
# saveRDS(final_data, "CMAQ_extracted_PM_201702.rds")
saveRDS(final_data, "CMAQ_extracted_PM_201703.rds")

#### Second batch, 2016 01, 06, 07, use raster ####

getwd()

nc_cmaq_month_nc = "hr2day_v54_gcc_CMAQ_ISAM_201102.nc"

nc_cmaq_month_nc = "hr2day_SA_v54_gcc_CMAQ_ISAM_201102.nc"

nc_cmaq_month = nc_open(nc_cmaq_month_nc)
# nc_cmaq_month = nc_open("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/COMBINE_ACONC_v54_gcc_CMAQ_ISAM_2017_Feb_201702.nc")
# print(nc_cmaq_month)

cmaq_var_names <- names(nc_cmaq_month$var) 
length(cmaq_var_names)

print(cmaq_var_names)
# [1] "TFLAG"         "ATOTI_EGU"     "ATOTJ_EGU"     "ATOTI_ONR"     "ATOTJ_ONR"     "ATOTI_NRD"     "ATOTJ_NRD"    
# [8] "ATOTI_BCO"     "ATOTJ_BCO"     "ATOTI_ICO"     "ATOTJ_ICO"     "ATOTI_OTH"     "ATOTJ_OTH"     "ATOTI_BIOG"   
# [15] "ATOTJ_BIOG"    "ATOTI_DUST"    "ATOTJ_DUST"    "ATOTI_ASEA"    "ATOTJ_ASEA"    "ATOTI_LTNG"    "ATOTJ_LTNG"   
# [22] "PM25_TOT_EGU"  "PM25_TOT_ONR"  "PM25_TOT_NRD"  "PM25_TOT_BCO"  "PM25_TOT_ICO"  "PM25_TOT_OTH"  "PM25_TOT_BIOG"
# [29] "PM25_TOT_DUST" "PM25_TOT_ASEA" "PM25_TOT_LTNG"

# extract variables from the .nc file
# dim(ncvar_get(nc_cmaq_month, "TFLAG")) # 2 30 672
# dim(ncvar_get(nc_cmaq_month, "ATOTJ_EGU")) # 442 265 672
# dim(ncvar_get(nc_cmaq_month, "PM25_TOT_EGU")) # 442 265 672

PM_TOT_EGU_nc = ncvar_get(nc_cmaq_month, "PM25_TOT_EGU")
head(PM_TOT_EGU_nc); dim(PM_TOT_EGU_nc)

# # get all global attributes
# ncatt_get(nc_cmaq_month, varid = 0)

# create crs from file information
xorig <- ncatt_get(nc_cmaq_month, varid = 0, 'XORIG')$value # X, Y-coordinate origin of the grid
yorig <- ncatt_get(nc_cmaq_month, varid = 0, 'YORIG')$value  
xcent <- ncatt_get(nc_cmaq_month, varid = 0, 'XCENT')$value # X, Y-coordinate center of the projection
ycent <- ncatt_get(nc_cmaq_month, varid = 0, 'YCENT')$value
ncols <- ncatt_get(nc_cmaq_month, varid = 0, 'NCOLS')$value # Number of columns & rows in the grid
nrows <- ncatt_get(nc_cmaq_month, varid = 0, 'NROWS')$value # X, Y-cell size
xcell <- ncatt_get(nc_cmaq_month, varid = 0, 'XCELL')$value
ycell <- ncatt_get(nc_cmaq_month, varid = 0, 'YCELL')$value
p_alp <- ncatt_get(nc_cmaq_month, varid = 0, 'P_ALP')$value # Projection parameter: latitude of first standard parallel
p_bet <- ncatt_get(nc_cmaq_month, varid = 0, 'P_BET')$value # Projection parameter: latitude of second standard parallel

ntime <- dim(ncvar_get(nc_cmaq_month, "TFLAG"))[3]
nday <- ntime
start_day_intg <- ncatt_get(nc_cmaq_month, varid = 0)$SDATE

# more about grids
gdtyp <- ncatt_get(nc_cmaq_month, varid = 0)$GDTYP # grid type, (e.g., 1 = Lambert Conformal, 2 = Polar Stereographic).
gdnam <- ncatt_get(nc_cmaq_month, varid = 0)$GDNAM 
upnam <- ncatt_get(nc_cmaq_month, varid = 0)$UPNAM

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


# Define the list of all .nc files in the getwd()
# meteorology
nc_files <- list.files(pattern = "hr2day_v54_gcc_CMAQ_ISAM_20\\d{4}.nc")
# combined PM sources
nc_files <- list.files(pattern = "COMBINE_ACONC_v54_gcc_CMAQ_ISAM_20\\d{4}.nc")

nc_files <- c("COMBINE_ACONC_v54_gcc_CMAQ_ISAM_2017_Jan_201702.nc")

nc_files <- c("COMBINE_SA_ACONC_v54_gcc_CMAQ_ISAM_201102.nc")

nc_files <- c("hr2day_v54_gcc_CMAQ_ISAM_201102.nc")

# Define the cmaq_var_use_list based on variables of interest
nc_cmaq_month_check = nc_open(nc_files[1])
cmaq_var_names <- names(nc_cmaq_month_check$var) 
print(cmaq_var_names)
# cmaq_var_use_list = cmaq_var_names[c(2:5, 19)] # air pollutant
cmaq_var_use_list = cmaq_var_names[c(22:24, 29:30)] # PM sources
print(cmaq_var_use_list)

# Initialize a list to store combined raster bricks for each variable
combined_rasters <- vector("list", length(cmaq_var_use_list))
names(combined_rasters) <- cmaq_var_use_list

# Process each .nc file
for (nc_file in nc_files) {
  # Get grid information and CRS
  grid_info <- get_nc_grid_create_crs(nc_file)
  
  # Process each variable
  for (i in seq_along(cmaq_var_use_list)) {
    cmaq_var <- cmaq_var_use_list[i]
    
    # Process the variable to create the raster brick
    var_raster <- 
      select_cmaq_var_add_date(
        nc_file, 
        cmaq_var, 
        grid_info$lon, grid_info$lat, grid_info$p4s)
    
    # Stack or combine the raster bricks across files
    if (is.null(combined_rasters[[cmaq_var]])) {
      combined_rasters[[cmaq_var]] <- var_raster
    } else {
      combined_rasters[[cmaq_var]] <- 
        stack(combined_rasters[[cmaq_var]], var_raster)
    }
  }
}

# Save each variable's raster stack to a GeoTIFF file
for (cmaq_var in names(combined_rasters)) {
  # Define the output filename
  output_file <- paste0(cmaq_var, "_combined_raster.tif")
  
  # Save the raster stack to a GeoTIFF file
  writeRaster(combined_rasters[[cmaq_var]], 
              filename = output_file, 
              format = "GTiff", 
              overwrite = TRUE)
}





