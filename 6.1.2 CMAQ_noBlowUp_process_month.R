# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran

#### 0.1 Packages ####

# install.packages("ncdf4")
# install.packages("fst")

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
library(stringr)

#### 0.2 Functions ####

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

# # nc_file = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/CMAQ_previous_extract_tries/hr2day_SA_v54_gcc_CMAQ_ISAM_201102.nc"
# nc_file = hy2day_month_noBlow_path
# grid_info <- get_nc_grid_create_crs(nc_file)
# lon = grid_info$lon; lat = grid_info$lat; p4s = grid_info$p4s
# # cmaq_var = "PM25_TOT_NRD"
# cmaq_var = "NO2"

###### Daily, extract selected variable from .nc file, add crs and date and return the corresponding raster brick ###### 
daily_cmaq_var_add_date <- 
  function(nc_file, us_grid_raster, cmaq_var, lon, lat, p4s) {
    
    # Open the .nc file
    # nc_cmaq_file <- nc_open(nc_file)
    nc_temp <- nc_open(nc_file)
    nc_cmaq_file <- brick(nc_file,
                          varname = cmaq_var,
                          crs = p4s)
    
    #create an empty raster for grids with crs descriotion
    latlon_raster.r <- 
      expand.grid( lon = lon,
                   lat = lat,
                   values = NA) %>%
      as.data.table %>%
      rasterFromXYZ( 
        crs = p4s)
    # plot( latlon_raster.r)
    
    extent( nc_cmaq_file)  <- extent( latlon_raster.r)
    # plot( nc_cmaq_file)
    
    # nc_cmaq_file_1 <- projectRaster(nc_cmaq_file, latlon_raster.r)
    # plot( nc_cmaq_file_1)
    
    # Extract the variable from the NetCDF file (dimensions: 442, 265, nday)
    # pollu_var_array <- ncvar_get(nc_cmaq_file, cmaq_var)
    
    # get the number of days in the file and the start day (day of the year)
    n_time <- dim(ncvar_get(nc_temp, "TFLAG"))[3]
    start_day_intg <- ncatt_get(nc_temp, varid = 0)$SDATE
    
    # convert the start day to date, create a sequence of all dates
    start_day = day_of_year_toDate(start_day_intg)
    day_seq <- seq.Date(start_day, by = "day", length.out = n_time)
    
    # # Convert the 3D array to a raster brick (30 layers representing 30 days)
    # pollu_var_brick <- brick(pollu_var_array, 
    #                          xmn = min(lon), xmx = max(lon), 
    #                          ymn = min(lat), ymx = max(lat), 
    #                          crs = p4s)
    
    # Then project pollu_var_brick to match the target US grid of 0.1 or 0.01 degree
    pollu_var_projected <- projectRaster(nc_cmaq_file, # pollu_var_brick, 
                                         us_grid_raster, 
                                         method = "bilinear")
    # extent(nc_cmaq_file); extent(us_grid_raster); extent(pollu_var_projected)
    # plot(pollu_var_projected[[12]])
    # plot(nc_cmaq_file[[12]])
    
    # Only use the day information to rename the layers
    names(pollu_var_projected) <- paste0("Date_", day_seq)
    # plot( pollu_var_projected[[c(1,10,20)]])
    
    # Close the .nc file after processing
    nc_close(nc_temp)
    
    # Return the raster brick
    return(pollu_var_projected)
  }

#### Process Blow-up cleans monthly data, 2026.06 ####

hy2day_month_path = "/projects/HAQ_LAB/Sumaiya/cmaq/cmaq_output/R/data/filtered_hr2day/median_ratio"
hy2day_month_list = list.files(hy2day_month_path, pattern = "\\.nc$", full.names = TRUE)
hy2day_month_list <- hy2day_month_list[str_detect(basename(hy2day_month_list),  "^hr2day_")]
print(hy2day_month_list)

output_path = "/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_noBlowUp"

for(hy2day_month_noBlow_path in hy2day_month_list) { # hy2day_month_noBlow_path = hy2day_month_list[1]
  print("Path of the .nc to be processed:")
  hy2day_month_noBlow_path
  
  hy2day_date = 
    gsub(".*_(\\d{6})\\.nc$", "\\1", 
         basename(hy2day_month_noBlow_path))
  nc_cmaq_month = nc_open(hy2day_month_noBlow_path)
  cmaq_var_names <- names(nc_cmaq_month$var) 

  print("All available CMAQ-ISAM variables:")
  print(cmaq_var_names)
  
  # Get the variable list
  # cmaq_var_use_list_full = c("PM25_TOT_EGU", "PM25_TOT_OTA",
  #                            "PM25_TOT_ONR", "PM25_TOT_NRD",  "PM25_TOT_ACM",
  #                            "PM25_TOT_ASEA", "PM25_TOT_DUST", "PM25_TOT_ARS",
  #                            "PM25_TOT_BIOG", "PM25_TOT_AFI",
  #                            "PM25_TOT_BCO", "PM25_TOT_ICO", "PM25_TOT_LTNG",
  #                            "O3", "NH3", "SO2", "NO2")
  
  # The source sectors have annual records already
  cmaq_var_use_list_full = c("O3", "NH3", "SO2", "NO2")
  
  cmaq_var_use_list = cmaq_var_use_list_full[cmaq_var_use_list_full %in% cmaq_var_names]
  
  print("CMAQ Variable to be used: ")
  cmaq_var_use_list

  # Initialize a list to store combined raster bricks for each variable
  combined_rasters <- vector("list", length(cmaq_var_use_list))
  names(combined_rasters) <- cmaq_var_use_list
  
  # Get grid information and CRS
  grid_info <- get_nc_grid_create_crs(hy2day_month_noBlow_path)
  
  # Process each variable
  for (i in seq_along(cmaq_var_use_list)) {
    cmaq_var <- cmaq_var_use_list[i]
    print(paste0("CMAQ Variable for projection & date assignment: ", cmaq_var))
    
    # Process the variable to create the raster brick
    var_raster <- 
      daily_cmaq_var_add_date(
        hy2day_month_noBlow_path, 
        us_grid_raster,
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
  
  # Save each variable's raster stack to a .fst file
  for (cmaq_var in names(combined_rasters)) {
    
    # extract date and layer names
    layer_names <- names(combined_rasters[[cmaq_var]])
    start_month <- gsub("Date_(\\d{4}\\.\\d{2})\\.\\d{2}", "\\1", layer_names[1])
    dates <- as.Date(gsub("Date_", "", layer_names), format = "%Y.%m.%d")
    
    # Extract spatial information and values
    cmaq_dt = 
      as.data.frame(combined_rasters[[cmaq_var]], # can only as.data.frame and the set to DT
                    xy = TRUE, na.rm = TRUE) # xy = TRUE adds x, y coordinates
    setDT(cmaq_dt)
    # head(cmaq_dt); dim(cmaq_dt)
    
    # Reshape data from wide to long format (each variable in a single column)
    cmaq_var_long <- 
      melt(cmaq_dt, id.vars = c("x", "y"), 
           variable.name = "Layer", value.name = "cmaq_values")
    
    # Add Date and CMAQ variable columns
    cmaq_var_long[, Date := dates[match(Layer, layer_names)]]
    cmaq_var_long[, cmaq_variable := cmaq_var]
    
    print("Check the file head before output")
    head(cmaq_var_long)
    print("CMAQ variables include: ")
    unique(cmaq_var_long$cmaq_variable)
    
    # ggplot() +
    #   geom_point(data = subset(cmaq_var_long,
    #                            cmaq_variable == cmaq_var & 
    #                              Date == "2011-04-21"), 
    #              aes(x = x, y = y, color = cmaq_values),
    #              size = 0.35, alpha = 0.8) +
    #   geom_sf(data = us_states, 
    #           fill = NA, color = "grey70", size = 0.3) + 
    #   scale_color_viridis_c(name = paste(cmaq_var, "Concentration"), option = "plasma") +
    #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) 
    
    # Remove unnecessary columns
    cmaq_var_long[, Layer := NULL]
    
    # Define the output filename
    output_file <- paste0(cmaq_var, "_cmaq_", start_month, ".rds")
   
    # Save the raster stack to a .rds file
    saveRDS(cmaq_var_long, file = file.path(output_path, output_file))
  }
}

#######################################################
#### Use 6.1.3 to combine annual, then below to extra mainland data only #####
#### Extrac mainland pollutant data #####
#######################################################
library(data.table)
library(fst)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_noBlowUp")
getwd()

us_point_coord = 
  read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Long_lat_Mainland_US_0.1_degree.fst")
# dim(us_point_coord)
setDT(us_point_coord)
names(us_point_coord) = c("lon", "lat")

us_point_coord[, lon := round(lon, 2)]
us_point_coord[, lat := round(lat, 2)]

# setkey(), a data.table optimization, sorts a table and builds an internal index for fast joins and filtering.
setkey(us_point_coord, lon, lat)

## File pattern
pollu_annu_files <-
  list.files(
    pattern = "_cmaq_\\d{4}-\\d{2}_\\d{4}-\\d{2}\\.rds$",
    full.names = TRUE
  )

# Extrac CONUS data points
for (pollu_annu in pollu_annu_files){ # pollu_annu = pollu_annu_files[1]
  
  # Output file name
  file_name <- basename(pollu_annu)
  pollutant <- sub("_cmaq.*", "", file_name)
  year <- sub(".*_(\\d{4})-\\d{2}_.*", "\\1", file_name)
  output_name <- paste0(pollutant, "_CMAQ_CONUS_", year, ".rds")
  
  # Read file
  dat <- readRDS(pollu_annu)
  setDT(dat)
  names(dat)[1:2] = c("lon", "lat")
  
  dat[, lon := round(lon, 2)]
  dat[, lat := round(lat, 2)]
  
  setkey(dat, lon, lat)
  
  # Mainland US based on setkey!
  dat_conus <- dat[us_point_coord, nomatch = 0]
  
  # Save
  saveRDS(
    dat_conus,
    file.path("pollutant_mainland", output_name)
  )
  
  rm(dat, dat_conus)
  gc()
}




