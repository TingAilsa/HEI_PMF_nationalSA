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
    # lon= grid_info$lon; lat =  grid_info$lat; p4s = grid_info$p4s
    
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

# #### Check what files are lacking ####
# 
# # Path, CMAQ variables, and YYYYMM combinations
# cmaq_path <- "/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_noBlowUp"
# 
# cmaq_var_list <- c("O3", "NH3", "SO2", "NO2",
#                    "PM25_FRM", "PM25_TOT",
#                    "PM25_TOT_ACM", "PM25_TOT_AFI", "PM25_TOT_ARS",
#                    "PM25_TOT_ASEA", "PM25_TOT_BIOG", "PM25_TOT_EGU",
#                    "PM25_TOT_NRD", "PM25_TOT_ONR", "PM25_TOT_OTA")
# 
# years <- 2011:2020
# months <- sprintf("%02d", 1:12)
# 
# # Generate expected filenames
# expected <- character()
# for (var in cmaq_var_list) {
#   for (year in years) {
#     for (month in months) {
#       expected <- c(expected, sprintf("%s_001_%d%s.fst", var, year, month))
#     }
#   }
# }
# 
# # Get actual files
# actual <- list.files(cmaq_path, pattern = "\\.fst$")
# 
# # Find missing
# missing <- expected[!expected %in% actual]
# 
# cat("Total expected:", length(expected), "\n")
# cat("Total actual:  ", length(actual), "\n")
# cat("Missing:       ", length(missing), "\n\n")
# 
# if (length(missing) > 0) {
#   # Parse missing filenames with correct variable extraction
#   missing_info <- data.frame(
#     filename = missing,
#     stringsAsFactors = FALSE
#   )
# 
#   # Extract variable name (everything before the first underscore)
#   missing_info$var <- sub("_.*", "", missing_info$filename)
# 
#   # Extract year (4 digits after the second underscore)
#   missing_info$year <- as.integer(sub(".*_\\d{3}_(\\d{4})\\d{2}\\.fst", "\\1", missing_info$filename))
# 
#   # Extract month (2 digits after the year)
#   missing_info$month <- sub(".*_(\\d{4})(\\d{2})\\.fst", "\\2", missing_info$filename)
# 
#   # Create YYYYMM column
#   missing_info$YYYYMM <- paste0(missing_info$year, missing_info$month)
# 
#   # Reorder columns for better readability
#   missing_info <- missing_info[, c("filename", "var", "YYYYMM", "year", "month")]
# 
#   # Sort by variable and YYYYMM
#   missing_info <- missing_info[order(missing_info$var, missing_info$YYYYMM), ]
# 
#   # Display results
#   cat("First 20 missing files:\n")
#   print(head(missing_info, 20))
# 
#   # Summary by variable (now showing full PM25 names)
#   cat("\n\n=== SUMMARY BY VARIABLE ===\n")
#   var_summary <- as.data.frame(table(missing_info$var))
#   names(var_summary) <- c("Variable", "Missing_Count")
#   var_summary <- var_summary[order(-var_summary$Missing_Count), ]
#   print(var_summary)
#   cat("\nTotal variables with missing files:", nrow(var_summary), "\n")
# 
#   # Summary by year
#   cat("\n\n=== SUMMARY BY YEAR ===\n")
#   year_summary <- as.data.frame(table(missing_info$year))
#   names(year_summary) <- c("Year", "Missing_Count")
#   year_summary <- year_summary[order(year_summary$Year), ]
#   print(year_summary)
# 
#   # Summary by year-month
#   cat("\n\n=== SUMMARY BY YEAR-MONTH (YYYYMM) ===\n")
#   yyyymm_summary <- as.data.frame(table(missing_info$YYYYMM))
#   names(yyyymm_summary) <- c("YYYYMM", "Missing_Count")
#   yyyymm_summary <- yyyymm_summary[order(yyyymm_summary$YYYYMM), ]
#   print(yyyymm_summary)
# 
#   # Summary by month (across all years)
#   cat("\n\n=== SUMMARY BY MONTH (across all years) ===\n")
#   month_summary <- as.data.frame(table(missing_info$month))
#   names(month_summary) <- c("Month", "Missing_Count")
#   month_summary <- month_summary[order(month_summary$Month), ]
#   print(month_summary)
# 
#   # Which variables have all files?
#   complete_vars <- setdiff(cmaq_var_list, unique(missing_info$var))
#   cat("\n\n=== COMPLETE VARIABLES (no missing files) ===\n")
#   if (length(complete_vars) > 0) {
#     print(complete_vars)
#   } else {
#     cat("None - all variables have at least one missing file\n")
#   }
# 
#   # Check if any specific year-month has all files missing
#   cat("\n\n=== YEAR-MONTHS WITH MOST MISSING ===\n")
#   top_missing <- yyyymm_summary[order(-yyyymm_summary$Missing_Count), ]
#   print(head(top_missing, 10))
# 
#   # Save detailed missing files list
#   write.csv(missing_info,
#             file.path(cmaq_path, "missing_cmaq_files_detailed.csv"),
#             row.names = FALSE)
#   cat("\n\nDetailed missing files list saved to:",
#       file.path(cmaq_path, "missing_cmaq_files_detailed.csv"), "\n")
# 
#   # Also save summaries
#   write.csv(var_summary,
#             file.path(cmaq_path, "missing_summary_by_variable.csv"),
#             row.names = FALSE)
# 
#   write.csv(yyyymm_summary,
#             file.path(cmaq_path, "missing_summary_by_yyyymm.csv"),
#             row.names = FALSE)
# 
# } else {
#   cat("\n✓ ALL FILES ARE PRESENT!\n")
# }

#### Process Blow-up cleans monthly data, Limit to the MISSING ONES till 20260817 ####

hy2day_month_path = "/projects/HAQ_LAB/Sumaiya/cmaq/cmaq_output/R/data/filtered_hr2day/median_ratio"
hy2day_month_list = list.files(hy2day_month_path, pattern = "\\.nc$", full.names = TRUE)
hy2day_month_list <- hy2day_month_list[str_detect(basename(hy2day_month_list),  "^hr2day_")]
print(hy2day_month_list)
length(hy2day_month_list)

output_path = "/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_noBlowUp"

us_point_coord = 
  read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Long_lat_CONUS_0.01_degree.fst")

grid_points <- vect(us_point_coord,
                    geom = c("Longitude", "Latitude"),
                    crs  = "EPSG:4326")
n_points    <- nrow(us_point_coord)

# # Convert to spatial file for raster::extract instead of terra::extract
# # 0.01 deg files too big; and somehow when converting data to terra, an unfixable error persisted
# grid_points_sp <- as(grid_points, "Spatial")

# US raster grid resolution, read file, rast or raster, need to double check
# us_grid_raster_01 = 
#   rast(file.path("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/base_raster_grid_sf/us_grid_raster_01.tif"))
# us_grid_raster_001 =
#   terra::rast(file.path("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/base_raster_grid_sf/us_grid_raster_001.tif"))
us_grid_raster_001 =
  raster::raster(file.path("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/base_raster_grid_sf/us_grid_raster_001.tif"))

# Choose the US grid resolution to project
us_grid_raster = us_grid_raster_001
# us_grid_raster = us_grid_raster_01

# included_years = c(2018, 2019, 2020)
included_years = 2011:2020

# Full variable list
# cmaq_var_use_list_full = 
#   c("PM25_TOT_EGU", "PM25_TOT_OTA",
#     "PM25_TOT_ONR", "PM25_TOT_NRD",  "PM25_TOT_ACM",
#     "PM25_TOT_ASEA", "PM25_TOT_DUST", "PM25_TOT_ARS",
#     "PM25_TOT_BIOG", "PM25_TOT_AFI",
#     "PM25_TOT_BCO", "PM25_TOT_ICO", "PM25_TOT_LTNG",
#     "PM25_TOT", "PM25_FRM",
#     "O3", "NH3", "SO2", "NO2")

cmaq_var_use_list_full <- 
  c("O3", "NH3", "SO2", "NO2",           
    "PM25_FRM", "PM25_TOT",
    "PM25_TOT_ACM", "PM25_TOT_AFI", "PM25_TOT_ARS", 
    "PM25_TOT_ASEA", "PM25_TOT_BIOG", "PM25_TOT_EGU",  
    "PM25_TOT_NRD", "PM25_TOT_ONR", "PM25_TOT_OTA")

# Read the missing files summary
missing_info <- 
  read.csv(file.path(output_path, "missing_cmaq_files_detailed.csv"))
missing_info$var <-
  sub("_001_.*", "", missing_info$filename)
  
# All YYYYMM with missing CMAQ processed outputs
missing_dates = unique(missing_info$YYYYMM)
# length(missing_dates); nrow(missing_info)


# Create a tracking file to avoid reprocessing if script fails
tracking_file <- file.path(output_path, "processing_tracking.csv")
if (file.exists(tracking_file)) {
  processed <- read.csv(tracking_file)
} else {
  processed <- data.frame(
    var = character(),
    YYYYMM = character(),
    processed_date = character(),
    status = character(),
    stringsAsFactors = FALSE
  )
}

# Loop through missing files
total_missing <- nrow(missing_info)
cat("Total missing files to process:", total_missing, "\n\n")

# Original CMAQ-ISAM files
hy2day_month_path = "/projects/HAQ_LAB/Sumaiya/cmaq/cmaq_output/R/data/filtered_hr2day/median_ratio"
hy2day_month_list = list.files(hy2day_month_path, pattern = "\\.nc$", full.names = TRUE)
hy2day_month_list <- hy2day_month_list[str_detect(basename(hy2day_month_list),  "^hr2day_")]


# 201702 cannot be processed for now
# missing_dates[31]

for (ym in missing_dates) { # length(missing_dates) = 73 # missing_dates[1:36]
  # ym = missing_dates[31]
  # Missing file info at this date
  cmaq_miss_ym <-
    subset(missing_info, YYYYMM == ym)
  year <- cmaq_miss_ym$year[1]
  month <- cmaq_miss_ym$month[1]
  cmaq_miss_vars_ym <-
    unique(cmaq_miss_ym$var)
  
  for (cmaq_var in cmaq_miss_vars_ym) {
    # cmaq_var = cmaq_miss_vars_ym[1]
    cat("Processing:", 
        cmaq_var, ym, "\n")
    
    tryCatch({
      
      missing_file_cmaq <- sprintf("hr2day_filtered_w_organics_%s.nc", ym)
      hy2day_month_noBlow_path <- 
        file.path(hy2day_month_path, missing_file_cmaq)
      ## Check if the file is within hy2day_month_list
      # hy2day_month_noBlow_path %in% hy2day_month_list
      
      cat("Processing:", basename(hy2day_month_noBlow_path), "\n")
      
      # Get monthly CMAQ-ISAM
      nc_cmaq_month = nc_open(hy2day_month_noBlow_path)
      cmaq_var_names <- names(nc_cmaq_month$var) 
      nc_close(nc_cmaq_month)
      
      # Get grid information and CRS
      grid_info <- get_nc_grid_create_crs(hy2day_month_noBlow_path)
      
      cat("All available CMAQ-ISAM variables:", cmaq_var_names, "\n")
      
      # Assign dates and grid info
      var_raster_org <- daily_cmaq_var_add_date(
        hy2day_month_noBlow_path, us_grid_raster, cmaq_var,
        grid_info$lon, grid_info$lat, grid_info$p4s
      )
      
      # Extract dates
      layer_names <- names(var_raster_org)
      dates <- as.Date(gsub("Date_", "", layer_names), format = "%Y.%m.%d")
      
      ############# For 0.01 degree #############
      ## raster::extract, but very very slow, so change to terra::extract
      # cmaq_conus <- raster::extract(var_raster_org, grid_points_sp)
      
      ## Force terra & extract CONUS points
      ## output is ALREADY exactly the CONUS points, in us_point_coord's row order
      # terra::rast method 5
      var_raster_org_all <- raster::readAll(var_raster_org)
      var_raster     <- terra::rast(var_raster_org_all)
      
      # Extraction onto CONUS points
      # output is ALREADY exactly the CONUS points, in us_point_coord's row order
      cmaq_conus <- terra::extract(var_raster, grid_points, ID = FALSE)
      
      stopifnot(nrow(cmaq_conus) == n_points)  # tripwire: catches any grid mismatch immediately
      colnames(cmaq_conus) <- format(dates, "%Y%m%d")
      # head(cmaq_conus); dim(cmaq_conus)
      # summary(cmaq_conus)
      
      # Output
      write_fst(cmaq_conus,
                file.path(output_path, paste0(cmaq_var, "_001_", format(dates[1], "%Y%m"), ".fst")),
                compress = 100)
      cat("Complete task:", paste0(cmaq_var, "_", format(dates[1], "%Y%m")), "\n")
      
      # file_output = file.path(output_path, paste0(cmaq_var, "_", format(dates[1], "%Y%m"), ".fst"))
      # file.exists(file_output)
      
      # If successful, record it
      processed <- 
        rbind(processed, data.frame(
          var = cmaq_var,
          YYYYMM = ym,
          processed_date = Sys.time(),
          status = "success",
          stringsAsFactors = FALSE
        ))
      
      write.csv(processed, tracking_file, row.names = FALSE)
      
    }, error = function(e) {
      cat("ERROR processing", cmaq_var, ym, ":", e$message, "\n")
      
      # Record failure
      processed <- rbind(processed, data.frame(
        var = cmaq_var,
        YYYYMM = ym,
        processed_date = Sys.time(),
        status = paste("failed:", e$message),
        stringsAsFactors = FALSE
      ))
      
      write.csv(processed, tracking_file, row.names = FALSE)
    })
  }
}

cat("\nProcessing complete!\n")
cat("Total processed:", sum(processed$status == "success"), "files\n")
cat("Failed:", sum(processed$status != "success"), "files\n")

# Check if any files are still missing after processing
remaining_missing <- missing_info[!paste(var, YYYYMM) %in% 
                                    paste(processed$var[processed$status == "success"], 
                                          processed$YYYYMM[processed$status == "success"]), ]

if (nrow(remaining_missing) > 0) {
  cat("\nStill missing after processing:", nrow(remaining_missing), "files\n")
  write.csv(remaining_missing, 
            file.path(output_path, "still_missing_after_processing.csv"), 
            row.names = FALSE)
} else {
  cat("\n✓ All missing files successfully processed!\n")
}






