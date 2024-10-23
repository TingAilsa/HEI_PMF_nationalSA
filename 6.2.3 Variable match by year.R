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

setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data")
getwd()

###### 1 PMF process & merge with date info ######
# read files
pmf_source = fread("PMF_results/CSN_IMPROVE_source_daily_contribution.csv") 
date_use = read.fst("Date_DOW_Holiday_2011-20.fst")
sapply(date_use, class)

# Merge date info with source info
pmf_source$Date = as.Date(pmf_source$Date)
pmf_source = base::merge(pmf_source, date_use, all.x = TRUE)

# Raster does not include date, convert date to integer and change back later
# pmf_source$Date <- as.integer(format(pmf_source$Date, "%Y%m%d"))

# change the characters to factor
pmf_source$Source_aftermanual = as.factor(pmf_source$Source_aftermanual)
pmf_source$day_of_week = as.factor(pmf_source$day_of_week)
pmf_source$is_holiday = as.factor(pmf_source$is_holiday)
sapply(pmf_source, class)

#### rasterize can only contain numeric, integer, and logical values, pmf_source is y, so not rasterize, merge it in the last step
# Convert pmf_single_source to sf with crs 4326
# here, cannot use crs = st_crs(cmaq_single_source_sf), 
#       which will treat long and lat as the projected CRS in CMAQ dataset, 
#       causing the coordinates to appear incorrectly in a much smaller area.
pmf_source_date <-
  st_as_sf(pmf_source,
           coords = c("Longitude", "Latitude"),
           crs = 4326) # EPSG:4326 (WGS 84) is for longitude/latitude
head(pmf_source_date)

###### instead of SF, convert to RASTER for easier merging later

# ###### 2 Census varaibles for Aim3 modeling preparation ######
# # Geometry of each GEOID at the included census tract level
# census_tract_geo =
#   st_read(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS",
#                     "ACS_census_tract_geoid_geometry_4326.gpkg"))
# st_geometry(census_tract_geo) <- "geometry"
# head(census_tract_geo)
# 
# # Census tract level info
# census_tract_acs =
#   fread(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS",
#                   "US_Census_ACS_tract_2011-2020.csv"))
# 
# # extract ACS Census variables to use for fine-scale modeling
# census_acs_use =
#   subset(census_tract_acs,
#          variable %in%
#            c(c("B08006_002", "B08006_008", paste0("B08006_", sprintf("%03d", 14:17))), # commute method
#              paste0("B08303_", sprintf("%03d", 2:13)))) # commute time
# 
# # convert GEOID to 11 digits character, probably starting with 0
# census_acs_use$GEOID <- ifelse(
#   nchar(as.character(census_acs_use$GEOID)) < 11,
#   sprintf("%011s", as.character(census_acs_use$GEOID)),
#   as.character(census_acs_use$GEOID)
# )
# head(census_tract_acs)
# head(census_acs_use)
# 
# # expand the census_acs_use with variables as colnames
# census_acs_wide <-
#   census_acs_use %>%
#   pivot_wider(
#     names_from = "variable",
#     values_from = "estimate"
#   )
# 
# # re-estimate the commute time based on paste0("B08303_", sprintf("%03d", 2:13)) series columns
# # c("min_0-4", "min_5-9", "min_10-14", "min_15-19", "min_20-24", "min_25-29", "min_30-34", "min_35-39", "min_40-44", "min_45-59", "min_60-89", "min_90-more")
# # Define the column names for commute times
# commute_cols <- paste0("B08303_", sprintf("%03d", 2:13))
# 
# # Define the assumed commute times (0, 5, 10, ..., 90 mins)
# commute_times <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 60, 90)
# 
# # Calculate the average commute time
# census_acs_wide$commute_time <- 
#   rowSums(sweep(census_acs_wide[, commute_cols], 2, commute_times, FUN = "*")) / 
#   rowSums(census_acs_wide[, commute_cols])
# 
# # remove commute_cols
# census_acs_wide[, paste0("B08303_", sprintf("%03d", 2:13))] <- NULL
# 
# # rename
# names(census_acs_wide)[3:8]
# names(census_acs_wide)[3:8] = 
#   c("car_truck_van", "public_transport", "bike", "walk", "taxi_moto_etc", "work_home")
# head(census_acs_wide)
# 
# # create dataset to list what census variables represent
# census_variable = data.frame(
#   variable = 
#     c(c("B08006_002", "B08006_008", paste0("B08006_", sprintf("%03d", 14:17))), # commute method
#       paste0("B08303_", sprintf("%03d", 2:13))),
#   Census_group = 
#     c(rep("Commute_method", 6), rep("commute_time", 12)),
#   Census_sub = 
#     c("car_truck_van", "public_transport", "bike", "walk", "taxi_moto_etc", "work_home",
#       "min_0-4", "min_5-9", "min_10-14", "min_15-19", "min_20-24", "min_25-29", 
#       "min_30-34", "min_35-39", "min_40-44", "min_45-59", "min_60-89", "min_90-more"))
# 
# write_fst(census_acs_wide, 
#           file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS",
#                     "US_Census_ACS_tract_commute_2011-2020.fst"))

######### 3 Create grid-like data, something can be done later ########
#Define the approximate bounding box for mainland U.S.
# us_bbox <- c(xmin = -125, xmax = -66, ymin = 24, ymax = 50)
# crs_proj <- "+proj=longlat +datum=WGS84 +no_defs"
# 
# # Convert the bounding box to an extent object for the raster package
# us_extent <- extent(us_bbox["xmin"], us_bbox["xmax"], us_bbox["ymin"], us_bbox["ymax"])
# 
# # Create the raster grid with a desired resolution (e.g., 0.1 degrees)
# us_grid_raster <- raster(ext = us_extent, resolution = 0.1)
# crs(us_grid_raster) <- crs_proj
# dim(us_grid_raster)
# writeRaster(us_grid_raster, "US_O.1_grid_raster.tif", 
#             format = "GTiff", 
#             options = "COMPRESS=LZW", 
#             overwrite = TRUE)
# 
# # convert the grid to an sf object
# us_grid_sf <- st_as_sf(as(us_grid_raster, "SpatialPolygons"))
# st_crs(us_grid_sf) <- crs_proj

# # convert census_tract_geo to sf
# census_tract_geo_sf <- st_transform(census_tract_geo, st_crs(us_grid_sf))
# 
# # Save it as an sf file for future use
# st_write(census_tract_geo_sf,
#          file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS",
#                    "census_tract_geo_reprojected.gpkg"))

# Load the pre-saved geometry for census tracts 
# census_tract_geo_sf <-
#   st_read(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS",
#                     "census_tract_geo_reprojected.gpkg"))

# us_grid_censusGeo_sf <-
#   st_join(us_grid_sf, census_tract_geo_sf, join = st_intersects, left = TRUE)
# 
# # Save it as an sf file for future use
# st_write(us_grid_censusGeo_sf,
#          file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/",
#                    "US_O.1_grid_censusID.gpkg"))

# Load the US 0.1 Degree grid and census tract ID
us_grid_censusGeo_sf <- 
  st_read(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/",
                    "US_O.1_grid_censusID.gpkg"))
st_crs(us_grid_censusGeo_sf)
dim(us_grid_censusGeo_sf)
length(unique(us_grid_censusGeo_sf$geom))

# Load the US 0.1 Degree raster
us_grid_raster <- raster("US_O.1_grid_raster.tif")
head(us_grid_raster)
crs(us_grid_raster)
dim(us_grid_raster)

# # check if mainland US has been covered by GEOID
# plot(us_grid_censusGeo_sf["GEOID"])
# plot(st_geometry(us_grid_censusGeo_sf[is.na(us_grid_censusGeo_sf$GEOID), ]), 
#      col = "red", main = "Areas with NA GEOID")


###### 4 NCLD info ######
##### !!!Different grid-like files are easier to combine when reading NCLD data via raster compared with terra::rast
##### !!! Efficient grid file combination via raster than terra::rast !!!
# ncld_2011 <- raster("NLCD_landcover/nlcd_2011_fact30_landcover_resampled.tif")
# ncld_2013 <- raster("NLCD_landcover/nlcd_2013_fact30_landcover_resampled.tif")
# ncld_2016 <- raster("NLCD_landcover/nlcd_2016_fact30_landcover_resampled.tif")
# ncld_2019 <- raster("NLCD_landcover/nlcd_2019_fact30_landcover_resampled.tif")

ncld_2011 <- raster("NLCD_landcover/nlcd_2011_fact300_landcover_resampled.tif")
ncld_2013 <- raster("NLCD_landcover/nlcd_2013_fact300_landcover_resampled.tif")
ncld_2016 <- raster("NLCD_landcover/nlcd_2016_fact300_landcover_resampled.tif")
ncld_2019 <- raster("NLCD_landcover/nlcd_2019_fact300_landcover_resampled.tif")

# plot(ncld_2011)

###### 5 merge PMF, Census & NCLD for each year ######
census_acs_wide = 
  read_fst(
    file.path(
      "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS",
      "US_Census_ACS_tract_commute_2011-2020.fst"))

# check if crs match
# crs(pmf_source_date); crs(census_acs_wide); crs(ncld_2011) 
# class(pmf_source_date); class(census_acs_wide); class(ncld_2011) 
# head(pmf_source_date); head(census_acs_wide); head(ncld_2011) 

# Define year groups for NLCD usage, using preloaded rasters
ncld_year_groups <- list(
  list(years = c(2011, 2012), ncld_raster = ncld_2011),
  list(years = c(2013, 2014), ncld_raster = ncld_2013),
  list(years = c(2015, 2016, 2017), ncld_raster = ncld_2016),
  list(years = c(2018, 2019, 2020), ncld_raster = ncld_2019)
)

# Iterate over each group of years and process data
for (ncld_group in ncld_year_groups) { # ncld_group = ncld_year_groups[[1]]
  # NLCD raster for this group
  ncld_raster <- ncld_group$ncld_raster
  
  # projectRaster the ncld_raster with the project 0.1*0.1 degree grid raster
  # projectRaster takes the first argument and reprojects it to match the CRS of the second 
  us_grid_ncld_raster <- projectRaster( ncld_raster, us_grid_raster)
    
  # Process each year in the group
  for (study_year in ncld_group$years) { # study_year = ncld_group$years[1]
    # Filter pmf_source_date and census_acs_geom for the current year
    pmf_source_year <- subset(pmf_source_date, year == study_year)
    census_acs_year <- subset(census_acs_wide, year == study_year)
    
    # head(pmf_source_year); unique(pmf_source_year$year); unique(census_acs_year$year)
    # dim(pmf_source_year); dim(pmf_source_date)
    # head(census_acs_year); dim(census_acs_year)
    
    # Spatial join with grid - pmf_source_date, ncld_poly
    pmf_grid <- 
      st_join(us_grid_censusGeo_sf, pmf_source_year, join = st_intersects, left = TRUE)

    # The current merge would lose grids of those with no info such as PMF and/or others
    # pmf_ncld_grid <-
    #   st_join(pmf_grid, ncld_poly, join = st_intersects, left = TRUE)
    pmf_ncld_grid <- 
      st_join(pmf_grid, ncld_poly_simplified, join = st_within, left = TRUE)
    
    # Merge with census_acs_year
    pmf_census_ncld_grid <-
      base::merge(pmf_ncld_grid, census_acs_year)
    
    # Save the result for this year with spatial information
    st_write(pmf_census_ncld_grid, paste0("PMF_source_NCLD_land_ACS_Census_", study_year, ".gpkg"))
  }
}
