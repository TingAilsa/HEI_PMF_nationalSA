# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran

library(ncdf4)
library(ggrepel)
library(base)
library(dplyr)
library(tidyr)
library(data.table)
library(doParallel)
library(sf)
library(raster)
library(terra)
library(fst)
library(gstat)

# Detect the number of cores available on the server
num_cores <- detectCores() - 1  # Use all but one core
num_cores

# Create a parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Working path
setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data")

# setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds")
getwd()

dir_path = getwd()

#### Functions ####
# Function to process one sf file to grid, cannot make sure there is no NA for each grid
# project_sf_to_grid <- function(sf_file, us_raster_file) {
# 
#   # Get non-geometry column names
#   vars <- setdiff(names(sf_file), "geometry")
#   sprintf(vars)
# 
#   # Process each variable using the same template, and save in the list
#   projected_results <- list()
#   
#   for(var in vars) {
#     message(sprintf("  Processing variable: %s", var))
#     
#     tryCatch({
#       # Determine the variable type
#       var_class = class(sf_file[[var]])
#       message(sprintf("    Variable class: %s", var_class))
#       
#       # Set rasterize function based on variable type
#       if(var_class == "numeric") {
#         raster_fun = "mean"
#         project_method = "bilinear"
#       } else {
#         # For categorical variables, use mode (most frequent value)
#         raster_fun = function(x, ...) {
#           ux <- unique(x)
#           ux[which.max(tabulate(match(x, ux)))]
#         }
#         project_method = "ngb"
#       }
#       
#       # Create a raster for the input sf points
#       points_raster <- rasterize(
#         sf_file, 
#         us_raster_file,  # use target raster as template
#         field = var,     # use current variable
#         fun = raster_fun, # use appropriate function
#         method = project_method
#       )
#       # head(points_raster); dim(points_raster)
#       
#       # Get coordinates from target raster
#       coords <- xyFromCell(points_raster, 1:ncell(points_raster))
#       coords_df <- data.frame(
#         Longitude = coords[,1],
#         Latitude = coords[,2]
#       )
#       
#       # Extract values
#       var_values <- extract(points_raster, 
#                             coords_df[, c("Longitude", "Latitude")])
#       
#       # Create dataframe for this variable
#       var_df <- coords_df
#       var_df[[var]] <- var_values
#       
#       # Check for NAs and report
#       na_count <- sum(is.na(var_df[[var]]))
#       if(na_count > 0) {
#         message(sprintf("    Warning: %d NAs found in %s", na_count, var))
#       }
#       
#       # Store result
#       projected_results[[var]] <- var_df
#       
#       
#     }, error = function(e) {
#       warning(sprintf("Error processing %s: %s", var, e$message))
#       return(NULL)
#     })
#   }
#   
#   # Merge all variables by longitude and latitude
#   merged_df <- Reduce(function(x, y) {
#     merge(x, y, by = c("Longitude", "Latitude"), all = TRUE)
#   }, projected_results)
#   
#   # Check for NAs
#   na_summary <- colSums(is.na(merged_df))
#   if(any(na_summary > 0)) {
#     message("\nNA summary:")
#     print(na_summary)
#   }
# 
#   return(merged_df)
# }

# Project, with interpolations, based on sf files
project_sf_to_grid <- function(sf_file, us_sf_file, interpolation_method) {
  require(gstat)
  require(sp)
  
  # Get non-geometry column names
  vars <- setdiff(names(sf_file), "geometry")
  
  # Get target grid coordinates directly from us_sf_file
  grid_coords <- st_coordinates(us_sf_file)
  coords_df <- data.frame(
    Longitude = grid_coords[,1],
    Latitude = grid_coords[,2]
  )
  grid_sp <- SpatialPoints(coords_df, proj4string = crs(us_sf_file))
  
  # Process each variable
  projected_results <- list()
  
  for(var in vars) {
    message(sprintf("Processing variable: %s", var))
    
    tryCatch({
      # Convert source sf points to SpatialPointsDataFrame
      sf_coords <- st_coordinates(sf_file)
      points_sp <- SpatialPointsDataFrame(
        coords = sf_coords,
        data = data.frame(value = sf_file[[var]]),
        proj4string = crs(sf_file)
      )
      
      # Apply specified interpolation method
      if(interpolation_method == "idw") {
        # IDW interpolation
        idw <- gstat::idw(value ~ 1, points_sp, grid_sp)
        var_values <- idw$var1.pred
        
      } else if(interpolation_method == "nn") {
        # Nearest neighbor
        nn <- get.knnx(coordinates(points_sp), coordinates(grid_sp), k = 1)
        var_values <- sf_file[[var]][nn$nn.index]
        
      } else if(interpolation_method == "krige") {
        # Simple kriging
        auto_vgm <- autofitVariogram(value ~ 1, points_sp)
        krige_result <- krige(value ~ 1, points_sp, grid_sp, 
                              model = auto_vgm$var_model)
        var_values <- krige_result$var1.pred
      }
      
      # Create and store result
      var_df <- coords_df
      var_df[[var]] <- var_values
      projected_results[[var]] <- var_df
      
    }, error = function(e) {
      warning(sprintf("Error processing %s: %s", var, e$message))
      return(NULL)
    })
  }
  
  # Merge results
  merged_df <- Reduce(function(x, y) {
    merge(x, y, by = c("Longitude", "Latitude"), all = TRUE)
  }, projected_results)
  
  return(merged_df)
}


#### Create overall 10km*10km / 1km*1km grid-like data #### 
# Define the approximate bounding box for mainland U.S.
us_bbox <- c(xmin = -125, xmax = -66, ymin = 24, ymax = 50)
crs_proj <- "+proj=longlat +datum=WGS84 +no_defs"

# Convert the bounding box to an extent object for the raster package
us_extent <- extent(us_bbox["xmin"], us_bbox["xmax"], us_bbox["ymin"], us_bbox["ymax"])

# Create the raster grid with a desired resolution (e.g., 0.1 degrees)
us_grid_raster_01 <- raster(ext = us_extent, resolution = 0.1)
crs(us_grid_raster_01) <- crs_proj
dim(us_grid_raster_01)

us_grid_raster_001 <- raster(ext = us_extent, resolution = 0.01)
crs(us_grid_raster_001) <- crs_proj
dim(us_grid_raster_001)

# convert the grid to an sf object
us_grid_sf_01 <- st_as_sf(as(us_grid_raster_01, "SpatialPolygons"))
st_crs(us_grid_sf_01) <- crs_proj

us_grid_sf_001 <- st_as_sf(as(us_grid_raster_001, "SpatialPolygons"))
st_crs(us_grid_sf_01) <- crs_proj

# Save the spatial files for CMAQ complete grid

writeRaster(us_grid_raster_01,
            file.path("base_raster_grid_sf/us_grid_raster_01.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(us_grid_raster_001,
            file.path("base_raster_grid_sf/us_grid_raster_001.tif"), format = "GTiff", overwrite = TRUE)

st_write(us_grid_sf_01,
            file.path("base_raster_grid_sf/us_grid_sf_01.fgb"))
st_write(us_grid_sf_001,
         file.path("base_raster_grid_sf/us_grid_sf_001.fgb"))

# Convert multipolygon data to centroids for faster approximate matching
us_grid_centroids_01 <- st_centroid(us_grid_sf_01)
us_grid_centroids_001 <- st_centroid(us_grid_sf_001)

st_write(us_grid_centroids_01,
         file.path("base_raster_grid_sf/us_grid_centroids_01.fgb"))
st_write(us_grid_centroids_001,
         file.path("base_raster_grid_sf/us_grid_centroids_001.fgb"))

# us_grid_raster_01 = raster(file.path("base_raster_grid_sf/us_grid_raster_01.tif"))
# us_grid_raster_001 = raster(file.path("base_raster_grid_sf/us_grid_raster_001.tif"))
 
# us_grid_sf_01 = st_read(file.path("base_raster_grid_sf/us_grid_sf_01.fgb"))
# us_grid_sf_001 = st_read(file.path("base_raster_grid_sf/us_grid_sf_001.fgb"))
# 
# us_grid_centroids_01 = st_read(file.path("base_raster_grid_sf/us_grid_centroids_01.fgb"))
# us_grid_centroids_001 = st_read(file.path("base_raster_grid_sf/us_grid_centroids_001.fgb"))


#### PMF, CSN & IMPROVE points ####
# read file and selected columns to use
csn_imp_site = read.csv("pmf_ncld_meteo_census/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
csn_imp_site = dplyr::select(csn_imp_site, Dataset, SiteCode, Longitude, Latitude)

# Remove duplicated site
csn_imp_site$dup = duplicated(csn_imp_site$SiteCode)
csn_imp_site = subset(csn_imp_site, dup == FALSE); csn_imp_site$dup = NULL

csn_imp_site = csn_imp_site[with(csn_imp_site, order(Dataset, SiteCode)), ]
nrow(csn_imp_site) == length(unique(csn_imp_site$SiteCode))

# Add info for later matching with concentrations
csn_imp_site$org_long = csn_imp_site$Longitude
csn_imp_site$org_lat = csn_imp_site$Latitude

# Create sf file
csn_imp_site_sf = st_as_sf(csn_imp_site,
                           coords = c("Longitude", "Latitude"),
                           crs = 4326) # "WGS84"

# Detect the closet points in US_grid for CSN & IMPROVE sites
st_crs(csn_imp_site_sf) == st_crs(us_grid_centroids_01)
nearest_csn_imp_in_USgrid <- st_nearest_feature(csn_imp_site_sf, us_grid_centroids_01)

# Extract the corresponding nearest points from us_grid_centroids
nearest_points_csn_imp_in_USgrid <- us_grid_centroids_01[nearest_csn_imp_in_USgrid, ]

# Combine the csn_imp_site_sf with the nearest points information
csn_imp_site_with_nearest <-
  cbind(st_drop_geometry(csn_imp_site_sf),
        nearest_points_csn_imp_in_USgrid)

# Get coordinates & Rename
csn_imp_site_with_nearest <-
  cbind(csn_imp_site_with_nearest,
        st_coordinates(nearest_points_csn_imp_in_USgrid))

ncol_csn_imp = ncol(csn_imp_site_with_nearest)
names(csn_imp_site_with_nearest)[(ncol_csn_imp-1):ncol_csn_imp] =
  c("Longitude", "Latitude")
head(csn_imp_site_with_nearest)

# Output file
st_write(csn_imp_site_with_nearest,
         file.path("base_raster_grid_sf/PMF_sites_in_US_grid_01.fgb"))

# csn_imp_site_with_nearest = 
#   st_read( file.path("base_raster_grid_sf/PMF_sites_in_US_grid_01.fgb"))

#### GRIDMET ####
gridmet = read_fst("pmf_ncld_meteo_census/GRIDMET_commom_2011.fst")
head(gridmet); dim(gridmet)

# Extract the coordinate info
met_geo_extract = subset(gridmet, 
                         Date %in% c("2011-01-01", "2011-04-01", "2011-07-01", "2011-10-01"))
met_geo_extract = dplyr::select(met_geo_extract, x, y)
dim(met_geo); head(met_geo)

# Exclude duplicates
met_geo_extract = met_geo[!duplicated(met_geo), ]
met_geo_extract$Longitude = met_geo_extract$x
met_geo_extract$Latitude = met_geo_extract$y
dim(met_geo_extract); head(met_geo_extract)

# Set as sf
met_geo_sf = st_as_sf(met_geo_extract, 
                      coords = c("Longitude", "Latitude"), 
                      crs = 4326) # "WGS84"

# Detect the closet points in US_grid for CSN & IMPROVE sites
st_crs(met_geo_sf) == st_crs(us_grid_centroids_01)
nearest_met_geo_in_USgrid <- st_nearest_feature(met_geo_sf, us_grid_centroids_01)

# Extract the corresponding nearest points from us_grid_centroids
nearest_points_met_geo_in_USgrid <- us_grid_centroids_01[nearest_met_geo_in_USgrid, ]

# Combine the met_geo_sf with the nearest points information
met_geo_with_nearest <- 
  cbind(st_drop_geometry(met_geo_sf), 
        nearest_points_met_geo_in_USgrid)

# Get coordinates & Rename
met_geo_with_nearest <-
  cbind(met_geo_with_nearest,
        st_coordinates(nearest_points_met_geo_in_USgrid))

ncol_csn_imp = ncol(met_geo_with_nearest)
names(met_geo_with_nearest)[(ncol_csn_imp-1):ncol_csn_imp] = 
  c("Longitude", "Latitude")
head(met_geo_with_nearest); dim(met_geo_with_nearest)

met_geo_with_nearest$Longitude = round(met_geo_with_nearest$Longitude, 2)
met_geo_with_nearest$Latitude = round(met_geo_with_nearest$Latitude, 2)

met_geo_with_nearest$long = met_geo_with_nearest$Longitude
met_geo_with_nearest$lat = met_geo_with_nearest$Latitude
met_geo_with_nearest_sf = st_as_sf(met_geo_with_nearest, 
                                   coords = c("long", "lat"), 
                                   crs = 4326) # "WGS84"

# Output file
st_write(met_geo_with_nearest_sf, 
         file.path("base_raster_grid_sf/GRIDMET_in_US_grid_01.fgb"))

# Reestimate the GRIDMET values for each US grids
gridmet$Date = as.Date(gridmet$Date)
setDT(gridmet)
write_fst(gridmet, file.path("pmf_ncld_meteo_census/GRIDMET_commom_2011_Date.fst"))

# Drop the geometry for merge
met_geo_US_nearest = as.data.table(st_drop_geometry(met_geo_with_nearest_sf))
# summary(met_geo_US_nearest$x %in% gridmet$x & met_geo_US_nearest$y %in% gridmet$y)
# summary(gridmet$x %in% met_geo_US_nearest$x)
# summary(gridmet$y %in% met_geo_US_nearest$y)

# Merging via original coords
gridmet_us = merge(gridmet, 
                   met_geo_US_nearest, 
                   by = c("x", "y"))
dim(gridmet); dim(met_geo_US_nearest); dim(gridmet_us)
head(gridmet_us)

# Get the averages
gridmet_us_grid_mean =
  dplyr::select(gridmet_us, -x, -y) %>%
  group_by(Date, Longitude, Latitude) %>%
  summarise( 
    tmmx = mean(tmmx), 
    tmmn = mean(tmmn), 
    rmax = mean(rmax), 
    rmin = mean(rmin), 
    vs = mean(vs), 
    th = mean(th),
    .groups = "drop")

# Set <0 to 0
gridmet_us_grid_mean <- 
  gridmet_us_grid_mean %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::where(is.numeric), 
      ~ ifelse(. < 0, 0, .)))

head(gridmet_us_grid_mean)

write_fst(gridmet_us_grid_mean, file.path("pmf_ncld_meteo_census/GRIDMET_commom_2011_in_US_grid_01.fst"))

#### Roadiness ####
# us_grid_sf_01

us_grid_centroids_01 = st_read(file.path("/Users/TingZhang/Documents/HEI HAQ PMF/HEI_PMF_nationalSA/us_grid_centroids_01.fgb"))
class(us_grid_centroids_01); head(us_grid_centroids_01)

roadiness = read_fst("pmf_ncld_meteo_census/us_roadiness_longlat.fst")
# roadiness = read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Roadiness/us_roadiness_longlat.fst")

# Set as sf
roadiness_sf = st_as_sf(roadiness, 
                      coords = c("Longitude", "Latitude"), 
                      crs = 4326) # "WGS84"
head(roadiness_sf)
# Rename
names(roadiness_sf) = 
  c("sRoadLength", "sRoadiness_1", "sRoadiness_1.5", "sRoadiness_2",
    "sRoadCount", "sRoadCount_1", "sRoadCount_1.5", "sRoadCount_2",
    "VMT_vs_area", "geometry")

summary(roadiness_sf)

### IDW interpolation, inverse distance weighted 
# Calculate distances and weights
roadiness_distances <- st_distance(us_grid_centroids_01, roadiness_sf)
roadiness_weights <- 1/roadiness_distances
roadiness_weights[is.infinite(roadiness_weights)] <- 1

# Normalize roadiness_weights
roadiness_weights <- roadiness_weights/rowSums(roadiness_weights)

# Project variables using IDW
grid_roadiness <- us_grid_centroids_01 %>%
  mutate(
    sRoadLength = as.numeric(roadiness_weights %*% roadiness_sf$sRoadLength),
    sRoadiness_1 = as.numeric(roadiness_weights %*% roadiness_sf$sRoadiness_1),
    sRoadiness_1.5 = as.numeric(roadiness_weights %*% roadiness_sf$sRoadiness_1.5),
    sRoadiness_2 = as.numeric(roadiness_weights %*% roadiness_sf$sRoadiness_2),
    sRoadCount = as.numeric(roadiness_weights %*% roadiness_sf$sRoadCount),
    sRoadCount_1 = as.numeric(roadiness_weights %*% roadiness_sf$sRoadCount_1),
    sRoadCount_1.5 = as.numeric(roadiness_weights %*% roadiness_sf$sRoadCount_1.5),
    sRoadCount_2 = as.numeric(roadiness_weights %*% roadiness_sf$sRoadCount_2),
    VMT_vs_area = as.numeric(roadiness_weights %*% roadiness_sf$VMT_vs_area)
  )


# Nearest point interpolation
nearest_points <- st_nearest_feature(us_grid_centroids_01, roadiness_sf)

# Project variables using nearest neighbor interpolation
grid_roadiness <- us_grid_centroids_01 %>%
  mutate(
    sRoadLength = roadiness_sf$sRoadLength[nearest_points],
    sRoadiness_1 = roadiness_sf$sRoadiness_1[nearest_points],
    sRoadiness_1.5 = roadiness_sf$sRoadiness_1.5[nearest_points],
    sRoadiness_2 = roadiness_sf$sRoadiness_2[nearest_points],
    sRoadCount = roadiness_sf$sRoadCount[nearest_points],
    sRoadCount_1 = roadiness_sf$sRoadCount_1[nearest_points],
    sRoadCount_1.5 = roadiness_sf$sRoadCount_1.5[nearest_points],
    sRoadCount_2 = roadiness_sf$sRoadCount_2[nearest_points],
    VMT_vs_area = roadiness_sf$VMT_vs_area[nearest_points]
  )

dim(grid_roadiness); summary(grid_roadiness)
head(grid_roadiness)

# Get long and lat, and Round to 2 digits
road_coords <- st_coordinates(grid_roadiness)
Longitude <- round(road_coords[,1], 2)
Latitude <- round(road_coords[,2], 2)

grid_roadiness$Longitude = Longitude
grid_roadiness$Latitude = Latitude


# Get the mean values
roadiness_us_grid_mean =
  st_drop_geometry(grid_roadiness) %>%
  group_by(Longitude, Latitude) %>%
  summarise( 
    sRoadLength = mean(sRoadLength), 
    sRoadiness_1 = mean(sRoadiness_1), 
    sRoadiness_1.5 = mean(sRoadiness_1.5), 
    sRoadiness_2 = mean(sRoadiness_2), 
    sRoadCount = mean(sRoadCount), 
    sRoadCount_1 = mean(sRoadCount_1), 
    sRoadCount_1.5 = mean(sRoadCount_1.5), 
    sRoadCount_2 = mean(sRoadCount_2), 
    VMT_vs_area = mean(VMT_vs_area), 
    .groups = "drop")
dim(roadiness_us_grid_mean); dim(roadiness_us_grid_mean)

# # Get new geometry
# roadiness_us_grid_mean$long = roadiness_us_grid_mean$Longitude
# roadiness_us_grid_mean$lat = roadiness_us_grid_mean$Latitude
# 
# roadiness_with_nearest_sf = st_as_sf(roadiness_us_grid_mean, 
#                                      coords = c("long", "lat"), 
#                                      crs = 4326) # "WGS84"
# 
# Output file
write_fst(roadiness_us_grid_mean,
         file.path("grid_roadiness/Roadiness_in_US_grid_01.fst"))

# write_fst(roadiness_us_grid_mean, "Roadiness/Roadiness_in_US_grid_01.fst")


#### Census ####
# # County tract GEOID & geometry
# census_tract_geo = st_read("pmf_ncld_meteo_census/ACS_census_tract_geoid_geometry_4326.fgb")
# st_geometry(census_tract_geo) <- "geometry"
# head(census_tract_geo)
# 
# # County tract level census for Aim3
# census_acs_wide = read_fst("pmf_ncld_meteo_census/US_Census_ACS_tract_commute_2011-2020.fst")
# 
# # Spatial join to US grid
# census_tract_in_USgrid =
#   st_join(us_grid_sf_01, census_tract_geo, join = st_intersects)

# census_tract_geo = st_read("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/ACS_census_tract_geoid_geometry_4326.fgb")
# # Exclude GEOID outside of the mainland US
# census_acs_geo_mainland <- 
#   census_tract_geo %>%
#   st_crop(xmin = -124.848974, xmax = -66.885444, 
#           ymin = 24.396308, ymax = 49.384358)
# length(unique(census_acs_geo_mainland$GEOID))
# length(unique(census_tract_geo$GEOID))
# st_bbox(census_acs_geo_mainland)
# 
# st_write(census_acs_geo_mainland,
#          file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/ACS_census_tract_geoid_geometry_4326_USmainland.fgb"))

census_acs_geo_mainland =
  st_read("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/ACS_census_tract_geoid_geometry_4326_USmainland.fgb")
length(unique(census_acs_geo_mainland$GEOID))

census_acs_wide = read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_Census_ACS_tract_commute_2011-2020.fst")

us_grid_centroids_01 = st_read(file.path("/Users/TingZhang/Documents/HEI HAQ PMF/HEI_PMF_nationalSA/us_grid_centroids_01.fgb"))
class(us_grid_centroids_01); head(us_grid_centroids_01)

subset(census_acs_wide, is.na(car_truck_van))
census_acs_wide = na.omit(census_acs_wide)
head(census_acs_wide); dim(census_acs_wide); summary(census_acs_wide)

st_geometry(census_acs_geo_mainland) <- "geometry"
head(census_acs_geo_mainland)
subset(census_acs_geo_mainland, st_is_empty(geometry)) # check if there is empty geometry

# file to save GEOID-geometry-year match for 2011-2020
grid_cTract_final_all = NULL
grid_cTract_census_all = NULL

for (census_year in 2011:2020){ # census_year = 2011
  
  ###### Prepare basic data ###### 
  
  # Prepare census commute data for each year, and get geometry
  census_acs_wide_year = subset(census_acs_wide, year == census_year)
  census_acs_geo_year = 
    merge(census_acs_wide_year, census_acs_geo_mainland, all.x = TRUE) %>%
    dplyr::select(GEOID, geometry)
  
  # Set as sf
  census_acs_geo_year = st_as_sf(census_acs_geo_year)
  head(census_acs_geo_year); summary(census_acs_geo_year); dim(census_acs_geo_year)
  
  # Check if there is empty geometry
  census_acs_geo_year_emptyGeo = subset(census_acs_geo_year, st_is_empty(geometry))
  nrow(census_acs_geo_year_emptyGeo); head(census_acs_geo_year_emptyGeo)
  
  # Remove GEOID with empty geomety (GEOID without a geometry to match, probably outside of the mainland US)
  census_acs_geo_year <- 
    census_acs_geo_year %>%
    filter(!st_is_empty(geometry))
  
  census_acs_use_year <- 
    subset(census_acs_wide_year, 
           GEOID %in% unique(census_acs_geo_year$GEOID))
  
  # # Checking the boundaries
  # st_bbox(us_grid_centroids_01)
  # st_bbox(census_acs_geo_year)

  ###### Match GEOID in census with geometry in US_grids  ###### 
  
  ### Keep all GEOID and all grids (try the best)
  # Track counts for validation
  n_orig_tracts <- length(unique(census_acs_geo_year$GEOID))
  n_orig_grids <- length(unique(us_grid_centroids_01$geometry))
  # n_orig_tracts; n_orig_grids
  
  # Get base grid-tract matches from spatial join
  base_matches <- 
    st_join(us_grid_centroids_01, census_acs_geo_year, join = st_within) 
  # head(base_matches); dim(base_matches)
  # length(unique(base_matches$GEOID)); length(unique(base_matches$geometry))
  
  # Get unmatched grids and assign nearest tract
  unmatched_grids <- 
    base_matches %>%
    filter(is.na(GEOID)) %>%
    mutate(
      GEOID = census_acs_geo_year$GEOID[st_nearest_feature(geometry, census_acs_geo_year)]
    )
  # head(unmatched_grids); dim(unmatched_grids)
  # length(unique(unmatched_grids$GEOID)); length(unique(unmatched_grids$geometry))
  
  # Get unmatched tracts and assign nearest grid
  unmatched_tracts <- 
    census_acs_geo_year %>%
    filter(!GEOID %in% base_matches$GEOID) %>%
    mutate(
      near_grid = st_nearest_feature(st_centroid(geometry), us_grid_centroids_01)
    ) %>%
    st_drop_geometry() %>%
    mutate(
      geometry = us_grid_centroids_01$geometry[near_grid]
    ) %>%
    dplyr::select(-near_grid)
  # head(unmatched_tracts); dim(unmatched_tracts)
  # length(unique(unmatched_tracts$GEOID)); length(unique(unmatched_tracts$geometry))
  
  # Combine all
  grid_with_censusTract <- bind_rows(
    base_matches %>% filter(!is.na(GEOID)),
    unmatched_grids,
    unmatched_tracts
  )
  
  # head(grid_with_censusTract)
  # # summary(grid_with_censusTract)
  # length(unique(grid_with_censusTract$GEOID))
  # length(unique(census_acs_geo_year$GEOID))
  # length(unique(grid_with_censusTract$geometry))
  # length(unique(us_grid_centroids_01$geometry))
  # nrow(census_acs_geo_year); nrow(us_grid_centroids_01); nrow(grid_with_censusTract) 
  # 
  # nrow(subset(grid_with_censusTract, is.na(GEOID)))
  # nrow(subset(grid_with_censusTract, is.na(geometry)))
  # subset(grid_with_censusTract, !(geometry %in% us_grid_centroids_01$geometry))
  
  # Check if there is still GEOID with empty geometry (there should be NO!!!)
  geoid_empty_geo = 
    unique(
      subset(grid_with_censusTract, 
             !(geometry %in% us_grid_centroids_01$geometry))$GEOID)
  head(geoid_empty_geo); length(geoid_empty_geo)
  
  # geoid_with_geo =
  #   unique(
  #     subset(grid_with_censusTract, 
  #            geometry %in% us_grid_centroids_01$geometry)$GEOID)
  # head(geoid_with_geo); length(geoid_with_geo)
  
  # # Check if there any GEOID with empty geometry included in that with some other grids assigned
  # # If TRUE, then not, only need to find grid geometry for geoid_empty_geo
  # length(geoid_with_geo) + length(geoid_empty_geo) == length(unique(census_acs_geo_year$GEOID))
  # geoid_empty_geo = geoid_empty_geo[!(geoid_empty_geo %in% geoid_with_geo)]
  # census_acs_year_emptyGeo = subset(census_acs_geo_year, GEOID %in% geoid_empty_geo)
  
  #### Handle the cases that multiple grids assigned to one GEOID; again need to keep all GEOID and grids
  # Estimate the appearance frequency of each GEOID and geometry
  grid_with_censusTract <- 
    grid_with_censusTract %>%
    group_by(GEOID) %>%
    mutate(geoid_freq = n()) %>%
    ungroup() %>%
    group_by(geometry) %>%
    mutate(geom_freq = n()) %>%
    ungroup()
  length(unique(grid_with_censusTract$GEOID)) # 70,208
  length(unique(census_acs_geo_year$GEOID)) # 70,208
  length(unique(grid_with_censusTract$geometry)) # 153,400
  length(unique(us_grid_centroids_01$geometry)) # 153,400
  
  # # Get long lat
  # grid_with_censusTract$Longitude = round(st_coordinates(grid_with_censusTract)[, 1], 2)
  # grid_with_censusTract$Latitude = round(st_coordinates(grid_with_censusTract)[, 2], 2)
  # grid_with_censusTract$coords = paste(grid_with_censusTract$Longitude, grid_with_censusTract$Latitude)
  
  # Identify the GEOID that only have one match, file for final use directly
  grid_cTract_oneGEOID <-
    subset(grid_with_censusTract, 
           geoid_freq == 1)
  nrow(grid_cTract_oneGEOID); length(unique(grid_cTract_oneGEOID$geometry))
  # 59,551,  11,821
  
  # Identify the geometry that only have one match, but multiple GEIOD, need to handle GEOID
  grid_cTract_oneGrid <-
    subset(grid_with_censusTract, 
           geoid_freq > 1 & geom_freq == 1)
  nrow(grid_cTract_oneGrid); length(unique(grid_cTract_oneGrid$GEOID))
  # 141,439,  10,306
  
  # Combine above two files
  grid_cTract_1to1 =
    rbind(grid_cTract_oneGEOID, grid_cTract_oneGrid)
  length(unique(grid_cTract_1to1$geometry)); length(unique(grid_cTract_1to1$GEOID))
  # 153,260,  69,857
  summary(grid_cTract_1to1$geoid_freq == 1 | grid_cTract_1to1$geom_freq == 1)
  nrow(grid_cTract_1to1) # 200,990
  
 # Identify the GEOID or geometry with >1 matches for both
  grid_cTract_nMatch <-
    subset(grid_with_censusTract,
           geoid_freq > 1 & geom_freq > 1) 
  nrow(grid_cTract_nMatch)
  length(unique(grid_cTract_nMatch$geometry)); length(unique(grid_cTract_nMatch$GEOID))
  summary(grid_cTract_nMatch)
  
  ### Handle those multi matches
  ## Situation 1, both geoID & gEOmetry appear in 1to1, not to use
  grid_cTract_nMatch_gIDgEO_in_1to1 <-
    subset(grid_cTract_nMatch, 
           GEOID %in% grid_cTract_1to1$GEOID & geometry %in% grid_cTract_1to1$geometry)
  nrow(grid_cTract_nMatch_gIDgEO_in_1to1)
  
  ## Situation 2, geoid not but gEOmetry appear in 1to1, detect any geometry for each GEOID
  grid_cTract_nMatch_gEO_in_1to1 <-
    subset(grid_cTract_nMatch, 
           !(GEOID %in% grid_cTract_1to1$GEOID) & geometry %in% grid_cTract_1to1$geometry)
  nrow(grid_cTract_nMatch_gEO_in_1to1)
  
  grid_cTract_nMatch_gEO_in_1to1_single <-
    grid_cTract_nMatch_gEO_in_1to1 %>%
    group_by(GEOID) %>%
    slice(1)
  nrow(grid_cTract_nMatch_gEO_in_1to1_single)
  
  ## Situation 3, geometry not but geoID appear in 1to1, detect any GEOID for each geometry
  grid_cTract_nMatch_gID_in_1to1 <-
    subset(grid_cTract_nMatch, 
           GEOID %in% grid_cTract_1to1$GEOID & !(geometry %in% grid_cTract_1to1$geometry))
  nrow(grid_cTract_nMatch_gID_in_1to1)
  
  grid_cTract_nMatch_gID_in_1to1_single <-
    grid_cTract_nMatch_gID_in_1to1 %>%
    group_by(geometry) %>%
    slice(1)
  nrow(grid_cTract_nMatch_gID_in_1to1_single)
  
  ## Situation 4, neither geometry nor geoid appear in 1to1,  detect one geometry for each GEOID
  grid_cTract_nMatch_neither_in_1to1 <-
    subset(grid_cTract_nMatch, 
           !(GEOID %in% grid_cTract_1to1$GEOID) &
             !(geometry %in% grid_cTract_1to1$geometry))
  nrow(grid_cTract_nMatch_neither_in_1to1)
  
  grid_cTract_nMatch_neither_in_1to1 <-
    grid_cTract_nMatch_neither_in_1to1 %>%
    subset(!(GEOID %in% grid_cTract_nMatch_gEO_in_1to1_single$GEOID) &
             !(GEOID %in% grid_cTract_nMatch_gID_in_1to1_single$GEOID))
  nrow(grid_cTract_nMatch_neither_in_1to1)
  
  # Get all geometry that are not used for GEOID matching 
  rest_geometry = 
    unique(
      subset(grid_with_censusTract, 
             !(geometry %in% grid_cTract_1to1$geometry | 
               geometry %in% grid_cTract_nMatch_gID_in_1to1$geometry))$geometry)
  length(rest_geometry)
  
  # For each GEOID, detect one geometry
  # Choose the geometry that is not in rest_geometry, if possible
  grid_cTract_nMatch_neither_in_1to1_single <-
    grid_cTract_nMatch_neither_in_1to1 %>%
    group_by(GEOID) %>%
    mutate(
      # Checks if geometry already exists in grid_cTract_oneGEOID_oneGrid
      in_oneMatch = geometry %in% rest_geometry,
      # Prioritize the geometry not in in_oneMatch
      priority = if(any(!in_oneMatch)) !in_oneMatch else TRUE
    ) %>%
    filter(priority) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-in_oneMatch, -priority)
  nrow(grid_cTract_nMatch_neither_in_1to1_single)
  
  ## Combine all matches
  grid_cTract_final =
    rbind(grid_cTract_1to1,
          grid_cTract_nMatch_gEO_in_1to1_single,
          grid_cTract_nMatch_gID_in_1to1_single,
          grid_cTract_nMatch_neither_in_1to1_single)

  length(unique(grid_cTract_final$GEOID))
  length(unique(census_acs_geo_year$GEOID))
  length(unique(grid_cTract_final$geometry))
  length(unique(us_grid_centroids_01$geometry))
  head(grid_cTract_final); dim(grid_cTract_final)
  summary(is.na(grid_cTract_final$GEOID))
  summary(st_is_empty(grid_cTract_final$geometry))
  

  # Add long & lat, and round to 2 digits
  grid_cTract_final$Longitude = round(st_coordinates(grid_cTract_final)[,1], 2)
  grid_cTract_final$Latitude = round(st_coordinates(grid_cTract_final)[,2], 2)
  
  # Exclude and add columns
  grid_cTract_final$geoid_freq = grid_cTract_final$geom_freq = NULL
  grid_cTract_final$year = census_year
  grid_cTract_final = st_drop_geometry(grid_cTract_final)
  
  # Combine into final file for all years and output it
  grid_cTract_final_all = rbind(grid_cTract_final_all, grid_cTract_final)
  write_fst(grid_cTract_final_all, 
            ## "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_census_tract_grid_01_4326_2011.fst"
            "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_census_tract_grid_01_4326_2011-20.fst"
            )

  ###### Match census commute info with coordinates from US grid ###### 
  census_acs_use_year_coords =
    merge(census_acs_use_year, grid_cTract_final, by = c("GEOID", "year"))
  dim(census_acs_use_year_coords); dim(census_acs_use_year)
  head(census_acs_use_year_coords)
  
  # Calculate median for all columns
  census_acs_year_coords_avg =
    dplyr::select(census_acs_use_year_coords, -GEOID) %>%
    group_by(Longitude, Latitude) %>%
    summarize(across(everything(), \(x) median(x)),
              .groups = "drop") 
  head(census_acs_year_coords_avg); dim(census_acs_year_coords_avg)
  summary(census_acs_year_coords_avg)
  # unique combination counts of long, lat
  census_acs_year_coords_avg %>%
    summarize(unique_combinations = n_distinct(paste(Longitude, Latitude)))
  
  # Combine into final file for all years and output it
  grid_cTract_census_all = rbind(grid_cTract_census_all, census_acs_year_coords_avg)
  write_fst(grid_cTract_census_all, 
            ## "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_census_tract_grid_01_Commute_2011.fst"
            "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_census_tract_grid_01_Commute_2011-20.fst"
            )
}



