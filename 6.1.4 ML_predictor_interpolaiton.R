# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran

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
library(ggrepel)
library(gstat)
library(exactextractr)
library(doParallel)


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

# #### GRIDMET, change to use projectRaster from the original combination ####
# gridmet = read_fst("pmf_ncld_meteo_census/GRIDMET_commom_2011.fst")
# head(gridmet); dim(gridmet)
# 
# # Extract the coordinate info
# met_geo_extract = subset(gridmet, 
#                          Date %in% c("2011-01-01", "2011-04-01", "2011-07-01", "2011-10-01"))
# met_geo_extract = dplyr::select(met_geo_extract, x, y)
# dim(met_geo); head(met_geo)
# 
# # Exclude duplicates
# met_geo_extract = met_geo[!duplicated(met_geo), ]
# met_geo_extract$Longitude = met_geo_extract$x
# met_geo_extract$Latitude = met_geo_extract$y
# dim(met_geo_extract); head(met_geo_extract)
# 
# # Set as sf
# met_geo_sf = st_as_sf(met_geo_extract, 
#                       coords = c("Longitude", "Latitude"), 
#                       crs = 4326) # "WGS84"
# 
# # Detect the closet points in US_grid for CSN & IMPROVE sites
# st_crs(met_geo_sf) == st_crs(us_grid_centroids_01)
# nearest_met_geo_in_USgrid <- st_nearest_feature(met_geo_sf, us_grid_centroids_01)
# 
# # Extract the corresponding nearest points from us_grid_centroids
# nearest_points_met_geo_in_USgrid <- us_grid_centroids_01[nearest_met_geo_in_USgrid, ]
# 
# # Combine the met_geo_sf with the nearest points information
# met_geo_with_nearest <- 
#   cbind(st_drop_geometry(met_geo_sf), 
#         nearest_points_met_geo_in_USgrid)
# 
# # Get coordinates & Rename
# met_geo_with_nearest <-
#   cbind(met_geo_with_nearest,
#         st_coordinates(nearest_points_met_geo_in_USgrid))
# 
# ncol_csn_imp = ncol(met_geo_with_nearest)
# names(met_geo_with_nearest)[(ncol_csn_imp-1):ncol_csn_imp] = 
#   c("Longitude", "Latitude")
# head(met_geo_with_nearest); dim(met_geo_with_nearest)
# 
# met_geo_with_nearest$Longitude = round(met_geo_with_nearest$Longitude, 2)
# met_geo_with_nearest$Latitude = round(met_geo_with_nearest$Latitude, 2)
# 
# met_geo_with_nearest$long = met_geo_with_nearest$Longitude
# met_geo_with_nearest$lat = met_geo_with_nearest$Latitude
# met_geo_with_nearest_sf = st_as_sf(met_geo_with_nearest, 
#                                    coords = c("long", "lat"), 
#                                    crs = 4326) # "WGS84"
# 
# # Output file
# st_write(met_geo_with_nearest_sf, 
#          file.path("base_raster_grid_sf/GRIDMET_in_US_grid_01.fgb"))
# 
# # Reestimate the GRIDMET values for each US grids
# gridmet$Date = as.Date(gridmet$Date)
# setDT(gridmet)
# write_fst(gridmet, file.path("pmf_ncld_meteo_census/GRIDMET_commom_2011_Date.fst"))
# 
# # Drop the geometry for merge
# met_geo_US_nearest = as.data.table(st_drop_geometry(met_geo_with_nearest_sf))
# # summary(met_geo_US_nearest$x %in% gridmet$x & met_geo_US_nearest$y %in% gridmet$y)
# # summary(gridmet$x %in% met_geo_US_nearest$x)
# # summary(gridmet$y %in% met_geo_US_nearest$y)
# 
# # Merging via original coords
# gridmet_us = merge(gridmet, 
#                    met_geo_US_nearest, 
#                    by = c("x", "y"))
# dim(gridmet); dim(met_geo_US_nearest); dim(gridmet_us)
# head(gridmet_us)
# 
# # Get the averages
# gridmet_us_grid_mean =
#   dplyr::select(gridmet_us, -x, -y) %>%
#   group_by(Date, Longitude, Latitude) %>%
#   summarise( 
#     tmmx = mean(tmmx), 
#     tmmn = mean(tmmn), 
#     rmax = mean(rmax), 
#     rmin = mean(rmin), 
#     vs = mean(vs), 
#     th = mean(th),
#     .groups = "drop")
# 
# # Set <0 to 0
# gridmet_us_grid_mean <- 
#   gridmet_us_grid_mean %>%
#   dplyr::mutate(
#     dplyr::across(
#       dplyr::where(is.numeric), 
#       ~ ifelse(. < 0, 0, .)))
# 
# head(gridmet_us_grid_mean)
# 
# write_fst(gridmet_us_grid_mean, file.path("pmf_ncld_meteo_census/GRIDMET_commom_2011_in_US_grid_01.fst"))

#### NLCD Land use type ####

project_categorical_raster <- function(source_raster, target_raster) {
  require(raster)

  # Get factor levels from source raster
  source_levels <- levels(source_raster)[[1]]
  source_vals <- as.numeric(source_raster[])

  # Transform source coordinates to target CRS
  source_coords <- xyFromCell(source_raster, 1:ncell(source_raster))
  source_points_sp <- SpatialPoints(
    coords = source_coords,
    proj4string = crs(source_raster)
  )

  # Transform to target CRS
  source_points_transformed <- spTransform(source_points_sp, crs(target_raster))
  source_coords_transformed <- coordinates(source_points_transformed)

  # Create source points dataframe with transformed coordinates
  valid_idx <- !is.na(source_vals)
  source_points <- data.frame(
    x = source_coords_transformed[valid_idx, 1],
    y = source_coords_transformed[valid_idx, 2],
    value = source_vals[valid_idx]
  )

  # Get target coordinates
  target_coords <- xyFromCell(target_raster, 1:ncell(target_raster))

  message("Finding nearest neighbors for all target cells...")

  # Find nearest neighbor function
  find_nearest <- function(point, source_points) {
    dists <- sqrt((source_points$x - point[1])^2 +
                    (source_points$y - point[2])^2)
    return(source_points$value[which.min(dists)])
  }

  # Process in chunks
  chunk_size <- 1000
  n_chunks <- ceiling(nrow(target_coords) / chunk_size)
  result_values <- numeric(nrow(target_coords))

  for(i in 1:n_chunks) {
    if(i %% 10 == 0) message(sprintf("Processing chunk %d of %d", i, n_chunks))

    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, nrow(target_coords))
    chunk_idx <- start_idx:end_idx

    for(j in seq_along(chunk_idx)) {
      result_values[chunk_idx[j]] <- find_nearest(
        target_coords[chunk_idx[j], ],
        source_points
      )
    }
  }

  # Create output raster
  result_raster <- target_raster
  values(result_raster) <- result_values

  # Set factor levels
  result_raster <- ratify(result_raster)
  levels(result_raster) <- source_levels

  return(result_raster)
}

# Find nearest neighbor function for points in two grid cooridnates
find_nearest <-
  function(point, ncld_year_points) {
    dists <- sqrt((ncld_year_points$x - point[1])^2 +
                    (ncld_year_points$y - point[2])^2)
    return(ncld_year_points$value[which.min(dists)])
  }

####### NLCD 1, basic files #########
# Read original file
ncld_2011 <- raster("pmf_ncld_meteo_census/nlcd_2011_fact300_landcover_resampled.tif")
ncld_2013 <- raster("pmf_ncld_meteo_census/nlcd_2013_fact300_landcover_resampled.tif")
ncld_2016 <- raster("pmf_ncld_meteo_census/nlcd_2016_fact300_landcover_resampled.tif")
ncld_2019 <- raster("pmf_ncld_meteo_census/nlcd_2019_fact300_landcover_resampled.tif")

# Select the year to use
ncld_year_use = 2011
ncld_year = ncld_2011

ncld_year_use = 2013
ncld_year = ncld_2013

ncld_year_use = 2016
ncld_year = ncld_2016

ncld_year_use = 2019
ncld_year = ncld_2019

####### NLCD 2, project the NLCD category raster to US grid raster #########
# Choose the US grid
us_grid_raster = us_grid_raster_01
# us_grid_raster = us_grid_raster_001

# Convert to factor
ncld_year_factor <- ratify(ncld_year)
ncld_year_use

# Get factor levels from ncld_year raster
ncld_year_levels <- levels(ncld_year)[[1]]
ncld_year_vals <- as.numeric(ncld_year[])

# Transform ncld_year coordinates to target CRS
ncld_year_coords <- xyFromCell(ncld_year, 1:ncell(ncld_year))
ncld_year_points_sp <- SpatialPoints(
  coords = ncld_year_coords,
  proj4string = crs(ncld_year)
)
head(ncld_year_points_sp)

# Transform to target CRS
ncld_year_points_transformed <- spTransform(ncld_year_points_sp, crs(us_grid_raster))
ncld_year_coords_transformed <- coordinates(ncld_year_points_transformed)

# Create ncld_year points dataframe with transformed coordinates
ncld_valid_idx <- !is.na(ncld_year_vals)
summary(ncld_valid_idx)

ncld_year_points <- data.frame(
  x = ncld_year_coords_transformed[ncld_valid_idx, 1],
  y = ncld_year_coords_transformed[ncld_valid_idx, 2],
  value = ncld_year_vals[ncld_valid_idx]
)
head(ncld_year_points)

# Get target coordinates
us_grid_coords <- xyFromCell(us_grid_raster, 1:ncell(us_grid_raster))

print("Finding nearest neighbors for all target cells")

####### Process in chunks
# settings for chunks
chunk_size <- 10000
n_chunks <- ceiling(nrow(us_grid_coords) / chunk_size)
us_grid_ncld_values <- numeric(nrow(us_grid_coords))

# process
for(i in 1:n_chunks) {
  if(i %% 10 == 0) message(sprintf("Processing chunk %d of %d", i, n_chunks))

  start_idx <- (i - 1) * chunk_size + 1
  end_idx <- min(i * chunk_size, nrow(us_grid_coords))
  chunk_idx <- start_idx:end_idx

  for(j in seq_along(chunk_idx)) {
    us_grid_ncld_values[chunk_idx[j]] <- find_nearest(
      us_grid_coords[chunk_idx[j], ],
      ncld_year_points
    )
  }
}

# Create output raster
us_grid_ncld_raster <- us_grid_raster
values(us_grid_ncld_raster) <- us_grid_ncld_values

# Set factor levels
us_grid_ncld_raster <- ratify(us_grid_ncld_raster)
levels(us_grid_ncld_raster) <- ncld_year_levels

# # Project the raster preserving factor levels
# us_grid_ncld_raster <-
#   project_categorical_raster(ncld_year_factor, us_grid_raster_01)

# Check the results
table(values(ncld_year_factor)); table(values(us_grid_ncld_raster))
summary(ncld_year_factor); dim(ncld_year_factor)
summary(us_grid_ncld_raster); dim(us_grid_ncld_raster)

# Get coordinates and values of us_grid_ncld_raster
us_grid_ncld_coords = xyFromCell(us_grid_ncld_raster, 1:ncell(us_grid_ncld_raster))
us_grid_ncld_values <- values(us_grid_ncld_raster)

# Create dataframe
us_grid_ncld_df <- data.frame(
  Longitude = us_grid_ncld_coords[,1],
  Latitude = us_grid_ncld_coords[,2],
  NLCD.Land.Cover.Class = us_grid_ncld_values
)
us_grid_ncld$NLCD.Land.Cover.Class = as.factor(us_grid_ncld_df$NLCD.Land.Cover.Class)
summary(us_grid_ncld_df); dim(us_grid_ncld_df)

# Make sure there are only two digits for coordinates
us_grid_ncld_df$Longitude = round(us_grid_ncld_df$Longitude, 2)
us_grid_ncld_df$Latitude = round(us_grid_ncld_df$Latitude, 2)

# Get the sf file for future use (in case)
us_grid_ncld_df$long = us_grid_ncld_df$Longitude
us_grid_ncld_df$lat = us_grid_ncld_df$Latitude

us_grid_ncld_sf =
  st_as_sf(us_grid_ncld_df,
           coords = c("long", "lat"),
           crs = st_crs(us_grid_raster_01))  # use the same CRS as your raster

# Save the file
st_write(us_grid_ncld_sf,
         file.path(
           paste0("pmf_ncld_meteo_census/NCLD_", ncld_year_use, "_in_US_grid_01.fgb")))
write_fst(us_grid_ncld_df,
         file.path(
           paste0("pmf_ncld_meteo_census/NCLD_", ncld_year_use, "_in_US_grid_01.fst")))


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

# ### IDW interpolation, inverse distance weighted 
# # Calculate distances and weights
# roadiness_distances <- st_distance(us_grid_centroids_01, roadiness_sf)
# roadiness_weights <- 1/roadiness_distances
# roadiness_weights[is.infinite(roadiness_weights)] <- 1
# 
# # Normalize roadiness_weights
# roadiness_weights <- roadiness_weights/rowSums(roadiness_weights)
# 
# # Project variables using IDW
# grid_roadiness <- us_grid_centroids_01 %>%
#   mutate(
#     sRoadLength = as.numeric(roadiness_weights %*% roadiness_sf$sRoadLength),
#     sRoadiness_1 = as.numeric(roadiness_weights %*% roadiness_sf$sRoadiness_1),
#     sRoadiness_1.5 = as.numeric(roadiness_weights %*% roadiness_sf$sRoadiness_1.5),
#     sRoadiness_2 = as.numeric(roadiness_weights %*% roadiness_sf$sRoadiness_2),
#     sRoadCount = as.numeric(roadiness_weights %*% roadiness_sf$sRoadCount),
#     sRoadCount_1 = as.numeric(roadiness_weights %*% roadiness_sf$sRoadCount_1),
#     sRoadCount_1.5 = as.numeric(roadiness_weights %*% roadiness_sf$sRoadCount_1.5),
#     sRoadCount_2 = as.numeric(roadiness_weights %*% roadiness_sf$sRoadCount_2),
#     VMT_vs_area = as.numeric(roadiness_weights %*% roadiness_sf$VMT_vs_area)
#   )


#### Nearest point interpolation
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


#### EPA TRI ####

###### TRI, read & prepare data for interpolation ######

tri_metal_use = 
  read.csv("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nation_SA_data/EPA_TRI_Metals_2011-20.csv")
head(tri_metal_use); dim(tri_metal_use); names(tri_metal_use)
# View(tri_metal_use)
unique(tri_metal_use$chem_metal)

# Columns to use
tri_metal_to_project = 
  dplyr::select(tri_metal_use, year, longitude, latitude, chem_metal, air_stack_source)
head(tri_metal_to_project); dim(tri_metal_to_project)

# In case of duplicated
tri_metal_to_project = 
  tri_metal_to_project %>%
  dplyr::group_by(year, longitude, latitude, chem_metal) %>%
  dplyr::summarise(
    air_stack_source = mean(air_stack_source, rm.na = TRUE),
    .groups = "drop"
  )
dim(tri_metal_to_project)

# Get the dataset, emission of each metal into air, fill NULL by 0
tri_metal_to_project_use =
  tri_metal_to_project %>%
  pivot_wider(
    values_from = "air_stack_source", # use the original emission to air directly, ignore the off.site_release
    names_from = "chem_metal",
    values_fill = 0 # fill those NULL with 0, not NA
  ) 
head(tri_metal_to_project_use); dim(tri_metal_to_project_use)

# Convert to sf
tri_metal_sf_project <- 
  st_as_sf(tri_metal_to_project_use, 
           coords = c("longitude", "latitude"), crs = 4326)
head(tri_metal_sf_project)

###### TRI, project ######

us_grid_raster_01 = 
  raster(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/CMAQ_previous_extract_tries/us_grid_raster_01.tif"))

us_grid_raster = us_grid_raster_01

cared_metal_all = 
  c("Al", "As", "Ba", "Be", "Ca", "Cd", "Co", "Cr", "Cu", "Fe",
    "Hg", "Mn", "Na", "Ni", "Pb", "Sb", "Se", "Sn", "V", "Zn")

study_years = 2011:2020

# rm(list = ls())
# Function for projection to US raster and extract date info
brick_var_date_project <- 
  function(rast_file_path, grid_raster) { 
    # Open the rast file, use brick for multi-band file!!
    rast_file <- brick(rast_file_path)
    
    # Extract the layer names and dates
    layer_names <- names(rast_file)
    layer_dates <- gsub(".*_(\\d{4})_(\\d{2})_(\\d{2})", "\\1-\\2-\\3", layer_names)
    # print("First five dates in in layers:")
    # layer_dates[1:5]
    
    # Then project to match the target US grid of 0.1 or 0.01 degree
    # When using projectRaster, both rast file should be from 
    projected_var_raster <- projectRaster(rast_file, 
                                          grid_raster, 
                                          method = "bilinear")
    # extent(rast_file); extent(grid_raster); extent(projected_var_raster)
    
    # Only use the day information to rename the layers
    names(projected_var_raster) <- paste0("Date_", layer_dates)
    # plot( projected_var_raster[[c(1,10,20)]])
    
    # Return the raster brick
    return(projected_var_raster = projected_var_raster)
  }

# Initialize an empty vector to store the names of files that return NULL
# some downloaded .tif cannot be read directly via raster() or terra::rast()
failed_files <- c()

# Merge and output file by EPA TRI variable and by year
for (tri_metal in cared_metal_all) { # tri_metal = "Cu"
  for (tri_year in study_years) { # tri_year = 2011
    
    # List all .tif files for the specific variable and year
    tri_yr_metal_sf <- 
      subset(tri_metal_sf_project, 
             year == tri_year)
    
    if (nrow(tri_yr_metal_sf) > 0) {
      # Initialize an empty list to store successfully loaded rasters
      rasters <- list()
      
      for (tif_file in tri_yr_metal_sf) {
        # Extract the name and use it as rast name
        rast.name = sub("\\.tif$", "", basename(tif_file))
        
        
        # Only add successfully loaded rasters to the list
        if (!is.null(raster_data)) {
          rasters <- c(rasters, list(raster_data))
        }
      }
      
      # Only proceed if there are valid rasters to stack
      if (length(rasters) > 0) {
        # Stack the valid rasters into a multi-layer raster (stacked temporally)
        tri_merged_raster <- rast(rasters)
        
        # Define the output filename
        meter_merged_filename <- 
          file.path("/EPA_TRI_industry/Projected_element_year",
                    paste0(tri_metal, "_", tri_year, "_stacked.tif")) 
        
        # Save the stacked raster to a new .tif file
        writeRaster(
          tri_merged_raster,
          filename = meter_merged_filename, 
          overwrite = TRUE)
        
        print(paste0("File saved: ", meter_merged_filename))
      } else {
        print(paste0("No valid rasters to stack for ", tri_metal, " in ", tri_year))
      }
    }
  }
}

# Loop through each year and combine the selected variables
for (met_year in study_years) { # met_year = 2017
  print(paste("TRImetal year to be used:", met_year))
  
  # Initialize an empty list to store rasters for this year
  rasters_to_merge <- list()
  
  # Loop through each selected variable
  for (tri_var in tri_var_comm) { # tri_var = tri_var_comm[1]
    print(paste("TRImetal Variable to be used:", tri_var))
    
    # Construct the file path for the stacked raster of this variable and year
    file_path <- 
      file.path(gridmet_stack_path, 
                paste0(tri_var, "_", met_year, "_stacked.tif"))
    
    # Process the variable to create the raster brick
    met_var_project <- 
      brick_var_date_project(
        file_path, 
        us_grid_raster)
    
    # Extract spatial information and values
    tri_var_dt = 
      as.data.table(as.data.frame(met_var_project, xy = TRUE, na.rm = TRUE))
    # head(tri_var_dt); dim(tri_var_dt)
    
    # Reshape data from wide to long format (each variable in a single column)
    tri_var_long <- 
      melt(tri_var_dt, 
           id.vars = c("x", "y"), 
           variable.name = "Layer", 
           value.name = tri_var,
           na.rm = TRUE)
    
    # Add Date 
    tri_var_long[, Date := 
                     gsub(".*_(\\d{4}).(\\d{2}).(\\d{2})", 
                          "\\1-\\2-\\3", 
                          Layer)]
    tri_var_long[, Layer := NULL]
    
    print("Check the file for the applied tri_var")
    head(tri_var_long); summary(tri_var_long)
    
    # Store in list
    tri_var_list[[tri_var]] <- tri_var_long
  }
  
  # Save the single tri data one by one for this year
  for (tri_var in tri_var_comm) { # tri_var = tri_var_comm[2]
    
    tri_var_year = tri_var_list[[tri_var]]
    tri_var_year = 
      tri_var_year[with(tri_var_year, order(Date, x, y)), ]
    names(tri_var_year)[1:2]
    names(tri_var_year)[1:2] = c("Longitude", "Latitude")
    dim(tri_var_year)
    
    print(paste("Check the output file for a single tri_var", 
                head(tri_var_year)
    ))
    
    write_fst(tri_var_year, 
              file.path(gridmet_stack_source_path, 
                        paste0("Common_GRIDMET_", tri_var, "_", met_year, "_stacked.fst")))
  }
  
  # Clear memory
  rm(tri_merged_year, tri_var_list)
  gc()
}



#### Census-1: commute ####

# ###### Census-1, Commute variable extract & matching with GEOID ######
# ### Census tract level info - for commute
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
# 
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
class(census_acs_geo_mainland)

census_acs_wide = read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_Census_ACS_tract_commute_2011-2020.fst")

# For raster extract
us_grid_sf_01 = st_read(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/CMAQ_previous_extract_tries/us_grid_sf_01.fgb"))
us_grid_sf = us_grid_sf_01
us_grid_raster_01 = raster(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/CMAQ_previous_extract_tries/us_grid_raster_01.tif"))
us_grid_raster = us_grid_raster_01

subset(census_acs_wide, is.na(car_truck_van))
census_acs_wide = na.omit(census_acs_wide)
head(census_acs_wide); dim(census_acs_wide); summary(census_acs_wide)

st_geometry(census_acs_geo_mainland) <- "geometry"
head(census_acs_geo_mainland)
subset(census_acs_geo_mainland, st_is_empty(geometry)) # check if there is empty geometry

census_out_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS"

#### Census-1: commute, match via raster extract #### 

# Initialize a list to store all results for 2011-2020
census_acs_rast_list <- list()
census_extract_raster_year_list <-  list()

for (census_year in 2013:2020){ # census_year = 2011, 2011:2020
  print(paste("Processing year:", census_year))
  
  ###### Prepare basic data ###### 
  
  # Prepare census commute data for each year, and get GEOID that are within mainland US
  # census_acs_wide_year = subset(census_acs_wide, year == census_year)
  census_acs_geo_year = 
    merge(subset(census_acs_wide, year == census_year), 
          census_acs_geo_mainland, 
          by = "GEOID")
  print(paste("Rows after matching census_acs_geo_mainland in census_acs_geo_year:", 
              nrow(census_acs_geo_year)))
  
  # Set as sf and remove empty geometries, if any
  census_acs_year_sf = 
    st_as_sf(census_acs_geo_year) %>%
    filter(!st_is_empty(geometry))
  # head(census_acs_geo_year); summary(census_acs_geo_year); dim(census_acs_geo_year)
  print(paste("Rows after removing empty geometry in census_acs_year_sf, if any:", 
              nrow(census_acs_year_sf)))
  # ggplot(census_acs_year_sf) +
  #   geom_sf(aes(fill = car_truck_van), color = NA) +
  #   scale_fill_viridis_c(
  #     option = "plasma", 
  #     name = "Car/Truck/Van\nCommute %",
  #     labels = scales::percent_format()
  #   )
  
  # Extract census variables
  commute_variables = 
    setdiff(names(census_acs_year_sf), 
            c("GEOID", "year", "geometry"))
  print("Commute variables:"); commute_variables

  #### Use rasterize using area-weighted mean
  # Convert sf to vect for terra compatibility
  census_acs_year_vect <- vect(census_acs_year_sf)
  # print("Check the vect file for a single year:")
  # head(census_acs_year_vect); class(census_acs_year_vect); dim(census_acs_year_vect)
  
  # Get the empty raster, need SpatRaster format here
  raster_census <- rast(us_grid_raster)
  
  # Rasterize each variable using area-weighted mean
  census_acs_year_var_rast <- 
    rast(lapply(commute_variables, function(var) {
    rasterize(census_acs_year_vect, raster_census, 
              field = var, fun = "mean")
  }))
  
  # Copy variable names
  names(census_acs_year_var_rast) <- commute_variables
  print("Summary the census_acs_year_var_rast:")
  summary(census_acs_year_var_rast); census_acs_year_var_rast
  # plot(census_acs_year_var_rast$car_truck_van)
  
  census_acs_year_var_rast_all = rast(census_acs_year_var_rast)
  
  # Store annual results in list
  census_acs_rast_list[[as.character(census_year)]] =
    census_acs_year_var_rast

  ###### Match GEOID in census with geometry in US_grids: Raster Extract ###### 
  
  for (commute_var in commute_variables){ 
    print(paste("Processing variable:", commute_var))
    
    # Convert to SpatRaster and create new raster from template
    srast_census <- rast(us_grid_raster)
    print(paste("Class of srast_census:", class(srast_census)))
    
    # Extract values with exact=TRUE to get area fractions
    census_commute_values_var <- 
      terra::extract(srast_census, census_acs_year_sf, 
                     exact=TRUE, cells=TRUE, small=TRUE)  # Include small overlaps
    
    print(paste("Number of unique cells:", length(unique(census_commute_values_var$cell))))
    
    # Calculate weighted values using the commute variable
    census_commute_values_var$weighted_value <- 
      census_acs_year_sf[[commute_var]][census_commute_values_var$ID] * 
      census_commute_values_var$fraction
    
    # Aggregate by cell to get weighted mean for each grid cell
    agg_values <- aggregate(
      census_commute_values_var[!is.na(census_commute_values_var$weighted_value), 
                                c("weighted_value", "fraction")], 
      by=list(cell=census_commute_values_var$cell[!is.na(census_commute_values_var$weighted_value)]), 
      FUN=sum)
    
    # Calculate final weighted mean with threshold for small fractions
    agg_values$final_value <- ifelse(agg_values$fraction > 0.001,
                                     agg_values$weighted_value / agg_values$fraction,
                                     NA)
    
    # Initialize raster with NA and assign values
    values(srast_census)[agg_values$cell] <- agg_values$final_value
    # plot(srast_census, main=paste("Census Variable:", commute_var))
    
    # Store in list
    census_extract_raster_year_list[[commute_var]] <- srast_census
  }
  
  # Assign year and combine
  census_extract_raster_year_list$year = census_year
  census_extract_year_raster_all = rast(census_extract_raster_year_list)
  
  ###### Get centroids and save fst files & Save raster files - Rasterize process ######
  census_acs_year_centroid_df <- 
    data.frame(
      cell = 1:ncell(census_acs_year_var_rast),
      xyFromCell(census_acs_year_var_rast, 1:ncell(census_acs_year_var_rast)),
      values(census_acs_year_var_rast)
    )
  names(census_acs_year_centroid_df)[2:3]
  names(census_acs_year_centroid_df)[2:3] = c("Longitude", "Latitude")
  
  # round to 2 digits
  census_acs_year_centroid_df$Longitude = round(census_acs_year_centroid_df$Longitude, 2)
  census_acs_year_centroid_df$Latitude = round(census_acs_year_centroid_df$Latitude, 2)
  
  print("Check the output fst file using Rasterize:")
  head(census_acs_year_centroid_df); summary(census_acs_year_centroid_df); dim(census_acs_year_centroid_df)
  
  # setDT(census_acs_year_centroid_df)
  write_fst(
    census_acs_year_centroid_df,
    file.path(census_out_path, 
              paste0("Census_commute_Rasterize_", census_year, ".fst")))
  
  writeRaster(
    census_acs_year_var_rast,
    file.path(census_out_path, 
              paste0("Census_commute_Rasterize_", census_year, ".tif")),
    overwrite = TRUE,
    gdal = "COMPRESS=LZW"  # To minimize file size
  )
  
  ###### Get centroids and save fst files & Save raster files - Extract process ######
  
  census_extract_year_raster_df <- 
    data.frame(
      cell = 1:ncell(census_extract_year_raster_all),
      xyFromCell(census_extract_year_raster_all, 1:ncell(census_extract_year_raster_all)),
      values(census_extract_year_raster_all)
    )
  names(census_extract_year_raster_df)[2:3]
  names(census_extract_year_raster_df)[2:3] = c("Longitude", "Latitude")
  
  # round to 2 digits
  census_extract_year_raster_df$Longitude = round(census_extract_year_raster_df$Longitude, 2)
  census_extract_year_raster_df$Latitude = round(census_extract_year_raster_df$Latitude, 2)
  
  print("Check the output fst file using Extract:")
  head(census_extract_year_raster_df); summary(census_extract_year_raster_df); dim(census_extract_year_raster_df)
  
  write_fst(
    census_extract_year_raster_df,
    file.path(census_out_path, 
              paste0("Census_commute_Extract_", census_year, ".fst")))
  
  writeRaster(
    census_extract_year_raster_all,
    file.path(census_out_path, 
              paste0("Census_commute_Extract_", census_year, ".tif")),
    overwrite = TRUE,
    gdal = "COMPRESS=LZW"  # To minimize file size
  )
}


#### Census-2: Race, Gender, etc. ####

# # Geometry & GEOID
# census_acs_geo_mainland =
#   st_read("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/ACS_census_tract_geoid_geometry_4326_USmainland.fgb")
# 
# census_acs_ej =
#   read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_Census_ACS_tract_EJ_2011-2020.fst")
# 
# census_out_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS"


setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/")
getw()

census_acs_geo_mainland = 
  st_read("pmf_ncld_meteo_census/ACS_census_tract_geoid_geometry_4326_USmainland.fgb")
census_acs_ej = 
  read_fst("pmf_ncld_meteo_census/US_Census_ACS_tract_EJ_2011-2020.fst")

length(unique(census_acs_geo_mainland$GEOID))
class(census_acs_geo_mainland)
head(census_acs_geo_mainland)


# # For raster extract
# us_grid_sf_01 =
#   st_read(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/CMAQ_previous_extract_tries/us_grid_sf_01.fgb"))
# us_grid_raster_01 =
#   raster(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/CMAQ_previous_extract_tries/us_grid_raster_01.tif"))

# For raster extract
us_grid_sf_01 = 
  st_read(file.path("base_raster_grid_sf/us_grid_sf_01.fgb"))
us_grid_raster_01 = 
  raster(file.path("base_raster_grid_sf/us_grid_raster_01.tif"))

us_grid_sf = us_grid_sf_01
us_grid_raster = us_grid_raster_01
grid_centroids <- st_centroid(us_grid_sf)


#### Census-2: GEOID match with Longitude Latitude #### 

# Match all GEOID with the geometry
census_acs_geoid_geo = 
  merge(census_acs_geoid, census_acs_geo_mainland)
st_geometry(census_acs_geoid_geo) <- "geometry"

head(census_acs_geoid_geo)
subset(census_acs_geoid_geo, st_is_empty(geometry)) # check if there is empty geometry

# #### Beyong memory
# # Find nearest GEOID for EVERY grid cell
# grid_nearest_idx <- st_nearest_feature(grid_centroids, census_acs_geoid_geo)
# 
# # Assign GEOIDs to grid cells 
# us_grid_sf_geoid = us_grid_sf
# us_grid_sf_geoid$assigned_geoid <- 
#   census_acs_geoid_geo$GEOID[grid_nearest_idx]
# 
# # Verify coverage
# # Check that no grid cells are unassigned
# stopifnot("Some grid cells have no GEOID assigned" = !any(is.na(us_grid_sf$assigned_geoid)))
# 
# # Optional: Check distribution of assignments
# geoid_assignment_counts <- table(us_grid_sf$assigned_geoid)
# print(summary(as.numeric(geoid_assignment_counts)))
# 
# st_write(census_acs_geoid_geo, "US_01_grid_GEOID.geojson")


# Check a small sample to confirm structure
sample_check <- 
  function(grid_points, census_polygons, n_sample = 10) {
  # Sample some points
  sample_idx <- sample(1:nrow(grid_points), min(n_sample, nrow(grid_points)))
  sample_points <- grid_points[sample_idx, ]
  
  # Find nearest
  nearest_idx <- st_nearest_feature(sample_points, census_polygons)
  
  # Create results
  results <- data.table(
    grid_id = sample_idx,
    grid_x = st_coordinates(sample_points)[,1],
    grid_y = st_coordinates(sample_points)[,2],
    geoid = census_polygons$GEOID[nearest_idx],
    year = census_polygons$year[nearest_idx]
  )
  
  return(results)
}

sample_check(
  grid_centroids, 
  census_acs_geo_mainland
)

# Optimized function with more fields and progress tracking
process_nearest_in_chunks <- 
  function(grid_points, 
           census_polygons, 
           chunk_size, 
           output_file,
           resume_from) {
    
  # Total number of points to process
  total_points <- nrow(grid_points)
  total_chunks <- ceiling((total_points - resume_from + 1) / chunk_size)
  
  # Create empty list for batch results
  result_batches <- list()
  current_batch <- 1
  
  # Track timing for progress estimates
  start_time <- Sys.time()
  last_time <- start_time
  
  # Process each chunk
  for (chunk in 1:total_chunks) {
    # Calculate chunk indices
    start_idx <- resume_from + (chunk - 1) * chunk_size
    end_idx <- min(resume_from + chunk * chunk_size - 1, total_points)
    
    # Extract current points chunk
    points_chunk <- grid_points[start_idx:end_idx, ]
    
    # Find nearest feature for this chunk
    nearest_idx <- st_nearest_feature(points_chunk, census_polygons)
    
    # Get coordinates
    coords <- st_coordinates(points_chunk)
    
    # Create results
    results <- data.table(
      grid_id = start_idx:end_idx,
      Longi = coords[,1],  # Add X coordinate
      grid_y = coords[,2],  # Add Y coordinate
      GEIOD = census_polygons$GEOID[nearest_idx]
     )
    
    # Store in result batch
    result_batches[[length(result_batches) + 1]] <- results
    
    # Save to disk every 5 chunks or on the last chunk
    if (chunk %% 5 == 0 || chunk == total_chunks) {
      # Combine batches
      batch_results <- rbindlist(result_batches)
      
      # Write part file, .fst
      part_file <- paste0(output_file, "_part", current_batch, ".fst")
      write_fst(batch_results, part_file)
      cat("Saved batch to:", part_file, "\n")
      
      # Clear batch data and increment batch counter
      result_batches <- list()
      current_batch <- current_batch + 1
      
      # Force garbage collection
      gc(full = TRUE)
    }
    
    # Clean up to free memory
    rm(points_chunk, nearest_idx, results, coords)
    gc(full = TRUE)
    
    # Calculate and report progress with time estimates
    current_time <- Sys.time()
    elapsed <- as.numeric(difftime(current_time, start_time, units = "secs"))
    chunk_time <- as.numeric(difftime(current_time, last_time, units = "secs"))
    last_time <- current_time
    
    pct_complete <- 100 * chunk / total_chunks
    est_remaining <- elapsed / pct_complete * (100 - pct_complete)
    
    cat(sprintf("Chunk %d/%d (%.1f%%) - This chunk: %.1f sec - Est. remaining: %.1f min\n", 
                chunk, total_chunks, pct_complete,
                chunk_time, est_remaining/60))
  }
  
  # Reminder about combining files
  cat("\nProcessing complete! To combine all part files, run:\n")
  cat("part_files <- list.files(pattern = \"", basename(output_file), "_part.*\\.fst$\", full.names=TRUE)\n", sep="")
  cat("all_results <- rbindlist(lapply(part_files, read_fst))\n")
  cat("write_fst(all_results, \"", output_file, "\")\n", sep="")
}


# Process with very small chunks to manage memory
process_nearest_in_chunks(
  grid_centroids, 
  census_acs_geo_mainland, 
  chunk_size = 500,
  output_file = "GEOID_US_Grid_01",
  resume_from = 1)





census_acs_geoid =
  dplyr::select(census_acs_ej, GEOID, year)
head(census_acs_geoid); summary(census_acs_geoid)
census_acs_geoid = 
  subset(census_acs_geoid, GEOID %in% census_acs_geo_mainland$GEOID)
summary(unique(census_acs_geoid$GEOID) %in% census_acs_geo_mainland$GEOID)

census_acs_geoid_unique = 
  data.table(table(census_acs_geoid$GEOID, census_acs_geoid$geometry))
nrow(census_acs_geoid_unique)










#### Census-2: Race, Gender, etc. commute, match via raster extract only #### 


# Initialize a list to store all results for 2011-2020
geoid_rast_list <- list()
geoid_extract_year_list <-  list()

for (geoid_year in 2011:2020){ # geoid_year = 2017, 2011:2020
  print(paste("Processing year:", geoid_year))
  
  ###### Prepare basic data ###### 
  
  # Prepare census commute data for each year, and get GEOID that are within mainland US
  geoid_geo_year = 
    merge(subset(census_acs_ej, year == geoid_year), 
          census_acs_geo_mainland, 
          by = "GEOID")
  print(paste("Rows after matching census_acs_geo_mainland in geoid_geo_year:", 
              nrow(geoid_geo_year)))
  
  # Set as sf and remove empty geometries, if any
  geoid_year_sf = 
    st_as_sf(geoid_geo_year) %>%
    filter(!st_is_empty(geometry))
  # head(geoid_geo_year); summary(geoid_geo_year); dim(geoid_geo_year)
  print(paste("Rows after removing empty geometry in geoid_year_sf, if any:", 
              nrow(geoid_year_sf)))
  # ggplot(geoid_year_sf) +
  #   geom_sf(aes(fill = car_truck_van), color = NA) +
  #   scale_fill_viridis_c(
  #     option = "plasma", 
  #     name = "Car/Truck/Van\nCommute %",
  #     labels = scales::percent_format()
  #   )
  
  # Extract census variables
  geoid_variables = 
    setdiff(names(geoid_year_sf), 
            c("GEOID", "year", "geometry"))
  print("EJ variables:"); geoid_variables
  
  #### Use rasterize using area-weighted mean
  # Convert sf to vect for terra compatibility
  geoid_year_vect <- vect(geoid_year_sf)
  # print("Check the vect file for a single year:")
  # head(geoid_year_vect); class(geoid_year_vect); dim(geoid_year_vect)
  
  # Get the empty raster, need SpatRaster format here
  raster_census <- rast(us_grid_raster)
  
  # Rasterize each variable using area-weighted mean
  geoid_year_var_rast <- 
    rast(lapply(geoid_variables, function(var) {
      rasterize(geoid_year_vect, raster_census, 
                field = var, fun = "mean")
    }))
  
  # Copy variable names
  names(geoid_year_var_rast) <- geoid_variables
  print("Summary the geoid_year_var_rast:")
  summary(geoid_year_var_rast); geoid_year_var_rast
  # plot(geoid_year_var_rast$car_truck_van)
  
  geoid_year_var_rast_all = rast(geoid_year_var_rast)
  
  # Store annual results in list
  geoid_rast_list[[as.character(geoid_year)]] =
    geoid_year_var_rast
  
  ###### Match GEOID in census with geometry in US_grids: Raster Extract ###### 
  
  for (geoid_var in geoid_variables){ 
    print(paste("Processing variable:", geoid_var))
    
    # Convert to SpatRaster and create new raster from template
    srast_census <- rast(us_grid_raster)
    print(paste("Class of srast_census:", class(srast_census)))
    
    # Extract values with exact=TRUE to get area fractions
    geoid_values_var <- 
      terra::extract(srast_census, geoid_year_sf, 
                     exact=TRUE, cells=TRUE, small=TRUE)  # Include small overlaps
    
    print(paste("Number of unique cells:", length(unique(geoid_values_var$cell))))
    
    # Calculate weighted values using the commute variable
    geoid_values_var$weighted_value <- 
      geoid_year_sf[[geoid_var]][geoid_values_var$ID] * 
      geoid_values_var$fraction
    
    # Aggregate by cell to get weighted mean for each grid cell
    agg_values <- aggregate(
      geoid_values_var[!is.na(geoid_values_var$weighted_value), 
                                c("weighted_value", "fraction")], 
      by=list(cell=geoid_values_var$cell[!is.na(geoid_values_var$weighted_value)]), 
      FUN=sum)
    
    # Calculate final weighted mean with threshold for small fractions
    agg_values$final_value <- ifelse(agg_values$fraction > 0.001,
                                     agg_values$weighted_value / agg_values$fraction,
                                     NA)
    
    # Initialize raster with NA and assign values
    values(srast_census)[agg_values$cell] <- agg_values$final_value
    # plot(srast_census, main=paste("Census Variable:", geoid_var))
    
    # Store in list
    geoid_extract_year_list[[geoid_var]] <- srast_census
  }
  
  # Assign year and combine
  geoid_extract_year_list$year = geoid_year
  census_extract_year_raster_all = rast(geoid_extract_year_list)
  
  ###### Get centroids and save fst files & Save raster files - Extract process ######
  
  census_extract_year_raster_df <- 
    data.frame(
      cell = 1:ncell(census_extract_year_raster_all),
      xyFromCell(census_extract_year_raster_all, 1:ncell(census_extract_year_raster_all)),
      values(census_extract_year_raster_all)
    )
  names(census_extract_year_raster_df)[2:3]
  names(census_extract_year_raster_df)[2:3] = c("Longitude", "Latitude")
  
  # round to 2 digits
  census_extract_year_raster_df$Longitude = round(census_extract_year_raster_df$Longitude, 2)
  census_extract_year_raster_df$Latitude = round(census_extract_year_raster_df$Latitude, 2)
  
  print("Check the output fst file using Extract:")
  head(census_extract_year_raster_df); summary(census_extract_year_raster_df); dim(census_extract_year_raster_df)
  
  write_fst(
    census_extract_year_raster_df,
    file.path(census_out_path, 
              paste0("Census_EJ_geoid_Extract_", geoid_year, ".fst")))
  
  writeRaster(
    census_extract_year_raster_all,
    file.path(census_out_path, 
              paste0("Census_EJ_geoid_Extract_", geoid_year, ".tif")),
    overwrite = TRUE,
    gdal = "COMPRESS=LZW"  # To minimize file size
  )
}


####  HMS SMOKE ####
setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/HMS_SMOKE/")
# setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/")
getwd()

# The raster for projection
us_grid_raster_01 = rast(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/CMAQ_previous_extract_tries/us_grid_raster_01.tif"))
us_grid_raster = us_grid_raster_01
head(us_grid_raster)

### Below two files only include points matching CSN and IMPROVE sites, no grid
hms_shp_all_valid_us =
  st_read("NOAA_HMS_all_daily_match_2011-20.gpkg")
hms_shp_all_valid_us
hms_shp_all_valid_us[260, ]

# hms_shp_all_valid_us_csv =
#   read.csv( "NOAA_HMS_CSN_IMPROVE_daily_match_2011-20.csv")
# hms_shp_all_valid_us_csv$X = NULL
# View(hms_shp_all_valid_us_csv)
# hms_shp_all_valid_us_csv[260, ]

# Get date info
hms_shp_all_valid_us$Date_str = 
  substr(hms_shp_all_valid_us$Start, 1, 7)

st_geometry(hms_shp_all_valid_us) <- "geometry"

# Extract dataset for interpolation
hms_us_org = 
  dplyr::select(hms_shp_all_valid_us,
                Date_str, Severity, geometry)
names(hms_us_org)[2] = "smoke_level"
hms_us_org

# Check validity of geometries
invalid_polys <- hms_us_org[!st_is_valid(hms_us_org), ]
print(unique(invalid_polys$Date_str))

######  HMS: rasterize the smoke level by day, and spatial projection ######
# Get unique day to split polygons by Date
hms_dates <- unique(hms_us_org$Date_str)
length(hms_dates); head(hms_dates)
hms_stack_project <- list()

for (hms_day in hms_dates[580:1460]) { # hms_day = hms_dates[200], hms_dates incldue 1460 dates in total
  # 2015081, 2015105, 2015106
  # Subset polygons for the current hms_day
  hms_daily_poly <- hms_us_org[hms_us_org$Date_str == hms_day, ]
  
  # Skip if no polygons exist for this hms_day (result will be NA grid)
  if (nrow(hms_daily_poly) == 0) {
    hms_daily_raster <- us_grid_raster
    values(hms_daily_raster) <- NA
    
  } else {
    # Convert SF to SpatVector and rasterize
    hms_daily_raster <- 
      rasterize(
      vect(hms_daily_poly), 
      us_grid_raster, 
      field = "smoke_level",
      fun = "max" # Choose "max", "min", or "last" for overlapping polygons
    )
    # plot(hms_daily_raster)
  }
  
  # Name the layer after the date and add to stack
  names(hms_daily_raster) <- hms_day
  hms_stack_project[[hms_day]] <- hms_daily_raster
}

#### Handle problematic polygons
hms_problem_day = c("2015081", "2015105", "2015106")
for (hms_prom_day in hms_problem_day){ # hms_prom_day = hms_problem_day[1]
  hms_daily_poly_promb <- hms_us_org[hms_us_org$Date_str == hms_prom_day, ]
  hms_daily_poly_promb_fix <- st_buffer(hms_daily_poly_promb, 0)
  
  # Skip if no polygons exist for this hms_prom_day (result will be NA grid)
  if (nrow(hms_daily_poly_promb_fix) == 0) {
    hms_daily_poly_promb_fix <- us_grid_raster
    values(hms_daily_poly_promb_fix) <- NA
    
  } else {
    # Convert SF to SpatVector and rasterize
    hms_daily_poly_promb_fix <- 
      rasterize(
        vect(hms_daily_poly), 
        us_grid_raster, 
        field = "smoke_level",
        fun = "max" # Choose "max", "min", or "last" for overlapping polygons
      )
    # plot(hms_daily_poly_promb_fix)
  }
  
  # Name the layer after the date and add to stack
  names(hms_daily_poly_promb_fix) <- hms_prom_day
  hms_stack_project[[hms_prom_day]] <- hms_daily_poly_promb_fix
}

# Check the enrolled days
length(hms_stack_project) # 1459, 1 less than the original one

# Combine all layers into a single SpatRaster
hms_final_stack <- rast(hms_stack_project)

# Save as GeoTIFF 
writeRaster(hms_final_stack, "HMS_US_grids_01_2011-2020.tif", overwrite=TRUE)

######  HMS: to df format, and date match ######
#### Convert to df with coordinates and values

# Get coordinates
hms_coords <- xyFromCell(hms_final_stack, 1:ncell(hms_final_stack))
hms_coords_df <- data.table(hms_coords)
names(hms_coords_df) <- c("Longitude", "Latitude")
head(hms_coords_df)

# Get values for all dates
# Loop through each layer (hms_datestr) in the stack
for (hms_datestr in names(hms_final_stack)) { # hms_datestr = names(hms_final_stack)[2]
  # Extract smoke levels for this hms_datestr
  smoke_values <- values(hms_final_stack[[hms_datestr]], mat = FALSE)
  # summary(smoke_values)
  
  # Add smoke levels and hms_datestr to the data.table
  hms_coords_df[, (hms_datestr) := smoke_values]
}

# Convert to long format
hms_coords_long <-
  hms_coords_df %>%
  pivot_longer(
    cols = -c(Longitude, Latitude),
    names_to = "Date_str",
    values_to = "smoke_level"
  )
head(hms_coords_long); dim(hms_coords_long)

write_fst(hms_coords_long, "HMS_US_grids_01_2011-2020.fst")

#### Extract Date info, beyong mac storage, run it on HOPPER
#### 
# hms_coords_long = read_fst("HMS_US_grids_01_2011-2020.fst")
# hms_coords_long = read_fst("pmf_ncld_meteo_census/HMS_US_grids_01_2011-2020.fst")
setDT(hms_coords_long)

hms_date = as.data.table(table(hms_coords_long$Date_str))
names(hms_date)[1] = "Date_str"
hms_date[, Date := as.Date(Date_str, format = "%Y%j")]
hms_date$N = NULL
head(hms_date)

## Merge with the date info
# # sapply(hms_coords_long, class)
# # sapply(hms_date, class)
# hms_coords_long = 
#   merge(hms_coords_long, hms_date)
# 
# hms_coords_long$Date_str = NULL
# 
# # Replace NaN with 0 in smoke_level column
# hms_coords_long[is.nan(smoke_level), smoke_level := 0]

## Merge with the date info with chunks
# Process in chunks
chunk_size = 5000000  
total_rows = nrow(hms_coords_long)
num_chunks = ceiling(total_rows/chunk_size)

# Initialize an empty list to store processed chunks
hms_processed_chunks = vector("list", num_chunks)

for(i in 1:num_chunks) {
  # Calculate chunk indices
  start_idx = ((i-1) * chunk_size) + 1
  end_idx = min(i * chunk_size, total_rows)
  
  # Process chunk - merge with date
  hms_chunk = hms_coords_long[start_idx:end_idx, ]
  hms_chunk = merge(hms_chunk, hms_date, by="Date_str")
  hms_chunk[, Date_str := NULL]
  
  # Process chunk - replace NaN with 0 in smoke_level column
  hms_chunk[is.nan(smoke_level), smoke_level := 0]
  
  # Store processed chunk
  hms_processed_chunks[[i]] = hms_chunk
  
  # Optional: Print progress
  cat(sprintf("Processed chunk %d of %d\n", i, num_chunks))
}

# Combine all chunks
hms_coords_long_final = rbindlist(hms_processed_chunks)
head(hms_coords_long_final); summary(hms_coords_long_final)

# Clean up
rm(hms_processed_chunks, chunk)
gc()

# Save file
# write_fst(hms_coords_long_final, "HMS_US_grids_01_2011-2020_final.fst")
write_fst(hms_coords_long_final, "pmf_ncld_meteo_census/HMS_US_grids_01_2011-2020_final.fst")

hms_coords_long_final = read_fst("pmf_ncld_meteo_census/HMS_US_grids_01_2011-2020_final.fst")
head(hms_coords_long_final)

