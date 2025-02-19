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
class(census_acs_geo_mainland)

census_acs_wide = read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_Census_ACS_tract_commute_2011-2020.fst")

# # For nearest point
# us_grid_centroids_01 = st_read(file.path("/Users/TingZhang/Documents/HEI HAQ PMF/HEI_PMF_nationalSA/us_grid_centroids_01.fgb"))
# class(us_grid_centroids_01); head(us_grid_centroids_01)
# us_grid_centroids = us_grid_centroids_01

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


#### Use parallel method, but the storage is not enough
# # install.packages("future.apply", dependencies = TRUE)
# library(future.apply)
# plan(multisession)  # Use available cores (test with 4-6 workers)
# 
# # Get GEOID that are within mainland US
# census_acs_geo = 
#   merge(census_acs_wide, 
#         census_acs_geo_mainland, 
#         by = "GEOID")
# print("Dim of census_acs_geo and census_acs_geo_mainland")
# dim(census_acs_geo); dim(census_acs_geo_mainland)
# 
# # Set as sf and remove empty geometries, if any
# census_acs_geo_sf = 
#   st_as_sf(census_acs_geo) %>%
#   filter(!st_is_empty(geometry))
# print("Head & dim of census_acs_geo_sf")
# head(census_acs_geo_sf); dim(census_acs_geo_sf)
# 
# # Extract unique years from your dataset
# all_years <- unique(census_acs_geo_sf$year) %>% sort()
# print("Included years in census_acs_geo_sf")
# data.frame(table(census_acs_geo_sf$year))
# 
# # Extract census variables
# commute_variables = 
#   setdiff(names(census_acs_geo_sf), 
#           c("GEOID", "year", "geometry"))
# print("Commute variables:"); commute_variables
# 
# # Process years in parallel
# census_acs_rast_list <- 
#   future_lapply(all_years, function(y) {
#     
#   # Year filtering and conversion
#   census_grid_year_sf <- 
#     census_acs_geo_sf %>% 
#     filter(year == y) %>% 
#     vect()
#   
#   # Rasterize with fixed template
#   rast(lapply(commute_variables, \(var) {
#     rasterize(census_grid_year_sf, 
#               raster_census, 
#               field = var, fun = "mean")
#   }))
#   
# }, future.seed = TRUE)  # Required for reproducibility
# 
# # Combine results
# final_raster <- rast(census_acs_rast_list)


#### Census: match via raster extract #### 

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
  
  # ####### Tries with exact_extract, which is more often to extract raster values and assign to polygons
  # # Prepare output raster with the same structure
  # output_rast_list <- NULL
  # # names(output_rast_list) <- commute_variables
  # 
  # for (commute_var in commute_variables) {
  #   # Rasterize the current variable (using 'first' to assign polygon values)
  #   var_raster <- 
  #     rasterize(census_acs_year_sf, us_grid_raster, 
  #               field = commute_var, fun = "first")
  #   
  #   # Calculate weighted mean using exact_extract (coverage fraction as weights)
  #   weighted_means <- 
  #     exact_extract(var_raster, us_grid_sf, 
  #                   'weighted_mean', 
  #                   weights = 'area', 
  #                   append_cols = FALSE)
  #   
  #   # Assign results to the output raster layer
  #   output_rast_list[[commute_var]] <- weighted_means
  # }
  # 
  # # Create template raster from your grid
  # template <- rast(us_grid_raster)
  # 
  # # Convert list to raster stack
  # output_rast <- rast(
  #   lapply(commute_variables, function(var) {
  #     # Convert data.frame column to vector and assign to raster
  #     values(template) <- output_rast_list[[var]][[1]]  # Extract first (only) column
  #     return(template)
  #   })
  # )
  # 
  # # Set proper layer names
  # names(output_rast) <- commute_variables
  # summary(output_rast); head(output_rast)
  # plot(output_rast$car_truck_van)
  

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

# 
# # Get centroids
# grid_census_commute_centroid = st_centroid(grid_census_commute_all)
# 
# #
# grid_census_commute_all_fst
# # Prepare the .csv file, drop geometry
# 
# grid_census_commute_all_fst = st_drop_geometry(grid_census_commute_all)
# 
# 
# 
# # Output files
# st_write(grid_census_commute_all, 
#          file.path(census_out_path, "US_census_tract_grid_01_Commute_rasterExtract_2011-20.fgb"))
# 
# 
# 
# 
# ###### Census: match via nearest point ###### 
# 
# # file to save GEOID-geometry-year match for 2011-2020
# grid_cTract_final_all = NULL
# grid_cTract_census_all = NULL
# 
# 
# for (census_year in 2011:2020){ # census_year = 2011
#   
#   ###### Prepare basic data ###### 
#   
#   # Prepare census commute data for each year, and get geometry
#   census_acs_wide_year = subset(census_acs_wide, year == census_year)
#   census_acs_geo_year = 
#     merge(census_acs_wide_year, census_acs_geo_mainland, all.x = TRUE) %>%
#     dplyr::select(GEOID, geometry)
#   
#   # Set as sf
#   census_acs_geo_year = st_as_sf(census_acs_geo_year)
#   head(census_acs_geo_year); summary(census_acs_geo_year); dim(census_acs_geo_year)
#   
#   # Check if there is empty geometry
#   census_acs_geo_year_emptyGeo = subset(census_acs_geo_year, st_is_empty(geometry))
#   nrow(census_acs_geo_year_emptyGeo); head(census_acs_geo_year_emptyGeo)
#   
#   # Remove GEOID with empty geomety (GEOID without a geometry to match, probably outside of the mainland US)
#   census_acs_geo_year <- 
#     census_acs_geo_year %>%
#     filter(!st_is_empty(geometry))
#   
#   census_acs_use_year <- 
#     subset(census_acs_wide_year, 
#            GEOID %in% unique(census_acs_geo_year$GEOID))
#   
#   # # Checking the boundaries
#   # st_bbox(us_grid_centroids)
#   # st_bbox(census_acs_geo_year)
# 
#   ###### Match GEOID in census with geometry in US_grids: Nearest Point  ###### 
#   
#   ### Keep all GEOID and all grids (try the best)
#   # Track counts for validation
#   n_orig_tracts <- length(unique(census_acs_geo_year$GEOID))
#   n_orig_grids <- length(unique(us_grid_centroids$geometry))
#   # n_orig_tracts; n_orig_grids
#   
#   # Get base grid-tract matches from spatial join
#   base_matches <- 
#     st_join(us_grid_centroids, census_acs_geo_year, join = st_within) 
#   # head(base_matches); dim(base_matches)
#   # length(unique(base_matches$GEOID)); length(unique(base_matches$geometry))
#   
#   # Get unmatched grids and assign nearest tract
#   unmatched_grids <- 
#     base_matches %>%
#     filter(is.na(GEOID)) %>%
#     mutate(
#       GEOID = census_acs_geo_year$GEOID[st_nearest_feature(geometry, census_acs_geo_year)]
#     )
#   # head(unmatched_grids); dim(unmatched_grids)
#   # length(unique(unmatched_grids$GEOID)); length(unique(unmatched_grids$geometry))
#   
#   # Get unmatched tracts and assign nearest grid
#   unmatched_tracts <- 
#     census_acs_geo_year %>%
#     filter(!GEOID %in% base_matches$GEOID) %>%
#     mutate(
#       near_grid = st_nearest_feature(st_centroid(geometry), us_grid_centroids)
#     ) %>%
#     st_drop_geometry() %>%
#     mutate(
#       geometry = us_grid_centroids$geometry[near_grid]
#     ) %>%
#     dplyr::select(-near_grid)
#   # head(unmatched_tracts); dim(unmatched_tracts)
#   # length(unique(unmatched_tracts$GEOID)); length(unique(unmatched_tracts$geometry))
#   
#   # Combine all
#   grid_with_censusTract <- bind_rows(
#     base_matches %>% filter(!is.na(GEOID)),
#     unmatched_grids,
#     unmatched_tracts
#   )
#   
#   # head(grid_with_censusTract)
#   # # summary(grid_with_censusTract)
#   # length(unique(grid_with_censusTract$GEOID))
#   # length(unique(census_acs_geo_year$GEOID))
#   # length(unique(grid_with_censusTract$geometry))
#   # length(unique(us_grid_centroids$geometry))
#   # nrow(census_acs_geo_year); nrow(us_grid_centroids); nrow(grid_with_censusTract) 
#   # 
#   # nrow(subset(grid_with_censusTract, is.na(GEOID)))
#   # nrow(subset(grid_with_censusTract, is.na(geometry)))
#   # subset(grid_with_censusTract, !(geometry %in% us_grid_centroids$geometry))
#   
#   # Check if there is still GEOID with empty geometry (there should be NO!!!)
#   geoid_empty_geo = 
#     unique(
#       subset(grid_with_censusTract, 
#              !(geometry %in% us_grid_centroids$geometry))$GEOID)
#   head(geoid_empty_geo); length(geoid_empty_geo)
#   
#   # geoid_with_geo =
#   #   unique(
#   #     subset(grid_with_censusTract, 
#   #            geometry %in% us_grid_centroids$geometry)$GEOID)
#   # head(geoid_with_geo); length(geoid_with_geo)
#   
#   # # Check if there any GEOID with empty geometry included in that with some other grids assigned
#   # # If TRUE, then not, only need to find grid geometry for geoid_empty_geo
#   # length(geoid_with_geo) + length(geoid_empty_geo) == length(unique(census_acs_geo_year$GEOID))
#   # geoid_empty_geo = geoid_empty_geo[!(geoid_empty_geo %in% geoid_with_geo)]
#   # census_acs_year_emptyGeo = subset(census_acs_geo_year, GEOID %in% geoid_empty_geo)
#   
#   #### Handle the cases that multiple grids assigned to one GEOID; again need to keep all GEOID and grids
#   # Estimate the appearance frequency of each GEOID and geometry
#   grid_with_censusTract <- 
#     grid_with_censusTract %>%
#     group_by(GEOID) %>%
#     mutate(geoid_freq = n()) %>%
#     ungroup() %>%
#     group_by(geometry) %>%
#     mutate(geom_freq = n()) %>%
#     ungroup()
#   length(unique(grid_with_censusTract$GEOID)) # 70,208
#   length(unique(census_acs_geo_year$GEOID)) # 70,208
#   length(unique(grid_with_censusTract$geometry)) # 153,400
#   length(unique(us_grid_centroids$geometry)) # 153,400
#   
#   # # Get long lat
#   # grid_with_censusTract$Longitude = round(st_coordinates(grid_with_censusTract)[, 1], 2)
#   # grid_with_censusTract$Latitude = round(st_coordinates(grid_with_censusTract)[, 2], 2)
#   # grid_with_censusTract$coords = paste(grid_with_censusTract$Longitude, grid_with_censusTract$Latitude)
#   
#   # Identify the GEOID that only have one match, file for final use directly
#   grid_cTract_oneGEOID <-
#     subset(grid_with_censusTract, 
#            geoid_freq == 1)
#   nrow(grid_cTract_oneGEOID); length(unique(grid_cTract_oneGEOID$geometry))
#   # 59,551,  11,821
#   
#   # Identify the geometry that only have one match, but multiple GEIOD, need to handle GEOID
#   grid_cTract_oneGrid <-
#     subset(grid_with_censusTract, 
#            geoid_freq > 1 & geom_freq == 1)
#   nrow(grid_cTract_oneGrid); length(unique(grid_cTract_oneGrid$GEOID))
#   # 141,439,  10,306
#   
#   # Combine above two files
#   grid_cTract_1to1 =
#     rbind(grid_cTract_oneGEOID, grid_cTract_oneGrid)
#   length(unique(grid_cTract_1to1$geometry)); length(unique(grid_cTract_1to1$GEOID))
#   # 153,260,  69,857
#   summary(grid_cTract_1to1$geoid_freq == 1 | grid_cTract_1to1$geom_freq == 1)
#   nrow(grid_cTract_1to1) # 200,990
#   
#  # Identify the GEOID or geometry with >1 matches for both
#   grid_cTract_nMatch <-
#     subset(grid_with_censusTract,
#            geoid_freq > 1 & geom_freq > 1) 
#   nrow(grid_cTract_nMatch)
#   length(unique(grid_cTract_nMatch$geometry)); length(unique(grid_cTract_nMatch$GEOID))
#   summary(grid_cTract_nMatch)
#   
#   ### Handle those multi matches
#   ## Situation 1, both geoID & gEOmetry appear in 1to1, not to use
#   grid_cTract_nMatch_gIDgEO_in_1to1 <-
#     subset(grid_cTract_nMatch, 
#            GEOID %in% grid_cTract_1to1$GEOID & geometry %in% grid_cTract_1to1$geometry)
#   nrow(grid_cTract_nMatch_gIDgEO_in_1to1)
#   
#   ## Situation 2, geoid not but gEOmetry appear in 1to1, detect any geometry for each GEOID
#   grid_cTract_nMatch_gEO_in_1to1 <-
#     subset(grid_cTract_nMatch, 
#            !(GEOID %in% grid_cTract_1to1$GEOID) & geometry %in% grid_cTract_1to1$geometry)
#   nrow(grid_cTract_nMatch_gEO_in_1to1)
#   
#   grid_cTract_nMatch_gEO_in_1to1_single <-
#     grid_cTract_nMatch_gEO_in_1to1 %>%
#     group_by(GEOID) %>%
#     slice(1)
#   nrow(grid_cTract_nMatch_gEO_in_1to1_single)
#   
#   ## Situation 3, geometry not but geoID appear in 1to1, detect any GEOID for each geometry
#   grid_cTract_nMatch_gID_in_1to1 <-
#     subset(grid_cTract_nMatch, 
#            GEOID %in% grid_cTract_1to1$GEOID & !(geometry %in% grid_cTract_1to1$geometry))
#   nrow(grid_cTract_nMatch_gID_in_1to1)
#   
#   grid_cTract_nMatch_gID_in_1to1_single <-
#     grid_cTract_nMatch_gID_in_1to1 %>%
#     group_by(geometry) %>%
#     slice(1)
#   nrow(grid_cTract_nMatch_gID_in_1to1_single)
#   
#   ## Situation 4, neither geometry nor geoid appear in 1to1,  detect one geometry for each GEOID
#   grid_cTract_nMatch_neither_in_1to1 <-
#     subset(grid_cTract_nMatch, 
#            !(GEOID %in% grid_cTract_1to1$GEOID) &
#              !(geometry %in% grid_cTract_1to1$geometry))
#   nrow(grid_cTract_nMatch_neither_in_1to1)
#   
#   grid_cTract_nMatch_neither_in_1to1 <-
#     grid_cTract_nMatch_neither_in_1to1 %>%
#     subset(!(GEOID %in% grid_cTract_nMatch_gEO_in_1to1_single$GEOID) &
#              !(GEOID %in% grid_cTract_nMatch_gID_in_1to1_single$GEOID))
#   nrow(grid_cTract_nMatch_neither_in_1to1)
#   
#   # Get all geometry that are not used for GEOID matching 
#   rest_geometry = 
#     unique(
#       subset(grid_with_censusTract, 
#              !(geometry %in% grid_cTract_1to1$geometry | 
#                geometry %in% grid_cTract_nMatch_gID_in_1to1$geometry))$geometry)
#   length(rest_geometry)
#   
#   # For each GEOID, detect one geometry
#   # Choose the geometry that is not in rest_geometry, if possible
#   grid_cTract_nMatch_neither_in_1to1_single <-
#     grid_cTract_nMatch_neither_in_1to1 %>%
#     group_by(GEOID) %>%
#     mutate(
#       # Checks if geometry already exists in grid_cTract_oneGEOID_oneGrid
#       in_oneMatch = geometry %in% rest_geometry,
#       # Prioritize the geometry not in in_oneMatch
#       priority = if(any(!in_oneMatch)) !in_oneMatch else TRUE
#     ) %>%
#     filter(priority) %>%
#     slice(1) %>%
#     ungroup() %>%
#     dplyr::select(-in_oneMatch, -priority)
#   nrow(grid_cTract_nMatch_neither_in_1to1_single)
#   
#   ## Combine all matches
#   grid_cTract_final =
#     rbind(grid_cTract_1to1,
#           grid_cTract_nMatch_gEO_in_1to1_single,
#           grid_cTract_nMatch_gID_in_1to1_single,
#           grid_cTract_nMatch_neither_in_1to1_single)
# 
#   length(unique(grid_cTract_final$GEOID))
#   length(unique(census_acs_geo_year$GEOID))
#   length(unique(grid_cTract_final$geometry))
#   length(unique(us_grid_centroids$geometry))
#   head(grid_cTract_final); dim(grid_cTract_final)
#   summary(is.na(grid_cTract_final$GEOID))
#   summary(st_is_empty(grid_cTract_final$geometry))
#   
# 
#   # Add long & lat, and round to 2 digits
#   grid_cTract_final$Longitude = round(st_coordinates(grid_cTract_final)[,1], 2)
#   grid_cTract_final$Latitude = round(st_coordinates(grid_cTract_final)[,2], 2)
#   
#   # Exclude and add columns
#   grid_cTract_final$geoid_freq = grid_cTract_final$geom_freq = NULL
#   grid_cTract_final$year = census_year
#   grid_cTract_final = st_drop_geometry(grid_cTract_final)
#   
#   # Combine into final file for all years and output it
#   grid_cTract_final_all = rbind(grid_cTract_final_all, grid_cTract_final)
#   write_fst(grid_cTract_final_all, 
#             ## "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_census_tract_grid_01_4326_2011.fst"
#             "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_census_tract_grid_01_4326_2011-20.fst"
#             )
# 
#   ###### Match census commute info with coordinates from US grid ###### 
#   census_acs_use_year_coords =
#     merge(census_acs_use_year, grid_cTract_final, by = c("GEOID", "year"))
#   dim(census_acs_use_year_coords); dim(census_acs_use_year)
#   head(census_acs_use_year_coords)
#   
#   # Calculate median for all columns
#   census_acs_year_coords_avg =
#     dplyr::select(census_acs_use_year_coords, -GEOID) %>%
#     group_by(Longitude, Latitude) %>%
#     summarize(across(everything(), \(x) median(x)),
#               .groups = "drop") 
#   head(census_acs_year_coords_avg); dim(census_acs_year_coords_avg)
#   summary(census_acs_year_coords_avg)
#   # unique combination counts of long, lat
#   census_acs_year_coords_avg %>%
#     summarize(unique_combinations = n_distinct(paste(Longitude, Latitude)))
#   
#   # Combine into final file for all years and output it
#   grid_cTract_census_all = rbind(grid_cTract_census_all, census_acs_year_coords_avg)
#   write_fst(grid_cTract_census_all, 
#             ## "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_census_tract_grid_01_Commute_2011.fst"
#             "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_census_tract_grid_01_Commute_2011-20.fst"
#             )
# }

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

