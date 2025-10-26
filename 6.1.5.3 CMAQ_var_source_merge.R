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
library(stARS) # raster to stARS, then to sf

# Detect the number of cores available on the server
num_cores <- detectCores() - 1  # Use all but one core
num_cores

# Create a parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Working path
setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds")
getwd()

dir_path = getwd()

# Function to fill NA values with average of 2 days before and 2 days after
fill_na_with_adjacent_days <- function(df, variable_name, na_date) {
  # Convert to data.table for faster processing
  dt <- as.data.table(df)
  
  # Convert Date to proper Date format 
  na_date = as.Date(na_date)
  
  # Create a vector of the dates we want to use for averaging
  date_vector <- c(
    na_date - 2,
    na_date - 1,
    na_date + 1, 
    na_date + 2
  )
  
  # Subset the data to only include the dates we need
  reference_data <- dt[Date %in% date_vector]
  
  # Group by location and calculate average for each location
  variable_sym <- sym(variable_name)
  avg_values <- reference_data %>%
    dplyr::group_by(Longitude, Latitude) %>%
    dplyr::summarize(avg_value = mean(!!variable_sym, na.rm = TRUE),
                     .group = "drop")
  
  # Create a template of NA rows that need to be filled
  na_rows <- dt[Date == na_date]
  na_rows[, eval(variable_name) := NA]
  
  # Join the average values to the NA rows
  na_rows <- merge(na_rows, avg_values, by = c("Longitude", "Latitude"), all.x = TRUE)
  
  # Apply the calculated average value to the original variable
  na_rows[, eval(variable_name) := avg_value]
  na_rows[, avg_value := NULL]
  
  # Remove the NA rows from the original data
  clean_dt <- dt[Date != na_date]
  
  # Combine the datasets
  result_dt <- rbind(clean_dt, na_rows)
  
  # Sort by Date, Longitude, Latitude to restore original order
  setorder(result_dt, Date, Longitude, Latitude)
  
  return(result_dt)
}


#### Files for all years ####

###### For plotting ######

library(USAboundaries)
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]


###### Mainland US grids ######

# Mainland US coordinates of 0.1 * 0.1 degree
us_point_coord = 
  read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Long_lat_Mainland_US_0.1_degree.fst")
# us_point_coord = 
#   read.fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/MLresult_RF/Long_lat_Mainland_US_0.1_degree.fst")
# plot(us_point_coord$Longitude, us_point_coord$Latitude)

#### Create grid-like data
# Define the approximate bounding box for mainland U.S.
us_bbox <- c(xmin = -125, xmax = -66, ymin = 24, ymax = 50)
crs_proj <- "+proj=longlat +datum=WGS84 +no_defs"

us_grid_raster_01 = raster(file.path("base_raster_grid_sf/us_grid_raster_01.tif"))
# us_grid_raster_001 = raster(file.path("base_raster_grid_sf/us_grid_raster_001.tif"))

# us_grid_sf_01 = st_read(file.path("base_raster_grid_sf/us_grid_sf_01.fgb"))
# us_grid_sf_001 = st_read(file.path("base_raster_grid_sf/us_grid_sf_001.fgb"))

# us_grid_centroids_01 = st_read(file.path("base_raster_grid_sf/us_grid_centroids_01.fgb"))
# us_grid_centroids_001 = st_read(file.path("base_raster_grid_sf/us_grid_centroids_001.fgb"))

###### Read PMF source data for ML inputs ######

pmf_traffic = read_fst("pmf_ncld_meteo_census/PMF_F1-Traffic.fst")
pmf_sulfate = read_fst("pmf_ncld_meteo_census/PMF_F3-Secondary_Sulfate.fst")
pmf_biomass = read_fst("pmf_ncld_meteo_census/PMF_F8-Biomass.fst")
pmf_dust = read_fst("pmf_ncld_meteo_census/PMF_F9-Soil_Dust.fst")
# pmf_industry = read_fst("pmf_ncld_meteo_census/PMF_F5-Industry.fst")
pmf_nitrate = read_fst("pmf_ncld_meteo_census/PMF_F2-Secondary_Nitrate.fst")
# pmf_salt = read_fst("pmf_ncld_meteo_census/PMF_F6-Salt.fst")
# pmf_nontailpipe = read_fst("pmf_ncld_meteo_census/PMF_F7-Non-tailpipe.fst")
# pmf_oprich = read_fst("pmf_ncld_meteo_census/PMF_F10-OP-rich.fst")
pmf_pm25 = read_fst("pmf_ncld_meteo_census/PMF_total_PM2.5.fst")

# length(unique(pmf_traffic$SiteCode))
# length(unique(pmf_sulfate$SiteCode))
# length(unique(pmf_dust$SiteCode))
# length(unique(pmf_salt$SiteCode))
# length(unique(pmf_oprich$SiteCode))
length(unique(pmf_pm25$SiteCode))

###### Roadiness ######
# roadiness_with_nearest_sf =
#   st_read(file.path("base_raster_grid_sf/Roadiness_in_US_grid_01.fgb"))
# dim(roadiness_with_nearest_sf)
roadiness_us_grid_mean = 
  read_fst(file.path("base_raster_grid_sf/Roadiness_in_US_grid_01.fst"))
dim(roadiness_us_grid_mean)

roadiness_us_grid_mean$Longitude = round(roadiness_us_grid_mean$Longitude, 2)
roadiness_us_grid_mean$Latitude = round(roadiness_us_grid_mean$Latitude, 2)

#### Raodiness plotting
# Conver the df for plotting
roadiness_us_grid_mean_long =
  roadiness_us_grid_mean %>%
  pivot_longer(
    cols = sRoadLength:VMT_vs_area,
    names_to = "Roadiness_metric",
    values_to = "Roadiness_value"
    )
head(roadiness_us_grid_mean_long)

# roadiness_plot <-
#   ggplot() +
#   geom_point(data = roadiness_us_grid_mean_long,
#              aes(x = Longitude, y = Latitude, color = Roadiness_value),
#              size = 0.35, alpha = 0.8) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey70", size = 0.3) +
#   facet_wrap(~Roadiness_metric, ncol = 3) +
#   scale_color_viridis_c(option = "magma") +
#   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude",
#        title = "Roadiness Metrics") +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(size = 22),
#     legend.text = element_text(size = 19),
#     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
#     # plot.subtitle = element_text(size = 22),
#     axis.title = element_text(size = 22),
#     axis.text = element_text(size = 19)
#   )
# 
# # roadiness_plot
# ggsave(
#   file.path("machine_learning_source_input/ML_plot/Roadiness_metrics.pdf"),
#   plot = roadiness_plot, width = 14.5, height = 8.5)


###### HMS smoke ######
# Read the file
hms_smoke_all = 
  read_fst("pmf_ncld_meteo_census/HMS_US_grids_01_2011-2020_final.fst")
# hms_smoke_all$smoke_level = as.factor(hms_smoke_all$smoke_level)
sapply(hms_smoke_all, class)
head(hms_smoke_all)

# hms_smoke_all_coords = 
#   as.data.table(table(hms_smoke_all$Longitude, hms_smoke_all$Latitude))

# 2 digits
hms_smoke_all$Longitude = round(hms_smoke_all$Longitude, 2)
hms_smoke_all$Latitude = round(hms_smoke_all$Latitude, 2)

#################################################
#### Combine data for each source ####
#################################################

# All included years
cmaq_years = 2012:2013 # 2011:2020  2017:2020  2013:2014 2015:2016 2018:2020

for (cmaq_year in cmaq_years) { # cmaq_year = cmaq_years[1]; cmaq_year = 2012
  # Get the cmaq_period
  cmaq_period = 
    paste0(cmaq_year, "-01_", cmaq_year, "-12")
  cat("CMAQ_Period", cmaq_period, "& CMAQ_Year", cmaq_year)
  
  ###### CMAQ data ######
  cmaq_sulfate_rds_noExe = read_fst(file.path(paste0("base_raster_grid_sf/CMAQ_Sulfate_", cmaq_period, ".fst")))
  cmaq_dust_rds_noExe = read_fst( file.path(paste0("base_raster_grid_sf/CMAQ_Dust_", cmaq_period, ".fst")))
  cmaq_traffic_rds_noExe = read_fst(file.path(paste0("base_raster_grid_sf/CMAQ_Traffic_", cmaq_period, ".fst")))
  cmaq_biom_rds_noExe = read_fst(file.path(paste0("base_raster_grid_sf/CMAQ_Biomass_", cmaq_period, ".fst")))
  cmaq_nitrate_rds_noExe = read_fst(file.path(paste0("base_raster_grid_sf/CMAQ_Nitrate_", cmaq_period, ".fst")))
  cmaq_pm25_rds_noExe = read_fst(file.path(paste0("base_raster_grid_sf/CMAQ_PM25_", cmaq_period, ".fst")))
  # length(unique(hms_smoke_all$Date))
  # length(unique(pmf_traffic$Date))
  # length(unique(cmaq_sulfate_rds_noExe$Date))
  
  ###### ACS Census info ######
  
  # census_year_with_centroids =
  #   rast(file.path(paste0("base_raster_grid_sf/Census_commute_Extract_", cmaq_year, ".tif")))
  census_year_centroids_noGeometry =
    read_fst(file.path(paste0("base_raster_grid_sf/Census_commute_Extract_", cmaq_year, ".fst")))
  head(census_year_centroids_noGeometry); dim(census_year_centroids_noGeometry)
  
  # 2 digits
  census_year_centroids_noGeometry$Longitude = round(census_year_centroids_noGeometry$Longitude, 2)
  census_year_centroids_noGeometry$Latitude = round(census_year_centroids_noGeometry$Latitude, 2)
  
  census_year_centroids_noGeometry$cell = NULL
  summary(census_year_centroids_noGeometry)
  
  # #### Census plotting
  # census_car_truck_van =
  #   ggplot() +
  #   geom_point(data = census_year_centroids_noGeometry,
  #              aes(x = Longitude, y = Latitude, color = car_truck_van),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_c(option = "magma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "car_truck_van") +
  #   theme(
  #     strip.text = element_text(size = 20),
  #     # legend.position = "bottom",
  #     # legend.title = element_text(size = 22),
  #     # legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # 
  # census_public_transport =
  #   ggplot() +
  #   geom_point(data = census_year_centroids_noGeometry,
  #              aes(x = Longitude, y = Latitude, color = public_transport),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_c(option = "magma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "public_transport") +
  #   theme(
  #     strip.text = element_text(size = 20),
  #     # legend.position = "bottom",
  #     # legend.title = element_text(size = 22),
  #     # legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # 
  # census_bike =
  #   ggplot() +
  #   geom_point(data = census_year_centroids_noGeometry,
  #              aes(x = Longitude, y = Latitude, color = bike),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_c(option = "magma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "bike") +
  #   theme(
  #     strip.text = element_text(size = 20),
  #     # legend.position = "bottom",
  #     # legend.title = element_text(size = 22),
  #     # legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # 
  # census_walk =
  #   ggplot() +
  #   geom_point(data = census_year_centroids_noGeometry,
  #              aes(x = Longitude, y = Latitude, color = walk),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_c(option = "magma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "walk") +
  #   theme(
  #     strip.text = element_text(size = 20),
  #     # legend.position = "bottom",
  #     # legend.title = element_text(size = 22),
  #     # legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # 
  # census_taxi_moto_etc =
  #   ggplot() +
  #   geom_point(data = census_year_centroids_noGeometry,
  #              aes(x = Longitude, y = Latitude, color = taxi_moto_etc),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_c(option = "magma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "taxi_moto_etc") +
  #   theme(
  #     strip.text = element_text(size = 20),
  #     # legend.position = "bottom",
  #     # legend.title = element_text(size = 22),
  #     # legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # 
  # census_work_home =
  #   ggplot() +
  #   geom_point(data = census_year_centroids_noGeometry,
  #              aes(x = Longitude, y = Latitude, color = work_home),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_c(option = "magma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "work_home") +
  #   theme(
  #     strip.text = element_text(size = 20),
  #     # legend.position = "bottom",
  #     # legend.title = element_text(size = 22),
  #     # legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # 
  # census_commute_time =
  #   ggplot() +
  #   geom_point(data = census_year_centroids_noGeometry,
  #              aes(x = Longitude, y = Latitude, color = commute_time),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_c(option = "magma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "commute_time") +
  #   theme(
  #     strip.text = element_text(size = 20),
  #     # legend.position = "bottom",
  #     # legend.title = element_text(size = 22),
  #     # legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # 
  # 
  # # Output
  # ggsave(
  #   file.path("machine_learning_source_input/ML_plot", 
  #             paste0("Census_commute_car_truck_van_", cmaq_period, ".pdf"),
  #   plot = census_car_truck_van, width = 9, height = 6))
  # 
  # ggsave(
  #   file.path("machine_learning_source_input/ML_plot", 
  #             paste0("Census_commute_public_transport_", cmaq_period, ".pdf"),
  #             plot = census_public_transport, width = 9, height = 6))
  # 
  # ggsave(
  #   file.path("machine_learning_source_input/ML_plot", 
  #             paste0("Census_commute_bike_", cmaq_period, ".pdf"),
  #             plot = census_bike, width = 9, height = 6))
  # 
  # ggsave(
  #   file.path("machine_learning_source_input/ML_plot", 
  #             paste0("Census_commute_walk_", cmaq_period, ".pdf"),
  #             plot = census_walk, width = 9, height = 6))
  # 
  # ggsave(
  #   file.path("machine_learning_source_input/ML_plot", 
  #             paste0("Census_commute_taxi_moto_etc_", cmaq_period, ".pdf"),
  #             plot = census_taxi_moto_etc, width = 9, height = 6))
  # 
  # ggsave(
  #   file.path("machine_learning_source_input/ML_plot", 
  #             paste0("Census_commute_work_home_", cmaq_period, ".pdf"),
  #             plot = census_work_home, width = 9, height = 6))
  # 
  # ggsave(
  #   file.path("machine_learning_source_input/ML_plot", 
  #             paste0("Census_commute_commute_time_", cmaq_period, ".pdf"),
  #             plot = census_commute_time, width = 9, height = 6))
  
  ###### Common variables, GRIDMET, NCLD ###### 
  ## NCLD
  
  # Determine the NCLD data to match
  ncld_year = case_when(
    cmaq_year <= 2012 ~ 2011,
    cmaq_year <= 2014 ~ 2013,
    cmaq_year <= 2017 ~ 2016,
    cmaq_year <= 2020 ~ 2019
  )
  cat(paste0("NCLD year data use: ", ncld_year))
  
  ncld_with_centroids_coords_noGeo =
    read_fst(file.path(paste0("pmf_ncld_meteo_census/NCLD_", ncld_year, "_in_US_grid_01.fst")))
  # ncld_with_centroids_coords =
  #   st_read(file.path(paste0("pmf_ncld_meteo_census/NCLD_", ncld_year, "_in_US_grid_01.fgb")))
  
  #### GRIDMET
  gridmet_us_grid_mean = 
    read_fst(file.path(paste0("pmf_ncld_meteo_census/GRIDMET_commom_", cmaq_year, "_in_US_grid_01.fst")))
  gridmet_us_grid_mean$th[gridmet_us_grid_mean$th < 0] = 0
  gridmet_us_grid_mean = relocate(gridmet_us_grid_mean, "Date", .before = "Longitude")
  head(gridmet_us_grid_mean)
  
  ### round to 2 digits
  ncld_with_centroids_coords_noGeo$Longitude = round(ncld_with_centroids_coords_noGeo$Longitude, 2)
  ncld_with_centroids_coords_noGeo$Latitude = round(ncld_with_centroids_coords_noGeo$Latitude, 2)
  
  gridmet_us_grid_mean$Longitude = round(gridmet_us_grid_mean$Longitude, 2)
  gridmet_us_grid_mean$Latitude = round(gridmet_us_grid_mean$Latitude, 2)
  
  # ###### Replace NA by geometric mean of nearby points, for year 2012 only!!
  # ###### rmin, no data on 2012-06-22; vs, no data on 2012-02-27
  # gridmet_us_grid_mean_tobe_intp = gridmet_us_grid_mean
  # 
  # gridmet_us_grid_mean_tobe_intp = 
  #   fill_na_with_adjacent_days(gridmet_us_grid_mean_tobe_intp, "rmin", "2012-06-22")
  # gridmet_us_grid_mean_tobe_intp = 
  #   fill_na_with_adjacent_days(gridmet_us_grid_mean_tobe_intp, "vs", "2012-02-27")
  # summary(gridmet_us_grid_mean_tobe_intp)
  # dim(gridmet_us_grid_mean_tobe_intp); dim(gridmet_us_grid_mean)
  # head(gridmet_us_grid_mean_tobe_intp); head(gridmet_us_grid_mean)
  # 
  # gridmet_us_grid_mean = gridmet_us_grid_mean_tobe_intp
  # 
  # # Assign back & save
  # write_fst(gridmet_us_grid_mean,
  #           file.path(paste0("pmf_ncld_meteo_census/GRIDMET_commom_", cmaq_year, "_in_US_grid_01.fst")))
  
  # sapply(gridmet_us_grid_mean, class)
  # sapply(ncld_with_centroids_coords_noGeo, class)
  
  ### merge them
  met_ncld_us_grid = 
    merge(ncld_with_centroids_coords_noGeo, gridmet_us_grid_mean, 
          by = c("Longitude", "Latitude"), all.x = TRUE)
  met_ncld_us_grid$Date = as.Date(met_ncld_us_grid$Date)
  
  head(met_ncld_us_grid); dim(met_ncld_us_grid)
  # summary(met_ncld_us_grid)
  # length(unique(met_ncld_us_grid$Longitude, met_ncld_us_grid$Latitude))
  
  #### Land cover
  # land cover labels for match 
  landcover_labels <- c(
    "0" = "Unclassified",
    "NA" = "Unclassified",
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
  
  #### Plotting: GRIDMET & NCLD Mapping
  met_ncld_us_grid_use = subset(met_ncld_us_grid, !is.na(th))
  met_ncld_us_grid_use$long = met_ncld_us_grid_use$lat = NULL
  # summary(met_ncld_us_grid_use)
  head(met_ncld_us_grid_use)
  
  met_ncld_us_grid_plot =
    subset(met_ncld_us_grid_use,
           Date == unique(met_ncld_us_grid_use$Date)[1])
  
  ##### Land cover
  landcover_df$NLCD.Land.Cover.Class = as.factor(landcover_df$NLCD.Land.Cover.Class)
  head(landcover_df); dim(landcover_df)
  
  ## NLCD Land Use
  met_ncld_us_grid_plot =
    merge(met_ncld_us_grid_plot, landcover_df,
          by = "NLCD.Land.Cover.Class", all.x = TRUE)
  
  # land_use <-
  #   ggplot() +
  #   geom_point(data = met_ncld_us_grid_plot,
  #              aes(x = Longitude, y = Latitude, color = land_type),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_d(option = "plasma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "land_type") +
  #   theme(
  #     legend.position = "bottom",
  #     legend.title = element_text(size = 22),
  #     legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     # plot.subtitle = element_text(size = 22),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # # land_use
  # 
  # ggsave(
  #   file.path("machine_learning_source_input/ML_plot", "GridMet_NLCD_2012_Land_Use.pdf"),
  #   plot = land_use, width = 14.5, height = 8.5)
  # 
  # met_ncld_us_grid_plot =
  #   dplyr::select(met_ncld_us_grid_use, -Date) %>%
  #   dplyr::group_by(Longitude, Latitude) %>%
  #   dplyr::summarise(
  #     NLCD.Land.Cover.Class = median(NLCD.Land.Cover.Class, na.rm = TRUE),
  #     tmmx = median(tmmx, na.rm = TRUE),
  #     tmmn = median(tmmn, na.rm = TRUE),
  #     rmax = median(rmax, na.rm = TRUE),
  #     rmin = median(rmin, na.rm = TRUE),
  #     vs = median(vs, na.rm = TRUE),
  #     th = median(th, na.rm = TRUE)
  #   )
  # head(met_ncld_us_grid_plot)
  # summary(met_ncld_us_grid_plot)
  # 
  # #### Temp
  # met_tmmx <-
  #   ggplot() +
  #   geom_point(data = met_ncld_us_grid_plot,
  #              aes(x = Longitude, y = Latitude, color = tmmx),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_c(option = "plasma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "tmmx") +
  #   theme(
  #     legend.position = "bottom",
  #     legend.title = element_text(size = 22),
  #     legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     # plot.subtitle = element_text(size = 22),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # # met_tmmx
  # 
  # met_tmmn <-
  #   ggplot() +
  #   geom_point(data = met_ncld_us_grid_plot,
  #              aes(x = Longitude, y = Latitude, color = tmmn),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_c(option = "plasma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "tmmn") +
  #   theme(
  #     legend.position = "bottom",
  #     legend.title = element_text(size = 22),
  #     legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     # plot.subtitle = element_text(size = 22),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # # met_tmmn
  # met_Temp = met_tmmx + met_tmmn
  # 
  # temp_name = paste0("METEO_map_Temp_", cmaq_year, ".pdf"); temp_name
  # ggsave(
  #   file.path("machine_learning_source_input/ML_plot", temp_name),
  #   plot = met_Temp, width = 14.5, height = 8.5)
  # 
  # #### RH
  # met_rmax <-
  #   ggplot() +
  #   geom_point(data = met_ncld_us_grid_plot,
  #              aes(x = Longitude, y = Latitude, color = rmax),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_c(option = "plasma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "rmax") +
  #   theme(
  #     legend.position = "bottom",
  #     legend.title = element_text(size = 22),
  #     legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     # plot.subtitle = element_text(size = 22),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # # met_rmax
  # 
  # met_rmin <-
  #   ggplot() +
  #   geom_point(data = met_ncld_us_grid_plot,
  #              aes(x = Longitude, y = Latitude, color = rmin),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_c(option = "plasma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "rmin") +
  #   theme(
  #     legend.position = "bottom",
  #     legend.title = element_text(size = 22),
  #     legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     # plot.subtitle = element_text(size = 22),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # # met_rmin
  # met_RH = met_rmax + met_rmin
  # 
  # rh_name = paste0("METEO_map_RH_", cmaq_year, ".pdf"); rh_name
  # ggsave(
  #   file.path("machine_learning_source_input/ML_plot", rh_name),
  #   plot = met_RH, width = 14.5, height = 8.5)
  # 
  # #### Wind
  # met_th <-
  #   ggplot() +
  #   geom_point(data = met_ncld_us_grid_plot,
  #              aes(x = Longitude, y = Latitude, color = th),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_c(option = "plasma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "th") +
  #   theme(
  #     legend.position = "bottom",
  #     legend.title = element_text(size = 22),
  #     legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     # plot.subtitle = element_text(size = 22),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # # met_th
  # 
  # met_vs <-
  #   ggplot() +
  #   geom_point(data = met_ncld_us_grid_plot,
  #              aes(x = Longitude, y = Latitude, color = vs),
  #              size = 0.35, alpha = 0.8) +
  #   geom_sf(data = us_states,
  #           fill = NA, color = "grey70", size = 0.3) +
  #   scale_color_viridis_c(option = "plasma") +
  #   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  #   theme_minimal(base_size = 16) +
  #   labs(x = "Longitude",
  #        y = "Latitude",
  #        title = "vs") +
  #   theme(
  #     legend.position = "bottom",
  #     legend.title = element_text(size = 22),
  #     legend.text = element_text(size = 19),
  #     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
  #     # plot.subtitle = element_text(size = 22),
  #     axis.title = element_text(size = 22),
  #     axis.text = element_text(size = 19)
  #   )
  # # met_vs
  # met_Wind = met_th + met_vs
  # 
  # wind_name = paste0("METEO_map_Wind_", cmaq_year, ".pdf"); wind_name
  # ggsave(
  #   file.path("machine_learning_source_input/ML_plot", wind_name),
  #   plot = met_Wind, width = 14.5, height = 8.5)
  
  ###### Sulfate ######
  cat("Process source: Sulfate")

  setDT(cmaq_sulfate_rds_noExe)
  cmaq_sulfate_use = cmaq_sulfate_rds_noExe

  ### round to 2 digits
  pmf_sulfate$Longitude = round(pmf_sulfate$Longitude, 2)
  pmf_sulfate$Latitude = round(pmf_sulfate$Latitude, 2)

  cmaq_sulfate_use$Longitude = round(cmaq_sulfate_use$Longitude, 2)
  cmaq_sulfate_use$Latitude = round(cmaq_sulfate_use$Latitude, 2)

  # # Only use dates in PMF and CMAQ both
  # dim(cmaq_sulfate_use)
  # cmaq_sulfate_use = subset(cmaq_sulfate_use,
  #                           Date %in% pmf_sulfate$Date)
  # dim(cmaq_sulfate_use)
  # 
  # pmf_sulfate_use = subset(pmf_sulfate,
  #                          Date %in% cmaq_sulfate_use$Date)
  # 
  # # Merge PMF & CMAQ results, keep all points in CMAQ, and keep all sites in PMF with same grid coordinates
  # cmaq_pmf_sulfate =
  #   merge(cmaq_sulfate_use, pmf_sulfate_use,
  #         by = c("Date", "Longitude", "Latitude"),
  #         all.x = TRUE, all.y = TRUE)
  # Merge PMF & CMAQ results, keep all points and all dates in CMAQ 
  pmf_sulfate_use = subset(pmf_sulfate,
                           Date %in% cmaq_sulfate_use$Date)
  cmaq_pmf_sulfate =
    merge(cmaq_sulfate_use, pmf_sulfate_use,
          by = c("Date", "Longitude", "Latitude"),
          all.x = TRUE, all.y = TRUE)

  head(cmaq_pmf_sulfate)
  dim(cmaq_pmf_sulfate); dim(pmf_sulfate_use); dim(cmaq_sulfate_use)
  # table(cmaq_pmf_sulfate$Date)

  # Merge with meteorology & NCLD
  cmaq_pmf_sulfate_met_ncld =
    merge(cmaq_pmf_sulfate, met_ncld_us_grid,
          by = c("Date", "Longitude", "Latitude"), all.x = TRUE)

  cmaq_pmf_sulfate_met_ncld$NLCD.Land.Cover.Class =
    as.factor(cmaq_pmf_sulfate_met_ncld$NLCD.Land.Cover.Class)

  # head(cmaq_pmf_sulfate_met_ncld)
  # summary(cmaq_pmf_sulfate_met_ncld)

  # Merge with land use type
  sulfate_cmaq_pmf_met_landtype =
    merge(cmaq_pmf_sulfate_met_ncld, landcover_df,
          by = "NLCD.Land.Cover.Class", all.x = TRUE)
  sulfate_cmaq_pmf_met_landtype$NLCD.Land.Cover.Class = NULL
  # caret only deal with factor and numeric, not characters
  sulfate_cmaq_pmf_met_landtype$land_type =
    factor(sulfate_cmaq_pmf_met_landtype$land_type, ordered = FALSE)

  # For caret::train(method = "rf")
  sulfate_rf_use =
    subset(sulfate_cmaq_pmf_met_landtype, !is.na(Concentration))
  summary(sulfate_rf_use)
  length(unique(sulfate_rf_use$SiteCode))

  dim(sulfate_cmaq_pmf_met_landtype); dim(sulfate_rf_use)

  summary(sulfate_cmaq_pmf_met_landtype)
  # table(sulfate_cmaq_pmf_met_landtype$land_type)
  summary(sulfate_rf_use)

  ##### Check if there is NAs to decide if such NAs can be removed
  # if not delete ALL data from one point from mainland us
  subset(sulfate_rf_use, is.na(PM25_TOT_EGU))

  sulfate_rf_use = subset(sulfate_rf_use, !is.na(PM25_TOT_EGU))
  sulfate_cmaq_pmf_met_landtype = subset(sulfate_cmaq_pmf_met_landtype, !is.na(PM25_TOT_EGU))
  dim(sulfate_cmaq_pmf_met_landtype); dim(sulfate_rf_use)

  # write_fst(sulfate_cmaq_pmf_met_landtype,
  #           file.path(
  #             paste0("machine_learning_source_input/Sulfate_all_CMAQ_points_input_", cmaq_period, ".fst")))
  # 
  # write_fst(sulfate_rf_use,
  #           file.path(
  #             paste0("machine_learning_source_input/Sulfate_only_PMF_points_input_", cmaq_period, ".fst")))

  write_fst(sulfate_cmaq_pmf_met_landtype,
            file.path(
              paste0("machine_learning_source_input/Sulfate_all_CMAQ_points_Daily_", cmaq_period, ".fst")))
  
  write_fst(sulfate_rf_use,
            file.path(
              paste0("machine_learning_source_input/Sulfate_only_PMF_points_Daily_", cmaq_period, ".fst")))
  
  # sulfate_cmaq_pmf_met_landtype = read_fst(
  #   file.path(
  #     paste0("machine_learning_source_input/Sulfate_all_CMAQ_points_input_", cmaq_period, ".fst")))
  # head(sulfate_cmaq_pmf_met_landtype); dim(sulfate_cmaq_pmf_met_landtype)

  rm(cmaq_sulfate_rds_noExe) 
  # rm(pmf_sulfate) 
  rm(cmaq_sulfate_use) 
  rm(pmf_sulfate_use) 
  rm(cmaq_pmf_sulfate)
  rm(cmaq_pmf_sulfate_met_ncld)
  rm(sulfate_cmaq_pmf_met_landtype)
  rm(sulfate_rf_use)
  
  ###### Nitrate ######
  cat("Process source: Nitrate")

  setDT(cmaq_nitrate_rds_noExe)
  cmaq_nitrate_use = cmaq_nitrate_rds_noExe

  ### round to 2 digits
  pmf_nitrate$Longitude = round(pmf_nitrate$Longitude, 2)
  pmf_nitrate$Latitude = round(pmf_nitrate$Latitude, 2)

  cmaq_nitrate_use$Longitude = round(cmaq_nitrate_use$Longitude, 2)
  cmaq_nitrate_use$Latitude = round(cmaq_nitrate_use$Latitude, 2)

  # # Only use dates in PMF and CMAQ both
  # dim(cmaq_nitrate_use)
  # cmaq_nitrate_use = subset(cmaq_nitrate_use,
  #                           Date %in% pmf_nitrate$Date)
  # dim(cmaq_nitrate_use)
  # 
  # pmf_nitrate_use = subset(pmf_nitrate,
  #                          Date %in% cmaq_nitrate_use$Date)
  # 
  # # Merge PMF & CMAQ results, keep all points in CMAQ, and keep all sites in PMF with same grid coordinates
  # cmaq_pmf_nitrate =
  #   merge(cmaq_nitrate_use, pmf_nitrate_use,
  #         by = c("Date", "Longitude", "Latitude"),
  #         all.x = TRUE, all.y = TRUE)

  # Merge PMF & CMAQ results, keep all points and all dates in CMAQ 
  pmf_nitrate_use = subset(pmf_nitrate,
                           Date %in% cmaq_nitrate_use$Date)
  cmaq_pmf_nitrate =
    merge(cmaq_nitrate_use, pmf_nitrate_use,
          by = c("Date", "Longitude", "Latitude"),
          all.x = TRUE, all.y = TRUE)
  
  head(cmaq_pmf_nitrate)
  dim(cmaq_pmf_nitrate); dim(pmf_nitrate_use); dim(cmaq_nitrate_use)
  # table(cmaq_pmf_nitrate$Date)

  # Merge with meteorology & NCLD
  cmaq_pmf_nitrate_met_ncld =
    merge(cmaq_pmf_nitrate, met_ncld_us_grid,
          by = c("Date", "Longitude", "Latitude"), all.x = TRUE)

  cmaq_pmf_nitrate_met_ncld$NLCD.Land.Cover.Class =
    as.factor(cmaq_pmf_nitrate_met_ncld$NLCD.Land.Cover.Class)

  # head(cmaq_pmf_nitrate_met_ncld)
  # summary(cmaq_pmf_nitrate_met_ncld)

  # Merge with land use type
  nitrate_cmaq_pmf_met_landtype =
    merge(cmaq_pmf_nitrate_met_ncld, landcover_df,
          by = "NLCD.Land.Cover.Class", all.x = TRUE)
  nitrate_cmaq_pmf_met_landtype$NLCD.Land.Cover.Class = NULL
  # caret only deal with factor and numeric, not characters
  nitrate_cmaq_pmf_met_landtype$land_type =
    factor(nitrate_cmaq_pmf_met_landtype$land_type, ordered = FALSE)

  # For caret::train(method = "rf")
  nitrate_rf_use =
    subset(nitrate_cmaq_pmf_met_landtype, !is.na(Concentration))
  summary(nitrate_rf_use)
  length(unique(nitrate_rf_use$SiteCode))

  dim(nitrate_cmaq_pmf_met_landtype); dim(nitrate_rf_use)

  summary(nitrate_cmaq_pmf_met_landtype)
  # table(nitrate_cmaq_pmf_met_landtype$land_type)
  summary(nitrate_rf_use)

  ##### Check if there is NAs to decide if such NAs can be removed
  # if not delete ALL data from one point from mainland us
  subset(nitrate_rf_use, is.na(PM25_TOT_EGU))

  nitrate_rf_use = subset(nitrate_rf_use, !is.na(PM25_TOT_EGU))
  nitrate_cmaq_pmf_met_landtype = subset(nitrate_cmaq_pmf_met_landtype, !is.na(PM25_TOT_EGU))
  dim(nitrate_cmaq_pmf_met_landtype); dim(nitrate_rf_use)

  # write_fst(nitrate_cmaq_pmf_met_landtype,
  #           file.path(
  #             paste0("machine_learning_source_input/Nitrate_all_CMAQ_points_input_", cmaq_period, ".fst")))
  # 
  # write_fst(nitrate_rf_use,
  #           file.path(
  #             paste0("machine_learning_source_input/Nitrate_only_PMF_points_input_", cmaq_period, ".fst")))

  write_fst(nitrate_cmaq_pmf_met_landtype,
            file.path(
              paste0("machine_learning_source_input/Nitrate_all_CMAQ_points_Daily_", cmaq_period, ".fst")))
  
  write_fst(nitrate_rf_use,
            file.path(
              paste0("machine_learning_source_input/Nitrate_only_PMF_points_Daily_", cmaq_period, ".fst")))
  
  # nitrate_cmaq_pmf_met_landtype = read_fst(
  #   file.path(
  #     paste0("machine_learning_source_input/nitrate_all_CMAQ_points_input_", cmaq_period, ".fst")))
  # head(nitrate_cmaq_pmf_met_landtype); dim(nitrate_cmaq_pmf_met_landtype)

  rm(cmaq_nitrate_rds_noExe) 
  # rm(pmf_nitrate) 
  rm(cmaq_nitrate_use) 
  rm(pmf_nitrate_use) 
  rm(cmaq_pmf_nitrate)
  rm(cmaq_pmf_nitrate_met_ncld)
  rm(nitrate_cmaq_pmf_met_landtype)
  rm(nitrate_rf_use)
  
  ###### Dust ######
  cat("Process source: Dust")

  setDT(cmaq_dust_rds_noExe)
  cmaq_dust_use = cmaq_dust_rds_noExe

  ### round to 2 digits
  pmf_dust$Longitude = round(pmf_dust$Longitude, 2)
  pmf_dust$Latitude = round(pmf_dust$Latitude, 2)

  cmaq_dust_use$Longitude = round(cmaq_dust_use$Longitude, 2)
  cmaq_dust_use$Latitude = round(cmaq_dust_use$Latitude, 2)

  # # Only use dates in PMF & CMAQ both
  # dim(cmaq_dust_use)
  # cmaq_dust_use = subset(cmaq_dust_use,
  #                        Date %in% pmf_dust$Date)
  # dim(cmaq_dust_use)
  # 
  # pmf_dust_use = subset(pmf_dust,
  #                       Date %in% cmaq_dust_use$Date)
  # 
  # # Merge PMF & CMAQ results, keep all points in CMAQ, and keep all sites in PMF with same grid coordinates
  # cmaq_pmf_dust =
  #   merge(cmaq_dust_use, pmf_dust_use,
  #         by = c("Date", "Longitude", "Latitude"),
  #         all.x = TRUE, all.y = TRUE)

  # Merge PMF & CMAQ results, keep all points and all dates in CMAQ 
  pmf_dust_use = subset(pmf_dust,
                           Date %in% cmaq_dust_use$Date)
  cmaq_pmf_dust =
    merge(cmaq_dust_use, pmf_dust_use,
          by = c("Date", "Longitude", "Latitude"),
          all.x = TRUE, all.y = TRUE)
  
  head(cmaq_pmf_dust)
  dim(cmaq_pmf_dust); dim(pmf_dust); dim(cmaq_dust_use)
  # table(cmaq_pmf_dust$Date)

  # Merge with meteorology & NCLD
  cmaq_pmf_dust_met_ncld =
    merge(cmaq_pmf_dust, met_ncld_us_grid,
          by = c("Date", "Longitude", "Latitude"), all.x = TRUE)

  cmaq_pmf_dust_met_ncld$NLCD.Land.Cover.Class =
    as.factor(cmaq_pmf_dust_met_ncld$NLCD.Land.Cover.Class)

  # Merge with land use type
  dust_cmaq_pmf_met_landtype =
    merge(cmaq_pmf_dust_met_ncld, landcover_df,
          by = "NLCD.Land.Cover.Class", all.x = TRUE)
  dust_cmaq_pmf_met_landtype$NLCD.Land.Cover.Class = NULL
  # caret only deal with factor and numeric, not characters
  dust_cmaq_pmf_met_landtype$land_type =
    factor(dust_cmaq_pmf_met_landtype$land_type, ordered = FALSE)

  # For caret::train(method = "rf")
  dust_rf_use =
    subset(dust_cmaq_pmf_met_landtype, !is.na(Concentration))
  summary(dust_rf_use)
  length(unique(dust_rf_use$SiteCode))

  dim(dust_cmaq_pmf_met_landtype); dim(dust_rf_use)
  summary(dust_cmaq_pmf_met_landtype); summary(dust_rf_use)

  ##### Check if there is NAs to decide if such NAs can be removed
  # if not delete ALL data from one point from mainland us
  subset(dust_rf_use, is.na(PM25_TOT_ARS))


  # write_fst(dust_cmaq_pmf_met_landtype,
  #           file.path(
  #             paste0("machine_learning_source_input/Dust_all_CMAQ_points_input_", cmaq_period, ".fst")))
  # 
  # write_fst(dust_rf_use,
  #           file.path(
  #             paste0("machine_learning_source_input/Dust_only_PMF_points_input_", cmaq_period, ".fst")))

  write_fst(dust_cmaq_pmf_met_landtype,
            file.path(
              paste0("machine_learning_source_input/Dust_all_CMAQ_points_Daily_", cmaq_period, ".fst")))
  
  write_fst(dust_rf_use,
            file.path(
              paste0("machine_learning_source_input/Dust_only_PMF_points_Daily_", cmaq_period, ".fst")))
  
  
  rm(cmaq_dust_rds_noExe) 
  # rm(pmf_dust) 
  rm(cmaq_dust_use) 
  rm(pmf_dust_use) 
  rm(cmaq_pmf_dust)
  rm(cmaq_pmf_dust_met_ncld)
  rm(dust_cmaq_pmf_met_landtype)
  rm(dust_rf_use)
  
  ###### Traffic ######
  cat("Process source: Traffic")

  setDT(cmaq_traffic_rds_noExe)
  cmaq_traffic_use = cmaq_traffic_rds_noExe

  ### round to 2 digits
  pmf_traffic$Longitude = round(pmf_traffic$Longitude, 2)
  pmf_traffic$Latitude = round(pmf_traffic$Latitude, 2)

  cmaq_traffic_use$Longitude = round(cmaq_traffic_use$Longitude, 2)
  cmaq_traffic_use$Latitude = round(cmaq_traffic_use$Latitude, 2)

  # # Get all coordinates in PMF & CMAQ dataset
  # pmf_traffic_coords =
  #   data.frame(Longitude = pmf_traffic$Longitude,
  #              Latitude = pmf_traffic$Latitude)
  # cmaq_traffic_coords =
  #   data.frame(Longitude = cmaq_traffic_use$Longitude,
  #              Latitude = cmaq_traffic_use$Latitude)
  #
  # # Check if all PMF sites are included in the CMAQ grids
  # pmf_traffic_coords = pmf_traffic_coords[!duplicated(pmf_traffic_coords), ]
  # cmaq_traffic_coords = cmaq_traffic_coords[!duplicated(cmaq_traffic_coords), ]
  # dim(pmf_traffic_coords); dim(cmaq_traffic_coords)
  #
  # summary(unique(pmf_traffic_coords$Longitude) %in% unique(cmaq_traffic_coords$Longitude) )
  # summary(unique(pmf_traffic_coords$Latitude) %in% unique(cmaq_traffic_coords$Latitude) )

  # # Only use dates in PMF & CMAQ both
  # dim(cmaq_traffic_use)
  # cmaq_traffic_use = subset(cmaq_traffic_use,
  #                           Date %in% pmf_traffic$Date)
  # dim(cmaq_traffic_use)
  # 
  # pmf_traffic_use = subset(pmf_traffic,
  #                          Date %in% cmaq_traffic_use$Date)
  # 
  # # Merge PMF & CMAQ results, keep all points in CMAQ, and keep all sites in PMF with same grid coordinates
  # cmaq_pmf_traffic =
  #   merge(cmaq_traffic_use, pmf_traffic_use,
  #         by = c("Date", "Longitude", "Latitude"),
  #         all.x = TRUE, all.y = TRUE)

  # Merge PMF & CMAQ results, keep all points and all dates in CMAQ 
  pmf_traffic_use = subset(pmf_traffic,
                           Date %in% cmaq_traffic_use$Date)
  cmaq_pmf_traffic =
    merge(cmaq_traffic_use, pmf_traffic_use,
          by = c("Date", "Longitude", "Latitude"),
          all.x = TRUE, all.y = TRUE)
  
  head(cmaq_pmf_traffic)
  dim(cmaq_pmf_traffic); dim(pmf_traffic); dim(cmaq_traffic_use)
  # table(cmaq_pmf_traffic$Date)

  length(unique(pmf_traffic$SiteCode)) # 249
  length(unique(cmaq_pmf_traffic$SiteCode)) # 212

  # ### Check if all PMF sites and all CMAQ grids are kept
  # # Check PMF sites with duplicated Grid coordinates
  # pmf_traffic_dup_counts <- table(paste(pmf_traffic$Date, pmf_traffic$Longitude, pmf_traffic$Latitude))
  # pmf_traffic_dup_counts = data.frame(pmf_traffic_dup_counts[pmf_traffic_dup_counts > 1])
  # dim(pmf_traffic_dup_counts)
  #
  # # Check PMF_CMAQ combined file sites with duplicated Grid coordinates
  # cmaq_pmf_traffic_dup_counts <-
  #   table(
  #     paste(cmaq_pmf_traffic$Date,
  #           cmaq_pmf_traffic$Longitude,
  #           cmaq_pmf_traffic$Latitude))
  # cmaq_pmf_traffic_dup_counts =
  #   data.frame(cmaq_pmf_traffic_dup_counts[cmaq_pmf_traffic_dup_counts > 1])
  #
  # # If same dim, then all PMF sites are included in the combined file
  # dim(cmaq_pmf_traffic_dup_counts)
  # dim(pmf_traffic_dup_counts)

  # Merge with meteorology & NCLD
  cmaq_pmf_traffic_met_ncld =
    merge(cmaq_pmf_traffic, met_ncld_us_grid,
          by = c("Date", "Longitude", "Latitude"), all.x = TRUE)

  cmaq_pmf_traffic_met_ncld$NLCD.Land.Cover.Class =
    as.factor(cmaq_pmf_traffic_met_ncld$NLCD.Land.Cover.Class)

  # Merge with land use type
  traffic_cmaq_pmf_met_landtype =
    merge(cmaq_pmf_traffic_met_ncld, landcover_df,
          by = "NLCD.Land.Cover.Class", all.x = TRUE)
  traffic_cmaq_pmf_met_landtype$NLCD.Land.Cover.Class = NULL
  # caret only deal with factor and numeric, not characters
  traffic_cmaq_pmf_met_landtype$land_type =
    factor(traffic_cmaq_pmf_met_landtype$land_type, ordered = FALSE)

  # Merge with roadiness
  traffic_cmaq_pmf_met_landtype_road =
    merge(traffic_cmaq_pmf_met_landtype, roadiness_us_grid_mean,
          # dplyr::select(roadiness_us_grid_mean, -long, -lat),
          by = c("Longitude", "Latitude"), all.x = TRUE)

  # Merge with census
  traffic_cmaq_pmf_met_landtype_road_census =
    merge(traffic_cmaq_pmf_met_landtype_road, census_year_centroids_noGeometry,
          by = c("Longitude", "Latitude"), all.x = TRUE)
  dim(traffic_cmaq_pmf_met_landtype_road); dim(traffic_cmaq_pmf_met_landtype_road_census)

  # For caret::train(method = "rf")
  traffic_rf_use =
    subset(traffic_cmaq_pmf_met_landtype_road_census,
           !is.na(Concentration) & !is.na(work_home))
  dim(traffic_rf_use)
  # length(unique(traffic_rf_use$SiteCode))

  dim(traffic_cmaq_pmf_met_landtype_road_census); dim(traffic_rf_use)
  head(traffic_cmaq_pmf_met_landtype_road_census)

  summary(traffic_rf_use); summary(traffic_cmaq_pmf_met_landtype_road_census)
  paste0("machine_learning_source_input/Traffic_all_CMAQ_points_input_", cmaq_period, ".fst")

  ##### Check if there is NAs to decide if such NAs can be removed
  # if not delete ALL data from one point from mainland us
  subset(traffic_rf_use, is.na(PM25_TOT_NRD))

  traffic_rf_use = subset(traffic_rf_use, !is.na(PM25_TOT_NRD))
  traffic_cmaq_pmf_met_landtype = subset(traffic_cmaq_pmf_met_landtype, !is.na(PM25_TOT_NRD))
  summary(traffic_cmaq_pmf_met_landtype); summary(traffic_rf_use)

  # Output files
  # write_fst(traffic_cmaq_pmf_met_landtype_road_census,
  #           file.path(
  #             paste0("machine_learning_source_input/Traffic_all_CMAQ_points_input_", cmaq_period, ".fst")))
  # 
  # write_fst(traffic_rf_use,
  #           file.path(
  #             paste0("machine_learning_source_input/Traffic_only_PMF_points_input_", cmaq_period, ".fst")))


  write_fst(traffic_cmaq_pmf_met_landtype_road_census,
            file.path(
              paste0("machine_learning_source_input/Traffic_all_CMAQ_points_Daily_", cmaq_period, ".fst")))
  
  write_fst(traffic_rf_use,
            file.path(
              paste0("machine_learning_source_input/Traffic_only_PMF_points_Daily_", cmaq_period, ".fst")))
  
  
  rm(cmaq_traffic_rds_noExe) 
  # rm(pmf_traffic) 
  rm(cmaq_traffic_use) 
  rm(pmf_traffic_use) 
  rm(cmaq_pmf_traffic)
  rm(cmaq_pmf_traffic_met_ncld)
  rm(traffic_cmaq_pmf_met_landtype)
  rm(traffic_cmaq_pmf_met_landtype_road)
  rm(traffic_cmaq_pmf_met_landtype_road_census)
  rm(traffic_rf_use)
  
  ###### Biomass ######
  cat("Process source: Biomass")

  setDT(cmaq_biom_rds_noExe)
  cmaq_biomass_use = cmaq_biom_rds_noExe

  summary(unique(hms_smoke_use$Longitude) %in% unique(cmaq_biomass_use$Longitude))
  summary(unique(hms_smoke_use$Latitude) %in% unique(cmaq_biomass_use$Latitude))

  ### round to 2 digits
  pmf_biomass$Longitude = round(pmf_biomass$Longitude, 2)
  pmf_biomass$Latitude = round(pmf_biomass$Latitude, 2)

  cmaq_biomass_use$Longitude = round(cmaq_biomass_use$Longitude, 2)
  cmaq_biomass_use$Latitude = round(cmaq_biomass_use$Latitude, 2)

  # # Only use dates in PMF and CMAQ both
  # dim(cmaq_biomass_use)
  # cmaq_biomass_use = subset(cmaq_biomass_use,
  #                           Date %in% pmf_biomass$Date)
  # dim(cmaq_biomass_use)
  # 
  # pmf_biomass_use = subset(pmf_biomass,
  #                          Date %in% cmaq_biomass_use$Date)
  # 
  # # Merge PMF & CMAQ results, keep all points in CMAQ, and keep all sites in PMF with same grid coordinates
  # cmaq_pmf_biomass =
  #   merge(cmaq_biomass_use, pmf_biomass_use,
  #         by = c("Date", "Longitude", "Latitude"),
  #         all.x = TRUE, all.y = TRUE)

  # Merge PMF & CMAQ results, keep all points and all dates in CMAQ 
  pmf_biomass_use = subset(pmf_biomass,
                           Date %in% cmaq_biomass_use$Date)
  cmaq_pmf_biomass =
    merge(cmaq_biomass_use, pmf_biomass_use,
          by = c("Date", "Longitude", "Latitude"),
          all.x = TRUE, all.y = TRUE)
  
  head(cmaq_pmf_biomass)
  dim(cmaq_pmf_biomass); dim(pmf_biomass_use); dim(cmaq_biomass_use)
  # table(cmaq_pmf_biomass$Date)

  # Merge with meteorology & NCLD
  cmaq_pmf_biomass_met_ncld =
    merge(cmaq_pmf_biomass, met_ncld_us_grid,
          by = c("Date", "Longitude", "Latitude"), all.x = TRUE)

  cmaq_pmf_biomass_met_ncld$NLCD.Land.Cover.Class =
    as.factor(cmaq_pmf_biomass_met_ncld$NLCD.Land.Cover.Class)

  # Merge with land use type
  biomass_cmaq_pmf_met_landtype =
    merge(cmaq_pmf_biomass_met_ncld, landcover_df,
          by = "NLCD.Land.Cover.Class", all.x = TRUE)
  biomass_cmaq_pmf_met_landtype$NLCD.Land.Cover.Class = NULL
  # caret only deal with factor and numeric, not characters
  biomass_cmaq_pmf_met_landtype$land_type =
    factor(biomass_cmaq_pmf_met_landtype$land_type, ordered = FALSE)

  ### Merge with HMS Smoke file
  # Extract the date to use
  hms_smoke_use =
    subset(hms_smoke_all, Date %in% biomass_cmaq_pmf_met_landtype$Date)
  summary(hms_smoke_use)

  biomass_cmaq_pmf_met_landtype_smk =
    merge(biomass_cmaq_pmf_met_landtype, hms_smoke_use,
          by = c("Date", "Longitude", "Latitude"), all.x = TRUE)

  # Set NA in smoke_level as 0, NA is due to no record on that day
  biomass_cmaq_pmf_met_landtype_smk[is.na(smoke_level), smoke_level := 0]

  # Set smoke_level as factor
  biomass_cmaq_pmf_met_landtype_smk$smoke_level =
    as.factor(biomass_cmaq_pmf_met_landtype_smk$smoke_level)
  # unique(biomass_cmaq_pmf_met_landtype_smk$Date)[1:50]
  
  ### Smoke related meteorological variables
  gridmet_bio =
    read_fst(file.path(paste0("pmf_ncld_meteo_census/GRIDMET_Biomass_", cmaq_year, "_in_US_grid_01.fst")))
  summary(gridmet_bio)
  gridmet_bio$Date = as.Date(gridmet_bio$Date)
  head(gridmet_bio); sapply(gridmet_bio, class)

  # 2 digits
  gridmet_bio$Longitude = round(gridmet_bio$Longitude, 2)
  gridmet_bio$Latitude = round(gridmet_bio$Latitude, 2)

  gridmet_bio_use =
    subset(gridmet_bio, Date %in% biomass_cmaq_pmf_met_landtype_smk$Date)
  dim(gridmet_bio); dim(gridmet_bio_use)

  biomass_cmaq_pmf_met_landtype_smk_smkMet =
    merge(biomass_cmaq_pmf_met_landtype_smk, gridmet_bio_use,
          by = c("Date", "Longitude", "Latitude"), all.x = TRUE)

  # For caret::train(method = "rf")
  biomass_rf_use =
    subset(biomass_cmaq_pmf_met_landtype_smk_smkMet, !is.na(Concentration))
  length(unique(biomass_rf_use$SiteCode))

  dim(biomass_cmaq_pmf_met_landtype_smk_smkMet); dim(biomass_rf_use)

  summary(biomass_cmaq_pmf_met_landtype_smk_smkMet)
  # table(biomass_cmaq_pmf_met_landtype_smk_smkMet$land_type)
  summary(biomass_rf_use)

  ##### Check if there is NAs to decide if such NAs can be removed
  # if not delete ALL data from one point from mainland us
  subset(biomass_rf_use, is.na(PM25_TOT_AFI)) # (-108.25, 33.25) on 2017-03-23, no PM25_TOT_AFI
  subset(biomass_cmaq_pmf_met_landtype_smk_smkMet, is.na(PM25_TOT_AFI)) # (-108.25, 33.25) on 2017-03-23, no PM25_TOT_AFI
  subset(biomass_rf_use, is.na(PM25_TOT_BIOG))

  biomass_rf_use = subset(biomass_rf_use, !is.na(PM25_TOT_AFI))
  biomass_cmaq_pmf_met_landtype_smk_smkMet = subset(biomass_cmaq_pmf_met_landtype_smk_smkMet, !is.na(PM25_TOT_AFI))
  dim(biomass_cmaq_pmf_met_landtype_smk_smkMet); dim(biomass_rf_use)

  # write_fst(biomass_cmaq_pmf_met_landtype_smk_smkMet,
  #           file.path(
  #             paste0("machine_learning_source_input/Biomass_all_CMAQ_points_input_", cmaq_period, ".fst")))
  # 
  # write_fst(biomass_rf_use,
  #           file.path(
  #             paste0("machine_learning_source_input/Biomass_only_PMF_points_input_", cmaq_period, ".fst")))

  write_fst(biomass_cmaq_pmf_met_landtype_smk_smkMet,
            file.path(
              paste0("machine_learning_source_input/Biomass_all_CMAQ_points_Daily_", cmaq_period, ".fst")))
  
  write_fst(biomass_rf_use,
            file.path(
              paste0("machine_learning_source_input/Biomass_only_PMF_points_Daily_", cmaq_period, ".fst")))
  
  # biomass_cmaq_pmf_met_landtype_smk = read_fst(
  #   file.path(
  #     paste0("machine_learning_source_input/biomass_all_CMAQ_points_input_", cmaq_period, ".fst")))
  # head(biomass_cmaq_pmf_met_landtype_smk); dim(biomass_cmaq_pmf_met_landtype_smk)

  
  rm(cmaq_biomass_rds_noExe) 
  # rm(pmf_biomass) 
  rm(cmaq_biomass_use) 
  rm(pmf_biomass_use) 
  rm(cmaq_pmf_biomass)
  rm(cmaq_pmf_biomass_met_ncld)
  rm(biomass_cmaq_pmf_met_landtype)
  rm(biomass_cmaq_pmf_met_landtype_smk)
  rm(biomass_cmaq_pmf_met_landtype_smk_smkMet)
  rm(biomass_rf_use)
  
  #### PM2.5 #### 
  cat("Process source: PM2.5")
  
  setDT(cmaq_pm25_rds_noExe)
  cmaq_pm25_use = cmaq_pm25_rds_noExe
  
  # summary(unique(hms_smoke_use$Longitude) %in% unique(cmaq_pm25_use$Longitude))
  # summary(unique(hms_smoke_use$Latitude) %in% unique(cmaq_pm25_use$Latitude))
  
  ### round to 2 digits, double check
  pmf_pm25$Longitude = round(pmf_pm25$Longitude, 2)
  pmf_pm25$Latitude = round(pmf_pm25$Latitude, 2)
  
  cmaq_pm25_use$Longitude = round(cmaq_pm25_use$Longitude, 2)
  cmaq_pm25_use$Latitude = round(cmaq_pm25_use$Latitude, 2)
  
  # # Only use dates in PMF and CMAQ both
  # dim(cmaq_pm25_use)
  # cmaq_pm25_use = subset(cmaq_pm25_use, 
  #                           Date %in% pmf_pm25$Date)
  # dim(cmaq_pm25_use)
  # 
  # pmf_pm25_use = subset(pmf_pm25, 
  #                          Date %in% cmaq_pm25_use$Date)
  # 
  # # Merge PMF & CMAQ results, keep all points in CMAQ, and keep all sites in PMF with same grid coordinates
  # cmaq_pmf_pm25 = 
  #   merge(cmaq_pm25_use, pmf_pm25_use, 
  #         by = c("Date", "Longitude", "Latitude"), 
  #         all.x = TRUE, all.y = TRUE)

  # Merge PMF & CMAQ results, keep all points and all dates in CMAQ 
  pmf_pm25_use = subset(pmf_pm25,
                           Date %in% cmaq_pm25_use$Date)
  cmaq_pmf_pm25 =
    merge(cmaq_pm25_use, pmf_pm25_use,
          by = c("Date", "Longitude", "Latitude"),
          all.x = TRUE, all.y = TRUE)
  
  head(cmaq_pmf_pm25)
  dim(cmaq_pmf_pm25); dim(pmf_pm25_use); dim(cmaq_pm25_use)
  # table(cmaq_pmf_pm25$Date)
  
  # Merge with meteorology & NCLD
  cmaq_pmf_pm25_met_ncld =
    merge(cmaq_pmf_pm25, met_ncld_us_grid,
          by = c("Date", "Longitude", "Latitude"), all.x = TRUE)
  
  cmaq_pmf_pm25_met_ncld$NLCD.Land.Cover.Class = 
    as.factor(cmaq_pmf_pm25_met_ncld$NLCD.Land.Cover.Class)
  
  # Merge with land use type
  pm25_cmaq_pmf_met_landtype =
    merge(cmaq_pmf_pm25_met_ncld, landcover_df, 
          by = "NLCD.Land.Cover.Class", all.x = TRUE)
  pm25_cmaq_pmf_met_landtype$NLCD.Land.Cover.Class = NULL
  # caret only deal with factor and numeric, not characters
  pm25_cmaq_pmf_met_landtype$land_type = 
    factor(pm25_cmaq_pmf_met_landtype$land_type, ordered = FALSE)
  
  ### Merge with HMS Smoke file
  # Extract the date to use
  hms_smoke_use = 
    subset(hms_smoke_all, Date %in% pm25_cmaq_pmf_met_landtype$Date)
  summary(hms_smoke_use)
  
  pm25_cmaq_pmf_met_landtype_smk =
    merge(pm25_cmaq_pmf_met_landtype, hms_smoke_use,
          by = c("Date", "Longitude", "Latitude"), all.x = TRUE)
  
  # Set NA in smoke_level as 0, NA is due to no record on that day
  pm25_cmaq_pmf_met_landtype_smk[is.na(smoke_level), smoke_level := 0]
  
  # Set smoke_level as factor
  pm25_cmaq_pmf_met_landtype_smk$smoke_level = 
    as.factor(pm25_cmaq_pmf_met_landtype_smk$smoke_level)
  
  ### Smoke related meteorological variables
  gridmet_bio = 
    read_fst(file.path(paste0("pmf_ncld_meteo_census/GRIDMET_Biomass_", cmaq_year, "_in_US_grid_01.fst")))
  summary(gridmet_bio)
  gridmet_bio$Date = as.Date(gridmet_bio$Date)
  head(gridmet_bio); sapply(gridmet_bio, class)
  
  # 2 digits
  gridmet_bio$Longitude = round(gridmet_bio$Longitude, 2)
  gridmet_bio$Latitude = round(gridmet_bio$Latitude, 2)
  
  gridmet_bio_use =
    subset(gridmet_bio, Date %in% pm25_cmaq_pmf_met_landtype_smk$Date)
  dim(gridmet_bio); dim(gridmet_bio_use)
  
  pm25_cmaq_pmf_met_landtype_smk_smkMet =
    merge(pm25_cmaq_pmf_met_landtype_smk, gridmet_bio_use,
          by = c("Date", "Longitude", "Latitude"), all.x = TRUE)
  
  # Merge with roadiness
  pm25_cmaq_pmf_met_land_smk_smkMet_road =
    merge(pm25_cmaq_pmf_met_landtype_smk_smkMet, roadiness_us_grid_mean,
          # dplyr::select(roadiness_us_grid_mean, -long, -lat),
          by = c("Longitude", "Latitude"), all.x = TRUE)
  
  # For caret::train(method = "rf")
  pm25_rf_use = 
    subset(pm25_cmaq_pmf_met_land_smk_smkMet_road, !is.na(Concentration))
  length(unique(pm25_rf_use$SiteCode))
  
  dim(pm25_cmaq_pmf_met_land_smk_smkMet_road); dim(pm25_rf_use)
  
  summary(pm25_cmaq_pmf_met_land_smk_smkMet_road)
  # table(pm25_cmaq_pmf_met_land_smk_smkMet_road$land_type)
  summary(pm25_rf_use)
  
  ##### Check if there is NAs to decide if such NAs can be removed 
  # # if not delete ALL data from one point from mainland us
  # subset(pm25_rf_use, is.na(AFI)) # (-108.25, 33.25) on 2017-03-23, no AFI
  # subset(pm25_cmaq_pmf_met_land_smk_smkMet_road, is.na(AFI)) # (-108.25, 33.25) on 2017-03-23, no AFI
  # subset(pm25_rf_use, is.na(BIOG)) 
  
  pm25_rf_use = subset(pm25_rf_use, !is.na(AFI))
  pm25_cmaq_pmf_met_land_smk_smkMet_road = subset(pm25_cmaq_pmf_met_land_smk_smkMet_road, !is.na(AFI))
  dim(pm25_cmaq_pmf_met_land_smk_smkMet_road); dim(pm25_rf_use)
  
  # write_fst(pm25_cmaq_pmf_met_land_smk_smkMet_road,
  #           file.path(
  #             paste0("machine_learning_source_input/PM25_all_CMAQ_points_input_", cmaq_period, ".fst")))
  # 
  # write_fst(pm25_rf_use,
  #           file.path(
  #             paste0("machine_learning_source_input/PM25_only_PMF_points_input_", cmaq_period, ".fst")))
  
  write_fst(pm25_cmaq_pmf_met_land_smk_smkMet_road,
            file.path(
              paste0("machine_learning_source_input/PM25_all_CMAQ_points_Daily_", cmaq_period, ".fst")))
  
  write_fst(pm25_rf_use,
            file.path(
              paste0("machine_learning_source_input/PM25_only_PMF_points_Daily_", cmaq_period, ".fst")))
  
  rm(cmaq_pm25_rds_noExe) 
  # rm(pmf_pm25) 
  rm(cmaq_pm25_use) 
  rm(pmf_pm25_use) 
  rm(cmaq_pmf_pm25)
  rm(cmaq_pmf_pm25_met_ncld)
  rm(pm25_cmaq_pmf_met_landtype)
  rm(pm25_cmaq_pmf_met_landtype_smk)
  rm(pm25_cmaq_pmf_met_landtype_smk_smkMet)
  rm(pm25_cmaq_pmf_met_land_smk_smkMet_road)
  rm(pm25_rf_use)
  
}
