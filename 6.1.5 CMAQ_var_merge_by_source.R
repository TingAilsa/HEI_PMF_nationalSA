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
library(stars) # raster to stars, then to sf

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

#### Create grid-like data ####
# Define the approximate bounding box for mainland U.S.
us_bbox <- c(xmin = -125, xmax = -66, ymin = 24, ymax = 50)
crs_proj <- "+proj=longlat +datum=WGS84 +no_defs"

us_grid_raster_01 = raster(file.path("base_raster_grid_sf/us_grid_raster_01.tif"))
us_grid_raster_001 = raster(file.path("base_raster_grid_sf/us_grid_raster_001.tif"))

# us_grid_sf_01 = st_read(file.path("base_raster_grid_sf/us_grid_sf_01.fgb"))
# us_grid_sf_001 = st_read(file.path("base_raster_grid_sf/us_grid_sf_001.fgb"))

# us_grid_centroids_01 = st_read(file.path("base_raster_grid_sf/us_grid_centroids_01.fgb"))
# us_grid_centroids_001 = st_read(file.path("base_raster_grid_sf/us_grid_centroids_001.fgb"))

#### PMF ####

# # read files
# csn_imp_site_with_nearest =
#   st_read( file.path("base_raster_grid_sf/PMF_sites_in_US_grid_01.fgb"))
# 
# pmf_source = fread("pmf_ncld_meteo_census/CSN_IMPROVE_source_daily_contribution.csv")
# date_use = read.fst("pmf_ncld_meteo_census/Date_DOW_Holiday_2011-20.fst")
# sapply(date_use, class)
# 
# # Merge date info with source info
# pmf_source$Date = as.Date(pmf_source$Date)
# pmf_source = base::merge(pmf_source, date_use, all.x = TRUE)
# 
# # Raster does not include date, convert date to integer and change back later
# # pmf_source$Date <- as.integer(format(pmf_source$Date, "%Y%m%d"))
# 
# # change the characters to factor
# pmf_source$day_of_week = as.factor(pmf_source$day_of_week)
# pmf_source$is_holiday = as.factor(pmf_source$is_holiday)
# sapply(pmf_source, class)
# 
# # change colnames
# names(pmf_source)[3:4]
# names(pmf_source)[3:4] = c("org_long", "org_lat")
# 
# csn_imp_site_with_nearest$org_long = csn_imp_site_with_nearest$org_lat = NULL
# 
# # Assign new long and lat from US grid
# pmf_source_US = merge(pmf_source, csn_imp_site_with_nearest,
#                       by = "SiteCode", all.x = TRUE)
# unique(pmf_source_US$Source_aftermanual)
# 
# # Change different traffic/sea salt from CSN & IMPROVE to one traffic/sea salt
# pmf_source_US <-
#   pmf_source_US %>%
#   mutate(Source_aftermanual =
#            ifelse(grepl("Traffic", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
#                   "F1-Traffic",
#                   Source_aftermanual))
# 
# pmf_source_US <-
#   pmf_source_US %>%
#   mutate(Source_aftermanual =
#            ifelse(grepl("Sea Salt", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
#                   "F6-Salt",
#                   Source_aftermanual))
# 
# pmf_source_US <-
#   pmf_source_US %>%
#   mutate(Source_aftermanual =
#            ifelse(grepl("Non-tailpipe", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
#                   "F7-Non-tailpipe",
#                   Source_aftermanual))
# 
# pmf_source_US <-
#   pmf_source_US %>%
#   mutate(Source_aftermanual =
#            ifelse(grepl("Industry", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
#                   "F5-Industry",
#                   Source_aftermanual))
# 
# pmf_source_US <-
#   pmf_source_US %>%
#   mutate(Source_aftermanual =
#            ifelse(grepl("Biomass", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
#                   "F8-Biomass",
#                   Source_aftermanual))
# 
# unique(pmf_source_US$Source_aftermanual)
# 
# # change to factor for modeling
# pmf_source_US$Source_aftermanual = as.factor(pmf_source_US$Source_aftermanual)
# 
# # Get the total contribution of new groups
# pmf_source_US_sumContri <-
#   pmf_source_US %>%
#   group_by(SiteCode, Date, Source_aftermanual, Latitude, Longitude, 
#            year, month, day_of_week, is_holiday, Dataset) %>%
#   dplyr::summarise(Concentration = sum(Concentration),
#                    .groups = "drop")
# unique(pmf_source_US_sumContri$Source_aftermanual)
# 
# # PMF data for each sources
# pmf_traffic = subset(pmf_source_US_sumContri, Source_aftermanual == "F1-Traffic")
# pmf_nitrate = subset(pmf_source_US_sumContri, Source_aftermanual == "F2-Secondary Nitrate")
# pmf_sulfate = subset(pmf_source_US_sumContri, Source_aftermanual == "F3-Secondary Sulfate")
# pmf_industry = subset(pmf_source_US_sumContri, Source_aftermanual == "F5-Industry")
# pmf_salt = subset(pmf_source_US_sumContri, Source_aftermanual == "F6-Salt")
# pmf_biomass = subset(pmf_source_US_sumContri, Source_aftermanual == "F8-Biomass")
# pmf_dust = subset(pmf_source_US_sumContri, Source_aftermanual == "F9-Soil/Dust")
# pmf_nontailpipe = subset(pmf_source_US_sumContri, Source_aftermanual == "F7-Non-tailpipe")
# pmf_oprich = subset(pmf_source_US_sumContri, Source_aftermanual == "F10-OP-rich")
# 
# write_fst(pmf_traffic, "pmf_ncld_meteo_census/PMF_F1-Traffic.fst")
# write_fst(pmf_nitrate, "pmf_ncld_meteo_census/PMF_F2-Secondary_Nitrate.fst")
# write_fst(pmf_sulfate, "pmf_ncld_meteo_census/PMF_F3-Secondary_Sulfate.fst")
# write_fst(pmf_industry, "pmf_ncld_meteo_census/PMF_F5-Industry.fst")
# write_fst(pmf_salt, "pmf_ncld_meteo_census/PMF_F6-Salt.fst")
# write_fst(pmf_biomass, "pmf_ncld_meteo_census/PMF_F8-Biomass.fst")
# write_fst(pmf_dust, "pmf_ncld_meteo_census/PMF_F9-Soil_Dust.fst")
# write_fst(pmf_nontailpipe, "pmf_ncld_meteo_census/PMF_F7-Non-tailpipe.fst")
# write_fst(pmf_oprich, "pmf_ncld_meteo_census/PMF_F10-OP-rich.fst")

pmf_traffic = read_fst("pmf_ncld_meteo_census/PMF_F1-Traffic.fst")
pmf_nitrate = read_fst("pmf_ncld_meteo_census/PMF_F2-Secondary_Nitrate.fst")
pmf_sulfate = read_fst("pmf_ncld_meteo_census/PMF_F3-Secondary_Sulfate.fst")
pmf_industry = read_fst("pmf_ncld_meteo_census/PMF_F5-Industry.fst")
pmf_salt = read_fst("pmf_ncld_meteo_census/PMF_F6-Salt.fst")
pmf_biomass = read_fst("pmf_ncld_meteo_census/PMF_F8-Biomass.fst")
pmf_dust = read_fst("pmf_ncld_meteo_census/PMF_F9-Soil_Dust.fst")
pmf_nontailpipe = read_fst("pmf_ncld_meteo_census/PMF_F7-Non-tailpipe.fst")
pmf_oprich = read_fst("pmf_ncld_meteo_census/PMF_F10-OP-rich.fst")

length(unique(pmf_traffic$SiteCode))
length(unique(pmf_sulfate$SiteCode))
length(unique(pmf_dust$SiteCode))
length(unique(pmf_salt$SiteCode))
length(unique(pmf_oprich$SiteCode))

#### GRIDMET ####

# met_geo_with_nearest_sf = 
#   st_read(file.path("base_raster_grid_sf/GRIDMET_in_US_grid_01.fgb"))


#### CMAQ data ####
# cmaq_period = "2011-02_2011-09"
# cmaq_period = "2011-02_2011-12"
# cmaq_period = "2011-01_2011-12"; cmaq_year = 2011
cmaq_period = "2017-01_2017-12"; cmaq_year = 2017
print(paste0("Study period: ", cmaq_period, " & year ", cmaq_year))

###### Read CMAQ variable rds of a study period ######
# # Set file path and period to process
CMAQ_path = "/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_annual"

### CMAQ_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/var_combined_rds"

# List file of the name patterns (study period)
rds_period =  list.files(CMAQ_path,
                         pattern = paste0(".*", cmaq_period, "\\.rds$"),
                         full.names = TRUE)
print("CMAQ files to be combined:"); rds_period


# # NH3
# cmaq_nh3_rds = readRDS(rds_period[grepl("NH3", rds_period, fixed = T)])
# class(cmaq_nh3_rds); head(cmaq_nh3_rds)
# summary(cmaq_nh3_rds)
# 
# # SO2
# cmaq_so2_rds <- readRDS(rds_period[grepl("SO2", rds_period, fixed = T)])
# summary(cmaq_so2_rds)
# 
# # NO2
# cmaq_no2_rds <- readRDS(rds_period[grepl("NO2", rds_period, fixed = T)])
# summary(cmaq_no2_rds)
# 
# # O3
# cmaq_o3_rds <- readRDS(rds_period[grepl("O3", rds_period, fixed = T)])
# summary(cmaq_o3_rds)
# 
# # DUST
# cmaq_dust_rds <- readRDS(rds_period[grepl("TOT_DUST", rds_period, fixed = T)])
# summary(cmaq_dust_rds)
# 
# # EGU
# cmaq_egu_rds <- readRDS(rds_period[grepl("TOT_EGU", rds_period, fixed = T)])
# summary(cmaq_egu_rds)
# 
# # NRD
# cmaq_nrd_rds <- readRDS(rds_period[grepl("TOT_NRD", rds_period, fixed = T)])
# summary(cmaq_nrd_rds)
# 
# # ONR
# cmaq_onr_rds <- readRDS(rds_period[grepl("TOT_ONR", rds_period, fixed = T)])
# summary(cmaq_onr_rds)

# OTA
cmaq_ota_rds <- readRDS(rds_period[grepl("TOT_OTA", rds_period, fixed = T)])
summary(cmaq_ota_rds)

# ACM
cmaq_acm_rds <- readRDS(rds_period[grepl("TOT_ACM", rds_period, fixed = T)])
summary(cmaq_acm_rds)

# ASEA
cmaq_asea_rds <- readRDS(rds_period[grepl("TOT_ASEA", rds_period, fixed = T)])
summary(cmaq_asea_rds)

# ARS
cmaq_ars_rds <- readRDS(rds_period[grepl("TOT_ARS", rds_period, fixed = T)])
summary(cmaq_ars_rds)

# BIOG
cmaq_biog_rds <- readRDS(rds_period[grepl("TOT_BIOG", rds_period, fixed = T)])
summary(cmaq_biog_rds)

# AFI
cmaq_afi_rds <- readRDS(rds_period[grepl("TOT_AFI", rds_period, fixed = T)])
summary(cmaq_afi_rds)

###### Merge rds for each source & add crs ######
cmaq_sulfate_rds = merge(cmaq_nh3_rds, cmaq_so2_rds)
cmaq_sulfate_rds = merge(cmaq_sulfate_rds, cmaq_o3_rds)
cmaq_sulfate_rds = merge(cmaq_sulfate_rds, cmaq_egu_rds)
cmaq_sulfate_rds = merge(cmaq_sulfate_rds, cmaq_ota_rds)

cmaq_traffic_rds = merge(cmaq_nrd_rds, cmaq_onr_rds)
cmaq_traffic_rds = merge(cmaq_nrd_rds, cmaq_acm_rds)
cmaq_traffic_rds = merge(cmaq_traffic_rds, cmaq_no2_rds)
cmaq_traffic_rds = merge(cmaq_traffic_rds, cmaq_o3_rds)

# cmaq_dust_rds = merge(cmaq_dust_rds, cmaq_ars_rds)

cmaq_biom_rds = merge(cmaq_afi_rds, cmaq_biog_rds)

dim(cmaq_traffic_rds); dim(cmaq_onr_rds)

names(cmaq_sulfate_rds)[1:2] = c("Longitude", "Latitude")
# names(cmaq_dust_rds)[1:2] = c("Longitude", "Latitude")
names(cmaq_traffic_rds)[1:2] = c("Longitude", "Latitude")
names(cmaq_biom_rds)[1:2] = c("Longitude", "Latitude")

write_fst(cmaq_sulfate_rds,
          file.path(paste0("base_raster_grid_sf/CMAQ_Sulfate_", cmaq_period, ".fst")))
write_fst(cmaq_dust_rds,
          file.path(paste0("base_raster_grid_sf/CMAQ_Dust_", cmaq_period, ".fst")))
write_fst(cmaq_traffic_rds,
          file.path(paste0("base_raster_grid_sf/CMAQ_Traffic_", cmaq_period, ".fst")))
write_fst(cmaq_biom_rds,
          file.path(paste0("base_raster_grid_sf/CMAQ_Biomass_", cmaq_period, ".fst")))

##### PLOTTING
## Extract long & points with the continental US
library(USAboundaries)
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

# # List CMAQ rds files for each sector
# # cmaq_nh3_rds; cmaq_so2_rds; cmaq_no2_rds; cmaq_o3_rds; cmaq_dust_rds; cmaq_egu_rds; cmaq_nrd_rds; cmaq_onr_rds
# rds_list <- list("cmaq_nh3_rds", "cmaq_so2_rds", "cmaq_no2_rds",
#                  "cmaq_o3_rds", "cmaq_egu_rds", # "cmaq_dust_rds",
#                  "cmaq_nrd_rds", "cmaq_onr_rds")
# 
# # Corresponding variable names
# variables <- c("NH3", "SO2", "NO2", "O3", "EGU", "NRD", "ORD") # "Dust",
# 
# # Evaluate the character names to actual data.tables
# rds_tables <- lapply(rds_list, function(name) get(name))
# 
# # Process each data.table
# for (i in seq_along(rds_tables)) {
#   unique_col <- setdiff(names(rds_tables[[i]]), c("x", "y", "Date"))
#   setnames(rds_tables[[i]], unique_col, "cmaq_value")  # Rename the unique column
#   rds_tables[[i]][, cmaq_variable := variables[i]]    # Add cmaq_variable column
# }
# 
# # Combine all data.tables into one
# cmaq_rds_all <- data.table::rbindlist(rds_tables, use.names = TRUE, fill = TRUE)
# 
# # cmaq_dust_rds_adp = cmaq_dust_rds
# # names(cmaq_dust_rds_adp)[3] = "cmaq_value"
# # cmaq_dust_rds_adp$cmaq_variable = "Dust"
# # head(cmaq_dust_rds_adp)
# 
# names(cmaq_rds_all)[1:2]
# names(cmaq_rds_all)[1:2] = c("Longitude", "Latitude")
# # cmaq_rds_all = rbind(cmaq_rds_all, cmaq_dust_rds_adp)
# 
# cmaq_rds_all_noExe =
#   subset(cmaq_rds_all,
#          cmaq_value < quantile(cmaq_rds_all$cmaq_value, 0.995) &
#            cmaq_value > quantile(cmaq_rds_all$cmaq_value, 0.005))
# 
# cmaq_rds_by_source =
#   cmaq_rds_all_noExe %>%
#   group_by(Longitude, Latitude, cmaq_variable) %>%
#   dplyr::summarise(cmaq_value_mean = mean(cmaq_value),
#                    cmaq_value_med = median(cmaq_value))
# head(cmaq_rds_by_source); dim(cmaq_rds_by_source)

# cmaq_rds_by_source_plot <-
#   ggplot() +
#   geom_point(data = cmaq_rds_by_source,
#              aes(x = Longitude, y = Latitude, color = cmaq_value_med),
#              size = 0.35, alpha = 0.8) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey70", size = 0.3) +
#   facet_wrap(~cmaq_variable, ncol = 3) +
#   scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
#   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude",
#        title = "Median CMAQ contribution in 2011") +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(size = 22),
#     legend.text = element_text(size = 19),
#     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
#     # plot.subtitle = element_text(size = 22),
#     axis.title = element_text(size = 22),
#     axis.text = element_text(size = 19)
#   )
# # cmaq_rds_by_source_plot
# cmaq_source_name = paste0("CMAQ_source_map_", cmaq_year, ".pdf"); cmaq_source_name
# ggsave(
#   file.path("machine_learning_source_input/ML_plot", cmaq_source_name),
#   plot = cmaq_rds_by_source_plot, width = 14.5, height = 8.5)


# #### nh3
# names(cmaq_nh3_rds)[1:2] = c("Longitude", "Latitude")
# quantile(cmaq_nh3_rds$NH3, 0.995)
# 
# cmaq_nh3_rds_use =
#   subset(cmaq_nh3_rds,
#          NH3 < quantile(cmaq_nh3_rds$NH3, 0.995) &
#            NH3 > quantile(cmaq_nh3_rds$NH3, 0.005)) %>%
#   group_by(Longitude, Latitude) %>%
#   dplyr::summarise(cmaq_mean = mean(NH3),
#                    cmaq_median = median(NH3))
# head(cmaq_nh3_rds_use); dim(cmaq_nh3_rds_use)
# 
# cmaq_nh3_rds_plot <-
#   ggplot() +
#   geom_point(data = cmaq_nh3_rds_use,
#              aes(x = Longitude, y = Latitude, color = cmaq_median),
#              size = 0.35, alpha = 0.8) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey70", size = 0.3) +
#   scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
#   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude",
#        title = paste0("Median CMAQ contribution in ", cmaq_year)) +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(size = 22),
#     legend.text = element_text(size = 19),
#     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
#     # plot.subtitle = element_text(size = 22),
#     axis.title = element_text(size = 22),
#     axis.text = element_text(size = 19)
#   )
# # cmaq_nh3_rds_plot
# 
# nh3_name = paste0("CMAQ_source_map_NH3_", cmaq_period, ".pdf")
# ggsave(
#   file.path("machine_learning_source_input/ML_plot", nh3_name),
#   plot = cmaq_nh3_rds_plot, width = 14.5, height = 8.5)
# 
# #### so2
# names(cmaq_so2_rds)[1:2] = c("Longitude", "Latitude")
# quantile(cmaq_so2_rds$SO2, 0.995) 
# 
# cmaq_so2_rds_use =
#   subset(cmaq_so2_rds,
#          SO2 < quantile(cmaq_so2_rds$SO2, 0.995) &
#            SO2 > quantile(cmaq_so2_rds$SO2, 0.005)) %>%
#   group_by(Longitude, Latitude) %>%
#   dplyr::summarise(cmaq_mean = mean(SO2),
#                    cmaq_median = median(SO2))
# head(cmaq_so2_rds_use); dim(cmaq_so2_rds_use)
# 
# cmaq_so2_rds_plot <-
#   ggplot() +
#   geom_point(data = cmaq_so2_rds_use,
#              aes(x = Longitude, y = Latitude, color = cmaq_median),
#              size = 0.35, alpha = 0.8) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey70", size = 0.3) +
#   scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
#   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude",
#        title = paste0("Median CMAQ contribution in ", cmaq_year)) +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(size = 22),
#     legend.text = element_text(size = 19),
#     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
#     # plot.subtitle = element_text(size = 22),
#     axis.title = element_text(size = 22),
#     axis.text = element_text(size = 19)
#   )
# # cmaq_so2_rds_plot
# 
# so2_name = paste0("CMAQ_source_map_SO2_", cmaq_period, ".pdf")
# ggsave(
#   file.path("machine_learning_source_input/ML_plot", so2_name),
#   plot = cmaq_so2_rds_plot, width = 14.5, height = 8.5)
# 
# 
# #### no2
# names(cmaq_no2_rds)[1:2] = c("Longitude", "Latitude")
# quantile(cmaq_no2_rds$NO2, 0.995)
# 
# cmaq_no2_rds_use =
#   subset(cmaq_no2_rds,
#          NO2 < quantile(cmaq_no2_rds$NO2, 0.995) &
#            NO2 > quantile(cmaq_no2_rds$NO2, 0.005)) %>%
#   group_by(Longitude, Latitude) %>%
#   dplyr::summarise(cmaq_mean = mean(NO2),
#                    cmaq_median = median(NO2))
# head(cmaq_no2_rds_use); dim(cmaq_no2_rds_use)
# 
# cmaq_no2_rds_plot <-
#   ggplot() +
#   geom_point(data = cmaq_no2_rds_use,
#              aes(x = Longitude, y = Latitude, color = cmaq_median),
#              size = 0.35, alpha = 0.8) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey70", size = 0.3) +
#   scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
#   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude",
#        title = paste0("Median CMAQ contribution in ", cmaq_year)) +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(size = 22),
#     legend.text = element_text(size = 19),
#     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
#     # plot.subtitle = element_text(size = 22),
#     axis.title = element_text(size = 22),
#     axis.text = element_text(size = 19)
#   )
# # cmaq_no2_rds_plot
# 
# no2_name = paste0("CMAQ_source_map_NO2_", cmaq_period, ".pdf")
# ggsave(
#   file.path("machine_learning_source_input/ML_plot", no2_name),
#   plot = cmaq_no2_rds_plot, width = 14.5, height = 8.5)
# 
# 
# #### o3
# names(cmaq_o3_rds)[1:2] = c("Longitude", "Latitude")
# quantile(cmaq_o3_rds$O3, 0.995) 
# 
# cmaq_o3_rds_use =
#   subset(cmaq_o3_rds,
#          O3 < quantile(cmaq_o3_rds$O3, 0.995) &
#            O3 > quantile(cmaq_o3_rds$O3, 0.005)) %>%
#   group_by(Longitude, Latitude) %>%
#   dplyr::summarise(cmaq_mean = mean(O3),
#                    cmaq_median = median(O3))
# head(cmaq_o3_rds_use); dim(cmaq_o3_rds_use)
# 
# cmaq_o3_rds_plot <-
#   ggplot() +
#   geom_point(data = cmaq_o3_rds_use,
#              aes(x = Longitude, y = Latitude, color = cmaq_median),
#              size = 0.35, alpha = 0.8) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey70", size = 0.3) +
#   scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
#   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude",
#        title = paste0("Median CMAQ contribution in ", cmaq_year)) +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(size = 22),
#     legend.text = element_text(size = 19),
#     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
#     # plot.subtitle = element_text(size = 22),
#     axis.title = element_text(size = 22),
#     axis.text = element_text(size = 19)
#   )
# # cmaq_o3_rds_plot
# 
# o3_name = paste0("CMAQ_source_map_O3_", cmaq_period, ".pdf")
# ggsave(
#   file.path("machine_learning_source_input/ML_plot", o3_name),
#   plot = cmaq_o3_rds_plot, width = 14.5, height = 8.5)
# 
# 
# #### dust
# names(cmaq_dust_rds)[1:2] = c("Longitude", "Latitude")
# quantile(cmaq_dust_rds$PM25_TOT_DUST, 0.995)
# 
# cmaq_dust_rds_use =
#   subset(cmaq_dust_rds,
#          PM25_TOT_DUST < quantile(cmaq_dust_rds$PM25_TOT_DUST, 0.995) &
#            PM25_TOT_DUST > quantile(cmaq_dust_rds$PM25_TOT_DUST, 0.005)) %>%
#   group_by(Longitude, Latitude) %>%
#   dplyr::summarise(cmaq_mean = mean(PM25_TOT_DUST),
#                    cmaq_median = median(PM25_TOT_DUST))
# head(cmaq_dust_rds_use); dim(cmaq_dust_rds_use)
# 
# cmaq_dust_rds_plot <-
#   ggplot() +
#   geom_point(data = cmaq_dust_rds_use,
#              aes(x = Longitude, y = Latitude, color = cmaq_median),
#              size = 0.35, alpha = 0.8) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey70", size = 0.3) +
#   scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
#   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude",
#        title = paste0("Median CMAQ contribution in ", cmaq_year)) +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(size = 22),
#     legend.text = element_text(size = 19),
#     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
#     # plot.subtitle = element_text(size = 22),
#     axis.title = element_text(size = 22),
#     axis.text = element_text(size = 19)
#   )
# # cmaq_dust_rds_plot
# 
# dust_name = paste0("CMAQ_source_map_Dust_", cmaq_period, ".pdf")
# ggsave(
#   file.path("machine_learning_source_input/ML_plot", dust_name),
#   plot = cmaq_dust_rds_plot, width = 14.5, height = 8.5)
# 
# 
# #### egu
# names(cmaq_egu_rds)[1:2] = c("Longitude", "Latitude")
# quantile(cmaq_egu_rds$PM25_TOT_EGU, 0.995)
# 
# cmaq_egu_rds_use =
#   subset(cmaq_egu_rds,
#          PM25_TOT_EGU < quantile(cmaq_egu_rds$PM25_TOT_EGU, 0.995) &
#            PM25_TOT_EGU > quantile(cmaq_egu_rds$PM25_TOT_EGU, 0.005)) %>%
#   group_by(Longitude, Latitude) %>%
#   dplyr::summarise(cmaq_mean = mean(PM25_TOT_EGU),
#                    cmaq_median = median(PM25_TOT_EGU))
# head(cmaq_egu_rds_use); dim(cmaq_egu_rds_use)
# 
# cmaq_egu_rds_plot <-
#   ggplot() +
#   geom_point(data = cmaq_egu_rds_use,
#              aes(x = Longitude, y = Latitude, color = cmaq_median),
#              size = 0.35, alpha = 0.8) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey70", size = 0.3) +
#   scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
#   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude",
#        title = paste0("Median CMAQ contribution in ", cmaq_year)) +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(size = 22),
#     legend.text = element_text(size = 19),
#     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
#     # plot.subtitle = element_text(size = 22),
#     axis.title = element_text(size = 22),
#     axis.text = element_text(size = 19)
#   )
# # cmaq_egu_rds_plot
# 
# egu_name = paste0("CMAQ_source_map_EGU_", cmaq_period, ".pdf")
# ggsave(
#   file.path("machine_learning_source_input/ML_plot", egu_name),
#   plot = cmaq_egu_rds_plot, width = 14.5, height = 8.5)
# 
# 
# #### nrd
# names(cmaq_nrd_rds)[1:2] = c("Longitude", "Latitude")
# quantile(cmaq_nrd_rds$PM25_TOT_NRD, 0.995)
# 
# cmaq_nrd_rds_use =
#   subset(cmaq_nrd_rds,
#          PM25_TOT_NRD < quantile(cmaq_nrd_rds$PM25_TOT_NRD, 0.995) &
#            PM25_TOT_NRD > quantile(cmaq_nrd_rds$PM25_TOT_NRD, 0.005)) %>%
#   group_by(Longitude, Latitude) %>%
#   dplyr::summarise(cmaq_mean = mean(PM25_TOT_NRD),
#                    cmaq_median = median(PM25_TOT_NRD))
# head(cmaq_nrd_rds_use); dim(cmaq_nrd_rds_use)
# 
# cmaq_nrd_rds_plot <-
#   ggplot() +
#   geom_point(data = cmaq_nrd_rds_use,
#              aes(x = Longitude, y = Latitude, color = cmaq_median),
#              size = 0.35, alpha = 0.8) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey70", size = 0.3) +
#   scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
#   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude",
#        title = paste0("Median CMAQ contribution in ", cmaq_year)) +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(size = 22),
#     legend.text = element_text(size = 19),
#     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
#     # plot.subtitle = element_text(size = 22),
#     axis.title = element_text(size = 22),
#     axis.text = element_text(size = 19)
#   )
# # cmaq_nrd_rds_plot
# 
# nrd_name = paste0("CMAQ_source_map_NRD_", cmaq_period, ".pdf")
# ggsave(
#   file.path("machine_learning_source_input/ML_plot", nrd_name),
#   plot = cmaq_nrd_rds_plot, width = 14.5, height = 8.5)
# 
# 
# #### onr
# names(cmaq_onr_rds)[1:2] = c("Longitude", "Latitude")
# quantile(cmaq_onr_rds$PM25_TOT_ONR, 0.995) 
# 
# cmaq_onr_rds_use =
#   subset(cmaq_onr_rds,
#          PM25_TOT_ONR < quantile(cmaq_onr_rds$PM25_TOT_ONR, 0.995) &
#            PM25_TOT_ONR > quantile(cmaq_onr_rds$PM25_TOT_ONR, 0.005)) %>%
#   group_by(Longitude, Latitude) %>%
#   dplyr::summarise(cmaq_mean = mean(PM25_TOT_ONR),
#                    cmaq_median = median(PM25_TOT_ONR))
# head(cmaq_onr_rds_use); dim(cmaq_onr_rds_use)
# 
# cmaq_onr_rds_plot <-
#   ggplot() +
#   geom_point(data = cmaq_onr_rds_use,
#              aes(x = Longitude, y = Latitude, color = cmaq_median),
#              size = 0.35, alpha = 0.8) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey70", size = 0.3) +
#   scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
#   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude",
#        title = paste0("Median CMAQ contribution in ", cmaq_year)) +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(size = 22),
#     legend.text = element_text(size = 19),
#     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
#     # plot.subtitle = element_text(size = 22),
#     axis.title = element_text(size = 22),
#     axis.text = element_text(size = 19)
#   )
# # cmaq_onr_rds_plot
# 
# onr_name = paste0("CMAQ_source_map_ONR_", cmaq_period, ".pdf")
# ggsave(
#   file.path("machine_learning_source_input/ML_plot", onr_name),
#   plot = cmaq_onr_rds_plot, width = 14.5, height = 8.5)


cmaq_sulfate_rds = read_fst(file.path(paste0("base_raster_grid_sf/CMAQ_Sulfate_", cmaq_period, ".fst")))
cmaq_dust_rds = read_fst( file.path(paste0("base_raster_grid_sf/CMAQ_Dust_", cmaq_period, ".fst")))
cmaq_traffic_rds = read_fst(file.path(paste0("base_raster_grid_sf/CMAQ_Traffic_", cmaq_period, ".fst")))

#### Roadiness ####
# roadiness_with_nearest_sf =
#   st_read(file.path("base_raster_grid_sf/Roadiness_in_US_grid_01.fgb"))
# dim(roadiness_with_nearest_sf)
roadiness_us_grid_mean = 
  read_fst(file.path("base_raster_grid_sf/Roadiness_in_US_grid_01.fst"))
dim(roadiness_us_grid_mean)

roadiness_us_grid_mean$Longitude = round(roadiness_us_grid_mean$Longitude, 2)
roadiness_us_grid_mean$Latitude = round(roadiness_us_grid_mean$Latitude, 2)

# #### Raodiness plotting
# # Conver the df for plotting
# roadiness_us_grid_mean_long =
#   roadiness_us_grid_mean %>%
#   pivot_longer(
#     cols = sRoadLength:VMT_vs_area,
#     names_to = "Roadiness_metric",
#     values_to = "Roadiness_value"
#     )
# head(roadiness_us_grid_mean_long)
# 
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


#### ACS Census info ####
# # County tract GEOID & geometry
# census_tract_geo = st_read("pmf_ncld_meteo_census/ACS_census_tract_geoid_geometry_4326.fgb")
# st_geometry(census_tract_geo) <- "geometry"
# head(census_tract_geo); dim(census_tract_geo)
# 
# # County tract level census for Aim3
# census_acs_wide = read_fst("pmf_ncld_meteo_census/US_Census_ACS_tract_commute_2011-2020.fst")
# 
# # Select the year to use
# census_acs_wide_year =
#   dplyr::select(subset(census_acs_wide, year == 2011), -year)
# 
# # Merge by GEOID
# census_acs_wide_year_geo =
#   merge(census_acs_wide_year, census_tract_geo,
#         by = "GEOID", all.x = TRUE)
# 
# # Convert to sf
# census_acs_wide_year_geo_sf = st_as_sf(census_acs_wide_year_geo, crs = 4326)
# 
# # Spatial join to US grid
# # The census county tract can vary by year, thus, not st_join census_tract_geo and us_grid_sf_01 directly
# # Only process the data by year
# census_year_in_USgrid =
#   st_join(us_grid_sf_01, census_acs_wide_year_geo_sf, join = st_intersects)
# # subset(census_year_in_USgrid, is.na(GEOID))
# # subset(census_year_in_USgrid, is.na(car_truck_van))
# 
# # Calculate the centroids of the geometries & extract centroid coordinates
# census_year_centroids = st_centroid(census_year_in_USgrid)
# census_year_coords = st_coordinates(census_year_centroids)
# 
# # Combine centroid coordinates with census_year_in_USgrid
# census_year_with_centroids =
#   census_year_in_USgrid %>%
#   mutate(Longitude = census_year_coords[, 1],
#          Latitude = census_year_coords[, 2])
# 
# census_year_centroids_noGeometry =
#   st_drop_geometry(census_year_with_centroids)
# 
# # Get the mean values
# census_year_centroids_noGeometry =
#   dplyr::select(census_year_centroids_noGeometry, -GEOID) %>%
#   group_by(Longitude, Latitude) %>%
#   dplyr::summarise(
#     car_truck_van = mean(car_truck_van, na.rm = TRUE),
#     public_transport = mean(public_transport, na.rm = TRUE),
#     bike = mean(bike, na.rm = TRUE),
#     walk = mean(walk, na.rm = TRUE),
#     taxi_moto_etc = mean(taxi_moto_etc, na.rm = TRUE),
#     work_home = mean(work_home, na.rm = TRUE),
#     commute_time = mean(commute_time, na.rm = TRUE),
#     .groups = "drop")
# 
# st_write(census_year_with_centroids,
#          file.path("base_raster_grid_sf/Census_2011_in_US_grid_01.fgb"))
# write_fst(census_year_centroids_noGeometry,
#          file.path("base_raster_grid_sf/Census_2011_in_US_grid_01.fst"))

# census_year_with_centroids =
#   st_read(file.path("base_raster_grid_sf/Census_2011_in_US_grid_01.fgb"))
# census_year_centroids_noGeometry =
#   read_fst(file.path("base_raster_grid_sf/Census_2011_in_US_grid_01.fst"))

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

#### Raodiness plotting
# Conver the df for plotting
census_year_centroids_noGeometry_long =
  census_year_centroids_noGeometry %>%
  pivot_longer(
    cols = car_truck_van:public_transport,
    names_to = "Census_commute_metric",
    values_to = "Census_commute_value"
  )
census_year_centroids_noGeometry_long = 
  na.omit(census_year_centroids_noGeometry_long)
head(census_year_centroids_noGeometry_long)

# census_commute_plot <-
#   ggplot() +
#   geom_point(data = census_year_centroids_noGeometry_long,
#              aes(x = Longitude, y = Latitude, color = Census_commute_value),
#              size = 0.35, alpha = 0.8) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey70", size = 0.3) +
#   facet_wrap(~Census_commute_metric, ncol = 3) +
#   scale_color_viridis_c(option = "magma") +
#   coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude",
#        title = "Census_commute Metrics") +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(size = 22),
#     legend.text = element_text(size = 19),
#     plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
#     # plot.subtitle = element_text(size = 22),
#     axis.title = element_text(size = 22),
#     axis.text = element_text(size = 19)
#   )

census_commute_plot <- 
  ggplot() +
  geom_point(data = census_year_centroids_noGeometry_long,
             aes(x = Longitude, y = Latitude, color = Census_commute_value),
             size = 0.35, alpha = 0.8) +
  geom_sf(data = us_states,
          fill = NA, color = "grey70", size = 0.3) +
  facet_wrap(~Census_commute_metric, ncol = 3, scales = "free") +
  scale_color_viridis_c(option = "magma") +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Census_commute Metrics",
       color = "Value") +
  theme(
    strip.text = element_text(size = 20),
    # legend.position = "bottom",
    # legend.title = element_text(size = 22),
    # legend.text = element_text(size = 19),
    plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 19)
  )

census_commute_name = paste0("Census_commute_", cmaq_period, ".pdf")
# Census commute
ggsave(
  file.path("machine_learning_source_input/ML_plot", census_commute_name),
  plot = census_commute_plot, width = 14.5, height = 8.5)


#### NLCD Land use type ####

# project_categorical_raster <- function(source_raster, target_raster) {
#   require(raster)
#   
#   # Get factor levels from source raster
#   source_levels <- levels(source_raster)[[1]]
#   source_vals <- as.numeric(source_raster[])
#   
#   # Transform source coordinates to target CRS
#   source_coords <- xyFromCell(source_raster, 1:ncell(source_raster))
#   source_points_sp <- SpatialPoints(
#     coords = source_coords,
#     proj4string = crs(source_raster)
#   )
#   
#   # Transform to target CRS
#   source_points_transformed <- spTransform(source_points_sp, crs(target_raster))
#   source_coords_transformed <- coordinates(source_points_transformed)
#   
#   # Create source points dataframe with transformed coordinates
#   valid_idx <- !is.na(source_vals)
#   source_points <- data.frame(
#     x = source_coords_transformed[valid_idx, 1],
#     y = source_coords_transformed[valid_idx, 2],
#     value = source_vals[valid_idx]
#   )
#   
#   # Get target coordinates
#   target_coords <- xyFromCell(target_raster, 1:ncell(target_raster))
#   
#   message("Finding nearest neighbors for all target cells...")
#   
#   # Find nearest neighbor function
#   find_nearest <- function(point, source_points) {
#     dists <- sqrt((source_points$x - point[1])^2 + 
#                     (source_points$y - point[2])^2)
#     return(source_points$value[which.min(dists)])
#   }
#   
#   # Process in chunks
#   chunk_size <- 1000
#   n_chunks <- ceiling(nrow(target_coords) / chunk_size)
#   result_values <- numeric(nrow(target_coords))
#   
#   for(i in 1:n_chunks) {
#     if(i %% 10 == 0) message(sprintf("Processing chunk %d of %d", i, n_chunks))
#     
#     start_idx <- (i - 1) * chunk_size + 1
#     end_idx <- min(i * chunk_size, nrow(target_coords))
#     chunk_idx <- start_idx:end_idx
#     
#     for(j in seq_along(chunk_idx)) {
#       result_values[chunk_idx[j]] <- find_nearest(
#         target_coords[chunk_idx[j], ], 
#         source_points
#       )
#     }
#   }
#   
#   # Create output raster
#   result_raster <- target_raster
#   values(result_raster) <- result_values
#   
#   # Set factor levels
#   result_raster <- ratify(result_raster)
#   levels(result_raster) <- source_levels
#   
#   return(result_raster)
# }

# # Find nearest neighbor function for points in two grid cooridnates
# find_nearest <- 
#   function(point, ncld_year_points) {
#     dists <- sqrt((ncld_year_points$x - point[1])^2 + 
#                     (ncld_year_points$y - point[2])^2)
#     return(ncld_year_points$value[which.min(dists)])
#   }
# 
# ####### NLCD 1, basic files #########
# # Read original file
# ncld_2011 <- raster("pmf_ncld_meteo_census/nlcd_2011_fact300_landcover_resampled.tif")
# ncld_2013 <- raster("pmf_ncld_meteo_census/nlcd_2013_fact300_landcover_resampled.tif")
# ncld_2016 <- raster("pmf_ncld_meteo_census/nlcd_2016_fact300_landcover_resampled.tif")
# ncld_2019 <- raster("pmf_ncld_meteo_census/nlcd_2019_fact300_landcover_resampled.tif")
# 
# # Select the year to use 
# ncld_year_use = 2011 
# ncld_year = ncld_2011
# 
# ncld_year_use = 2013
# ncld_year = ncld_2013
# 
# ncld_year_use = 2016 
# ncld_year = ncld_2016
# 
# ncld_year_use = 2019
# ncld_year = ncld_2019
# 
# ####### NLCD 2, project the NLCD category raster to US grid raster #########
# # Choose the US grid
# us_grid_raster = us_grid_raster_01
# # us_grid_raster = us_grid_raster_001
# 
# # Convert to factor
# ncld_year_factor <- ratify(ncld_year)
# ncld_year_use
# 
# # Get factor levels from ncld_year raster
# ncld_year_levels <- levels(ncld_year)[[1]]
# ncld_year_vals <- as.numeric(ncld_year[])
# 
# # Transform ncld_year coordinates to target CRS
# ncld_year_coords <- xyFromCell(ncld_year, 1:ncell(ncld_year))
# ncld_year_points_sp <- SpatialPoints(
#   coords = ncld_year_coords,
#   proj4string = crs(ncld_year)
# )
# head(ncld_year_points_sp)
# 
# # Transform to target CRS
# ncld_year_points_transformed <- spTransform(ncld_year_points_sp, crs(us_grid_raster))
# ncld_year_coords_transformed <- coordinates(ncld_year_points_transformed)
# 
# # Create ncld_year points dataframe with transformed coordinates
# ncld_valid_idx <- !is.na(ncld_year_vals)
# summary(ncld_valid_idx)
# 
# ncld_year_points <- data.frame(
#   x = ncld_year_coords_transformed[ncld_valid_idx, 1],
#   y = ncld_year_coords_transformed[ncld_valid_idx, 2],
#   value = ncld_year_vals[ncld_valid_idx]
# )
# head(ncld_year_points)
# 
# # Get target coordinates
# us_grid_coords <- xyFromCell(us_grid_raster, 1:ncell(us_grid_raster))
# 
# print("Finding nearest neighbors for all target cells")
# 
# ####### Process in chunks
# # settings for chunks
# chunk_size <- 10000
# n_chunks <- ceiling(nrow(us_grid_coords) / chunk_size)
# us_grid_ncld_values <- numeric(nrow(us_grid_coords))
# 
# # process
# for(i in 1:n_chunks) {
#   if(i %% 10 == 0) message(sprintf("Processing chunk %d of %d", i, n_chunks))
#   
#   start_idx <- (i - 1) * chunk_size + 1
#   end_idx <- min(i * chunk_size, nrow(us_grid_coords))
#   chunk_idx <- start_idx:end_idx
#   
#   for(j in seq_along(chunk_idx)) {
#     us_grid_ncld_values[chunk_idx[j]] <- find_nearest(
#       us_grid_coords[chunk_idx[j], ], 
#       ncld_year_points
#     )
#   }
# }
# 
# # Create output raster
# us_grid_ncld_raster <- us_grid_raster
# values(us_grid_ncld_raster) <- us_grid_ncld_values
# 
# # Set factor levels
# us_grid_ncld_raster <- ratify(us_grid_ncld_raster)
# levels(us_grid_ncld_raster) <- ncld_year_levels
# 
# # # Project the raster preserving factor levels
# # us_grid_ncld_raster <- 
# #   project_categorical_raster(ncld_year_factor, us_grid_raster_01)
# 
# # Check the results
# table(values(ncld_year_factor)); table(values(us_grid_ncld_raster))
# summary(ncld_year_factor); dim(ncld_year_factor)
# summary(us_grid_ncld_raster); dim(us_grid_ncld_raster)
# 
# # Get coordinates and values of us_grid_ncld_raster
# us_grid_ncld_coords = xyFromCell(us_grid_ncld_raster, 1:ncell(us_grid_ncld_raster))
# us_grid_ncld_values <- values(us_grid_ncld_raster)
# 
# # Create dataframe
# us_grid_ncld_df <- data.frame(
#   Longitude = us_grid_ncld_coords[,1],
#   Latitude = us_grid_ncld_coords[,2],
#   NLCD.Land.Cover.Class = us_grid_ncld_values
# )
# us_grid_ncld$NLCD.Land.Cover.Class = as.factor(us_grid_ncld_df$NLCD.Land.Cover.Class)
# summary(us_grid_ncld_df); dim(us_grid_ncld_df)
# 
# # Make sure there are only two digits for coordinates
# us_grid_ncld_df$Longitude = round(us_grid_ncld_df$Longitude, 2)
# us_grid_ncld_df$Latitude = round(us_grid_ncld_df$Latitude, 2)
# 
# # Get the sf file for future use (in case)
# us_grid_ncld_df$long = us_grid_ncld_df$Longitude
# us_grid_ncld_df$lat = us_grid_ncld_df$Latitude
# 
# us_grid_ncld_sf =
#   st_as_sf(us_grid_ncld_df, 
#            coords = c("long", "lat"),
#            crs = st_crs(us_grid_raster_01))  # use the same CRS as your raster
# 
# # Save the file
# st_write(us_grid_ncld_sf,
#          file.path(
#            paste0("pmf_ncld_meteo_census/NCLD_", ncld_year_use, "_in_US_grid_01.fgb")))
# write_fst(us_grid_ncld_df,
#          file.path(
#            paste0("pmf_ncld_meteo_census/NCLD_", ncld_year_use, "_in_US_grid_01.fst")))


#### Combine data for each source ####

###### Common variables, GRIDMET, NCLD ###### 
## NCLD
# ncld_with_centroids_coords =
#   st_read(file.path("pmf_ncld_meteo_census/NCLD_2011_in_US_grid_01.fgb"))
ncld_with_centroids_coords_noGeo =
  read_fst(file.path(paste0("pmf_ncld_meteo_census/NCLD_", cmaq_year, "_in_US_grid_01.fst")))

ncld_with_centroids_coords_noGeo =
  read_fst(file.path(paste0("pmf_ncld_meteo_census/NCLD_", 2016, "_in_US_grid_01.fst")))

## GRIDMET
gridmet_us_grid_mean = 
  read_fst(file.path(paste0("pmf_ncld_meteo_census/GRIDMET_commom_", cmaq_year, "_in_US_grid_01.fst")))
gridmet_us_grid_mean$th[gridmet_us_grid_mean$th < 0] = 0

### round to 2 digits
ncld_with_centroids_coords_noGeo$Longitude = round(ncld_with_centroids_coords_noGeo$Longitude, 2)
ncld_with_centroids_coords_noGeo$Latitude = round(ncld_with_centroids_coords_noGeo$Latitude, 2)

gridmet_us_grid_mean$Longitude = round(gridmet_us_grid_mean$Longitude, 2)
gridmet_us_grid_mean$Latitude = round(gridmet_us_grid_mean$Latitude, 2)

sapply(gridmet_us_grid_mean, class)
sapply(ncld_with_centroids_coords_noGeo, class)

### merge them
met_ncld_us_grid = 
  merge(ncld_with_centroids_coords_noGeo, gridmet_us_grid_mean, 
        by = c("Longitude", "Latitude"), all.x = TRUE)
met_ncld_us_grid$Date = as.Date(met_ncld_us_grid$Date)

head(met_ncld_us_grid); dim(met_ncld_us_grid)
# summary(met_ncld_us_grid)
# length(unique(met_ncld_us_grid$Longitude, met_ncld_us_grid$Latitude))

#### GRIDMET & NCLD Mapping
met_ncld_us_grid_use = subset(met_ncld_us_grid, !is.na(th))
met_ncld_us_grid_use$long = met_ncld_us_grid_use$lat = NULL
# summary(met_ncld_us_grid_use)
head(met_ncld_us_grid_use)

## Extract long & points within the continental US
library(USAboundaries)
library(patchwork)
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

# met_ncld_us_grid_plot = 
#   subset(met_ncld_us_grid_use, 
#          Date == unique(met_ncld_us_grid_use$Date)[1])

met_ncld_us_grid_plot =
  dplyr::select(met_ncld_us_grid_use, -Date) %>%
  group_by(Longitude, Latitude) %>%
  summarise(
    NLCD.Land.Cover.Class = median(NLCD.Land.Cover.Class, na.rm = TRUE),
    tmmx = median(tmmx, na.rm = TRUE),
    tmmn = median(tmmn, na.rm = TRUE),
    rmax = median(rmax, na.rm = TRUE),
    rmin = median(rmin, na.rm = TRUE),
    vs = median(vs, na.rm = TRUE),
    th = median(th, na.rm = TRUE)
  )
head(met_ncld_us_grid_plot)
summary(met_ncld_us_grid_plot)

#### Temp
met_tmmx <-
  ggplot() +
  geom_point(data = met_ncld_us_grid_plot,
             aes(x = Longitude, y = Latitude, color = tmmx),
             size = 0.35, alpha = 0.8) +
  geom_sf(data = us_states,
          fill = NA, color = "grey70", size = 0.3) +
  scale_color_viridis_c(option = "plasma") +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "tmmx") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 19),
    plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
    # plot.subtitle = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 19)
  )
# met_tmmx

met_tmmn <-
  ggplot() +
  geom_point(data = met_ncld_us_grid_plot,
             aes(x = Longitude, y = Latitude, color = tmmn),
             size = 0.35, alpha = 0.8) +
  geom_sf(data = us_states,
          fill = NA, color = "grey70", size = 0.3) +
  scale_color_viridis_c(option = "plasma") +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "tmmn") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 19),
    plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
    # plot.subtitle = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 19)
  )
# met_tmmn
met_Temp = met_tmmx + met_tmmn

temp_name = paste0("METEO_map_Temp_", cmaq_year, ".pdf"); temp_name
ggsave(
  file.path("machine_learning_source_input/ML_plot", temp_name),
  plot = met_Temp, width = 14.5, height = 8.5)

#### RH
met_rmax <-
  ggplot() +
  geom_point(data = met_ncld_us_grid_plot,
             aes(x = Longitude, y = Latitude, color = rmax),
             size = 0.35, alpha = 0.8) +
  geom_sf(data = us_states,
          fill = NA, color = "grey70", size = 0.3) +
  scale_color_viridis_c(option = "plasma") +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "rmax") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 19),
    plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
    # plot.subtitle = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 19)
  )
# met_rmax

met_rmin <-
  ggplot() +
  geom_point(data = met_ncld_us_grid_plot,
             aes(x = Longitude, y = Latitude, color = rmin),
             size = 0.35, alpha = 0.8) +
  geom_sf(data = us_states,
          fill = NA, color = "grey70", size = 0.3) +
  scale_color_viridis_c(option = "plasma") +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "rmin") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 19),
    plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
    # plot.subtitle = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 19)
  )
# met_rmin
met_RH = met_rmax + met_rmin

rh_name = paste0("METEO_map_RH_", cmaq_year, ".pdf"); rh_name
ggsave(
  file.path("machine_learning_source_input/ML_plot", rh_name),
  plot = met_RH, width = 14.5, height = 8.5)

#### Wind
met_th <-
  ggplot() +
  geom_point(data = met_ncld_us_grid_plot,
             aes(x = Longitude, y = Latitude, color = th),
             size = 0.35, alpha = 0.8) +
  geom_sf(data = us_states,
          fill = NA, color = "grey70", size = 0.3) +
  scale_color_viridis_c(option = "plasma") +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "th") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 19),
    plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
    # plot.subtitle = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 19)
  )
# met_th

met_vs <-
  ggplot() +
  geom_point(data = met_ncld_us_grid_plot,
             aes(x = Longitude, y = Latitude, color = vs),
             size = 0.35, alpha = 0.8) +
  geom_sf(data = us_states,
          fill = NA, color = "grey70", size = 0.3) +
  scale_color_viridis_c(option = "plasma") +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "vs") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 19),
    plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
    # plot.subtitle = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 19)
  )
# met_vs
met_Wind = met_th + met_vs

wind_name = paste0("METEO_map_Wind_", cmaq_year, ".pdf"); wind_name
ggsave(
  file.path("machine_learning_source_input/ML_plot", wind_name),
  plot = met_Wind, width = 14.5, height = 8.5)

##### Land cover
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

landcover_df$NLCD.Land.Cover.Class = as.factor(landcover_df$NLCD.Land.Cover.Class)
head(landcover_df); dim(landcover_df)

## NLCD Land Use
met_ncld_us_grid_plot =  
  merge(met_ncld_us_grid_plot, landcover_df, 
        by = "NLCD.Land.Cover.Class", all.x = TRUE)

land_use <-
  ggplot() +
  geom_point(data = met_ncld_us_grid_plot,
             aes(x = Longitude, y = Latitude, color = land_type),
             size = 0.35, alpha = 0.8) +
  geom_sf(data = us_states,
          fill = NA, color = "grey70", size = 0.3) +
  scale_color_viridis_d(option = "plasma") +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "land_type") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 19),
    plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
    # plot.subtitle = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 19)
  )
# land_use

ggsave(
  file.path("machine_learning_source_input/ML_plot", "GridMet_NLCD_2011_Land_Use.pdf"),
  plot = land_use, width = 14.5, height = 8.5)


###### Sulfate ###### 
setDT(cmaq_sulfate_rds)
cmaq_sulfate_use = cmaq_sulfate_rds
  
### round to 2 digits
pmf_sulfate$Longitude = round(pmf_sulfate$Longitude, 2)
pmf_sulfate$Latitude = round(pmf_sulfate$Latitude, 2)

cmaq_sulfate_use$Longitude = round(cmaq_sulfate_use$Longitude, 2)
cmaq_sulfate_use$Latitude = round(cmaq_sulfate_use$Latitude, 2)

# Only use dates in PMF and CMAQ both
dim(cmaq_sulfate_use)
cmaq_sulfate_use = subset(cmaq_sulfate_use, 
                          Date %in% pmf_sulfate$Date)
dim(cmaq_sulfate_use)

pmf_sulfate_use = subset(pmf_sulfate, 
                         Date %in% cmaq_sulfate_use$Date)

# Merge PMF & CMAQ results, keep all points in CMAQ, and keep all sites in PMF with same grid coordinates
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
table(sulfate_cmaq_pmf_met_landtype$land_type)
summary(sulfate_rf_use)

write_fst(sulfate_cmaq_pmf_met_landtype,
  file.path(
    paste0("machine_learning_source_input/Sulfate_all_CMAQ_points_input_", cmaq_period, ".fst")))

write_fst(sulfate_rf_use,
          file.path(
            paste0("machine_learning_source_input/Sulfate_only_PMF_points_input_", cmaq_period, ".fst")))

# sulfate_cmaq_pmf_met_landtype = read_fst(
#   file.path(
#     paste0("machine_learning_source_input/Sulfate_all_CMAQ_points_input_", cmaq_period, ".fst")))
# head(sulfate_cmaq_pmf_met_landtype); dim(sulfate_cmaq_pmf_met_landtype)

###### Dust ###### 
setDT(cmaq_dust_rds)
cmaq_dust_use = cmaq_dust_rds

### round to 2 digits
pmf_dust$Longitude = round(pmf_dust$Longitude, 2)
pmf_dust$Latitude = round(pmf_dust$Latitude, 2)

cmaq_dust_use$Longitude = round(cmaq_dust_use$Longitude, 2)
cmaq_dust_use$Latitude = round(cmaq_dust_use$Latitude, 2)

# Only use dates in PMF & CMAQ both
dim(cmaq_dust_use)
cmaq_dust_use = subset(cmaq_dust_use, 
                          Date %in% pmf_dust$Date)
dim(cmaq_dust_use)

pmf_dust_use = subset(pmf_dust, 
                      Date %in% cmaq_dust_use$Date)

# Merge PMF & CMAQ results, keep all points in CMAQ, and keep all sites in PMF with same grid coordinates
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

write_fst(dust_cmaq_pmf_met_landtype,
          file.path(
            paste0("machine_learning_source_input/Dust_all_CMAQ_points_input_", cmaq_period, ".fst")))

write_fst(dust_rf_use,
          file.path(
            paste0("machine_learning_source_input/Dust_only_PMF_points_input_", cmaq_period, ".fst")))

###### Traffic ######
setDT(cmaq_traffic_rds)
cmaq_traffic_use = cmaq_traffic_rds

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

# Only use dates in PMF & CMAQ both
dim(cmaq_traffic_use)
cmaq_traffic_use = subset(cmaq_traffic_use,
                          Date %in% pmf_traffic$Date)
dim(cmaq_traffic_use)

pmf_traffic_use = subset(pmf_traffic,
                         Date %in% cmaq_traffic_use$Date)

# Merge PMF & CMAQ results, keep all points in CMAQ, and keep all sites in PMF with same grid coordinates
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
summary(traffic_rf_use); dim(traffic_rf_use)
length(unique(traffic_rf_use$SiteCode))

dim(traffic_cmaq_pmf_met_landtype_road_census); dim(traffic_rf_use)
head(traffic_cmaq_pmf_met_landtype_road_census)
summary(traffic_cmaq_pmf_met_landtype_road_census)
paste0("machine_learning_source_input/Traffic_all_CMAQ_points_input_", cmaq_period, ".fst")

# Output files
write_fst(traffic_cmaq_pmf_met_landtype_road_census,
          file.path(
            paste0("machine_learning_source_input/Traffic_all_CMAQ_points_input_", cmaq_period, ".fst")))

write_fst(traffic_rf_use,
          file.path(
            paste0("machine_learning_source_input/Traffic_only_PMF_points_input_", cmaq_period, ".fst")))


# #### GRIDMET & to sf file ####
# # read with terra::rast not raster, otherwise, the name info will be lost
# gridmet_year_rast = terra::rast("pmf_ncld_meteo_census/Common_GRIDMET_2011_stacked.tif")
# 
# # Extract the number of layers (bands) & coordinates (X and Y)
# meteo_layers <- nlyr(gridmet_year_rast)
# meteo_coords <- as.data.table(xyFromCell(gridmet_year_rast, 1:ncell(gridmet_year_rast)))
# 
# # Convert the raster to a data frame (with each band as a separate column)
# gridmet_year_df = as.data.table(as.matrix(gridmet_year_rast))
# 
# # Add coordinates (X and Y) to the data table
# gridmet_year_dt <- cbind(meteo_coords, gridmet_year_df)
# dim(gridmet_year_dt)
# head(gridmet_year_dt[, 1:10])
# 
# # The original data is too long to convert, handle it with chunks
# # Function to melt a chunk of data
# gridmet_melt_chunk <- 
#   function(data_chunk, meteo_cols_to_melt) {
#   melt(data_chunk, 
#        id.vars = c("x", "y"), 
#        measure.vars = meteo_cols_to_melt, 
#        variable.name = "Met_var", 
#        value.name = "Met_value")
# }
# 
# # Define months to loop over 
# months <- sprintf("-%02d-", 1:12)
# 
# # Initialize an empty list to store the melted chunks
# gridmet_melted_chunk_list <- list()
# 
# # The meteo variables to use 
# meteo_var_comm <- 
#   c("tmmx", "tmmn", "rmax", "rmin", "vs", "th") 
# col_order = c("x", "y", "Date", meteo_var_comm)
# 
# # Process the dataset in chunks
# for (month in months) {
#   # Extract the columns x, y, and those that contain the current month in the name
#   meteo_data_chunk <- 
#     gridmet_year_dt[, .SD, .SDcols = c("x", "y", grep(month, names(gridmet_year_dt), value = TRUE))]
#   
#   # Reshape the data into long format (one row per X, Y, and Date)
#   meteo_cols_to_melt <- 
#     grep("_(\\d{4}-\\d{2}-\\d{2})$", names(meteo_data_chunk), value = TRUE)
#   
#   # Melt the chunk 
#   gridmet_melted_chunk <- gridmet_melt_chunk(meteo_data_chunk, meteo_cols_to_melt)
#   
#   # Split the 'Met_var' column into 'Variable' and 'Date'
#   gridmet_melted_chunk[, c("Variable", "Date") := tstrsplit(Met_var, "_", fixed = TRUE)]
#   gridmet_melted_chunk$Met_var = NULL
#   
#   # Expand to list all meteorological variable columns
#   gridmet_melted_wide <- 
#     gridmet_melted_chunk %>%
#     pivot_wider(
#       names_from = Variable,  
#       values_from = Met_value  
#     )
#   
#   # setDT(gridmet_melted_wide)
#   
#   # Convert to Date format
#   # gridmet_melted_wide$Date = 
#   #   as.Date(gridmet_melted_wide$Date, format = "%Y-%m-%d")
#   
#   # Store the result
#   gridmet_melted_chunk_list[[length(gridmet_melted_chunk_list) + 1]] <- 
#     gridmet_melted_wide
#   
#   # Optionally print progress
#   print(paste0("Processed month ", month))
# }
# 
# # Combine all the melted chunks into a single data.table
# gridmet_year_long <- rbindlist(gridmet_melted_chunk_list)
# 
# met_year = 2011 #  year(gridmet_melted_wide$Date[1])
# 
# write_fst(gridmet_year_long, file.path("pmf_ncld_meteo_census/", paste0("GRIDMET_commom_", met_year, ".fst")))
# 
# gridmet_year_long = read_fst("pmf_ncld_meteo_census/GRIDMET_commom_2011.fst")
# setDT(gridmet_year_long)

