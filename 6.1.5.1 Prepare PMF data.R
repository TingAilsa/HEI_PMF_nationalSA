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


###### Extract and manage source specific data from PMF ######

# read files
csn_imp_site_with_nearest =
  st_read( file.path("base_raster_grid_sf/PMF_sites_in_US_grid_01.fgb"))

# pmf_source = fread("pmf_ncld_meteo_census/CSN_IMPROVE_source_daily_contribution.csv")
pmf_source = read.fst("pmf_ncld_meteo_census/CSN_IMPROVE_Daily_Source_Impacts_region_2011-20.fst")
date_use = read.fst("pmf_ncld_meteo_census/Date_DOW_Holiday_2011-20.fst")
head(pmf_source); head(date_use)
sapply(pmf_source, class); sapply(date_use, class)

# Merge date info with source info
# pmf_source$Date = as.Date(pmf_source$Date)
pmf_source = base::merge(pmf_source, date_use, all.x = TRUE)

# Raster does not include date, convert date to integer and change back later
# pmf_source$Date <- as.integer(format(pmf_source$Date, "%Y%m%d"))

# change the characters to factor
pmf_source$day_of_week = as.factor(pmf_source$day_of_week)
pmf_source$is_holiday = as.factor(pmf_source$is_holiday)
sapply(pmf_source, class)

# change colnames
# names(pmf_source)[3:4]
# names(pmf_source)[3:4] = c("org_long", "org_lat")
names(pmf_source)[7:8]
names(pmf_source)[7:8] = c("org_long", "org_lat")

csn_imp_site_with_nearest$org_long = csn_imp_site_with_nearest$org_lat = NULL

# Assign new long and lat from US grid
pmf_source_US = merge(pmf_source, csn_imp_site_with_nearest,
                      by = "SiteCode", all.x = TRUE)
unique(pmf_source_US$Source_aftermanual)

# Change different traffic/sea salt from CSN & IMPROVE to one traffic/sea salt
pmf_source_US <-
  pmf_source_US %>%
  mutate(Source_aftermanual =
           ifelse(grepl("Traffic", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
                  "F1-Traffic",
                  Source_aftermanual))

pmf_source_US <-
  pmf_source_US %>%
  mutate(Source_aftermanual =
           ifelse(grepl("Sea Salt", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
                  "F6-Salt",
                  Source_aftermanual))

pmf_source_US <-
  pmf_source_US %>%
  mutate(Source_aftermanual =
           ifelse(grepl("ailpipe", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
                  "F7-Non-tailpipe",
                  Source_aftermanual))

pmf_source_US <-
  pmf_source_US %>%
  mutate(Source_aftermanual =
           ifelse(grepl("Industry", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
                  "F5-Industry",
                  Source_aftermanual))

pmf_source_US <-
  pmf_source_US %>%
  mutate(Source_aftermanual =
           ifelse(grepl("Biomass", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
                  "F8-Biomass",
                  Source_aftermanual))

unique(pmf_source_US$Source_aftermanual)

# change to factor for modeling
pmf_source_US$Source_aftermanual = as.factor(pmf_source_US$Source_aftermanual)

# Only one dataset
pmf_source_US$Dataset.y = NULL
pmf_source_US = 
  plyr::rename(
  pmf_source_US,
  c("Dataset.x" = "Dataset")
)

# Get the tOTAl contribution of new groups
pmf_source_US_sumContri <-
  pmf_source_US %>%
  group_by(SiteCode, Date, Source_aftermanual, Latitude, Longitude,
           year, month, day_of_week, is_holiday, Dataset) %>%
  dplyr::summarise(Concentration = sum(Concentration),
                   .groups = "drop")
unique(pmf_source_US_sumContri$Source_aftermanual)

# PMF data for each sources
pmf_traffic = subset(pmf_source_US_sumContri, Source_aftermanual == "F1-Traffic")
pmf_nitrate = subset(pmf_source_US_sumContri, Source_aftermanual == "F2-Secondary Nitrate")
pmf_sulfate = subset(pmf_source_US_sumContri, Source_aftermanual == "F3-Secondary Sulfate")
pmf_industry = subset(pmf_source_US_sumContri, Source_aftermanual == "F5-Industry")
pmf_salt = subset(pmf_source_US_sumContri, Source_aftermanual == "F6-Salt")
pmf_biomass = subset(pmf_source_US_sumContri, Source_aftermanual == "F8-Biomass")
pmf_dust = subset(pmf_source_US_sumContri, Source_aftermanual == "F8-Soil/Dust")
pmf_nontailpipe = subset(pmf_source_US_sumContri, Source_aftermanual == "F7-Non-tailpipe")
pmf_oprich = subset(pmf_source_US_sumContri, Source_aftermanual == "F10-OP-rich")

write_fst(pmf_traffic, "pmf_ncld_meteo_census/PMF_F1-Traffic.fst")
write_fst(pmf_nitrate, "pmf_ncld_meteo_census/PMF_F2-Secondary_Nitrate.fst")
write_fst(pmf_sulfate, "pmf_ncld_meteo_census/PMF_F3-Secondary_Sulfate.fst")
write_fst(pmf_industry, "pmf_ncld_meteo_census/PMF_F5-Industry.fst")
write_fst(pmf_salt, "pmf_ncld_meteo_census/PMF_F6-Salt.fst")
write_fst(pmf_biomass, "pmf_ncld_meteo_census/PMF_F8-Biomass.fst")
write_fst(pmf_dust, "pmf_ncld_meteo_census/PMF_F9-Soil_Dust.fst")
write_fst(pmf_nontailpipe, "pmf_ncld_meteo_census/PMF_F7-Non-tailpipe.fst")
write_fst(pmf_oprich, "pmf_ncld_meteo_census/PMF_F10-OP-rich.fst")

### Preparing PM2.5 data
pmf_pm25_org = fread("pmf_ncld_meteo_census/CSN_IMPROVE_Daily_PM_2011-20.csv")
pmf_pm25_org =
  merge(
    dplyr::select(
      pmf_pm25_org, Dataset, SiteCode, Date, PM2.5_pred_org),
    date_use)

# change the characters to factor
pmf_pm25_org$day_of_week = as.factor(pmf_pm25_org$day_of_week)
pmf_pm25_org$is_holiday = as.factor(pmf_pm25_org$is_holiday)
sapply(pmf_pm25_org, class)

# Assign new long and lat from US grid
csn_imp_site_with_nearest$org_long = csn_imp_site_with_nearest$org_lat = NULL
pmf_pm25 = 
  merge(pmf_pm25_org, 
        dplyr::select(csn_imp_site_with_nearest, -Dataset),
        by = "SiteCode", all.x = TRUE) %>%
  st_drop_geometry()
pmf_pm25$geometry = NULL

# Rename
pmf_pm25 = 
  plyr::rename(pmf_pm25,
               c("PM2.5_pred_org" = "Concentration"))
head(pmf_pm25)
length(unique(pmf_pm25$SiteCode))

write_fst(pmf_pm25, "pmf_ncld_meteo_census/PMF_total_PM2.5.fst")



