library(data.table)
library(dplyr)
library(plyr)
library(base)
library(sf)
library(ggplot2)
library(furrr)
library(USAboundaries)
library(quanteda)
library(dplyr)
library(tidycensus) # ACS census
library(terra) # .img
library(raster) # .img
library(ncdf4) # .nc
library(oro.dicom) # .vol
library(fst)
library(ff) # handle large files without hitting memory limits
library(bigmemory)  # handle large files without hitting memory limits

#### Read the generated census level geometry, after ACScensus step ####

# census_tract_geoid_geo =
#   st_read(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS", 
#                    "ACS_census_tract_geoid_geometry.gpkg"))
# head(census_tract_geoid_geo)
# 
# census_tract_geoid_geo_4326 <- st_transform(census_tract_geoid_geo, crs = 4326)
# head(census_tract_geoid_geo_4326)
# 
# census_tract_geo = dplyr::select(census_tract_geoid_geo_4326, GEOID, geom)
# st_geometry(census_tract_geo) <- "geometry" # reassign the geometry column
# head(census_tract_geo)
# 
# st_write(census_tract_geo, 
#          file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS", 
#                    "ACS_census_tract_geoid_geometry_4326.gpkg"))

census_tract_geo =
  st_read(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS", 
                    "ACS_census_tract_geoid_geometry_4326.gpkg"))
head(census_tract_geo)

#### Unique CSN dates ####

setwd("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nation_SA_data/")
getwd()

data_use = "CSN_Site_15t1mdl0unc"

dir_path <- paste0("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/", 
                   data_use, "/base_DISPres1")

# list & merge all daily data
csv_daily_list <- list.files(dir_path, pattern = ".*daily\\.csv$", full.names = TRUE)

csv_daily <- 
  do.call(
    rbind, 
    (lapply(
      csv_daily_list, 
      fread)))
# csv_daily$Dataset = "CSN"
csv_daily$V1 = NULL

# extract all unique site & date combinations
site_date_unique = select(csv_daily, Serial.No, Date)
# remove duplicates
site_date_unique = site_date_unique[!duplicated(site_date_unique), ]

# check the dates included 
daily_dates = data.frame(table(csv_daily$Date))
names(daily_dates) = c("Date", "Freq")
daily_dates$Date = as.Date(daily_dates$Date)

# change to yyyymmdd format
daily_dates$date_numeric = format(daily_dates$Date, "%Y%m%d")

# get yyyy/mm format
daily_dates$year_month = format(daily_dates$Date, "%Y/%m")

head(daily_dates)
dim(daily_dates)

# total date number
n_date = nrow(daily_dates)

write.csv(daily_dates, "CSN_unique_date_use.csv", row.names = FALSE)
write.csv(site_date_unique, "CSN_unique_site_date_pairs.csv", row.names = FALSE)


########################################################################
#### Aim3 predictor downloading ####
########################################################################

#### HMS-SMOKE, NOAA ####

setwd("/Users/TingZhang/XXXX")
getwd()

# read data with all used dates 
daily_dates = fread("CSN_unique_date_use.csv")
n_date = nrow(daily_dates)
daily_dates$Date = as.Date(daily_dates$Date)

# read data with all site_date pairs
site_date_unique = fread("CSN_unique_site_date_pairs.csv")

# Hazard Mapping System, HMS
onedrive_path = "/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nation_SA_data/"

# create folder to save data
hms_smoke_folder = "HMS_SMOKE_shp"
if (!dir.exists(hms_smoke_folder)) {
  dir.create(hms_smoke_folder)
}

# set default folder
setwd(file.path(onedrive_path, hms_smoke_folder))
getwd()

###### SMOKE 1. Download - shape - whole study period ######

# download data to local folder
# examples of links
# date.no = 10, "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2011/02/hms_smoke20110202.zip"
# https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2016/06/hms_smoke20160606.zip

# download data to local folder
for(date.no in 1:n_date){ # missing date, 20150602, 20170718
  
  # extract unique info related to dates in link
  year_month_link = daily_dates$year_month[date.no]
  date_link = daily_dates$date_numeric[date.no]
  
  # create url
  url = paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/",
               year_month_link, "/hms_smoke", date_link, ".zip")
  destfile <- paste0("hms_smoke", date_link, ".zip")
  
  # download data, and use tryCatch in case of missing link
  tryCatch({
    
    download.file(url, file.path(destfile))

    # unzip and remove zip
    unzip(destfile, exdir = hms_smoke_folder)
    unlink(destfile)
    
  }, error = function(e) {
    
    print(paste("Error at iteration", date_link))
  })
}

###### SMOKE 2. combine & process shp data ######
# some tries 
# date_kml_1 = st_read("HMS_SMOKE_shp/hms_smoke20200518.shp") # 20200518, 20110106
# dim(date_kml_1); head(date_kml_1)
# length(unique(date_kml_1$geometry))
# 
# ggplot(date_kml_1) + geom_sf(fill = "#69b3a2", color = "white") + theme_void()

hms_smoke_folder = "/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nation_SA_data/HMS_SMOKE_shp/HMS_SMOKE_shp"
setwd(hms_smoke_folder)

# list & merge all .shp data
hms_shp_list <- list.files(hms_smoke_folder, pattern = ".shp", full.names = TRUE)
length(hms_shp_list)

### In the Start & End column, it include "Year, day of the year, HH:MM", 2011105, the 105th day of 2011, that's 2011-04-15

# read_and_add_day("hms_smoke20110103.shp")

# Set up parallel processing
plan(multisession, workers = availableCores()) # Use all available cores, purrr{}

# Apply the function to each file in the list and combine the results
# future_map_dfr{furrr}, parallel processing and combines results into a data frame, speed up the combining
hms_shp_all <- future_map_dfr(hms_shp_list, read_and_add_day)
summary(hms_shp_all); head(hms_shp_all); dim(hms_shp_all)
unique(hms_shp_all$Density); length(unique(hms_shp_all$Day)) # 1129 valid days

# Create Severity column based on Density
hms_shp_all <- hms_shp_all %>%
  mutate(Severity = case_when(
    Density == "Light"  ~ 1,
    Density == "Medium" ~ 2,
    Density == "Heavy"  ~ 3,
    TRUE ~ NA_real_    # Assign NA for any values not matching the conditions
  ))

# Check for invalid geometries
invalid_geometries <- !st_is_valid(hms_shp_all)
summary(invalid_geometries) # 223 invalid

# Remove rows with invalid geometries
hms_shp_all_valid <- hms_shp_all[!invalid_geometries, ]

# Check for missing values in Severity
sum(is.na(hms_shp_all_valid$Severity))

# Check the geometry type of the valid shapefile
unique(st_geometry_type(hms_shp_all_valid))

# Inspect a sample of geometries
sample_geometries <- hms_shp_all_valid[sample(nrow(hms_shp_all_valid), 100), ]
plot(st_geometry(sample_geometries))

# Identify potentially problematic geometries
problematic_indices <- which(!st_is_valid(hms_shp_all_valid))
problematic_geometries <- hms_shp_all_valid[problematic_indices, ]

geom_types <- st_geometry_type(hms_shp_all_valid)
table(geom_types)

# in the subset analyses, find problematic data during row 30000:33000, something out of mainland US
# ggplot(data = hms_shp_all_valid[30000:33000, ]) +
#   geom_sf(aes(fill = Severity)) +
#   theme_minimal()

# Define the approximate bounding box for mainland US
apprx_bbox <- 
  st_sfc(st_polygon(list(matrix(c(
  -125, 24,  # bottom-left
  -66.5, 24, # bottom-right
  -66.5, 49, # top-right
  -125, 49,  # top-left
  -125, 24   # close the polygon
), ncol = 2, byrow = TRUE))))
st_crs(apprx_bbox) <- 4326

# Filter the geometries based on the bounding box
hms_shp_all_valid_us <-
  hms_shp_all_valid[
    st_intersects(
      hms_shp_all_valid, apprx_bbox, sparse = FALSE), ]

st_write(hms_shp_all_valid_us,
         "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/HMS_SMOKE/NOAA_HMS_CSN_IMPROVE_daily_match_2011-20.gpkg")

ggplot(data = hms_shp_all_valid) +
  geom_sf(aes(fill = Severity)) + 
  theme_minimal() +         
  labs(title = "Severity Map",
       fill = "Severity")    # Add labels


###### SMOKE 3. match with IMPROVE & CSN sites, .csv ######
#  site GPS
site_geoid = read.csv("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nationwide_SA/data/intermediate/pmf/PMF_progress_files/CSN_IMPROVE/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
site_geoid = select(site_geoid, Dataset, SiteCode, Longitude, Latitude, geoid)

# Convert site_geoid to an sf object
site_geoid_sf <- st_as_sf(site_geoid, coords = c("Longitude", "Latitude"), crs = 4326)

# Check CRS 
st_crs(site_geoid_sf) 
st_crs(hms_shp_all_valid_us)

# Perform a spatial join
site_geoid_joined <- st_join(site_geoid_sf, hms_shp_all_valid_us)

site_smoke_day = site_geoid_joined
site_smoke_day$Date = as.Date(as.character(site_smoke_day$Day), format = "%Y%m%d")
site_smoke_day$Satellite = site_smoke_day$geometry = site_smoke_day$Day = NULL

# write into a file with geometry
# write.csv(site_smoke_day, "NOAA_HMS_CSN_IMPROVE_daily_match_2011-20.csv")

####### calculate & plot the accumulated smoke

# get the year & month
site_smoke_day$year = year(site_smoke_day$Date)
site_smoke_day$month = month(site_smoke_day$Date)

# calculate the accumulated smoke
site_smoke_accum =
  site_smoke_day %>%
  dplyr::group_by(Dataset, SiteCode, geoid) %>%
  dplyr::summarize(Severity_sum = sum(Severity),
                   Severity_median = median(Severity),
                   Severity_mean = mean(Severity),
                   .groups = "drop")

# calculate the accumulated smoke by year or month
site_smoke_accum_year =
  site_smoke_day %>%
  dplyr::group_by(Dataset, SiteCode, geoid, year) %>%
  dplyr::summarize(Severity_sum = sum(Severity),
                   Severity_median = median(Severity),
                   Severity_mean = mean(Severity),
                   .groups = "drop")

site_smoke_accum_month = 
  site_smoke_day %>%
  dplyr::group_by(Dataset, SiteCode, geoid, month) %>%
  dplyr::summarize(Severity_sum = sum(Severity),
                   Severity_median = median(Severity),
                   Severity_mean = mean(Severity),
                   .groups = "drop")

# match with GPS
site_smoke_accum = merge(site_smoke_accum, site_geoid)
site_smoke_accum_year = merge(site_smoke_accum_year, site_geoid)
site_smoke_accum_month = merge(site_smoke_accum_month, site_geoid)

###### SMOKE 4. mapping the HMS trends ######

# us_states, mainland
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

# plot the severity, total smoke
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = site_smoke_accum, 
             aes(x = Longitude, y = Latitude, 
                 fill = Severity_sum),
             size = 2.5, alpha = 0.8, 
             shape = 21, color = "grey66") +
  scale_fill_gradient2(low = "white", high = "red", 
                       limits = c(0, NA),
                       oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  # guides(color=guide_legend(title="Slope: µg/m3")) + 
  coord_sf(datum = NA) +
  labs(fill=addline_space("Accumulated Severity")) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10),
        axis.title = element_text(size = 0),
        # legend.position = c(0.95, 0.18),
        legend.text = element_text(size = 14), 
        legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
        legend.spacing.y = unit(1, "cm")) 

# plot the severity, total smoke, by year
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = site_smoke_accum_year, 
             aes(x = Longitude, y = Latitude, 
                 # fill = Severity_median),
                 fill = Severity_sum),
             size = 2.5, alpha = 0.8, 
             shape = 21, color = "grey66") +
  scale_fill_gradient2(low = "white", high = "red", 
                       limits = c(0, NA),
                       oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  # guides(color=guide_legend(title="Slope: µg/m3")) + 
  facet_wrap(~ year) + # , ncol = 3
  coord_sf(datum = NA) +
  labs(fill=addline_space("Accumulated Severity")) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10),
        axis.title = element_text(size = 0),
        legend.position = c(0.8, 0.12),
        legend.text = element_text(size = 14), 
        legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
        legend.spacing.y = unit(1, "cm")) 

# plot the severity, total smoke, by month
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = site_smoke_accum_month, 
             aes(x = Longitude, y = Latitude, 
                 # fill = Severity_median),
                 fill = Severity_sum),
             size = 2.5, alpha = 0.8, 
             shape = 21, color = "grey66") +
  scale_fill_gradient2(low = "white", high = "red", 
                       limits = c(0, NA),
                       oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  # guides(color=guide_legend(title="Slope: µg/m3")) + 
  facet_wrap(~ month) + # , ncol = 3
  coord_sf(datum = NA) +
  labs(fill=addline_space("Accumulated Severity")) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10),
        axis.title = element_text(size = 0),
        # legend.position = c(0.8, 0.12),
        legend.text = element_text(size = 14), 
        legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
        legend.spacing.y = unit(1, "cm")) 

#### EPA TRI ####

# EPA Toxics Release Inventory (TRI) Program Basic Data Files
# https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-present

# Industry classification according to North American Industry Classification System (NAICS)
# https://www.census.gov/naics/?58967?yearbck=2012

# create folder to save data
tri_file_folder = "EPA_TRI_industry"

if (!dir.exists(file.path(onedrive_path, tri_file_folder))) {
  dir.create(file.path(onedrive_path, tri_file_folder))
}

# set default folder
setwd(file.path(onedrive_path, tri_file_folder))
getwd()

###### TRI 1. Download ######

# download data to local folder
# examples of links
# https://data.epa.gov/efservice/downloads/tri/mv_tri_basic_download/2017_US/csv
# https://data.epa.gov/efservice/downloads/tri/mv_tri_basic_download/2012_US/csv

study_year = 2011:2020

# download data to local folder
for(study_year_use in study_year){ 

  # create url
  url = paste0("https://data.epa.gov/efservice/downloads/tri/mv_tri_basic_download/",
               study_year_use, "_US/csv")
  destfile <- paste0(study_year_use, "_US.csv")
  
  # download data
  download.file(url, file.path(destfile))
}

###### TRI 2. Combine & extract air pollution related info ######

setwd("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nation_SA_data/EPA_TRI_industry")

# read all TRI files
csv_tri_list <- list.files(pattern = ".*US\\.csv$", full.names = TRUE)

tri_overall <- 
  do.call(
    rbind, 
    (lapply(
      csv_tri_list, 
      fread)))
dim(tri_overall)
head(tri_overall)

# remove prefix in colnames
names(tri_overall) = sub("^\\d+\\.\\s*", "", names(tri_overall))

### extract columns to use 
# PARENT CO NAME: The name of the parent company of the facility.
# BIA: Bureau of Indian Affairs region, if applicable (used for facilities on tribal lands).
# HORIZONTAL DATUM: The coordinate system used for the latitude and longitude data.
# FEDERAL FACILITY: Indicates if the facility is a federal facility.
# PRIMARY NAICS: The primary North American Industry Classification System (NAICS) code for the facility.
# CLEAN AIR ACT CHEMICAL: Indicates if the chemical is regulated under the Clean Air Act.
# 5.1 - FUGITIVE AIR: Quantity of the chemical released into the air from fugitive sources.
# 5.2 - STACK AIR: Quantity of the chemical released into the air from stack emissions.
# OFF-SITE RELEASE TOTAL: Total quantity of the chemical released off-site.
# OFF-SITE RECYCLED TOTAL: Total quantity of the chemical recycled off-site.
# 6.2 - TOTAL TRANSFER: Total quantity of the chemical transferred for all purposes.
# TOTAL RELEASES: Total quantity of the chemical released, including all types of releases.
# 8.1 - RELEASES: Total releases reported.

tri_use = 
  select(tri_overall,
         "YEAR", "TRIFD", "FRS ID", "STANDARD PARENT CO NAME", "CITY", "COUNTY", 
         "ST", "ZIP", "BIA", "LATITUDE", "LONGITUDE", "HORIZONTAL DATUM", "FEDERAL FACILITY", 
         "INDUSTRY SECTOR CODE", "INDUSTRY SECTOR", "PRIMARY NAICS", "NAICS 2", 
         "CHEMICAL", "CLEAN AIR ACT CHEMICAL", "CLASSIFICATION", "METAL", "METAL CATEGORY", 
         "CARCINOGEN", "PBT",
         "FORM TYPE", "UNIT OF MEASURE", "5.1 - FUGITIVE AIR", "5.2 - STACK AIR", 
         "OFF-SITE RELEASE TOTAL", "OFF-SITE RECYCLED TOTAL", 
         "6.2 - TOTAL TRANSFER", "TOTAL RELEASES", "8.1 - RELEASES")


# quanteda{tolower}
names(tri_use) = tolower(names(tri_use)) # change capital to lower
names(tri_use) <- gsub(" ", "_", names(tri_use)) # replace space
names(tri_use)[27:28] = c("air_fugitive_source", "air_stack_source")
names(tri_use)[31] = "total_transfer"
names(tri_use)[33] = "total_release_report"

write.csv(tri_use, "EPA_TRI_to_clean.csv")

###### TRI 3. explore the info, and prepare dataset to use ######
tri_use = fread("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nation_SA_data/EPA_TRI_industry/EPA_TRI_to_clean.csv")
tri_use$V1 = NULL

unique(tri_use$industry_sector); length(unique(tri_use$industry_sector))
unique(tri_use$chemical); length(unique(tri_use$chemical))
unique(tri_use$classification); length(unique(tri_use$classification))
unique(tri_use$metal_category); length(unique(tri_use$metal_category))

unique(subset(tri_use, metal_category == "Non_Metal")$chemical)
unique(subset(tri_use, metal_category != "Non_Metal")$chemical)
nrow(subset(tri_use, metal_category != "Non_Metal"))

tri_metal = subset(tri_use, metal_category != "Non_Metal")
unique(tri_metal$chemical); length(unique(tri_metal$chemical)); nrow(tri_metal)

######## extract metals in the "chemical" and save their symbols separately

# create a new column chem_metal to store the metal element symbols
tri_metal$chem_metal = NA
class(tri_metal)

# Create a vector with lowercase metal names and their symbols
metal_symbols <- c(
  "nickel" = "Ni",
  "mercury" = "Hg",
  "vanadium" = "V",
  "zinc" = "Zn",
  "lead" = "Pb",
  "molybdenum" = "Mo",
  "copper" = "Cu",
  "cobalt" = "Co",
  "manganese" = "Mn",
  "chromium" = "Cr",
  "calcium" = "Ca",
  "barium" = "Ba",
  "antimony" = "Sb",
  "selenium" = "Se",
  "beryllium" = "Be",
  "arsenic" = "As",
  "cadmium" = "Cd",
  "thallium" = "Tl",
  "silver" = "Ag",
  "aluminum" = "Al",
  "lithium" = "Li",
  "potassium" = "K",
  "sodium" = "Na",
  "tin" = "Sn",
  "boron" = "B",
  "tungsten" = "W",
  "thorium" = "Th",
  "iron" = "Fe",
  "titanium" = "Ti",
  "osmium" = "Os",
  "zirconium" = "Zr",
  "gold" = "Au"
)

# Add the new column and populate it based on the chemical names
tri_metal$chem_metal <- sapply(tri_metal$chemical, function(chemical_name) {
  # Convert chemical name to lowercase
  lower_chemical <- tolower(chemical_name)
  
  # Find matches
  match <- sapply(names(metal_symbols), function(metal_name) {
    if (grepl(metal_name, lower_chemical, ignore.case = TRUE)) {
      return(metal_symbols[metal_name])
    }
    return(NA)
  })
  
  # Return the first non-NA match if any
  return(na.omit(match)[1])
})

# Check if any chemical including metal was not included
tri_metal_chemMetalNA = subset(tri_metal, is.na(chem_metal))
unique(tri_metal_chemMetalNA$chemical)

# remove the non-metal related records
tri_metal_use = subset(tri_metal, !is.na(chem_metal))

### only use data in mainland US
tri_metal_use = subset(tri_metal_use, 
                       longitude > -125 & longitude < -67 &
                         latitude > 23.5 & latitude < 49.5)

write.csv(tri_metal_use, "EPA_TRI_Metals_2011-20.csv")

###### TRI 4. mapping the metal trends ######

# us_states, mainland
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

# only check certain metals
cared_metal = c("Al", "As", "Ba", "Be", "Ca", "Cd", "Co", "Cr", "Cu", "Fe",
                "Hg", "Mn", "Na", "Ni", "Pb", "Sb", "Se", "Sn", "V", "Zn")
length(cared_metal)

tri_metal_explore = subset(tri_metal_use, chem_metal %in% cared_metal)

tri_metal_explore_med = 
  tri_metal_explore %>%
  dplyr::group_by(trifd, frs_id, standard_parent_co_name, zip, chem_metal) %>%
  dplyr::summarize(total_releases = median(total_releases),
                   air_fugitive_source = median(air_fugitive_source),
                   air_stack_source = median(air_stack_source),
                   longitude = mean(longitude),
                   latitude = mean(latitude),
                   .groups = "drop")


# plot the metal distribution, by metal type
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = tri_metal_explore_med, # subset(tri_metal_explore, year == 2020), 
             aes(x = longitude, y = latitude, 
                 color = chem_metal, size = air_fugitive_source),
             alpha = 0.4) +
  # guides(color=guide_legend(title="Slope: µg/m3")) + 
  facet_wrap(~ chem_metal) +
  coord_sf(datum = NA) +
  labs(fill=addline_space("Accumulated Severity")) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10),
        axis.title = element_text(size = 0),
        # legend.position = c(0.95, 0.18),
        legend.text = element_text(size = 14), 
        legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
        legend.spacing.y = unit(1, "cm")) 


#### US Census, American Community Survey ####

# GEOID of census tract, 11 digits
# Census Tract: STATE (2 digits) + COUNTY (3 digits) + TRACT (6 digits) = 11 digits

###### ACScensus 1.1 Download all census data ######

# check the variables
"https://api.census.gov/data/2020/acs/acs5/variables.xml"
"https://api.census.gov/data/2011/acs/acs5/variables.xml"

##### install API key
# get a Census API key at, http://api.census.gov/data/key_signup.html
census_api_key("e7eaeb297611081d1ec608ba94f10394c8f97ee4", install = TRUE)
readRenviron("~/.Renviron")

####### download ACS data from selected variables

# FIPS of all mainland states in the US
mainland_state_fips = c("01", "02", "04", "05", "06", "08",
                        "09", "10", "12", "13", "16", "17",
                        "18", "19", "20", "21", "22", "23",
                        "24", "25", "26", "27", "28", "29",
                        "30", "31", "32", "33", "34", "35",
                        "36", "37", "38", "39", "40", "41",
                        "42", "44", "45", "46", "47", "48",
                        "49", "50", "51", "53", "54", "55", "56")
length(mainland_state_fips)

# Check all census variables at tract levels
acs_vars <- load_variables(2020, "acs5", cache = TRUE)
acs_vars = subset(acs_vars, geography = "tract")
write.csv(acs_vars, "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_Census_ACS_tract_variables.csv")

# Generate the sequences for each range of variables
B01001_vars <- paste0("B01001_", sprintf("%03d", 1:49)) # population by age and sex
B02001_vars <- paste0("B02001_", sprintf("%03d", 1:10)) # population by race
B08006_vars <- c("B08006_001", "B08006_002", "B08006_008", 
                 paste0("B08006_", sprintf("%03d", 14:17))) # means of transportation to work
B08303_vars <- paste0("B08303_", sprintf("%03d", 1:13)) # Commute time
B12001_vars <- c("B12001_002", "B12001_011") # Marital status
B19001_vars <- "B19001_001" # Household Income
B19019_vars <- paste0("B19019_", sprintf("%03d", 1:8)) # Median household income by house size (how many people) with inflation adjusted
B25077_vars <- "B25077_001" # Median Home Value
B25058_vars <- "B25058_001" # Median contract rent
B25001_vars <- "B25001_001" # Housing Units
B25068_vars <- "B25068_001" # Bedrooms by gross rent

# Combine all the variable sequences into a single list
variable_list <- c(B01001_vars, B02001_vars, B08006_vars, B08303_vars, 
                   B12001_vars, B19001_vars, B19019_vars, B25077_vars, 
                   B25058_vars, B25001_vars, B25068_vars)
length(variable_list)

# Initialize empty files to store the data for each period and all periods all states
all_year_data <- list()
all_year_state_census = NULL

# other settings
study_year = 2011:2020
county_code <- NULL  # NULL means data for all counties in the state; set to a specific county FIPS code if needed
geography_select = "tract"

# Loop through each year and each state to download the data
for (state_code in mainland_state_fips) {
  for (acs_year in study_year) {
    # Specify the corresponding ACS 5-Year period
    acs_year = acs_year
    message("Downloading data for ACS ", acs_year)
    
    # Download all data for the available variables at census tract level
    single_year_data <- 
      get_acs(
        geography = geography_select,
        variables = variable_list,
        year = acs_year,
        survey = "acs5",
        state = state_code,
        county = county_code
      )
    
    single_year_data$NAME = single_year_data$moe = NULL # Remove name and moe info
    # dim(single_year_data)
    
    # Add the ACS year column
    single_year_data$year = acs_year

    # Store the data in the list
    all_year_data[[as.character(acs_year)]] <- single_year_data
  }
  
  # Combine all census from each state into a single data frame
  all_year_census <- bind_rows(all_year_data)
  all_year_state_census = rbind(all_year_state_census, all_year_census)
}

dim(all_year_state_census)
head(all_year_state_census)

# Save the combined data to a CSV file (optional)
write.csv(all_year_state_census, 
          "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_Census_ACS_tract_2011-2020.csv", 
          row.names = FALSE)

all_year_state_census = fread("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_Census_ACS_tract_2011-2020.csv")
head(all_year_state_census)

length(unique(all_year_state_census$GEOID))
census_tract_geoid = data.frame(table(all_year_state_census$GEOID))
head(census_tract_geoid); dim(census_tract_geoid)
census_tract_geoid$Var1[1:5]
names(census_tract_geoid)[1] = "GEOID"
census_tract_geoid$GEOID = as.character(census_tract_geoid$GEOID)

write.csv(census_tract_geoid, "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_Census_ACS_tract_geoid.csv")

###### ACScensus 1.1.2 plot the number of GEOID by state ######
census_tract_geoid$state <- substr(as.character(census_tract_geoid$GEOID), 1, 2)
census_tract_state = data.frame(table(census_tract_geoid$state))
names(census_tract_state)[1] = "statefp"

# Create a data frame with state names and their corresponding FIPS codes
state_fips <- data.frame(
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                 "Connecticut", "Delaware", "Florida", "Georgia", "Idaho", "Illinois",
                 "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
                 "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                 "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                 "New Jersey", "New Mexico", "New York", "North Carolina",
                 "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
                 "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
                 "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
                 "Wisconsin", "Wyoming"),
  statefp = c("01", "02", "04", "05", "06", "08",
                "09", "10", "12", "13", "16", "17",
                "18", "19", "20", "21", "22", "23",
                "24", "25", "26", "27", "28", "29",
                "30", "31", "32", "33", "34", "35",
                "36", "37", "38", "39", "40", "41",
                "42", "44", "45", "46", "47", "48",
                "49", "50", "51", "53", "54", "55", "56")
)

census_tract_state = merge(census_tract_state, statefp)


library(USAboundaries)
library(USAboundariesData)

# generate the US county boundary data
MainStates <- map_data("state")

us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]
us_states_geo = dplyr::select(us_states, state_name, statefp, stusps, geometry)

census_tract_state = merge(census_tract_state, us_states_geo)
st_geometry(census_tract_state) <- "geometry" # reassign the geometry column

ggplot() +
  geom_sf(data = us_states, 
          fill = NA, color = "white") +  # Fill color for states without border
  geom_sf(data = census_tract_state, 
          aes(fill = Freq), 
          color = "white", lwd = 0.5, alpha = 0.5) +
  scale_fill_viridis_c(option = "plasma") + 
  theme_minimal() 

###### ACScensus 1.2 Download & merge all census tract geoid & paired geometry, 2020 data ######

## census shapefile
# https://www2.census.gov/geo/tiger/TIGER2020/TRACT/

## link example
# https://www2.census.gov/geo/tiger/TIGER2020/TRACT/tl_2020_01_tract.zip
# https://www2.census.gov/geo/tiger/TIGER2020/TRACT/tl_2020_56_tract.zip

### basic info, TIGER2020 & TIGER2013 (the first year with tract info)
## attention! two year, 2020 and 2013, to see if the combined GEOID can cover all those appeared in census tract data
## use "Replace" to conduct downloading and merging for the two year data seperately

file_serials = 1:78
base_url = "https://www2.census.gov/geo/tiger/TIGER2020/TRACT/"

setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/Census_tract_geoid_2020/")

# download data to local folder
for(file_serial_No in file_serials){ # missing date, 20150602, 20170718
  # change to two-digit mode
  file_serial <- sprintf("%02d", file_serial_No)
  
  # tract_file
  tract_file = paste0("tl_2020_", file_serial, "_tract.zip")
  
  # create url & destination file path
  url <- paste0(base_url, tract_file)
  destfile <- file.path(getwd(), tract_file)
  
  # download data, and use tryCatch in case of missing link
  tryCatch({
    
    download.file(url, destfile, mode = "wb")
    
    # unzip and remove zip
    unzip(destfile, 
          exdir = file.path(getwd(), 
                            paste0("census_tract_geoid_geometry_", file_serial)))
    unlink(destfile)
    
  }, error = function(e) {
    
    print(paste("Error at iteration", file_serial))
  })
}


##### merging data from all states
# subfolder for the census tract geoid & geometry file of each state
census_tract_geoid_folder = getwd()
tract_geoid_subfolders = list.dirs(census_tract_geoid_folder, recursive = FALSE)

# Initialize an empty list to store the spatial data frames
tract_geoid_shp_list <- list()

# Loop over each subfolder to get the .shp
for (tract_geoid_sub in tract_geoid_subfolders) {
  # Extract the file_serial from the folder name
  file_serial <- sub("\\D+", "", basename(tract_geoid_sub))
  
  # Construct the path to the .shp file in the current subfolder
  tract_geoid_shp_path <- file.path(tract_geoid_sub, paste0("tl_2020_", file_serial, "_tract.shp"))
  
  # Read the .shp file
  tract_geoid_shp <- st_read(tract_geoid_shp_path)
  
  # Append to the list
  tract_geoid_shp_list[[length(tract_geoid_shp_list) + 1]] <- tract_geoid_shp
}

# Combine all shapefiles into one
combined_tract_geoid_geo_shp <- do.call(rbind, tract_geoid_shp_list)
dim(combined_tract_geoid_geo_shp)
head(combined_tract_geoid_geo_shp)

# save as .gpkg format to keep the geometry info
st_write(combined_tract_geoid_geo_shp, 
          file.path(census_tract_geoid_folder, "All_census_tract_geoid_geometry_2020.gpkg"))

# check how the GEOID looks like
geoid_tract_geometry = data.frame(table(combined_tract_geoid_geo_shp$GEOID))
dim(geoid_tract_geometry); head(geoid_tract_geometry)
geoid_tract_geometry[1, ]; geoid_tract_geometry[154, ]; geoid_tract_geometry[49877, ]




###### ACScensus 2. Match geoid and geometry with geographic info ######

###### geoid from census data
census_tract_geoid = fread("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_Census_ACS_tract_geoid.csv")
census_tract_geoid$V1 = NULL
head(census_tract_geoid); dim(census_tract_geoid)

# convert all GEOID into 11 digits character
census_tract_geoid$GEOID <- ifelse(
  nchar(as.character(census_tract_geoid$GEOID)) < 11,
  sprintf("%011s", as.character(census_tract_geoid$GEOID)),
  as.character(census_tract_geoid$GEOID)
)
census_tract_geoid$GEOID[90]; census_tract_geoid$GEOID[90000]
dim(census_tract_geoid)

###### geoid with geometry from 2013 & 2020
used_tract_geoid_geo_shp_2013 =
  st_read("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/Census_tract_geoid_2013/All_census_tract_geoid_geometry_2013.gpkg")
used_tract_geoid_geo_shp_2020 =
  st_read("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/Census_tract_geoid_2020/All_census_tract_geoid_geometry_2020.gpkg")

# check if there is overlap in GEOID in the two year data
summary(census_tract_geoid$GEOID %in% used_tract_geoid_geo_shp_2020$GEOID)
summary(census_tract_geoid$GEOID %in% used_tract_geoid_geo_shp_2013$GEOID)

used_tract_geoid_geo_shp_2013$InCensus = used_tract_geoid_geo_shp_2020$InCensus = "Y"
used_tract_geoid_geo_shp_2013$InCensus[!(used_tract_geoid_geo_shp_2013$GEOID %in% census_tract_geoid$GEOID)] = "N"
used_tract_geoid_geo_shp_2020$InCensus[!(used_tract_geoid_geo_shp_2020$GEOID %in% census_tract_geoid$GEOID)] = "N"

geoid_census_in_geometry_13 <-
  ggplot(used_tract_geoid_geo_shp_2013) +
  geom_sf(aes(fill = InCensus), lwd = 0.02) +
  scale_fill_manual(values = c("Y" = "#377eb8", "N" = "#ff7f00")) +
  theme_minimal()

geoid_census_in_geometry_20 <-
  ggplot(used_tract_geoid_geo_shp_2020) +
  geom_sf(aes(fill = InCensus), lwd = 0.02) +
  scale_fill_manual(values = c("Y" = "#377eb8", "N" = "#ff7f00")) +  
  theme_minimal()

ggsave("geoid_census_in_geometry_2013.pdf", plot = geoid_census_in_geometry_13, width = 12, height = 7.4)
ggsave("geoid_census_in_geometry_2020.pdf", plot = geoid_census_in_geometry_20, width = 12, height = 7.4)

used_tract_geoid_geo_shp_2013$InCensus = used_tract_geoid_geo_shp_2020$InCensus = NULL

# rbind and remove duplicates
combined_tract_geoid_geo_shp = 
  rbind(used_tract_geoid_geo_shp_2013, used_tract_geoid_geo_shp_2020)
combined_tract_geoid_geo_shp = 
  combined_tract_geoid_geo_shp[!duplicated(combined_tract_geoid_geo_shp$GEOID), ]

head(combined_tract_geoid_geo_shp)
dim(combined_tract_geoid_geo_shp); dim(used_tract_geoid_geo_shp_2013); dim(used_tract_geoid_geo_shp_2020)

# select variables for merging
used_tract_geoid_geo_shp = 
  dplyr::select(combined_tract_geoid_geo_shp, 
                GEOID, INTPTLAT, INTPTLON, geom)

# convert to long and lat
names(used_tract_geoid_geo_shp)[2:4] = c("Latitude", "Longitude", "geometry")
st_geometry(used_tract_geoid_geo_shp) <- "geometry" # reassign the geometry column

used_tract_geoid_geo_shp$Longitude = as.numeric(used_tract_geoid_geo_shp$Longitude)
used_tract_geoid_geo_shp$Latitude = as.numeric(used_tract_geoid_geo_shp$Latitude)

# only use mainland US
used_tract_geoid_geo_shp = 
  subset(used_tract_geoid_geo_shp, 
         Latitude > 25.8 & Latitude < 49.5 &
           Longitude > -124.75 & Longitude < -66.95)

head(used_tract_geoid_geo_shp); dim(used_tract_geoid_geo_shp)

# match the geometry with GEOID (more GEOID in census_tract_geoid was covered after combining 2013 and 2020 data)
summary(used_tract_geoid_geo_shp$GEOID %in% census_tract_geoid$GEOID)
summary(census_tract_geoid$GEOID %in% used_tract_geoid_geo_shp$GEOID)

# subset and merge files
census_tract_geoid_nogeom = 
  subset(census_tract_geoid, 
         !(GEOID %in% used_tract_geoid_geo_shp$GEOID))
dim(census_tract_geoid_nogeom)

census_tract_geoid_geo = 
  merge(census_tract_geoid, used_tract_geoid_geo_shp)
dim(census_tract_geoid_geo)

census_tract_geoid_geo$Freq = census_tract_geoid_geo$state = NULL
head(census_tract_geoid_geo)

# save as .gpkg format to keep the geometry info
st_write(census_tract_geoid_geo, 
         file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS", 
                   "ACS_census_tract_geoid_geometry.gpkg"))

write.csv(census_tract_geoid_nogeom, 
         file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS", 
                   "ACS_census_tract_geoid_NO_matched_geometry.csv"))

#### NLCD, land cover ####
# download from, https://www.mrlc.gov/data,  NLCD Land Cover (CONUS) All Years

setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/NLCD_landcover")
getwd()

# Define the path to the .img file (associated with the .ige file)
img_file <- "nlcd_2016_land_cover_l48_20210604/nlcd_2016_land_cover_l48_20210604.img"

# Load the raster data using terra
ncld_year <- terra::rast(img_file)
print(ncld_year)
summary(ncld_year)
res(ncld_year); crs(ncld_year); nlyr(ncld_year)

plot(ncld_year, main = "NLCD 2016 Land Cover")

# Get unique land cover values
unique_values <- unique(values(ncld_year))
unique_values

# Calculate the area covered by each land cover class
class_areas <- terra::freq(ncld_year)


#### AQS air pollutants ####

# SO2, "https://aqs.epa.gov/aqsweb/airdata/daily_42401_2020.zip" 
# NO2, "https://aqs.epa.gov/aqsweb/airdata/daily_42602_2016.zip"

###### AQS 1. Download air pollutant data ######
pollutant_codes = c(42401, 42602)
study.year = 2011:2020

air_pollu_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Air_pollutant"

# download 2011-20 NO2 & SO2 to local folder
for(pollutant_code in pollutant_codes){
  for(year.use in study.year){
    url = paste0("https://aqs.epa.gov/aqsweb/airdata/daily_", 
                 pollutant_code, "_",
                 year.use, ".zip")
    destfile <- paste0("daily_", pollutant_code, "_", year.use, ".zip")
    download.file(url, file.path(air_pollu_path, destfile))
    
    # unzip and remove zip
    unzip(file.path(air_pollu_path, destfile), exdir = air_pollu_path)
    unlink(file.path(air_pollu_path, destfile))
  }
}

###### AQS 2. read & merge all data ######
# get a list of all CSV files in the folder
air_pollu_files <- list.files(air_pollu_path, 
                       pattern = "\\.csv$", 
                       full.names = TRUE)

# read & combine
air_pollu_data <- lapply(air_pollu_files, fread)
daily_air_pollu <- rbindlist(air_pollu_data, use.names = TRUE, fill = TRUE)
# daily_air_pollu_1 = daily_air_pollu

# only keep 1-hour data
daily_air_pollu = subset(daily_air_pollu, `Sample Duration` == "1 HOUR") # "1 HOUR" & "3-HR BLK AVG"
head(daily_air_pollu); dim(daily_air_pollu)

# select columns not to be used 
names(daily_air_pollu) <- gsub(" ", ".", names(daily_air_pollu)) # add "." to replace space in names
daily_air_pollu_use = 
  select(daily_air_pollu,
         State.Code, County.Code, Site.Num, Latitude, Longitude, 
         Parameter.Code, Date.Local, Arithmetic.Mean)

daily_air_pollu_use$Pollutant = "SO2"
daily_air_pollu_use$Pollutant[daily_air_pollu_use$Parameter.Code == 42602] = "NO2"
daily_air_pollu_use$Parameter.Code = NULL
names(daily_air_pollu_use)[7:8]
names(daily_air_pollu_use)[7:8] = c("Date", "Concentration_ppb")

daily_air_pollu_use = 
  select(daily_air_pollu_use, Date, Latitude, Longitude, Pollutant, Concentration_ppb)
head(daily_air_pollu_use); dim(daily_air_pollu_use)

write_fst(daily_air_pollu_use, 
          file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Air_pollutant",
                    "EPA_AQS_SO2_NO2_1Hour_2011-20.fst"))

#### PLOT the distribution of these stations
daily_air_pollu_use = 
  read_fst(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Air_pollutant",
          "EPA_AQS_SO2_NO2_1Hour_2011-20.fst"))

head(daily_air_pollu_use)

daily_air_pollu_use$site = paste(daily_air_pollu_use$Latitude, daily_air_pollu_use$Longitude)
air_pollu_site = data.frame(table(daily_air_pollu_use$site))
dim(air_pollu_site) # 1046, 2

#### Roadiness - Lucas ####
# https://osf.io/mucs4/

roadiness_org = fread("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Roadiness/us_roadiness_dataset.csv")

# convert Lambert Conformal Conic (LCC) projection to longitude and latitude
long_lat = 
  as.data.table(lcc_to_longlat(roadiness_org$x, roadiness_org$y))
names(long_lat) = c("Longitude", "Latitude")
roadiness_ll = 
  cbind(long_lat, roadiness_org[, -c(1:2)])

roadiness_ll = 
  subset(roadiness_ll, 
         Latitude > 25.8 & Latitude < 49.5 &
           Longitude > -124.75 & Longitude < -66.95)

dim(roadiness_ll)

write_fst(roadiness_ll, 
          "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Roadiness/us_roadiness_longlat.fst")


#### U.S. Traffic Volume Data ####
setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya")
getwd()

# "https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/#y11"
# "https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/2011/january_2011_ccs_data.zip"

state_year_traff <- readDICOM("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Traffic_volume/2011/1. JAN/CO_JAN_2011 (TMAS).VOL", boffset = 128)
state_year_traff <- readLines("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Traffic_volume/2011/1. JAN/CO_JAN_2011 (TMAS).VOL")
head(as.char(state_year_traff))

state_year_traff_1 = state_year_traff[1]
# Count numeric & alphabetic characters (digits)
num_digits <- sum(grepl("[0-9]", unlist(strsplit(state_year_traff_1, ""))))
num_letters <- sum(grepl("[A-Za-z]", unlist(strsplit(state_year_traff_1, ""))))
num_digits; num_letters; num_digits + num_letters

char_52_59 <- substr(state_year_traff_1, 52, 59)
char_60_68 <- substr(state_year_traff_1, 60, 68)
char_102_109 <- substr(state_year_traff_1, 102, 109)
char_110_118 <- substr(state_year_traff_1, 110, 118)
char_52_59; char_60_68; char_102_109; char_110_118

#### Meteorology, NARR ####
# NCEP North American Regional Reanalysis (NARR)

## RH
# "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/rhum.2m.2011.nc"

## T
# "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/air.2m.2012.nc"

## Wind
# "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/uwnd.10m.2015.nc"
# "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/vwnd.10m.2016.nc"

## Vegetation
# "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/veg.2019.nc"

# BLH, planetary boundary layer height
# "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/hpbl.2015.nc"

setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Meteorology_NNAR")
getwd()

meteo_var_need = c("rhum.2m", # RH
                   "air.2m", # T
                   "uwnd.10m", "vwnd.10m", #Wind
                   "veg", # vegetation
                   "hpbl") # BLH

meteo_var_need = c("vwnd.10m", #Wind
                   "veg", # vegetation
                   "hpbl") # BLH


study_year = 2011:2020
base_url_metro = "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/"

###### Meteorogoly NARR, 1. download ######

# Create an empty data.table to store all combined data
all_meteo_data <- data.table()

# download, extract, and save data 
for(meteo_var in meteo_var_need){
  for(study_year_use in study_year){
    
    meteo_var_use = meteo_var
    
    # download .nc files via the link
    meteo_data_url = 
      paste0(base_url_metro, meteo_var, ".", study_year_use, ".nc")
    metro_destfile <- paste0(meteo_var, ".", study_year_use, ".nc")
    download.file(meteo_data_url, file.path(metro_destfile))
    
    # open & explore the .nc file
    meteo_data = nc_open(paste0(meteo_var, ".", study_year_use, ".nc"))
    # print(meteo_data)               # Shows the full metadata
    variable_names <- names(meteo_data$var)  # Get the variable names
    # print(variable_names)           # Print the variable names
    
    # extract variables from the .nc file
    lat <- as.vector(ncvar_get(meteo_data, "lat"))
    lon <- as.vector(ncvar_get(meteo_data, "lon"))
    Lambert_Conformal <- ncvar_get(meteo_data, "Lambert_Conformal")
    meteo_var_data <- as.vector(ncvar_get(meteo_data, variable_names[4]))
    time_bnds_org <- as.vector(ncvar_get(meteo_data, "time_bnds"))
    time_bnds_info <- rep(time_bnds_org, each = length(lat) / length(time_bnds_org))
    
    # close & remove the .nc file
    nc_close(meteo_data)
    unlink(metro_destfile)
    
    # merge data into data.table
    annual_meteo <- 
      data.table(Latitude = lat, Longitude = lon, 
                 Meteo_value = meteo_var_data, Date = time_bnds_info)
    
    # Filter data for mainland US (approximate lat/lon bounds)
    annual_meteo_us <- 
      annual_meteo[Latitude >= 24 & Latitude <= 50 & 
                     Longitude >= -125 & Longitude <= -66]

    # Add variable name to the data
    annual_meteo_us[, `:=`(Meteo_variable = meteo_var_use)]
    
    # Combine this year's data with the overall data table
    all_meteo_data <- rbind(all_meteo_data, annual_meteo_us, use.names = TRUE, fill = TRUE)
  }
}

## need to .fst file for R, otherwise, take too much storage
write_fst(all_meteo_data, "combined_meteo_data.fst")
write_fst(all_meteo_data, "combined_meteo_data_from_vwind.fst")


#### Traffic: DANA TMAS ####

# mannual download from: https://www.fhwa.dot.gov/environment/air_quality/methodologies/dana/
# download if via link, then from: https://www.fhwa.dot.gov/environment/air_quality/methodologies/dana/dana_tmas2015_installer.zip

setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Traffic_DANA_TMAS")
getwd()

# tmas_year = fread("TMAS 2015/TMAS_Class_Clean_2015.csv") # not work, Error: vector memory limit of 16.0 Gb reached, see mem.maxVSize()
# tmas_year <- read.csv.ffdf(file="TMAS 2015/TMAS_Class_Clean_2015.csv") 
# Error in scan(file = file, what = what, sep = sep, quote = quote, dec = dec, : scan() expected 'a logical', got '3.0'
tmas_year <- read.big.matrix("TMAS 2015/TMAS_Class_Clean_2015.csv", header = FALSE, skip = 1)
dim(tmas_year)
head(tmas_year)
summary(tmas_year)

# get colnames of the tmas file
tmas_year_1k_row <- fread(file="TMAS 2015/TMAS_Class_Clean_2015.csv", nrows = 100) 
head(tmas_year_1k_row)
names(tmas_year_1k_row)
View(tmas_year_1k_row)

# select(tmas_year, DATE, LAT, LONG, VOL, F_SYSTEM, URB_RURAL, REPCTY, HPMS_TYPE10, HPMS_TYPE25, HPMS_TYPE40, HPMS_TYPE50, HPMS_TYPE60, HPMS_ALL)
# but non-numeric infor was not included in tmas_year matrix, so, only choose the numeric ones
# they are select(YEAR, MONTH, DAY, ??, LAT, LONG, HPMS_TYPE10, HPMS_TYPE25, HPMS_TYPE40, HPMS_TYPE50, HPMS_TYPE60, HPMS_ALL)
# group & round by YEAR, MONTH, DAY, LAT, LONG, current data include each hour
# head(tmas_year[, c(5, 6, 7, 8, 18, 19, 22, 23, 24, 25, 26, 27)])

tmas_year_select = tmas_year[, c(5, 6, 7, 18, 19, 22, 23, 24, 25, 26, 27)]
tmas_year_select = tmas_year_select[2:nrow(tmas_year_select), ]
tmas_year_select = write_fst(tmas_year_select, "TMAS 2015/TMAS_Class_Clean_2015_select.fst")

#### US Flights ####
# https://www.transtats.bts.gov/Data_Elements.aspx?Data=1

# https://data.bts.gov/stories/s/Port-Throughput-Metrics/8sfc-juwb/#:~:text=Port%20throughput%20can%20be%20measured,a%20port%20handles%20over%20time.
# https://www.bts.gov/browse-statistical-products-and-data/freight-facts-and-figures/number-vessel-calls-type-us-ports


########################################################################
####  Aim3 predictor transferring - to gridded data ####
########################################################################





