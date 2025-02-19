library(data.table)
library(dplyr)
library(plyr)
library(base)
library(sf)
library(ggplot2)
library(furrr)
library(USAboundaries)
library(quanteda)
library(tidyr)
library(tidycensus) # ACS census
library(terra) # .img
library(raster) # .img
library(ncdf4) # .nc
library(oro.dicom) # .vol
library(fst)
library(ff) # handle large files without hitting memory limits
library(bigmemory)  # handle large files without hitting memory limits
library(corrplot)
library(tiff)

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
         "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/HMS_SMOKE/NOAA_HMS_all_daily_match_2011-20.gpkg")

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

###### NLCD 1. exploration ######

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

###### NLCD 2. aggregate the land use to different resolutions, fact = 0, 5, 10, 30 based on southeast results######

# Land Use Type
# NCLD land use type
ncld_2016_org <- terra::rast("nlcd_2016_land_cover_l48_20210604/nlcd_2016_land_cover_l48_20210604.img")

dim(ncld_2016_org); crs(ncld_2016_org); res(ncld_2016_org)
is.factor(ncld_2016_org); is.factor(ncld_2016_org$`NLCD Land Cover Class`)
plot(ncld_2016_org)
unique(ncld_2016_org$`NLCD Land Cover Class`)


# Define the bounding box for the Southeast US (approximate coordinates)
southeast_bbox <- ext(-2000000, 500000, 0, 1500000)

# Crop the original raster to the Southeast bounding box
ncld_southeast <- crop(ncld_2016_org, southeast_bbox)
ncld_southeast_charLevel = as.factor(ncld_southeast)

# Plot the cropped region to visualize the Southeast US
plot(ncld_southeast, main = "Southeastern US Land Cover")
ncld_southeast_df = data.frame(unique(ncld_southeast$`NLCD Land Cover Class`))

freq_ncld = as.data.frame((freq(ncld_southeast_charLevel)))
freq_ncld$fraction = freq_ncld$count/(sum(freq_ncld$count))


## fact = 30
ncld_southeast_app_30 = 
  terra::aggregate(ncld_southeast, 
                   fact = 30, 
                   fun = modal, 
                   expand = TRUE, 
                   na.rm = TRUE)

ncld_southeast_charLevel_30 = as.factor(ncld_southeast_app_30)
levels(ncld_southeast_app_30); dim(ncld_southeast_app_30)
levels(ncld_southeast_charLevel_30); dim(ncld_southeast_charLevel_30)
# levels(ncld_southeast_charLevel_30) <- landcover_labels # incorrectly assign the labels

freq_ncld_30 = as.data.frame((freq(ncld_southeast_charLevel_30)))
freq_ncld_30$fraction = freq_ncld_30$count/(sum(freq_ncld_30$count))


plot(ncld_southeast_app_30, main = "Southeastern US Land Cover - factor = 30")
plot(ncld_southeast_charLevel_30, main = "Southeastern US Land Cover - factor = 30")

ncld_southeast_30_df = 
  data.frame(unique(ncld_southeast_app_30$`NLCD Land Cover Class`))
ncld_southeast_charLevel_30_df = 
  data.frame(unique(ncld_southeast_charLevel_30$`NLCD Land Cover Class`))


## fact = 10
ncld_southeast_app_10 = 
  terra::aggregate(ncld_southeast, 
                   fact = 10, 
                   fun = modal, 
                   expand = TRUE, 
                   na.rm = TRUE)

ncld_southeast_charLevel_10 = as.factor(ncld_southeast_app_10)
levels(ncld_southeast)
levels(ncld_southeast_app_10); dim(ncld_southeast_app_10)
levels(ncld_southeast_charLevel_10); dim(ncld_southeast_charLevel_10)
# levels(ncld_southeast_charLevel_10) <- landcover_labels

freq_ncld_10 = as.data.frame((freq(ncld_southeast_charLevel_10)))
freq_ncld_10$fraction = freq_ncld_10$count/(sum(freq_ncld_10$count))


plot(ncld_southeast_app_10, main = "Southeastern US Land Cover - factor = 10")
plot(ncld_southeast_charLevel_10, main = "Southeastern US Land Cover - factor = 10")

ncld_southeast_10_df = 
  data.frame(unique(ncld_southeast_app_10$`NLCD Land Cover Class`))
ncld_southeast_charLevel_10_df = 
  data.frame(unique(ncld_southeast_charLevel_10$`NLCD Land Cover Class`))


## fact = 5
ncld_southeast_app_5 = 
  terra::aggregate(ncld_southeast, 
                   fact = 5, 
                   fun = modal, 
                   expand = TRUE, 
                   na.rm = TRUE)

ncld_southeast_charLevel_5 = as.factor(ncld_southeast_app_5)
levels(ncld_southeast_app_5); dim(ncld_southeast_app_5)
levels(ncld_southeast_charLevel_5); dim(ncld_southeast_charLevel_5)
# levels(ncld_southeast_charLevel_5) <- landcover_labels

freq_ncld_5 = as.data.frame((freq(ncld_southeast_charLevel_5)))
freq_ncld_5$fraction = freq_ncld_5$count/(sum(freq_ncld_5$count))

plot(ncld_southeast_app_5, main = "Southeastern US Land Cover - factor = 5")
plot(ncld_southeast_charLevel_5, main = "Southeastern US Land Cover - factor = 5")

ncld_southeast_5_df = 
  data.frame(unique(ncld_southeast_app_5$`NLCD Land Cover Class`))
ncld_southeast_charLevel_5_df = 
  data.frame(unique(ncld_southeast_charLevel_5$`NLCD Land Cover Class`))

freq_ncld$fact = "fact_0"
freq_ncld_5$fact = "fact_5"
freq_ncld_10$fact = "fact_10"
freq_ncld_30$fact = "fact_30"

freq_ncld_comp = rbind(freq_ncld, freq_ncld_5, freq_ncld_10, freq_ncld_30)
sapply(freq_ncld_comp, class)
names(freq_ncld_comp)[2] = "Code"
freq_ncld_comp_landtype = join(freq_ncld_comp, landcover_df)

ggplot(freq_ncld_comp_landtype, 
       aes(x = reorder(Description, fraction),
           y = fraction)) +
  geom_bar(stat = "identity",
           fill = "steelblue",
           alpha = 0.6,
           width = 0.5) +
  facet_grid(fact ~ .) +
  coord_flip() +  # Flip coordinates to make a horizontal bar plot
  labs(title = "Fraction of different land use type when changing the fact value",
       x = "Land Use Type",
       y = "Fraction (%)") +
  theme_minimal(base_size = 16)

# expand the df
freq_ncld_comp_landtype_wide =
  dplyr::select(freq_ncld_comp_landtype, -count, -layer, -Description) %>%
  pivot_wider(
    names_from = fact,
    values_from = fraction
  )

# Calculate & plot the correlation matrix for the fraction columns
freq_ncld_corr_matrix <- 
  cor(freq_ncld_comp_landtype_wide[, c("fact_0", "fact_5", "fact_10", "fact_30")], 
      use = "complete.obs")

# Plot the correlation matrix using corrplot
corrplot(freq_ncld_corr_matrix,
         method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 45)


###### NLCD 3. aggregate the land use to fact = 30 based on southeast results######

for (Year in c(2011, 2013, 2016, 2019)) {
  
  # Year = 2011
  
  # Load the SpatRaster for the respective year
  ncld_year_org <- terra::rast(paste0("nlcd_", Year, "_land_cover_l48_20210604", 
                                      "/nlcd_", Year, "_land_cover_l48_20210604.img"))
  
  # # Covert the SpatRaster (class(ncld_2016_org$NLCD Land Cover Class)) to factor for terra::resample
  # ncld_year_org <- as.factor(ncld_year_org)
  
  # Check if the covertion is correct
  # levels(ncld_year_org) 
  # dim(ncld_year_org); plot(ncld_year_org)
  
  # Resample using the modal method for categorical (factor) data
  # modal function helps determine which class appears most often when aggregating grid cells
  ## ncld_year <- terra::resample(ncld_year_org, r_template, method = "modal"), modal no in resample
  ncld_year <-
    terra::aggregate(ncld_year_org, 
                     fact = 30, 
                     fun = modal, 
                     expand = TRUE, 
                     na.rm = TRUE)
  
  # Attach the land cover labels to the aggregated raster
  # levels(ncld_year) <- landcover_labels # cannot correctly assign
  
  ncld_year_factor = as.factor(ncld_year)
  
  # Check the re-sampled raster
  head(ncld_year_factor)
  unique(ncld_year_factor$`NLCD Land Cover Class`)
  plot(ncld_year_factor)
  
  # Save the aggregated raster
  terra::writeRaster(ncld_year_factor, 
                     # paste0("nlcd_", Year, "_fact30_landcover_resampled.tif"), 
                     paste0("nlcd_", Year, "_fact300_landcover_resampled.tif"), 
                     overwrite = TRUE)
}


###### NLCD 3. aggregate the land use to fact = 30 based on southeast results######

for (Year in c(2011, 2013, 2016, 2019)) {
  
  # Year = 2011
  
  # Load the SpatRaster for the respective year
  ncld_year_org <- terra::rast(paste0("nlcd_", Year, "_fact30_landcover_resampled.tif"))
  
  # # Covert the SpatRaster (class(ncld_2016_org$NLCD Land Cover Class)) to factor for terra::resample
  # ncld_year_org <- as.factor(ncld_year_org)
  
  # Check if the covertion is correct
  # levels(ncld_year_org) 
  # dim(ncld_year_org); plot(ncld_year_org)
  
  # Resample using the modal method for categorical (factor) data
  # modal function helps determine which class appears most often when aggregating grid cells
  ## ncld_year <- terra::resample(ncld_year_org, r_template, method = "modal"), modal no in resample
  ncld_year <-
    terra::aggregate(ncld_year_org, 
                     fact = 10, 
                     fun = modal, 
                     expand = TRUE, 
                     na.rm = TRUE)
  
  # Attach the land cover labels to the aggregated raster
  # levels(ncld_year) <- landcover_labels # cannot correctly assign
  
  ncld_year_factor = as.factor(ncld_year)
  
  # Check the re-sampled raster
  head(ncld_year_factor)
  unique(ncld_year_factor$`NLCD Land Cover Class`)
  plot(ncld_year_factor)
  
  # Save the aggregated raster
  terra::writeRaster(ncld_year_factor, 
                     paste0("nlcd_", Year, "_fact300_landcover_resampled.tif"), 
                     overwrite = TRUE)
}

###### NLCD 4. transfer 4-year NLCD to annual level ######

ncld_2011 <- terra::rast("nlcd_2011_fact30_landcover_resampled.tif")
ncld_2013 <- terra::rast("nlcd_2013_fact30_landcover_resampled.tif")
ncld_2016 <- terra::rast("nlcd_2016_fact30_landcover_resampled.tif")
ncld_2019 <- terra::rast("nlcd_2019_fact30_landcover_resampled.tif")

# Create a list of the rasters with corresponding years
ncld_rasters <- list(ncld_2011, ncld_2013, ncld_2016, ncld_2019)
ncld_years <- list(c(2011, 2012), c(2013, 2014), c(2015, 2016, 2017), c(2018, 2019, 2020))
# ncld_rasters <- list(ncld_2013, ncld_2019)
# ncld_years <- list(c(2011, 2012, 2013, 2014, 2015), c(2016, 2017, 2018, 2019, 2020))

# Initialize an empty list to store merged results
merged_ncld_rasters <- list()

# Loop through each ncld_raster and its associated years
for (i in seq_along(ncld_rasters)) {
  # Replicate the ncld_raster for each year in the list
  for (ncld_year in ncld_years[[i]]) {
    # Add year as a new layer or attribute (if needed, based on structure)
    current_ncld_raster <- ncld_rasters[[i]]
    current_ncld_raster$ncld_year <- ncld_year # Add year attribute
    merged_ncld_rasters <- append(merged_ncld_rasters, list(current_ncld_raster)) # Store in list
  }
}

# Merge all ncld_rasters into one multilayer ncld_raster 
final_ncld_raster <- terra::merge(merged_ncld_rasters[[1]], merged_ncld_rasters[-1])

ncld_raster_stack <- terra::rast(merged_ncld_rasters)
dim(ncld_raster_stack)

## Define the land cover classes based on NLCD classification
# if running unique(ncld_2016_org$`NLCD Land Cover Class`) after the as.factor(ncld_2016_org), then got the numbers on the left side
# The labels for the NLCD dataset are predefined
landcover_labels <- c(
  "0" = "Unclassified",
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
  Code = names(landcover_labels),
  Description = landcover_labels,
  stringsAsFactors = FALSE # Keep the values as characters
)

# merge land use code with land use type
final_ncld_raster_type = 
  final_ncld_raster

# Save the merged raster as a GeoTIFF file
output_ncld_file <- "NLCD_raster_30fact_2011-20.tif"
writeRaster(final_ncld_raster_type, filename = output_ncld_file, format = "GTiff", overwrite = TRUE)


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

setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Meteorology_NARR")
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


setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Traffic_volume/")
getwd()

#### U.S. Traffic Volume Data, Highway ####

###### Traffic Volume 1, download ###### 
# "https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/2011/2011_station_data.zip"
# "https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/2011/november_2011_ccs_data.zip"
# "https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/2012/feb_2012_tmas.zip"
# "https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/2013/mar_2013_ccs_data.zip"
# "https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/2014/feb_2014_ccs_data.zip"
# "https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/2015/jan_2015_ccs_data.zip"
# "https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/2016/aug_2016_ccs_data.zip"
# "https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/2017/sep_2017_ccs_data.zip"
# "https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/2018/mar_2018_ccs_data.zip"
# "https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/2019/feb_2019_ccs_data.zip"
# "https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/2020/may_2020_ccs_data.zip"

study_year = 2011
study_month = c("january", "february", "march", "april", "may", "june",
                "july", "august", "september", "october", "november", "december")

# 2012 download separately due to different link components

study_year = 2013:2020 # 2013:2020
study_month = c("jan", "feb", "mar", "apr", "may", "jun",
                "jul", "aug", "sep", "oct", "nov", "dec")

base_dir <- getwd()

# download data to local folder
for(study_year_use in study_year) { 
  for(study_month_use in study_month) {
    # create url
    url = paste0("https://www.fhwa.dot.gov/policyinformation/tables/tmasdata/",
                 study_year_use, "/", study_month_use, "_", study_year_use, "_ccs_data.zip")
    
    # Construct full destination path for the downloaded zip file
    destfile <- file.path(base_dir, study_year_use, paste0(study_year_use, "_", study_month_use, ".zip"))
    
    # download data, and use tryCatch in case of missing link
    tryCatch({
      
      # Download the file
      download.file(url, destfile)
      
      # Extract the file contents into the year folder (study_year_use) and ignore directory structure
      unzip(destfile, exdir = file.path(base_dir, study_year_use), junkpaths = TRUE)
      
      # Remove the zip file after extraction
      unlink(destfile)
      
    }, error = function(e) {
      
      print(paste("Error at iteration", url))
    })
    
  }
}


###### Traffic Volume 2, extract volume info ###### 
setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Traffic_volume/")
getwd()
base_dir = getwd()

# set traffic volume file names
traffic_volume_colname <-
  c("Record_Type", "State_FIPS", "Function_Class", "Station_ID", 
    "Travel_Direction", "Travel_Lane", "Year", "Month", "Day", 
    "Day_of_Week", paste0("Traffic_Volume_Hour_", 1:24), "Restrictions")

# study years
study_years = 2011:2020 #  2011:2020

# define the fixed-width format for included variables
# https://github.com/policyinfo/TMAS-Traffic-Volume-Data-Rearrangement-Tool/blob/main/Tool_Document.pdf
column_widths <- c(1, 2, 2, 6, 1, 1, 2, 2, 2, 1, rep(5, 24), 1) 


#### using tryCatch for each year is not enough, use it for each file.

# # process and save each year's data
# for (study_year_use in study_years) {
#   
#   vol_list = 
#     list.files(file.path(base_dir, study_year_use), 
#                pattern = ".*\\.VOL$", full.names = TRUE)
#   
#   # Initialize an empty list to store the processed data.tables
#   annual_traffic_volume_list <- list()
#   
#   # use tryCatch to process data, records of some state are sometimes zero byte
#   tryCatch({
#     
#     # Process each file in vol_list using traffic_volume_process
#     for (vol_file in vol_list) {
#       # Process the file with function traffic_volume_process, hourly to daily
#       traffic_vol_dt <- 
#         traffic_volume_process(vol_file, traffic_volume_colname, column_widths)  
#       # Add the result to the list
#       annual_traffic_volume_list[[length(annual_traffic_volume_list) + 1]] <- traffic_vol_dt    
#     }
#   }, error = function(e) {
#     
#     print(paste("Error at iteration", vol_file))
#   })
#   
#   # Combine all data.tables from the current year
#   annual_traffic_volume <- rbindlist(annual_traffic_volume_list)
#   
#   # Output to .fst file for the current year
#   fst::write_fst(annual_traffic_volume, 
#                  file.path(base_dir, 
#                            paste0("Traffic_Volume_", study_year_use, ".fst")))
# }

# Process and save each year's data
for (study_year_use in study_years) {
  
  vol_list <- list.files(file.path(base_dir, study_year_use), 
                         pattern = ".*\\.VOL$", full.names = TRUE)
  
  # Initialize an empty list to store the processed data.tables
  annual_traffic_volume_list <- list()
  
  for (vol_file in vol_list) {
    # Check if the file size is greater than zero
    if (file.info(vol_file)$size > 0) {
      
      tryCatch({
        # Process the file with function traffic_volume_process
        traffic_vol_dt <- 
          traffic_volume_process(vol_file, traffic_volume_colname, column_widths)
        
        # Add the result to the list only if it has data
        if (nrow(traffic_vol_dt) > 0) {
          annual_traffic_volume_list[[length(annual_traffic_volume_list) + 1]] <- traffic_vol_dt
        } else {
          print(paste("No rows processed for file:", vol_file))
        }
      }, error = function(e) {
        # Report any error that occurs during processing
        print(paste("Error processing file:", vol_file, ":", e$message))
      })
    } else {
      print(paste("Skipped zero-byte file:", vol_file))
    }
  }
  
  # Combine all data.tables from the current year, if any valid data is available
  if (length(annual_traffic_volume_list) > 0) {
    annual_traffic_volume <- rbindlist(annual_traffic_volume_list)
    
    # Output to .fst file for the current year
    fst::write_fst(annual_traffic_volume, 
                   file.path(base_dir, paste0("Traffic_Volume_", study_year_use, ".fst")))
  } else {
    print(paste("No valid data for year:", study_year_use))
  }
}


###### Traffic Volume 3, station info ###### 
getwd()

# List all txt files
traffic_vol_station_files <-
  list.files(
    path = "Traffic_volume_station", pattern = "\\.txt$", full.names = TRUE)

# Initialize an empty list to store the processed data frames
traffic_vol_station_list <- list()

# Loop through each txt file
for (traffic_vol_station_path in traffic_vol_station_files) {
  
  # Read the file using read.fwf with column separator "|"
  traffic_vol_station_full <- 
    fread(traffic_vol_station_path, 
          sep = "|", # columns are separated by "|"
          fill = TRUE, header = TRUE)  
  # View(traffic_vol_station_full)
  
  # Select columns to match stations
  traffic_vol_station_match <- 
    traffic_vol_station_full[, .(State_Code, Station_Id, Travel_Dir,  
                                 Travel_Lane, Year_Record, 
                                 Latitude, Longitude, Station_Location)]
  
  # Store the processed data frame in the list
  traffic_vol_station_list[[length(traffic_vol_station_list) + 1]] <- 
    traffic_vol_station_match
}

# Merge all data frames into one
traffic_vol_station_all <- do.call(rbind, traffic_vol_station_list)
# View(traffic_vol_station_all)
write_fst(traffic_vol_station_all, "Traffic_volume_station_info_2011-20.fst")

traffic_vol_station_all = read_fst("Traffic_volume_station_info_2011-20.fst")
names(traffic_vol_station_all)[2] = "Station_ID"
setDT(traffic_vol_station_all)

# Howeve, in the following check, Station_ID-Latitude-Longitude-Station_Location infor are always consistent 
# while the State_Code may change for the same location
subset(traffic_vol_station_all, Station_Location == "US6 & 34 WEST OF ARAPAHOE")
subset(traffic_vol_station_all, Station_Location == "I80 0.4 MILES EAST OF THE OVERTON INTG")
subset(traffic_vol_station_all, Station_Location == "I 8")

# File with all IDs
traffic_vol_station_ids = 
  traffic_vol_station_all[, .(State_Code, Station_ID, Station_Location)]
traffic_vol_station_ids = 
  traffic_vol_station_ids[!duplicated(traffic_vol_station_ids), ]
dim(traffic_vol_station_ids) # 334854      5


# File with stations with long and lat info
traffic_vol_station_gps =
  traffic_vol_station_all[, .(State_Code, Station_ID, Station_Location, Latitude, Longitude)]
dim(traffic_vol_station_gps)
traffic_vol_station_gps = 
  subset(traffic_vol_station_gps, 
         !(is.na(Latitude) | is.na(Latitude) | Latitude < 100))
traffic_vol_station_gps = 
  traffic_vol_station_gps[!duplicated(traffic_vol_station_gps), ]
# View(traffic_vol_station_gps)
write_fst(traffic_vol_station_gps, "Traffic_volume_station_GPS.fst")
dim(traffic_vol_station_gps) # 20957     5


traffic_vol_station_ids_NOgps =
  subset(traffic_vol_station_ids, 
         !(Station_ID %in% traffic_vol_station_gps$Station_ID))
traffic_vol_station_ids_NOgps =
  traffic_vol_station_ids_NOgps[!duplicated(traffic_vol_station_ids_NOgps), ]
write_fst(traffic_vol_station_ids_NOgps, "Traffic_volume_station_Without_GPS.fst")
dim(traffic_vol_station_ids_NOgps) # 3437    3

###### Traffic Volume 4, combine volume and station GPS ###### 

traf_vol_list =   
  list.files(
    pattern = "Traffic_Volume_\\d{4}\\.fst$", full.names = TRUE)
print(traf_vol_list)

## Via this check, we can see that the Station_ID could be repeated among States, also for the Travel_Direction & Travel_Lane
# BUT, even though State_FIPS is repeated among States, the address and long lat are the same for the same Station_ID
traf_vol_y1 = read_fst("Traffic_Volume_2014.fst")
traf_vol_y2 = read_fst("Traffic_Volume_2015.fst")
min(traf_vol_y1$Date); max(traf_vol_y1$Date)
min(traf_vol_y2$Date); max(traf_vol_y2$Date)

traf_vol_y1_station = 
  subset(traf_vol_y1, 
         Date == as.Date("2014-04-01") & Station_ID == "000101")

traf_vol_all = 
  rbindlist(
    lapply(
      traf_vol_list, read_fst, as.data.table = TRUE))
dim(traf_vol_all)

# Even though State_FIPS is repeated among States, the address and long lat are the same for the same Station_ID
traf_vol_use =
  traf_vol_all[, .(Date, Station_ID, Daily_Traffic_Volume)]
dim(traf_vol_use); class(traf_vol_use)
# length(unique(traf_vol_use$Station_ID))
# length(unique(traf_vol_use$Date))
sapply(traf_vol_use, class)


# for those of different lanes or directions, add up the results
# btw, group_by is more efficient than ddply since dplyr package is designed to handle large dataset
traf_vol_addup <- 
  traf_vol_use[, 
               .(Daily_Traffic_Volume = sum(Daily_Traffic_Volume)), 
               by = .(Date, Station_ID)]
head(traf_vol_addup); dim(traf_vol_addup); class(traf_vol_addup)
# nrow of traf_vol_addup is 24.4% of the traf_vol_use
dim(traf_vol_addup[!duplicated(traf_vol_addup), ])

# subset(traf_vol_use, Date == as.Date("2011-04-01") & Station_ID == "000101")
# subset(traf_vol_addup, Date == as.Date("2011-04-01") & Station_ID == "000101")

######## traffic station info
traffic_vol_station_gps = read_fst("Traffic_volume_station_GPS.fst")

# in case of wrong info in Station_Location, only use GPS 
traffic_vol_station_gps = dplyr::select(traffic_vol_station_gps, -Station_Location, -State_Code) 
traffic_vol_station_gps = traffic_vol_station_gps[!duplicated(traffic_vol_station_gps),]
setDT(traffic_vol_station_gps)

# remove duplication
traffic_vol_station_gps_nodup =
  traffic_vol_station_gps[, 
                          .(Latitude = mean(Latitude), 
                            Longitude = mean(Longitude)),
                          by = .(Station_ID)]

head(traffic_vol_station_gps_nodup); dim(traffic_vol_station_gps_nodup)
dim(traffic_vol_station_gps_nodup[!duplicated(traffic_vol_station_gps_nodup), ])

summary(unique(traffic_vol_station_gps$Station_ID) %in% unique(traf_vol_addup$Station_ID)) # FALSE 1909  TRUE 6898 
summary(unique(traf_vol_addup$Station_ID) %in% unique(traffic_vol_station_gps$Station_ID)) # FALSE 202  TRUE 6898 

# merge the data.table
traf_vol_station =
  merge(traf_vol_addup,
        traffic_vol_station_gps_nodup, 
        by = "Station_ID", all.x = TRUE)

# get traffic volume and station info and those without station GPS info
traf_vol_station_noGPS = 
  subset(traf_vol_station, is.na(Latitude) | is.na(Longitude)) # 82069 5
traf_vol_station_GPS = 
  subset(traf_vol_station, !(is.na(Latitude) | is.na(Longitude))) # 13746889 5

# get the normal Long and Lat 
traf_vol_station_GPS$Latitude = traf_vol_station_GPS$Latitude / 1E+6
traf_vol_station_GPS$Longitude = -traf_vol_station_GPS$Longitude / 1E+6
summary(traf_vol_station_GPS)

# grab those inside the mainland US
# bounding box for the mainland US
us_bbox <- list(
  xmin = -125,  # Westernmost longitude
  xmax = -66,   # Easternmost longitude
  ymin = 24,    # Southernmost latitude
  ymax = 50     # Northernmost latitude
)
# filter the points that fall within the bounding box
traf_vol_station_GPS_US <- 
  traf_vol_station_GPS %>%
  dplyr::filter(Longitude >= us_bbox$xmin & Longitude <= us_bbox$xmax &
                  Latitude >= us_bbox$ymin & Latitude <= us_bbox$ymax)
dim(traf_vol_station_GPS_US) # 11528055  5
write_fst(traf_vol_station_GPS_US, "FHWA_Traffic_Volume_2011-20.fst")

# get stations with volume record and with GPS
traf_vol_stations_withGPS =
  dplyr::select(traf_vol_station_GPS_US, Station_ID, Latitude, Longitude)
traf_vol_stations_withGPS =
  traf_vol_stations_withGPS[!duplicated(traf_vol_stations_withGPS), ]
nrow(traf_vol_stations_withGPS) # 6894, after remvoing those outside of mainland US, 6084

# get the median of volume across the study period
traf_vol_station_GPS_US_med <-
  traf_vol_station_GPS_US[, .(Daily_Traffic_Volume = median(Daily_Traffic_Volume)),
                          by = .(Station_ID, Latitude, Longitude)]

# transfer to sf
traf_vol_station_GPS_US_med_sf <- 
  st_as_sf(traf_vol_station_GPS_US_med, 
           coords = c("Longitude", "Latitude"), 
           crs = 4326) # EPSG:4326 (WGS 84) is for longitude/latitude
# plot(traf_vol_station_GPS_US_med_sf$Daily_Traffic_Volume)

ggplot(data = traf_vol_station_GPS_US_med_sf) +
  geom_sf(aes(color = Daily_Traffic_Volume)) +
  scale_color_viridis_c() +
  theme_minimal(base_size = 16) +
  labs(title = "Median Traffic Volume by Station across 2011-2020", 
       color = "Daily Traffic Volume")

#### US Flights ####
# https://www.transtats.bts.gov/Data_Elements.aspx?Data=1

# https://data.bts.gov/stories/s/Port-Throughput-Metrics/8sfc-juwb/#:~:text=Port%20throughput%20can%20be%20measured,a%20port%20handles%20over%20time.
# https://www.bts.gov/browse-statistical-products-and-data/freight-facts-and-figures/number-vessel-calls-type-us-ports


#### Biodiesel Plant US 20190101 ####
setwd("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nation_SA_data/Rasel_US_oil_gas_industry")
getwd()

shapefile_data <- st_read("Biodiesel_Plants_US_20190101/Biodiesel_Plants_US_20190101.shp")
plot(shapefile_data)
# PADD, Petroleum Administration for Defense District 
# Cap_Mmgal, Annual production capacity in millions of gallons, EIA-22M (Monthly Biodiesel Production Survey)


#### Totography ####
# https://nrcs.app.box.com/v/gateway/folder/39640733878
# https://gdg.sc.egov.usda.gov/Catalog/ProductDescription/NED.html
library(foreign)

topo_ned60m = st_read("/Users/TingZhang/Downloads/NED60M_SpatialMetadata/NED60M_fe2898_October2018.shp")
head(topo_ned60m)
attributes_data <- read.dbf("/Users/TingZhang/Downloads/NED60M_SpatialMetadata/NED60M_fe2898_October2018.dbf")
head(attributes_data)

# topo_ned60m = st_read("NED60M_SpatialMetadata/NED60M_fe2898_October2018.shp")
plot(topo_ned60m)
# ullat and ullon, "Upper Left Latitude" and "Upper Left Longitude," defining the geographic coordinates of the upper-left corner of the raster or grid.
# lrlat and lrlon: these might represent the "Lower Right Latitude" and "Lower Right Longitude," which define the geographic coordinates of the lower-right corner of the raster or grid.
# xshift and yshift, adjustments made to the x (longitude) and y (latitude) coordinates
# horizres_m, Horizontal resolution in meters
# s_date, start date of the data collection
# i_date: The issue or creation date of the dataset
# hdatum, Horizontal datum, which is the reference system
# utmzone, UTM zone
# vdatum, Vertical datum, which refers to the reference system for elevations 
# UTM, Vertical datum, which refers to the reference system for elevations 
# zstep, increment or step size in the z-dimension (elevation). It could define how finely the elevation data is categorized.
# zshift, vertical shift 
# zsigma, standard deviation 
# absx and absy, absolute horizontal accuracy
# absz, absolute vertical accuracy
# abspts, number of points used to calculate the absolute accuracy.
# quaddate, date associated with a specific quadrangle (quad) or tile in the dataset
# meta_p_are, metadata polygon area?
# meta_p_per, metadata polygon perimeter
# pts_id, ID for a specific point?

# grab those inside the mainland US
# bounding box for the mainland US
us_bbox <- list(
  xmin = -125,  # Westernmost longitude
  xmax = -66,   # Easternmost longitude
  ymin = 24,    # Southernmost latitude
  ymax = 50     # Northernmost latitude
)

# filter the points that fall within the bounding box
topo_ned60m_US <- 
  topo_ned60m %>%
  dplyr::filter(ullon >= us_bbox$xmin & ullon <= us_bbox$xmax &
                  ullat >= us_bbox$ymin & ullat <= us_bbox$ymax)


topo_ned60m_use =
  dplyr::select(topo_ned60m, zmean, geometry)


#### Meteorology, GRIDMET ####
# GRIDMET: University of Idaho Gridded Surface Meteorological Dataset
# file downloaded with Python

###### GRIDMET 1, combine daily to annual by variable by year ###### 

setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Meteorology_GRIDMET")
# setwd("/groups/HAQ_LAB/tzhang/gridmet_daily")
getwd()
tif_directory = getwd()

# rm(list = ls()) # Removes all objects in the environment
# gc() # Triggers garbage collection

# Define the date range for each year
study_years <- 2011:2020 # 2011:2020   2011:2015

# Define the list of variables you want to download
meteo_variables <- 
  c("tmmx", "tmmn", "rmax", "rmin", # max & min T, RH,
    "vs", "th", # vs, wind velocity at 10m; th, wind direction
    "fm100", "fm1000", "bi", # 100-hour  and 1000-hour dead fuel moisture, bi, burning index
    "pr", "vpd", "etr") # pr, Precipitation amount; vpd, Mean vapor pressure deficit; etr, Daily alfalfa reference evapotranspiration

# meteo_variables = "bi"

# us_grid_raster_01 = raster(file.path("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/base_raster_grid_sf/us_grid_raster_01.tif"))
# us_grid_raster_001 = raster(file.path("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/base_raster_grid_sf/us_grid_raster_001.tif"))

us_grid_raster_01 = raster(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/CMAQ_previous_extract_tries/us_grid_raster_01.tif"))

us_grid_raster = us_grid_raster_01

# b = raster("vs_2011_01_01.tif")
# a = raster("vs_2011_merged.tif")
# plot(a)

# Initialize an empty vector to store the names of files that return NULL
# some downloaded .tif cannot be read directly via raster() or terra::rast()
failed_files <- c()

# Merge and output file by meteorological variable and by year
for (meteo_var in meteo_variables) {
  for (meteo_year in study_years) {
    
    # List all .tif files for the specific variable and year
    tif_files <- 
      list.files(
        tif_directory, 
        pattern = paste0(meteo_var, "_", meteo_year, ".*\\.tif$"), 
        full.names = TRUE)
    
    if (length(tif_files) > 0) {
      # Initialize an empty list to store successfully loaded rasters
      rasters <- list()
      
      for (tif_file in tif_files) {
        # Extract the name and use it as rast name
        rast.name = sub("\\.tif$", "", basename(tif_file))
        
        # Try reading each .tif file and skip the ones that cannot be read
        raster_data <- tryCatch({
          rast(tif_file)  # Use rast() from terra to read the raster file
        }, error = function(e) {
          message(paste("Error reading file:", tif_file, "- skipping"))
          failed_files <- c(failed_files, tif_file)
          return(NULL)  # Return NULL if there is an error
        })
        
        # Only add successfully loaded rasters to the list
        if (!is.null(raster_data)) {
          rasters <- c(rasters, list(raster_data))
        }
      }
      
      # Only proceed if there are valid rasters to stack
      if (length(rasters) > 0) {
        # Stack the valid rasters into a multi-layer raster (stacked temporally)
        meteo_merged_raster <- rast(rasters)
        
        # Define the output filename
        meter_merged_filename <- 
          # file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/aa_GRIDMET_stacked", 
          file.path("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/GRIDMET_stacked", 
                    paste0(meteo_var, "_", meteo_year, "_stacked.tif")) 
        
        # Save the stacked raster to a new .tif file
        writeRaster(
          meteo_merged_raster,
          filename = meter_merged_filename, 
          overwrite = TRUE)
        
        print(paste0("File saved: ", meter_merged_filename))
      } else {
        print(paste0("No valid rasters to stack for ", meteo_var, " in ", meteo_year))
      }
    }
  }
}

###### GRIDMET 2.1, combine shared variables used for all source prediction ###### 
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

# The raster for projection
us_grid_raster_01 = raster(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/CMAQ_previous_extract_tries/us_grid_raster_01.tif"))
us_grid_raster = us_grid_raster_01
# us_grid_raster = rast(us_grid_raster_01)

# Set the paths for reading and saving files
gridmet_stack_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/aa_GRIDMET_stacked"
gridmet_stack_source_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/aa_GRIDMET_source_stacked"

# crs_meteo = st_crs(tmmx_year)
# names(tmmx_year) 

# Define the date range for each year
study_years <- 2016 # 2011:2020   2011:2015

# Use rast to read, not raster, so as to directly get date info 
# Annual data, some variables may miss some days or grids, need to check every time!!!
# need recheck!!! and add the lost days inside!!!
tmmx_year = rast(file.path(gridmet_stack_path, paste0("tmmx_", study_years, "_stacked.tif")))
tmmx_year # 2014, tmmx 364; 2015, tmmx 364

tmmn_year = rast(file.path(gridmet_stack_path, paste0("tmmn_", study_years, "_stacked.tif")))
tmmn_year

rmax_year = rast(file.path(gridmet_stack_path, paste0("rmax_", study_years, "_stacked.tif")))
rmax_year # 2013, rmax 364

rmin_year = rast(file.path(gridmet_stack_path, paste0("rmin_", study_years, "_stacked.tif")))
rmin_year # 2012, rmin 365; 

vs_year = rast(file.path(gridmet_stack_path, paste0("vs_", study_years, "_stacked.tif")))
vs_year # 2012, vs 365; 

th_year = rast(file.path(gridmet_stack_path, paste0("th_", study_years, "_stacked.tif")))
th_year

# Meteorological variables
meteo_var_comm <- 
  c("tmmx", "tmmn", "rmax", "rmin", # max & min T, RH,
    "vs", "th") # vs, wind velocity at 10m; th, wind direction; pr, Precipitation amoun

# meteo_var_comm = "th"

# Initialize a list to store data.tables for each variable
meteo_var_list <- list()

# study_years = 2016

# Loop through each year and combine the selected variables
for (met_year in study_years) { # met_year = 2017
  print(paste("GRIDMET year to be used:", met_year))
  
  # Initialize an empty list to store rasters for this year
  rasters_to_merge <- list()
  
  # Loop through each selected variable
  for (meteo_var in meteo_var_comm) { # meteo_var = meteo_var_comm[1]
    print(paste("GRIDMET Variable to be used:", meteo_var))
    
    # Construct the file path for the stacked raster of this variable and year
    file_path <- 
      file.path(gridmet_stack_path, 
                paste0(meteo_var, "_", met_year, "_stacked.tif"))
    
    # Process the variable to create the raster brick
    met_var_project <- 
      brick_var_date_project(
        file_path, 
        us_grid_raster)
    
    # Extract spatial information and values
    meteo_var_dt = 
      as.data.table(as.data.frame(met_var_project, xy = TRUE, na.rm = TRUE))
    # head(meteo_var_dt); dim(meteo_var_dt)
    
    # Reshape data from wide to long format (each variable in a single column)
    meteo_var_long <- 
      melt(meteo_var_dt, 
           id.vars = c("x", "y"), 
           variable.name = "Layer", 
           value.name = meteo_var,
           na.rm = TRUE)
    
    # Add Date 
    meteo_var_long[, Date := 
                     gsub(".*_(\\d{4}).(\\d{2}).(\\d{2})", 
                                  "\\1-\\2-\\3", 
                                  Layer)]
    meteo_var_long[, Layer := NULL]

    print("Check the file for the applied meteo_var")
    head(meteo_var_long); summary(meteo_var_long)
    
    # Store in list
    meteo_var_list[[meteo_var]] <- meteo_var_long
  }
  
  # # Merge all variables for this year
  # meteo_merged_year <- 
  #   Reduce(function(x, y) {
  #   merge(x, y, by = c("x", "y", "Date"), all = TRUE)
  # }, meteo_var_list)
  # # meteo_merged_year$Date = as.Date(meteo_merged_year$Date)
  # 
  # # meteo_merged_year_wide =
  # #   meteo_merged_year %>%
  #   
  # print("Check the file for the all variable in this year")
  # head(meteo_merged_year); dim(meteo_merged_year)
  # 
  # # Save the merged data for this year
  # write_fst(meteo_merged_year, 
  #           file.path(gridmet_stack_source_path, 
  #                     paste0("Common_GRIDMET_", met_year, "_stacked.fst")))

  
  # Save the single meteo data one by one for this year
  for (meteo_var in meteo_var_comm) { # meteo_var = meteo_var_comm[2]
    
    meteo_var_year = meteo_var_list[[meteo_var]]
    meteo_var_year = 
      meteo_var_year[with(meteo_var_year, order(Date, x, y)), ]
    names(meteo_var_year)[1:2]
    names(meteo_var_year)[1:2] = c("Longitude", "Latitude")
    dim(meteo_var_year)
    
    print(paste("Check the output file for a single meteo_var", 
                head(meteo_var_year)
    ))
    
    write_fst(meteo_var_year, 
              file.path(gridmet_stack_source_path, 
                        paste0("Common_GRIDMET_", meteo_var, "_", met_year, "_stacked.fst")))
  }
  
  # Clear memory
  rm(meteo_merged_year, meteo_var_list)
  gc()
}

###### GRIDMET 2.2, separately read and then combine shared variables, and interpolated if necessary ######  

# rm(list = ls())

# Set the paths for reading and saving files
gridmet_stack_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/aa_GRIDMET_stacked"
gridmet_stack_source_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/aa_GRIDMET_source_stacked"

met_year = 2016 # 2015

# Year 2015, lack one day

tmmx_year = 
  read_fst(file.path(gridmet_stack_source_path, 
                     paste0("Common_GRIDMET_", "tmmx", "_", met_year, "_stacked.fst")))
tmmn_year = 
  read_fst(file.path(gridmet_stack_source_path, 
                     paste0("Common_GRIDMET_", "tmmn", "_", met_year, "_stacked.fst")))
rmax_year = 
  read_fst(file.path(gridmet_stack_source_path, 
                     paste0("Common_GRIDMET_", "rmax", "_", met_year, "_stacked.fst")))
rmin_year =
  read_fst(file.path(gridmet_stack_source_path, 
                     paste0("Common_GRIDMET_", "rmin", "_", met_year, "_stacked.fst")))
vs_year = 
  read_fst(file.path(gridmet_stack_source_path, 
                     paste0("Common_GRIDMET_", "vs", "_", met_year, "_stacked.fst")))
th_year = 
  read_fst(file.path(gridmet_stack_source_path, 
                     paste0("Common_GRIDMET_", "th", "_", met_year, "_stacked.fst")))

summary(tmmx_year$Longitude == tmmn_year$Longitude & 
          tmmx_year$Latitude == tmmn_year$Latitude & 
          tmmx_year$Date == tmmn_year$Date)
# summary(tmmx_year$Longitude == rmax_year$Longitude & 
#           tmmx_year$Latitude == rmax_year$Latitude & 
#           tmmx_year$Date == rmax_year$Date)
# summary(tmmx_year$Longitude == vs_year$Longitude & 
#           tmmx_year$Latitude == vs_year$Latitude & 
#           tmmx_year$Date == vs_year$Date)
# summary(tmmx_year$Longitude == th_year$Longitude & 
#           tmmx_year$Latitude == th_year$Latitude & 
#           tmmx_year$Date == th_year$Date)

meteo_year_all = tmmx_year
meteo_year_all$tmmn = tmmn_year$tmmn
meteo_year_all$rmax = rmax_year$rmax
meteo_year_all$rmin = rmin_year$rmin
meteo_year_all$vs = vs_year$vs
meteo_year_all$th = th_year$th
head(meteo_year_all); dim(meteo_year_all) # summary(meteo_year_all); 

# Save the merged data for this year
write_fst(meteo_year_all, 
          file.path(gridmet_stack_source_path, 
                    paste0("GRIDMET_commom_", met_year, "_in_US_grid_01.fst")))


library(USAboundaries)
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

ggplot() +
  geom_point(data = subset(meteo_year_all,
                           Date == "2017-04-21"),
             aes(x = Longitude, y = Latitude, color = rmax),
             size = 0.35, alpha = 0.8) +
  geom_sf(data = us_states,
          fill = NA, color = "grey70", size = 0.3) +
  scale_color_viridis_c(name = "rmax", option = "plasma") +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE)


###### GRIDMET 3.1, combine variables used for special sources ###### 
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

# The raster for projection
us_grid_raster_01 = raster(file.path("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/CMAQ_previous_extract_tries/us_grid_raster_01.tif"))
us_grid_raster = us_grid_raster_01
# us_grid_raster = rast(us_grid_raster_01)

# Set the paths for reading and saving files
gridmet_stack_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/aa_GRIDMET_stacked"
gridmet_stack_source_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/aa_GRIDMET_source_stacked"

# Define the date range for each year
study_years <- 2012 # 2011:2020   2011:2015

# Use rast to read, not raster, so as to directly get date info 
# Annual data, some variables may miss some days or grids, need to check every time!!!
# need recheck!!! and add the lost days inside!!!
fm100_year = rast(file.path(gridmet_stack_path, paste0("fm100_", study_years, "_stacked.tif")))
fm100_year

fm1000_year = rast(file.path(gridmet_stack_path, paste0("fm1000_", study_years, "_stacked.tif")))
fm1000_year # 2014, 363; 

bi_year = rast(file.path(gridmet_stack_path, paste0("bi_", study_years, "_stacked.tif")))
bi_year # 2014, 363; 2016, 365

# crs_biomass = st_crs(fm100_year)
# names(fm100_year) 

biomass_var_list <- 
  c("fm100", "fm1000", "bi") # 100-hour  and 1000-hour dead fuel moisture, bi, burning index
    # "pr", "vpd", "etr") # pr, Precipitation amount; vpd, Mean vapor pressure deficit; etr, Daily alfalfa reference evapotranspiration

# Initialize a list to store data.tables for each variable
meteo_biomass_list <- list()

# Loop through each year and combine the selected variables
for (met_year in study_years) { # met_year = 2017
  print(paste("GRIDMET year to be used:", met_year))
  
  # Initialize an empty list to store rasters for this year
  rasters_to_merge <- list()
  
  # Loop through each selected variable
  for (biomass_var in biomass_var_list) { # biomass_var = biomass_var_list[1]
    print(paste("GRIDMET Variable to be used:", biomass_var))
    
    # Construct the file path for the stacked raster of this variable and year
    file_path <- 
      file.path(gridmet_stack_path, 
                paste0(biomass_var, "_", met_year, "_stacked.tif"))
    
    # Process the variable to create the raster brick
    met_biomass_project <- 
      brick_var_date_project(
        file_path, 
        us_grid_raster)
    
    # Extract spatial information and values
    meteo_biomass_dt = 
      as.data.table(as.data.frame(met_biomass_project, xy = TRUE, na.rm = TRUE))
    # head(meteo_biomass_dt); dim(meteo_biomass_dt)
    
    # Reshape data from wide to long format (each variable in a single column)
    meteo_biomass_long <- 
      melt(meteo_biomass_dt, 
           id.vars = c("x", "y"), 
           variable.name = "Layer", 
           value.name = biomass_var,
           na.rm = TRUE)
    
    # Add Date 
    meteo_biomass_long[, Date := 
                     gsub(".*_(\\d{4}).(\\d{2}).(\\d{2})", 
                          "\\1-\\2-\\3", 
                          Layer)]
    meteo_biomass_long[, Layer := NULL]
    
    print("Check the file for the applied biomass_var")
    head(meteo_biomass_long); summary(meteo_biomass_long)
    
    # Store in list
    meteo_biomass_list[[biomass_var]] <- meteo_biomass_long
  }
  
  # # Merge all variables for this year
  # meteo_merged_year <- 
  #   Reduce(function(x, y) {
  #     merge(x, y, by = c("x", "y", "Date"), all = TRUE)
  #   }, meteo_biomass_list)
  # # meteo_merged_year$Date = as.Date(meteo_merged_year$Date)
  # 
  # # meteo_merged_year_wide =
  # #   meteo_merged_year %>%
  # 
  # print("Check the file for the all variable in this year")
  # head(meteo_merged_year); dim(meteo_merged_year)
  # 
  # # Save the merged data for this year
  # write_fst(meteo_merged_year, 
  #           file.path(gridmet_stack_source_path, 
  #                     paste0("Biomass_GRIDMET_", met_year, "_stacked.fst")))
  
  
  # Save the single meteo data one by one for this year
  for (biomass_var in biomass_var_list) { # biomass_var = biomass_var_list[2]
    
    meteo_biomass_year = meteo_biomass_list[[biomass_var]]
    meteo_biomass_year = 
      meteo_biomass_year[with(meteo_biomass_year, order(Date, x, y)), ]
    names(meteo_biomass_year)[1:2]
    names(meteo_biomass_year)[1:2] = c("Longitude", "Latitude")
    dim(meteo_biomass_year)
    
    print(paste("Check the output file for a single biomass_var", 
                head(meteo_biomass_year)
    ))
    
    write_fst(meteo_biomass_year, 
              file.path(gridmet_stack_source_path, 
                        paste0("Biomass_GRIDMET_", biomass_var, "_", met_year, "_stacked.fst")))
  }
  
  # Clear memory
  rm(meteo_merged_year, meteo_biomass_list)
  gc()
}

###### GRIDMET 3.2, separately read and then combine biomass variables, and interpolated if necessary ######  
# Read data of each variable and combine them into one
# Otherwise it takes too much memory and the computer cannot handle that

# rm(list = ls())

# Set the paths for reading and saving files
gridmet_stack_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/aa_GRIDMET_stacked"
gridmet_stack_source_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/aa_GRIDMET_source_stacked"

met_year = 2015 # 2015

fm100_year = 
  read_fst(file.path(gridmet_stack_source_path, 
                     paste0("Biomass_GRIDMET_", "fm100", "_", met_year, "_stacked.fst")))
fm1000_year = 
  read_fst(file.path(gridmet_stack_source_path, 
                     paste0("Biomass_GRIDMET_", "fm1000", "_", met_year, "_stacked.fst")))
bi_year = 
  read_fst(file.path(gridmet_stack_source_path, 
                     paste0("Biomass_GRIDMET_", "bi", "_", met_year, "_stacked.fst")))

dim(fm100_year); dim(fm1000_year); dim(bi_year)

##### Interpolation for the variable that lack days of records
# File of complete of the year
# 2016
biomass_full_date = fm100_year 

# 2015
biomass_full_date = fm100_year 

# 2014
biomass_full_date = fm100_year 

# 2013
biomass_full_date = fm100_year 

# 2012
biomass_full_date = fm100_year 


# Prepare complete date file of the year 
biomass_date = data.frame(table(biomass_full_date$Date))
names(biomass_date)[1] = "Date"
biomass_date$Freq = NULL
head(biomass_date); dim(biomass_date)

## 2016
bi_year = merge(biomass_date, bi_year, all.x = TRUE)
dim(biomass_full_date); dim(bi_year)



# Combine to get the final output
meteo_year_biomass = fm100_year
meteo_year_biomass$fm1000 = fm1000_year$fm1000
meteo_year_biomass$bi = bi_year$bi
head(meteo_year_biomass); dim(meteo_year_biomass) # ; summary(meteo_year_biomass)
sapply(meteo_year_biomass, class)

# Save the merged data for this year
write_fst(meteo_year_biomass, 
          file.path(gridmet_stack_source_path, 
                    paste0("GRIDMET_Biomass_", met_year, "_in_US_grid_01.fst")))


library(USAboundaries)
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

ggplot() +
  geom_point(data = subset(meteo_year_biomass,
                           Date == "2017-04-21"),
             aes(x = Longitude, y = Latitude, color = rmax),
             size = 0.35, alpha = 0.8) +
  geom_sf(data = us_states,
          fill = NA, color = "grey70", size = 0.3) +
  scale_color_viridis_c(name = "rmax", option = "plasma") +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE)




