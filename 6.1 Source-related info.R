library(data.table)
library(dplyr)
library(plyr)
library(base)
library(sf)

setwd("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nation_SA_data/")
getwd()

#### Unique CSN dates ####
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

#### HMS-SMOKE, NOAA ####

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


# list & merge all .shp data
hms_shp_list <- list.files(hms_smoke_folder, pattern = ".shp", full.names = TRUE)
hms_shp_list

# Define a function to extract the day from the file name and read the file
read_and_add_day <- function(file_path) {
  # Extract the day from the file name using a regular expression
  day <- as.numeric(gsub(".*?(\\d+).*", "\\1", basename(file_path)))
  
  # Read the shapefile
  shp <- st_read(file_path)
  
  # Add the day as a new column
  shp$Day <- day
  
  return(shp)
}

read_and_add_day("HMS_SMOKE_shp/hms_smoke20200518.shp")
# Apply the function to each file in the list and combine the results
hms_shp_all <- do.call(
  rbind,
  lapply(hms_shp_list, read_and_add_day)
)

hms_shp_all <- 
  do.call(
    rbind, 
    (lapply(
      hms_shp_list, 
      st_read)))




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

# extract columns to use 
tri_use_1 = 
  select(tri_overall,
         "YEAR", "TRIFD", "FRS ID", "STANDARD PARENT CO NAME", "CITY", "COUNTY", 
         "ST", "ZIP", "BIA", "LATITUDE", "LONGITUDE", "HORIZONTAL DATUM", "FEDERAL FACILITY", 
         "INDUSTRY SECTOR CODE", "INDUSTRY SECTOR", "PRIMARY NAICS", "NAICS 2", 
         "CHEMICAL", "CLASSIFICATION", "METAL", "METAL CATEGORY", 
         "FORM TYPE", "UNIT OF MEASURE", "5.1 - FUGITIVE AIR", "5.2 - STACK AIR")

library(quanteda)
names(tri_use_1) = tolower(names(tri_use_1)) # change capital to lower
names(tri_use_1) = sub(" ", "_", names(tri_use_1)) # replace space
names(tri_use_1)[(ncol(tri_use_1)-1):ncol(tri_use_1)] = c("fugitive_air", "stack_air")
