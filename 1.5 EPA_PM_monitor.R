##clear environment
# rm(list=ls())

##set working directory
# setwd("/Users/ztttttt/Documents/HEI PMF/EPA_AQS_monitor")
# getwd()
# data.dir <- "/Users/ztttttt/Documents/HEI PMF/EPA_AQS_monitor"

setwd("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/EPA_AQS_monitor")
getwd()
data.dir <- "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/EPA_AQS_monitor"


library(data.table)
library(dplyr)
library(plyr)
library(ggplot2) 
library(ggrepel)

#### download data ####
# https://aqs.epa.gov/aqsweb/airdata/download_files.html#Daily
# "https://aqs.epa.gov/aqsweb/airdata/daily_88101_2015.zip"
local_pm_path = "original_AQSdata"

# download 2010-20 PM2.5 to local folder
study.year = 2010:2020
for(year.use in study.year){
  url = paste0("https://aqs.epa.gov/aqsweb/airdata/daily_88101_",
               year.use, ".zip")
  destfile <- paste0("daily_88101_", year.use, ".zip")
  download.file(url, file.path(local_pm_path, destfile))
  
  # unzip and remove zip
  unzip(file.path(local_pm_path, destfile), exdir = local_pm_path)
  unlink(file.path(local_pm_path, destfile))
}

#### read & combine 2011-20 PM2.5 daily data into one ####
# get a list of all CSV files in the folder
PM_files <- list.files(file.path(getwd(), "original_AQSdata"), 
                       pattern = "\\.csv$", full.names = TRUE)

# read & combine
PM_data <- lapply(PM_files, fread)
daily_PM25 <- 
  Reduce(
  function(x, y) 
    merge(x, y, by = intersect(names(x), names(y)), all = T), 
  PM_data)

dim(daily_PM25)
# daily_PM25_1 = daily_PM25

# delete 1-hour data
daily_PM25 = subset(daily_PM25, 
                    grepl(24, `Sample Duration`, fixed = T))

# remove columns not to be used 
daily_PM25$Parameter.Code = daily_PM25$Parameter.Name = 
  daily_PM25$Sample.Duration = daily_PM25$Pollutant.Standard = 
  daily_PM25$Units.of.Measure = daily_PM25$Observation.Count = 
  daily_PM25$Observation.Percent = daily_PM25$X1st.Max.Value = 
  daily_PM25$X1st.Max.Hour = daily_PM25$AQI = daily_PM25$Method.Name = 
  daily_PM25$Date.of.Last.Change = NULL
head(daily_PM25)
names(daily_PM25) <- gsub(" ", ".", names(daily_PM25))

# write.csv(daily_PM25, "EPA_AQS_PM2.5_orignial_combine_2011-20.csv")

aqs_site = select(daily_PM25, 
                  State.Code, County.Code, Site.Num, 
                  Latitude, Longitude, Address)
aqs_site = aqs_site[!duplicated(aqs_site), ] 
write.csv(aqs_site, "EPA_AQS_PM2.5_orignial_2011-20_AQSsites.csv")

#### match with species data ####
aqs_site = read.csv("EPA_AQS_PM2.5_orignial_2011-20_AQSsites.csv")
aqs_site$X = NULL

# site_info = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original CSN/CSN metadata 157 sample merged sites 2010-20.csv")
# site_info = select(site_info, Site, Country, State, County,  AQSCode, Latitude,  Longitude, StreetAddress)
site_info = 
  read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
site_info = 
  dplyr::select(site_info, 
                Dataset, SiteCode, geoid, state_abbr, namelsad, Latitude,  Longitude)
# site_info = subset(site_info, Dataset == "IMPAER")

# Approximate the Longitude and Latitude values to the nearest 0.001
aqs_site$long_approx <- round(aqs_site$Longitude, digits = 3)
aqs_site$lat_approx <- round(aqs_site$Latitude, digits = 3)
site_info$long_approx <- round(site_info$Longitude, digits = 3)
site_info$lat_approx <- round(site_info$Latitude, digits = 3)
length(unique(aqs_site$Site.Num))

# match the datasets based on the approximated values
matched_site <-
  merge(site_info, aqs_site, 
        by = c("long_approx", "lat_approx"))

# compare the original GPS coordinate
summary(matched_site$Longitude.x == matched_site$Longitude.y)
summary(matched_site$Latitude.x == matched_site$Latitude.y)
summary(matched_site$StreetAddress == matched_site$Address)

# select Sites that GPS do not match
not_same_GPS_site = subset(matched_site, Longitude.x != Longitude.y)

# filter the matched data based on the original GPS with a tolerance of 0.0001
update_match_site <- 
  matched_site[abs(matched_site$Longitude.x - matched_site$Longitude.y) < 0.0001 & 
                 abs(matched_site$Latitude.x - matched_site$Latitude.y) < 0.0001,]

# Sites not in the update_match_site
sites.not.match.gps = subset(matched_site, 
                             !(SiteCode %in% update_match_site$SiteCode))
sites.not.match.gps$Address
sites.not.match.gps$StreetAddress
sites.not.match.gps$SiteCode
# the Address and StreetAddress the sites in sites.not.match.gps are the same
# Thus, all sites in matched_site can be use for further study

# Remove the approximated columns
matched_site$long_approx <- NULL
matched_site$lat_approx <- NULL

# Only one GPS coordinates
matched_site$Latitude = (matched_site$Latitude.x + matched_site$Latitude.y)/2
matched_site$Longitude = (matched_site$Longitude.x + matched_site$Longitude.y)/2
matched_site$Latitude.x = matched_site$Latitude.y =
  matched_site$Longitude.x = matched_site$Longitude.y = NULL

# Change colname
# matched_site= plyr::rename(matched_site, 
#                            c("AQSCode" = "SiteCode"))

write.csv(matched_site, "EPA_CSN_AQS_Site_Match.csv")  

#### Match-1: START FROME HERE generate PM2.5 in AQS data ####
daily_PM25 = fread("EPA_AQS_PM2.5_orignial_combine_2011-20.csv")
daily_PM25$V1 = NULL
matched_site = read.csv("EPA_CSN_AQS_Site_Match.csv")

csn_daily_PM25 = subset(daily_PM25,
                        Address %in% matched_site$Address)
csn_daily_PM25 = join(csn_daily_PM25, 
                      select(matched_site, Address, SiteCode))

# generate data to use 
csn_aqs_PM25 = select(csn_daily_PM25, 
                      SiteCode, Date.Local, POC, Arithmetic.Mean)
colnames(csn_aqs_PM25)[2:4] = c("Date", "POC", "PM25_AQS")
csn_aqs_PM25$Date = as.Date(csn_aqs_PM25$Date)

#### Match-2: duplicated site-date groups in AQS data & determine PM for them ####
# check in case of duplicates
aqs_date_site = csn_aqs_PM25

aqs_date_site$dup.site.date = duplicated(aqs_date_site[, 1:2])
aqs_date_site$dup.poc = duplicated(aqs_date_site[, 1:3])
summary(aqs_date_site$dup.site.date)
summary(aqs_date_site$dup.poc)
unique(aqs_date_site$POC)

# detect the duplicated site-date groups
aqs_date_site$site.date = paste(aqs_date_site$SiteCode, aqs_date_site$Date)
csn_daily_PM25$site.date = paste(csn_daily_PM25$SiteCode, csn_daily_PM25$Date.Local)
csn_daily_PM25_dup = subset(csn_daily_PM25, 
                        site.date %in% 
                          subset(aqs_date_site, dup.site.date)$site.date)

# roughly checked the difference among duplicates
subset(csn_daily_PM25_dup, site.date == csn_daily_PM25_dup$site.date[1])
subset(csn_daily_PM25_dup, site.date == csn_daily_PM25_dup$site.date[2000])
subset(csn_daily_PM25_dup, site.date == csn_daily_PM25_dup$site.date[13000])

#### Match-3: replace those with duplicates with the mean ####
aqs_date_site_dup_PM = ddply(aqs_date_site, 
                             .(SiteCode, Date),
                             summarise,
                             PM25_AQS = mean(PM25_AQS))

write.csv(aqs_date_site_dup_PM, "EPA_CSN_AQS_daily_PM25.csv")

