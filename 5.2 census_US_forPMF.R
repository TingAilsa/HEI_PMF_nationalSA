
getwd()
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/US_census")
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/US_census"

# library(dplyr)
# library(readr)
# library(purrr) # txt combine
library(data.table)
library(readr)

##### Combine & get the industry txt #######
# Define the path to your industry_folder containing txt files
industry_folder <- "/Users/TingZhang/Documents/HEI HAQ PMF/US_census/county_industry"

# Define the year range
years <- 2011:2020

# Read the file and bind it with others using data.table
read_and_bind <- function(year) {
  file_path <- file.path(industry_folder, paste0("cbp_", year, ".txt"))
  dt <- fread(file_path) # fread from data.table package
  dt[, Year := year]
  return(dt)
}

# Combine files
combined_dt <- rbindlist(lapply(years, read_and_bind), fill = TRUE)

# Save combined data as CSV
write_csv(as.data.frame(combined_dt), file.path(industry_folder, "US_industry_2011-20.csv"))

combined_use = select(combined_dt, fipstate, fipscty, naics, EMP)
combined_use_noNA = na.omit(combined_use)


##### traffic #######
library(dplyr)
library(stringr)

us_tra = fread("/Users/TingZhang/Documents/HEI HAQ PMF/US_census/County_Transportation_Profiles__bts.gov_ctp_-2.csv")
us_tra = select(us_tra, "County FIPS", "County Name", "State FIPS", "State Name", 
                 "Primary and Commercial Airports", "Total Docks", 
                 "Route miles of freight railroad", "Route miles of passenger railroad and rail transit")
colnames(us_tra) = c("county_fips", "county_name", "state_fips", "state_name", 
                     "Airports", "Docks", "rail_freight", "rail_passenger")

us_tra <- us_tra %>%
  mutate(
    fipstate = str_pad(state_fips, 2, pad = "0"),  # Ensure 2 digits for state
    fipscty = str_pad(county_fips, 3, pad = "0"),   # Ensure 3 digits for county
    fips = as.character(paste0(fipstate, fipscty))            # Combine into a 5-digit code
  )

us_tra$geoid = ifelse(us_tra$county_fips < 10000, 
                      paste0("0", us_tra$county_fips), 
                      us_tra$county_fips)

us_tra_use = select(us_tra, geoid, county_name, state_name, 
                    Airports, Docks, rail_freight, rail_passenger)

cty_rural_urban = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$X.1 = NULL
cty_rural_urban = cty_rural_urban[!duplicated(cty_rural_urban$SiteCode), ] 

csn_site = read.csv("/Users/TingZhang/Downloads/CSN_RF_cluster5training.csv")
imp_site = read.csv("/Users/TingZhang/Downloads/IMPROVE_RF_cluster_SiteLevel.csv")
imp_site$X = NULL
csn_site$Dataset = "EPACSN"
imp_site$Dataset = "IMPAER"
csn_imp_cluster = rbind(csn_site, imp_site)

cty_fips_cluster = merge(cty_rural_urban, csn_imp_cluster)

cty_cluster_traffic = merge(cty_fips_cluster, us_tra_use)
cty_cluster = ddply(cty_cluster_traffic, .(Dataset, Final.Decision), 
                    summarise,
                    Longitude = mean(Longitude),
                    Latitude = mean(Latitude))
write.csv(cty_cluster_traffic, "County_cluster_traffic_info.csv")

cty_cluster_traffic = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/results_R_data/County_cluster_traffic_info.csv")
cty_cluster_traffic$X = NULL
cluster_traffic = ddply(cty_cluster_traffic,
                        .(Dataset, as.factor(Final.Decision)), 
                        summarise,
                        PopDensity = mean(PopDensity),
                        Airports = mean(Airports, rm.na = T),
                        Docks = mean(Docks, rm.na = T),
                        rail_freight = mean(rail_freight, rm.na = T),
                        rail_passenger = mean(rail_passenger, rm.na = T))

cluster_traffic$Dataset[cluster_traffic$Dataset == "EPACSN"] = "CSN"
cluster_traffic$Dataset[cluster_traffic$Dataset == "IMPAER"] = "IMPROVE"
colnames(cluster_traffic)[2] = "Cluster"

datasets = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/results_R_data/IMPROVE_CSN_datasets.csv")
cluster_traffic = merge(datasets, cluster_traffic, all.x = T)
write.csv(cluster_traffic, "cluster_traffic_dock_rail_airport.csv")

library(ggplot2)
library(maps)
us_map <- map_data("state")

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = data_frame, aes(x = Longitude, y = Latitude), color = "blue") +
  geom_text(data = data_frame, aes(x = Longitude, y = Latitude, label = Final.Decision), vjust = -1, size = 3) +
  labs(title = "Map of Final Decisions across US Sites") +
  theme_minimal()

##### cluter-site-location-rural/urban groups ####

###### 1. CSN
csn_site_rural_urban = read.csv("CSN_site_rural_urban_land_use.csv")
csn_site_rural_urban = select(csn_site_rural_urban, 
                              Code, State, Site, 
                              DemographicCode, LandUseCode)
colnames(csn_site_rural_urban)[1] = "SiteCode"

csn_cluster = read.csv("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Attachments/CSN_RF_cluster5training.csv")
csn_cluster$X = NULL
csn_cluster_siteinfo = merge(csn_cluster, 
                             csn_site_rural_urban, 
                             all.x = T)
dim(csn_cluster_siteinfo)

csn_cluster_siteinfo$Sites.in.Cluster = 
  paste0(csn_cluster_siteinfo$SiteCode, "-", 
         csn_cluster_siteinfo$State, "-",
         csn_cluster_siteinfo$Site, "-",
         csn_cluster_siteinfo$DemographicCode, "-",
         csn_cluster_siteinfo$LandUseCode, "-")

csn_cluster_siteinfo = select(csn_cluster_siteinfo, 
                              Final.Decision, Sites.in.Cluster)

a = csn_cluster_siteinfo %>% spread(Sites.in.Cluster, )
head(a)

###### 2. IMPROVE

imp_site_rural_urban = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
imp_site_rural_urban = select(imp_site_rural_urban, 
                              SiteCode, state_abbr, namelsad, 
                              RuralUrban)
colnames(imp_site_rural_urban)[2:4] = c("State", "Site", "DemographicCode")

imp_cluster = read.csv("/Users/TingZhang/Downloads/IMPROVE_RF_cluster_SiteLevel.csv")
imp_cluster$X = NULL
imp_cluster_siteinfo = merge(imp_cluster, 
                             imp_site_rural_urban, 
                             all.x = T)
dim(imp_cluster_siteinfo)

imp_cluster_siteinfo$Sites.in.Cluster = 
  paste0(imp_cluster_siteinfo$SiteCode, "-", 
         imp_cluster_siteinfo$State, "-",
         imp_cluster_siteinfo$Site, "-",
         imp_cluster_siteinfo$DemographicCode, "-")

imp_cluster_siteinfo = select(imp_cluster_siteinfo, 
                              Final.Decision, Sites.in.Cluster)


###### 3. combine CSN & IMPROVE
csn_cluster_siteinfo$Dataset = "CSN"
imp_cluster_siteinfo$Dataset = "IMPROVE"
cluster_site_info = rbind(csn_cluster_siteinfo, imp_cluster_siteinfo)

cluster_site_info_spread = 
  cluster_site_info %>%
  spread(Final.Decision, Sites.in.Cluster) #Sites.in.Cluster, Dataset, Final.Decision
head(cluster_site_info_spread)


