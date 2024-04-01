##clear environment
# rm(list=ls())

##set working directory
# setwd("/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp")
# getwd()
# data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp"

setwd("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_IMPROVE_ownPC")
getwd()
data.dir <- "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_IMPROVE_ownPC"


##packages in need
library(tidyr) # separate{tidyr}, gather{tidyr}, spread{tidyr},  spread is VIP function, str_split_fixed{stringr} is better than separate
library(stats) # aggregate{stats}, VIP function
library(ggplot2)
library(scales) # percent{}
library(dplyr)

library(randomForest)

library(ClustGeo)
library(cluster)
library(sf)
library(sp)
# library(spdep)
library(ggsci)
library(data.table)

library(usmap)
library(USAboundaries)


#############################################################
##### 1111. no train/test: interpolation for IMPROVE #####
#############################################################
# changed! # cty_rural_urban = fread("/Users/ztttttt/Dropbox/HEI_US_PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban = fread("/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$V1 = NULL

# imp_daily = read.csv("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/IMPROVE_Component_with_missing.csv")
# imp_daily = fread("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/IMPROVE_interpulation_random-forest_afterLog.csv")
# imp_daily = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/IMPROVE_interpulation_random-forest_afterLog.csv")
imp_daily = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_IMPROVE_ownPC/IMPROVE_interpulation_random-forest_2023.csv")
imp_daily$V1 = NULL
imp_daily$Date = as.Date(imp_daily$Date)
sapply(imp_daily, class)
head(imp_daily)
dim(imp_daily)

imp_cty_rural_urban = subset(cty_rural_urban, Dataset == "IMPAER")
# imp_cty_rural_urban = select(imp_cty_rural_urban, SiteCode, RuralUrban, geoid, Longitude, Latitude)
imp_cty_rural_urban = 
  imp_cty_rural_urban[ , 
                       .(SiteCode, RuralUrban, 
                         geoid, Longitude, Latitude)]

imp_cty_rural_urban$geoid = ifelse(imp_cty_rural_urban$geoid < 10000, 
                                   paste0("0", imp_cty_rural_urban$geoid), 
                                   imp_cty_rural_urban$geoid)
# cty_rural_urban_count = data.frame(table(cty_rural_urban$RuralUrban))
cty_rural_urban_count = as.data.table(table(cty_rural_urban$RuralUrban))
colnames(cty_rural_urban_count)[1] = "Rural Urban Classification"

## setting 30 colors for plotting clusters
npg.color = c("1" = "firebrick3", "2" = "lightskyblue", "3" = "lightseagreen", 
              "4" = "royalblue4", "5" = "salmon3", "6" = "skyblue3", 
              "7" = "turquoise", "8" = "darksalmon", "9" = "burlywood4", 
              "10" = "burlywood3", "11" = "sandybrown", "12" = "goldenrod3", 
              "13" = "darkcyan", "14" = "bisque3", "15" = "darkseagreen",
              "16" = "lightpink3", "17" = "lightsteelblue2", "18" = "mistyrose3", 
              "19" = "mediumturquoise", "20" = "indianred2", "21" = "darkslategray4", 
              "22" = "mistyrose3", "23" = "lightgoldenrod2", "24" = "lightblue2", 
              "25" = "plum2", "26" = "orchid3", "27" = "steelblue3",
              "28" = "khaki4", "29" = "indianred", "30" = "mediumpurple2")


### only for the file "IMPROVE_Component_with_missing.csv"
# imp_daily = subset(imp_daily, Date > as.Date("2010-12-31"))
# colnames(imp_daily)[37:48] # OPC sub-group
# imp_daily_opt_sub = imp_daily[ ,37:48]
# imp_daily = cbind(imp_daily[,1:36], imp_daily[,49:ncol(imp_daily)])
# remove those not directly detected, except of RC.PM2.5, ammoniaSO4, ammoniaNO3, OC & EC
# imp_daily$SeaSalt = imp_daily$Soil = imp_daily$PM10 = imp_daily$RC.PM10 = imp_daily$site.date.qualifier = NULL
# imp_daily$OC_UCD = imp_daily$EC_UCD = imp_daily$OMC = imp_daily$CM_calculated = imp_daily$TC =NULL

### exclude PM species not to be used for PMF 
imp_tracer = imp_daily
# imp_tracer$ammNO3 = imp_tracer$ammSO4 = # recalculated species
  # imp_tracer$NO2. = imp_tracer$Cl. = # only existed in IMPROVE dataset
  imp_tracer$Rb = imp_tracer$Zr = # imp_tracer$Se = 
  imp_tracer$PM25 = NULL # not PM species
sapply(imp_tracer, class)

# log the species concentrations before clustering
imp_tracer = cbind(imp_tracer[, 1:3],
                   log(imp_tracer[, 4:(ncol(imp_tracer))]))

head(imp_tracer)

imp_tracer$year = year(imp_tracer$Date)
imp_tracer$month = month(imp_tracer$Date)

##### mean of SITE level of Whole study period
imp_tracer_mean = 
  select(imp_tracer, -Date, -State) %>% 
  group_by(SiteCode) %>%
  summarise_all(# across(Al:Zn), # when using across, sometimes error can hardly be resolved
                mean, na.rm = T)
imp_tracer_mean$Date = imp_tracer_mean$Qualifier = imp_tracer_mean$State = 
  imp_tracer_mean$year = imp_tracer_mean$month = NULL

##### mean of SITE level of montly average
imp_tracer_mean = 
  select(imp_tracer, -Date, -State) %>% 
  group_by(SiteCode, year, month) %>%
  summarise_all(# across(Al:Zn), # when using across, sometimes error can hardly be resolved
    mean, na.rm = T)
imp_tracer_mean$year = log(imp_tracer_mean$year)
imp_tracer_mean$month = log(imp_tracer_mean$month)
imp_tracer_mean$Date = imp_tracer_mean$Qualifier = imp_tracer_mean$State = NULL

# check if there are NAs in df
which(is.na(imp_tracer_mean), arr.ind=TRUE) # 

# add geoid (here equals to fips information here) for each site
imp_tracer_gps = merge(imp_tracer_mean, 
                        imp_cty_rural_urban, 
                        all.x = T)
# remove the site without state, geoid info, 
imp_tracer_gps = subset(imp_tracer_gps,
                        !is.na(geoid))
colnames(imp_tracer_gps)

# log the absolute values of longitudes and latitudes in case of considering gegraphic locations
imp_tracer_gps$log.Long = log(abs(imp_tracer_gps$Longitude))
imp_tracer_gps$log.Lat = log(abs(imp_tracer_gps$Latitude))

# generate the US county boundary data
us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr[, 13] = NULL # there are two "state_name"
head(us_cty_bdr)
us_cty_bdr_geo = dplyr::select(us_cty_bdr, geoid, stusps, geometry)

# check whether all geoid in IMPROVE are in US county boundary data
sum(imp_tracer_gps$geoid %in% us_cty_bdr_geo$geoid)
sum(us_cty_bdr_geo$geoid %in% imp_tracer_gps$geoid)

##### check if missing geoid for sites, can jump, START  #####
# unique(imp_tracer_gps$geoid[!(imp_tracer_gps$geoid %in% 
#                                 us_cty_bdr_geo$geoid)])
# unique(imp_tracer_gps$geoid[imp_tracer_gps$geoid %in% 
#                               us_cty_bdr_geo$geoid])
# 
# # extract site info for those missing fips/geoid
# cty_rural_urban_NA_geoid = subset(cty_rural_urban,
#                                   !(SiteCode %in% 
#                                     unique(imp_tracer_gps$SiteCode)))
# 
# imp_tracer_gps_NA_geoid = subset(imp_tracer_gps, 
#                                   is.na(geoid) & 
#                                     (!is.na(Longitude)))
# imp_tracer_NA_geoid_site = dplyr::select(
#   imp_tracer_gps_NA_geoid, 
#   SiteCode, Longitude, Latitude)
# 
# # detect the sites in US county boundary data for that missing fips/geoid
# imp_tracer_NA_geoid_site_sf <- st_as_sf(imp_tracer_NA_geoid_site, 
#                                         coords = c("Longitude", "Latitude"), 
#                                         crs = 4326) #crs = 4326 refers to WGS84.
# nearest_index <- st_nearest_feature(imp_tracer_NA_geoid_site_sf, us_cty_bdr)
# 
# # extract the geoid from us_cty_bdr 
# imp_tracer_NA_geoid_site$nearest_geoid <- us_cty_bdr$geoid[nearest_index]
# us_cty_bdr$namelsad[nearest_index] 
# # Keweenaw County, geoid 26083, is MI's least populous county, ~2100 population 
# 
# # complete the geoid info
# imp_tracer_gps <- 
#   left_join(imp_tracer_gps, 
#             dplyr::select(imp_tracer_NA_geoid_site,
#                           SiteCode, nearest_geoid), 
#             by = "SiteCode") %>%
#   mutate(geoid = coalesce(geoid, 
#                           nearest_geoid)) %>%
#   dplyr::select(-nearest_geoid) # remove the column
# 
# imp_tracer_gps$RuralUrban[imp_tracer_gps$SiteCode == "ISLE1"] = "Rural"
# imp_tracer_gps = subset(imp_tracer_gps, 
#                          !(is.na(geoid)))
##### CAN JUMP THIS PART, end ######

######## IMPROVE - Cluster #########

# add geometry information 
imp_tracer_gps_bdr = merge(us_cty_bdr_geo, 
                           imp_tracer_gps)

colnames(imp_tracer_gps_bdr)
colnames(imp_tracer_gps_bdr)[1] = "fips"

imp_dat_tra = imp_tracer_gps_bdr
imp_dat_tra$geometry.1 = imp_dat_tra$geometry = NULL #imp_dat_tra$SiteCode = 
imp_dat_tra$Longitude = imp_dat_tra$Latitude = NULL
# change rowname (SiteCode) to first column
# rownames(imp_dat_tra) <- imp_dat_tra$SiteCode
colnames(imp_dat_tra)

# double check if fips and rural/urban classification match
summary(imp_dat_tra$RuralUrban == imp_tracer_gps_bdr$RuralUrban)
summary(imp_dat_tra$SiteCode == imp_tracer_gps_bdr$SiteCode)
summary(imp_dat_tra$fips == imp_tracer_gps_bdr$fips)

######  NO GPS info in the random forest model
imp_log_rf_fit <- randomForest(x = imp_dat_tra[, 4:(ncol(imp_dat_tra) - 2)], 
                               y = NULL, 
                               ntree = 10000, 
                               proximity = TRUE, 
                               oob.prox = TRUE)
imp_log_rf_hclust <- hclust(as.dist(1-imp_log_rf_fit$proximity), method = "ward.D2")

######  With GPS info in the random forest model
imp_log_rf_gps_fit <- randomForest(x = imp_dat_tra[, 4:ncol(imp_dat_tra)], 
                               y = NULL, 
                               ntree = 10000, 
                               proximity = TRUE, 
                               oob.prox = TRUE)
imp_log_rf_hclust <- hclust(as.dist(1-imp_log_rf_gps_fit$proximity), method = "ward.D2")

# generate 25 clusters
imp_log_rf_hclust_tree = cutree(imp_log_rf_hclust, k=25)

imp_rf_cluster = imp_dat_tra
imp_rf_cluster$SiteCode = imp_tracer_gps_bdr$SiteCode

# assign cluster results
imp_rf_cluster$rf.clusters <- imp_log_rf_hclust_tree
# table(imp_log_rf_hclust_tree, iris$Species)
imp_rf_cluster$rf.clusters.fc = as.factor(imp_rf_cluster$rf.clusters)
data.frame(table(imp_rf_cluster$rf.clusters))

imp_rf_cluster$Longitude = -exp(imp_rf_cluster$log.Long)
imp_rf_cluster$Latitude = exp(imp_rf_cluster$log.Lat)

plot_usmap(data = imp_rf_cluster, values = "rf.clusters.fc", labels = F, 
           label_color = "white", regions = "counties") + 
  theme(panel.background=element_blank()) +
  scale_fill_manual(values = npg.color[1:25])

# MainStates <- map_data("state")
UScounty <- map_data("county")
ggplot(imp_rf_cluster, 
       aes(Longitude, Latitude, color= rf.clusters.fc)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = rf.clusters.fc), linetype="dashed") +
  scale_color_manual(values = npg.color[1:25]) +
  theme_void()

#############################################################
#### 1111. no train/test: Interpolation for CSN ####
#############################################################
##### 1. import and match CSN #####

# import CSN
# csn_daily_before = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_interpulation_random-forest_afterLog_before_2015.csv")
# csn_daily_after = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_interpulation_random-forest_afterLog_after_2015.csv")
csn_daily_before = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/R - original IMPROVE/CSN_interpulation_random-forest_until_2015_2023.03.csv")
csn_daily_after = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/R - original IMPROVE/CSN_interpulation_random-forest_from_2016_2023.03.csv")

csn_daily_before$X = csn_daily_after$X = NULL
csn_daily_before$Date = as.Date(csn_daily_before$Date)
csn_daily_after$Date = as.Date(csn_daily_after$Date)

csn_bfr_remove = c(# "EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   # "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC.unadjusted.88", "EC.unadjusted.88", "OPC.unadjusted.88",
                   "Accept.PM2.5",
                   # "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
                   # "K.", "Na.", "NH4.", "Sb", "Sn", 
                   "Rb", "Zr")

csn_aft_remove = c(# "EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   # "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC1.88", "OC2.88", "OC3.88", "OC4.88",
                   "EC1.88", "EC2.88", "EC3.88", 
                   "EC1", "EC2", "EC3", 
                   "EC.unadjusted.88", "EC.TOR.unadjust.88", "EC.88",
                   "OC.unadjusted.88", "OC.TOR.unadjusted.88", "OC", 
                   "OPC.88", "OP.TOR.unadjusted.88", "OPC.unadjusted.88",
                   "PM2.5RC",
                   # "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
                   # "K.", "Na.", "NH4.", "Sb", "Sn", 
                   "Rb", "Zr", "Cl.")

csn_daily_before[ ,csn_bfr_remove] <- list(NULL)
csn_daily_after[ ,csn_aft_remove] <- list(NULL)

colnames(csn_daily_before)[c(17, 32, 37)] = c("EC", "OC", "OP")
colnames(csn_daily_after)[c(17, 32, 37)] = c("EC", "OC", "OP")

setDT(csn_daily_before)
setDT(csn_daily_after)

summary(colnames(csn_daily_after) == colnames(csn_daily_before))
csn_daily = rbind(csn_daily_before, csn_daily_after)

## setting 30 colors for plotting clusters
npg.color = c("1" = "firebrick3", "2" = "lightskyblue", "3" = "lightseagreen", 
              "4" = "royalblue4", "5" = "salmon3", "6" = "skyblue3", 
              "7" = "turquoise", "8" = "darksalmon", "9" = "burlywood4", 
              "10" = "burlywood3", "11" = "sandybrown", "12" = "goldenrod3", 
              "13" = "darkcyan", "14" = "bisque3", "15" = "darkseagreen",
              "16" = "lightpink3", "17" = "lightsteelblue2", "18" = "mistyrose3", 
              "19" = "mediumturquoise", "20" = "indianred2", "21" = "darkslategray4", 
              "22" = "mistyrose3", "23" = "lightgoldenrod2", "24" = "lightblue2", 
              "25" = "plum2", "26" = "orchid3", "27" = "steelblue3",
              "28" = "khaki4", "29" = "indianred", "30" = "mediumpurple2")

##### 2. get the population data - rural & urban #####
cty_rural_urban = fread("/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$V1 = NULL

csn_cty_rural_urban = cty_rural_urban
csn_cty_rural_urban = select(csn_cty_rural_urban, 
                             SiteCode, RuralUrban, geoid, Longitude, Latitude)
csn_cty_rural_urban$geoid = ifelse(csn_cty_rural_urban$geoid < 10000, 
                                       paste0("0", csn_cty_rural_urban$geoid), 
                                       csn_cty_rural_urban$geoid)
cty_rural_urban_count = data.frame(table(cty_rural_urban$RuralUrban))
colnames(cty_rural_urban_count)[1] = "Rural Urban Classification"
# 33 mix, 190 rural, 106 urban 

csn_cty_rural_urban$dup.site = duplicated(csn_cty_rural_urban$SiteCode)
csn_cty_rural_urban = subset(csn_cty_rural_urban, !dup.site) ### removed the duplicated 60290014
csn_cty_rural_urban$dup.site = NULL


##### 3. prepare logged data for clustering on each site #####
# log the species concentrations before clustering
csn_tracer = cbind(csn_daily[, 1:4],
                       log(csn_daily[, 5:(ncol(csn_daily))]))
head(csn_tracer)
csn_tracer$SiteCode = as.character(csn_tracer$SiteCode)

# get the mean value of all common PM species 
csn_tracer_mean = csn_tracer  %>% 
  group_by(SiteCode) %>%
  summarise_all(# across(Al:Zn), # when using across, sometimes error can hardly be resolved
    mean, na.rm = T)
csn_tracer_mean$Date = csn_tracer_mean$Qualifier = csn_tracer_mean$State = NULL

# double check if there are NAs in csn_tracer_mean
which(is.na(csn_tracer_mean), arr.ind=TRUE) # 

# add geoid (here equals to fips information here) for each site
csn_tracer_mean = merge(csn_tracer_mean, 
                            csn_cty_rural_urban, 
                            all.x = T)
head(csn_tracer_mean)
# site 20900035 has no geoid???

# log the absolute values of longitudes and latitudes in case of considering geographic locations
# this step was cancelled to increase the influence from location 
# (sites far away could be grouped in log GPS coordinates)
# csn_tracer_mean$log.Long = log(abs(csn_tracer_mean$Longitude))
# csn_tracer_mean$log.Lat = log(abs(csn_tracer_mean$Latitude))

# generate the US county boundary data
us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr[, 13] = NULL # there are two "state_name"
head(us_cty_bdr)
us_cty_bdr_geo = dplyr::select(us_cty_bdr, geoid, stusps, geometry)

# add geometry information 
csn_tracer_mean_bdr = merge(us_cty_bdr_geo, csn_tracer_mean)
dim(csn_tracer_mean_bdr)
colnames(csn_tracer_mean_bdr)
colnames(csn_tracer_mean_bdr)[1] = "fips"


csn_dat_tra = csn_tracer_mean_bdr
csn_dat_tra$geometry.1 = csn_dat_tra$geometry = NULL
# csn_dat_tra$Longitude = csn_dat_tra$Latitude = NULL

# change rowname (SiteCode) to first column
rownames(csn_dat_tra) <- csn_dat_tra$SiteCode

# add information of the fips and whether if a county is rural or urban
csn_dat_tra$RuralUrban = csn_tracer_mean_bdr$RuralUrban
csn_dat_tra$fips = csn_tracer_mean_bdr$fips
head(csn_dat_tra)

##### 4. cluster with random forest - NO GPS #####
# consider no gps info in the random forest model
csn_log_rf_fit <- randomForest(x = csn_dat_tra[, 4:(ncol(csn_dat_tra) - 2)], 
                                   y = NULL, 
                                   ntree = 10000, 
                                   proximity = TRUE, 
                                   oob.prox = TRUE)

csn_log_rf_hclust <- hclust(as.dist(1-csn_log_rf_fit$proximity), method = "ward.D2")

csn_log_rf_hclust_tree = cutree(csn_log_rf_hclust, k=25)

csn_dat_rf = csn_dat_tra
csn_dat_rf$rf.clusters <- csn_log_rf_hclust_tree
# table(csn_log_rf_hclust_tree, iris$Species)
csn_dat_rf$rf.clusters.fc = as.factor(csn_dat_rf$rf.clusters)
data.frame(table(csn_dat_rf$rf.clusters))


plot_usmap(data = csn_dat_rf, values = "rf.clusters.fc", labels = F, 
           label_color = "white", regions = "counties") + 
  # scale_fill_manual(values = npg.color[1:20])+
  theme(panel.background=element_blank()) 

# MainStates <- map_data("state")
UScounty <- map_data("county")
ggplot(csn_dat_rf, 
       aes(Longitude, Latitude, color= rf.clusters.fc)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = rf.clusters.fc), linetype="dashed") +
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()

ggplot(subset(csn_dat_rf, rf.clusters == 19), # with gps, 18
       aes(Longitude, Latitude, color= rf.clusters.fc)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = rf.clusters.fc), linetype="dashed") +
  scale_color_manual(values = "forestgreen") +
  theme_linedraw()

##### 5. cluster with random forest - with GPS #####
#consider gps info in the random forest model
csn_log_rf_gps_fit <- randomForest(x = csn_dat_tra[, 4:ncol(csn_dat_tra)], 
                                       y = NULL, 
                                       ntree = 10000, 
                                       proximity = TRUE, 
                                       oob.prox = TRUE)

rf_hclust_gps <- hclust(as.dist(1-csn_log_rf_gps_fit$proximity), method = "ward.D2")

rf_hclust_gps_tree = cutree(rf_hclust_gps, k=25)

csn_rf_hclust_gps = csn_dat_tra
csn_rf_hclust_gps$rf.clusters <- rf_hclust_gps_tree
# table(rf_hclust_gps_tree, iris$Species)
csn_rf_hclust_gps$rf.clusters.fc = as.factor(csn_rf_hclust_gps$rf.clusters)
data.frame(table(csn_rf_hclust_gps$rf.clusters))

plot_usmap(data = csn_rf_hclust_gps, values = "rf.clusters.fc", labels = F, 
           label_color = "white", regions = "counties") + 
  # scale_fill_manual(values = npg.color[1:20])+
  theme(panel.background=element_blank()) 

# MainStates <- map_data("state")
UScounty <- map_data("county")
ggplot(csn_rf_hclust_gps, 
       aes(Longitude, Latitude, color= rf.clusters.fc)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = rf.clusters.fc), linetype="dashed") +
  scale_color_manual(values = npg.color[1:25]) +
  theme_void()

ggplot(subset(csn_rf_hclust_gps, rf.clusters == 18), 
       aes(Longitude, Latitude, color= rf.clusters.fc)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = rf.clusters.fc), linetype="dashed") +
  scale_color_manual(values = "forestgreen") +
  theme_linedraw()

#############################################################
#### 2222. Interpolation  - trainging & testing ####
#############################################################
##### 1.1 import and match CSN #####

# import CSN
# csn_daily_before = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/CSN_Interpolation/CSN_interpulation_random-forest_afterLog_before_2015.csv")
# csn_daily_after = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/CSN_Interpolation/CSN_interpulation_random-forest_afterLog_after_2015.csv")

csn_daily_before = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/R - original IMPROVE/CSN_interpulation_random-forest_until_2015_2023.03.csv")
csn_daily_after = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/R - original IMPROVE/CSN_interpulation_random-forest_from_2016_2023.03.csv")

csn_daily_before$X = csn_daily_after$X = NULL
csn_daily_before$Date = as.Date(csn_daily_before$Date)
csn_daily_after$Date = as.Date(csn_daily_after$Date)

csn_bfr_remove = c(# "EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   # "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC.unadjusted.88", "EC.unadjusted.88", "OPC.unadjusted.88",
                   "Accept.PM2.5",
                   # "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
                   # "K.", "Na.", "NH4.", "Sb", "Sn", 
                   "Rb", "Zr")

csn_aft_remove = c(# "EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   # "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC1.88", "OC2.88", "OC3.88", "OC4.88",
                   "EC1.88", "EC2.88", "EC3.88", 
                   "EC1", "EC2", "EC3", 
                   "EC.unadjusted.88", "EC.TOR.unadjust.88", "EC.88",
                   "OC.unadjusted.88", "OC.TOR.unadjusted.88", "OC", 
                   "OPC.88", "OP.TOR.unadjusted.88", "OPC.unadjusted.88",
                   "PM2.5RC",
                   # "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
                   # "K.", "Na.", "NH4.", "Sb", "Sn", 
                   "Rb", "Zr", "Cl.")

csn_daily_before[ ,csn_bfr_remove] <- list(NULL)
csn_daily_after[ ,csn_aft_remove] <- list(NULL)

colnames(csn_daily_before)[c(17, 32, 37)] = c("EC", "OC", "OP")
colnames(csn_daily_after)[c(17, 32, 37)] = c("EC", "OC", "OP")

setDT(csn_daily_before)
setDT(csn_daily_after)

csn_daily = rbind(csn_daily_before, csn_daily_after)
csn_daily$Date = as.Date(csn_daily$Date)

##### 1.2 import and match IMPROVE #####
imp_daily = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_IMPROVE_ownPC/IMPROVE_interpulation_random-forest_2023.csv")
imp_daily$V1 = NULL
imp_daily$Date = as.Date(imp_daily$Date)

## setting 30 colors for plotting clusters
npg.color = c("1" = "firebrick3", "2" = "lightskyblue", "3" = "lightseagreen", 
              "4" = "royalblue4", "5" = "salmon3", "6" = "skyblue3", 
              "7" = "turquoise", "8" = "darksalmon", "9" = "burlywood4", 
              "10" = "burlywood3", "11" = "sandybrown", "12" = "goldenrod3", 
              "13" = "darkcyan", "14" = "bisque3", "15" = "darkseagreen",
              "16" = "lightpink3", "17" = "lightsteelblue2", "18" = "mistyrose3", 
              "19" = "mediumturquoise", "20" = "indianred2", "21" = "darkslategray4", 
              "22" = "mistyrose3", "23" = "lightgoldenrod2", "24" = "lightblue2", 
              "25" = "plum2", "26" = "orchid3", "27" = "steelblue3",
              "28" = "khaki4", "29" = "indianred", "30" = "mediumpurple2")

##### 2. get the population data - rural & urban #####
cty_rural_urban = fread("/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$V1 = NULL

species_cty_rural_urban = cty_rural_urban
species_cty_rural_urban = select(species_cty_rural_urban, 
                                 SiteCode, RuralUrban, geoid, Longitude, Latitude)
species_cty_rural_urban$geoid = ifelse(species_cty_rural_urban$geoid < 10000, 
                                       paste0("0", species_cty_rural_urban$geoid), 
                                       species_cty_rural_urban$geoid)
cty_rural_urban_count = data.frame(table(cty_rural_urban$RuralUrban))
colnames(cty_rural_urban_count)[1] = "Rural Urban Classification"

species_cty_rural_urban$dup.site = duplicated(species_cty_rural_urban$SiteCode)
species_cty_rural_urban = subset(species_cty_rural_urban, !dup.site) ### removed the duplicated 60290014
species_cty_rural_urban$dup.site = NULL

# generate the US county boundary data
us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr[, 13] = NULL # there are two "state_name"
head(us_cty_bdr)
us_cty_bdr_geo = dplyr::select(us_cty_bdr, geoid, stusps, geometry)

##### 3. prepare logged data & generate train & test groups #####

# select datasets
# species_daily = csn_daily
# dataset = "CSN"

# species_daily = imp_daily
# dataset = "IMPROVE"

# log the species concentrations before clustering
species_log_daily = cbind(species_daily[, 1:4],
                          log(species_daily[, 5:(ncol(species_daily))]))
head(species_log_daily)

species_log_daily$group = sample(
  rep(1:5, 
      times = (nrow(species_log_daily) %/% 5 + 1)))[
        1:nrow(species_log_daily)]

test_group = 5
species_train = subset(species_log_daily, group != test_group)
species_test = subset(species_log_daily, group == test_group)
species_train$group = species_test$group = NULL
dim(species_log_daily)

##### 4.1 Site Level - prepare data for clustering on each site - TRAIN data #####

# get the mean value of all common PM species 
species_train_mean = 
  select(species_train, -State, -Date)  %>% 
  group_by(SiteCode) %>%
  summarise_all(# across(Al:Zn), # when using across, sometimes error can hardly be resolved
    mean, na.rm = T)
species_train_mean$Date = species_train_mean$Qualifier = species_train_mean$State = NULL

# double check if there are NAs in species_train_mean
which(is.na(species_train_mean), arr.ind=TRUE) # 

# add geoid (here equals to fips information here) for each site
species_train_mean = merge(species_train_mean, 
                           species_cty_rural_urban, 
                           all.x = T)
head(species_train_mean)

# add geometry information 
species_train_mean_bdr = merge(us_cty_bdr_geo, species_train_mean)
dim(species_train_mean_bdr)
colnames(species_train_mean_bdr)
colnames(species_train_mean_bdr)[1] = "fips"

species_train_tracer = species_train_mean_bdr
species_train_tracer$geometry.1 = species_train_tracer$geometry = NULL

# change rowname (SiteCode) to first column
rownames(species_train_tracer) <- species_train_tracer$SiteCode
dim(species_train_tracer)

##### 4.2 Site Level - prepare data for clustering on each site - TEST data #####

# get the mean value of all common PM species 
species_test_mean = 
  select(species_test, -State, -Date)  %>% 
  group_by(SiteCode) %>%
  summarise_all(# across(Al:Zn), # when using across, sometimes error can hardly be resolved
    mean, na.rm = T)
species_test_mean$Date = species_test_mean$Qualifier = species_test_mean$State = NULL

# double check if there are NAs in species_test_mean
which(is.na(species_test_mean), arr.ind=TRUE) # 

# add geoid (here equals to fips information here) for each site
species_test_mean = merge(species_test_mean, 
                          species_cty_rural_urban, 
                          all.x = T)
head(species_test_mean)

# add geometry information 
species_test_mean_bdr = merge(us_cty_bdr_geo, species_test_mean)
dim(species_test_mean_bdr)
colnames(species_test_mean_bdr)
colnames(species_test_mean_bdr)[1] = "fips"

species_test_tracer = species_test_mean_bdr
species_test_tracer$geometry.1 = species_test_tracer$geometry = NULL

# change rowname (SiteCode) to first column
rownames(species_test_tracer) <- species_test_tracer$SiteCode
dim(species_test_tracer)

##### 4.3 Site Level - cluster with random forest - GPS #####
# consider no gps info in the random forest model
species_log_rf_fit <- randomForest(x = species_train_tracer[, 4:ncol(species_train_tracer)], 
                                   y = NULL, 
                                   ntree = 10000, 
                                   proximity = TRUE, 
                                   oob.prox = TRUE)

species_log_rf_hclust <- hclust(as.dist(1-species_log_rf_fit$proximity), method = "ward.D2")

species_log_rf_hclust_tree = cutree(species_log_rf_hclust, k=25)

species_dat_rf = species_train_tracer
species_dat_rf$rf.clusters <- species_log_rf_hclust_tree
# table(species_log_rf_hclust_tree, iris$Species)
species_dat_rf$rf.clusters.fc = as.factor(species_dat_rf$rf.clusters)
data.frame(table(species_dat_rf$rf.clusters))


##### 4.4 Site Level - traing vs. testing result comparison - GPS #####

# Set a random seed
set.seed(51)

# Training using ‘random forest’ algorithm
library(caret)
species_rf = train(rf.clusters.fc ~ Al:Latitude,
                   data = species_dat_rf,
                   method = "rf",
                   trControl = trainControl(method = 'cv', # Use cross-validation
                                            number = 5)) # Use 5 folds for cross-validation

# species_rf = train(rf.clusters.fc ~ Al:RuralUrban,
#                    data = species_dat_rf,
#                    method = "rf") 

species_test_tracer$pred.clusters = 
  predict(species_rf, 
          newdata = species_test_tracer[, 4:ncol(species_test_tracer)])
species_test_tracer$pred.clusters.fc = as.factor(species_test_tracer$pred.clusters)
data.frame(table(species_test_tracer$pred.clusters.fc))

species_rf_train_testPred = dplyr::select(species_dat_rf, RuralUrban,
                                          fips, stusps, SiteCode, rf.clusters.fc)
species_rf_train_testPred$pred.clusters.fc = species_test_tracer$pred.clusters.fc
#write.csv(species_rf_train_testPred, "species_site_level_randomForest_20230111.csv")
# write.csv(species_rf_train_testPred, "species_site_level_randomForest_20230124_5.csv")
write.csv(species_rf_train_testPred, 
          paste0(dataset, "_gps_site_level_randomForest_20230124_", test_group, ".csv"))

# write.csv(species_rf_train_testPred, 
#           paste0(dataset, "_site_level_randomForest_20230124_", test_group, ".csv"))

# ##### 4.5 Site Level - propensity score - NO GPS #####
# # library(MatchIt) # could not use, matchit{} treatment must be a binary variable.
# species_match = matchit(rf.clusters.fc ~ Al:RuralUrban,
#                         data = species_dat_rf,
#                         method='nearest',
#                         distance = csn.ps)
# 
# # Training using ‘random forest’ algorithm
# species_rf = randomForest(rf.clusters.fc ~ Al:RuralUrban,
#                           data = species_dat_rf,
#                           trControl = trainControl(method = 'cv', # Use cross-validation
#                                                    number = 5)) # Use 5 folds for cross-validation
# 
# # use the fraction of votes as treatment probabilities, i.e., the propensity score
# species_dat_rf$csn.ps = species_rf$votes[, 2] 
# 
# ggplot(species_dat_rf, aes(rf.clusters.fc, csn.ps)) + 
#   geom_boxplot() +
#   geom_jitter() +
#   theme_light() +
#   xlab("species_Cluster_Random-Forest") +
#   ylab("Propensity Score")

# ##### 5.1 Daily & Site Level - prepare train & test data for clustering, take forever!!! #####
# 
# species_train_daily = species_train
# species_train_daily$Qualifier = species_train_daily$State = NULL
# 
# # double check if there are NAs in species_train_daily
# which(is.na(species_train_daily), arr.ind=TRUE) # 
# 
# # add geoid (here equals to fips information here) for each site
# species_train_daily$SiteCode = as.character(species_train_daily$SiteCode)
# species_train_daily = merge(species_train_daily, 
#                        species_cty_rural_urban, 
#                        all.x = T)
# head(species_train_daily)
# 
# species_train_daily = species_train_daily %>% relocate(geoid, .before = SiteCode)
# colnames(species_train_daily)[1] = "fips"
# 
# species_train_tra_daily = species_train_daily
# dim(species_train_tra_daily)
# 
# #### prepare test data with similar methods
# species_test_daily = species_test
# species_test_daily$Qualifier = species_test_daily$State = NULL
# species_test_daily$SiteCode = as.character(species_test_daily$SiteCode)
# 
# # double check if there are NAs in species_test_daily
# which(is.na(species_test_daily), arr.ind=TRUE) # 
# 
# # add geoid (here equals to fips information here) for each site
# species_test_daily = merge(species_test_daily, 
#                       species_cty_rural_urban, 
#                       all.x = T)
# head(species_test_daily)
# 
# species_test_daily = species_test_daily %>% relocate(geoid, .before = SiteCode)
# colnames(species_test_daily)[1] = "fips"
# 
# species_test_tra_daily = species_test_daily
# dim(species_test_tra_daily)
# 
# # temporarily remove influence from 20900035, which was not matched to any county
# species_train_tra_daily = na.omit(species_train_tra_daily)
# species_test_tra_daily = na.omit(species_test_tra_daily)

##### 5.2 convert daily to monthly #####
## not daily, cause always lead to error, vector memory exhausted (limit reached?)
## even after extending the memory via terminal
## change memory, https://stackoverflow.com/questions/51248293/error-vector-memory-exhausted-limit-reached-r-3-5-0-macos


### prepare training data
species_train_month = species_train
species_train_month$Qualifier = species_train_month$State = NULL

# get the Year-Month date 
species_train_month$yr.month <- format(
  as.Date(
    species_train_month$Date), 
  "%Y-%m")

# calculate the year-month average mean for all variables
species_train_month = species_train_month  %>% 
  group_by(SiteCode, yr.month) %>%
  summarise_all(# across(Al:Zn), # when using across, sometimes error can hardly be resolved
    mean, na.rm = T)

# double check if there are NAs in species_train_month
which(is.na(species_train_month), arr.ind=TRUE) # 

# add geoid (here equals to fips information here) for each site
species_train_month$SiteCode = as.character(species_train_month$SiteCode)
species_train_tra_month = merge(species_train_month, 
                                species_cty_rural_urban, 
                                all.x = T)
head(species_train_tra_month)

species_train_tra_month = species_train_tra_month %>% relocate(geoid, .before = SiteCode)
colnames(species_train_tra_month)[1] = "fips"
species_train_tra_month = na.omit(species_train_tra_month)
species_train_tra_month$Date = NULL
dim(species_train_tra_month)
colnames(species_train_tra_month)


### prepare testing data
species_test_month = species_test
species_test_month$Qualifier = species_test_month$State = NULL

species_test_month$yr.month <- format(
  as.Date(
    species_test_month$Date), 
  "%Y-%m")

species_test_month = species_test_month  %>% 
  group_by(SiteCode, yr.month) %>%
  summarise_all(# across(Al:Zn), # when using across, sometimes error can hardly be resolved
    mean, na.rm = T)

# double check if there are NAs in species_test_month
which(is.na(species_test_month), arr.ind=TRUE) # 

# add geoid (here equals to fips information here) for each site
species_test_month$SiteCode = as.character(species_test_month$SiteCode)
species_test_tra_month = merge(species_test_month, 
                               species_cty_rural_urban, 
                               all.x = T)
head(species_test_tra_month)

species_test_tra_month = species_test_tra_month %>% 
  relocate(geoid, .before = SiteCode)
colnames(species_test_tra_month)[1] = "fips"
species_test_tra_month = na.omit(species_test_tra_month)
species_test_tra_month$Date = NULL
dim(species_test_tra_month)
colnames(species_test_tra_month)


##### 5.3 Monthly & Site Level - cluster with random forest - GPS #####

# consider no gps info in the random forest model
species_month_rf_fit <- randomForest(x = species_train_tra_month[, ncol(species_train_tra_month)], 
                                     y = NULL, 
                                     ntree = 10000, 
                                     proximity = TRUE, 
                                     oob.prox = TRUE)

species_month_rf_hclust <- hclust(as.dist(1-species_month_rf_fit$proximity), method = "ward.D2")

species_month_rf_hclust_tree = cutree(species_month_rf_hclust, k=25)

species_month_rf = species_train_tra_month
species_month_rf$rf.clusters <- species_month_rf_hclust_tree
# table(species_log_rf_hclust_tree, iris$Species)
species_month_rf$rf.clusters.fc = as.factor(species_month_rf$rf.clusters)
data.frame(table(species_month_rf$SiteCode, species_month_rf$rf.clusters))

# Set a random seed
set.seed(51)

# Training using ‘random forest’ algorithm
library(caret)
species_rf = train(rf.clusters.fc ~ Al:RuralUrban,
                   data = species_month_rf,
                   method = "rf",
                   trControl = trainControl(method = 'cv', # Use cross-validation
                                            number = 5)) # Use 5 folds for cross-validation


species_test_tra_month$pred.clusters <- predict(species_rf, 
                                                newdata = species_test_tra_month[, 4:ncol(species_test_tra_month)])
species_test_tra_month$pred.clusters.fc = as.factor(species_test_tra_month$pred.clusters)
data.frame(table(species_test_tra_month$SiteCode, species_test_tra_month$pred.clusters.fc))
data.frame(table(species_test_tra_month$pred.clusters.fc))

species_month_train = dplyr::select(species_month_rf, RuralUrban, Longitude, Latitude,
                                    fips, SiteCode, yr.month, rf.clusters.fc)
species_month_Pred = dplyr::select(species_test_tra_month, 
                                   SiteCode, yr.month, pred.clusters.fc)
species_month_train_testPred = merge(species_month_train, species_month_Pred, all.x = T)
# write.csv(species_month_train_testPred, "species_site_Month_randomForest_20130111_result1.csv")
# write.csv(species_month_train_testPred, "species_site_Month_randomForest_20130111_result2.csv")
write.csv(species_month_train_testPred, 
          paste0(dataset, "_site_Month_randomForest_cluster_pred_", test_group, ".csv"))

sites = unique(species_month_train_testPred$SiteCode)
site_summary = NULL

for(k in 1:length(sites)){
  site.study = sites[k]
  site_cluster = subset(species_month_train_testPred, SiteCode == site.study)
  train.cluster = tail(names(sort(table(site_cluster$rf.clusters.fc))), 1)
  test.cluster = tail(names(sort(table(site_cluster$pred.clusters.fc))), 1)
  fips.study = site_cluster$fips[1]
  Longitude  = site_cluster$Longitude[1]
  Latitude = site_cluster$Latitude[1]
  
  site_info = data.frame(site.study, fips.study, Longitude, Latitude,
                         train.cluster, test.cluster)
  site_data = site_info[1, ]
  site_summary = rbind(site_summary, site_data)
}

UScounty <- map_data("county")
ggplot(site_summary, 
       aes(Longitude, Latitude, color= train.cluster)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = train.cluster), linetype="dashed") +
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()

ggplot(site_summary, 
       aes(Longitude, Latitude, color= test.cluster)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = test.cluster), linetype="dashed") +
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()

##### 6. summarize results from 5 training process #####
# detailed process are in file Bspecies_site_level_randomForest_20230124_1-5.xlsx
### which is combination of species_site_level_randomForest_20230124_1.csv to ~_5.csv
# species_rf_5train = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/species_IMPROVE_ownPC/Cluster_decision_BasedOn_species_site_level_randomForest_20230124_1-5.csv")

# merge results from 5 train datasets
rf_cluster_train_list = 
  list.files(pattern = "site_level_randomForest_.*\\.csv$", full.names = TRUE)
rf_cluster_train_list = 
  rf_cluster_train_list[
    (length(rf_cluster_train_list)-4):length(rf_cluster_train_list)]

rf_cluster_train = 
  do.call(
    rbind, 
    (lapply(
      rf_cluster_train_list, 
      read.csv)))
rf_cluster_train$X = NULL
rf_cluster_train$RF_cluster =
  rep(paste0("RFtrain_", 1:5), 
      each = nrow(rf_cluster_train)/5)

rf_cluster_train = 
  select(rf_cluster_train, SiteCode, rf.clusters.fc, RF_cluster)

# data for clustering summary
rf_cluster_sp = 
  rf_cluster_train %>%
  spread(RF_cluster, rf.clusters.fc)

rf_cluster_sp$RFtrain_2 = rf_cluster_sp$RFtrain_2 * 10^1
rf_cluster_sp$RFtrain_3 = rf_cluster_sp$RFtrain_3 * 10^2
rf_cluster_sp$RFtrain_4 = rf_cluster_sp$RFtrain_4 * 10^3
rf_cluster_sp$RFtrain_5 = rf_cluster_sp$RFtrain_5 * 10^4

# covert to factor in case of between-train-result influence
rf_cluster_sp[, 2:6] = 
  lapply(rf_cluster_sp[, 2:6], as.factor)
sapply(rf_cluster_sp, class)

# use matrix to store pair-wise co-occurrence counts 
co_occurrences = list()

for (col in paste0("RFtrain_", 1:5)) {
  tab = table(rf_cluster_sp$SiteCode, rf_cluster_sp[[col]])
  # 1 indicates group membership
  mat = as.matrix(ifelse(tab > 0, 1, 0))
  co_occurrences[[col]] = mat %*% t(mat)
}

# sum the co-occurrence matrices to get total co-occurrences
total_co_occurrences = Reduce(`+`, co_occurrences)
co_occ_df = 
  as.data.frame(
    as.table(total_co_occurrences))

# remove those never grouped together
co_occ_df = 
  subset(co_occ_df, 
         !(Var1 == Var2) & Freq > 0)
names(co_occ_df) = c("site1", "site2", "co_occur_freq")

# for the same pairs appearing in column site1 & 2, keep one only
co_occ_nodup = co_occ_df
co_occ_nodup$site1 = as.character(co_occ_nodup$site1)
co_occ_nodup$site2 = as.character(co_occ_nodup$site2)
co_occ_nodup =
  co_occ_nodup %>%
  mutate(siteA = ifelse(site1 < site2, site1, site2),
         siteB = ifelse(site1 < site2, site2, site1))
co_occ_nodup$dup.pair =
  duplicated(paste(co_occ_nodup$siteA, co_occ_nodup$siteB))
co_occ_nodup = 
  select(
    subset(co_occ_nodup, dup.pair == FALSE), 
    -dup.pair, -site1, -site2)

co_occ_nodup = 
  relocate(co_occ_nodup, 
           co_occur_freq, .after = siteB)

co_occ_nodup = 
  co_occ_nodup[with(
    co_occ_nodup, order(siteA, siteB)), ]

# those frequently grouped together, same cluster
co_occ_5 =
  subset(co_occ_nodup, co_occur_freq == 5)
co_occ_5_siteA =
  data.frame(table(co_occ_5$siteA))
names(co_occ_5_siteA)[1] = "siteA"
co_occ_5_siteA$sequence_5occ = 1:nrow(co_occ_5_siteA)

co_occ_5 = merge(co_occ_5, co_occ_5_siteA)

##### for the sites more frequently grouped together, use them to get the main cluster results
co_occ_5_more = subset(co_occ_5, Freq >=2)

# group the existing combinations accroding to the siteA
co_occ_5_more =
  co_occ_5_more %>%
  mutate(
    sequence_5occ_3more = 
      cumsum(siteA != lag(siteA, 
                          default = first(siteA))))
co_occ_5_more$sequence_5occ_3more = co_occ_5_more$sequence_5occ_3more + 1

co_occ_5_more$sequence_cluster = 1
co_occ_5_more$siteA.new = co_occ_5_more$siteA[1]
site.5.more = unique(co_occ_5_more$siteA)
site.count.5.more = length(site.5.more)

# check if some groups are to be combined, shared sites
for(site.check in site.5.more[2:length(site.5.more)]) {
  # first appearance in siteA
  site.first.C.row = 
    match(TRUE, co_occ_5_more$siteA == site.check)
  
  co_occ_5_more_bef = 
    subset(co_occ_5_more, siteA < site.check)
  
  # if assign a new sequence or not
  new.sequence = 
    ifelse(
      site.check %in% (co_occ_5_more_bef$siteB),
      co_occ_5_more_bef$sequence_cluster[
        match(TRUE, co_occ_5_more_bef$siteB == site.check)],
      max(co_occ_5_more_bef$sequence_cluster) +1 )
  new.siteA =
    ifelse(
      site.check %in% (co_occ_5_more_bef$siteB),
      co_occ_5_more_bef$siteA[
        match(TRUE, co_occ_5_more_bef$siteB == site.check)],
      site.check)
  
  co_occ_5_more$sequence_cluster[co_occ_5_more$siteA == site.check] = new.sequence
  co_occ_5_more$siteA.new[co_occ_5_more$siteA == site.check] = new.siteA
}

co_occ_5_cluster =
  select(co_occ_5_more, siteA.new, siteA, siteB, sequence_cluster)
co_occ_5_cluster =
  co_occ_5_cluster[
    with(co_occ_5_cluster, 
         order(sequence_cluster, siteA.new, siteB)), ]
co_occ_5_cluster$siteA = NULL

max(co_occ_5_more$sequence_cluster)

##### deal with sites not frequently grouped together
all_sites = unique(rf_cluster_train$SiteCode)
sites_in_5_cluster = unique(append(co_occ_5_more$siteA, co_occ_5_more$siteB))
summary(all_sites %in% sites_in_5_cluster)

sites_to_cluster =
  all_sites[!(all_sites %in% sites_in_5_cluster)]
# summary(sites_to_cluster %in% sites_in_5_cluster)

co_occ_4 = subset(co_occ_nodup, co_occur_freq == 4)
co_occ_3 = subset(co_occ_nodup, co_occur_freq == 3)
co_occ_2 = subset(co_occ_nodup, co_occur_freq == 2)
co_occ_1 = subset(co_occ_nodup, co_occur_freq == 1)
co_occ_5_1occ = subset(co_occ_5, Freq == 1)

left_site_cluster_sum = NULL
site_assign = NULL
lef_site_manual_sum = NULL

### 1. reassign the left sides based on other co-occurence situations
for(site.left in sites_to_cluster) {
  potential_cluster = NULL
  sites_1_coocc = sites_2_coocc = sites_3_coocc = sites_4_coocc = 
    sites_5_1occ = NULL
  
  co_occ_5_1occ_site =
    subset(co_occ_5_1occ, 
           siteA == site.left | siteB == site.left)
  co_occ_4_site =
    subset(co_occ_4, 
           siteA == site.left | siteB == site.left)
  co_occ_3_site =
    subset(co_occ_3, 
           siteA == site.left | siteB == site.left)
  co_occ_2_site =
    subset(co_occ_2, 
           siteA == site.left | siteB == site.left)
  co_occ_1_site =
    subset(co_occ_1, 
           siteA == site.left | siteB == site.left)
  
  # check if they appear in other co-occurence
  if(nrow(co_occ_5_1occ_site) > 0) {
    sites_5_1occ = 
      unique(append(co_occ_5_1occ_site$siteA, co_occ_5_1occ_site$siteB))
    
    if(sum(sites_5_1occ %in% sites_in_5_cluster) > 0){
      left_comb_in_5cluster = 
        sites_5_1occ[sites_5_1occ %in% sites_in_5_cluster]
      potential_cluster =
        co_occ_5_cluster$sequence_cluster[
          co_occ_5_cluster$siteA.new %in% left_comb_in_5cluster |
            co_occ_5_cluster$siteB %in% left_comb_in_5cluster]
      potential_cluster = unique(potential_cluster)
      
    }
  } 
  
  if(is.null(potential_cluster) & nrow(co_occ_4_site) > 0) {
    sites_4_coocc =
      unique(append(co_occ_4_site$siteA, co_occ_4_site$siteB))
    
    if(sum(sites_4_coocc %in% sites_in_5_cluster) > 0){
      left_comb_in_5cluster = 
        sites_4_coocc[sites_4_coocc %in% sites_in_5_cluster]
      potential_cluster =
        co_occ_5_cluster$sequence_cluster[
          co_occ_5_cluster$siteA.new %in% left_comb_in_5cluster |
            co_occ_5_cluster$siteB %in% left_comb_in_5cluster]
      potential_cluster = unique(potential_cluster)
      
    }
  }
  
  
  if(is.null(potential_cluster) & nrow(co_occ_3_site) > 0) {
    sites_3_coocc =
      unique(append(co_occ_3_site$siteA, co_occ_3_site$siteB))
    
    if(sum(sites_3_coocc %in% sites_in_5_cluster) > 0){
      left_comb_in_5cluster = 
        sites_3_coocc[sites_3_coocc %in% sites_in_5_cluster]
      potential_cluster =
        co_occ_5_cluster$sequence_cluster[
          co_occ_5_cluster$siteA.new %in% left_comb_in_5cluster |
            co_occ_5_cluster$siteB %in% left_comb_in_5cluster]
      potential_cluster = unique(potential_cluster)
    }
  }
  
  if(is.null(potential_cluster) & nrow(co_occ_2_site) > 0) {
    sites_2_coocc =
      unique(append(co_occ_2_site$siteA, co_occ_2_site$siteB))
    
    if(sum(sites_2_coocc %in% sites_in_5_cluster) > 0){
      left_comb_in_5cluster = 
        sites_2_coocc[sites_2_coocc %in% sites_in_5_cluster]
      potential_cluster =
        co_occ_5_cluster$sequence_cluster[
          co_occ_5_cluster$siteA.new %in% left_comb_in_5cluster |
            co_occ_5_cluster$siteB %in% left_comb_in_5cluster]
      potential_cluster = unique(potential_cluster)
    }
  }
  
  if(is.null(potential_cluster) & nrow(co_occ_1_site) > 0) {
    sites_1_coocc =
      unique(append(co_occ_1_site$siteA, co_occ_1_site$siteB))
    
    if(sum(sites_1_coocc %in% sites_in_5_cluster) > 0){
      left_comb_in_5cluster = 
        sites_1_coocc[sites_1_coocc %in% sites_in_5_cluster]
      potential_cluster =
        co_occ_5_cluster$sequence_cluster[
          co_occ_5_cluster$siteA.new %in% left_comb_in_5cluster |
            co_occ_5_cluster$siteB %in% left_comb_in_5cluster]
      potential_cluster = unique(potential_cluster)
    }
  }
  
  # if in co-occurence, reassign, if not, check self-combination
  if(is.null(potential_cluster)){
    site_assign = append(site_assign, site.left)
    potential_comb = 
      unique(
        unlist(
          Filter(Negate(is.null), 
                 list(sites_1_coocc, sites_2_coocc, sites_3_coocc, 
                      sites_4_coocc, sites_5_1occ))))
    lef_site_manual =
      data.frame(SiteCode = site.left, potential.combine = potential_comb)
    lef_site_manual_sum = rbind(lef_site_manual_sum, lef_site_manual)
    
  } else{
    left_site_cluster = data.frame(
      SiteCode = site.left, potential.cluster = potential_cluster)
    left_site_cluster_sum = rbind(left_site_cluster_sum, left_site_cluster)
  }
}


### 2. reassign the left-in-left sides based on their own combinations
names(lef_site_manual_sum) = c("siteC", "siteD")
lef_site_manual_reassign = lef_site_manual_sum

# assign based on siteC
lef_site_manual_reassign =
  lef_site_manual_reassign %>%
  mutate(
    sequence_current = 
      cumsum(siteC != lag(siteC, 
                          default = first(siteC))))
lef_site_manual_reassign$sequence_current = lef_site_manual_reassign$sequence_current + 1

lef_site_manual_reassign$sequence_cluster = max(co_occ_5_more$sequence_cluster) + 1
lef_site_manual_reassign$siteC.new = lef_site_manual_reassign$siteC[1]

site_left_assign = unique(lef_site_manual_reassign$siteC)

for(site.left.left in site_left_assign[2:length(site_left_assign)]) {
  # first appearance in siteC
  site.first.C.row = 
    match(TRUE, lef_site_manual_reassign$siteC == site.left.left)
  
  lef_site_manual_reassign_bef = 
    subset(lef_site_manual_reassign, siteC < site.left.left)
  
  # if assign a new sequence or not
  new.sequence = 
    ifelse(
      site.left.left %in% (lef_site_manual_reassign_bef$siteD),
      lef_site_manual_reassign_bef$sequence_cluster[
        match(TRUE, lef_site_manual_reassign_bef$siteD == site.left.left)],
      max(lef_site_manual_reassign_bef$sequence_cluster) +1 )
  new.siteC =
    ifelse(
      site.left.left %in% (lef_site_manual_reassign_bef$siteD),
      lef_site_manual_reassign_bef$siteC[
        match(TRUE, lef_site_manual_reassign_bef$siteD == site.left.left)],
      site.left.left)
  
  lef_site_manual_reassign$sequence_cluster[lef_site_manual_reassign$siteC == site.left.left] = new.sequence
  lef_site_manual_reassign$siteC.new[lef_site_manual_reassign$siteC == site.left.left] = new.siteC
}

lef_site_manual_reassign =
  select(lef_site_manual_reassign, siteC.new, siteC, siteD, sequence_cluster)
lef_site_manual_reassign =
  lef_site_manual_reassign[
    with(lef_site_manual_reassign, 
         order(sequence_cluster, siteC.new, siteD)), ]

lef_site_manual_reassign$dup = 
  duplicated(
    paste(lef_site_manual_reassign$siteC.new, lef_site_manual_reassign$siteD))
lef_site_manual_reassign = subset(lef_site_manual_reassign,
                                  dup == FALSE &
                                    siteC.new != siteD)
lef_site_manual_reassign$siteC = lef_site_manual_reassign$dup = NULL

##### FINAL cluster results
names(lef_site_manual_reassign) = names(co_occ_5_cluster)
site_cluster = rbind(co_occ_5_cluster, lef_site_manual_reassign)
site_cluster$dup = duplicated(paste(site_cluster$siteA.new, site_cluster$siteB))
site_cluster = subset(site_cluster, dup == FALSE)
site_cluster$dup = NULL

write.csv(site_cluster, "species_Cluster_RF.csv")


# extract & assign GPS info
species_site_gps = dplyr::select(species_train_mean_bdr, fips, SiteCode, Longitude, Latitude)
species_rf_5train = merge(site_cluster, species_site_gps)

UScounty <- map_data("county")
ggplot(species_rf_5train, 
       aes(Longitude, Latitude, color= as.factor(Final.Decision))) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = as.factor(Final.Decision)), linetype="dashed") +
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()


