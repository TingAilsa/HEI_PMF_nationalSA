##clear environment
# rm(list=ls())

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp")
getwd()
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp"

##packages in need
library(tidyr) # separate{tidyr}, gather{tidyr}, spread{tidyr},  spread is VIP function, str_split_fixed{stringr} is better than separate
library(stats) # aggregate{stats}, VIP function
library(ggplot2)
library(scales) # percent{}
library(dplyr)

library(randomForest)

library(ClustGeo)
library(sf)
library(sp)
# library(spdep)
library(ggsci)
library(data.table)

library(usmap)
library(USAboundaries)


#############################################################
#### interpolation for IMPROVE data only ####
#############################################################
# changed! # cty_rural_urban = fread("/Users/ztttttt/Dropbox/HEI_US_PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban = fread("/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$V1 = NULL

# imp_daily = read.csv("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/IMPROVE_Component_with_missing.csv")
imp_daily = fread("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/IMPROVE_interpulation_random-forest_afterLog.csv")
imp_daily = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/IMPROVE_interpulation_random-forest_afterLog.csv")
imp_daily = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_data_prepare/IMPROVE_interpulation_random-forest_2023.csv")
imp_daily$V1 = NULL
imp_daily$Date = as.Date(imp_daily$Date)
sapply(imp_daily, class)
head(imp_daily)

imp_cty_rural_urban = subset(cty_rural_urban, Dataset == "IMPAER")
# imp_cty_rural_urban = select(imp_cty_rural_urban, SiteCode, RuralUrban, geoid, Longitude, Latitude)
imp_cty_rural_urban = imp_cty_rural_urban[ , 
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

### exclude PM species not to be used for PMF or have too many negative values in original data
imp_tracer = imp_daily
# imp_tracer$OC1 = imp_tracer$OC2 = imp_tracer$OC3 = imp_tracer$OC4 = # fail to meet QA/QC in CSN
  # imp_tracer$EC1 = imp_tracer$EC2 = imp_tracer$EC3 = # fail to meet QA/QC in CSN
  # imp_tracer$ammNO3 = imp_tracer$ammSO4 = # recalculated species
  # imp_tracer$NO2. = imp_tracer$Cl. = # only existed in IMPROVE dataset
  imp_tracer$Rb = imp_tracer$Zr = # imp_tracer$Se = 
  imp_tracer$PM25 = NULL # not PM species
sapply(imp_tracer, class)

# log the species concentrations before clustering
imp_tracer = cbind(imp_tracer[, 1:4],
                   log(imp_tracer[, 5:(ncol(imp_tracer))]))
head(imp_tracer)

imp_tracer$year = year(imp_tracer$Date)
imp_tracer$month = month(imp_tracer$Date)

##### mean of SITE level of Whole study period
imp_tracer_mean = imp_tracer %>% 
  group_by(SiteCode) %>%
  summarise_all(# across(Al:Zn), # when using across, sometimes error can hardly be resolved
                mean, na.rm = T)
imp_tracer_mean$Date = imp_tracer_mean$Qualifier = imp_tracer_mean$State = 
  imp_tracer_mean$year = imp_tracer_mean$month = NULL

##### mean of SITE level of montly average
imp_tracer_mean = imp_tracer %>% 
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

##### CAN JUMP THIS PART: find and match geoid for sites missing such info #####
unique(imp_tracer_gps$geoid[!(imp_tracer_gps$geoid %in% 
                                us_cty_bdr_geo$geoid)])
unique(imp_tracer_gps$geoid[imp_tracer_gps$geoid %in% 
                              us_cty_bdr_geo$geoid])

# extract site info for those missing fips/geoid
cty_rural_urban_NA_geoid = subset(cty_rural_urban,
                                  SiteCode %in% 
                                    unique(imp_tracer_gps_NA_geoid$SiteCode))

imp_tracer_gps_NA_geoid = subset(imp_tracer_gps, 
                                  is.na(geoid) & 
                                    (!is.na(Longitude)))
imp_tracer_NA_geoid_site = dplyr::select(
  imp_tracer_gps_NA_geoid, 
  SiteCode, Longitude, Latitude)

# detect the sites in US county boundary data for that missing fips/geoid
imp_tracer_NA_geoid_site_sf <- st_as_sf(imp_tracer_NA_geoid_site, 
                                        coords = c("Longitude", "Latitude"), 
                                        crs = 4326) #crs = 4326 refers to WGS84.
nearest_index <- st_nearest_feature(imp_tracer_NA_geoid_site_sf, us_cty_bdr)

# extract the geoid from us_cty_bdr 
imp_tracer_NA_geoid_site$nearest_geoid <- us_cty_bdr$geoid[nearest_index]
us_cty_bdr$namelsad[nearest_index] 
# Keweenaw County, geoid 26083, is MI's least populous county, ~2100 population 

# complete the geoid info
imp_tracer_gps <- 
  left_join(imp_tracer_gps, 
            dplyr::select(imp_tracer_NA_geoid_site,
                          SiteCode, nearest_geoid), 
            by = "SiteCode") %>%
  mutate(geoid = coalesce(geoid, 
                          nearest_geoid)) %>%
  dplyr::select(-nearest_geoid) # remove the column

imp_tracer_gps$RuralUrban[imp_tracer_gps$SiteCode == "ISLE1"] = "Rural"
imp_tracer_gps = subset(imp_tracer_gps, 
                         !(is.na(geoid)))
#### CAN JUMP THIS PART, end #####

######## IMPROVE - Cluster based on Site-average #########

# add geometry information 
imp_tracer_gps_bdr = merge(us_cty_bdr_geo, 
                           imp_tracer_gps)

colnames(imp_tracer_gps_bdr)
colnames(imp_tracer_gps_bdr)[1] = "fips"

imp_dat_tra = imp_tracer_gps_bdr
imp_dat_tra$geometry.1 = imp_dat_tra$geometry = imp_dat_tra$SideCode = NULL
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

### specially for results based on monthly data, assign only one cluster to each site
# if the site appears most frequently in given cluster, then define the result
imp_rf_cluster_month <- 
  imp_rf_cluster %>%
  mutate(rf.clusters = as.factor(rf.clusters)) %>%
  select(SiteCode, rf.clusters) %>%
  group_by(SiteCode, rf.clusters) %>%
  summarise(n = n(), .groups = 'drop') 

imp_rf_cluster_month_final <- 
  imp_rf_cluster_month %>%
  group_by(SiteCode) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

plot_usmap(data = imp_rf_cluster, values = "rf.clusters.fc", labels = F, 
           label_color = "white", regions = "counties") + 
  theme(panel.background=element_blank()) +
  scale_fill_manual(values = npg.color[1:20])

# MainStates <- map_data("state")
UScounty <- map_data("county")
ggplot(imp_rf_cluster, 
       aes(Longitude, Latitude, color= rf.clusters.fc)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = rf.clusters.fc), linetype="dashed") +
  scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()

#############################################################
#### Interpolation for IMPROVE & CSN together - till 2023.11 ####
#############################################################
##### DONE, 0-1. import and match IMPROVE & CSN - DONE#####
# imp_daily = read.csv("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/IMPROVE_Component_with_missing.csv")
imp_daily = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/IMPROVE_interpulation_random-forest_afterLog.csv")
imp_daily$X = imp_daily$X.1 = NULL
imp_daily$Date = as.Date(imp_daily$Date)
sapply(imp_daily, class)
head(imp_daily)
length(unique(imp_daily$SiteCode))

# import CSN
csn_daily_before = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_interpulation_random-forest_afterLog_before_2015.csv")
csn_daily_after = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_interpulation_random-forest_afterLog_after_2015.csv")
csn_daily_before$X = csn_daily_after$X = NULL
csn_daily_before$Date = as.Date(csn_daily_before$Date)
csn_daily_after$Date = as.Date(csn_daily_after$Date)


# remove items not to be used
imp_remove = c("ammNO3", "ammSO4", 
               "RC.PM2.5", "PM2.5", "NO2.", 
               "Rb", "Zr", "Cl.") # high NAs or not exits in a large part of CSN 

csn_bfr_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC.unadjusted.88", "EC.unadjusted.88", "OPC.unadjusted.88",
                   "Accept.PM2.5",
                   "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
                   "K.", "Na.", "NH4.", "Sb", "Sn", 
                   "Rb", "Zr")

csn_aft_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC1.88", "OC2.88", "OC3.88", "OC4.88",
                   "EC1.88", "EC2.88", "EC3.88", 
                   "EC1", "EC2", "EC3", 
                   "EC.unadjusted.88", "EC.TOR.unadjust.88", "EC.88",
                   "OC.unadjusted.88", "OC.TOR.unadjusted.88", "OC", 
                   "OPC.88", "OP.TOR.unadjusted.88", "OPC.unadjusted.88",
                   "PM2.5RC",
                   "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
                   "K.", "Na.", "NH4.", "Sb", "Sn", 
                   "Rb", "Zr", "Cl.")

imp_daily[ ,imp_remove] <- list(NULL)
csn_daily_before[ ,csn_bfr_remove] <- list(NULL)
csn_daily_after[ ,csn_aft_remove] <- list(NULL)

colnames(csn_daily_before)[c(12, 20, 21)] = c("EC", "OC", "OP")
colnames(csn_daily_after)[c(12, 20, 21)] = c("EC", "OC", "OP")

# change the order to columns in IMPROVE to be same as CSN
imp_daily <- imp_daily[, c(2, 4, 1, 3, 5:ncol(imp_daily))]

summary(unique(colnames(imp_daily)) == 
          unique(colnames(csn_daily_before)))
summary(unique(colnames(imp_daily)) == 
          unique(colnames(csn_daily_after)))


setDT(imp_daily)
setDT(csn_daily_before)
setDT(csn_daily_after)

imp_csn_daily = rbind(imp_daily, csn_daily_before, csn_daily_after)
# write.csv(imp_csn_daily, "IMPROVE_CSN_joint_PM_species.csv")
length(unique(imp_daily$SiteCode))
length(unique(imp_csn_daily$SiteCode))

##### 1. import and match IMPROVE & CSN - DONE#####
imp_csn_daily = read.csv("IMPROVE_CSN_joint_PM_species.csv")
imp_csn_daily$X = NULL
imp_csn_daily$Date = as.Date(imp_csn_daily$Date)
setDT(imp_csn_daily)

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
cty_rural_urban = read.csv("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$X.1 = NULL

imp_csn_cty_rural_urban = cty_rural_urban
imp_csn_cty_rural_urban = select(imp_csn_cty_rural_urban, SiteCode, RuralUrban, geoid, Longitude, Latitude)
imp_csn_cty_rural_urban$geoid = ifelse(imp_csn_cty_rural_urban$geoid < 10000, 
                                   paste0("0", imp_csn_cty_rural_urban$geoid), 
                                   imp_csn_cty_rural_urban$geoid)
cty_rural_urban_count = data.frame(table(cty_rural_urban$RuralUrban))
colnames(cty_rural_urban_count)[1] = "Rural Urban Classification"
# 33 mix, 189 rural, 106 urban 

imp_csn_cty_rural_urban$dup.site = duplicated(imp_csn_cty_rural_urban$SiteCode)
imp_csn_cty_rural_urban = subset(imp_csn_cty_rural_urban, !dup.site) ### removed the duplicated 60290014！！！！！
## why duplicated?!!!!!
write.csv(imp_csn_cty_rural_urban, "IMPROVE_CSN_Urban_Rural_geoid_330sites.csv")

imp_csn_dat_tra$dup.site = NULL


##### 3. prepare logged data for clustering on each site #####
# log the species concentrations before clustering
imp_csn_tracer = cbind(imp_csn_daily[, 1:4],
                   log(imp_csn_daily[, 5:(ncol(imp_csn_daily))]))
head(imp_csn_tracer)

# get the mean value of all common PM species 
imp_csn_tracer_mean = imp_csn_tracer  %>% 
  group_by(SiteCode) %>%
  summarise_all(# across(Al:Zn), # when using across, sometimes error can hardly be resolved
    mean, na.rm = T)
imp_csn_tracer_mean$Date = imp_csn_tracer_mean$Qualifier = imp_csn_tracer_mean$State = NULL

# double check if there are NAs in imp_csn_tracer_mean
which(is.na(imp_csn_tracer_mean), arr.ind=TRUE) # 

# add geoid (here equals to fips information here) for each site
imp_csn_tracer_mean = merge(imp_csn_tracer_mean, 
                            imp_csn_cty_rural_urban, 
                            all.x = T)
head(imp_csn_tracer_mean)

# log the absolute values of longitudes and latitudes in case of considering geographic locations
imp_csn_tracer_mean$log.Long = log(abs(imp_csn_tracer_mean$Longitude))
imp_csn_tracer_mean$log.Lat = log(abs(imp_csn_tracer_mean$Latitude))

# generate the US county boundary data
us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr[, 13] = NULL # there are two "state_name"
head(us_cty_bdr)
us_cty_bdr_geo = dplyr::select(us_cty_bdr, geoid, stusps, geometry)

# add geometry information 
imp_csn_tracer_mean_bdr = merge(us_cty_bdr_geo, imp_csn_tracer_mean)
dim(imp_csn_tracer_mean_bdr)
colnames(imp_csn_tracer_mean_bdr)
colnames(imp_csn_tracer_mean_bdr)[1] = "fips"

imp_csn_tracer_mean_bdr$dup.site = duplicated(imp_csn_tracer_mean_bdr$SiteCode)
imp_csn_tracer_mean_bdr = subset(imp_csn_tracer_mean_bdr, !dup.site) ### removed the duplicated 60290014！！！！！
## why duplicated?!!!!!
imp_csn_tracer_mean_bdr$dup.site = NULL


imp_csn_dat_tra = imp_csn_tracer_mean_bdr
imp_csn_dat_tra$geometry.1 = imp_csn_dat_tra$geometry = NULL
imp_csn_dat_tra$Longitude = imp_csn_dat_tra$Latitude = NULL
# change rowname (SiteCode) to first column
imp_csn_dat_tra$dup.site = duplicated(imp_csn_dat_tra$SiteCode)
imp_csn_dat_tra = subset(imp_csn_dat_tra, !dup.site) ### removed the duplicated 60290014！！！！！
## why duplicated?!!!!!
imp_csn_dat_tra$dup.site = NULL
rownames(imp_csn_dat_tra) <- imp_csn_dat_tra$SiteCode

# add information of the fips and whether if a county is rural or urban
imp_csn_dat_tra$RuralUrban = imp_csn_tracer_mean_bdr$RuralUrban
imp_csn_dat_tra$fips = imp_csn_tracer_mean_bdr$fips
head(imp_csn_dat_tra)

##### 4. cluster with random forest - NO GPS #####
# consider no gps info in the random forest model
imp_csn_log_rf_fit <- randomForest(x = imp_csn_dat_tra[, 4:(ncol(imp_csn_dat_tra) - 2)], 
                                   y = NULL, 
                                   ntree = 10000, 
                                   proximity = TRUE, 
                                   oob.prox = TRUE)

#consider gps info in the random forest model
imp_csn_log_rf_gps_fit <- randomForest(x = imp_csn_dat_tra[, 4:ncol(imp_csn_dat_tra)], 
                                       y = NULL, 
                                       ntree = 10000, 
                                       proximity = TRUE, 
                                       oob.prox = TRUE)

imp_csn_log_rf_hclust <- hclust(as.dist(1-imp_csn_log_rf_fit$proximity), method = "ward.D2")

imp_csn_log_rf_hclust_tree = cutree(imp_csn_log_rf_hclust, k=40)

imp_csn_dat_rf = imp_csn_dat_tra
imp_csn_dat_rf$rf.clusters <- imp_csn_log_rf_hclust_tree
# table(imp_csn_log_rf_hclust_tree, iris$Species)
imp_csn_dat_rf$rf.clusters.fc = as.factor(imp_csn_dat_rf$rf.clusters)
data.frame(table(imp_csn_dat_rf$rf.clusters))

imp_csn_dat_rf$Longitude = -exp(imp_csn_dat_rf$log.Long)
imp_csn_dat_rf$Latitude = exp(imp_csn_dat_rf$log.Lat)


plot_usmap(data = imp_csn_dat_rf, values = "rf.clusters.fc", labels = F, 
           label_color = "white", regions = "counties") + 
  # scale_fill_manual(values = npg.color[1:20])+
  theme(panel.background=element_blank()) 

# MainStates <- map_data("state")
UScounty <- map_data("county")
ggplot(imp_csn_dat_rf, 
       aes(Longitude, Latitude, color= rf.clusters.fc)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = rf.clusters.fc), linetype="dashed") +
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()

ggplot(subset(imp_csn_dat_rf, rf.clusters == 19), # with gps, 18
       aes(Longitude, Latitude, color= rf.clusters.fc)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = rf.clusters.fc), linetype="dashed") +
  scale_color_manual(values = "forestgreen") +
  theme_linedraw()

##### 5. cluster with random forest - with GPS #####
#consider gps info in the random forest model
imp_csn_log_rf_gps_fit <- randomForest(x = imp_csn_dat_tra[, 4:ncol(imp_csn_dat_tra)], 
                                       y = NULL, 
                                       ntree = 10000, 
                                       proximity = TRUE, 
                                       oob.prox = TRUE)

rf_hclust_gps <- hclust(as.dist(1-imp_csn_log_rf_gps_fit$proximity), method = "ward.D2")

rf_hclust_gps_tree = cutree(rf_hclust_gps, k=40)

imp_csn_rf_hclust_gps = imp_csn_dat_tra
imp_csn_rf_hclust_gps$rf.clusters <- rf_hclust_gps_tree
# table(rf_hclust_gps_tree, iris$Species)
imp_csn_rf_hclust_gps$rf.clusters.fc = as.factor(imp_csn_rf_hclust_gps$rf.clusters)
data.frame(table(imp_csn_rf_hclust_gps$rf.clusters))

imp_csn_rf_hclust_gps$Longitude = -exp(imp_csn_rf_hclust_gps$log.Long)
imp_csn_rf_hclust_gps$Latitude = exp(imp_csn_rf_hclust_gps$log.Lat)

plot_usmap(data = imp_csn_rf_hclust_gps, values = "rf.clusters.fc", labels = F, 
           label_color = "white", regions = "counties") + 
  # scale_fill_manual(values = npg.color[1:20])+
  theme(panel.background=element_blank()) 

# MainStates <- map_data("state")
UScounty <- map_data("county")
ggplot(imp_csn_rf_hclust_gps, 
       aes(Longitude, Latitude, color= rf.clusters.fc)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = rf.clusters.fc), linetype="dashed") +
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()

ggplot(subset(imp_csn_rf_hclust_gps, rf.clusters == 18), 
       aes(Longitude, Latitude, color= rf.clusters.fc)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = rf.clusters.fc), linetype="dashed") +
  scale_color_manual(values = "forestgreen") +
  theme_linedraw()

#############################################################
#### Interpolation for CSN ####
#############################################################
##### 1. import and match CSN #####

# import CSN
csn_daily_before = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_interpulation_random-forest_afterLog_before_2015.csv")
csn_daily_after = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_interpulation_random-forest_afterLog_after_2015.csv")
csn_daily_before$X = csn_daily_after$X = NULL
csn_daily_before$Date = as.Date(csn_daily_before$Date)
csn_daily_after$Date = as.Date(csn_daily_after$Date)

csn_bfr_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC.unadjusted.88", "EC.unadjusted.88", "OPC.unadjusted.88",
                   "Accept.PM2.5",
                   "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
                   "K.", "Na.", "NH4.", "Sb", "Sn", 
                   "Rb", "Zr")

csn_aft_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC1.88", "OC2.88", "OC3.88", "OC4.88",
                   "EC1.88", "EC2.88", "EC3.88", 
                   "EC1", "EC2", "EC3", 
                   "EC.unadjusted.88", "EC.TOR.unadjust.88", "EC.88",
                   "OC.unadjusted.88", "OC.TOR.unadjusted.88", "OC", 
                   "OPC.88", "OP.TOR.unadjusted.88", "OPC.unadjusted.88",
                   "PM2.5RC",
                   "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
                   "K.", "Na.", "NH4.", "Sb", "Sn", 
                   "Rb", "Zr", "Cl.")

csn_daily_before[ ,csn_bfr_remove] <- list(NULL)
csn_daily_after[ ,csn_aft_remove] <- list(NULL)

colnames(csn_daily_before)[c(12, 20, 21)] = c("EC", "OC", "OP")
colnames(csn_daily_after)[c(12, 20, 21)] = c("EC", "OC", "OP")

setDT(csn_daily_before)
setDT(csn_daily_after)

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
cty_rural_urban = read.csv("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$X.1 = NULL

csn_cty_rural_urban = cty_rural_urban
csn_cty_rural_urban = select(csn_cty_rural_urban, SiteCode, RuralUrban, geoid, Longitude, Latitude)
csn_cty_rural_urban$geoid = ifelse(csn_cty_rural_urban$geoid < 10000, 
                                       paste0("0", csn_cty_rural_urban$geoid), 
                                       csn_cty_rural_urban$geoid)
cty_rural_urban_count = data.frame(table(cty_rural_urban$RuralUrban))
colnames(cty_rural_urban_count)[1] = "Rural Urban Classification"
# 33 mix, 189 rural, 106 urban 

csn_cty_rural_urban$dup.site = duplicated(csn_cty_rural_urban$SiteCode)
csn_cty_rural_urban = subset(csn_cty_rural_urban, !dup.site) ### removed the duplicated 60290014！！！！！
## why duplicated?!!!!!
csn_cty_rural_urban$dup.site = NULL


##### 3. prepare logged data for clustering on each site #####
# log the species concentrations before clustering
csn_tracer = cbind(csn_daily[, 1:4],
                       log(csn_daily[, 5:(ncol(csn_daily))]))
head(csn_tracer)

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

csn_tracer_mean_bdr$dup.site = duplicated(csn_tracer_mean_bdr$SiteCode)
csn_tracer_mean_bdr = subset(csn_tracer_mean_bdr, !dup.site) ### removed the duplicated 60290014！！！！！
## why duplicated?!!!!!
csn_tracer_mean_bdr$dup.site = NULL


csn_dat_tra = csn_tracer_mean_bdr
csn_dat_tra$geometry.1 = csn_dat_tra$geometry = NULL
# csn_dat_tra$Longitude = csn_dat_tra$Latitude = NULL

# change rowname (SiteCode) to first column
csn_dat_tra$dup.site = duplicated(csn_dat_tra$SiteCode)
csn_dat_tra = subset(csn_dat_tra, !dup.site) ### removed the duplicated 60290014！！！！！
## why duplicated?!!!!!
csn_dat_tra$dup.site = NULL
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
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()

ggplot(subset(csn_rf_hclust_gps, rf.clusters == 18), 
       aes(Longitude, Latitude, color= rf.clusters.fc)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = rf.clusters.fc), linetype="dashed") +
  scale_color_manual(values = "forestgreen") +
  theme_linedraw()

#############################################################
#### Interpolation for CSN - trainging & testing ####
#############################################################
##### 1. import and match CSN #####

# import CSN
csn_daily_before = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/CSN_Interpolation/CSN_interpulation_random-forest_afterLog_before_2015.csv")
csn_daily_after = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/CSN_Interpolation/CSN_interpulation_random-forest_afterLog_after_2015.csv")

csn_daily_before$X = csn_daily_after$X = NULL
csn_daily_before$Date = as.Date(csn_daily_before$Date)
csn_daily_after$Date = as.Date(csn_daily_after$Date)

csn_bfr_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC.unadjusted.88", "EC.unadjusted.88", "OPC.unadjusted.88",
                   "Accept.PM2.5",
                   "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
                   "K.", "Na.", "NH4.", "Sb", "Sn", 
                   "Rb", "Zr")

csn_aft_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC1.88", "OC2.88", "OC3.88", "OC4.88",
                   "EC1.88", "EC2.88", "EC3.88", 
                   "EC1", "EC2", "EC3", 
                   "EC.unadjusted.88", "EC.TOR.unadjust.88", "EC.88",
                   "OC.unadjusted.88", "OC.TOR.unadjusted.88", "OC", 
                   "OPC.88", "OP.TOR.unadjusted.88", "OPC.unadjusted.88",
                   "PM2.5RC",
                   "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
                   "K.", "Na.", "NH4.", "Sb", "Sn", 
                   "Rb", "Zr", "Cl.")

csn_daily_before[ ,csn_bfr_remove] <- list(NULL)
csn_daily_after[ ,csn_aft_remove] <- list(NULL)

colnames(csn_daily_before)[c(12, 20, 21)] = c("EC", "OC", "OP")
colnames(csn_daily_after)[c(12, 20, 21)] = c("EC", "OC", "OP")

setDT(csn_daily_before)
setDT(csn_daily_after)

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
cty_rural_urban = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$X.1 = NULL

csn_cty_rural_urban = cty_rural_urban
csn_cty_rural_urban = select(csn_cty_rural_urban, SiteCode, RuralUrban, geoid, Longitude, Latitude)
csn_cty_rural_urban$geoid = ifelse(csn_cty_rural_urban$geoid < 10000, 
                                   paste0("0", csn_cty_rural_urban$geoid), 
                                   csn_cty_rural_urban$geoid)
cty_rural_urban_count = data.frame(table(cty_rural_urban$RuralUrban))
colnames(cty_rural_urban_count)[1] = "Rural Urban Classification"
# 33 mix, 189 rural, 106 urban 

csn_cty_rural_urban$dup.site = duplicated(csn_cty_rural_urban$SiteCode)
csn_cty_rural_urban = subset(csn_cty_rural_urban, !dup.site) ### removed the duplicated 60290014！！！！！
## why duplicated?!!!!!
csn_cty_rural_urban$dup.site = NULL


##### 3. prepare logged data & generate train & test groups #####
# log the species concentrations before clustering
csn_log_daily = cbind(csn_daily[, 1:4],
                   log(csn_daily[, 5:(ncol(csn_daily))]))
head(csn_log_daily)

csn_log_daily$group = sample(
  rep(1:5, 
      times = (nrow(csn_log_daily) %/% 5 + 1)))[
        1:nrow(csn_log_daily)]


csn_train = subset(csn_log_daily, group != 5)
csn_test = subset(csn_log_daily, group == 5)
csn_train$group = csn_test$group = NULL
dim(csn_log_daily)

##### 4.1 Site Level - prepare data for clustering on each site - TRAIN data #####

# get the mean value of all common PM species 
csn_train_mean = csn_train  %>% 
  group_by(SiteCode) %>%
  summarise_all(# across(Al:Zn), # when using across, sometimes error can hardly be resolved
    mean, na.rm = T)
csn_train_mean$Date = csn_train_mean$Qualifier = csn_train_mean$State = NULL

# double check if there are NAs in csn_train_mean
which(is.na(csn_train_mean), arr.ind=TRUE) # 

# add geoid (here equals to fips information here) for each site
csn_train_mean = merge(csn_train_mean, 
                        csn_cty_rural_urban, 
                        all.x = T)
head(csn_train_mean)
# site 20900035 has no geoid???

# generate the US county boundary data
us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr[, 13] = NULL # there are two "state_name"
head(us_cty_bdr)
us_cty_bdr_geo = dplyr::select(us_cty_bdr, geoid, stusps, geometry)

# add geometry information 
csn_train_mean_bdr = merge(us_cty_bdr_geo, csn_train_mean)
dim(csn_train_mean_bdr)
colnames(csn_train_mean_bdr)
colnames(csn_train_mean_bdr)[1] = "fips"

csn_train_mean_bdr$dup.site = duplicated(csn_train_mean_bdr$SiteCode)
csn_train_mean_bdr = subset(csn_train_mean_bdr, !dup.site) ### removed the duplicated 60290014！！！！！
## why duplicated?!!!!!
csn_train_mean_bdr$dup.site = NULL


csn_train_tracer = csn_train_mean_bdr
csn_train_tracer$geometry.1 = csn_train_tracer$geometry = NULL

# change rowname (SiteCode) to first column
rownames(csn_train_tracer) <- csn_train_tracer$SiteCode
dim(csn_train_tracer)

##### 4.2 Site Level - prepare data for clustering on each site - TEST data #####

# get the mean value of all common PM species 
csn_test_mean = csn_test  %>% 
  group_by(SiteCode) %>%
  summarise_all(# across(Al:Zn), # when using across, sometimes error can hardly be resolved
    mean, na.rm = T)
csn_test_mean$Date = csn_test_mean$Qualifier = csn_test_mean$State = NULL

# double check if there are NAs in csn_test_mean
which(is.na(csn_test_mean), arr.ind=TRUE) # 

# add geoid (here equals to fips information here) for each site
csn_test_mean = merge(csn_test_mean, 
                       csn_cty_rural_urban, 
                       all.x = T)
head(csn_test_mean)
# site 20900035 has no geoid???

# add geometry information 
csn_test_mean_bdr = merge(us_cty_bdr_geo, csn_test_mean)
dim(csn_test_mean_bdr)
colnames(csn_test_mean_bdr)
colnames(csn_test_mean_bdr)[1] = "fips"

csn_test_mean_bdr$dup.site = duplicated(csn_test_mean_bdr$SiteCode)
csn_test_mean_bdr = subset(csn_test_mean_bdr, !dup.site) ### removed the duplicated 60290014！！！！！
## why duplicated?!!!!!
csn_test_mean_bdr$dup.site = NULL

csn_test_tracer = csn_test_mean_bdr
csn_test_tracer$geometry.1 = csn_test_tracer$geometry = NULL

# change rowname (SiteCode) to first column
rownames(csn_test_tracer) <- csn_test_tracer$SiteCode
dim(csn_test_tracer)

##### 4.3 Site Level - cluster with random forest - NO GPS #####
# consider no gps info in the random forest model
csn_log_rf_fit <- randomForest(x = csn_train_tracer[, 4:(ncol(csn_train_tracer) - 2)], 
                               y = NULL, 
                               ntree = 10000, 
                               proximity = TRUE, 
                               oob.prox = TRUE)

csn_log_rf_hclust <- hclust(as.dist(1-csn_log_rf_fit$proximity), method = "ward.D2")

csn_log_rf_hclust_tree = cutree(csn_log_rf_hclust, k=25)

csn_dat_rf = csn_train_tracer
csn_dat_rf$rf.clusters <- csn_log_rf_hclust_tree
# table(csn_log_rf_hclust_tree, iris$Species)
csn_dat_rf$rf.clusters.fc = as.factor(csn_dat_rf$rf.clusters)
data.frame(table(csn_dat_rf$rf.clusters))


##### 4.4 Site Level - traing vs. testing result comparison - NO GPS #####

# Set a random seed
set.seed(51)

# Training using ‘random forest’ algorithm
library(caret)
csn_rf = train(rf.clusters.fc ~ Al:RuralUrban,
               data = csn_dat_rf,
               method = "rf",
               trControl = trainControl(method = 'cv', # Use cross-validation
                                        number = 5)) # Use 5 folds for cross-validation

csn_rf = train(rf.clusters.fc ~ Al:RuralUrban,
               data = csn_dat_rf,
               method = "rf") 

csn_test_tracer$pred.clusters <- predict(csn_rf, 
                                         newdata = csn_test_tracer[, 4:(ncol(csn_test_tracer) - 2)])
csn_test_tracer$pred.clusters.fc = as.factor(csn_test_tracer$pred.clusters)
data.frame(table(csn_test_tracer$pred.clusters.fc))

csn_rf_train_testPred = dplyr::select(csn_dat_rf, RuralUrban,
                                      fips, stusps, SiteCode, rf.clusters.fc)
csn_rf_train_testPred$pred.clusters.fc = csn_test_tracer$pred.clusters.fc
#write.csv(csn_rf_train_testPred, "CSN_site_level_randomForest_20230111.csv")
write.csv(csn_rf_train_testPred, "CSN_site_level_randomForest_20230124_5.csv")

##### 4.5 Site Level - propensity score - NO GPS #####
# library(MatchIt) # could not use, matchit{} treatment must be a binary variable.
csn_match = matchit(rf.clusters.fc ~ Al:RuralUrban,
                    data = csn_dat_rf,
                    method='nearest',
                    distance = csn.ps
                    )

# Training using ‘random forest’ algorithm
csn_rf = randomForest(rf.clusters.fc ~ Al:RuralUrban,
                      data = csn_dat_rf,
                      trControl = trainControl(method = 'cv', # Use cross-validation
                                               number = 5)) # Use 5 folds for cross-validation

# use the fraction of votes as treatment probabilities, i.e., the propensity score
csn_dat_rf$csn.ps = csn_rf$votes[, 2] 

ggplot(csn_dat_rf, aes(rf.clusters.fc, csn.ps)) + 
  geom_boxplot() +
  geom_jitter() +
  theme_light() +
  xlab("CSN_Cluster_Random-Forest") +
  ylab("Propensity Score")

##### 5.1 Daily & Site Level - prepare train & test data for clustering #####

csn_train_daily = csn_train
csn_train_daily$Qualifier = csn_train_daily$State = NULL

# double check if there are NAs in csn_train_daily
which(is.na(csn_train_daily), arr.ind=TRUE) # 

# add geoid (here equals to fips information here) for each site
csn_train_daily$SiteCode = as.character(csn_train_daily$SiteCode)
csn_train_daily = merge(csn_train_daily, 
                       csn_cty_rural_urban, 
                       all.x = T)
head(csn_train_daily)

csn_train_daily = csn_train_daily %>% relocate(geoid, .before = SiteCode)
colnames(csn_train_daily)[1] = "fips"

csn_train_tra_daily = csn_train_daily
dim(csn_train_tra_daily)

#### prepare test data with similar methods
csn_test_daily = csn_test
csn_test_daily$Qualifier = csn_test_daily$State = NULL
csn_test_daily$SiteCode = as.character(csn_test_daily$SiteCode)

# double check if there are NAs in csn_test_daily
which(is.na(csn_test_daily), arr.ind=TRUE) # 

# add geoid (here equals to fips information here) for each site
csn_test_daily = merge(csn_test_daily, 
                      csn_cty_rural_urban, 
                      all.x = T)
head(csn_test_daily)

csn_test_daily = csn_test_daily %>% relocate(geoid, .before = SiteCode)
colnames(csn_test_daily)[1] = "fips"

csn_test_tra_daily = csn_test_daily
dim(csn_test_tra_daily)

# temporarily remove influence from 20900035, which was not matched to any county
csn_train_tra_daily = na.omit(csn_train_tra_daily)
csn_test_tra_daily = na.omit(csn_test_tra_daily)

##### 5.2 convert daily to monthly #####
## not daily, cause always lead to error, vector memory exhausted (limit reached?)
## even after extending the memory via terminal
## change memory, https://stackoverflow.com/questions/51248293/error-vector-memory-exhausted-limit-reached-r-3-5-0-macos


### prepare training data
csn_train_month = csn_train
csn_train_month$Qualifier = csn_train_month$State = NULL

# get the Year-Month date 
csn_train_month$yr.month <- format(
  as.Date(
    csn_train_month$Date), 
  "%Y-%m")

# calculate the year-month average mean for all variables
csn_train_month = csn_train_month  %>% 
  group_by(SiteCode, yr.month) %>%
  summarise_all(# across(Al:Zn), # when using across, sometimes error can hardly be resolved
    mean, na.rm = T)

# double check if there are NAs in csn_train_month
which(is.na(csn_train_month), arr.ind=TRUE) # 

# add geoid (here equals to fips information here) for each site
csn_train_month$SiteCode = as.character(csn_train_month$SiteCode)
csn_train_tra_month = merge(csn_train_month, 
                        csn_cty_rural_urban, 
                        all.x = T)
head(csn_train_tra_month)

csn_train_tra_month = csn_train_tra_month %>% relocate(geoid, .before = SiteCode)
colnames(csn_train_tra_month)[1] = "fips"
csn_train_tra_month = na.omit(csn_train_tra_month)
csn_train_tra_month$Date = NULL
dim(csn_train_tra_month)
colnames(csn_train_tra_month)


### prepare testing data
csn_test_month = csn_test
csn_test_month$Qualifier = csn_test_month$State = NULL

csn_test_month$yr.month <- format(
  as.Date(
    csn_test_month$Date), 
  "%Y-%m")

csn_test_month = csn_test_month  %>% 
  group_by(SiteCode, yr.month) %>%
  summarise_all(# across(Al:Zn), # when using across, sometimes error can hardly be resolved
    mean, na.rm = T)

# double check if there are NAs in csn_test_month
which(is.na(csn_test_month), arr.ind=TRUE) # 

# add geoid (here equals to fips information here) for each site
csn_test_month$SiteCode = as.character(csn_test_month$SiteCode)
csn_test_tra_month = merge(csn_test_month, 
                            csn_cty_rural_urban, 
                            all.x = T)
head(csn_test_tra_month)

csn_test_tra_month = csn_test_tra_month %>% 
  relocate(geoid, .before = SiteCode)
colnames(csn_test_tra_month)[1] = "fips"
csn_test_tra_month = na.omit(csn_test_tra_month)
csn_test_tra_month$Date = NULL
dim(csn_test_tra_month)
colnames(csn_test_tra_month)


##### 5.3 Monthly & Site Level - cluster with random forest - NO GPS #####

# consider no gps info in the random forest model
csn_month_rf_fit <- randomForest(x = csn_train_tra_month[, 4:(ncol(csn_train_tra_month) - 2)], 
                                 y = NULL, 
                                 ntree = 10000, 
                                 proximity = TRUE, 
                                 oob.prox = TRUE)

csn_month_rf_hclust <- hclust(as.dist(1-csn_month_rf_fit$proximity), method = "ward.D2")

csn_month_rf_hclust_tree = cutree(csn_month_rf_hclust, k=25)

csn_month_rf = csn_train_tra_month
csn_month_rf$rf.clusters <- csn_month_rf_hclust_tree
# table(csn_log_rf_hclust_tree, iris$Species)
csn_month_rf$rf.clusters.fc = as.factor(csn_month_rf$rf.clusters)
data.frame(table(csn_month_rf$SiteCode, csn_month_rf$rf.clusters))

# Set a random seed
set.seed(51)

# Training using ‘random forest’ algorithm
library(caret)
csn_rf = train(rf.clusters.fc ~ Al:RuralUrban,
               data = csn_month_rf,
               method = "rf",
               trControl = trainControl(method = 'cv', # Use cross-validation
                                        number = 5)) # Use 5 folds for cross-validation


csn_test_tra_month$pred.clusters <- predict(csn_rf, 
                                         newdata = csn_test_tra_month[, 4:(ncol(csn_test_tra_month) - 2)])
csn_test_tra_month$pred.clusters.fc = as.factor(csn_test_tra_month$pred.clusters)
data.frame(table(csn_test_tra_month$SiteCode, csn_test_tra_month$pred.clusters.fc))
data.frame(table(csn_test_tra_month$pred.clusters.fc))

csn_month_train = dplyr::select(csn_month_rf, RuralUrban, Longitude, Latitude,
                                      fips, SiteCode, yr.month, rf.clusters.fc)
csn_month_Pred = dplyr::select(csn_test_tra_month, 
                               SiteCode, yr.month, pred.clusters.fc)
csn_month_train_testPred = merge(csn_month_train, csn_month_Pred, all.x = T)
write.csv(csn_month_train_testPred, "CSN_site_Month_randomForest_20130111_result1.csv")
write.csv(csn_month_train_testPred, "CSN_site_Month_randomForest_20130111_result2.csv")


sites = unique(csn_month_train_testPred$SiteCode)
site_summary = NULL

for(k in 1:length(sites)){
  site.study = sites[k]
  site_cluster = subset(csn_month_train_testPred, SiteCode == site.study)
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
# detailed process are in file BCSN_site_level_randomForest_20230124_1-5.xlsx
### which is combination of CSN_site_level_randomForest_20230124_1.csv to ~_5.csv
csn_rf_5train = read.csv("Cluster_decision_BasedOn_CSN_site_level_randomForest_20230124_1-5.csv")

# extract & assign GPS info
csn_site_gps = dplyr::select(csn_train_mean_bdr, fips, SiteCode, Longitude, Latitude)
csn_rf_5train = merge(csn_rf_5train, csn_site_gps)

UScounty <- map_data("county")
ggplot(csn_rf_5train, 
       aes(Longitude, Latitude, color= as.factor(Final.Decision))) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(group = as.factor(Final.Decision)), linetype="dashed") +
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()


#### example of random forest clustering ####
iris.rf <- randomForest(Species ~ ., 
                        data = iris, 
                        importance = TRUE,
                        proximity = TRUE)
print(iris.rf)
iris.pc <- prcomp(iris[,1:4], center = FALSE, scale. = FALSE)$x %>% as.data.frame()

rf.fit <- randomForest(x = iris[,1:4], y = NULL, ntree = 10000, proximity = TRUE, oob.prox = TRUE)
hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
rf.cluster = cutree(hclust.rf, k=3)
iris.pc$rf.clusters <- rf.cluster
table(rf.cluster, iris$Species)

