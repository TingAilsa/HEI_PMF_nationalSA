##clear environment
# rm(list=ls())

##set working directory
setwd("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE")
getwd()
data.dir <- "/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE"

library(tidyverse)
library(readxl)
library(data.table)
library(dplyr)
library(plyr)
library(ggplot2) 
library(base)
library(ggrepel)
# library(bspec) # signal-to-noise ratio, snr{}


##### NOT NEEDED! import and match CSN #####

# import CSN
# csn_daily_before = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_interpulation_random-forest_afterLog_before_2015.csv")
# csn_daily_after = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_interpulation_random-forest_afterLog_after_2015.csv")
csn_daily_before = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_interpulation_random-forest_until_2015_2023.03.csv")
csn_daily_after = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_interpulation_random-forest_from_2016_2023.03.csv")
csn_daily_before$X = csn_daily_after$X = NULL
csn_daily_before$Date = as.Date(csn_daily_before$Date)
csn_daily_after$Date = as.Date(csn_daily_after$Date)

##### `````` Has been replaced, Below is more consistent  `````` #####
# variables not to be used for PMF
# since 2023-02-16, add "NH4+" into species file for PMF analysis
# since 2023-03, add more species for PMF analysis
csn_bfr_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC.unadjusted.88", "EC.unadjusted.88", "OPC.unadjusted.88",
                   "Cl.")
# "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
# "K.", "Na.", "NH4.", "Sb", "Sn", 
# "Rb", "Zr", "Cl.")
# Above sepecies were kept since 2023.04

csn_aft_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC1.88", "OC2.88", "OC3.88", "OC4.88",
                   "EC1.88", "EC2.88", "EC3.88", 
                   "EC1", "EC2", "EC3", 
                   "EC.unadjusted.88", "EC.TOR.unadjust.88", "EC.88",
                   "OC.unadjusted.88", "OC.TOR.unadjusted.88", "OC", 
                   "OPC.88", "OP.TOR.unadjusted.88", "OPC.unadjusted.88",
                   "Cl.")
# "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
# "K.", "Na.", "NH4.", "Sb", "Sn", 
# "Rb", "Zr", "Cl.")
# Above sepecies were kept since 2023.04

summary(select(csn_daily_before, 
               EC1.unadjusted.88, EC2.unadjusted.88, EC3.unadjusted.88, 
               OC1.unadjusted.88, OC2.unadjusted.88, 
               OC3.unadjusted.88, OC4.unadjusted.88))

summary(select(csn_daily_after, 
               EC1.unadjusted.88, EC2.unadjusted.88, EC3.unadjusted.88, 
               OC1.unadjusted.88, OC2.unadjusted.88, 
               OC3.unadjusted.88, OC4.unadjusted.88,
               OC1.88, OC2.88, OC3.88, OC4.88,
               EC1, EC2, EC3))

csn_daily_before[ ,csn_bfr_remove] <- list(NULL)
csn_daily_after[ ,csn_aft_remove] <- list(NULL)

# reset colnames
colnames(csn_daily_before)[c(5, 13, 22, 23)]
colnames(csn_daily_after)[c(12, 21, 22, 25)]

colnames(csn_daily_before)[c(5, 13, 22, 23)] = c("PM25", "EC", "OC", "OP")
colnames(csn_daily_after)[c(12, 21, 22, 25)] = c("EC", "OC", "OP", "PM25")

# move PM2.5 concentration to the last column
csn_daily_before <- 
  csn_daily_before %>% 
  relocate(PM25, .after = last_col())
csn_daily_after <- 
  csn_daily_after %>% 
  relocate(PM25, .after = last_col())

setDT(csn_daily_before)
setDT(csn_daily_after)

csn_daily = rbind(csn_daily_before, csn_daily_after)
##### `````` Has been replaced, Below is more consistent  `````` #####

#### V.1 CSN, concentrations-- Combine data before & after 2015 - only OC/EC ####
# "OC" already existed in rownames after 2015, so delete it first
csn_daily_after$OC = NULL

# Changed selected to OC, EC & OP for PMF
csn_daily_before = plyr::rename(csn_daily_before, 
                                c("Accept.PM2.5" = "PM25",
                                  "EC.TOR.unadjust.88" = "EC", 
                                  "OC.TOR.unadjusted.88" = "OC",
                                  "OP.TOR.unadjusted.88" = "OP"))

csn_daily_after = plyr::rename(csn_daily_after, 
                               c("PM2.5RC" = "PM25",
                                 "EC.TOR.88" = "EC", 
                                 "OC.88" = "OC",
                                 "OPC.TOR.88" = "OP"))

# get shared colnames
conc_common_cols <- intersect(names(csn_daily_before), 
                             names(csn_daily_after))

# Subset data frames using shared colnames
csn_daily_before <- csn_daily_before[, conc_common_cols]
csn_daily_after <- csn_daily_after[, conc_common_cols]

# combine the dataset
csn_daily_OrigOrder = rbind(csn_daily_before, csn_daily_after)
setDT(csn_daily_OrigOrder)

# Remove variables not needed for PMF (C-subgroups)
csn_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
               "OC1.unadjusted.88", "OC2.unadjusted.88", 
               "OC3.unadjusted.88", "OC4.unadjusted.88",
               "OC.unadjusted.88", "EC.unadjusted.88", "OPC.unadjusted.88",
               "OP")
csn_daily_OrigOrder[ ,csn_remove] <- list(NULL)

# Move grouped columns to the right of the dataframe
# match(), select columns that match a specific pattern in their names
# everything(), include all remaining columns not selected by matches()
OC.EC = c("EC", "OC")
ions = c("Na.", "K.", "NH4.", "NO3", "SO4")
csn_daily = csn_daily_OrigOrder %>%
  select(!(matches(OC.EC)), 
         everything())
csn_daily = csn_daily %>%
  select(!(matches(ions)), 
         everything())

# Replace other columns
csn_daily = csn_daily %>% relocate(PM25, .after = SO4)
csn_daily = csn_daily %>% relocate(SiteCode, .before = Date)
csn_daily = csn_daily %>% relocate(Date, .before = State)

colnames(csn_daily)

# reorder the dataset for matching
csn_daily = csn_daily[with(
  csn_daily, 
  order(SiteCode, Date)), ]

write.csv(csn_daily, "CSN_RFinterpulated_combine_2023.04.csv")

#### V.2 CSN, concentrations-- Combine data - OC/EC Subgroups! ####
# "OC" already existed in rownames after 2015, so delete it first
csn_daily_after$OC = NULL

# Changed selected to OC, EC & OP for PMF
csn_daily_before = plyr::rename(csn_daily_before, 
                                c("EC1.unadjusted.88" = "EC1", 
                                  "EC2.unadjusted.88" = "EC2",
                                  "EC3.unadjusted.88" = "EC3",
                                  "OC1.unadjusted.88" = "OC1",
                                  "OC2.unadjusted.88" = "OC2", 
                                  "OC3.unadjusted.88" = "OC3",
                                  "OC4.unadjusted.88" = "OC4",
                                  "Accept.PM2.5" = "PM25",
                                  "EC.TOR.unadjust.88" = "EC", 
                                  "OC.TOR.unadjusted.88" = "OC",
                                  "OP.TOR.unadjusted.88" = "OP"))

csn_daily_after = plyr::rename(csn_daily_after, 
                               c("OC1.88" = "OC1",
                                 "OC2.88" = "OC2", 
                                 "OC3.88" = "OC3",
                                 "OC4.88" = "OC4",
                                 "PM2.5RC" = "PM25",
                                 "EC.TOR.88" = "EC", 
                                 "OC.88" = "OC",
                                 "OPC.TOR.88" = "OP"))

# get shared colnames
conc_common_cols <- intersect(names(csn_daily_before), 
                              names(csn_daily_after))

# Subset data frames using shared colnames
csn_daily_before <- csn_daily_before[, conc_common_cols]
csn_daily_after <- csn_daily_after[, conc_common_cols]

# combine the dataset
csn_daily_OrigOrder = rbind(csn_daily_before, csn_daily_after)
setDT(csn_daily_OrigOrder)

# Remove variables not needed for PMF (C-subgroups)
csn_remove = c("OC.unadjusted.88", "EC.unadjusted.88", 
               "OPC.unadjusted.88", "OP")
csn_daily_OrigOrder[ ,csn_remove] <- list(NULL)

# Move grouped columns to the right of the dataframe
# match(), select columns that match a specific pattern in their names
# everything(), include all remaining columns not selected by matches()
OC.EC = c("EC", "EC1", "EC2", "EC3", 
          "OC", "OC1", "OC2", "OC3", "OC4")
ions = c("Na.", "K.", "NH4.", "NO3", "SO4")
csn_daily = csn_daily_OrigOrder %>%
  select(!(matches(OC.EC)), 
         everything())
csn_daily = csn_daily %>%
  select(!(matches(ions)), 
         everything())

# Replace other columns
csn_daily = csn_daily %>% relocate(PM25, .after = SO4)
csn_daily = csn_daily %>% relocate(SiteCode, .before = Date)
csn_daily = csn_daily %>% relocate(Date, .before = State)

colnames(csn_daily)
dim(csn_daily)

# reorder the dataset for matching
csn_daily = csn_daily[with(
  csn_daily, 
  order(SiteCode, Date)), ]

write.csv(csn_daily, "CSN_RFinterpulated_combine_Csubgroup_2023.04.csv")

##############################################################
##### START #####
##############################################################

#### PM2.5 concentration distribution - Sites & Clusters ####
# csn_daily$year = year(csn_daily$Date)
csn_daily = read.csv("CSN_RFinterpulated_combine_2023.04.csv")
csn_daily = read.csv("CSN_RFinterpulated_combine_Csubgroup_2023.04.csv")
csn_daily$X = NULL
csn_daily$Date = as.Date(csn_daily$Date)
setDT(csn_daily)
sapply(csn_daily, class)

# csn_daily = csn_daily[with(
#   csn_daily, 
#   order(State, SiteCode, Date)), ]
# write.csv(csn_daily, "CSN_RFinterpulated_combine_2023.04.csv")

csn_cluster = read.csv("CSN_RF_cluster5training.csv")

csn_PM_avg = ddply(csn_daily, 
                   .(SiteCode), 
                   summarise,
                   PM25 = mean(PM25))

csn_PM_avg = merge(csn_PM_avg, csn_cluster)

# the mean PM level of each cluster
csn_PM_cluster = ddply(csn_PM_avg, 
                   .(Final.Decision), 
                   summarise,
                   PM25 = mean(PM25))

###### Cluster with the highest and lowest PM ###### 
# cluster with the highest and lowest PM
cluster.top.PM = csn_PM_cluster$Final.Decision[
  which(
    csn_PM_cluster$PM25 == 
      max(csn_PM_cluster$PM25))]
cluster.least.PM = csn_PM_cluster$Final.Decision[
  which(
    csn_PM_cluster$PM25 == 
      min(csn_PM_cluster$PM25))]

# sites in clusters with the highest and lowest PM
sites.top.PM.cluster = csn_PM_avg$SiteCode[
  which(
    csn_PM_avg$Final.Decision == 
      cluster.top.PM)]
sites.least.PM.cluster = csn_PM_avg$SiteCode[
  which(
    csn_PM_avg$Final.Decision == 
      cluster.least.PM)]

# number of sites in each cluster
csn_each_cluster = data.frame(table(csn_cluster$Final.Decision))
colnames(csn_each_cluster) = c("Final.Decision", "No.Site.in.Cluster")

# get the GPS of sites
cty_rural_urban = read.csv("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$X.1 = NULL

site_gps = select(cty_rural_urban, SiteCode, Longitude, Latitude)

csn_PM_site = merge(csn_PM_avg, site_gps)
csn_PM_site = merge(csn_PM_site, csn_cluster)

csn_PM_site$PM.level = "Middle"
csn_PM_site$PM.level[
  csn_PM_site$SiteCode %in% 
    sites.top.PM.cluster] = "Highest.PM"
csn_PM_site$PM.level[
  csn_PM_site$SiteCode %in% 
    sites.least.PM.cluster] = "Lowest.PM"

sapply(csn_PM_site, class)

UScounty <- map_data("county")
# map site-average concentration
ggplot(csn_PM_site, 
       aes(Longitude, Latitude, color = PM25, group = PM.level)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.8, aes(shape = PM.level)) +
  scale_color_continuous(low = "steelblue", high = "darkorange2") +
  theme_bw()

ggplot(subset(csn_PM_site, SiteCode == "60371103"), 
       aes(Longitude, Latitude, color = PM25, group = PM.level)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.8, aes(shape = PM.level)) +
  scale_color_continuous(low = "steelblue", high = "darkorange2") +
  theme_bw()

###### FINISHED, NOT NEEDED NOW!! CSN, combine the marked NA & qualifiers removed and interpolated ######
csn_NA_bef = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_Component_with_missing_Before_2015_2023.02.csv")
csn_NA_aft = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_Component_with_missing_After_2015_2023.02.csv")
csn_NA_bef$X = csn_NA_aft$X = NULL
csn_NA_bef$Date = as.Date((csn_NA_bef$Date))
csn_NA_aft$Date = as.Date((csn_NA_aft$Date))

# set to table
setDT(csn_NA_bef)
setDT(csn_NA_aft)

# "OC" already existed in rownames after 2015, so delete it first
csn_NA_aft$OC = NULL

###################### Choose OC EC or also C-Subgroups
###################### Choose 1. OC EC only
# Changed selected to OC, EC & OP for PMF, only OC EC
csn_NA_bef = plyr::rename(csn_NA_bef, 
                          c("Accept.PM2.5" = "PM25",
                            "EC.TOR.unadjust.88" = "EC", 
                            "OC.TOR.unadjusted.88" = "OC",
                            "OP.TOR.unadjusted.88" = "OP"))

csn_NA_aft = plyr::rename(csn_NA_aft, 
                          c("PM2.5RC" = "PM25",
                            "EC.TOR.88" = "EC", 
                            "OC.88" = "OC",
                            "OPC.TOR.88" = "OP"))

###################### Choose 2. also C-Subgroups
# Changed selected to OC, EC & OP for PMF, only OC EC
csn_NA_bef = plyr::rename(csn_NA_bef, 
                          c("EC1.unadjusted.88" = "EC1", 
                            "EC2.unadjusted.88" = "EC2",
                            "EC3.unadjusted.88" = "EC3",
                            "OC1.unadjusted.88" = "OC1",
                            "OC2.unadjusted.88" = "OC2", 
                            "OC3.unadjusted.88" = "OC3",
                            "OC4.unadjusted.88" = "OC4",
                            "Accept.PM2.5" = "PM25",
                            "EC.TOR.unadjust.88" = "EC", 
                            "OC.TOR.unadjusted.88" = "OC",
                            "OP.TOR.unadjusted.88" = "OP"))

csn_NA_aft = plyr::rename(csn_NA_aft, 
                          c("OC1.88" = "OC1",
                            "OC2.88" = "OC2", 
                            "OC3.88" = "OC3",
                            "OC4.88" = "OC4",
                            "PM2.5RC" = "PM25",
                            "EC.TOR.88" = "EC", 
                            "OC.88" = "OC",
                            "OPC.TOR.88" = "OP"))


# get common sites & colnames
csn_common_sites <- intersect(csn_NA_bef$SiteCode, 
                              csn_NA_aft$SiteCode)
length(csn_common_sites)
length(unique(csn_NA_bef$SiteCode))
length(unique(csn_NA_aft$SiteCode))

csn_common_cols <- intersect(names(csn_NA_bef), 
                             names(csn_NA_aft))

# remove other OC/EC subgroups
col_bef = csn_common_cols[!(grepl("88", csn_common_cols, fixed = T))]
col_aft = csn_common_cols[!(grepl("88", csn_common_cols, fixed = T))]
summary(col_bef %in% col_aft)
summary(col_aft %in% col_bef)

# Subset data frames using common column names
# do not work for data.table
csn_RowCol_bef <- data.frame(csn_NA_bef)[, col_bef] 
csn_RowCol_aft <- data.frame(csn_NA_aft)[, col_aft]
summary(colnames(csn_RowCol_bef) == colnames(csn_RowCol_aft))

csn_RowCol_bef$Date = as.Date(csn_RowCol_bef$Date)
csn_RowCol_aft$Date = as.Date(csn_RowCol_aft$Date)
csn_combine = rbind(csn_RowCol_bef, csn_RowCol_aft)

## remove the rows where all component concentrations are NAs
cols.comp = 4:ncol(csn_combine) # columns for PM/components

csn_noAllNA = subset(csn_combine, 
                     rowSums(is.na(csn_combine[, cols.comp])) != 
                       ncol(csn_combine[, cols.comp]))

# remove sites not in csn_daily, which are not in mainland US
csn_noAllNA = subset(csn_noAllNA, 
                     SiteCode %in% csn_daily$SiteCode)
dim(csn_noAllNA)

# define the data in csn_noAllNA but not csn_daily
csn_noAllNA$site.date = paste(csn_noAllNA$SiteCode, csn_noAllNA$Date)
csn_daily$site.date = paste(csn_daily$SiteCode, csn_daily$Date)

csn_noAllNA_notPMF = subset(csn_noAllNA, 
                            !(csn_noAllNA$site.date %in% csn_daily$site.date) )
csn_PMF_notNoAllNA = subset(csn_daily, 
                            !(csn_daily$site.date %in% csn_noAllNA$site.date) )
dim(csn_noAllNA_notPMF)
dim(csn_PMF_notNoAllNA)
unique(csn_noAllNA_notPMF$SiteCode)

csn_noAllNA_left = subset(csn_noAllNA, 
                          csn_noAllNA$site.date %in% csn_daily$site.date)
csn_daily$site.date = csn_noAllNA_left$site.date = NULL

################# not for C-subgroup!
csn_daily = subset(csn_daily, 
                  !(SiteCode == csn_PMF_notNoAllNA$SiteCode[1] &
                      Date == csn_PMF_notNoAllNA$Date[1]))
# write.csv(csn_daily, "CSN_RFinterpulated_combine_2023.04.csv")
# write.csv(csn_daily, "CSN_RFinterpulated_combine_Csubgroup_2023.04.csv")

# OP not used for PMF
csn_noAllNA_left$OP = NULL

dim(csn_noAllNA_left)
dim(csn_daily)

# Move grouped columns to the right of the dataframe
# OC.EC = c("EC", "OC")
OC.EC = c("EC", "EC1", "EC2", "EC3", 
          "OC", "OC1", "OC2", "OC3", "OC4")
ions = c("Na.", "K.", "NH4.", "NO3", "SO4")
csn_noAllNA_left = csn_noAllNA_left %>%
  select(!(matches(OC.EC)), 
         everything())
csn_noAllNA_left = csn_noAllNA_left %>%
  select(!(matches(ions)), 
         everything())

# Replace other columns
csn_noAllNA_left = csn_noAllNA_left %>% relocate(PM25, .after = SO4)
csn_noAllNA_left = csn_noAllNA_left %>% relocate(SiteCode, .before = Date)
csn_noAllNA_left = csn_noAllNA_left %>% relocate(Date, .before = State)

summary(colnames(csn_noAllNA_left) == colnames(csn_daily))

# reorder dataset
csn_noAllNA_left = csn_noAllNA_left[with(
  csn_noAllNA_left, 
  order(State, SiteCode, Date)), ]

summary(csn_noAllNA_left$Date == csn_daily$Date)
summary(csn_noAllNA_left$SiteCode == csn_daily$SiteCode)

# write.csv(csn_noAllNA_left, "CSN_withNA_combine_PMFuncertainty_Estimation.csv")
# write.csv(csn_noAllNA_left, "CSN_withNA_combine_PMFuncertainty_Estimation_C-subgroup.csv")

#### Selection1: use the MDL based on CSN dataset ####
###### CSN_MDL - prepare site & date matched MDL ###### 
csn_daily_conc = csn_daily
# csn_daily_conc = csn_daily_cluster_top_PM
csn_daily_conc$State = csn_daily_conc$Qualifier = NULL
csn_daily_conc$year = year(csn_daily_conc$Date)
csn_daily_conc$month = month(csn_daily_conc$Date)
dim(csn_daily_conc)

# get monthly MDL
# csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_MDL_monthly.csv")
# csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_MDL_monthly_2023.02.27.csv")
csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE/CSN_MDL_C-Sub_monthly_2023.05.csv")
csn_mdl$X = NULL

# remove Cl-
csn_mdl$Cl. = csn_mdl$OP = NULL

# reorder the dataset for matching
csn_daily_conc = csn_daily_conc[with(
  csn_daily_conc, 
  order(SiteCode, Date)), ]
csn_mdl = csn_mdl[with(
  csn_mdl, 
  order(SiteCode, year, month)), ]

# check if columns from concentration & MDL datasets match
summary(colnames(csn_daily_conc)[3:(ncol(csn_daily_conc)-2)] == 
          colnames(csn_mdl)[4:ncol(csn_mdl)])
dim(csn_daily_conc)
dim(csn_mdl)

######### THIS WAS NOT USED SINCE 2023.05 ############
#### From 2023.05 Use Originally caululated MDL
# Re-calculated the monthly MDL for OC, EC & OPC
## 1. NO real MDL data for them
## 2. MDL for given subgroups 20 changed, like OC2.88 & OC2, 0.24 vs. 0.017 
## use the 10th percentile concentration value
csn_mdl_OCEC = ddply(csn_daily_conc, 
                     .(SiteCode, year, month),
                     summarise,
                     EC.10th = quantile(EC, 0.1, na.rm = T),
                     OC.10th = quantile(OC, 0.1, na.rm = T), 
                     EC1.10th = quantile(EC1, 0.1, na.rm = T),
                     OC1.10th = quantile(OC1, 0.1, na.rm = T), 
                     EC2.10th = quantile(EC2, 0.1, na.rm = T),
                     OC2.10th = quantile(OC2, 0.1, na.rm = T), 
                     EC3.10th = quantile(EC3, 0.1, na.rm = T),
                     OC3.10th = quantile(OC3, 0.1, na.rm = T),
                     OC4.10th = quantile(OC4, 0.1, na.rm = T))

# replace and remove the old value, which are all -999
csn_mdl_newOCEC = merge(csn_mdl, csn_mdl_OCEC)
csn_mdl_newOCEC$EC = csn_mdl_newOCEC$EC.10th
csn_mdl_newOCEC$OC = csn_mdl_newOCEC$OC.10th
# csn_mdl_newOCEC$OP = csn_mdl_newOCEC$OP.10th
csn_mdl_newOCEC$EC.10th = csn_mdl_newOCEC$OC.10th = NULL
# csn_mdl_newOCEC$OP.10th = NULL
summary(csn_mdl_newOCEC)
######### THIS WAS NOT USED SINCE 2023.05 ############
#### From 2023.05 Use Originally caululated MDL

# expand MDL file to daily measurement 
# (in case of interpolation, not used original data directly)
csn_daily_conc_date = select(csn_daily_conc, SiteCode, Date, year, month)
csn_daily_fullMDL = merge(csn_daily_conc_date, csn_mdl, all.x = T)
dim(csn_daily_conc_date)

# move Date to the first column
csn_daily_fullMDL =
  csn_daily_fullMDL %>%
  relocate(Date, .before = SiteCode)

# reorder the dataset for matching
csn_daily_fullMDL = csn_daily_fullMDL[with(
  csn_daily_fullMDL, 
  order(SiteCode, Date)), ]

csn_daily_fullMDL$year = csn_daily_fullMDL$month = 
  csn_daily_conc$year = csn_daily_conc$month = NULL

# Again check if columns from concentration & new MDL datasets match
summary(colnames(csn_daily_conc) == 
          colnames(csn_daily_fullMDL))

###### FINISHED, CAN JUMP TO THE NEXT! CSN_MDL - NA/Unacceptable Flagged data - those using interpolation ###### 
csn_NA_intp = read.csv("CSN_withNA_combine_PMFuncertainty_Estimation.csv")
csn_NA_intp = read.csv("CSN_withNA_combine_PMFuncertainty_Estimation_C-subgroup.csv")
csn_NA_intp$Date = as.Date(csn_NA_intp$Date)
csn_NA_intp$X = NULL

# change all numeric in species columns to FALSE and rest to NA
csn_NA_intp[, 4:ncol(csn_NA_intp)] = 
  as.logical(is.na(csn_NA_intp[, 4:ncol(csn_NA_intp)]))
summary(csn_NA_intp)

# reorder the dataset for matching
csn_NA_intp = csn_NA_intp[with(
  csn_NA_intp, 
  order(SiteCode, Date)), ]

summary(csn_NA_intp$SiteCode == csn_daily_conc$SiteCode)
summary(csn_NA_intp$Date == csn_daily_conc$Date)

write.csv(csn_NA_intp, "CSN_TF_logical_InterpolatedOrNot.csv")
write.csv(csn_NA_intp, "CSN_TF_logical_InterpolatedOrNot_C-subgroup.csv")

###### CSN_MDL - estimate the uncertainty ###### 
# double check if date & site match
summary(csn_daily_fullMDL$SiteCode == csn_daily_conc$SiteCode)
summary(csn_daily_fullMDL$Date == csn_daily_conc$Date)
summary(colnames(csn_daily_fullMDL) == colnames(csn_daily_conc))

# compare concentration and MDL of a given component
csn_conc = csn_daily_conc[, 3:ncol(csn_daily_conc)]
csn_mdl = csn_daily_fullMDL[, 3:ncol(csn_daily_fullMDL)]

csn_conc_mdl = data.frame(Map(">", csn_conc, csn_mdl))
setDT(csn_conc_mdl)

# check variable class & dataset dimenssion
sapply(csn_conc, class)
sapply(csn_mdl, class)
sapply(csn_conc_mdl, class)
dim(csn_conc)
dim(csn_mdl)
dim(csn_conc_mdl)

# test uncertainty, error_fraction 
# comp_error_fraction = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/CSN_k_Error Fraction.csv")
comp_error_fraction = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/CSN_k_Error-Fraction_2023.04.csv")
comp_error_fraction$data = NULL

# set EF of subgroups to the same of OC, EC
comp_error_fraction$EC3 = comp_error_fraction$EC2 = comp_error_fraction$EC1 = comp_error_fraction$EC
comp_error_fraction$OC4 = comp_error_fraction$OC3 = comp_error_fraction$OC2 = comp_error_fraction$OC1 = comp_error_fraction$OC.A = comp_error_fraction$OC
comp_error_fraction$OC = NULL
comp_error_fraction = plyr::rename(comp_error_fraction, c("OC.A" = "OC"))

# only keep species existing in CSN dataset
# get common colnames
mdl_ef_cols <- intersect(names(csn_conc_mdl), 
                         names(comp_error_fraction))
# Subset data frames using common rownames and column names
comp_error_fraction <- comp_error_fraction[, mdl_ef_cols]

# expand error_fraction file to one with same row number as concentration file
comp_ef = do.call("rbind", replicate(nrow(csn_conc_mdl), 
                                     comp_error_fraction[1, ], 
                                     simplify = F))
setDT(comp_ef)

# assign data conditionally to concentration and uncertainty values for PMF
summary(colnames(csn_conc_mdl) == colnames(csn_conc) & 
          colnames(csn_conc_mdl) == colnames(csn_mdl) &
          colnames(csn_conc_mdl) == colnames(comp_ef))
summary(dim(csn_conc_mdl) == dim(csn_conc) & 
          dim(csn_conc_mdl) == dim(csn_mdl) & 
          dim(csn_conc_mdl) == dim(comp_ef))
summary(sapply(csn_conc_mdl, class) %in% "logical")
summary(sapply(csn_conc, class) %in% "numeric")
summary(sapply(csn_mdl, class) %in% "numeric")

# data.table & data.frame, error report when calculating
setDT(csn_conc)
setDT(csn_mdl)

conc_pmf = csn_conc_mdl * csn_conc +
  (!csn_conc_mdl) * csn_mdl * 0.5
unc_pmf = csn_conc_mdl * (csn_mdl / 3 + comp_ef * csn_conc) +
  (!csn_conc_mdl) * 5/6 * csn_mdl

conc_pmf$PM25 = csn_daily_conc$PM25
unc_pmf$PM25 = 3 * csn_daily_conc$PM25

# for those using refilling (NA or unacceptable Flag), add the uncertainty *1.5 
#csn_NA_intp = read.csv("CSN_TF_logical_InterpolatedOrNot.csv")
csn_NA_intp = read.csv("CSN_TF_logical_InterpolatedOrNot_C-subgroup.csv")
csn_NA_intp$X = NULL

# double check if rows & columns match
summary(csn_NA_intp$SiteCode == csn_daily_conc$SiteCode)
summary(csn_NA_intp$Date == csn_daily_conc$Date)
summary(colnames(csn_NA_intp)[4:ncol(csn_NA_intp)] == 
          colnames(csn_daily_conc)[3:ncol(csn_daily_conc)])

csn_NA_intp = csn_NA_intp[, 4:ncol(csn_NA_intp)]
csn_NA_intp[csn_NA_intp == TRUE] <- 1.5
csn_NA_intp[csn_NA_intp == FALSE] <- 1
setDT(csn_NA_intp)

summary(colnames(unc_pmf) == colnames(csn_NA_intp))
unc_pmf = unc_pmf * csn_NA_intp[, 4:ncol(csn_NA_intp)]

# insert date into  concentration & uncertainty datasets
conc_pmf <- cbind(csn_daily[, 1:3], conc_pmf) # csn_daily[, 1:4]
unc_pmf <- cbind(csn_daily[, 1:3], unc_pmf) # csn_daily[, 1:4]
csn_conc_mdl_Site = cbind(csn_daily[, 1:3], csn_conc_mdl)

################### For site & date match check, finished!
csn_conc_mdl_LA = subset(cbind(csn_daily[, 1:3], 
                               csn_conc_mdl), 
                         SiteCode == "60371103")
csn_mdl_LA = subset(csn_daily_fullMDL, 
                    SiteCode == "60371103")
csn_conc_LA = subset(csn_daily_conc, 
                     SiteCode == "60371103")

conc_pmf_LA = csn_conc_mdl_LA[, 4:ncol(csn_conc_mdl_LA)] * 
  csn_conc_LA[, 3:ncol(csn_conc_LA)] +
  (!csn_conc_mdl_LA[, 4:ncol(csn_conc_mdl_LA)]) * 
  csn_mdl_LA[, 3:ncol(csn_mdl_LA)] * 0.5
conc_pmf_LA_check = subset(conc_pmf, SiteCode == "60371103")

conc_pmf_LA[1:3, 1:10]
conc_pmf_LA_check[1:3, 4:13]
csn_conc_mdl_LA[1:3, 4:13]
################### For site & date match check, finished!

# write.csv(conc_pmf, "CSN_concentration_for_PMF.csv")
# write.csv(unc_pmf, "CSN_uncertainty_for_PMF.csv")
# write.csv(csn_conc_mdl_Site, "CSN_concentration_vs_MDL.csv")

dim(conc_pmf); dim(unc_pmf)

# write.csv(conc_pmf, "CSN_concentration_for_PMF_2023.04.csv")
# write.csv(unc_pmf, "CSN_uncertainty_for_PMF_2023.04.csv")
# write.csv(csn_conc_mdl_Site, "CSN_concentration_vs_MDL_2023.04.csv")

# write.csv(conc_pmf, "CSN_concentration_for_PMF_C-subgroup_2023.04.csv")
# write.csv(unc_pmf, "CSN_uncertainty_for_PMF_C-subgroup_2023.04.csv")
# write.csv(csn_conc_mdl_Site, "CSN_concentration_vs_MDL_C-subgroup_2023.04.csv")

write.csv(conc_pmf, "CSN_PMF_C-subgroup_PM_corrected_2023.05.csv")
write.csv(unc_pmf, "CSN_PMF_C-subgroup_corrected_2023.05.csv")
write.csv(csn_conc_mdl_Site, "CSN_conc_vs_MDL_C-subgroup_corrected_2023.05.csv")

###### CSN_MDL - signal-to-noise SNR S/N estimation ###### 
# conc_pmf = read.csv("CSN_concentration_for_PMF_2023.04.csv")
# unc_pmf = read.csv("CSN_uncertainty_for_PMF_2023.04.csv")
# csn_conc_mdl_Site = read.csv("CSN_concentration_vs_MDL_2023.04.csv")

# conc_pmf = read.csv("CSN_concentration_for_PMF_C-subgroup_2023.04.csv")
# unc_pmf = read.csv("CSN_uncertainty_for_PMF_C-subgroup_2023.04.csv")
# csn_conc_mdl_Site = read.csv("CSN_concentration_vs_MDL_C-subgroup_2023.04.csv")

conc_pmf = read.csv("CSN_PMF_C-subgroup_PM_corrected_2023.05.csv")
unc_pmf = read.csv("CSN_PMF_C-subgroup_corrected_2023.05.csv")
csn_conc_mdl_Site = read.csv("CSN_conc_vs_MDL_C-subgroup_corrected_2023.05.csv")

conc_pmf$X = unc_pmf$X = csn_conc_mdl_Site$X = NULL

conc.col = ncol(conc_pmf)
csn_conc_mdl = csn_conc_mdl_Site[, 4:conc.col]

# according to EPA PMF 5.0 User Guide.pdf, function 5-3 & 5-4 from page 23
conc_unc_diff_pmf = (conc_pmf[, 4:conc.col] - 
                       unc_pmf[, 4:conc.col])/
  unc_pmf[, 4:conc.col] * csn_conc_mdl

conc_unc_diff_pmf = cbind(csn_daily[, 1:3], conc_unc_diff_pmf)

# merge with cluster info
conc_unc_diff_pmf = merge(csn_cluster, conc_unc_diff_pmf)
conc_unc_diff_pmf$Date = conc_unc_diff_pmf$State = conc_unc_diff_pmf$Qualifier = NULL

# calculate SNR of each site 
site_snr = aggregate(. ~ as.factor(SiteCode), 
                     conc_unc_diff_pmf, 
                     sum) / 
  table(conc_unc_diff_pmf$SiteCode)
site_snr$`as.factor(SiteCode)` = NULL
site_snr$PM25 = -999

# calculate SNR of each cluster
cluster_snr = aggregate(. ~ as.factor(Final.Decision), 
                        conc_unc_diff_pmf, 
                        sum) / 
  table(conc_unc_diff_pmf$Final.Decision)
cluster_snr$`as.factor(Final.Decision)` = cluster_snr$SiteCode = NULL
cluster_snr$PM25 = -999

# write.csv(site_snr, "CSN_Site_SNR_for_PMF.csv")
# write.csv(cluster_snr, "CSN_Cluster_SNR_for_PMF.csv")

# write.csv(site_snr, "CSN_Site_SNR_for_PMF_C-subgroup.csv")
# write.csv(cluster_snr, "CSN_Cluster_SNR_for_PMF_C-subgroup.csv")

write.csv(site_snr, "CSN_Site_SNR_PMF_C-subgroup_PM_corrected.csv")
write.csv(cluster_snr, "CSN_Cluster_SNR_PMF_C-subgroup_PM_corrected.csv")

###### CSN_MDL - daily PM of selected cluster - PMF priori try ###### 
# determine the cluster with highest average PM concentrations
csn_PM_cluster = merge(csn_PM_cluster, csn_each_cluster)
Cluster.top.PM = csn_PM_cluster$Final.Decision[
  which(csn_PM_cluster$PM25 == 
          max(csn_PM_cluster$PM25))]
# cluster 6, with five sites

# map the sites with the highest PM average
ggplot(subset(csn_PM_site, Final.Decision == Cluster.top.PM), 
       aes(Longitude, Latitude, color= PM25)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6, shape = 16) 

# extract concentration & uncertainty of selected sites for PMF
csn_select_conc = subset(conc_pmf, SiteCode %in% 
                           csn_cluster$SiteCode[which(
                             csn_cluster$Final.Decision == Cluster.top.PM)])
csn_select_unc = subset(unc_pmf, SiteCode %in% 
                          csn_cluster$SiteCode[which(
                            csn_cluster$Final.Decision == Cluster.top.PM)])

csn_select_conc_use = subset(csn_select_conc, Date < as.Date("2014-01-01"))
csn_select_unc_use = subset(csn_select_unc, Date < as.Date("2014-01-01"))
csn_select_site_row = data.frame(table(csn_select_conc_use$SiteCode))
colnames(csn_select_site_row)[1] = c("SiteCode")

selectedSite = 3
csn_select_conc_1site = subset(csn_select_conc_use, 
                               SiteCode == 
                                 csn_select_site_row$SiteCode[selectedSite])
csn_select_unc_1site = subset(csn_select_unc_use, 
                              SiteCode == 
                                csn_select_site_row$SiteCode[selectedSite])


## cluster
# write.csv(csn_select_conc_use, "CSN_Cluster6_2011-14_PMF_conc.csv")
# write.csv(csn_select_unc_use, "CSN_Cluster6_2011-14_PMF_unc.csv")
# write.csv(csn_select_site_row, "CSN_Cluster6_date_number_of_site.csv")
## site
# write.csv(csn_select_conc_1site, "CSN_Cluster6_1site_2011-14_PMF_conc.csv")
# write.csv(csn_select_unc_1site, "CSN_Cluster6_1site_2011-14_PMF_unc.csv")

# cluster
write.csv(csn_select_conc_use, 
          paste0("CSN_C", Cluster.top.PM, "_2011-14_conc.csv"))
write.csv(csn_select_unc_use, 
          paste0("CSN_C", Cluster.top.PM, "_2011-14_unc.csv"))
write.csv(csn_select_site_row, 
          paste0("CSN_C", Cluster.top.PM, "_2011-14_date_number_of_site.csv"))
# site
write.csv(csn_select_conc_1site, 
          paste0("CSN_C", Cluster.top.PM, "_S", selectedSite, "_2011-14_conc.csv"))
write.csv(csn_select_unc_1site, 
          paste0("CSN_C", Cluster.top.PM, "_S", selectedSite, "_2011-14_unc.csv"))


##################### C-subgroup
# cluster
write.csv(csn_select_conc_use, 
          paste0("CSN_C", Cluster.top.PM, "_C-sub_2011-14_conc.csv"))
write.csv(csn_select_unc_use, 
          paste0("CSN_C", Cluster.top.PM, "_C-sub_2011-14_unc.csv"))
write.csv(csn_select_site_row, 
          paste0("CSN_C", Cluster.top.PM, "_C-sub_2011-14_date_number_of_site.csv"))
# site
write.csv(csn_select_conc_1site, 
          paste0("CSN_C", Cluster.top.PM, "_S", selectedSite, "_C-sub_2011-14_conc.csv"))
write.csv(csn_select_unc_1site, 
          paste0("CSN_C", Cluster.top.PM, "_S", selectedSite, "_C-sub_2011-14_unc.csv"))

# the result for the randomly selected site "60371103" is not good
## checking original concentrations
csn_daily_conc_cluster6_1site = subset(csn_daily,
                                       Date < as.Date("2014-01-01") &
                                         SiteCode == csn_select_site_row$SiteCode[3])
summary(csn_daily_conc_cluster6_1site)
sapply(csn_daily_conc_cluster6_1site, max)

### check the distribution high value of given component
conc_cluster6_1site_highK = subset(csn_daily_conc_cluster6_1site, 
                                   K > 0.2)

###### CSN_MDL - match with AQS PM2.5, DONE! Jump! ###### 
# import data
# conc_pmf = read.csv("CSN_concentration_for_PMF_C-subgroup_2023.04.csv")
# unc_pmf = read.csv("CSN_uncertainty_for_PMF_C-subgroup_2023.04.csv")

conc_pmf = read.csv("CSN_PMF_C-subgroup_PM_corrected_2023.05.csv")
unc_pmf = read.csv("CSN_PMF_C-subgroup_corrected_2023.05.csv")

conc_pmf$X = unc_pmf$X = NULL
conc_pmf$Date = as.Date(conc_pmf$Date)
unc_pmf$Date = as.Date(unc_pmf$Date)

conc_pmf$dup = duplicated(conc_pmf[, 1:3])
summary(conc_pmf$dup)
conc_pmf_dup = subset(conc_pmf, dup)

# read EPA AQS data
aqs_PM25 = read.csv("/Users/ztttttt/Documents/HEI PMF/EPA_AQS_monitor/EPA_CSN_AQS_daily_PM25.csv")
aqs_PM25$X = NULL
aqs_PM25$Date = as.Date(aqs_PM25$Date)
# aqs_PM25$dup = duplicated(aqs_PM25[, 1:2])
# summary(aqs_PM25$dup)

# match conc & unc with cluster info 
conc_pmf_cluster = merge(conc_pmf, csn_cluster, all.x = T)
unc_pmf_cluster = merge(unc_pmf, csn_cluster, all.x = T)

# match with EPA AQS data
conc_pmf_cluster = merge(conc_pmf_cluster,
                         aqs_PM25, 
                         all.x = T)

conc_pmf_cluster$PM25_Combine = ifelse((is.na(conc_pmf_cluster$PM25_AQS) |
                                         conc_pmf_cluster$PM25_AQS <= 2), 
                                       conc_pmf_cluster$PM25, 
                                       conc_pmf_cluster$PM25_AQS)
plot(conc_pmf_cluster$PM25, 
     conc_pmf_cluster$PM25_AQS)
plot(conc_pmf_cluster$PM25, 
     conc_pmf_cluster$PM25_Combine)
summary(conc_pmf_cluster$PM25_Combine)

conc_pmf_cluster$species.sum = sum(conc_pmf_cluster[, 4:37],
                                   conc_pmf_cluster[, 41],
                                   conc_pmf_cluster[, 46:50])

# weird, whole concentration of species is higher than PM2.5!!!
summary(conc_pmf_cluster$species.sum > conc_pmf_cluster$PM25_Combine)
summary(conc_pmf_cluster$species.sum > conc_pmf_cluster$PM25)

# reorder the dataframe before comparison
unc_pmf_cluster = unc_pmf_cluster[with(unc_pmf_cluster,
                                       order(SiteCode, Date)), ]
conc_pmf_cluster = conc_pmf_cluster[with(conc_pmf_cluster,
                                         order(SiteCode, Date)), ]
summary(colnames(unc_pmf_cluster)[4:(ncol(unc_pmf_cluster)-1)] == 
          colnames(conc_pmf_cluster)[4:(ncol(conc_pmf_cluster)-4)])
summary(unc_pmf_cluster$SiteCode == conc_pmf_cluster$SiteCode &
          unc_pmf_cluster$Date == conc_pmf_cluster$Date)

# detect PM2.5 from interpolation
unc_pmf_cluster.1 = unc_pmf_cluster
unc_pmf_cluster$PM25_interp = F
summary(unc_pmf_cluster$PM25/conc_pmf_cluster$PM25_Combine == 4.5)
unc_pmf_cluster$PM25_interp[unc_pmf_cluster$PM25/conc_pmf_cluster$PM25_Combine == 4.5] = T
summary(unc_pmf_cluster$PM25/conc_pmf_cluster$PM25_Combine)
summary(unc_pmf_cluster$PM25_interp)

# re-estiamte uncertainty for PM2.5
unc_pmf_cluster$PM25 = 3 * conc_pmf_cluster$PM25_Combine
# Extra 1.5 times for those from interpolation
unc_pmf_cluster$PM25 = ifelse(unc_pmf_cluster$PM25_interp, 
                              1.5 * unc_pmf_cluster$PM25,
                              unc_pmf_cluster$PM25)

# reasign PM value, mainly use AQS, and delete not used
conc_pmf_cluster$PM25 = conc_pmf_cluster$PM25_Combine
conc_pmf_cluster$species.sum = conc_pmf_cluster$PM25_Combine = 
  conc_pmf_cluster$PM25_AQS = conc_pmf_cluster$dup = NULL
unc_pmf_cluster$PM25_interp = NULL

# write.csv(conc_pmf_cluster, "CSN_concentration_AQS.PM_PMF_C-subgroup_2023.04.csv")
# write.csv(unc_pmf_cluster, "CSN_uncertainty_AQS.PM_PMF_C-subgroup_2023.04.csv")

write.csv(conc_pmf_cluster, "CSN_concentration_AQS.PM_PMF_C-sub_2023.05.csv")
write.csv(unc_pmf_cluster, "CSN_uncertainty_AQS.PM_PMF_C-sub_2023.05.csv")

###### CSN_MDL - EXTRACT data for each cluster for PMF GUI ###### 
conc_pmf = read.csv("CSN_concentration_AQS.PM_PMF_C-sub_2023.05.csv")
unc_pmf = read.csv("CSN_uncertainty_AQS.PM_PMF_C-sub_2023.05.csv")

# conc_pmf = read.csv("CSN_concentration_AQS.PM_PMF_C-subgroup_2023.04.csv")
# unc_pmf = read.csv("CSN_uncertainty_AQS.PM_PMF_C-subgroup_2023.04.csv")

# conc_pmf = read.csv("CSN_concentration_for_PMF_C-subgroup_2023.04.csv")
# unc_pmf = read.csv("CSN_uncertainty_for_PMF_C-subgroup_2023.04.csv")
csn_cluster = read.csv("CSN_RF_cluster5training.csv")
conc_pmf$X = unc_pmf$X = csn_cluster$X = NULL
conc_pmf$Date = as.Date(conc_pmf$Date)
unc_pmf$Date = as.Date(unc_pmf$Date)

conc_pmf = merge(conc_pmf, csn_cluster)
unc_pmf = merge(unc_pmf, csn_cluster)

# Create subfolders under the current working directory
Clusterfolder <- "PMF_GUI_Cluster"
# Clusterfolder <- "PMF_GUI_Cluster_originPM"
# dir.create(Clusterfolder)

Sitefolder <- "PMF_GUI_Site"
# Sitefolder <- "PMF_GUI_Site_originPM"
# dir.create(Sitefolder)

# extract concentration & uncertainty of single cluster for PMF
for( i in 1:25){
  # conc & unc for each cluster
  conc_cluster = subset(conc_pmf, Final.Decision == i)
  unc_cluster = subset(unc_pmf, Final.Decision == i)
  # row number of each site in a given cluster
  single_site_row = data.frame(table(conc_cluster$SiteCode))
  colnames(single_site_row)[1] = c("SiteCode")
  
  conc_cluster$Final.Decision = unc_cluster$Final.Decision = NULL

  conc_cluster = conc_cluster[with(conc_cluster, 
                                   order(SiteCode, Date)), ]
  unc_cluster = unc_cluster[with(unc_cluster, 
                                 order(SiteCode, Date)), ]
  
  # save files to direct folder
  write.csv(conc_cluster, 
            paste0(Clusterfolder, "/CSN_C", i, "_2011-20_conc.csv"))
  write.csv(unc_cluster, 
            paste0(Clusterfolder, "/CSN_C", i, "_2011-20_unc.csv"))
  write.csv(single_site_row, 
            paste0(Clusterfolder, "/CSN_C", i, "_2011-20_date_number_of_site.csv"))
  
  # extract concentration & uncertainty of single site for PMF
  for ( j in unique(conc_cluster$SiteCode)){
    # conc & unc for each site
    conc_site = subset(conc_pmf, SiteCode == j)
    unc_site = subset(unc_pmf, SiteCode == j)
    conc_site$Final.Decision = unc_site$Final.Decision = NULL
    
    # reorder by date
    conc_site = conc_site[with(conc_site, order(Date)), ]
    unc_site = unc_site[with(unc_site, order(Date)), ]
    
    # save files to direct folder
    write.csv(conc_site, 
              paste0(Sitefolder, "/CSN_C", i, "_S", j, "_2011-20_conc.csv"))
    write.csv(unc_site, 
              paste0(Sitefolder, "/CSN_C", i, "_S", j, "_2011-20_unc.csv"))
  }
}

### check the influence of qualifier, for data generated before 2023.03
### NO LONGER AVAILABLE, have all involved in missing & qualifier
conc_cluster6_1site_qlf5 = subset(csn_daily_conc_cluster6_1site, 
                                  grepl(5, Qualifier, fixed = T))
summary(conc_cluster6_1site_qlf5)
sapply(conc_cluster6_1site_qlf5, max)

## checking the ratio of concentrations below MDL for PMF resetting
csn_conc_mdl_Site_cluster6_1site = subset(csn_conc_mdl_Site,
                                          Date < as.Date("2014-01-01") &
                                            SiteCode == csn_select_site_row$SiteCode[3])
summary(csn_conc_mdl_Site_cluster6_1site)
round(sapply(
  csn_conc_mdl_Site_cluster6_1site[, 5:ncol(csn_conc_mdl_Site_cluster6_1site)], 
  sum)/356*100, 
  0)

#### Selection2: use the MDL from EPA, one value for all period ####
###### EPA_MDL - prepare site & date matched MDL ###### 
csn_daily_conc = csn_daily
csn_daily_conc$State = csn_daily_conc$Qualifier = NULL
csn_daily_conc$year = year(csn_daily_conc$Date)
csn_daily_conc$month = month(csn_daily_conc$Date)

# reorder the ROWs for matching
csn_daily_conc = csn_daily_conc[
  with(
    csn_daily_conc, 
    order(SiteCode, year, month)), ]

# move Date to the first column
csn_daily_conc =
  csn_daily_conc %>%
  relocate(Date, .before = SiteCode)

csn_daily_conc$year = csn_daily_conc$month = NULL

# get EPA MDL
csn_mdl_epa = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/CSN_EPA_BZ_MDL.csv")
csn_mdl_epa$X = csn_mdl_epa$data = NULL

# expand MDL file to daily measurement 
# previous slow method: csn_mdl = do.call("rbind", replicate(nrow(csn_daily_conc),csn_mdl_epa[1, ], simplify = F))
csn_mdl = csn_mdl_epa[rep(1, each = nrow(csn_daily_conc)), ]
setDT(csn_mdl)

# Again check if columns from concentration & new MDL datasets match
summary(colnames(csn_daily_conc)[3:ncol(csn_daily_conc)] == 
          colnames(csn_mdl))

###### EPA_MDL - estimate the uncertainty ###### 
# compare concentration and MDL of a given component
csn_conc = csn_daily_conc[, 3:ncol(csn_daily_conc)]
csn_conc_mdl = data.frame(Map(">", csn_conc, csn_mdl))
setDT(csn_conc_mdl)

# check variable class & dataset dimenssion
sapply(csn_conc, class)
sapply(csn_mdl, class)
sapply(csn_conc_mdl, class)
dim(csn_conc)
dim(csn_mdl)
dim(csn_conc_mdl)

# test uncertainty, error_fraction 
comp_error_fraction = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/CSN_k_Error Fraction.csv")
summary(colnames(comp_error_fraction)[2:ncol(comp_error_fraction)] == colnames(csn_conc_mdl))
comp_error_fraction$data = NULL

# expand error_fraction file to one with same row number as concentration file
# previous slow method: comp_ef = do.call("rbind", replicate(nrow(csn_conc_mdl), comp_error_fraction[1, ], simplify = F))
comp_ef = comp_error_fraction[rep(1, each = nrow(csn_daily_conc)), ]
setDT(comp_ef)

# assign data conditionally to concentration and uncertainty values for PMF
conc_pmf = csn_conc_mdl * csn_conc +
  (!csn_conc_mdl) * csn_mdl * 0.5
unc_pmf = csn_conc_mdl * (csn_mdl / 3 + comp_ef * csn_conc) +
  (!csn_conc_mdl) * 5/6 * csn_mdl
unc_pmf$PM25 = 3 * csn_daily_conc$PM25

###### EPA_MDL - signal-to-noise SNR S/N estimation ###### 
# according to EPA PMF 5.0 User Guide.pdf, function 5-3 & 5-4 from page 23
conc_unc_diff_pmf = (conc_pmf - unc_pmf)/unc_pmf * csn_conc_mdl
conc_unc_diff_pmf = cbind(csn_daily[, 1:4], conc_unc_diff_pmf)

# merge with cluster info
conc_unc_diff_pmf = merge(csn_cluster, conc_unc_diff_pmf)
conc_unc_diff_pmf$Date = conc_unc_diff_pmf$State = conc_unc_diff_pmf$Qualifier = NULL

# calculate SNR of each site 
site_snr = aggregate(. ~ as.factor(SiteCode), 
                     conc_unc_diff_pmf, 
                     sum) / 
  table(conc_unc_diff_pmf$SiteCode)
site_snr$`as.factor(SiteCode)` = NULL
site_snr$PM25 = -999

# calculate SNR of each cluster
cluster_snr = aggregate(. ~ as.factor(Final.Decision), 
                     conc_unc_diff_pmf, 
                     sum) / 
  table(conc_unc_diff_pmf$Final.Decision)
cluster_snr$`as.factor(Final.Decision)` = cluster_snr$SiteCode = NULL
cluster_snr$PM25 = -999

###### EPA_MDL - output files for all CSN conc, unc, snr etc. ###### 
# insert date into  concentration & uncertainty datasets
conc_pmf <- cbind(csn_daily[, 1:4], conc_pmf)
unc_pmf <- cbind(csn_daily[, 1:4], unc_pmf)

csn_conc_mdl_Site = cbind(csn_daily[, 1:4], csn_conc_mdl)

write.csv(conc_pmf, "CSN_concentration_for_PMF_EPAmdl.csv")
write.csv(unc_pmf, "CSN_uncertainty_for_PMF_EPAmdl.csv")
write.csv(csn_conc_mdl_Site, "CSN_concentration_vs_MDL_EPAmdl.csv")

write.csv(site_snr, "CSN_Site_SNR_for_PMF_EPAmdl.csv")
write.csv(cluster_snr, "CSN_Cluster_SNR_for_PMF_EPAmdl.csv")

###### EPA_MDL - daily PM of selected cluster - PMF priori try ###### 
# extract concentration & uncertainty of selected sites for PMF

# sites.top.PM.cluster
csn_select_conc = subset(conc_pmf, 
                         SiteCode %in% 
                           sites.top.PM.cluster)
csn_select_unc = subset(unc_pmf, 
                        SiteCode %in% 
                          sites.top.PM.cluster)

dim(csn_select_conc)
dim(csn_select_unc)

# sites.least.PM.cluster
csn_select_conc = subset(conc_pmf, 
                         SiteCode %in% 
                           sites.least.PM.cluster)
csn_select_unc = subset(unc_pmf, 
                        SiteCode %in% 
                          sites.least.PM.cluster)

csn_select_conc = csn_select_conc[!duplicated(csn_select_conc), ]
csn_select_unc = csn_select_unc[!duplicated(csn_select_unc), ]
dim(csn_select_conc)
dim(csn_select_unc)

# only use data before 2014 for priori try, those with top PM!!!
csn_select_conc_use = subset(csn_select_conc, Date < as.Date("2014-01-01"))
csn_select_unc_use = subset(csn_select_unc, Date < as.Date("2014-01-01"))

# only use data since 2018 for priori try, those with the lowest PM!!!
# multiple sites only have records from 2016
csn_select_conc_use = subset(csn_select_conc, Date >= as.Date("2018-01-01"))
csn_select_unc_use = subset(csn_select_unc, Date >= as.Date("2018-01-01"))

csn_select_site_row = data.frame(table(csn_select_conc_use$SiteCode))
colnames(csn_select_site_row)[1] = c("SiteCode")

csn_select_conc_use = csn_select_conc_use[!duplicated(csn_select_conc_use), ]
csn_select_unc_use = csn_select_unc_use[!duplicated(csn_select_unc_use), ]
dim(csn_select_conc_use)
dim(csn_select_unc_use)

# extract conc & unc data for selected sites
#site 3 & 4 for cluster with top PM and 2&1 for cluster with the lowest PM
csn_select_conc_1site = subset(csn_select_conc_use, 
                               SiteCode == csn_select_site_row$SiteCode[2]) 
csn_select_unc_1site = subset(csn_select_unc_use, 
                              SiteCode == csn_select_site_row$SiteCode[2]) 
dim(csn_select_conc_1site)
dim(csn_select_unc_1site)

csn_select_conc_use$State = csn_select_unc_use$State = 
  csn_select_conc_1site$State = csn_select_unc_1site$State = 
  csn_select_conc_use$Qualifier = csn_select_unc_use$Qualifier = 
  csn_select_conc_1site$Qualifier = csn_select_unc_1site$Qualifier = NULL

csn_select_conc_1site = csn_select_conc_1site[!duplicated(csn_select_conc_1site[, 1:2]), ]
csn_select_unc_1site = csn_select_unc_1site[!duplicated(csn_select_unc_1site[, 1:2]), ]
dim(csn_select_conc_1site)
dim(csn_select_unc_1site)

write.csv(csn_select_conc_use, "CSN_Cluster6_2011-14_PMF_conc_EPAmdl.csv")
write.csv(csn_select_unc_use, "CSN_Cluster6_2011-14_PMF_unc_EPAmdl.csv")
write.csv(csn_select_site_row, "CSN_Cluster6_date_number_of_site_EPAmdl.csv")

write.csv(csn_select_conc_1site, "CSN_Cluster6_site3_2011-14_PMF_conc_EPAmdl.csv") #site 3
write.csv(csn_select_unc_1site, "CSN_Cluster6_site3_2011-14_PMF_unc_EPAmdl.csv") #site 3

write.csv(csn_select_conc_1site, "CSN_Cluster6_site4_2011-14_PMF_conc_EPAmdl.csv")
write.csv(csn_select_unc_1site, "CSN_Cluster6_site4_2011-14_PMF_unc_EPAmdl.csv")

write.csv(csn_select_conc_use, "CSN_Cluster24_2011-14_PMF_conc_EPAmdl.csv")
write.csv(csn_select_unc_use, "CSN_Cluster24_2011-14_PMF_unc_EPAmdl.csv")
write.csv(csn_select_site_row, "CSN_Cluster24_date_number_of_site_EPAmdl.csv")

write.csv(csn_select_conc_1site, "CSN_Cluster24_site1_2011-14_PMF_conc_EPAmdl.csv") #site 3
write.csv(csn_select_unc_1site, "CSN_Cluster24_site1_2011-14_PMF_unc_EPAmdl.csv") #site 3

write.csv(csn_select_conc_1site, "CSN_Cluster24_site2_2011-14_PMF_conc_EPAmdl.csv")
write.csv(csn_select_unc_1site, "CSN_Cluster24_site2_2011-14_PMF_unc_EPAmdl.csv")

#### Convert file to fit for CMD (No GUI) running - ALL Cluster ####
###### DONE! the OOB Error & missing rate preparation DONE! ######
OOB_comb = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_OOBerror_random-forest_All.csv")
miss_comb = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_Missing_Qualifier_interpolation_All.csv")
# the miss is the combination of missing data and flagged data that has been removed and interpolated
OOB_comb$X = miss_comb$X = NULL

# set column for intersect check 
row.names(OOB_comb) = paste(OOB_comb$SiteCode, OOB_comb$Year)
row.names(miss_comb) = paste(miss_comb$SiteCode, miss_comb$Year)

# for miss_comb, only keep clusters appeared in OOB_comb
# get common rows 
obb_miss_rows <- intersect(row.names(OOB_comb), 
                           row.names(miss_comb))

# Subset data frames using common rownames and column names
OOB_comb <- OOB_comb[obb_miss_rows, ]
miss_comb <- miss_comb[obb_miss_rows, ]
row.names(OOB_comb) = row.names(miss_comb) = NULL

# Average of the time before & after 2015
OOB_comb_avg = aggregate(.~SiteCode, 
                         data=OOB_comb[, 1:(ncol(OOB_comb)-1)], 
                         mean)
miss_comb_avg = aggregate(.~SiteCode, 
                          data=miss_comb[, 1:(ncol(miss_comb)-1)], 
                          mean)
write.csv(OOB_comb_avg, "/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_OOBerror_random-forest_for_PMF.csv")
write.csv(miss_comb_avg, "/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_Missing_Qualifier_interpolation_for_PMF.csv")

################################################################################
###### 11111 START for single clusters -- ALL Values!!######
################################################################################
###### read data files ######
# read data about when the conc is above the unc
# csn_conc_mdl_cluster = read.csv("CSN_concentration_vs_MDL_EPAmdl.csv")

# No carbon subgroups
# csn_conc_mdl_cluster = read.csv("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE/CSN_concentration_vs_MDL_2023.04.csv") # original MDL in CSN

# with carbonaceous substances
# csn_conc_mdl_cluster = read.csv("CSN_concentration_vs_MDL_C-subgroup_2023.04.csv") # original MDL in CSN
csn_conc_mdl_cluster = read.csv("CSN_conc_vs_MDL_C-subgroup_corrected_2023.05.csv") # original MDL in CSN

csn_conc_mdl_cluster$X = NULL
csn_conc_mdl_cluster$Date = as.Date(csn_conc_mdl_cluster$Date)

# Signal-to-noise ratio
# cluster_snr = read.csv("CSN_Cluster_SNR_for_PMF.csv") # No carbon subgroups
cluster_snr = read.csv("CSN_Cluster_SNR_for_PMF_C-subgroup.csv") # carbonaceous substances
cluster_snr$X = NULL

# OOB & Missing rate
OOB_comb_avg = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_OOBerror_random-forest_for_PMF.csv")
miss_comb_avg = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_Missing_Qualifier_interpolation_for_PMF.csv")
OOB_comb_avg$X = miss_comb_avg$X = NULL

# read data of conc & unc
conc_pmf = read.csv("CSN_concentration_AQS.PM_PMF_C-sub_2023.05.csv")
unc_pmf = read.csv("CSN_uncertainty_AQS.PM_PMF_C-sub_2023.05.csv")

# conc_pmf = read.csv("CSN_concentration_AQS.PM_PMF_C-subgroup_2023.04.csv")
# unc_pmf = read.csv("CSN_uncertainty_AQS.PM_PMF_C-subgroup_2023.04.csv")

# conc_pmf = read.csv("CSN_concentration_for_PMF_C-subgroup_2023.04.csv")
#  unc_pmf = read.csv("CSN_uncertainty_for_PMF_C-subgroup_2023.04.csv")
csn_cluster = read.csv("CSN_RF_cluster5training.csv")
conc_pmf$X = unc_pmf$X = csn_cluster$X = NULL
conc_pmf$Date = as.Date(conc_pmf$Date)
unc_pmf$Date = as.Date(unc_pmf$Date)

conc_pmf = merge(conc_pmf, csn_cluster)
unc_pmf = merge(unc_pmf, csn_cluster)

# the sequence may change after merging, reorder to ensure the sequence for those need row-by-row process
conc_pmf = conc_pmf[with(
  conc_pmf, 
  order(State, SiteCode, Date)), ]
unc_pmf = unc_pmf[with(
  unc_pmf, 
  order(State, SiteCode, Date)), ]
csn_conc_mdl_cluster = csn_conc_mdl_cluster[with(
  csn_conc_mdl_cluster, 
  order(State, SiteCode, Date)), ]

##### if no OC EC subgroups 
OC.EC.sub = c("EC1", "EC2", "EC3",
              "OC1", "OC2", "OC3", "OC4")
conc_pmf[ , OC.EC.sub] <- list(NULL)
unc_pmf[ , OC.EC.sub] <- list(NULL)
OOB_comb_avg[ , OC.EC.sub] <- list(NULL)
miss_comb_avg[ , OC.EC.sub] <- list(NULL)
cluster_snr[ , OC.EC.sub] <- list(NULL)
csn_conc_mdl_cluster[ , OC.EC.sub] <- list(NULL)

# create a dataframe to save the decision of weak, bad, or strong 
# species.name = colnames(conc_pmf)[5:(ncol(conc_pmf) - 1)]
species.name = colnames(conc_pmf)[5:(ncol(conc_pmf) - 1)] ## if no OC EC subgroups 
Finaly.Decision = 1:25
cmd_species_class_cluster = data.frame(
  matrix(ncol = length(species.name), 
         nrow = length(Finaly.Decision)))
colnames(cmd_species_class_cluster) = species.name
# add PM2.5 and other variables
cmd_species_class_cluster$PM25 = 0
cmd_species_class_cluster$Finaly.Decision = Finaly.Decision
cmd_species_class_cluster$sum.weak.good = 0
cmd_species_class_cluster$style.weak.good = 0
cmd_species_class_cluster$cluster.row = 0

# define the subfolder to save cluster files
Clusterfolder <- "PMF_NoGUI_cluster"
# Clusterfolder <- "PMF_NoGUI_NoCsub_cluster" ## if no OC EC subgroups 
# Clusterfolder <- "PMF_NoGUI_cluster_originPM"

# extract concentration & uncertainty of single cluster for PMF
for( i in 1:25){
  # conc & unc for each cluster
  conc_cluster = subset(conc_pmf, Final.Decision == i)
  unc_cluster = subset(unc_pmf, Final.Decision == i)
  dim(unc_cluster)
  
  cluster.No = i
  conc_cluster$Final.Decision = unc_cluster$Final.Decision = NULL

  # site included in the cluster
  sites.in.cluster = unique(conc_cluster$SiteCode)
  
  # extract the conc_vs._mdl data of selected cluster(s)
  conc_mdl_cluster = subset(csn_conc_mdl_cluster,
                            SiteCode %in% sites.in.cluster)
  
  conc_mdl_cluster$State = NULL
  
  # double check if row and columns are matched
  summary(conc_mdl_cluster$Date == unc_cluster$Date &
            conc_mdl_cluster$SiteCode == unc_cluster$SiteCode)

  ## checking the ratio of concentrations above MDL for PMF resetting for selected cluster
  conc_cluster_aboveMDL = data.frame(round(sapply(
    conc_mdl_cluster[, 3:ncol(conc_mdl_cluster)], sum)/
      nrow(conc_mdl_cluster)*100, 0))
  
  conc_cluster_aboveMDL$CompName = rownames(conc_cluster_aboveMDL)
  colnames(conc_cluster_aboveMDL)[1] = "Percent"
  
  # conc_cluster_aboveMDL$Percent[conc_cluster_aboveMDL$CompName == "S"] = 
  #  conc_site_aboveMDL$Percent[conc_site_aboveMDL$CompName == "SO4"] 
  
  # Create factor levels for CompName in the order we want them to appear
  conc_cluster_aboveMDL$CompName <- factor(conc_cluster_aboveMDL$CompName, 
                                           levels = unique(conc_cluster_aboveMDL$CompName))
  
  # plot the percent of below_MDL_concentrations for each species
  # ggplot(conc_cluster_aboveMDL, aes(CompName, Percent)) +
  # ggplot(conc_cluster_aboveMDL, aes(CompName, Percent)) +
  #   geom_point() +
  #   geom_hline(yintercept = 10, color = "red") +
  #   geom_hline(yintercept = 20, color = "orange", linetype='dotted') +
  #   annotate("text", x = 15, y = 20, label = "Weak  ↓", vjust = -0.5, col = "orange") +
  #   annotate("text", x = 15, y =10, label = "Bad  ↓", vjust = -0.5, col = "red") +
  #   ylab("Percent of below-CSN_MDL concentration") +
  #   theme_bw()
  
  # get the SNR signal-to-noise ratio 
  snr_selected_cluster = subset(cluster_snr, 
                                Final.Decision == i)
  snr_selected_cluster$Final.Decision = NULL
  
  ###### Cluster-bad&weak 1. Strict SNR & Strict MDL - selected ######
  # generate the weak & bad species based on the conc_vs_mdl 
  cluster.species.weak.Pmdl = conc_cluster_aboveMDL$CompName[
    conc_cluster_aboveMDL$Percent <= 50 &
      conc_cluster_aboveMDL$Percent > 20]
  cluster.species.bad.Pmdl = conc_cluster_aboveMDL$CompName[
    conc_cluster_aboveMDL$Percent <= 20]
  # change to character
  cluster.species.weak.Pmdl = as.character(cluster.species.weak.Pmdl)
  cluster.species.bad.Pmdl = as.character(cluster.species.bad.Pmdl)
  
  # generate the weak & bad species based on SNR 
  cluster.species.bad.snr = colnames(snr_selected_cluster)[which(
    colSums(snr_selected_cluster) < 0.2)]
  cluster.species.weak.snr = colnames(snr_selected_cluster)[which(
    colSums(snr_selected_cluster) >= 0.2 &
      colSums(snr_selected_cluster) < 2)]
  
  ###### Cluster-bad&weak 2. Combine the rate need interpolation & OBB error ######
  # Extract data for cluster
  cluster_oob = subset(OOB_comb_avg, 
                       SiteCode %in% sites.in.cluster)
  cluster_miss = subset(miss_comb_avg, 
                        SiteCode %in% sites.in.cluster)
  
  # get cluster average
  cluster_oob = data.frame(sapply(cluster_oob, mean))
  colnames(cluster_oob)[1] = "OOB"
  
  cluster_miss = data.frame(sapply(cluster_miss, mean))
  colnames(cluster_miss)[1] = "Miss.Flag"
  
  cluster_oob_miss = cbind(cluster_oob, cluster_miss)
  cluster_oob_miss$CompName = row.names(cluster_oob_miss)
  # only keep rows for CompName
  cluster_oob_miss = cluster_oob_miss[2:nrow(cluster_oob_miss), ]
  
  row.names(cluster_oob_miss) = NULL
  
  # bad, weak
  cluster.oob.miss.weak = cluster_oob_miss$CompName[
    (cluster_oob_miss$Miss.Flag <= 15 & 
       cluster_oob_miss$Miss.Flag > 10 &
       cluster_oob_miss$OOB <= 10 &
       cluster_oob_miss$OOB > 5) | 
      (cluster_oob_miss$Miss.Flag <= 10 &
         cluster_oob_miss$Miss.Flag > 5 &
         cluster_oob_miss$OOB > 8)]
  cluster.oob.miss.bad = cluster_oob_miss$CompName[
    cluster_oob_miss$Miss.Flag > 15 |
      (cluster_oob_miss$Miss.Flag <= 15 & 
         cluster_oob_miss$Miss.Flag > 10 &
         cluster_oob_miss$OOB > 5)]
  
  ###### Cluster-bad&weak 3. other criteria ######
  
  # combine the "weak" & "bad" list
  cluster.species.bad = append(cluster.species.bad.snr, 
                               cluster.species.bad.Pmdl)
  cluster.species.bad = append(cluster.species.bad, 
                               cluster.oob.miss.bad)
  
  cluster.species.weak = append(cluster.species.weak.snr, 
                                cluster.species.weak.Pmdl)
  cluster.species.weak = append(cluster.species.weak, 
                                cluster.oob.miss.weak)
  
  # remove PM25, which potentially exists in the weak/bad list
  cluster.species.bad = cluster.species.bad[! cluster.species.bad %in% "PM25"]
  cluster.species.weak = cluster.species.weak[! cluster.species.weak %in% "PM25"]
  
  # if OC/EC/OPC is in bad, force remove to weak
  oc.ec = c("EC", "EC1", "EC2", "EC3", 
            "OC", "OC1", "OC2", 
            "OC3", "OC4", "OP")
  
  oc.ec.bad = cluster.species.bad[cluster.species.bad %in% oc.ec]
  cluster.species.bad = cluster.species.bad[! cluster.species.bad %in% oc.ec]
  cluster.species.weak = append(cluster.species.weak, 
                                oc.ec.bad)
  
  # if a species exist both in "bad" & "weak", define as "WEAK"
  cluster.species.bad = cluster.species.bad[! (
    cluster.species.bad %in% cluster.species.weak)]
  
  # add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
  cluster.species.bad = append(c("S", "Na", "K"), 
                               cluster.species.bad) 
  # in case any of this three is in weak, if so, remove
  cluster.species.weak = cluster.species.weak[! (
    cluster.species.weak %in% c("S", "Na", "K"))]
  
  # add Al, Mg, Na into weak, if there are not in bad, due to the lower reliability in XRF test
  Al.Mg.weak = c("Al", "Mg")
  # determine if Al or Mg is in bad, if so, remove the one in "Al.Mg.weak"
  Al.Mg.weak = Al.Mg.weak[! (
    Al.Mg.weak %in% cluster.species.bad)]
  # add the rest in Al.Mg.weak to "weak"
  cluster.species.weak = append(Al.Mg.weak, 
                                cluster.species.weak) 
  # "Na", "K" were removed, and use Na+, K+ instead

  # remove the duplicated
  # cluster.species.weak = cluster.species.weak[!duplicated(cluster.species.weak)]
  
  # remove the duplicated strings from the character vector
  cluster.species.bad = unique(
    unlist(
      strsplit(
        cluster.species.bad, " ")))
  cluster.species.weak = unique(
    unlist(
      strsplit(
        cluster.species.weak, " ")))
  
  ###### Cluster-bad&weak 4. Distribution of values above MDL ######
  # species stay in "bad" and related to higher Pmdl
  cluster.species.bad.check = cluster.species.bad[
    cluster.species.bad %in% 
      cluster.species.bad.Pmdl]
  
  # remove those with >95% of value below MDL
  cluster.species.bad.check = 
    unique(
      subset(conc_cluster_aboveMDL, 
             CompName %in% cluster.species.bad.check &
               Percent > 5)$CompName)
  
  # cluster.species.bad.check = c("Ag", "Ca")
  if(length(cluster.species.bad.check) > 0){
    # files for those with above MDL concentrations
    conc_cluster_mdl = conc_cluster[, 4:ncol(conc_cluster)] *
      conc_mdl_cluster[, 3:ncol(conc_mdl_cluster)]
    
    # subset those grouped as bad
    conc_cluster_bad = conc_cluster_mdl %>%
      select(all_of(cluster.species.bad.check))
    
    # replace 0 by NA
    conc_cluster_bad <-
      data.frame(
        apply(
          conc_cluster_bad, 
          2, 
          function(x) 
            replace(x, x == 0, NA)))
    
    # for concentrations above MDL, check the 10th vs. 90th percentile ratio
    # for some random comparison, ratio < 0.1 for strong species and < 0.2 for weak 
    # or, change to compare mean vs. sd? if mean < sd, or if mean > sd
    conc_cluster_bad_mean = 
      conc_cluster_bad %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          list(
            p = ~mean(., na.rm = T)
          )
        ))
    
    conc_cluster_bad_sd = 
      conc_cluster_bad %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          list(
            p = ~sd(., na.rm = T)
          )
        ))
    
    bad_sd_mean = conc_cluster_bad_sd/conc_cluster_bad_mean
    colnames(bad_sd_mean) = cluster.species.bad.check
    
    # Detect the species for which the sd > mean
    bad_comp <- which(colSums(bad_sd_mean > 1) > 0)
    cluster.species.bad.remove <- colnames(bad_sd_mean)[bad_comp]
    
  } else{
    cluster.species.bad.remove = NA
  }
  
  # remove the species with very scatter distribution from bad and add it into weak 
  cluster.species.bad = 
    cluster.species.bad[
      !(cluster.species.bad %in% 
          cluster.species.bad.remove)]
  
  cluster.species.weak = append(cluster.species.weak, 
                                cluster.species.bad.remove)
  cluster.species.weak = cluster.species.weak[
    !is.na(cluster.species.weak)]
  
  # arrange in alphabetic order 
  cluster.species.bad = sort(cluster.species.bad)
  cluster.species.weak = sort(cluster.species.weak)
  cluster.species.weak = append(cluster.species.weak, "PM25")
  cluster.species.strong = unique(
    subset(conc_cluster_aboveMDL, 
           !(CompName %in% cluster.species.bad |
               CompName %in% cluster.species.weak))$CompName)
  cluster.species.strong = cluster.species.strong[! cluster.species.strong %in% "PM25"]
  cluster.species.strong = as.character(cluster.species.strong)
  
  ###### Cluster- final 1: files of conc & unc for CMD ######
  # remove species marked as bad
  conc_cluster_pmf = conc_cluster[ ,-which(
    names(conc_cluster) %in% cluster.species.bad)]
  unc_cluster_pmf = unc_cluster[ ,-which(
    names(conc_cluster) %in% cluster.species.bad)]
  
  # for those marked as weak, set the uncertainty 3 times as much
  # unc_cluster_pmf_1 = unc_cluster_pmf
  
  unc_cluster_pmf[names(unc_cluster_pmf) %in% cluster.species.weak] = 
    3 * unc_cluster_pmf[names(unc_cluster_pmf) %in% cluster.species.weak]
  
  # for PM2.5, set the uncertainty 3*3 times as much
  unc_cluster_pmf$PM25 = 3 * 3 * conc_cluster_pmf$PM25
  
  #head(unc_cluster_pmf_1$Ca); head(unc_cluster_pmf$Ca); head(conc_cluster_pmf$Ca)
  #head(unc_cluster_pmf_1$Al); head(unc_cluster_pmf$Al); head(conc_cluster_pmf$Al)
  #head(unc_cluster_pmf_1$PM25); head(unc_cluster_pmf$PM25); head(conc_cluster_pmf$PM25)
  
  # conc_cluster.1 = conc_cluster; unc_cluster.1 = unc_cluster
  # add info into column names before combination
  colnames(conc_cluster_pmf) = paste0("conc_", colnames(conc_cluster_pmf))
  colnames(unc_cluster_pmf) = paste0("unc_", colnames(unc_cluster_pmf))
  
  # reorder before combine
  conc_cluster_pmf = conc_cluster_pmf[with(conc_cluster_pmf,
                                           order(conc_SiteCode, conc_Date)), ]
  unc_cluster_pmf = unc_cluster_pmf[with(unc_cluster_pmf,
                                         order(unc_SiteCode, unc_Date)), ]
    
  # combining conc & unc files
  cmd_cluster_conc_unc = cbind(conc_cluster_pmf, unc_cluster_pmf)
  
  # Interleave columns of concentration and uncertainty files
  ## generate a new vector to index the columns of cbind file
  interleave.cluster <- 
    rep(1:ncol(unc_cluster_pmf), each = 2) + 
    (0:1) * ncol(unc_cluster_pmf)
  ## reorder the cbind file
  cmd_cluster_interleave = cmd_cluster_conc_unc[interleave.cluster]
  
  # only keep one Date & cluster column
  cmd_cluster_interleave$conc_Date = 
    cmd_cluster_interleave$conc_State = 
    cmd_cluster_interleave$conc_SiteCode = NULL
  colnames(cmd_cluster_interleave)[1:3] = 
    c("SiteCode", "Date", "State")
  head(cmd_cluster_interleave)
  
  cmd_cluster_interleave = 
    cmd_cluster_interleave[
      with(cmd_cluster_interleave, 
           order(SiteCode, Date)), ]
  
  write.csv(cmd_cluster_interleave, 
            paste0(Clusterfolder, "/CSN_C_", 
                   cluster.No, "_PMF_CMD.csv"))
 
  write.csv(cmd_cluster_interleave, 
            paste0(Clusterfolder, "/CSN_noCsub_C_", 
                   cluster.No, "_PMF_CMD.csv"))
  
  ###### Cluster- final 2: files of weak, bad, strong for CMD ######
  cmd_species_class_cluster[i, cluster.species.strong] <- 1
  cmd_species_class_cluster[i, cluster.species.weak] <- 0
  cmd_species_class_cluster[i, cluster.species.bad] <- NA
  
  cmd_species_class_cluster$sum.weak.good[i] = 
    length(cluster.species.weak) +
    length(cluster.species.strong) 
    
  cmd_species_class_cluster$cluster.row[i] = nrow(conc_cluster)
  # detect the species for None-GUI PMF, thus, weak and strong species
  noGUI_values <- 
    cmd_species_class_cluster[
      i, 
      c(cluster.species.strong, cluster.species.weak)]
  species.name.use = species.name[
    species.name %in% 
      c(cluster.species.strong, cluster.species.weak)]
  
  # add PM25
  species.name.use = append(species.name.use, "PM25")
  
  noGUI_values <- noGUI_values[, species.name.use]
  cmd_species_class_cluster$style.weak.good[i] <- 
    paste0("/", noGUI_values, collapse = "")
  
  write.csv(cmd_species_class_cluster, 
            paste0(Clusterfolder, "/CSN_PMF_CMD_StrongWeakBad_Cluster.csv"))
  
  write.csv(cmd_species_class_cluster, 
            paste0(Clusterfolder, "/CSN_noCsub_PMF_CMD_StrongWeakBad_Cluster.csv"))
}


################################################################################
###### 2222 START for single clusters -- NO EXTREMES!! for GUI & noGUI######
################################################################################
###### read data files ######
# read data about when the conc is above the unc
# csn_conc_mdl_cluster = read.csv("CSN_concentration_vs_MDL_EPAmdl.csv")

# No carbon subgroups
# csn_conc_mdl_cluster = read.csv("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE/CSN_concentration_vs_MDL_2023.04.csv") # original MDL in CSN

# with carbonaceous substances
# csn_conc_mdl_cluster = read.csv("CSN_concentration_vs_MDL_C-subgroup_2023.04.csv") # original MDL in CSN
csn_conc_mdl_cluster = read.csv("CSN_conc_vs_MDL_C-subgroup_corrected_2023.05.csv") # original MDL in CSN

csn_conc_mdl_cluster$X = NULL
csn_conc_mdl_cluster$Date = as.Date(csn_conc_mdl_cluster$Date)

# Signal-to-noise ratio
# cluster_snr = read.csv("CSN_Cluster_SNR_for_PMF.csv") # No carbon subgroups
cluster_snr = read.csv("CSN_Cluster_SNR_for_PMF_C-subgroup.csv") # carbonaceous substances
cluster_snr$X = NULL

# OOB & Missing rate
OOB_comb_avg = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_OOBerror_random-forest_for_PMF.csv")
miss_comb_avg = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_Missing_Qualifier_interpolation_for_PMF.csv")
OOB_comb_avg$X = miss_comb_avg$X = NULL

# read data of conc & unc
conc_pmf = read.csv("CSN_concentration_AQS.PM_PMF_C-sub_2023.05.csv")
unc_pmf = read.csv("CSN_uncertainty_AQS.PM_PMF_C-sub_2023.05.csv")

# conc_pmf = read.csv("CSN_concentration_AQS.PM_PMF_C-subgroup_2023.04.csv")
# unc_pmf = read.csv("CSN_uncertainty_AQS.PM_PMF_C-subgroup_2023.04.csv")

# conc_pmf = read.csv("CSN_concentration_for_PMF_C-subgroup_2023.04.csv")
#  unc_pmf = read.csv("CSN_uncertainty_for_PMF_C-subgroup_2023.04.csv")
csn_cluster = read.csv("CSN_RF_cluster5training.csv")
conc_pmf$X = unc_pmf$X = csn_cluster$X = NULL
conc_pmf$Date = as.Date(conc_pmf$Date)
unc_pmf$Date = as.Date(unc_pmf$Date)

conc_pmf = merge(conc_pmf, csn_cluster)
unc_pmf = merge(unc_pmf, csn_cluster)

# the sequence may change after merging, reorder to ensure the sequence for those need row-by-row process
conc_pmf = conc_pmf[with(
  conc_pmf, 
  order(State, SiteCode, Date)), ]
unc_pmf = unc_pmf[with(
  unc_pmf, 
  order(State, SiteCode, Date)), ]
csn_conc_mdl_cluster = csn_conc_mdl_cluster[with(
  csn_conc_mdl_cluster, 
  order(State, SiteCode, Date)), ]

##### if no OC EC subgroups 
OC.EC.sub = c("EC1", "EC2", "EC3",
              "OC1", "OC2", "OC3", "OC4")
conc_pmf[ , OC.EC.sub] <- list(NULL)
unc_pmf[ , OC.EC.sub] <- list(NULL)
OOB_comb_avg[ , OC.EC.sub] <- list(NULL)
miss_comb_avg[ , OC.EC.sub] <- list(NULL)
cluster_snr[ , OC.EC.sub] <- list(NULL)
csn_conc_mdl_cluster[ , OC.EC.sub] <- list(NULL)

# create a dataframe to save the decision of weak, bad, or strong 
# species.name = colnames(conc_pmf)[4:(ncol(conc_pmf) - 1)]
species.name = colnames(conc_pmf)[5:(ncol(conc_pmf) - 1)] ## if no OC EC subgroups 
Finaly.Decision = 1:25
cmd_species_class_cluster = data.frame(
  matrix(ncol = length(species.name), 
         nrow = length(Finaly.Decision)))
colnames(cmd_species_class_cluster) = species.name
# add PM2.5 and other variables
cmd_species_class_cluster$PM25 = 0
cmd_species_class_cluster$Finaly.Decision = Finaly.Decision
cmd_species_class_cluster$sum.weak.good = 0
cmd_species_class_cluster$style.weak.good = 0
cmd_species_class_cluster$cluster.row = 0
head(cmd_species_class_cluster)

# define the subfolder to save cluster files
Clusterfolder <- "PMF_NoGUI_NoCsub_NoExtreme_cluster" ## if no OC EC subgroups, no extreme values 
GUI.cluster.folder <- "PMF_GUI_NoCsub_NoExtreme_cluster"

###### start the loop ######
# extract concentration & uncertainty of single cluster for PMF
for( i in 1:25){
  # conc & unc for each cluster
  conc_cluster = subset(conc_pmf, Final.Decision == i)
  unc_cluster = subset(unc_pmf, Final.Decision == i)
  dim(unc_cluster)
  
  cluster.No = i
  conc_cluster$Final.Decision = unc_cluster$Final.Decision = NULL
  
  ###### determine the extremes ######
  summary(conc_cluster)
  # med_conc_cluster = sapply(conc_cluster[4:ncol(conc_cluster)], median)
  mean_conc_cluster = sapply(conc_cluster[4:ncol(conc_cluster)], mean)
  max_conc_cluster = sapply(conc_cluster[4:ncol(conc_cluster)], max)
  # med_conc_cluster*25 < max_conc_cluster
  
  # mean value and 25 times were selected due to the data observation.
  # however, for some site, mean * 20 may alreay miss some very high values 
  # for earth elements (Si, Al), mean * 20 may filter too many values
  extreme_conc_cluster = mean_conc_cluster * 25
  
  # identify rows in conc_cluster with values higher than the extreme values
  rows_to_remove <- apply(conc_cluster[, 4:ncol(conc_cluster)], 1, 
                          function(row) 
                            any(row > extreme_conc_cluster))
  removed.row.count = sum(rows_to_remove)
  
  # extract the date and site infor of the rows to be removed
  remove_date_site = select(conc_cluster[rows_to_remove, ], 
                            SiteCode, Date)
  
  # remove rows with extreme values from conc_cluster and corresponding rows from unc_cluster
  conc_cluster <- conc_cluster[!rows_to_remove, ]
  unc_cluster <- unc_cluster[!rows_to_remove, ]
  dim(unc_cluster)

  # site included in the cluster
  sites.in.cluster = unique(conc_cluster$SiteCode)
  
  # extract the conc_vs._mdl data of selected cluster(s)
  conc_mdl_cluster = subset(csn_conc_mdl_cluster,
                            SiteCode %in% sites.in.cluster)
  
  # Remove rows in conc_mdl_cluster based on Site and Date groups in remove_date_site
  conc_mdl_cluster <- conc_mdl_cluster %>%
    anti_join(remove_date_site, by = c("SiteCode", "Date"))
  
  conc_mdl_cluster$State = NULL
  
  # double check if row and columns are matched
  summary(conc_mdl_cluster$Date == unc_cluster$Date &
            conc_mdl_cluster$SiteCode == unc_cluster$SiteCode)
  
  ###### the ratio of concentrations above MDL ######
  ## checking the ratio of concentrations above MDL for PMF resetting for selected cluster
  conc_cluster_aboveMDL = data.frame(round(sapply(
    conc_mdl_cluster[, 3:ncol(conc_mdl_cluster)], sum)/
      nrow(conc_mdl_cluster)*100, 0))
  
  conc_cluster_aboveMDL$CompName = rownames(conc_cluster_aboveMDL)
  colnames(conc_cluster_aboveMDL)[1] = "Percent"
  
  # conc_cluster_aboveMDL$Percent[conc_cluster_aboveMDL$CompName == "S"] = 
  #  conc_site_aboveMDL$Percent[conc_site_aboveMDL$CompName == "SO4"] 
  
  # Create factor levels for CompName in the order we want them to appear
  conc_cluster_aboveMDL$CompName <- factor(conc_cluster_aboveMDL$CompName, 
                                           levels = unique(conc_cluster_aboveMDL$CompName))
  
  # plot the percent of below_MDL_concentrations for each species
  # ggplot(conc_cluster_aboveMDL, aes(CompName, Percent)) +
  # ggplot(conc_cluster_aboveMDL, aes(CompName, Percent)) +
  #   geom_point() +
  #   geom_hline(yintercept = 10, color = "red") +
  #   geom_hline(yintercept = 20, color = "orange", linetype='dotted') +
  #   annotate("text", x = 15, y = 20, label = "Weak  ↓", vjust = -0.5, col = "orange") +
  #   annotate("text", x = 15, y =10, label = "Bad  ↓", vjust = -0.5, col = "red") +
  #   ylab("Percent of below-CSN_MDL concentration") +
  #   theme_bw()
  
  
  ###### Re-estimate signal-to-noise SNR after removing extremes ######
  conc.col = ncol(conc_cluster)
  csn_conc_mdl = conc_mdl_cluster[, 3:(conc.col-1)]
  
  # according to EPA PMF 5.0 User Guide.pdf, function 5-3 & 5-4 from page 23
  conc_unc_diff_cluster = (conc_cluster[, 4:conc.col] - 
                         unc_cluster[, 4:conc.col])/
    unc_cluster[, 4:conc.col] * csn_conc_mdl
  
  conc_unc_diff_cluster$PM25 = -999
  
  # get the SNR signal-to-noise ratio 
  snr_selected_cluster = 
    colSums(conc_unc_diff_cluster)/
    nrow(conc_unc_diff_cluster)
  # convert the result to a one-row data.frame for later match
  snr_selected_cluster = data.frame(t(snr_selected_cluster))  

  ###### Cluster-bad&weak 1. Strict SNR & Strict MDL - selected ######
  # generate the weak & bad species based on the conc_vs_mdl 
  cluster.species.weak.Pmdl = conc_cluster_aboveMDL$CompName[
    conc_cluster_aboveMDL$Percent <= 50 &
      conc_cluster_aboveMDL$Percent > 20]
  cluster.species.bad.Pmdl = conc_cluster_aboveMDL$CompName[
    conc_cluster_aboveMDL$Percent <= 20]
  # change to character
  cluster.species.weak.Pmdl = as.character(cluster.species.weak.Pmdl)
  cluster.species.bad.Pmdl = as.character(cluster.species.bad.Pmdl)
  
  # generate the weak & bad species based on SNR 
  cluster.species.bad.snr = colnames(snr_selected_cluster)[which(
    colSums(snr_selected_cluster) < 0.2)]
  cluster.species.weak.snr = colnames(snr_selected_cluster)[which(
    colSums(snr_selected_cluster) >= 0.2 &
      colSums(snr_selected_cluster) < 2)]
  
  ###### Cluster-bad&weak 2. Combine the rate need interpolation & OBB error ######
  # Extract data for cluster
  cluster_oob = subset(OOB_comb_avg, 
                       SiteCode %in% sites.in.cluster)
  cluster_miss = subset(miss_comb_avg, 
                        SiteCode %in% sites.in.cluster)
  
  # get cluster average
  cluster_oob = data.frame(sapply(cluster_oob, mean))
  colnames(cluster_oob)[1] = "OOB"
  
  cluster_miss = data.frame(sapply(cluster_miss, mean))
  colnames(cluster_miss)[1] = "Miss.Flag"
  
  cluster_oob_miss = cbind(cluster_oob, cluster_miss)
  cluster_oob_miss$CompName = row.names(cluster_oob_miss)
  # only keep rows for CompName
  cluster_oob_miss = cluster_oob_miss[2:nrow(cluster_oob_miss), ]
  
  row.names(cluster_oob_miss) = NULL
  
  # bad, weak
  cluster.oob.miss.weak = cluster_oob_miss$CompName[
    (cluster_oob_miss$Miss.Flag <= 15 & 
       cluster_oob_miss$Miss.Flag > 10 &
       cluster_oob_miss$OOB <= 10 &
       cluster_oob_miss$OOB > 5) | 
      (cluster_oob_miss$Miss.Flag <= 10 &
         cluster_oob_miss$Miss.Flag > 5 &
         cluster_oob_miss$OOB > 8)]
  cluster.oob.miss.bad = cluster_oob_miss$CompName[
    cluster_oob_miss$Miss.Flag > 15 |
      (cluster_oob_miss$Miss.Flag <= 15 & 
         cluster_oob_miss$Miss.Flag > 10 &
         cluster_oob_miss$OOB > 5)]
  
  ###### Cluster-bad&weak 3. other criteria ######
  
  # combine the "weak" & "bad" list
  cluster.species.bad = append(cluster.species.bad.snr, 
                               cluster.species.bad.Pmdl)
  cluster.species.bad = append(cluster.species.bad, 
                               cluster.oob.miss.bad)
  
  cluster.species.weak = append(cluster.species.weak.snr, 
                                cluster.species.weak.Pmdl)
  cluster.species.weak = append(cluster.species.weak, 
                                cluster.oob.miss.weak)
  
  # remove PM25, which potentially exists in the weak/bad list
  cluster.species.bad = cluster.species.bad[! cluster.species.bad %in% "PM25"]
  cluster.species.weak = cluster.species.weak[! cluster.species.weak %in% "PM25"]
  
  # if OC/EC/OPC is in bad, force remove to weak
  oc.ec = c("EC", "EC1", "EC2", "EC3", 
            "OC", "OC1", "OC2", 
            "OC3", "OC4", "OP")
  
  oc.ec.bad = cluster.species.bad[cluster.species.bad %in% oc.ec]
  cluster.species.bad = cluster.species.bad[! cluster.species.bad %in% oc.ec]
  cluster.species.weak = append(cluster.species.weak, 
                                oc.ec.bad)
  
  # if a species exist both in "bad" & "weak", define as "WEAK"
  cluster.species.bad = cluster.species.bad[! (
    cluster.species.bad %in% cluster.species.weak)]
  
  # add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
  cluster.species.bad = append(c("S", "Na", "K"), 
                               cluster.species.bad) 
  # in case any of this three is in weak, if so, remove
  cluster.species.weak = cluster.species.weak[! (
    cluster.species.weak %in% c("S", "Na", "K"))]
  
  # add Al, Mg, Na into weak, if there are not in bad, due to the lower reliability in XRF test
  Al.Mg.weak = c("Al", "Mg")
  # determine if Al or Mg is in bad, if so, remove the one in "Al.Mg.weak"
  Al.Mg.weak = Al.Mg.weak[! (
    Al.Mg.weak %in% cluster.species.bad)]
  # add the rest in Al.Mg.weak to "weak"
  cluster.species.weak = append(Al.Mg.weak, 
                                cluster.species.weak) 
  # "Na", "K" were removed, and use Na+, K+ instead
  
  # remove the duplicated
  # cluster.species.weak = cluster.species.weak[!duplicated(cluster.species.weak)]
  
  # remove the duplicated strings from the character vector
  cluster.species.bad = unique(
    unlist(
      strsplit(
        cluster.species.bad, " ")))
  cluster.species.weak = unique(
    unlist(
      strsplit(
        cluster.species.weak, " ")))
  
  ###### Cluster-bad&weak 4. Distribution of values above MDL ######
  # species stay in "bad" and related to higher Pmdl
  cluster.species.bad.check = cluster.species.bad[
    cluster.species.bad %in% 
      cluster.species.bad.Pmdl]
  
  # remove those with >95% of value below MDL
  cluster.species.bad.check = 
    unique(
      subset(conc_cluster_aboveMDL, 
             CompName %in% cluster.species.bad.check &
               Percent > 5)$CompName)
  
  # cluster.species.bad.check = c("Ag", "Ca")
  if(length(cluster.species.bad.check) > 0){
    # files for those with above MDL concentrations
    conc_cluster_mdl = conc_cluster[, 4:ncol(conc_cluster)] *
      conc_mdl_cluster[, 3:ncol(conc_mdl_cluster)]
    
    # subset those grouped as bad
    conc_cluster_bad = conc_cluster_mdl %>%
      select(all_of(cluster.species.bad.check))
    
    # replace 0 by NA
    conc_cluster_bad <-
      data.frame(
        apply(
          conc_cluster_bad, 
          2, 
          function(x) 
            replace(x, x == 0, NA)))
    
    # for concentrations above MDL, check the 10th vs. 90th percentile ratio
    # for some random comparison, r < 0.1 for strong species and < 0.2 for weak 
    # or, change to compare mean vs. sd? if mean < sd, or if mean > sd
    conc_cluster_bad_mean = 
      conc_cluster_bad %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          list(
            p = ~mean(., na.rm = T)
          )
        ))
    
    conc_cluster_bad_sd = 
      conc_cluster_bad %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          list(
            p = ~sd(., na.rm = T)
          )
        ))
    
    bad_sd_mean = conc_cluster_bad_sd/conc_cluster_bad_mean
    colnames(bad_sd_mean) = cluster.species.bad.check
    
    # Detect the species for which the sd > mean
    bad_comp <- which(colSums(bad_sd_mean > 1) > 0)
    cluster.species.bad.remove <- colnames(bad_sd_mean)[bad_comp]
    
  } else{
    cluster.species.bad.remove = NA
  }
  
  # remove the species with very scatter distribution from bad and add it into weak 
  cluster.species.bad = 
    cluster.species.bad[
      !(cluster.species.bad %in% 
          cluster.species.bad.remove)]
  
  cluster.species.weak = append(cluster.species.weak, 
                                cluster.species.bad.remove)
  cluster.species.weak = cluster.species.weak[
    !is.na(cluster.species.weak)]
  
  # arrange in alphabetic order 
  cluster.species.bad = sort(cluster.species.bad)
  cluster.species.weak = sort(cluster.species.weak)
  cluster.species.weak = append(cluster.species.weak, "PM25")
  cluster.species.strong = unique(
    subset(conc_cluster_aboveMDL, 
           !(CompName %in% cluster.species.bad |
               CompName %in% cluster.species.weak))$CompName)
  cluster.species.strong = cluster.species.strong[! cluster.species.strong %in% "PM25"]
  cluster.species.strong = as.character(cluster.species.strong)
  
  ###### Cluster- final 1: files of conc & unc for CMD ######
  # remove species marked as bad
  conc_cluster_pmf = conc_cluster[ ,-which(
    names(conc_cluster) %in% cluster.species.bad)]
  unc_cluster_pmf = unc_cluster[ ,-which(
    names(conc_cluster) %in% cluster.species.bad)]
  
  ## output files for PMF GUI
  write.csv(conc_cluster_pmf,
            paste0(GUI.cluster.folder, "/CSN_noCsub_noExtreme_C_", 
                   cluster.No, "_conc.csv"))
  
  write.csv(unc_cluster_pmf,
            paste0(GUI.cluster.folder, "/CSN_noCsub_noExtreme_C_", 
                   cluster.No, "_unc.csv"))
  
  siteRow_in_cluster = data.frame(table(conc_cluster_pmf$SiteCode))
  colnames(siteRow_in_cluster) = c("SiteCode", "Freq")
  
  write.csv(siteRow_in_cluster,
            paste0(GUI.cluster.folder, "/CSN_noCsub_noExtreme_C_", 
                   cluster.No, "_date_number_of_site.csv"))
  
  # for those marked as weak, set the uncertainty 3 times as much
  # unc_cluster_pmf_1 = unc_cluster_pmf
  
  unc_cluster_pmf[names(unc_cluster_pmf) %in% cluster.species.weak] = 
    3 * unc_cluster_pmf[names(unc_cluster_pmf) %in% cluster.species.weak]
  
  # for PM2.5, set the uncertainty 3*3 times as much
  unc_cluster_pmf$PM25 = 3 * 3 * conc_cluster_pmf$PM25
  
  # head(unc_cluster_pmf_1$Ca); head(unc_cluster_pmf$Ca); head(conc_cluster_pmf$Ca)
  # head(unc_cluster_pmf_1$Al); head(unc_cluster_pmf$Al); head(conc_cluster_pmf$Al)
  # head(unc_cluster_pmf_1$PM25); head(unc_cluster_pmf$PM25); head(conc_cluster_pmf$PM25)
  
  # conc_cluster.1 = conc_cluster; unc_cluster.1 = unc_cluster
  # add info into column names before combination
  colnames(conc_cluster_pmf) = paste0("conc_", colnames(conc_cluster_pmf))
  colnames(unc_cluster_pmf) = paste0("unc_", colnames(unc_cluster_pmf))
  
  # reorder before combine
  conc_cluster_pmf = conc_cluster_pmf[with(conc_cluster_pmf,
                                           order(conc_SiteCode, conc_Date)), ]
  unc_cluster_pmf = unc_cluster_pmf[with(unc_cluster_pmf,
                                         order(unc_SiteCode, unc_Date)), ]
  
  # combining conc & unc files
  cmd_cluster_conc_unc = cbind(conc_cluster_pmf, unc_cluster_pmf)
  
  # Interleave columns of concentration and uncertainty files
  ## generate a new vector to index the columns of cbind file
  interleave.cluster <- 
    rep(1:ncol(unc_cluster_pmf), each = 2) + 
    (0:1) * ncol(unc_cluster_pmf)
  ## reorder the cbind file
  cmd_cluster_interleave = cmd_cluster_conc_unc[interleave.cluster]
  
  # only keep one Date & cluster column
  cmd_cluster_interleave$conc_Date = 
    cmd_cluster_interleave$conc_State = 
    cmd_cluster_interleave$conc_SiteCode = NULL
  colnames(cmd_cluster_interleave)[1:3] = 
    c("SiteCode", "Date", "State")
  head(cmd_cluster_interleave)
  
  cmd_cluster_interleave = 
    cmd_cluster_interleave[
      with(cmd_cluster_interleave, 
           order(SiteCode, Date)), ]
  
  write.csv(cmd_cluster_interleave, 
            paste0(Clusterfolder, "/CSN_noCsub_noExtreme_C_", 
                   cluster.No, "_PMF_CMD.csv"))
  
  ###### Cluster- final 3: files of weak, bad, strong for CMD ######
  cmd_species_class_cluster[i, cluster.species.strong] <- 1
  cmd_species_class_cluster[i, cluster.species.weak] <- 0
  cmd_species_class_cluster[i, cluster.species.bad] <- NA
  
  cmd_species_class_cluster$sum.weak.good[i] = 
    length(cluster.species.weak) +
    length(cluster.species.strong) 
  
  cmd_species_class_cluster$cluster.row[i] = nrow(conc_cluster)
  # detect the species for None-GUI PMF, thus, weak and strong species
  noGUI_values <- 
    cmd_species_class_cluster[
      i, 
      c(cluster.species.strong, cluster.species.weak)]
  species.name.use = species.name[
    species.name %in% 
      c(cluster.species.strong, cluster.species.weak)]
  
  # add PM25
  species.name.use = append(species.name.use, "PM25")
  
  noGUI_values <- noGUI_values[, species.name.use]
  cmd_species_class_cluster$style.weak.good[i] <- 
    paste0("/", noGUI_values, collapse = "")
  
  write.csv(cmd_species_class_cluster, 
            paste0(Clusterfolder, "/CSN_noCsub_noExtreme_PMF_CMD_StrongWeakBad_Cluster.csv"))
  
}

#### Convert file to fit for CMD (No GUI) running - ALL Site ####
###### read data files ######
# read data about when the conc is above the unc
# csn_conc_mdl_site = read.csv("CSN_concentration_vs_MDL_EPAmdl.csv")

# No carbon subgroups
# csn_conc_mdl_site = read.csv("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE/CSN_concentration_vs_MDL_2023.04.csv") # original MDL in CSN

# with carbonaceous substances
csn_conc_mdl_site = read.csv("CSN_concentration_vs_MDL_C-subgroup_2023.04.csv") # original MDL in CSN

csn_conc_mdl_site$X = NULL
csn_conc_mdl_site$Date = as.Date(csn_conc_mdl_site$Date)

# Signal-to-noise ratio
# site_snr = read.csv("CSN_Site_SNR_for_PMF.csv") # No carbon subgroups
site_snr = read.csv("CSN_Site_SNR_for_PMF_C-subgroup.csv") # carbonaceous substances
site_snr$X = NULL

# OOB & Missing rate
OOB_comb_avg = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_OOBerror_random-forest_for_PMF.csv")
miss_comb_avg = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_Missing_Qualifier_interpolation_for_PMF.csv")
OOB_comb_avg$X = miss_comb_avg$X = NULL

# read data of conc & unc
conc_pmf = read.csv("CSN_concentration_AQS.PM_PMF_C-sub_2023.05.csv")
unc_pmf = read.csv("CSN_uncertainty_AQS.PM_PMF_C-sub_2023.05.csv")

# conc_pmf = read.csv("CSN_concentration_AQS.PM_PMF_C-subgroup_2023.04.csv")
# unc_pmf = read.csv("CSN_uncertainty_AQS.PM_PMF_C-subgroup_2023.04.csv")

# conc_pmf = read.csv("CSN_concentration_for_PMF_C-subgroup_2023.04.csv")
# unc_pmf = read.csv("CSN_uncertainty_for_PMF_C-subgroup_2023.04.csv")
csn_cluster = read.csv("CSN_RF_cluster5training.csv")
conc_pmf$X = unc_pmf$X = csn_cluster$X = NULL
conc_pmf$Date = as.Date(conc_pmf$Date)
unc_pmf$Date = as.Date(unc_pmf$Date)

conc_pmf = merge(conc_pmf, csn_cluster)

# total number of included sites
site.count = length(unique(conc_pmf$SiteCode))

# create a dataframe to save the decision of weak, bad, or strong 
species.name = colnames(conc_pmf)[5:ncol(conc_pmf)]
cmd_species_class_site = data.frame(
  matrix(ncol = length(species.name), 
         nrow = site.count))
colnames(cmd_species_class_site) = species.name
rownames(cmd_species_class_site) = unique(conc_pmf$SiteCode)
cmd_species_class_site$sum.weak.good = 0
cmd_species_class_site$style.weak.good = 0
cmd_species_class_site$site.row = 0

# define the subfolder to save site files
sitefolder <- "PMF_NoGUI_Site"
# sitefolder <- "PMF_NoGUI_Site_originPM"
# dir.create(sitefolder)

###### START for signle sites ######
# extract concentration & uncertainty of single site for PMF
for( j in unique(conc_pmf$SiteCode)){
  # conc & unc for each site
  conc_site = subset(conc_pmf, SiteCode == j)
  unc_site = subset(unc_pmf, SiteCode == j)
  site.No = j
  cluster.No = conc_site$Final.Decision[1]
  conc_site$Final.Decision = unc_site$Final.Decision = NULL  
  
  # extract the conc_vs._mdl data of selected site(s)
  conc_mdl_site = subset(csn_conc_mdl_site,
                            SiteCode == j)
  
  conc_mdl_site$State = NULL
  head(conc_mdl_site[, 1:2])
  head(unc_site[, 1:2])
  
  ## checking the ratio of concentrations above MDL for PMF resetting for selected site
  conc_site_aboveMDL = data.frame(round(sapply(
    conc_mdl_site[, 3:ncol(conc_mdl_site)], sum)/
      nrow(conc_mdl_site)*100, 0))
  
  conc_site_aboveMDL$CompName = rownames(conc_site_aboveMDL)
  colnames(conc_site_aboveMDL)[1] = "Percent"
  # conc_site_aboveMDL$Percent[conc_site_aboveMDL$CompName == "S"] = 
  #  conc_site_aboveMDL$Percent[conc_site_aboveMDL$CompName == "SO4"] 
  
  # Create factor levels for CompName in the order we want them to appear
  conc_site_aboveMDL$CompName <- factor(conc_site_aboveMDL$CompName, 
                                        levels = unique(conc_site_aboveMDL$CompName))
  
  # plot the percent of below_MDL_concentrations for each species
  # ggplot(conc_site_aboveMDL, aes(CompName, Percent)) +
  # ggplot(conc_site_aboveMDL, aes(CompName, Percent)) +
  #   geom_point() +
  #   geom_hline(yintercept = 10, color = "red") +
  #   geom_hline(yintercept = 20, color = "orange", linetype='dotted') +
  #   annotate("text", x = 15, y = 20, label = "Weak  ↓", vjust = -0.5, col = "orange") +
  #   annotate("text", x = 15, y =10, label = "Bad  ↓", vjust = -0.5, col = "red") +
  #   ylab("Percent of below-CSN_MDL concentration") +
  #   theme_bw()
  
  # get the SNR signal-to-noise ratio 
  snr_selected_site = subset(site_snr, 
                             SiteCode == j)
  snr_selected_site$Final.Decision = snr_selected_site$SiteCode = NULL
  
  ###### site-bad&weak 1. Strict SNR & Strict MDL - selected ######
  # generate the weak & bad species based on the conc_vs_mdl 
  site.species.weak.Pmdl = conc_site_aboveMDL$CompName[
    conc_site_aboveMDL$Percent <= 50 &
      conc_site_aboveMDL$Percent > 20]
  site.species.bad.Pmdl = conc_site_aboveMDL$CompName[
    conc_site_aboveMDL$Percent <= 20]
  # change to character
  site.species.weak.Pmdl = as.character(site.species.weak.Pmdl)
  site.species.bad.Pmdl = as.character(site.species.bad.Pmdl)
  
  # generate the weak & bad species based on SNR 
  site.species.bad.snr = colnames(snr_selected_site)[which(
    colSums(snr_selected_site) < 0.2)]
  site.species.weak.snr = colnames(snr_selected_site)[which(
    colSums(snr_selected_site) >= 0.2 &
      colSums(snr_selected_site) < 2)]
  
  ###### site-bad&weak 2. Combine the rate need interpolation & OBB error ######
  # Extract data for site
  site_oob = subset(OOB_comb_avg, 
                       SiteCode %in% sites.in.site)
  site_miss = subset(miss_comb_avg, 
                        SiteCode %in% sites.in.site)
  
  # get site average
  site_oob = data.frame(sapply(site_oob, mean))
  colnames(site_oob)[1] = "OOB"
  
  site_miss = data.frame(sapply(site_miss, mean))
  colnames(site_miss)[1] = "Miss.Flag"
  
  site_oob_miss = cbind(site_oob, site_miss)
  site_oob_miss$CompName = row.names(site_oob_miss)
  # only keep rows for CompName
  site_oob_miss = site_oob_miss[2:nrow(site_oob_miss), ]
  
  row.names(site_oob_miss) = NULL
  
  # bad, weak
  site.oob.miss.weak = site_oob_miss$CompName[
    (site_oob_miss$Miss.Flag <= 15 & 
       site_oob_miss$Miss.Flag > 10 &
       site_oob_miss$OOB <= 10 &
       site_oob_miss$OOB > 5) | 
      (site_oob_miss$Miss.Flag <= 10 &
         site_oob_miss$Miss.Flag > 5 &
         site_oob_miss$OOB > 8)]
  site.oob.miss.bad = site_oob_miss$CompName[
    site_oob_miss$Miss.Flag > 15 |
      (site_oob_miss$Miss.Flag <= 15 & 
         site_oob_miss$Miss.Flag > 10 &
         site_oob_miss$OOB > 5)]
  
  ###### site-bad&weak 3. other criteria ######
  
  # combine the "weak" & "bad" list
  site.species.bad = append(site.species.bad.snr, 
                               site.species.bad.Pmdl)
  site.species.bad = append(site.species.bad, 
                               site.oob.miss.bad)
  
  site.species.weak = append(site.species.weak.snr, 
                                site.species.weak.Pmdl)
  site.species.weak = append(site.species.weak, 
                                site.oob.miss.weak)
  
  # remove PM25, which potentially exists in the weak/bad list
  site.species.bad = site.species.bad[! site.species.bad %in% "PM25"]
  site.species.weak = site.species.weak[! site.species.weak %in% "PM25"]
  
  # if OC/EC/OPC is in bad, force remove to weak
  oc.ec = c("EC", "EC1", "EC2", "EC3", 
            "OC", "OC1", "OC2", 
            "OC3", "OC4", "OP")
  
  oc.ec.bad = site.species.bad[site.species.bad %in% oc.ec]
  site.species.bad = site.species.bad[! site.species.bad %in% oc.ec]
  site.species.weak = append(site.species.weak, 
                                oc.ec.bad)
  
  # if a species exist both in "bad" & "weak", define as "WEAK"
  site.species.bad = site.species.bad[! (
    site.species.bad %in% site.species.weak)]
  
  # add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
  site.species.bad = append(c("S", "Na", "K"), 
                               site.species.bad) 
  # in case any of this three is in weak, if so, remove
  site.species.weak = site.species.weak[! (
    site.species.weak %in% c("S", "Na", "K"))]
  
  # add Al, Mg, Na into weak, if there are not in bad, due to the lower reliability in XRF test
  Al.Mg.weak = c("Al", "Mg")
  # determine if Al or Mg is in bad, if so, remove the one in "Al.Mg.weak"
  Al.Mg.weak = Al.Mg.weak[! (
    Al.Mg.weak %in% site.species.bad)]
  # add the rest in Al.Mg.weak to "weak"
  site.species.weak = append(Al.Mg.weak, 
                                site.species.weak) 
  # "Na", "K" were removed, and use Na+, K+ instead
  
  # remove the duplicated
  # site.species.weak = site.species.weak[!duplicated(site.species.weak)]
  
  # remove the duplicated strings from the character vector
  site.species.bad = unique(
    unlist(
      strsplit(
        site.species.bad, " ")))
  site.species.weak = unique(
    unlist(
      strsplit(
        site.species.weak, " ")))
  
  ###### site-bad&weak 4. Distribution of values above MDL ######
  # species stay in "bad" and related to higher Pmdl
  site.species.bad.check = site.species.bad[
    site.species.bad %in% 
      site.species.bad.Pmdl]
  
  # remove those with >95% of value below MDL
  site.species.bad.check = 
    unique(
      subset(conc_site_aboveMDL, 
             CompName %in% site.species.bad.check &
               Percent > 5)$CompName)
  
  # site.species.bad.check = c("Ag", "Ca")
  if(length(site.species.bad.check) > 0){
    # files for those with above MDL concentrations
    conc_site_mdl = conc_site[, 4:ncol(conc_site)] *
      conc_mdl_site[, 3:ncol(conc_mdl_site)]
    
    # subset those grouped as bad
    conc_site_bad = conc_site_mdl %>%
      select(all_of(site.species.bad.check))
    
    # replace 0 by NA
    conc_site_bad <-
      data.frame(
        apply(
          conc_site_bad, 
          2, 
          function(x) 
            replace(x, x == 0, NA)))
    
    # for concentrations above MDL, check the 10th vs. 90th percentile ratio
    # for some random comparison, r < 0.1 for strong species and < 0.2 for weak 
    # or, change to compare mean vs. sd? if mean < sd, or if mean > sd
    conc_site_bad_mean = 
      conc_site_bad %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          list(
            p = ~mean(., na.rm = T)
          )
        ))
    
    conc_site_bad_sd = 
      conc_site_bad %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          list(
            p = ~sd(., na.rm = T)
          )
        ))
    
    bad_sd_mean = conc_site_bad_sd/conc_site_bad_mean
    colnames(bad_sd_mean) = site.species.bad.check
    
    # Detect the species for which the sd > mean
    bad_comp <- which(colSums(bad_sd_mean > 1) > 0)
    site.species.bad.remove <- colnames(bad_sd_mean)[bad_comp]
    
  } else{
    site.species.bad.remove = NA
  }
  
  # remove the species with very scatter distribution from bad and add it into weak 
  site.species.bad = 
    site.species.bad[
      !(site.species.bad %in% 
          site.species.bad.remove)]
  
  site.species.weak = append(site.species.weak, 
                                site.species.bad.remove)
  site.species.weak = site.species.weak[
    !is.na(site.species.weak)]
  
  # arrange in alphabetic order 
  site.species.bad = sort(site.species.bad)
  site.species.weak = sort(site.species.weak)
  site.species.weak = append(site.species.weak, "PM25")
  site.species.strong = unique(
    subset(conc_site_aboveMDL, 
           !(CompName %in% site.species.bad |
               CompName %in% site.species.weak))$CompName)
  site.species.strong = as.character(site.species.strong)
  
  ###### site- final 1: files of conc & unc for CMD ######
  # remove species marked as bad
  conc_site_pmf = conc_site[ ,-which(
    names(conc_site) %in% site.species.bad)]
  unc_site_pmf = unc_site[ ,-which(
    names(conc_site) %in% site.species.bad)]
  
  # unc_site_pmf_1 = unc_site_pmf
  
  # for those marked as weak, set the uncertainty 3 times as much
  unc_site_pmf[names(unc_site_pmf) %in% site.species.weak] = 
    3 * unc_site_pmf[names(unc_site_pmf) %in% site.species.weak]
  
  # for PM2.5, set the uncertainty 3*3 times as much
  unc_site_pmf$PM25 = 3 * 3 * unc_site_pmf$PM25
  
  # recorder for match
  conc_site_pmf = conc_site_pmf[with(conc_site_pmf,
                                     order(Date)), ]
  unc_site_pmf = unc_site_pmf[with(unc_site_pmf,
                                     order(Date)), ]
  
  # conc_site.1 = conc_site; unc_site.1 = unc_site
  # add info into column names before combination
  colnames(conc_site_pmf) = paste0("conc_", colnames(conc_site_pmf))
  colnames(unc_site_pmf) = paste0("unc_", colnames(unc_site_pmf))
  
  # combining conc & unc files
  cmd_site_conc_unc = cbind(conc_site_pmf, unc_site_pmf)
  # cmd_site_conc_unc_1 = merge(conc_site_pmf, unc_site_pmf,
  #                             by.x = "conc_Date", by.y = "unc_Date")
  
  # Interleave columns of concentration and uncertainty files
  ## generate a new vector to index the columns of cbind file
  interleave.site <- 
    rep(1:ncol(unc_site_pmf), each = 2) + 
    (0:1) * ncol(unc_site_pmf)
  ## reorder the cbind file
  cmd_site_interleave = cmd_site_conc_unc[interleave.site]
  
  # only keep one Date & site column
  cmd_site_interleave$conc_Date = 
    cmd_site_interleave$conc_State = 
    cmd_site_interleave$conc_SiteCode = NULL
  colnames(cmd_site_interleave)[1:3] = 
    c("SiteCode", "Date", "State")
  head(cmd_site_interleave)
  
  cmd_site_interleave = 
    cmd_site_interleave[
      with(cmd_site_interleave, 
           order(Date)), ]
  
  write.csv(cmd_site_interleave, 
            paste0(sitefolder, "/CSN_C_", 
                   cluster.No, "_S_",
                   site.No, "_PMF_CMD.csv"))
  
  ###### site- final 2: files of weak, bad, strong for CMD ######
  # row names are generally stored as character in R
  cmd_species_class_site[as.character(j), site.species.strong] <- 1
  cmd_species_class_site[as.character(j), site.species.weak] <- 0
  cmd_species_class_site[as.character(j), site.species.bad] <- NA
  
  cmd_species_class_site[as.character(j), "sum.weak.good"] =
    length(site.species.weak) +
    length(site.species.strong) 
  
  cmd_species_class_site[as.character(j), "site.row"] = nrow(conc_site)
  noGUI_values <- 
    cmd_species_class_site[
      as.character(j), 
      c(site.species.strong, site.species.weak)]
  species.name.use = species.name[
    species.name %in% 
      c(site.species.strong, site.species.weak)]
  
  noGUI_values <- noGUI_values[, species.name.use]
  cmd_species_class_site[as.character(j), "style.weak.good"] = 
    paste0("/", noGUI_values, collapse = "")

  cmd_species_class_site[as.character(j), "Final.Decision"] = cluster.No
    
  write.csv(cmd_species_class_site, 
            paste0(sitefolder, "/CSN_PMF_CMD_StrongWeakBad_Site.csv"))
  
}

#### trial -- convert file to fit for CMD running - selected site ####
# read conc & unc data
# conc_site = read.csv("CSN_Cluster6_site3_2011-14_PMF_conc_EPAmdl.csv")
# unc_site = read.csv("CSN_Cluster6_site3_2011-14_PMF_unc_EPAmdl.csv")

# conc_site = read.csv("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE/CSN_Cluster24_site2_2011-14_PMF_conc_EPAmdl.csv")
# unc_site = read.csv("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE/CSN_Cluster24_site2_2011-14_PMF_unc_EPAmdl.csv")

# OC EC
# conc_site = read.csv("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/CSN_PMF_Try_data/Top_PM_cluster&site/CSN_C6_S3_2011-14_conc.csv")
# unc_site = read.csv("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/CSN_PMF_Try_data/Top_PM_cluster&site/CSN_C6_S3_2011-14_unc.csv")

# C-sub
conc_site = read.csv("CSN_C6_S3_C-sub_2011-14_conc.csv")
unc_site = read.csv("CSN_C6_S3_C-sub_2011-14_unc.csv")

conc_site$X = unc_site$X = NULL
study.site = conc_site$SiteCode[1]
dim(conc_site)
dim(unc_site)

# remove duplicated dates (for data till 2023.Feb, will update the data later)
## conc_site_final = conc_site[!duplicated(conc_site$Date), ]
## unc_site_final = unc_site[!duplicated(unc_site$Date), ]

## write.csv(conc_site_final, "CSN_Cluster6_site3_2011-14_PMF_conc_EPAmdl.csv")
## write.csv(unc_site_final, "CSN_Cluster6_site3_2011-14_PMF_unc_EPAmdl.csv")

## write.csv(conc_site_final, "CSN_Cluster24_site3_2011-14_PMF_conc_EPAmdl.csv")
## write.csv(unc_site_final, "CSN_Cluster24_site3_2011-14_PMF_unc_EPAmdl.csv")

# read data about when the conc is above the unc
# csn_conc_mdl_Site = read.csv("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE/CSN_concentration_vs_MDL_EPAmdl.csv")

# csn_conc_mdl_Site = read.csv("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE/CSN_concentration_vs_MDL_2023.04.csv") # original MDL in CSN
# C-sub
csn_conc_mdl_Site = read.csv("CSN_concentration_vs_MDL_C-subgroup_2023.04.csv") # original MDL in CSN
csn_conc_mdl_Site$X = NULL
csn_conc_mdl_Site$Date = as.Date(csn_conc_mdl_Site$Date)

# extract the conc_vs._mdl data of selected site(s)
## for those with top PM
conc_mdl_Site = subset(csn_conc_mdl_Site,
                       Date < as.Date("2014-01-01") &
                         SiteCode == study.site)

## for those with the lowest PM
conc_mdl_Site = subset(csn_conc_mdl_Site,
                       Date >= as.Date("2018-01-01") &
                         SiteCode == study.site)

conc_mdl_Site$State = conc_mdl_Site$Qualifier = NULL

# NO LONGER NEEDED FROM 2023.03
# double check if there are duplicates
dim(conc_mdl_Site)
conc_mdl_Site = conc_mdl_Site[
  !duplicated(
    conc_mdl_Site[, 1:2]), ]
dim(conc_mdl_Site)

head(conc_mdl_Site[, 1:2])
head(unc_site[, 1:2])

## checking the ratio of concentrations above MDL for PMF resetting for selected cluster
conc_site_aboveMDL = data.frame(round(sapply(
  conc_mdl_Site[, 3:ncol(conc_mdl_Site)], sum)/
    nrow(conc_mdl_Site)*100, 0))
conc_site_aboveMDL$CompName = rownames(conc_site_aboveMDL)
colnames(conc_site_aboveMDL)[1] = "Percent"
# conc_site_aboveMDL$Percent[conc_site_aboveMDL$CompName == "S"] = 
#  conc_site_aboveMDL$Percent[conc_site_aboveMDL$CompName == "SO4"] 

# Create factor levels for CompName in the order we want them to appear
conc_site_aboveMDL$CompName <- factor(conc_site_aboveMDL$CompName, 
                                      levels = unique(conc_site_aboveMDL$CompName))

# plot the percent of below_MDL_concentrations for each species
# ggplot(conc_site_aboveMDL, aes(CompName, Percent)) +
ggplot(conc_site_aboveMDL, aes(CompName, Percent)) +
  geom_point() +
  geom_hline(yintercept = 10, color = "red") +
  geom_hline(yintercept = 20, color = "orange", linetype='dotted') +
  annotate("text", x = 15, y = 20, label = "Weak  ↓", vjust = -0.5, col = "orange") +
  annotate("text", x = 15, y =10, label = "Bad  ↓", vjust = -0.5, col = "red") +
  ylab("Percent of below-CSN_MDL concentration") +
  theme_bw()

# get the SNR signal-to-noise ratio 
# site_snr = read.csv("CSN_Site_SNR_for_PMF_EPAmdl.csv")
# site_snr = read.csv("CSN_Site_SNR_for_PMF.csv")
site_snr = read.csv("CSN_Site_SNR_for_PMF_C-subgroup.csv")
site_snr$X = site_snr$Final.Decision = NULL
snr_selected_site = subset(site_snr, 
                           SiteCode == study.site)

###### Site-bad&weak 1.1. Strict SNR & Mild MDL ######
# generate the weak & bad species based on the conc_vs_mdl
site.species.weak.Pmdl = conc_site_aboveMDL$CompName[
  conc_site_aboveMDL$Percent <= 20 &
    conc_site_aboveMDL$Percent > 10]
site.species.bad.Pmdl = conc_site_aboveMDL$CompName[
  conc_site_aboveMDL$Percent <= 10]
# change to factor
site.species.weak.Pmdl = as.character(site.species.weak.Pmdl)
site.species.bad.Pmdl = as.character(site.species.bad.Pmdl)
# add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
site.species.bad.Pmdl = append(c("S", "Na", "K"), 
                               site.species.bad.Pmdl) 

# generate the weak & bad species based on SNR
site.species.bad.snr = colnames(snr_selected_site)[which(
  colSums(snr_selected_site) < 0.2)]
site.species.weak.snr = colnames(snr_selected_site)[which(
  colSums(snr_selected_site) >= 0.2 &
    colSums(snr_selected_site) < 2)]

###### Site-bad&weak 1.2. Mild SNR & Strict MDL ######
# generate the weak & bad species based on the conc_vs_mdl 
site.species.weak.Pmdl = conc_site_aboveMDL$CompName[
  conc_site_aboveMDL$Percent <= 50 &
    conc_site_aboveMDL$Percent > 20]
site.species.bad.Pmdl = conc_site_aboveMDL$CompName[
  conc_site_aboveMDL$Percent <= 20]
# change to character
site.species.weak.Pmdl = as.character(site.species.weak.Pmdl)
site.species.bad.Pmdl = as.character(site.species.bad.Pmdl)

# add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
site.species.bad.Pmdl = append(c("S", "Na", "K"), 
                               site.species.bad.Pmdl) 

# generate the weak & bad species based on SNR
site.species.bad.snr = colnames(snr_selected_site)[which(
  colSums(snr_selected_site) < 0.1)]
site.species.weak.snr = colnames(snr_selected_site)[which(
  colSums(snr_selected_site) >= 0.1 &
    colSums(snr_selected_site) < 1)]

###### Site-bad&weak 1.3. Mild SNR & Mild MDL  ######
# generate the weak & bad species based on the conc_vs_mdl 
site.species.weak.Pmdl = conc_site_aboveMDL$CompName[
  conc_site_aboveMDL$Percent <= 20 &
    conc_site_aboveMDL$Percent > 10]
site.species.bad.Pmdl = conc_site_aboveMDL$CompName[
  conc_site_aboveMDL$Percent <= 10]
# change to character
site.species.weak.Pmdl = as.character(site.species.weak.Pmdl)
site.species.bad.Pmdl = as.character(site.species.bad.Pmdl)

# add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
site.species.bad.Pmdl = append(c("S", "Na", "K"), 
                               site.species.bad.Pmdl) 

# generate the weak & bad species based on SNR 
site.species.bad.snr = colnames(snr_selected_site)[which(
  colSums(snr_selected_site) < 0.1)]
site.species.weak.snr = colnames(snr_selected_site)[which(
  colSums(snr_selected_site) >= 0.1 &
    colSums(snr_selected_site) < 1)]

###### Site-bad&weak 1.4. Strict SNR & Strict MDL - selected ######
# generate the weak & bad species based on the conc_vs_mdl 
site.species.weak.Pmdl = conc_site_aboveMDL$CompName[
  conc_site_aboveMDL$Percent <= 50 &
    conc_site_aboveMDL$Percent > 20]
site.species.bad.Pmdl = conc_site_aboveMDL$CompName[
  conc_site_aboveMDL$Percent <= 20]
# change to character
site.species.weak.Pmdl = as.character(site.species.weak.Pmdl)
site.species.bad.Pmdl = as.character(site.species.bad.Pmdl)

# add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
site.species.bad.Pmdl = append(c("S", "Na", "K"), 
                               site.species.bad.Pmdl) 

# generate the weak & bad species based on SNR 
site.species.bad.snr = colnames(snr_selected_site)[which(
  colSums(snr_selected_site) < 0.2)]
site.species.weak.snr = colnames(snr_selected_site)[which(
  colSums(snr_selected_site) >= 0.2 &
    colSums(snr_selected_site) < 2)]

###### Site-bad&weak 2. Combine the rate need interpolation & OBB error ######
OOB_comb = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_OOBerror_random-forest_All.csv")
miss_comb = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_Missing_Qualifier_interpolation_All.csv")
# the miss is the combination of missing data and flagged data that has been removed and interpolated
OOB_comb$X = miss_comb$X = NULL

# set column for intersect check 
row.names(OOB_comb) = paste(OOB_comb$SiteCode, OOB_comb$Year)
row.names(miss_comb) = paste(miss_comb$SiteCode, miss_comb$Year)

# for miss_comb, only keep sites appeared in OOB_comb
# get common rows 
obb_miss_rows <- intersect(row.names(OOB_comb), 
                           row.names(miss_comb))

# Subset data frames using common rownames and column names
OOB_comb <- OOB_comb[obb_miss_rows, ]
miss_comb <- miss_comb[obb_miss_rows, ]
row.names(OOB_comb) = row.names(miss_comb) = NULL

# Average of the time before & after 2015
OOB_comb_avg = aggregate(.~SiteCode, 
                         data=OOB_comb[, 1:(ncol(OOB_comb)-1)], 
                         mean)
miss_comb_avg = aggregate(.~SiteCode, 
                          data=miss_comb[, 1:(ncol(miss_comb)-1)], 
                          mean)

# Extract data for site
site_oob = subset(OOB_comb_avg, 
                  SiteCode == study.site)
site_miss = subset(miss_comb_avg, 
                   SiteCode == study.site)
site_oob$parameter = "OOB"
site_miss$parameter = "Miss.Flag"

site_oob_miss = rbind(site_oob, site_miss)
site_oob_miss = data.frame(t(site_oob_miss))

# change colnames, add new row, & remove unused row
colnames(site_oob_miss) = site_oob_miss["parameter", ]
site_oob_miss$CompName = row.names(site_oob_miss)
# only keep rows for CompName
site_oob_miss = site_oob_miss[2:(nrow(site_oob_miss)-2), ]

row.names(site_oob_miss) = NULL

# change to numeric
site_oob_miss[, 1:2] = lapply(
  site_oob_miss[, 1:2], 
  as.numeric)

# bad, weak
site.oob.miss.weak = site_oob_miss$CompName[
  (site_oob_miss$Miss.Flag <= 15 & 
     site_oob_miss$Miss.Flag > 10 &
     site_oob_miss$OOB <= 10 &
     site_oob_miss$OOB > 5) | 
    (site_oob_miss$Miss.Flag <= 10 &
       site_oob_miss$Miss.Flag > 5 &
       site_oob_miss$OOB > 8)]
site.oob.miss.bad = site_oob_miss$CompName[
  site_oob_miss$Miss.Flag > 15 |
    (site_oob_miss$Miss.Flag <= 15 & 
       site_oob_miss$Miss.Flag > 10 &
       site_oob_miss$OOB > 5)]

###### Site-bad&weak 3. other criteria ######

# combine the "weak" & "bad" list
site.species.bad = append(site.species.bad.snr, 
                          site.species.bad.Pmdl)
site.species.bad = append(site.species.bad, 
                          site.oob.miss.bad)

site.species.weak = append(site.species.weak.snr, 
                           site.species.weak.Pmdl)
site.species.weak = append(site.species.weak, 
                           site.oob.miss.weak)

# remove the duplicated strings from the character vector
site.species.bad = unique(unlist(strsplit(site.species.bad, " ")))
site.species.weak = unique(unlist(strsplit(site.species.weak, " ")))

# remove PM25, which potentially exists in the weak/bad list
site.species.bad = site.species.bad[! site.species.bad %in% "PM25"]
site.species.weak = site.species.weak[! site.species.weak %in% "PM25"]

# if OC/EC/OPC is in bad, force remove to weak
# oc.ec = c("EC", "OC", "OP")
oc.ec = c("EC", "EC1", "EC2", "EC3", 
          "OC", "OC1", "OC2", "OC3", "OC4", 
          "OP")
oc.ec.bad = site.species.bad[site.species.bad %in% oc.ec]
site.species.bad = site.species.bad[! site.species.bad %in% oc.ec]
site.species.weak = append(site.species.weak, 
                           oc.ec.bad)

# add Al, Mg, Na into weak, due to the lower reliability in XRF test
site.species.weak = append(c("Al", "Mg"), site.species.weak) 
# "Na", "K" were removed, and use Na+, K+ instead
# remove the duplicated
site.species.weak = site.species.weak[!duplicated(site.species.weak)]

# if a species exist both in "bad" & "weak", define as "bad"
site.species.weak = site.species.weak[! (
  site.species.weak %in% site.species.bad)]

# arrange in alphabetic order 
site.species.bad = sort(site.species.bad)
site.species.weak = sort(site.species.weak)

site.species.bad
site.species.weak

###### Site-bad&weak 4. set uncertainties or remove - files for CMD ######
# remove species marked as bad
conc_site_pmf = conc_site[ ,-which(
  names(conc_site) %in% site.species.bad)]
unc_site_pmf = unc_site[ ,-which(
  names(conc_site) %in% site.species.bad)]

# for those marked as weak, set the uncertainty 3 times as much
unc_site_pmf[names(unc_site_pmf) %in% site.species.weak] = 
  3 * unc_site_pmf[names(unc_site_pmf) %in% site.species.weak]

# add info into column names before combination
colnames(conc_site) = paste0("conc_", colnames(conc_site))
colnames(unc_site) = paste0("unc_", colnames(unc_site))

# combining conc & unc files
cmd_site_conc_unc = cbind(conc_site, unc_site)

# Interleave columns of concentration and uncertainty files
## generate a new vector to index the columns of cbind file
interleave.site <- rep(1:ncol(unc_site), each = 2) + (0:1) * ncol(unc_site)
## reorder the cbind file
cmd_site_interleave = cmd_site_conc_unc[interleave.site]

# only keep one Date & Site column
cmd_site_interleave$conc_Date = cmd_site_interleave$conc_State = 
  cmd_site_interleave$conc_SiteCode = NULL
colnames(cmd_site_interleave)[1:3] = c("Date", "State", "SiteCode")
head(cmd_site_interleave)

# remove duplicated dates (for data till 2023.Feb, will update the data later)
# cmd_site_final = cmd_site_interleave[!duplicated(cmd_site_interleave$Date), ]
# no longer needed since have fix the problem

# write.csv(cmd_site_final, paste0("CSN_site_", study.site, "_conc_unc_PMF_CMD.csv"))
write.csv(cmd_site_interleave, paste0("CSN_site_", study.site, "_conc_unc_PMF_CMD.csv"))

#### trial -- convert file to fit for CMD running - selected Cluster ####
# read conc & unc data
# conc_cluster = read.csv("CSN_Cluster6_2011-14_PMF_conc_EPAmdl.csv")
# unc_cluster = read.csv("CSN_Cluster6_2011-14_PMF_unc_EPAmdl.csv")

# conc_cluster = read.csv("CSN_Cluster24_2011-14_PMF_conc_EPAmdl.csv")
# unc_cluster = read.csv("CSN_Cluster24_2011-14_PMF_unc_EPAmdl.csv")

conc_cluster = read.csv("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/CSN_PMF_Try_data/Top_PM_cluster&site/CSN_C6_2011-14_conc.csv")
unc_cluster = read.csv("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/CSN_PMF_Try_data/Top_PM_cluster&site/CSN_C6_2011-14_unc.csv")

conc_cluster$X = unc_cluster$X = NULL
dim(conc_cluster)
dim(unc_cluster)
sites.in.cluster = unique(conc_cluster$SiteCode)
cluster.Decision = 6
# cluster.Decision = 24

# remove duplicates according to dates & sidecode
# (for data till 2023.Feb, will update the data later)
## conc_cluster_final = conc_cluster[!duplicated(conc_cluster[, 1:2]), ]
## unc_cluster_final = unc_cluster[!duplicated(unc_cluster[, 1:2]), ]
## dim(conc_cluster_final)
## dim(unc_cluster_final)
## write.csv(conc_cluster_final, "CSN_Cluster6_2011-14_PMF_conc_EPAmdl.csv")
## write.csv(unc_cluster_final, "CSN_Cluster6_2011-14_PMF_unc_EPAmdl.csv")

# read data about when the conc is above the unc
# csn_conc_mdl_cluster = read.csv("CSN_concentration_vs_MDL_EPAmdl.csv")
csn_conc_mdl_cluster = read.csv("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE/CSN_concentration_vs_MDL_2023.04.csv") # original MDL in CSN
csn_conc_mdl_cluster$X = NULL
csn_conc_mdl_cluster$Date = as.Date(csn_conc_mdl_cluster$Date)

# extract the conc_vs._mdl data of selected cluster(s)
conc_mdl_cluster = subset(csn_conc_mdl_cluster,
                          Date < as.Date("2014-01-01") &
                            SiteCode %in% sites.in.cluster)

conc_mdl_cluster = subset(csn_conc_mdl_cluster,
                          Date >= as.Date("2018-01-01") &
                            SiteCode %in% sites.in.cluster)

conc_mdl_cluster$State = conc_mdl_cluster$Qualifier = NULL
# conc_mdl_cluster = conc_mdl_cluster[!duplicated(conc_mdl_cluster[, 1:2]), ]
head(conc_mdl_cluster[, 1:2])
head(unc_cluster[, 1:2])

## checking the ratio of concentrations above MDL for PMF resetting for selected cluster
conc_cluster_aboveMDL = data.frame(round(sapply(
  conc_mdl_cluster[, 3:ncol(conc_mdl_cluster)], sum)/
    nrow(conc_mdl_cluster)*100, 0))
conc_cluster_aboveMDL$CompName = rownames(conc_cluster_aboveMDL)
colnames(conc_cluster_aboveMDL)[1] = "Percent"
# conc_cluster_aboveMDL$Percent[conc_cluster_aboveMDL$CompName == "S"] = 
#  conc_site_aboveMDL$Percent[conc_site_aboveMDL$CompName == "SO4"] 

# Create factor levels for CompName in the order we want them to appear
conc_cluster_aboveMDL$CompName <- factor(conc_cluster_aboveMDL$CompName, 
                                         levels = unique(conc_cluster_aboveMDL$CompName))

# plot the percent of below_MDL_concentrations for each species
# ggplot(conc_cluster_aboveMDL, aes(CompName, Percent)) +
ggplot(conc_cluster_aboveMDL, aes(CompName, Percent)) +
  geom_point() +
  geom_hline(yintercept = 10, color = "red") +
  geom_hline(yintercept = 20, color = "orange", linetype='dotted') +
  annotate("text", x = 15, y = 20, label = "Weak  ↓", vjust = -0.5, col = "orange") +
  annotate("text", x = 15, y =10, label = "Bad  ↓", vjust = -0.5, col = "red") +
  ylab("Percent of below-CSN_MDL concentration") +
  theme_bw()

# get the SNR signal-to-noise ratio 
# cluster_snr = read.csv("CSN_cluster_SNR_for_PMF_EPAmdl.csv")
cluster_snr = read.csv("CSN_Cluster_SNR_for_PMF.csv")
cluster_snr$X = NULL
snr_selected_cluster = subset(cluster_snr, 
                              cluster.Decision == Final.Decision)

###### Cluster-bad&weak 1.1. Strict SNR & Mild MDL ######
# generate the weak & bad species based on the conc_vs_mdl
cluster.species.weak.Pmdl = conc_cluster_aboveMDL$CompName[
  conc_cluster_aboveMDL$Percent <= 20 &
    conc_cluster_aboveMDL$Percent > 10]
cluster.species.bad.Pmdl = conc_cluster_aboveMDL$CompName[
  conc_cluster_aboveMDL$Percent <= 10]
# change to factor
cluster.species.weak.Pmdl = as.character(cluster.species.weak.Pmdl)
cluster.species.bad.Pmdl = as.character(cluster.species.bad.Pmdl)
# add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
cluster.species.bad.Pmdl = append(c("S", "Na", "K"), 
                                  cluster.species.bad.Pmdl) 

# generate the weak & bad species based on SNR
cluster.species.bad.snr = colnames(snr_selected_cluster)[which(
  colSums(snr_selected_cluster) < 0.2)]
cluster.species.weak.snr = colnames(snr_selected_cluster)[which(
  colSums(snr_selected_cluster) >= 0.2 &
    colSums(snr_selected_cluster) < 2)]

###### Cluster-bad&weak 1.2. Mild SNR & Strict MDL ######
# generate the weak & bad species based on the conc_vs_mdl 
cluster.species.weak.Pmdl = conc_cluster_aboveMDL$CompName[
  conc_cluster_aboveMDL$Percent <= 50 &
    conc_cluster_aboveMDL$Percent > 20]
cluster.species.bad.Pmdl = conc_cluster_aboveMDL$CompName[
  conc_cluster_aboveMDL$Percent <= 20]
# change to character
cluster.species.weak.Pmdl = as.character(cluster.species.weak.Pmdl)
cluster.species.bad.Pmdl = as.character(cluster.species.bad.Pmdl)

# add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
cluster.species.bad.Pmdl = append(c("S", "Na", "K"), 
                                  cluster.species.bad.Pmdl) 

# generate the weak & bad species based on SNR
cluster.species.bad.snr = colnames(snr_selected_cluster)[which(
  colSums(snr_selected_cluster) < 0.1)]
cluster.species.weak.snr = colnames(snr_selected_cluster)[which(
  colSums(snr_selected_cluster) >= 0.1 &
    colSums(snr_selected_cluster) < 1)]

###### Cluster-bad&weak 1.3. Mild SNR & Mild MDL  ######
# generate the weak & bad species based on the conc_vs_mdl 
cluster.species.weak.Pmdl = conc_cluster_aboveMDL$CompName[
  conc_cluster_aboveMDL$Percent <= 20 &
    conc_cluster_aboveMDL$Percent > 10]
cluster.species.bad.Pmdl = conc_cluster_aboveMDL$CompName[
  conc_cluster_aboveMDL$Percent <= 10]
# change to character
cluster.species.weak.Pmdl = as.character(cluster.species.weak.Pmdl)
cluster.species.bad.Pmdl = as.character(cluster.species.bad.Pmdl)

# add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
cluster.species.bad.Pmdl = append(c("S", "Na", "K"), 
                                  cluster.species.bad.Pmdl) 

# generate the weak & bad species based on SNR 
cluster.species.bad.snr = colnames(snr_selected_cluster)[which(
  colSums(snr_selected_cluster) < 0.1)]
cluster.species.weak.snr = colnames(snr_selected_cluster)[which(
  colSums(snr_selected_cluster) >= 0.1 &
    colSums(snr_selected_cluster) < 1)]

###### Cluster-bad&weak 1.4. Strict SNR & Strict MDL - selected ######
# generate the weak & bad species based on the conc_vs_mdl 
cluster.species.weak.Pmdl = conc_cluster_aboveMDL$CompName[
  conc_cluster_aboveMDL$Percent <= 50 &
    conc_cluster_aboveMDL$Percent > 20]
cluster.species.bad.Pmdl = conc_cluster_aboveMDL$CompName[
  conc_cluster_aboveMDL$Percent <= 20]
# change to character
cluster.species.weak.Pmdl = as.character(cluster.species.weak.Pmdl)
cluster.species.bad.Pmdl = as.character(cluster.species.bad.Pmdl)

# add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
cluster.species.bad.Pmdl = append(c("S", "Na", "K"), 
                                  cluster.species.bad.Pmdl) 

# generate the weak & bad species based on SNR 
cluster.species.bad.snr = colnames(snr_selected_cluster)[which(
  colSums(snr_selected_cluster) < 0.2)]
cluster.species.weak.snr = colnames(snr_selected_cluster)[which(
  colSums(snr_selected_cluster) >= 0.2 &
    colSums(snr_selected_cluster) < 2)]

###### Cluster-bad&weak 2. Combine the rate need interpolation & OBB error ######
OOB_comb = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_OOBerror_random-forest_All.csv")
miss_comb = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_Missing_Qualifier_interpolation_All.csv")
# the miss is the combination of missing data and flagged data that has been removed and interpolated
OOB_comb$X = miss_comb$X = NULL

# set column for intersect check 
row.names(OOB_comb) = paste(OOB_comb$SiteCode, OOB_comb$Year)
row.names(miss_comb) = paste(miss_comb$SiteCode, miss_comb$Year)

# for miss_comb, only keep clusters appeared in OOB_comb
# get common rows 
obb_miss_rows <- intersect(row.names(OOB_comb), 
                           row.names(miss_comb))

# Subset data frames using common rownames and column names
OOB_comb <- OOB_comb[obb_miss_rows, ]
miss_comb <- miss_comb[obb_miss_rows, ]
row.names(OOB_comb) = row.names(miss_comb) = NULL

# Average of the time before & after 2015
OOB_comb_avg = aggregate(.~SiteCode, 
                         data=OOB_comb[, 1:(ncol(OOB_comb)-1)], 
                         mean)
miss_comb_avg = aggregate(.~SiteCode, 
                          data=miss_comb[, 1:(ncol(miss_comb)-1)], 
                          mean)

# Extract data for cluster
cluster_oob = subset(OOB_comb_avg, 
                     SiteCode %in% sites.in.cluster)
cluster_miss = subset(miss_comb_avg, 
                      SiteCode %in% sites.in.cluster)

# get cluster average
cluster_oob = data.frame(sapply(cluster_oob, mean))
colnames(cluster_oob)[1] = "OOB"

cluster_miss = data.frame(sapply(cluster_miss, mean))
colnames(cluster_miss)[1] = "Miss.Flag"

cluster_oob_miss = cbind(cluster_oob, cluster_miss)
cluster_oob_miss$CompName = row.names(cluster_oob_miss)
# only keep rows for CompName
cluster_oob_miss = cluster_oob_miss[2:nrow(cluster_oob_miss), ]

row.names(cluster_oob_miss) = NULL

# bad, weak
cluster.oob.miss.weak = cluster_oob_miss$CompName[
  (cluster_oob_miss$Miss.Flag <= 15 & 
     cluster_oob_miss$Miss.Flag > 10 &
     cluster_oob_miss$OOB <= 10 &
     cluster_oob_miss$OOB > 5) | 
    (cluster_oob_miss$Miss.Flag <= 10 &
       cluster_oob_miss$Miss.Flag > 5 &
       cluster_oob_miss$OOB > 8)]
cluster.oob.miss.bad = cluster_oob_miss$CompName[
  cluster_oob_miss$Miss.Flag > 15 |
    (cluster_oob_miss$Miss.Flag <= 15 & 
       cluster_oob_miss$Miss.Flag > 10 &
       cluster_oob_miss$OOB > 5)]

###### Cluster-bad&weak 3. other criteria ######

# combine the "weak" & "bad" list
cluster.species.bad = append(cluster.species.bad.snr, 
                             cluster.species.bad.Pmdl)
cluster.species.bad = append(cluster.species.bad, 
                             cluster.oob.miss.bad)

cluster.species.weak = append(cluster.species.weak.snr, 
                              cluster.species.weak.Pmdl)
cluster.species.weak = append(cluster.species.weak, 
                              cluster.oob.miss.weak)

# remove the duplicated strings from the character vector
cluster.species.bad = unique(unlist(strsplit(cluster.species.bad, " ")))
cluster.species.weak = unique(unlist(strsplit(cluster.species.weak, " ")))

# remove PM25, which potentially exists in the weak/bad list
cluster.species.bad = cluster.species.bad[! cluster.species.bad %in% "PM25"]
cluster.species.weak = cluster.species.weak[! cluster.species.weak %in% "PM25"]

# if OC/EC/OPC is in bad, force remove to weak
oc.ec = c("EC", "OC", "OP")
oc.ec.bad = cluster.species.bad[cluster.species.bad %in% oc.ec]
cluster.species.bad = cluster.species.bad[! cluster.species.bad %in% oc.ec]
cluster.species.weak = append(cluster.species.weak, 
                              oc.ec.bad)

# add Al, Mg, Na into weak, due to the lower reliability in XRF test
cluster.species.weak = append(c("Al", "Mg"), cluster.species.weak) 
# "Na", "K" were removed, and use Na+, K+ instead
# remove the duplicated
cluster.species.weak = cluster.species.weak[!duplicated(cluster.species.weak)]

# if a species exist both in "bad" & "weak", define as "bad"
cluster.species.weak = cluster.species.weak[! (
  cluster.species.weak %in% cluster.species.bad)]

# arrange in alphabetic order 
cluster.species.bad = sort(cluster.species.bad)
cluster.species.weak = sort(cluster.species.weak)

cluster.species.bad
cluster.species.weak

###### Cluster-bad&weak 4. set uncertainties or remove - files for CMD ######
# remove species marked as bad
conc_cluster_pmf = conc_cluster[ ,-which(
  names(conc_cluster) %in% cluster.species.bad)]
unc_cluster_pmf = unc_cluster[ ,-which(
  names(conc_cluster) %in% cluster.species.bad)]

# for those marked as weak, set the uncertainty 3 times as much
unc_cluster_pmf[names(unc_cluster_pmf) %in% cluster.species.weak] = 
  3 * unc_cluster_pmf[names(unc_cluster_pmf) %in% cluster.species.weak]

# add info into column names before combination
colnames(conc_cluster) = paste0("conc_", colnames(conc_cluster))
colnames(unc_cluster) = paste0("unc_", colnames(unc_cluster))

# combining conc & unc files
cmd_cluster_conc_unc = cbind(conc_cluster, unc_cluster)

# Interleave columns of concentration and uncertainty files
## generate a new vector to index the columns of cbind file
interleave.cluster <- rep(1:ncol(unc_cluster), each = 2) + (0:1) * ncol(unc_cluster)
## reorder the cbind file
cmd_cluster_interleave = cmd_cluster_conc_unc[interleave.cluster]

# only keep one Date & cluster column
cmd_cluster_interleave$conc_Date = cmd_cluster_interleave$conc_State = 
  cmd_cluster_interleave$conc_SiteCode = NULL
colnames(cmd_cluster_interleave)[1:3] = c("Date", "State", "SiteCode")
head(cmd_cluster_interleave)

# remove duplicated dates (for data till 2023.Feb, will update the data later)
# cmd_cluster_final = cmd_cluster_interleave[!duplicated(cmd_cluster_interleave$Date), ]
# no longer needed since have fix the problem

# write.csv(cmd_cluster_final, paste0("CSN_cluster_", cluster.Decision, "_conc_unc_PMF_CMD.csv"))

write.csv(cmd_cluster_interleave, paste0("CSN_cluster_", cluster.Decision, "_conc_unc_PMF_CMD.csv"))


##### compare MDL - EPA vs. monthly #####
csn_mdl_epa$Source = "EPA"
csn_daily_fullMDL$Source = "Monthly_CSN"

csn_fullMDL_month = csn_daily_fullMDL
csn_fullMDL_month$Date = csn_fullMDL_month$SiteCode = NULL

csn_mdl_epa_plot = csn_mdl_epa
csn_fullMDL_month_plot = csn_fullMDL_month

csn_mdl_epa_plot = gather(csn_mdl_epa_plot, "Species", "MDL", -Source)
csn_fullMDL_month_plot = gather(csn_fullMDL_month_plot, "Species", "MDL", -Source)

csn_mdl_plot = rbind(csn_mdl_epa_plot, csn_fullMDL_month_plot)

ggplot(NULL, aes(Species, MDL)) +
  geom_boxplot(csn_fullMDL_month_plot) +
  geom_point(csn_mdl_epa_plot) +
  geom_line(csn_mdl_epa_plot) +
  ylab("Percent of above-EPA_MDL concentration") +
  theme_bw()

ggplot(subset(csn_mdl_plot, Species != "PM25"), 
       aes(Species, MDL, col = Source)) +
  geom_boxplot() +
  facet_grid(Source~., scales = "free") + 
  # scale_color_npg() + 
  theme_bw()

ggplot(subset(csn_mdl_plot, Species != "PM25"), 
       aes(Species, MDL, col = Source)) +
  geom_boxplot() +
  facet_grid(Source~., scales = "free") + 
  ylim(0, 0.1) +
  # scale_color_npg() + 
  theme_bw()
  
theme(plot.title = element_text(hjust = 0.05, vjust = 0, size = 16),
        strip.text.x = element_text(size = 20, colour = "Orange", angle = 0),
        legend.title = element_text(face="italic", size=16), # family="Times", colour="red", 
        legend.text = element_text(size=14), 
        axis.title.x = element_text(color="grey25", size = 20, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 20, vjust=1, margin=margin(0,2,0,0)),
        axis.text.x = element_text(color="grey25", size = 14, angle = 0, hjust = 0.5, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.y = element_text(color="grey25", size = 14, angle = 0, hjust = 0.5))

##### plot distribution of clusters #######
library( sf)
library( ggsci)

csn_cluster = read.csv("CSN_RF_cluster5training.csv")
csn_cluster$X = NULL
csn_meta_sites = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original CSN/CSN metadata sample sites 2010-20 use.csv")
csn_meta_sites = select(csn_meta_sites, 
                        State, SiteCode, Latitude, Longitude)

csn_cluster_gps = merge(csn_cluster, csn_meta_sites,
                        all.x = T)
# get US states
us_states <- USAboundaries::us_states()
us_states <- us_states[us_states$state_abbr %in% csn_cluster_gps$State,]

csn_cluster_gps = ddply(csn_cluster_gps, .(Final.Decision), 
                        summarise,
                        Latitude = mean(Latitude),
                        Longitude = mean(Longitude))

csn.cluster.sf <- 
  st_as_sf( csn_cluster_gps, 
            coords = c( x = 'Longitude', y = 'Latitude'),
            crs = 'WGS84')

ggplot( csn.cluster.sf) + 
  geom_sf( data = us_states) +
  geom_sf( aes( color = as.factor(Final.Decision))) +
  scale_fill_npg() +
  geom_sf_text(data = csn.cluster.sf, aes(label = Final.Decision)) +
  theme(legend.position = "none")




