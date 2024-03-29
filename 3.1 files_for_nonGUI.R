##clear environment
# rm(list=ls())

##set working directory
# setwd("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE")
# getwd()
# data.dir <- "/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE"

setwd("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_IMPROVE_ownPC")
getwd()
data.dir <- "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_IMPROVE_ownPC"

library(tidyverse)
library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2) 
library(base)
library(ggrepel)
library(missForest)


##### Combine CSN till & after 2015 - concentrations #####

# import CSN
# csn_daily_before = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/CSN_Interpolation/CSN_interpulation_random-forest_afterLog_before_2015.csv")
# csn_daily_after = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/CSN_Interpolation/CSN_interpulation_random-forest_afterLog_after_2015.csv")

csn_daily_before = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/R - original IMPROVE/CSN_interpulation_random-forest_until_2015_2023.03.csv")
csn_daily_after = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/R - original IMPROVE/CSN_interpulation_random-forest_from_2016_2023.03.csv")

csn_daily_before$X = csn_daily_after$X = NULL
csn_daily_before$Date = as.Date(csn_daily_before$Date)
csn_daily_after$Date = as.Date(csn_daily_after$Date)

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

##### Combine CSN till & after 2015 - the marked NA & replaced qualifiers - interpolated points #####

csn_NA_bef = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE/CSN_Component_with_missing_Before_2015_2023.02.csv")
csn_NA_aft = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE/CSN_Component_with_missing_After_2015_2023.02.csv")
csn_NA_bef$V1 = csn_NA_aft$V1 = NULL
csn_NA_bef$Date = as.Date((csn_NA_bef$Date))
csn_NA_aft$Date = as.Date((csn_NA_aft$Date))

# "OC" already existed in rownames after 2015, so delete it first
csn_NA_aft$OC = NULL

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
# write.csv(csn_daily, "CSN_RFinterpulated_combine_Csubgroup_2023.04.csv")

# OP not used for PMF
# csn_noAllNA_left$OP = NULL

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

# relocate columns
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

# write.csv(csn_noAllNA_left, "CSN_withNA_combine_PMFuncertainty_Estimation_C-subgroup.csv")

# csn_noAllNA_left = fread("CSN_withNA_combine_PMFuncertainty_Estimation_C-subgroup.csv")
csn_NA_intp = csn_noAllNA_left

# change all numeric in species columns to FALSE and rest to NA
csn_NA_intp[, 4:ncol(csn_NA_intp)] = 
  as.logical(is.na(csn_NA_intp[, 4:ncol(csn_NA_intp)]))
summary(csn_NA_intp)

# reorder the dataset for matching
csn_NA_intp = csn_NA_intp[with(
  csn_NA_intp, 
  order(SiteCode, Date)), ]

write.csv(csn_NA_intp, "CSN_TF_logical_InterpolatedOrNot_C-subgroup.csv")

##### CSN - replace PM2.5 with EPA AQS #####
## CSN
# species_daily = fread("CSN_RFinterpulated_combine_2023.04.csv")
csn_daily = fread("CSN_RFinterpulated_combine_Csubgroup_2023.04.csv")
csn_daily$V1 = NULL
csn_daily$Date = as.Date(csn_daily$Date)

# read EPA AQS data
aqs_PM25 = fread("EPA_CSN_AQS_daily_PM25.csv")
aqs_PM25$V1 = NULL
aqs_PM25$Date = as.Date(aqs_PM25$Date)
# aqs_PM25$dup = duplicated(aqs_PM25[, 1:2])
# summary(aqs_PM25$dup)

# match with EPA AQS data
csn_aqs_pm = join(csn_daily,
                        aqs_PM25)

csn_aqs_pm$PM25_Combine = 
  ifelse((is.na(csn_aqs_pm$PM25_AQS) |
            csn_aqs_pm$PM25_AQS <= 2), 
         csn_aqs_pm$PM25, 
         csn_aqs_pm$PM25_AQS)

# check the correlation
plot(csn_aqs_pm$PM25, 
     csn_aqs_pm$PM25_AQS)
plot(csn_aqs_pm$PM25, 
     csn_aqs_pm$PM25_Combine)
summary(lm(csn_aqs_pm$PM25 ~ csn_aqs_pm$PM25_Combine))
summary(lm(csn_aqs_pm$PM25 ~ csn_aqs_pm$PM25_AQS))
summary(csn_aqs_pm$PM25_Combine)

# sum of species vs. PM2.5
csn_aqs_pm$species.sum = sum(csn_aqs_pm[, 4:37],
                                   csn_aqs_pm[, 41],
                                   csn_aqs_pm[, 46:50])

summary(csn_aqs_pm$species.sum > csn_aqs_pm$PM25_Combine)
summary(csn_aqs_pm$species.sum > csn_aqs_pm$PM25)

# reorder the dataframe before comparison
csn_aqs_pm = 
  csn_aqs_pm[
    with(csn_aqs_pm,
         order(SiteCode, Date)), ]

summary(colnames(unc_pmf_cluster)[4:(ncol(unc_pmf_cluster)-1)] == 
          colnames(csn_aqs_pm)[4:(ncol(csn_aqs_pm)-5)])
summary(unc_pmf_cluster$SiteCode == csn_aqs_pm$SiteCode &
          unc_pmf_cluster$Date == csn_aqs_pm$Date)

# detect PM2.5 from interpolation
unc_pmf_cluster.1 = unc_pmf_cluster
unc_pmf_cluster$PM25_interp = F
summary(unc_pmf_cluster$PM25/csn_aqs_pm$PM25_Combine == 4.5)
unc_pmf_cluster$PM25_interp[unc_pmf_cluster$PM25/csn_aqs_pm$PM25_Combine == 4.5] = T
summary(unc_pmf_cluster$PM25/csn_aqs_pm$PM25_Combine)
summary(unc_pmf_cluster$PM25_interp)

# reasign PM value, mainly use AQS, and delete not used
csn_aqs_pm$PM25 = csn_aqs_pm$PM25_Combine
csn_aqs_pm$species.sum = csn_aqs_pm$PM25_Combine = 
  csn_aqs_pm$PM25_AQS = csn_aqs_pm$dup = NULL

csn_aqs_pm$K = NULL

# earlier version, use "_corrected_*.csv" concentrations already reset for PMF (half MDL, etc.)
# write.csv(conc_pmf_cluster, "CSN_concentration_AQS.PM_PMF_C-subgroup_2023.04.csv")
# write.csv(conc_pmf_cluster, "CSN_concentration_AQS.PM_PMF_C-sub_2023.05.csv")
# write.csv(conc_pmf_cluster, "CSN_concentration_AQS.PM_PMF_C-sub_2024.02.csv")

# the reestimation will be combined when preparing GUI/nonGUI data
write.csv(csn_aqs_pm, "CSN_concentration_AQS.PM_PMF_C-sub_2024.03.csv")

##### CSN & IMPROVE - concentration vs. MDL #####
## CSN
# species_daily = fread("CSN_RFinterpulated_combine_2023.04.csv")
species_daily = fread("CSN_RFinterpulated_combine_Csubgroup_2023.04.csv")

## IMPROVE
species_daily = fread("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/IMPROVE_interpulation_random-forest_2023.csv")

species_daily$V1 = NULL
species_daily$Date = as.Date(species_daily$Date)

# get monthly MDL
### CSN
# csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_MDL_monthly.csv")
# csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_MDL_monthly_2023.02.27.csv")
species_mdl = fread("CSN_MDL_C-Sub_monthly_2023.05.csv")
species_mdl$V1 = species_mdl$Cl. = NULL # csn_mdl$OP = 

### IMPROVE
species_mdl = fread("IMPROVE_MDL_monthly_2023.csv")
species_mdl$V1 = species_mdl$ClIon = NULL # imp_mdl$OP = 

# reorder
species_daily = species_daily[with(
  species_daily, 
  order(SiteCode, Date)), ]

# get year, month, for matching with monthly MDL
species_daily_conc = species_daily

species_daily_conc$State = NULL # species_daily_conc$Qualifier = NULL
species_daily_conc$year = year(species_daily_conc$Date)
species_daily_conc$month = month(species_daily_conc$Date)
dim(species_daily_conc)

# reorder the dataset for matching
species_daily_conc = species_daily_conc[with(
  species_daily_conc, 
  order(SiteCode, Date)), ]
species_mdl = species_mdl[with(
  species_mdl, 
  order(SiteCode, year, month)), ]

# check if columns from concentration & MDL datasets match
summary(colnames(species_daily_conc)[3:(ncol(species_daily_conc)-2)] == 
          colnames(species_mdl)[4:ncol(species_mdl)])
dim(species_daily_conc)
dim(species_mdl)

# expand MDL file to daily measurement 
# (in case of interpolation, not used original data directly)
species_daily_conc_date = select(species_daily_conc, 
                                 SiteCode, Date, year, month)
species_daily_fullMDL = merge(species_daily_conc_date, 
                              species_mdl, 
                              all.x = T)
dim(species_daily_conc_date)

# reorder rows the dataset for matching
species_daily_fullMDL = species_daily_fullMDL[with(
  species_daily_fullMDL, 
  order(SiteCode, Date)), ]

species_daily = species_daily[with(
  species_daily, 
  order(SiteCode, Date)), ]

# reorder columns the dataset for matching
setcolorder(species_daily_fullMDL, 
            names(species_daily_conc))

species_daily_fullMDL$year = species_daily_fullMDL$month = 
  species_daily_conc$year = species_daily_conc$month = NULL

# Again check if columns from concentration & new MDL datasets match
summary(colnames(species_daily_conc) == 
          colnames(species_daily_fullMDL))

# double check if date & site match
summary(species_daily_fullMDL$SiteCode == species_daily_conc$SiteCode)
summary(species_daily_fullMDL$Date == species_daily_conc$Date)
summary(colnames(species_daily_fullMDL) == colnames(species_daily_conc))

# compare concentration and MDL of a given component
cols_to_extract <- setdiff(names(species_daily_conc), 
                           c("SiteCode", "Date"))
species_conc = species_daily_conc[, ..cols_to_extract]
species_mdl = species_daily_fullMDL[, ..cols_to_extract]

species_conc_mdl = data.frame(Map(">", species_conc, species_mdl))
setDT(species_conc_mdl)

species_conc[1:3, 1:10]
species_mdl[1:3, 1:10]
species_conc_mdl[1:3, 1:10]

# check variable class & dataset dimenssion
sapply(species_conc, class)
sapply(species_mdl, class)
sapply(species_conc_mdl, class)
dim(species_conc)
dim(species_mdl)
dim(species_conc_mdl)

species_conc_mdl_Site = cbind(species_daily[, 1:3], species_conc_mdl)

####### For site & date match check, finished!
species_conc_mdl_randomsite = subset(cbind(species_daily[, 1:3], 
                                           species_conc_mdl), 
                                     SiteCode == "60371103") # "BADL1", "60371103"
species_mdl_randomsite = subset(species_daily_fullMDL, 
                                SiteCode == "60371103")
species_conc_randomsite = subset(species_daily_conc, 
                                 SiteCode == "60371103")

conc_pmf_randomsite = species_conc_mdl_randomsite[, 4:ncol(species_conc_mdl_randomsite)] * 
  species_conc_randomsite[, 3:ncol(species_conc_randomsite)] +
  (!species_conc_mdl_randomsite[, 4:ncol(species_conc_mdl_randomsite)]) * 
  species_mdl_randomsite[, 3:ncol(species_mdl_randomsite)] * 0.5
conc_pmf_randomsite_check = subset(conc_pmf, SiteCode == "60371103")

conc_pmf_randomsite[1:3, 1:10]
conc_pmf_randomsite_check[1:3, 4:13]
species_conc_mdl_randomsite[1:3, 4:13]

####### For site & date match check, finished!

# only SiteCode & Date before species
species_conc_mdl_cluster$K = species_conc_mdl_cluster$State = NULL

# write.csv(species_conc_mdl_Site, "CSN_conc_vs_MDL_C-subgroup_corrected_2023.05.csv")
# write.csv(species_conc_mdl_Site, "CSN_conc_vs_MDL_C-subgroup_corrected_2024.02.csv")
write.csv(species_conc_mdl_Site, "CSN_conc_vs_MDL_C-subgroup_corrected_2024.03.csv")

species_conc_mdl_Site = subset(species_conc_mdl_Site, !is.na(OP))

# write.csv(species_conc_mdl_Site, "IMPROVE_conc_vs_MDL_C-subgroup_corrected_2023.csv")
write.csv(species_conc_mdl_Site, "IMPROVE_conc_vs_MDL_C-subgroup_corrected_2024.csv")


