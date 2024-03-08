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

# library(bspec) # signal-to-noise ratio, snr{}


#### PM2.5 concentration distribution - Sites & Clusters ####

## CSN
# species_daily$year = year(species_daily$Date)
species_daily = fread("CSN_RFinterpulated_combine_2023.04.csv")
species_daily = fread("CSN_RFinterpulated_combine_Csubgroup_2023.04.csv")
pm_cluster = read.csv("CSN_RF_cluster5training.csv")

## IMPROVE
species_daily = fread("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/IMPROVE_interpulation_random-forest_2023.csv")
pm_cluster = read.csv("IMPROVE_RF_cluster_SiteLevel.csv")
pm_cluster$X =NULL

species_daily$V1 = NULL
species_daily$Date = as.Date(species_daily$Date)

species_daily = species_daily[with(
  species_daily, 
  order(SiteCode, Date)), ]

# setDT(species_daily)
# sapply(species_daily, class)

# species_daily = species_daily[with(
#   species_daily, 
#   order(State, SiteCode, Date)), ]
# write.csv(species_daily, "CSN_RFinterpulated_combine_2023.04.csv")

PM_avg_site = ddply(species_daily, 
               .(SiteCode), 
               summarise,
               PM25 = mean(PM25))

PM_avg_site = merge(PM_avg_site, pm_cluster)

# the mean PM level of each cluster
PM_cluster = ddply(PM_avg_site, 
                   .(Final.Decision), 
                   summarise,
                   PM25 = mean(PM25))

# number of sites in each cluster
sites_in_cluster = data.frame(table(pm_cluster$Final.Decision))
colnames(sites_in_cluster) = c("Final.Decision", "No.Site.in.Cluster")

# get the GPS of sites
cty_rural_urban = read.csv("IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$X.1 = NULL

site_gps = select(cty_rural_urban, SiteCode, Longitude, Latitude)

PM_site = merge(PM_avg_site, site_gps)
PM_site = merge(PM_site, pm_cluster)

sapply(PM_site, class)

# cluster with the highest and lowest PM
cluster.top.PM = 
  PM_cluster$Final.Decision[
    which(
      PM_cluster$PM25 == 
        max(PM_cluster$PM25))]
cluster.least.PM = 
  PM_cluster$Final.Decision[
    which(
      PM_cluster$PM25 == 
        min(PM_cluster$PM25))]

# sites in clusters with the highest and lowest PM
sites.top.PM.cluster = PM_avg_site$SiteCode[
  which(
    PM_avg_site$Final.Decision == 
      cluster.top.PM)]
sites.least.PM.cluster = PM_avg_site$SiteCode[
  which(
    PM_avg_site$Final.Decision == 
      cluster.least.PM)]

# number of sites in each cluster
sites_in_cluster = data.frame(table(pm_cluster$Final.Decision))
colnames(sites_in_cluster) = c("Final.Decision", "No.Site.in.Cluster")

PM_site$PM.level = "Middle"
PM_site$PM.level[
  PM_site$SiteCode %in% 
    sites.top.PM.cluster] = "Highest.PM"
PM_site$PM.level[
  PM_site$SiteCode %in% 
    sites.least.PM.cluster] = "Lowest.PM"


# map site-average concentration
UScounty <- map_data("county")
ggplot(PM_site, 
       aes(Longitude, Latitude, color = PM25, group = PM.level)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.8, aes(shape = PM.level)) +
  scale_color_continuous(low = "steelblue", high = "darkorange2") +
  theme_bw()

# check randomly selected site
ggplot(subset(PM_site, SiteCode == "60371103"), 
       aes(Longitude, Latitude, color = PM25, group = PM.level)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.8, aes(shape = PM.level)) +
  scale_color_continuous(low = "steelblue", high = "darkorange2") +
  theme_bw()

###### Origin_MDL - prepare site & date matched MDL ###### 
species_daily_conc = species_daily
# species_daily_conc = species_daily_cluster_top_PM
species_daily_conc$State = NULL # species_daily_conc$Qualifier = NULL
species_daily_conc$year = year(species_daily_conc$Date)
species_daily_conc$month = month(species_daily_conc$Date)
dim(species_daily_conc)

# get monthly MDL
### CSN
# csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_MDL_monthly.csv")
# csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_MDL_monthly_2023.02.27.csv")
species_mdl = fread("CSN_MDL_C-Sub_monthly_2023.05.csv")
species_mdl$V1 = species_mdl$Cl. = NULL # csn_mdl$OP = 

### IMPROVE
species_mdl = fread("IMPROVE_MDL_monthly_2023.csv")
species_mdl$V1 = species_mdl$ClIon = NULL # imp_mdl$OP = 

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

# test uncertainty, error_fraction 
# comp_error_fraction = fread("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/CSN_k_Error Fraction.csv")
comp_error_fraction = fread("CSN_k_Error-Fraction_2023.04.csv")
comp_error_fraction$data = NULL

# set EF of subgroups to the same of OC, EC
comp_error_fraction$EC3 = comp_error_fraction$EC2 = comp_error_fraction$EC1 = comp_error_fraction$EC
comp_error_fraction$OC4 = comp_error_fraction$OC3 = comp_error_fraction$OC2 = comp_error_fraction$OC1 = comp_error_fraction$OC
comp_error_fraction$OP = comp_error_fraction$EC

comp_error_fraction = species_col_reorder(comp_error_fraction)
species_conc_mdl = species_col_reorder(species_conc_mdl)
species_conc = species_col_reorder(species_conc)
species_mdl = species_col_reorder(species_mdl)

# only keep species existing in CSN dataset
# get common colnames
mdl_ef_cols <- intersect(names(species_conc_mdl), 
                         names(comp_error_fraction))
# Subset data frames using common rownames and column names
comp_error_fraction <- comp_error_fraction[, ..mdl_ef_cols]

# expand error_fraction file to one with same row number as concentration file
comp_ef <- 
  comp_error_fraction[1][
    rep(1, nrow(species_conc_mdl)), 
    .SD] # .SD selects the data for each repetition.

# assign data conditionally to concentration and uncertainty values for PMF
summary(colnames(species_conc_mdl) == colnames(species_conc) & 
          colnames(species_conc_mdl) == colnames(species_mdl) &
          colnames(species_conc_mdl) == colnames(comp_ef))
summary(dim(species_conc_mdl) == dim(species_conc) & 
          dim(species_conc_mdl) == dim(species_mdl) & 
          dim(species_conc_mdl) == dim(comp_ef))
summary(sapply(species_conc_mdl, class) %in% "logical")
summary(sapply(species_conc, class) %in% "numeric")
summary(sapply(species_mdl, class) %in% "numeric")

# estimate the uncertainty 
# EPA PMF5.0 user guide, page 16-17, function 5-1 & 5-2
conc_pmf = species_conc_mdl * species_conc +
  (!species_conc_mdl) * species_mdl * 0.5
# unc_pmf_1 = species_conc_mdl * (species_mdl / 3 + comp_ef * species_conc) +
#   (!species_conc_mdl) * 5/6 * species_mdl
unc_pmf = species_conc_mdl * (((species_mdl / 2)^2 + (comp_ef * species_conc)^2)^0.5) +
  (!species_conc_mdl) * 5/6 * species_mdl

conc_pmf$PM25 = species_daily_conc$PM25
unc_pmf$PM25 = 3 * species_daily_conc$PM25

# for those using refilling (NA or unacceptable Flag), add the uncertainty *1.5 
#species_NA_intp = fread("CSN_TF_logical_InterpolatedOrNot.csv")
species_NA_intp = fread("CSN_TF_logical_InterpolatedOrNot_C-subgroup.csv")

species_NA_intp = fread("IMPROVE_TF_logical_InterpolatedOrNot_allSpecies.csv")

species_NA_intp = species_col_reorder(species_NA_intp)
species_NA_intp$V1 = NULL

# double check if rows & columns match
summary(species_NA_intp$SiteCode == species_daily_conc$SiteCode)
summary(species_NA_intp$Date == species_daily_conc$Date)
summary(colnames(species_NA_intp)[4:ncol(species_NA_intp)] == 
          colnames(species_daily_conc)[3:ncol(species_daily_conc)])

cols_comp = col_comp(species_NA_intp, "Al", "PM25")

species_NA_intp_species = species_NA_intp[, 4:ncol(species_NA_intp)]
species_NA_intp_species[species_NA_intp_species == TRUE] <- 1.5
species_NA_intp_species[species_NA_intp_species == FALSE] <- 1


summary(colnames(unc_pmf) == colnames(species_NA_intp_species))
unc_pmf = unc_pmf * species_NA_intp_species

summary(species_daily$SiteCode == species_daily_conc$SiteCode)
summary(species_daily$Date == species_daily_conc$Date)

# insert date into  concentration & uncertainty datasets
conc_pmf <- cbind(species_daily[, 1:3], conc_pmf) # species_daily[, 1:4]
unc_pmf <- cbind(species_daily[, 1:3], unc_pmf) # species_daily[, 1:4]
species_conc_mdl_Site = cbind(species_daily[, 1:3], species_conc_mdl)

################### For site & date match check, finished!
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

################### For site & date match check, finished!

dim(conc_pmf); dim(unc_pmf)

# write.csv(conc_pmf, "CSN_concentration_for_PMF_2023.04.csv")
# write.csv(unc_pmf, "CSN_uncertainty_for_PMF_2023.04.csv")
# write.csv(species_conc_mdl_Site, "CSN_concentration_vs_MDL_2023.04.csv")

# write.csv(conc_pmf, "CSN_concentration_for_PMF_C-subgroup_2023.04.csv")
# write.csv(unc_pmf, "CSN_uncertainty_for_PMF_C-subgroup_2023.04.csv")
# write.csv(species_conc_mdl_Site, "CSN_concentration_vs_MDL_C-subgroup_2023.04.csv")

# write.csv(conc_pmf, "CSN_PMF_C-subgroup_PM_corrected_2023.05.csv")
# write.csv(unc_pmf, "CSN_PMF_C-subgroup_corrected_2023.05.csv")
# write.csv(species_conc_mdl_Site, "CSN_conc_vs_MDL_C-subgroup_corrected_2023.05.csv")

species_conc_mdl_cluster$K = NULL

write.csv(conc_pmf, "CSN_PMF_C-subgroup_PM_corrected_2024.02.csv")
write.csv(unc_pmf, "CSN_PMF_C-subgroup_corrected_2024.02.csv")
write.csv(species_conc_mdl_Site, "CSN_conc_vs_MDL_C-subgroup_corrected_2024.02.csv")

dim(conc_pmf); dim(unc_pmf); dim(species_conc_mdl_Site)
summary(conc_pmf); summary(unc_pmf); summary(species_conc_mdl_Site)

conc_pmf = subset(conc_pmf, !is.na(OP))
unc_pmf = subset(unc_pmf, !is.na(OP))
species_conc_mdl_Site = subset(species_conc_mdl_Site, !is.na(OP))

write.csv(conc_pmf, "IMPROVE_PMF_C-subgroup_PM_corrected_2023.csv")
write.csv(unc_pmf, "IMPROVE_PMF_C-subgroup_corrected_2023.csv")
write.csv(species_conc_mdl_Site, "IMPROVE_conc_vs_MDL_C-subgroup_corrected_2023.csv")


###### Origin_MDL - signal-to-noise SNR S/N estimation ###### 
# conc_pmf = fread("CSN_PMF_C-subgroup_PM_corrected_2023.05.csv")
# unc_pmf = fread("CSN_PMF_C-subgroup_corrected_2023.05.csv")
# species_conc_mdl_Site = fread("CSN_conc_vs_MDL_C-subgroup_corrected_2023.05.csv")

conc_pmf = fread("CSN_PMF_C-subgroup_PM_corrected_2024.02.csv")
unc_pmf = fread("CSN_PMF_C-subgroup_corrected_2024.02.csv")
conc_pmf$K = unc_pmf$K = NULL
species_conc_mdl_Site = fread("CSN_conc_vs_MDL_C-subgroup_corrected_2024.02.csv")

conc_pmf = fread("IMPROVE_PMF_C-subgroup_PM_corrected_2023.csv")
unc_pmf = fread("IMPROVE_PMF_C-subgroup_corrected_2023.csv")
species_conc_mdl_Site = fread("IMPROVE_conc_vs_MDL_C-subgroup_corrected_2023.csv")

conc_pmf$V1 = unc_pmf$V1 = species_conc_mdl_Site$V1 = NULL

# conc.col = ncol(conc_pmf)
# identify PM & species columns
species_cols <- setdiff(names(conc_pmf),
                        c("SiteCode", "Date", "State"))

# according to EPA PMF 5.0 User Guide.pdf, function 5-3 & 5-4 from page 23
conc_unc_diff_pmf = 
  (conc_pmf[, ..species_cols] - 
     unc_pmf[, ..species_cols])/
  unc_pmf[, ..species_cols] * 
  species_conc_mdl_Site[, ..species_cols]

conc_unc_diff_pmf = cbind(species_daily[, 1:3], conc_unc_diff_pmf)

conc_unc_diff_pmf$Date = conc_unc_diff_pmf$State = NULL

sapply(conc_unc_diff_pmf, class)

###### calculate SNR of each site
# calculate the sums for each column and the number of occurrences for each group
site_sums <- conc_unc_diff_pmf[, lapply(.SD, sum), by = SiteCode]
site_counts <- conc_unc_diff_pmf[, .N, by = SiteCode]
site_snr <- site_sums[site_counts, on = "SiteCode"]

# divide all columns (excluding SiteCode and N) by the counts
cols_to_divide <- setdiff(names(site_snr), c("SiteCode", "N"))
site_snr[, (cols_to_divide) := 
           lapply(.SD, 
                  function(col) 
                    col / N), 
         .SDcols = cols_to_divide]

# drop the counts column
site_snr[, N := NULL]

# merge with cluster info
conc_unc_diff_pmf_cluster = merge(pm_cluster, 
                                  conc_unc_diff_pmf)
conc_unc_diff_pmf_cluster$SiteCode = NULL
setDT(conc_unc_diff_pmf_cluster)

# SNR for clusters
cluster_sums <- conc_unc_diff_pmf_cluster[, lapply(.SD, sum), by = Final.Decision]
cluster_counts <- conc_unc_diff_pmf_cluster[, .N, by = Final.Decision]
cluster_snr <- cluster_sums[cluster_counts, on = "Final.Decision"]

cols_to_divide <- setdiff(names(cluster_snr), c("Final.Decision", "N"))
cluster_snr[, (cols_to_divide) := 
              lapply(.SD, 
                     function(col) 
                       col / N), 
            .SDcols = cols_to_divide]
cluster_snr[, N := NULL]

# data output
write.csv(site_snr, "CSN_Site_SNR_PMF_C-subgroup_PM_corrected.csv")
write.csv(cluster_snr, "CSN_cluster_SNR_PMF_C-subgroup_PM_corrected.csv")

write.csv(site_snr, "IMPROVE_Site_SNR_PMF_C-subgroup_PM_corrected.csv")
write.csv(cluster_snr, "IMPROVE_cluster_SNR_PMF_C-subgroup_PM_corrected.csv")

###### Origin_MDL - daily PM of selected cluster - PMF priori try, DONE! Jump!  ###### 
# determine the cluster with highest average PM concentrations
PM_cluster = merge(PM_cluster, sites_in_cluster)
Cluster.top.PM = PM_cluster$Final.Decision[
  which(PM_cluster$PM25 == 
          max(PM_cluster$PM25))]
# cluster 6, with five sites

# map the sites with the highest PM average
ggplot(subset(PM_site, Final.Decision == Cluster.top.PM), 
       aes(Longitude, Latitude, color= PM25)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6, shape = 16) 

# extract concentration & uncertainty of selected sites for PMF
species_select_conc = subset(conc_pmf, SiteCode %in% 
                               pm_cluster$SiteCode[which(
                                 pm_cluster$Final.Decision == Cluster.top.PM)])
species_select_unc = subset(unc_pmf, SiteCode %in% 
                              pm_cluster$SiteCode[which(
                                pm_cluster$Final.Decision == Cluster.top.PM)])

species_select_conc_use = subset(species_select_conc, Date < as.Date("2014-01-01"))
species_select_unc_use = subset(species_select_unc, Date < as.Date("2014-01-01"))
species_select_site_row = data.frame(table(species_select_conc_use$SiteCode))
colnames(species_select_site_row)[1] = c("SiteCode")

selectedSite = 3
species_select_conc_1site = subset(species_select_conc_use, 
                                   SiteCode == 
                                     species_select_site_row$SiteCode[selectedSite])
species_select_unc_1site = subset(species_select_unc_use, 
                                  SiteCode == 
                                    species_select_site_row$SiteCode[selectedSite])

######## total OC/EC
# cluster
write.csv(species_select_conc_use, 
          paste0("CSN_C", Cluster.top.PM, "_2011-14_conc.csv"))
write.csv(species_select_unc_use, 
          paste0("CSN_C", Cluster.top.PM, "_2011-14_unc.csv"))
write.csv(species_select_site_row, 
          paste0("CSN_C", Cluster.top.PM, "_2011-14_date_number_of_site.csv"))
# site
write.csv(species_select_conc_1site, 
          paste0("CSN_C", Cluster.top.PM, "_S", selectedSite, "_2011-14_conc.csv"))
write.csv(species_select_unc_1site, 
          paste0("CSN_C", Cluster.top.PM, "_S", selectedSite, "_2011-14_unc.csv"))


######## C-subgroup
# cluster
write.csv(species_select_conc_use, 
          paste0("CSN_C", Cluster.top.PM, "_C-sub_2011-14_conc.csv"))
write.csv(species_select_unc_use, 
          paste0("CSN_C", Cluster.top.PM, "_C-sub_2011-14_unc.csv"))
write.csv(species_select_site_row, 
          paste0("CSN_C", Cluster.top.PM, "_C-sub_2011-14_date_number_of_site.csv"))
# site
write.csv(species_select_conc_1site, 
          paste0("CSN_C", Cluster.top.PM, "_S", selectedSite, "_C-sub_2011-14_conc.csv"))
write.csv(species_select_unc_1site, 
          paste0("CSN_C", Cluster.top.PM, "_S", selectedSite, "_C-sub_2011-14_unc.csv"))

# the result for the randomly selected site "60371103" is not good
## checking original concentrations
species_daily_conc_cluster6_1site = subset(species_daily,
                                           Date < as.Date("2014-01-01") &
                                             SiteCode == species_select_site_row$SiteCode[3])
summary(species_daily_conc_cluster6_1site)
sapply(species_daily_conc_cluster6_1site, max)

### check the distribution high value of given component
conc_cluster6_1site_highK = subset(species_daily_conc_cluster6_1site, 
                                   K > 0.2)

###### Origin_MDL - match with AQS PM2.5, DONE! Jump! ###### 
# import data
# conc_pmf = read.csv("CSN_concentration_for_PMF_C-subgroup_2023.04.csv")
# unc_pmf = read.csv("CSN_uncertainty_for_PMF_C-subgroup_2023.04.csv")

# conc_pmf = read.csv("CSN_PMF_C-subgroup_PM_corrected_2023.05.csv")
# unc_pmf = read.csv("CSN_PMF_C-subgroup_corrected_2023.05.csv")

conc_pmf = fread("CSN_PMF_C-subgroup_PM_corrected_2024.02.csv")
unc_pmf = fread("CSN_PMF_C-subgroup_corrected_2024.02.csv")
conc_pmf$K = unc_pmf$K = NULL

conc_pmf$V1 = unc_pmf$V1 = NULL
conc_pmf$Date = as.Date(conc_pmf$Date)
unc_pmf$Date = as.Date(unc_pmf$Date)

# double check if there are duplicates
conc_pmf$dup = duplicated(conc_pmf[, 1:3])
summary(conc_pmf$dup)
conc_pmf_dup = subset(conc_pmf, dup)

# read EPA AQS data
aqs_PM25 = fread("EPA_CSN_AQS_daily_PM25.csv")
aqs_PM25$V1 = NULL
aqs_PM25$Date = as.Date(aqs_PM25$Date)
# aqs_PM25$dup = duplicated(aqs_PM25[, 1:2])
# summary(aqs_PM25$dup)

# match conc & unc with cluster info 
conc_pmf_cluster = merge(conc_pmf, pm_cluster, all.x = T)
unc_pmf_cluster = merge(unc_pmf, pm_cluster, all.x = T)

# match with EPA AQS data
conc_pmf_cluster = join(conc_pmf_cluster,
                         aqs_PM25)

conc_pmf_cluster$PM25_Combine = ifelse((is.na(conc_pmf_cluster$PM25_AQS) |
                                          conc_pmf_cluster$PM25_AQS <= 2), 
                                       conc_pmf_cluster$PM25, 
                                       conc_pmf_cluster$PM25_AQS)
plot(conc_pmf_cluster$PM25, 
     conc_pmf_cluster$PM25_AQS)
plot(conc_pmf_cluster$PM25, 
     conc_pmf_cluster$PM25_Combine)
summary(lm(conc_pmf_cluster$PM25 ~ conc_pmf_cluster$PM25_Combine))
summary(lm(conc_pmf_cluster$PM25 ~ conc_pmf_cluster$PM25_AQS))
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
          colnames(conc_pmf_cluster)[4:(ncol(conc_pmf_cluster)-5)])
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

# write.csv(conc_pmf_cluster, "CSN_concentration_AQS.PM_PMF_C-sub_2023.05.csv")
# write.csv(unc_pmf_cluster, "CSN_uncertainty_AQS.PM_PMF_C-sub_2023.05.csv")

conc_pmf_cluster$K = unc_pmf_cluster$K = NULL
write.csv(conc_pmf_cluster, "CSN_concentration_AQS.PM_PMF_C-sub_2024.02.csv")
write.csv(unc_pmf_cluster, "CSN_uncertainty_AQS.PM_PMF_C-sub_2024.02.csv")

####################################################################
###### Origin_MDL - data for PMF GUI ###### 
####################################################################

###### read basic data ###### 
# conc_pmf = fread("CSN_concentration_AQS.PM_PMF_C-sub_2023.05.csv")
# unc_pmf = fread("CSN_uncertainty_AQS.PM_PMF_C-sub_2023.05.csv")

conc_pmf = fread("CSN_concentration_AQS.PM_PMF_C-sub_2024.02.csv")
unc_pmf = fread("CSN_uncertainty_AQS.PM_PMF_C-sub_2024.02.csv")
pm_cluster = read.csv("CSN_RF_cluster5training.csv")

conc_pmf$V1 = unc_pmf$V1 = pm_cluster$V1 = NULL
conc_pmf$Date = as.Date(conc_pmf$Date)
unc_pmf$Date = as.Date(unc_pmf$Date)

conc_pmf = merge(conc_pmf, pm_cluster)
unc_pmf = merge(unc_pmf, pm_cluster)

# Create subfolders under the current working directory
Clusterfolder <- "PMF_GUI_Cluster"
# Clusterfolder <- "PMF_GUI_Cluster_originPM"
# dir.create(Clusterfolder)

Sitefolder <- "PMF_GUI_Site"
# Sitefolder <- "PMF_GUI_Site_originPM"
# dir.create(Sitefolder)

###### extract concentration & uncertainty of single cluster for PMF GUI ###### 
###### 2024.02 Need UPDATE, NOT DEAL WITH EXTREMES YET! ###### 

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

####################################################################
###### Non-GUI-1 - ALL values - Origin_MDL ###### 
####################################################################

###### read data files ######

######### Above MDL or not
#### CSN
# gui_conc_mdl_cluster = fread("CSN_concentration_vs_MDL_C-subgroup_2023.04.csv") # original MDL in CSN
# gui_conc_mdl_cluster = fread("CSN_conc_vs_MDL_C-subgroup_corrected_2023.05.csv") # original MDL in CSN
gui_conc_mdl_cluster = fread("CSN_conc_vs_MDL_C-subgroup_corrected_2024.02.csv") # original MDL in CSN
gui_conc_mdl_cluster$V1 = NULL

#### IMPROVE
gui_conc_mdl_cluster = fread("IMPROVE_conc_vs_MDL_C-subgroup_corrected_2023.csv") # original MDL 
gui_conc_mdl_cluster$V1 = NULL

gui_conc_mdl_cluster$Date = as.Date(gui_conc_mdl_cluster$Date)

######### Signal-to-noise ratio
#### CSN
# cluster_snr = read.csv("CSN_cluster_SNR_for_PMF.csv") # No carbon subgroups
# cluster_snr = read.csv("CSN_cluster_SNR_for_PMF_C-subgroup.csv") # carbonaceous substances
cluster_snr = read.csv("CSN_cluster_SNR_PMF_C-subgroup_PM_corrected.csv")

#### IMPROVE
cluster_snr = read.csv("IMPROVE_cluster_SNR_PMF_C-subgroup_PM_corrected.csv") # carbonaceous substances

cluster_snr$X = NULL


#########  OOB & Missing rate
OOB_comb_avg = read.csv("CSN_OOBerror_random-forest_for_PMF.csv")
miss_comb_avg = read.csv("CSN_Missing_Qualifier_interpolation_for_PMF.csv")
OOB_comb_avg$X = miss_comb_avg$X = NULL

#########  conc & unc
#### CSN
# conc_pmf = read.csv("CSN_concentration_AQS.PM_PMF_C-subgroup_2023.04.csv")
# unc_pmf = read.csv("CSN_uncertainty_AQS.PM_PMF_C-subgroup_2023.04.csv")

# conc_pmf = fread("CSN_concentration_AQS.PM_PMF_C-sub_2023.05.csv")
# unc_pmf = fread("CSN_uncertainty_AQS.PM_PMF_C-sub_2023.05.csv")

conc_pmf = fread("CSN_concentration_AQS.PM_PMF_C-sub_2024.02.csv")
unc_pmf = fread("CSN_uncertainty_AQS.PM_PMF_C-sub_2024.02.csv")
pm_cluster = read.csv("CSN_RF_cluster5training.csv")

#### IMPROVE
conc_pmf = fread("IMPROVE_PMF_C-subgroup_PM_corrected_2023.csv")
unc_pmf = fread("IMPROVE_PMF_C-subgroup_corrected_2023.csv")
pm_cluster = read.csv("IMPROVE_RF_cluster_SiteLevel.csv")

conc_pmf$V1 = unc_pmf$V1 = NULL
pm_cluster$X = NULL

conc_pmf$Date = as.Date(conc_pmf$Date)
unc_pmf$Date = as.Date(unc_pmf$Date)

conc_pmf = merge(conc_pmf, pm_cluster)
unc_pmf = merge(unc_pmf, pm_cluster)

# the sequence may change after merging, reorder to ensure the sequence for those need row-by-row process
conc_pmf = conc_pmf[with(
  conc_pmf, 
  order(State, SiteCode, Date)), ]
unc_pmf = unc_pmf[with(
  unc_pmf, 
  order(State, SiteCode, Date)), ]
gui_conc_mdl_cluster = gui_conc_mdl_cluster[with(
  gui_conc_mdl_cluster, 
  order(State, SiteCode, Date)), ]

##### if using total OC&EC 
OC.EC.sub = c("EC1", "EC2", "EC3",
              "OC1", "OC2", "OC3", "OC4")
conc_pmf[ , OC.EC.sub] <- list(NULL)
unc_pmf[ , OC.EC.sub] <- list(NULL)
OOB_comb_avg[ , OC.EC.sub] <- list(NULL)
miss_comb_avg[ , OC.EC.sub] <- list(NULL)
cluster_snr[ , OC.EC.sub] <- list(NULL)
gui_conc_mdl_cluster[ , OC.EC.sub] <- list(NULL)

# create a dataframe to save the decision of weak, bad, or strong 
species.name = 
  setdiff(names(conc_pmf),
          c("SiteCode", "Date", "X", "V1", 
            "State", "Final.Decision"))
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
# Clusterfolder <- "PMF_NoGUI_cluster"
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
  conc_mdl_cluster = subset(gui_conc_mdl_cluster,
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
  interleave.col.order <- 
    rep(1:ncol(unc_cluster_pmf), each = 2) + 
    (0:1) * ncol(unc_cluster_pmf)
  ## reorder the cbind file
  cmd_cluster_interleave = cmd_cluster_conc_unc[interleave.col.order]
  
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
  nonGUI_disp_species <- 
    cmd_species_class_cluster[
      i, 
      c(cluster.species.strong, cluster.species.weak)]
  species.name.use = 
    species.name[
      species.name %in% 
        c(cluster.species.strong, cluster.species.weak)]
  
  # add PM25
  species.name.use = append(species.name.use, "PM25")
  
  nonGUI_disp_species <- nonGUI_disp_species[, species.name.use]
  cmd_species_class_cluster$style.weak.good[i] <- 
    paste0("/", nonGUI_disp_species, collapse = "")
  
  write.csv(cmd_species_class_cluster, 
            paste0(Clusterfolder, "/CSN_PMF_CMD_StrongWeakBad_Cluster.csv"))
  
  write.csv(cmd_species_class_cluster, 
            paste0(Clusterfolder, "/CSN_noCsub_PMF_CMD_StrongWeakBad_Cluster.csv"))
}


################################################################################
#### Non-GUI-2 - NO EXTREMES - Origin_MDL ####
################################################################################

#### read data files & create dataframes to store results ####

###### data - whether the conc is above the unc
#### CSN
# species_conc_mdl_cluster = fread("CSN_concentration_vs_MDL_C-subgroup_2023.04.csv") # original MDL in CSN
# species_conc_mdl_cluster = fread("CSN_conc_vs_MDL_C-subgroup_corrected_2023.05.csv") # original MDL in CSN
# species_conc_mdl_cluster = fread("CSN_conc_vs_MDL_C-subgroup_corrected_2024.02.csv")

#### IMPROVE
# species_conc_mdl_cluster = fread("IMPROVE_conc_vs_MDL_C-subgroup_corrected_2023.csv") # original MDL 

# species_conc_mdl_cluster$V1 = NULL
# species_conc_mdl_cluster$Date = as.Date(species_conc_mdl_cluster$Date)

######  data - Signal-to-noise ratio
#### CSN
# cluster_snr = read.csv("CSN_cluster_SNR_for_PMF_C-subgroup.csv") 
# cluster_snr = read.csv("CSN_cluster_SNR_PMF_C-subgroup_PM_corrected.csv")

#### IMPROVE
# cluster_snr = read.csv("IMPROVE_cluster_SNR_PMF_C-subgroup_PM_corrected.csv")

# cluster_snr$X = NULL

###### OOB & Missing rate
# OOB_comb_avg = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_OOBerror_random-forest_for_PMF.csv")
# miss_comb_avg = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_Missing_Qualifier_interpolation_for_PMF.csv")
OOB_comb_avg = read.csv("CSN_OOBerror_random-forest_for_PMF.csv")
miss_comb_avg = read.csv("CSN_Missing_Qualifier_interpolation_for_PMF.csv")
OOB_comb_avg$X = miss_comb_avg$X = NULL

###### data - conc & unc
# uncertainties would be re-estimated after removing and replacing the extreme data points

#### CSN
# conc_pmf = fread("CSN_concentration_AQS.PM_PMF_C-sub_2023.05.csv")
# unc_pmf = fread("CSN_uncertainty_AQS.PM_PMF_C-sub_2023.05.csv")

conc_pmf = fread("CSN_concentration_AQS.PM_PMF_C-sub_2024.02.csv")
# unc_pmf = fread("CSN_uncertainty_AQS.PM_PMF_C-sub_2024.02.csv")
# unc_pmf$Final.Decision = NULL

#### IMPROVE
conc_pmf = fread("IMPROVE_PMF_C-subgroup_PM_corrected_2023.csv")
# unc_pmf = fread("IMPROVE_PMF_C-subgroup_corrected_2023.csv")

conc_pmf$Final.Decision = conc_pmf$V1 = NULL

###### data - site clustering results
#### CSN
# pm_cluster = fread("CSN_RF_cluster5training.csv")

#### IMPROVE
# pm_cluster = fread("IMPROVE_RF_cluster_SiteLevel.csv")

# conc_pmf$V1 = unc_pmf$V1 = pm_cluster$V1 = NULL
conc_pmf$Date = as.Date(conc_pmf$Date)
# unc_pmf$Date = as.Date(unc_pmf$Date)

# conc_pmf = merge(conc_pmf, pm_cluster)
# unc_pmf = merge(unc_pmf, pm_cluster)

# the sequence may change after merging, reorder to ensure the sequence for those need row-by-row process
conc_pmf = 
  conc_pmf[with(
  conc_pmf, 
  order(SiteCode, Date)), ]
# unc_pmf = 
#   unc_pmf[with(
#   unc_pmf, 
#   order(SiteCode, Date)), ]

# species_conc_mdl_cluster = species_conc_mdl_cluster[with(
#   species_conc_mdl_cluster, 
#   order(SiteCode, Date)), ]

##### test uncertainty, error_fraction 
# comp_error_fraction = fread("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/CSN_k_Error Fraction.csv")
comp_error_fraction = fread("CSN_k_Error-Fraction_2023.04.csv")
comp_error_fraction$data = NULL
comp_error_fraction = species_col_reorder(comp_error_fraction)

# set EF of subgroups to the same of OC, EC
comp_error_fraction$EC3 = comp_error_fraction$EC2 = comp_error_fraction$EC1 = comp_error_fraction$EC
comp_error_fraction$OC4 = comp_error_fraction$OC3 = comp_error_fraction$OC2 = comp_error_fraction$OC1 = comp_error_fraction$OC
comp_error_fraction$OP = comp_error_fraction$EC

##### VC for dispersion normalization, based on ERA5 data 

dn_vc = fread("Nearest_ERA5_Wind_BLH_VC_CSN&IMPROVE.csv")
dn_vc$V1 = NULL
dn_vc$Date = as.Date(dn_vc$Date)
dn_vc_use = select(dn_vc,
                   Dataset, SiteCode, Date, VC_coef)
dn_vc_use = 
  dn_vc_use[
    with(dn_vc_use,
      order(Dataset, SiteCode, Date)), ]

#### get SiteCode & site.serial matching list for both CSN & IMPROVE datasets

# site_code_serial = select(dn_vc_use, Dataset, SiteCode)
# site_code_serial = unique(site_code_serial)
# site_code_serial$serial.No = 1:nrow(site_code_serial)
# 
# # add 0 to force the serial.No being three digits
# site_code_serial$serial.No =
#   ifelse(site_code_serial$serial.No < 100,
#          sprintf("%03d", site_code_serial$serial.No),
#          as.character(site_code_serial$serial.No))
# write.csv(site_code_serial, "CSN_IMPROVE_site.serial.csv")

site_code_serial = fread("CSN_IMPROVE_site.serial.csv"); site_code_serial$V1 = NULL

## CSN
site_code_serial = subset(site_code_serial, Dataset == "EPACSN") 

## IMPROVE
site_code_serial = subset(site_code_serial, Dataset == "IMPAER") 

# exclude sites that only include in conc_pmf
all_sites = unique(conc_pmf$SiteCode)
site_list = subset(site_list,
                   SiteCode %in% all_sites)

###### create a dataframe to save the decision of weak, bad, or strong 
species.name = 
  setdiff(names(conc_pmf),
          c("SiteCode", "Date", "X", "V1", 
            "State", "Final.Decision"))
# Finaly.Decision = 1:25
length(all_sites)

cmd_species_class_site = 
  data.frame(
  matrix(ncol = length(species.name), 
         nrow = length(all_sites)))
names(cmd_species_class_site) = species.name

# add PM2.5 and other variables
cmd_species_class_site = 
  data.frame(SiteCode = NA, serial.No = NA, 
             cmd_species_class_site, 
             sum.weak.good = NA, style.weak.good = NA, site.row = NA)

head(cmd_species_class_site)

# extreme_K = NULL
# thresholds_Seasonal99th = NULL
thresholds_TimesMean = NULL
extreme_events_remove = extreme_events_keep = extreme_events_replace = NULL

#### estimate whether the conc is above monthly MDL  #### 

### CSN
# csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_MDL_monthly.csv")
# csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_MDL_monthly_2023.02.27.csv")
species_mdl = fread("CSN_MDL_C-Sub_monthly_2023.05.csv")
species_mdl$V1 = species_mdl$Cl. = NULL # csn_mdl$OP = 

### IMPROVE
species_mdl = fread("IMPROVE_MDL_monthly_2023.csv")
species_mdl$V1 = species_mdl$ClIon = NULL # imp_mdl$OP = 


species_only_mdl = species_mdl[, 4:ncol(species_mdl)]
species_only_mdl = species_col_reorder(species_only_mdl)
species_mdl_use = cbind(species_mdl[, 1:3], species_only_mdl)

##### excluding species not to be used
OC.EC.sub = c("EC1", "EC2", "EC3",
              "OC1", "OC2", "OC3", "OC4")
# species_exclude <- OC.EC.sub
species_exclude <- c(OC.EC.sub, c("Ag", "K", "Na", "S")) # not used for SA, or colinearity

conc_pmf[ , species_exclude] <- list(NULL)
# unc_pmf[ , species_exclude] <- list(NULL)

OOB_comb_avg[ , species_exclude] <- list(NULL)
miss_comb_avg[ , species_exclude] <- list(NULL)
# comp_error_fraction[ , species_exclude] <- list(NULL)
species_mdl_use[ , species_exclude] <- list(NULL)
cmd_species_class_site[, species_exclude] <- list(NULL)
# cluster_snr[ , OC.EC.sub] <- list(NULL) 
# species_conc_mdl_cluster[ , OC.EC.sub] <- list(NULL)

# extract species colnames, those after excluding "SiteCode", "State" & "Date"
species_columns = 
  setdiff(names(conc_pmf), 
          c("SiteCode", "Date", "State"))
# comp_error_fraction include more species than in conc_pmf, exclude them
comp_error_fraction <- comp_error_fraction[, ..species_columns]
summary(names(comp_error_fraction) == names(conc_pmf)[4:ncol(conc_pmf)])


species_daily_conc = conc_pmf
species_daily_conc$State = NULL

# expand MDL file to daily measurement 
# (in case of interpolation, not used original data directly)
species_daily_conc$year = year(species_daily_conc$Date)
species_daily_conc$month = month(species_daily_conc$Date)

species_daily_conc_date = select(species_daily_conc, 
                                 SiteCode, Date, year, month)
species_daily_fullMDL = 
  join(species_daily_conc_date, 
        species_mdl_use)
dim(species_daily_conc_date)

# reorder columns the dataset for matching
setcolorder(species_daily_fullMDL, 
            names(species_daily_conc))

# reorder rows the dataset for matching
species_daily_fullMDL = 
  species_daily_fullMDL[
    with(
      species_daily_fullMDL, 
      order(SiteCode, Date)), ]

species_daily_conc = 
  species_daily_conc[
    with(
      species_daily_conc, 
      order(SiteCode, Date)), ]

species_daily_fullMDL$year = species_daily_fullMDL$month = 
  species_daily_conc$year = species_daily_conc$month = NULL

# double check if date & site & columns match
summary(species_daily_fullMDL$SiteCode == species_daily_conc$SiteCode)
summary(species_daily_fullMDL$Date == species_daily_conc$Date)
summary(names(species_daily_fullMDL) == names(species_daily_conc))

# compare concentration and MDL of a given component
species_conc = species_daily_conc[, ..species_columns]
species_mdl_full = species_daily_fullMDL[, ..species_columns]

species_conc_above_mdl = data.frame(Map(">", species_conc, species_mdl_full))
setDT(species_conc_above_mdl)

species_conc[1:3, 1:10]
species_mdl_full[1:3, 1:10]
species_conc_above_mdl[1:3, 1:10]

species_conc_above_mdl_use =
  cbind(species_daily_fullMDL[, 1:2], species_conc_above_mdl)

# check variable class & dataset dimenssion
# sapply(species_conc, class)
# sapply(species_mdl_full, class)
# sapply(species_conc_above_mdl, class)
# dim(species_conc)
# dim(species_mdl_full)
# dim(species_conc_above_mdl)

#### pre-process the marked interpolated points #### 

# for those using refilling (NA or unacceptable Flag), add the uncertainty *1.5 

### CSN
#species_NA_intp = fread("CSN_TF_logical_InterpolatedOrNot.csv")
species_NA_intp = fread("CSN_TF_logical_InterpolatedOrNot_C-subgroup.csv")

### IMPROVE
species_NA_intp = fread("IMPROVE_TF_logical_InterpolatedOrNot_allSpecies.csv")

species_NA_intp = species_col_reorder(species_NA_intp)
species_NA_intp$V1 = NULL
species_NA_intp = species_NA_intp[, ..species_columns]

species_NA_intp = 
  cbind(species_daily_conc_date,
        species_NA_intp)

species_NA_intp = 
  species_NA_intp[
    with(
      species_NA_intp, 
      order(SiteCode, Date)), ]

# double check if rows & columns match
summary(species_NA_intp$SiteCode == species_daily_conc$SiteCode)
summary(species_NA_intp$Date == species_daily_conc$Date)
summary(names(species_NA_intp)[5:ncol(species_NA_intp)] == 
          names(species_daily_conc)[3:ncol(species_daily_conc)])

# cols_comp = col_comp(species_NA_intp, "Al", "PM25")
species_NA_intp$year = species_NA_intp$month = NULL

#### define the species_source_groups #### 

species_source_groups = 
  list(earth_elements = c("Al", "Fe", "Ca", "Si", "Ti", "AlIon", "FeIon", "CaIon", "Mg", "MgIon"),
       resuspend_dust = c("Ba", "Br", "Pb", "Ti", "Fe"),
       construction = c("Ca", "Mg", "CaIon", "MgIon"), # Peilin Chen_2023, or only Ca, Liu_2017_EP_PMF
       fugtive_dust = c("Ti", "K", "KIon"), # Kotchenruther_2016_AE
       sec_nitrate = c("NH4Ion", "NO3Ion"),
       sulfate = c("NH4Ion", "SO4Ion"),
       fresh_sea_salt = c("NaIon", "ClIon", "Na", "Cl", "Sr", "Br"), #Louie_2005_STE
       aged_sea_salt = c("SO4Ion", "NO3Ion", "Mg", "MgIon", "NaIon", "ClIon", "Na", "Cl", "NO3Ion"),
       galvanizing = c("Pb", "Zn"), # Dai_2023_EP
       ferros_metal = c("Fe", "Zn", "Mn"), # Yang_2023_Atmosphere
       non_ferros_metal = c("Cu", "Cr", "Ni", "Pb"), # Yang_2023_Atmosphere
       biomass = c("OC", "K", "KIon", "Br", "Cl", "ClIon", "S", "SO4Ion"), # Singh_2022_AAQR OC/EC ratio, Masiol_2017_AE
       heavy_oil = c("Ni", "V"), # Ni:V ratio, Kotchenruther_2013_AE_PM, SPECIATE, Hadley_2017_AE
       coal_burn = c("S", "SO4Ion", "EC", "OC", "As", "Se", "Pb", "Cl", "ClIon"), # Xie_2022_EP
       firework = c("K", "Pb", "Cu", "Sr", "As", "Ba", "Na", "KIon", "NaIon", "Mg", "MgIon", "OC", "EC", "NO3Ion", "SO4Ion"), # Phil, slides, natural relationships
       non_tailpipe = c("Fe", "Cu", "Zn", "Pb", "Mn", "Ba", "Sb", "Al", "Cr"), # Hasheminassab_2014_ACP, also , EC/OC gas/diesel; Nanjing_Zheng_2019; Park_STOTEN_2022_Beijing-Seoul
       vehicle = c("OC", "EC", "Fe", "Zn", "NO3Ion") # Dai_2023_EP, Nanjing_Zheng_2019
  )


#### choose the folder #### 
# ONLY apply this to SITEs, and combine site data if running the cluster analyses

###### define the subfolder to save cluster files
dropbox_path = "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/"

# #### CSN, extreme, mean*25 as threshold of outlier, remove all above threshold
# Clusterfolder <- "PMF_NoGUI_NoCsub_NoExtreme_cluster" ## if no OC EC subgroups, no extreme values 
# GUI.cluster.folder <- "PMF_GUI_NoCsub_NoExtreme_cluster"
# prefix = "CSN_noCsub_noExtreme_C_"
# 
# #### CSN, extreme, mean*25 as threshold of outlier, only remove those with high K on July.4th
# Clusterfolder <- "CSN_NoGUI_NoCsub_25TimesMean_cluster" ## if no OC EC subgroups, no extreme values 
# GUI.cluster.folder <- "CSN_GUI_NoCsub_25TimesMean_cluster"
# prefix = "CSN_noCsub_25timesMean_C_"

#### CSN, extreme, mean*15 as threshold of outlier, remove those with high K, and replace those with single species high by median
nonGUI.site.folder <- "CSN_NoGUI_NoCsub_15tMean_site" ## if no OC EC subgroups, no extreme values 
GUI.site.folder <- "CSN_GUI_NoCsub_15tMean_site"
prefix = "CSN_noCsub_15timesMean_S_"

# #### CSN, extreme, season 99th as threshold of outlier
# Clusterfolder <- "CSN_NoGUI_NoCsub_99season_cluster"
# GUI.cluster.folder <- "CSN_GUI_NoCsub_99season_cluster"
# prefix = "CSN_noCsub_noSeason99_C_"
# 
# #### IMPROVE, extreme, mean*25
# Clusterfolder <- "IMPROVE_noGUI_NoCsub_NoExtreme_cluster"
# GUI.cluster.folder <- "IMPROVE_GUI_NoCsub_NoExtreme_cluster"
# prefix = "IMPROVE_noCsub_noExtreme_C_"

# prefix_swb = sub("C_$", "", str_extract(prefix,  "^(.*?)C_"))

#### start the loop #### 

prefix_swb = sub("S_$", "", str_extract(prefix,  "^(.*?)S_"))

# extract concentration & uncertainty of single cluster for PMF
for( site.serial in unique(site_list$serial.No)){
  
  # generate SiteCode and corresponding datasets
  site_code = site_list$SiteCode[site_list$serial.No == site.serial]
  conc_site = subset(conc_pmf, SiteCode == site_code)

  species_NA_intp_site = 
    subset(species_NA_intp, SiteCode == site_code)
  conc_above_mdl_site = 
    subset(species_conc_above_mdl_use, SiteCode == site_code)
  species_fullMDL_site =
    subset(species_daily_fullMDL, SiteCode == site_code)
  
  # summary(species_NA_intp_site$Date == conc_site$Date)
  # summary(conc_above_mdl_site$Date == conc_site$Date)
  # summary(species_fullMDL_site$Date == conc_site$Date)

  dim(conc_site)
  dim(species_NA_intp_site)
  
  ##### determine the extremes 1.1 - N*mean #####
  conc_site_forExe = conc_site
  # summary(conc_site_forExe)
  cols_comp = col_comp(conc_site_forExe, "Al", "PM25")
  mean_conc_site = conc_site_forExe[, lapply(.SD, mean), .SDcols = cols_comp]
  
  # med_conc_site*25 < max_conc_site
  
  # # mean value and 25 times were selected due to the data observation.
  # # however, for some site, mean * 20 may alreay miss some very high values
  # # for earth elements (Si, Al), mean * 20 may filter too many values
  # UPdate 2024.02, 1-if extreme values are detected in multiple featured species of a given source, then keep the extremes
  # UPdate 2024.02, 2-with threshold 25, extremes in many other sources were kept, influence the overall performance
  # UPdate 2024.02, 3-after checking more source peaks, using the value of 15
  times_value = 15
  extreme_conc_site = mean_conc_site * times_value

  # identify rows in conc_site_forExe with values higher than the extreme values
  # rows_to_remove_25mean <- apply(conc_site_forExe[, 4:ncol(conc_site_forExe)], 1, 
  #                        function(row) 
  #                          any(row > extreme_conc_site))
  
  # expand the extreme rows to the same as conc_site
  extreme_conc_site_exp = 
    extreme_conc_site %>% 
    slice(rep(1:n(), 
              each = nrow(conc_site_forExe)))
  
  # 1/3 of extreme to detect other high concentrations
  second_extreme_conc_site_exp = extreme_conc_site_exp/3
  
  thresholds_TimesMean = rbind(thresholds_TimesMean, extreme_conc_site)
  
  # write.csv(thresholds_TimesMean, "CSN_thresholds_25TimesMean.csv")
  write.csv(thresholds_TimesMean, "CSN_site_thresholds_15TimesMean.csv")



  ###### determine the extremes 1.2 - seasonal 99th STOP Using #####
  # already decide using the N times mean as threshold
  # conc_site_forExe = conc_site
  # conc_site_forExe$Season = season_date(conc_site_forExe$Date)
  # conc_site_forExe$Year = year(conc_site_forExe$Date)
  # 
  # # generate the seasonal 99th for each site #yearly
  # # use pivot_longer and pivor_wider instead of gather/spread to rearrange the df
  # conc_site_long = 
  # conc_site_forExe %>%
  #   pivot_longer(
  #     cols = -c(State, SiteCode, Final.Decision, Date, Year, Season),
  #     names_to = "Chemical",
  #     values_to = "Concentration"
  #   )
  # 
  # conc_site_season99 =
  #   ddply(conc_site_long, 
  #       .(State, SiteCode, Final.Decision, Season, Chemical), # Year, 
  #       summarise, 
  #       p99th = quantile(Concentration, 
  #                        probs = 0.99, 
  #                        na.rm = T)) %>%
  #   pivot_wider(
  #     names_from = Chemical,
  #     values_from = p99th)
  # 
  # thresholds_Seasonal99th = rbind(thresholds_Seasonal99th, conc_site_season99)
  # write.csv(thresholds_Seasonal99th, "CSN_thresholds_Seasonal99th.csv")
  # 
  # ######## match the season99 threshold info with daily time sereis
  # conc_site_forExe_identifyInfo = select(conc_site_forExe,
  #                                           State, SiteCode, Final.Decision, 
  #                                           Year, Season, Date)
  # conc_site_season99_daily = merge(conc_site_forExe_identifyInfo,
  #                                     conc_site_season99,
  #                                     all.x = T)
  # 
  # conc_site_extreme_daily = conc_site_season99_daily
  
  
  ##### determine the extremes 2 - decisions on how to deal with extremes #####

  # identify rows in conc_site with values higher than the extreme values
  conc_site_forExe_species = conc_site_forExe[, ..species_columns]
  
  # summary(names(extreme_conc_site_exp) == names(conc_site_forExe_species))
  # summary(names(second_extreme_conc_site_exp) == names(conc_site_forExe_species))

  conc_site_extreme_comp = 
    data.frame(conc_site_forExe_species > extreme_conc_site_exp)
  second_conc_site_extreme_comp = 
    data.frame(conc_site_forExe_species > second_extreme_conc_site_exp)
  
  # # mostly focus on days with firework on holidays with high K
  # conc_site_extreme_comp_K = subset(conc_site_extreme_comp, K == T)
  # conc_site_extreme_comp_K$row.No = row.names(conc_site_extreme_comp_K)
  # 
  # # extract rows with extremes
  # conc_site_extreme_comp_K$Month = month(conc_site_extreme_comp_K$Date)
  # conc_site_extreme_comp_K$Day = 
  #   as.integer(format(conc_site_extreme_comp_K$Date, 
  #                     format = "%d"))
  # conc_site_extreme_comp_K_holiday = 
  #   subset(conc_site_extreme_comp_K,
  #          (Month == 7 & Day %in% 1:7)) # | (Month == 12 & Day %in% 23:31) | (Month == 1 & Day %in% 1:7)
  # removed.row.count = nrow(conc_site_extreme_comp_K_holiday)
  # 
  # rows_to_remove = 1:nrow(conc_site_forExe) %in% unique(conc_site_extreme_comp_K_holiday$row.No)
  # sum(rows_to_remove)
  # 
  # removed_rows = conc_site[rows_to_remove, ]
  # removed_rows$row.No = conc_site_extreme_comp_K_holiday$row.No
  # removed_rows$total.remove = removed.row.count
  # 
  # extreme_K = rbind(extreme_K, removed_rows)
  # 
  # # write.csv(extreme_K, "CSN_Extreme_K_seasonal99th.csv")
  # # write.csv(extreme_K, "CSN_Extreme_K_25TimesMean.csv")
  # write.csv(extreme_K, "CSN_Extreme_K_20TimesMean.csv")

    
  # extract days with extremes and high values (extreme/3)
  day_extreme = 
    conc_site_extreme_comp[rowSums(conc_site_extreme_comp) >= 1, ]
  day_high = 
    second_conc_site_extreme_comp[rowSums(second_conc_site_extreme_comp) >= 1, ]
  
  day_extreme$row_serial = row.names(day_extreme)
  day_high$row_serial = row.names(day_high)
  day_high = subset(day_high, row_serial %in% day_extreme$row_serial)
  
  # dim(day_extreme); dim(day_high)
  
  ##### detect days with extremes and whether the extreme is only for one species or there are covarying species
  ## do not consider the K-extreme days for now
  
  extreme_site <- data.table(
    day_extreme_rows = integer(0),
    SiteCode = character(0),
    serial.No = character(0),
    Date = as.Date(character(0)),
    extreme_species = character(0),
    co_detected_species_high = character(0),
    potential_source = character(0)
  )
  
  for (exe.row in 1:nrow(day_extreme)) {
    day_extreme_rows = as.integer(day_extreme$row_serial[exe.row])
    
    # species with extreme values in day_extreme & covarying species in day_high, if any
    species_extreme_true <- 
      names(day_extreme[exe.row, ])[day_extreme[exe.row, ] == TRUE]
    species_high_true <- names(day_high[exe.row,])[day_high[exe.row,] == TRUE]
    
    # check for matching source groups among species_extreme_true and species_high_true
    matched_species <- list()
    
    for (source_group in names(species_source_groups)) {
      
      # identify if any species in species_extreme_true & species_high_true be in this source_group
      species_in_group_extreme <- 
        species_extreme_true[
          species_extreme_true %in% 
            species_source_groups[[source_group]]]
      
      species_in_group_high <- 
        species_high_true[
          species_high_true %in% 
            species_source_groups[[source_group]]]
      
      if (length(species_in_group_extreme) > 0) {
        matched_species[[source_group]] <- 
          list(species_extreme = species_in_group_extreme, 
               species_high = species_in_group_high)
      }
      
      # save the information for each source group where a match was found
      
      for (matched_source_group in names(matched_species)) {
        species_high = matched_species[[matched_source_group]]$species_high
        species_extreme = matched_species[[matched_source_group]]$species_extreme
        species_high_not_extreme = species_high[!(species_high %in% species_extreme)]
        
        # combine all species, if more than one, into a single string
        species_extreme_combined <- 
          paste(species_extreme, collapse = ", ")
        species_high_not_extreme_combined <- 
          paste(species_high_not_extreme, collapse = ", ")
        
        extreme_event_daily <-
          data.frame(
            # cluster.No = cluster.No,
            SiteCode = site_code,
            serial.No = site.serial,
            day_extreme_rows = day_extreme_rows, 
            Date = conc_site_forExe$Date[day_extreme_rows],
            extreme_species = species_extreme_combined, 
            co_detected_species_high = species_high_not_extreme_combined,
            potential_source = matched_source_group)
        
            extreme_site <- rbind(extreme_site, extreme_event_daily)
      }
    }
  }
  
  # remove duplicates if any
  extreme_site_use <- unique(extreme_site)
  
  # extreme_site_k =
  #   subset(
  #     extreme_site_use,
  #     grepl("K", species_extreme, fixed = T))
  
  # detect days potentially related BB & firework
  extreme_site_use$real_bb_firework = FALSE
  
  extreme_site_use$real_bb_firework[
    grepl("K", extreme_site_use$extreme_species)] = TRUE
  
  # remove the case when "K" is not in species_extreme and no covarying species, they are supposed to be replaced 
  extreme_site_use$extreme_species_count = 
    lengths(strsplit(extreme_site_use$extreme_species, ",\\s*"))
  extreme_site_use$covarying_species_count = 
    lengths(strsplit(extreme_site_use$co_detected_species_high, ",\\s*"))
  
  
  extreme_day_remove =
    subset(extreme_site_use, real_bb_firework)
  extreme_day_remove = 
    unique(
      select(
        extreme_day_remove,
        SiteCode, serial.No, 
        day_extreme_rows, Date, extreme_species, co_detected_species_high))
  
  extreme_day_replace = 
    subset(extreme_site_use, 
           extreme_species_count == 1 &
             covarying_species_count == 0 &
             (! Date %in% extreme_day_remove$Date))
  extreme_day_replace = 
    unique(
      select(
        extreme_day_replace,
        SiteCode, serial.No, 
        day_extreme_rows, Date, extreme_species, co_detected_species_high))
  
  extreme_day_keep =
    subset(extreme_site_use, 
           !(Date %in% extreme_day_replace$Date |
               Date %in% extreme_day_remove$Date))
  extreme_day_keep = 
    unique(
      select(
        extreme_day_keep,
        SiteCode, serial.No, 
        day_extreme_rows, Date, extreme_species, co_detected_species_high))
  
  # extreme days to remove (firework or biomass)  
  rows_to_remove = 
    unique(extreme_day_remove$day_extreme_rows)
  # extreme days to keep (days with other source detected) 
  rows_to_keep = 
    unique(extreme_day_keep$day_extreme_rows)
  # extreme days to replace (no source detected)
  rows_to_replace = 
    unique(extreme_day_replace$day_extreme_rows)
  
  rowNo_to_replace = length(rows_to_replace)
  rowNo_to_remove = length(rows_to_remove)
  rowNo_to_keep = length(rows_to_keep)
  rowNo_after_remove = nrow(conc_site) - rowNo_to_remove
  
  # check if all rows with extreme(s) are classified
  # rowNo_to_replace + rowNo_to_remove + rowNo_to_keep == nrow(day_extreme)
  # length(unique(append(append(rows_to_remove, rows_to_keep), rows_to_replace))) == nrow(day_extreme)

  # combine for later check if the handling with extremes looks fine or not
  extreme_events_remove = rbind(extreme_events_remove, extreme_day_remove)
  extreme_events_keep = rbind(extreme_events_keep, extreme_day_keep)
  extreme_events_replace = rbind(extreme_events_replace, extreme_day_replace)
  
  ##### determine the extremes 3 - use random-forest to interpolate extremes to be replaced #####

  day_extreme_replace = 
    subset(day_extreme,
           row_serial %in% rows_to_replace)
  
  # for each species column, update conc_site and species_NA_intp_site where day_extreme_replace is TRUE
  for (species_col in species_columns) {
    rows_to_update <- 
      as.numeric(
        day_extreme_replace$row_serial[
          day_extreme_replace[[species_col]] == TRUE])
    
    conc_site[rows_to_update, (species_col) := NA]
    # extra points that are from interpolation
    species_NA_intp_site[rows_to_update, (species_col) := TRUE]
  }
  
  ## log all value to avoid negative interpolation 
  conc_site_noExe = conc_site[!as.integer(rows_to_remove), ]
  
  site_log = 
    cbind(conc_site_noExe[, 1:3],
          conc_site_noExe[, 4:ncol(conc_site_noExe)] %>% 
            dplyr::select(where(is.numeric)) %>%
            log())
  
  #interpolation with missForest, using random forest, option "variablewise = T"
  site_intp_rf_mf = 
    missForest(site_log[, ..species_columns], 
               variablewise = T)
  site_rf_conc = 
    cbind(site_log[, 1:3], 
          exp(site_rf_conc_mf$ximp))
  
  ##### determine the extremes 4 - remove rows that needs to be removed #####
  species_NA_site = species_NA_intp_site[!as.integer(rows_to_remove), ]
  species_fullMDL_site = species_fullMDL_site[!as.integer(rows_to_remove), ]
  
  summary(site_rf_conc$Date == species_NA_site$Date &
            species_fullMDL_site$SiteCode == species_NA_site$SiteCode)
  
  ###### Uncertainty estimation ######
  
  conc_above_mdl_site <- conc_above_mdl_site[!as.integer(rows_to_remove), ]

  summary(conc_above_mdl_site$Date == site_rf_conc$Date)

  # expand error_fraction file to one with same row number as concentration file
  comp_ef <- 
    comp_error_fraction[1][
      rep(1, nrow(conc_above_mdl_site)), 
      .SD] # .SD selects the data for each repetition.
  
  # estimate the uncertainty 
  # EPA PMF5.0 user guide, page 16-17, function 5-1 & 5-2
  conc_above_mdl_species = conc_above_mdl_site[, ..species_columns]
  conc_rf_species = site_rf_conc[, ..species_columns]
  species_MDL_site = species_fullMDL_site[, ..species_columns]
  
  conc_rf_pmf = 
    conc_above_mdl_species * conc_rf_species +
    (!conc_above_mdl_species) * species_MDL_site * 0.5
  # unc_pmf_1 = conc_above_mdl_species * (species_MDL_site / 3 + comp_ef * conc_rf_species) +
  #   (!conc_above_mdl_species) * 5/6 * species_MDL_site
  unc_rf_pmf = 
    conc_above_mdl_species * (((species_MDL_site / 2)^2 + 
                                   (comp_ef * conc_rf_species)^2)^0.5) +
    (!conc_above_mdl_species) * 5/6 * species_MDL_site
  
  # conc_above_mdl_species[1:5, 4:10]; conc_rf_species[1:5, 4:10]; species_MDL_site[1:5, 4:10]; comp_ef[1:5, 4:10]
  # conc_rf_pmf[1:5, 4:10]; unc_rf_pmf[1:5, 4:10]
  
  conc_rf_pmf$PM25 = site_rf_conc$PM25
  unc_rf_pmf$PM25 = 3 * site_rf_conc$PM25

  # extra uncertainty for the interpolated points
  species_NA_intp_unc = species_NA_site[, 3:ncol(species_NA_site)]
  species_NA_intp_unc[species_NA_intp_unc == TRUE] <- 1.5
  species_NA_intp_unc[species_NA_intp_unc == FALSE] <- 1

  summary(names(unc_rf_pmf) == names(species_NA_intp_unc))
  
  unc_rf_pmf = unc_rf_pmf * species_NA_intp_unc
  
  # insert date info
  conc_rf_pmf <- cbind(site_rf_conc[, 1:3], conc_rf_pmf) # site_rf_conc[, 1:4]
  unc_rf_pmf <- cbind(site_rf_conc[, 1:3], unc_rf_pmf) # site_rf_conc[, 1:4]

  ###### Re-estimate signal-to-noise SNR after removing extremes ######
  conc.col = ncol(conc_rf_pmf)

  # according to EPA PMF 5.0 User Guide.pdf, function 5-3 & 5-4 from page 23
  conc_unc_diff_site = 
    (conc_rf_pmf[, ..species_columns] - 
       unc_rf_pmf[, ..species_columns])/
    unc_rf_pmf[, ..species_columns] * conc_above_mdl_species
  
  conc_unc_diff_site$PM25 = -999
  # conc_unc_diff_site[1:5, 4:10]
  
  # get the SNR signal-to-noise ratio 
  snr_selected_site = 
    colSums(conc_unc_diff_site)/
    nrow(conc_unc_diff_site)
  # convert the result to a one-row data.frame for later match
  snr_selected_site = data.frame(t(snr_selected_site))  

  ###### Classify-bad&weak 1. Strict SNR & Strict MDL - selected ######

  # estimate the percent of species-specific fration of above MDL
  conc_rf_pmf_aboveMDL = data.frame(colSums(conc_above_mdl_species))
  conc_rf_pmf_aboveMDL$CompName = row.names(conc_rf_pmf_aboveMDL)
  names(conc_rf_pmf_aboveMDL)[1] = "above_MDL_count"
  conc_rf_pmf_aboveMDL$Percent = 
    conc_rf_pmf_aboveMDL$above_MDL_count * 100 / rowNo_after_remove
  
  # generate the weak & bad species based on the conc_vs_mdl 
  site.species.weak.Pmdl = conc_rf_pmf_aboveMDL$CompName[
    conc_rf_pmf_aboveMDL$Percent <= 50 &
      conc_rf_pmf_aboveMDL$Percent > 20]
  site.species.bad.Pmdl = conc_rf_pmf_aboveMDL$CompName[
    conc_rf_pmf_aboveMDL$Percent <= 20]
  # change to character
  site.species.weak.Pmdl = as.character(site.species.weak.Pmdl)
  site.species.bad.Pmdl = as.character(site.species.bad.Pmdl)
  
  # generate the weak & bad species based on SNR 
  site.species.bad.snr = colnames(snr_selected_site)[which(
    colSums(snr_selected_site) < 0.2)]
  site.species.weak.snr = colnames(snr_selected_site)[which(
    colSums(snr_selected_site) >= 0.2 &
      colSums(snr_selected_site) < 2)]
  
  ###### Classify-bad&weak 2. Combine the rate need interpolation & OBB error ######
  # Extract data for cluster
  site_oob = subset(OOB_comb_avg, 
                       SiteCode == site_code)
  site_miss = subset(miss_comb_avg, 
                        SiteCode == site_code)
  
  # get cluster average
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
  
  # for site need no interpolation for missing values, site.oob.miss.weak(bad) could be NA
  # ifelse{} only show the same type/length, so cannot be used here
  site.oob.miss.weak = 
    if(all(is.na(site.oob.miss.weak))) {
      character(0)
    } else {
      site.oob.miss.weak
    }
  site.oob.miss.bad = 
    if(all(is.na(site.oob.miss.bad))) {
      character(0)
    } else {
      site.oob.miss.bad
    }
  
  ###### Classify-bad&weak 3. other criteria ######
  
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
  # site.species.weak = site.species.weak[! site.species.weak %in% "PM25"]
  
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
  
  ##### For CSN
  # add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
  site.species.bad = append(c("S", "Na", "K"), 
                               site.species.bad) 
  # in case any of this three is in weak, if so, remove
  site.species.weak = site.species.weak[! (
    site.species.weak %in% c("S", "Na", "K"))]
  
  ##### For IMPROVE
  # add "S into bad to exclude co-linear effects with SO4, Na+, K+
  site.species.bad = append(c("S"), 
                               site.species.bad) 
  # in case "S  is in weak, if so, remove
  site.species.weak = site.species.weak[! (
    site.species.weak %in% c("S"))]
  
  
  # add Al, Mg, Na into weak, if there are not in bad, due to the lower reliability in XRF test
  if("NaIon" %in% species_columns){
    Al.Mg.weak = c("Al", "Mg")
  } else {
    Al.Mg.weak = c("Al", "Mg", "Na")
  }
  # determine if Al or Mg or Na is in bad, if so, remove the one in "Al.Mg.weak"
  Al.Mg.weak = Al.Mg.weak[! (
    Al.Mg.weak %in% site.species.bad)]
  # add the rest in Al.Mg.weak to "weak"
  site.species.weak = append(Al.Mg.weak, 
                                site.species.weak) 
  # In CSN, "Na", "K" were removed, and use Na+, K+ instead
  
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
  
  ###### Classify-bad&weak 4. Distribution of values above MDL ######
  # species stay in "bad" and related to higher Pmdl
  site.species.bad.check = site.species.bad[
    site.species.bad %in% 
      site.species.bad.Pmdl]
  
  # remove those with >95% of value below MDL
  site.species.bad.check = 
    unique(
      subset(conc_rf_pmf_aboveMDL, 
             CompName %in% site.species.bad.check &
               Percent > 5)$CompName)
  
  # site.species.bad.check = c("Ag", "Ca")
  if(length(site.species.bad.check) > 0){
    # files for those with above MDL concentrations
    conc_rf_pmf_mdl = 
      conc_rf_pmf[, 4:ncol(conc_rf_pmf)] * conc_above_mdl_species
    
    # subset those grouped as bad
    conc_rf_pmf_bad = conc_rf_pmf_mdl %>%
      select(all_of(site.species.bad.check))
    
    # replace 0 by NA
    conc_rf_pmf_bad <-
      data.frame(
        apply(
          conc_rf_pmf_bad, 
          2, 
          function(x) 
            replace(x, x == 0, NA)))
    
    # for concentrations above MDL, check the 10th vs. 90th percentile ratio
    # for some random comparison, r < 0.1 for strong species and < 0.2 for weak 
    # or, change to compare mean vs. sd? if mean < sd, or if mean > sd, already
    conc_rf_pmf_bad_mean = 
      conc_rf_pmf_bad %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          list(
            p = ~mean(., na.rm = T)
          )
        ))
    
    conc_rf_pmf_bad_sd = 
      conc_rf_pmf_bad %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          list(
            p = ~sd(., na.rm = T)
          )
        ))
    
    bad_sd_mean = conc_rf_pmf_bad_sd/conc_rf_pmf_bad_mean
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
  
  site.species.weak = 
    append(site.species.weak, 
           site.species.bad.remove)
  site.species.weak = 
    site.species.weak[
      !is.na(site.species.weak)]
  
  # arrange in alphabetic order 
  site.species.bad = sort(site.species.bad)
  site.species.weak = sort(site.species.weak)
  site.species.weak = append(site.species.weak, "PM25")
  site.species.strong = unique(
    subset(conc_rf_pmf_aboveMDL, 
           !(CompName %in% site.species.bad |
               CompName %in% site.species.weak))$CompName)
  site.species.strong = site.species.strong[! site.species.strong %in% "PM25"]
  site.species.strong = as.character(site.species.strong)
  
  ######### output 1:  GUI #########
  # remove species marked as bad
  conc_rf_pmf_gui <- conc_rf_pmf[, !site.species.bad, with=FALSE]
  unc_rf_pmf_gui <- unc_rf_pmf[, !site.species.bad, with=FALSE]
  conc_rf_pmf_gui$State = unc_rf_pmf_gui$State = 
    conc_rf_pmf_gui$SiteCode = unc_rf_pmf_gui$SiteCode = NULL
  
  write.csv(conc_rf_pmf_gui,
            file = file.path(
              dropbox_path, GUI.site.folder, 
              paste0(prefix, 
                     site.serial, "_conc.csv")),
            row.names = FALSE)
  write.csv(conc_rf_pmf_gui,
            file = file.path(
              dropbox_path, GUI.site.folder, 
              paste0(prefix, 
                     site.serial, "_unc.csv")),
            row.names = FALSE)

  # siteRow_in_site = data.frame(table(conc_rf_pmf_gui$SiteCode))
  # colnames(siteRow_in_site) = c("SiteCode", "Freq")
  
  # write.csv(siteRow_in_site,
  #           paste0(GUI.site.folder, "/", prefix, 
  #                  site.serial, "_date_number_of_site.csv"))
  
  # for those marked as weak, set the uncertainty 3 times as much
  # unc_rf_pmf_gui_1 = unc_rf_pmf_gui
  
  
  ######### output 2.1: non-GUI, conc & unc #########
  
  unc_rf_pmf_cmd = unc_rf_pmf_gui
  conc_rf_pmf_cmd = conc_rf_pmf_gui
  
  # unc_rf_pmf_cmd[names(unc_rf_pmf_cmd) %in% site.species.weak] = 
  #   3 * unc_rf_pmf_cmd[names(unc_rf_pmf_cmd) %in% site.species.weak]
  unc_rf_pmf_cmd[, (site.species.weak) := 
                    lapply(.SD, 
                           function(x) 
                             x * 3), 
                  .SDcols = site.species.weak]
  
  # for PM2.5, set the uncertainty 3*3 times as much
  unc_rf_pmf_cmd$PM25 = 3 * conc_rf_pmf_cmd$PM25
  
  # unc_rf_pmf_cmd[1:5, 1:10]; conc_rf_pmf_cmd[1:5, 1:10]

  # add info into column names before combination
  names(conc_rf_pmf_cmd) = paste0("conc_", names(conc_rf_pmf_cmd))
  names(unc_rf_pmf_cmd) = paste0("unc_", names(unc_rf_pmf_cmd))
  
  # summary(conc_rf_pmf_cmd$conc_Date == unc_rf_pmf_cmd$unc_Date)

  # combining conc & unc files
  cmd_conc_unc = cbind(conc_rf_pmf_cmd, unc_rf_pmf_cmd)
  
  # Interleave columns of concentration and uncertainty files
  ## generate a new vector to index the columns of cbind file
  interleave.col.order <- 
    rep(1:ncol(unc_rf_pmf_cmd), each = 2) + 
    (0:1) * ncol(unc_rf_pmf_cmd)
  
  ## reorder the cbind file
  # cmd_input_interleave = cmd_conc_unc[interleave.col.order] # for data.frame only
  cmd_input_interleave = 
    as.data.table(
      as.matrix(
        cmd_conc_unc)[, 
                      interleave.col.order])
  
  # only keep one Date & cluster column
  cmd_input_interleave$unc_Date = NULL
  names(cmd_input_interleave)[1] = "Date"
  # head(cmd_input_interleave)
  
  cmd_input_interleave = 
    cmd_input_interleave[
      with(cmd_input_interleave, 
           order(Date)), ]
  
  write.csv(cmd_input_interleave, 
            file = file.path(
              dropbox_path, nonGUI.site.folder, 
              paste0(prefix, site.serial, "_PMF_CMD.csv")),
            row.names = FALSE)
  
  ###### output 2.2: non-GUI, weak, bad or strong ######
  # get the corresponding row number
  cmd_class_rowNo = 
    as.integer(
      row.names(
      cmd_species_class_site[
        cmd_species_class_site$serial.No == site.serial, ]))
  
  # assign values
  cmd_species_class_site[cmd_class_rowNo, site.species.strong] <- 1
  cmd_species_class_site[cmd_class_rowNo, site.species.weak] <- 0
  cmd_species_class_site[cmd_class_rowNo, site.species.bad] <- NA
  
  cmd_species_class_site$sum.weak.good[cmd_class_rowNo] = 
    length(site.species.weak) +
    length(site.species.strong) 
  
  cmd_species_class_site$site.row[cmd_class_rowNo] = nrow(conc_rf_pmf)
  # detect the species for None-GUI PMF, thus, weak and strong species
  nonGUI_disp_species <- 
    cmd_species_class_site[
      cmd_class_rowNo, 
      c(site.species.strong, site.species.weak)]
  species.name.use = species.name[
    species.name %in% 
      c(site.species.strong, site.species.weak)]
  
  # add PM25
  # species.name.use = append(species.name.use, "PM25")
  
  nonGUI_disp_species <- nonGUI_disp_species[, species.name.use]
  cmd_species_class_site$style.weak.good[cmd_class_rowNo] <- 
    paste0("/", nonGUI_disp_species, collapse = "")
  # cmd_species_class_site[cmd_class_rowNo, ]
  
  write.csv(cmd_species_class_site, 
            file = file.path(
              dropbox_path, nonGUI.site.folder, 
              paste0(prefix_swb, "PMF_SWD_site.csv")),
            row.names = FALSE)
}


#### random selection of sites for sensitivity analyses ####

dataset_cluster = read.csv("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE/CSN_RF_cluster5training.csv")

random_sites <- 
  dataset_cluster %>%
  group_by(Final.Decision) %>%
  sample_n(1) %>%
  select(SiteCode, Final.Decision)

sites_diff_pattern = 
  data.frame(SiteCode = c(391510017, 120110034, 550790010, 540390020, 490050007), 
             Final.Decision = c(1, 7, 9, 12, 24))

site_sensitivity = 
  rbind(sites_diff_pattern, random_sites)

site_sensitivity = 
  site_sensitivity[with(
    site_sensitivity, 
    order(Final.Decision, SiteCode)), ]

write.csv(site_sensitivity, "CSN_Site_selected_sensitivity_analysis.csv")



################################################################################
#### Extract Non-GUI PMF data for selected Sites ####
################################################################################

#### NO LONGER USED. we carried out the analyses for all sites.

###### read selected site info  ######
# 
# site_sensitivity = read.csv("CSN_Site_selected_sensitivity_analysis.csv")
# site_sensitivity$X = NULL
# 
# ###### choose the folder  ######
# 
# ###### subfolders saving cluster files
# #### CSN, extreme, mean*25 as threshold of outlier
# Clusterfolder <- "CSN_NoGUI_NoCsub_NoExtreme_cluster" ## if no OC EC subgroups, no extreme values
# # dir.create(paste0(data.dir, "/", "PMF_NoGUI_NoCsub_NoExtreme_Site"))
# Sitefolder = "CSN_NoGUI_NoCsub_NoExtreme_Site"
# #GUI.cluster.folder <- "PMF_GUI_NoCsub_NoExtreme_cluster"
# prefix = "CSN_noCsub_noExtreme_C_"
# 
# #### CSN, extreme, mean*25 as threshold of outlier
# Clusterfolder <- "CSN_NoGUI_NoCsub_25TimesMean_cluster" ## if no OC EC subgroups, no extreme values
# Sitefolder = "CSN_NoGUI_NoCsub_25TimesMean_Site"
# #GUI.cluster.folder <- "CSN_GUI_NoCsub_25TimesMean_cluster"
# prefix = "CSN_noCsub_25timesMean_C_"
# 
# #### CSN, extreme, season 99th as threshold of outlier
# Clusterfolder <- "CSN_NoGUI_NoCsub_99season_cluster"
# Sitefolder = "CSN_NoGUI_NoCsub_99season_Site"
# #GUI.cluster.folder <- "CSN_GUI_NoCsub_99season_cluster"
# prefix = "CSN_noCsub_noSeason99_C_"
# 
# #### IMPROVE, extreme, mean*25
# Clusterfolder <- "IMPROVE_noGUI_NoCsub_NoExtreme_cluster"
# Sitefolder = "IMPROVE_noGUI_NoCsub_NoExtreme_Site"
# #GUI.cluster.folder <- "IMPROVE_GUI_NoCsub_NoExtreme_cluster"
# prefix = "IMPROVE_noCsub_noExtreme_C_"
# 
# ###### prepare data for selected sites  ######
# 
# ## list file names matching the pattern
# file_names <- list.files(Clusterfolder, 
#                          pattern = paste0(prefix, "[0-9]+_PMF_CMD\\.csv"), 
#                          full.names = TRUE)
# 
# prefix_swb = sub("C_$", "", str_extract(prefix,  "^(.*?)C_"))
# swb_cluster = 
#   read.csv(
#     paste0(
#       Clusterfolder, "/",
#       prefix_swb, 
#       "PMF_CMD_StrongWeakBad_Cluster.csv"))
# swb_cluster$X = NULL
# 
# swb_site_summary = NULL
# # iterate over each file to extract dataset for each site
# for (file_name in file_names) {
#   # extract the Cluster.No from the file name
#   Cluster.No <- as.integer(str_extract(file_name, "(?<=_C_)[0-9]+(?=_PMF_CMD)"))
#   
#   species_cluster <- read.csv(file_name)
#   species_cluster$X = NULL
#   
#   # ubset the selected site(s) for each cluster 
#   species_site_all <- 
#     species_cluster[
#     species_cluster$SiteCode %in% 
#       site_sensitivity$SiteCode[site_sensitivity$Final.Decision == Cluster.No], ]
# 
#   # site strong/weak/bad species
#   swb_site <- swb_cluster[swb_cluster$Finaly.Decision == Cluster.No, ]
# 
#   # Check if there are multiple SiteCode, save each 
#   unique_sites <- unique(species_site_all$SiteCode)
#   
#   for (select_site in unique_sites) {
# 
#     # Generate site info
#     site_file_name <- paste0(prefix, select_site, ".csv")
#     species_single_site <- subset(species_site_all, SiteCode == select_site)
#     site_row = nrow(species_single_site)
#     
#     # Site SWB (strong weak bad)
#     swb_site$SiteCode = select_site
#     swb_site$site.row = site_row
#     swb_site_summary = rbind(swb_site_summary, swb_site)
# 
#     # save site non-GUI inputs
#     write.csv(species_single_site, 
#               file.path(Sitefolder, site_file_name))
#   }
#   
#   # save site SWB info
#   write.csv(swb_site_summary, 
#             file.path(Sitefolder, 
#                       paste0(prefix_swb, 
#                       "PMF_CMD_StrongWeakBad_Site.csv")))
#   
# }


################################################################################
#### Copy files to Dropbox ####
################################################################################

dropbox_path = "/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/National_SA_PMF"
cluster_folder = paste0(dropbox_path, "/", Clusterfolder)
site_folder = paste0(dropbox_path, "/", Sitefolder)

###### for cluster
# create the destination folder if it does not exist
if (!dir.exists(cluster_folder)) {
  dir.create(cluster_folder, recursive = TRUE)
}

cluster_files <- list.files(paste0(data.dir, "/", Clusterfolder), full.names = TRUE)
# copy each file to the destination folder
for (cluster_file in cluster_files) {
  file.copy(cluster_file, cluster_folder)
}

###### for site
# create the destination folder if it does not exist
if (!dir.exists(site_folder)) {
  dir.create(site_folder, recursive = TRUE)
}

site_files <- list.files(paste0(data.dir, "/", Sitefolder), full.names = TRUE)
# copy each file to the destination folder
for (site_file in site_files) {
  file.copy(site_file, site_folder)
}


