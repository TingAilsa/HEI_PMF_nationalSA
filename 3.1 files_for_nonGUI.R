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
library(ggthemes)
library(ggplot2) 
library(base)
library(ggrepel)
library(missForest)


##### Combine CSN till & after 2015 - concentrations #####

# import CSN
# csn_daily_before = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/CSN_Interpolation/CSN_interpulation_random-forest_afterLog_before_2015.csv")
# csn_daily_after = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/CSN_Interpolation/CSN_interpulation_random-forest_afterLog_after_2015.csv")

# csn_daily_before = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/R - original IMPROVE/CSN_interpulation_random-forest_until_2015_2023.03.csv")
# csn_daily_after = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/R - original IMPROVE/CSN_interpulation_random-forest_from_2016_2023.03.csv")

csn_daily_before = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE/CSN_interpulation_random-forest_until_2015_2024.04.csv")
csn_daily_after = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE/CSN_interpulation_random-forest_from_2016_2024.04.csv")

csn_daily_before$V1 = csn_daily_after$V1 = NULL
csn_daily_before$Date = as.Date(csn_daily_before$Date)
csn_daily_after$Date = as.Date(csn_daily_after$Date)

# "OC" already existed in rownames after 2015, so delete it first
csn_daily_after$OC = NULL

# changed selected to OC, EC & OP for PMF
csn_daily_before = plyr::rename(csn_daily_before, 
                                c("EC1.unadjusted.88" = "EC1", 
                                  "EC2.unadjusted.88" = "EC2",
                                  "EC3.unadjusted.88" = "EC3",
                                  "OC1.unadjusted.88" = "OC1",
                                  "OC2.unadjusted.88" = "OC2", 
                                  "OC3.unadjusted.88" = "OC3",
                                  "OC4.unadjusted.88" = "OC4",
                                  # "Accept.PM2.5" = "PM25",
                                  "EC.TOR.unadjust.88" = "EC", 
                                  "OC.TOR.unadjusted.88" = "OC",
                                  "OP.TOR.unadjusted.88" = "OP"))

csn_daily_after = plyr::rename(csn_daily_after, 
                               c("OC1.88" = "OC1",
                                 "OC2.88" = "OC2", 
                                 "OC3.88" = "OC3",
                                 "OC4.88" = "OC4",
                                 # "PM2.5RC" = "PM25",
                                 "EC.TOR.88" = "EC", 
                                 "OC.88" = "OC",
                                 "OPC.TOR.88" = "OP"))

# get shared colnames
conc_common_cols <- intersect(names(csn_daily_before), 
                              names(csn_daily_after))

# Subset data frames using shared colnames
# csn_daily_before <- csn_daily_before[, conc_common_cols]
# csn_daily_after <- csn_daily_after[, conc_common_cols]
csn_daily_before <- csn_daily_before[, ..conc_common_cols]
csn_daily_after <- csn_daily_after[, ..conc_common_cols]

# combine the dataset
csn_daily_OrigOrder = rbind(csn_daily_before, csn_daily_after)
# setDT(csn_daily_OrigOrder)

# Remove variables not needed for PMF (C-subgroups)
csn_remove = c("OC.unadjusted.88", "EC.unadjusted.88", 
               "OPC.unadjusted.88") # , "OP"
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

csn_daily = species_col_reorder(species_daily)
csn_daily = relocate(csn_daily, SiteCode, .before = Date)
csn_daily = relocate(csn_daily, OP, .after = OC4)

names(csn_daily)

# write.csv(csn_daily, "CSN_RFinterpulated_combine_Csubgroup_2023.04.csv")
write.csv(csn_daily, "CSN_RFinterpulated_combine_Csubgroup_2024.04.csv")
length(unique(csn_daily$SiteCode))

##### Combine CSN till & after 2015 - the marked NA & replaced qualifiers - interpolated points #####

# csn_NA_bef = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE/CSN_Component_with_missing_Before_2015_2023.02.csv")
# csn_NA_aft = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE/CSN_Component_with_missing_After_2015_2023.02.csv")
csn_NA_bef = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE/CSN_NA_after_matching_AQS_PM_Before_2015_2024.04.csv")
csn_NA_aft = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE/CSN_NA_after_matching_AQS_PM_Since_2016_2024.04.csv")
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
csn_noAllNA = rbind(csn_RowCol_bef, csn_RowCol_aft)

summary(csn_noAllNA$SiteCode %in% csn_daily$SiteCode)
summary(csn_noAllNA$Date %in% csn_daily$Date)
dim(csn_noAllNA)

# define the data in csn_noAllNA but not csn_daily
csn_noAllNA$site.date = paste(csn_noAllNA$SiteCode, csn_noAllNA$Date)
csn_daily$site.date = paste(csn_daily$SiteCode, csn_daily$Date)
csn_noAllNA$dup = duplicated(csn_noAllNA$site.date)
summary(csn_noAllNA$dup)

# site 11130001 has no Ca data since 2016, not able to interpolate
csn_noAllNA_notPMF = subset(csn_noAllNA, 
                            !(csn_noAllNA$site.date %in% csn_daily$site.date) )
csn_PMF_notNoAllNA = subset(csn_daily, 
                            !(csn_daily$site.date %in% csn_noAllNA$site.date) )
dim(csn_noAllNA_notPMF)
dim(csn_PMF_notNoAllNA)
unique(csn_noAllNA_notPMF$SiteCode)

csn_noAllNA_left = subset(csn_noAllNA, 
                          csn_noAllNA$site.date %in% csn_daily$site.date)
csn_daily$site.date = csn_noAllNA_left$site.date = csn_noAllNA_left$dup = NULL

# OP not used for PMF
# csn_noAllNA_left$OP = NULL

dim(csn_noAllNA_left)
dim(csn_daily)

# Move grouped columns to the right of the dataframe
# OC.EC = c("EC", "OC")
OC.EC = c("EC", "EC1", "EC2", "EC3",
          "OC", "OC1", "OC2", "OC3", "OC4", "OP")
ions = c("Na.", "K.", "NH4.", "NO3", "SO4")
csn_noAllNA_left = csn_noAllNA_left %>%
  select(!(matches(OC.EC)), 
         everything())
csn_noAllNA_left = csn_noAllNA_left %>%
  select(!(matches(ions)), 
         everything())

# relocate columns
csn_noAllNA_left = csn_noAllNA_left %>% relocate(PM25, .after = SO4)
csn_noAllNA_left = csn_noAllNA_left %>% relocate(Date, .before = State)
csn_noAllNA_left = csn_noAllNA_left %>% relocate(SiteCode, .before = Date)

summary(colnames(csn_noAllNA_left) == colnames(csn_daily))

# reorder dataset
csn_noAllNA_left = csn_noAllNA_left[with(
  csn_noAllNA_left, 
  order(SiteCode, Date)), ]

summary(csn_noAllNA_left$Date == csn_daily$Date)
summary(csn_noAllNA_left$SiteCode == csn_daily$SiteCode)

# write.csv(csn_noAllNA_left, "CSN_withNA_combine_PMFuncertainty_Estimation_C-subgroup.csv")
# write.csv(csn_noAllNA_left, "CSN_withNA_combine_PMFuncertainty_Estimation_C-subgroup_2024.04.csv") # previous naming pattern
write.csv(csn_noAllNA_left, "CSN_withNA_combined_concentration_AQS_PM_C-subgroup_2024.04.csv")

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

# write.csv(csn_NA_intp, "CSN_TF_logical_InterpolatedOrNot_C-subgroup.csv")
write.csv(csn_NA_intp, "CSN_TF_logical_InterpolatedOrNot_C-subgroup_2024.04.csv")

##### CSN - replace PM2.5 with EPA AQS, combined into 2.1 interpolation since 2024.04 #####
## CSN
# species_daily = fread("CSN_RFinterpulated_combine_2023.04.csv")
csn_daily = fread("CSN_RFinterpulated_combine_Csubgroup_2023.04.csv") # interpolation after AQS PM matching
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

# compare the PM2.5 from measurement+interpolation vs. from AQS
# compare data till 2015 (more measurements) and from 2016 (mostly interpolated)
plot(csn_aqs_pm$PM25, csn_aqs_pm$PM25_AQS)
plot(csn_aqs_pm$PM25, csn_aqs_pm$PM25_AQS, 
     ylim = c(0,100), xlim = c(0,100))
cor(csn_aqs_pm$PM25, csn_aqs_pm$PM25_AQS, 
    method = "spearman", use = "complete.obs")
cor(csn_aqs_pm$PM25, csn_aqs_pm$PM25_AQS, 
    method = "pearson", use = "complete.obs")

csn_aqs_pm_2015 = 
  subset(csn_aqs_pm, Date < as.Date("2016-01-01"))
plot(csn_aqs_pm_2015$PM25, csn_aqs_pm_2015$PM25_AQS)
plot(csn_aqs_pm_2015$PM25, csn_aqs_pm_2015$PM25_AQS, 
     ylim = c(0,100), xlim = c(0,100))
cor(csn_aqs_pm_2015$PM25, csn_aqs_pm_2015$PM25_AQS, 
    method = "spearman", use = "complete.obs")
cor(csn_aqs_pm_2015$PM25, csn_aqs_pm_2015$PM25_AQS, 
    method = "pearson", use = "complete.obs")

csn_aqs_pm_2016 = 
  subset(csn_aqs_pm, Date > as.Date("2015-12-31"))
plot(csn_aqs_pm_2016$PM25, csn_aqs_pm_2016$PM25_AQS)
plot(csn_aqs_pm_2016$PM25, csn_aqs_pm_2016$PM25_AQS, 
     ylim = c(0,100), xlim = c(0,100))
cor(csn_aqs_pm_2016$PM25, csn_aqs_pm_2016$PM25_AQS, 
    method = "spearman", use = "complete.obs")
cor(csn_aqs_pm_2016$PM25, csn_aqs_pm_2016$PM25_AQS, 
    method = "pearson", use = "complete.obs")

csn_aqs_pm$PM25_Combine = 
  ifelse((is.na(csn_aqs_pm$PM25_AQS) |
            csn_aqs_pm$PM25_AQS <= 2), 
         csn_aqs_pm$PM25, 
         csn_aqs_pm$PM25_AQS)

# check the overall correlation
plot(csn_aqs_pm$PM25, 
     csn_aqs_pm$PM25_AQS, ylim = c(1, 100))
plot(csn_aqs_pm$PM25, 
     csn_aqs_pm$PM25_Combine, ylim = c(1, 100))
cor(csn_aqs_pm$PM25, csn_aqs_pm$PM25_Combine, 
    method = "pearson", use = "complete.obs")
cor(csn_aqs_pm$PM25, csn_aqs_pm$PM25_AQS, 
    method = "pearson", use = "complete.obs")

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

summary(colnames(csn_daily)[4:ncol(csn_daily)] == 
          colnames(csn_aqs_pm)[4:(ncol(csn_aqs_pm)-3)])
summary(csn_daily$SiteCode == csn_aqs_pm$SiteCode &
          csn_daily$Date == csn_aqs_pm$Date)

# reasign PM value, mainly use AQS, and delete not used
csn_aqs_pm$PM25 = csn_aqs_pm$PM25_Combine
csn_aqs_pm$species.sum = csn_aqs_pm$PM25_Combine = 
  csn_aqs_pm$PM25_AQS = csn_aqs_pm$K = NULL

# earlier version, use "_corrected_*.csv" concentrations already reset for PMF (half MDL, etc.)
# write.csv(conc_pmf_cluster, "CSN_concentration_AQS.PM_PMF_C-subgroup_2023.04.csv")
# write.csv(conc_pmf_cluster, "CSN_concentration_AQS.PM_PMF_C-sub_2023.05.csv")
# write.csv(conc_pmf_cluster, "CSN_concentration_AQS.PM_PMF_C-sub_2024.02.csv")

# the reestimation will be combined when preparing GUI/nonGUI data
write.csv(csn_aqs_pm, "CSN_concentration_AQS.PM_PMF_C-sub_2024.03.csv")

##### CSN - MDL mostly based on pre-2015 #####
csn_mdl = fread("CSN_MDL_monthly_2024.04.csv")
csn_mdl$V1 = NULL
csn_mdl = species_col_reorder(csn_mdl)
csn_mdl = relocate(csn_mdl, SiteCode, .before = "year")
names(csn_mdl)

csn_mdl_till15 = subset(csn_mdl, year < 2016)
csn_mdl_till15 = csn_mdl_till15[!(csn_mdl_till15$year == 2015 & csn_mdl_till15$month == 12), ]
csn_mdl_till15$SiteCode = as.character(csn_mdl_till15$SiteCode)

csn_mdl_till15_month_mean =
  select(csn_mdl_till15, -year) %>%
  group_by(SiteCode, month) %>% 
  dplyr::summarize(
    across(where(is.numeric), 
           mean, na.rm = TRUE),
    .groups = 'drop') %>%
  ungroup()

csn_mdl_till15_month_mean = 
  csn_mdl_till15_month_mean[with(
    csn_mdl_till15_month_mean, order(SiteCode, month)
  ), ]

csn_mdl_till15_month_median =
  select(csn_mdl_till15, -year) %>%
  group_by(SiteCode, month) %>% 
  dplyr::summarize(
    across(where(is.numeric), 
           median, na.rm = TRUE),
    .groups = 'drop') %>%
  ungroup()

csn_mdl_till15_month_median = 
  csn_mdl_till15_month_median[with(
    csn_mdl_till15_month_median, order(SiteCode, month)
  ), ]

####### calculate the correlation between corresponding species columns
matrix_mdl_till15_month_median <- as.matrix(select(csn_mdl_till15_month_median, -SiteCode, -month, -ClIon))
matrix_mdl_till15_month_mean <- as.matrix(select(csn_mdl_till15_month_mean, -SiteCode, -month, -ClIon))

csn_mdl_till15_month_correl <-
  sapply(seq.int(ncol(matrix_mdl_till15_month_median)), function(i) {
  cor(matrix_mdl_till15_month_median[, i],
      matrix_mdl_till15_month_mean[, i], 
      use = "complete.obs")
})

species_names = names(csn_mdl_till15_month_median)[col_comp(csn_mdl_till15_month_median, "Ag", "PM25")]
species_names = species_names[species_names != "ClIon"]
csn_mdl_till15_cor =
  data.frame(csn_mdl_till15_month_correl)
colnames(csn_mdl_till15_cor)[1] = "correl"
csn_mdl_till15_cor$Species = species_names
  
csn_mdl_till15_cor = relocate(csn_mdl_till15_cor, Species, .before = correl)
summary(csn_mdl_till15_cor)

ggplot(csn_mdl_till15_cor, aes(x = correl)) +
  geom_histogram(bins = 50) +
  xlim(0.75, 0.95) +
  theme_base()

####### plot the mean and median site-specific monthly MDL
csn_mdl_till15_month_mean_long = 
  csn_mdl_till15_month_mean %>%
  pivot_longer(
    cols = Ag:PM25,
    names_to = c("Species"),
    values_to = "MDL"
    )
csn_mdl_till15_month_mean_long$data = "mean"

csn_mdl_till15_month_median_long = 
  csn_mdl_till15_month_median %>%
  pivot_longer(
    cols = Ag:PM25,
    names_to = c("Species"),
    values_to = "MDL"
  )
csn_mdl_till15_month_median_long$data = "median"

csn_mdl_till15_month_plot = 
  rbind(csn_mdl_till15_month_mean_long, csn_mdl_till15_month_median_long)

# position of ggtext
csn_mdl_till15_month_plot_position =
  ddply(csn_mdl_till15_month_plot, .(Species),
        summarise,
        y_position = quantile(MDL, 0.9),
        x_position = 6.5)

# plot
ggplot(csn_mdl_till15_month_plot,
       aes(month, MDL, color = data, shape = data)) +
  geom_point(alpha = 0.3) +
  facet_wrap(Species~. , scales = "free", ncol = 5) +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = function(x) pretty(x, n = 3)) +  # reduce the number of breaks
  scale_x_continuous(breaks = c(1, 3, 6, 9, 12)) +
  scale_color_manual(values = c("#377eb8", "#ff7f00")) + # c("steelblue", "brown2"), c("#377eb8", "#ff7f00")
  geom_text(data = csn_mdl_till15_month_plot_position, size = 3.8,
            aes(x = x_position, y = y_position, label = Species), 
            inherit.aes = FALSE) + 
  theme_base() +
  theme(
    panel.grid = element_line(colour = "white"),
    plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
    strip.background = element_blank(), strip.text = element_blank(),
    legend.position = "bottom", legend.background = element_blank(),
    legend.title = element_text(size = 0),
    axis.text = element_text(size = 11, color = "grey25")
  )

##### check sites with only < 12-month of mdl
species_daily = fread("CSN_RFinterpulated_combine_Csubgroup_2024.04.csv") 

csn_mdl_till15_month_count = data.frame(table(csn_mdl_till15_month_median$SiteCode))
names(csn_mdl_till15_month_count)[1] = "SiteCode"
csn_mdl_till15_12monthLess = subset(csn_mdl_till15_month_count, Freq < 12)

species_mdl_12monthLess_daily = 
  subset(species_daily, 
         SiteCode %in% unique(csn_mdl_till15_12monthLess$SiteCode))
table(species_mdl_12monthLess_daily$SiteCode)
# 120110034 530330030 
#   683       310 
# seems to have enough data for PMF analyses, keep
# these two sites have the Val of that month, need to fill !!!!

#### for sites having no record pre 2016
summary(unique(csn_mdl_till15_month_median$SiteCode) %in% unique(species_daily$SiteCode))
summary(unique(species_daily$SiteCode) %in% unique(csn_mdl_till15_month_median$SiteCode))
unique(subset(species_daily, !(SiteCode %in% unique(csn_mdl_till15_month_median$SiteCode)))$SiteCode)
# 11130003  60731022 180190010 320310031 340230011 380150003 420710012 490050007 540390020
# sites with MDL missing from pre2016 based data, but have concentrations

summary(unique(csn_mdl$SiteCode) %in% unique(species_daily$SiteCode))
summary(unique(species_daily$SiteCode) %in% unique(csn_mdl$SiteCode))
# above 9 sites have MDL from post-2016

csn_mdl_from16 = subset(csn_mdl, year >= 2016)
csn_mdl_from16 = 
  subset(csn_mdl_from16, 
         !(SiteCode %in% csn_mdl_till15_month_median$SiteCode) &
           SiteCode %in% species_daily$SiteCode)
unique(csn_mdl_from16$SiteCode)
# 11130003 60731022 180190010 320310031 340230011 380150003 420710012 490050007 540390020  

# csn_mdl_from16_median = 
#   select(csn_mdl_from16, -year) %>%
#   group_by(SiteCode, month) %>% 
#   dplyr::summarize(
#     across(where(is.numeric), 
#            median, na.rm = TRUE),
#     .groups = 'drop') %>%
#   ungroup()
 

# use the median of csn_mdl_till15_month_median for these 9 sites
csn_mdl_till15_month_median_overall = 
    select(csn_mdl_till15_month_median, -SiteCode) %>%
    group_by(month) %>%
    dplyr::summarize(
      across(where(is.numeric),
             median, na.rm = TRUE),
      .groups = 'drop') %>%
    ungroup()

csn_mdl_from16_sitedate = 
  select(csn_mdl_from16, SiteCode, month)
csn_mdl_from16_sitedate =
  csn_mdl_from16_sitedate[!duplicated(csn_mdl_from16_sitedate), ]

csn_mdl_from16_15based = merge(csn_mdl_from16_sitedate, 
                               csn_mdl_till15_month_median_overall)  
csn_mdl_from16_15based = 
  relocate(csn_mdl_from16_15based, month, .after = SiteCode)

# use median values
csn_mdl_month_median_use =
  rbind(csn_mdl_till15_month_median, csn_mdl_from16_15based)

summary(unique(csn_mdl_month_median_use$SiteCode) %in% unique(species_daily$SiteCode))
summary(unique(species_daily$SiteCode) %in% unique(csn_mdl_month_median_use$SiteCode))

# reorder & rename columns
csn_mdl_month_median_use = species_col_reorder(csn_mdl_month_median_use)
csn_mdl_month_median_use = 
  relocate(csn_mdl_month_median_use, SiteCode, .before = month)
names(csn_mdl_month_median_use)

csn_mdl_till15_month_median_overall = species_col_reorder(csn_mdl_till15_month_median_overall)
csn_mdl_till15_month_median_overall = 
  relocate(csn_mdl_till15_month_median_overall, SiteCode, .before = month)
names(csn_mdl_till15_month_median_overall)

sum(duplicated(select(csn_mdl_month_median_use, SiteCode, month)))
write.csv(csn_mdl_month_median_use, "CSN_MDL_C-Sub_monthly_forPMF_2024.04.csv")
write.csv(csn_mdl_till15_month_median_overall, "CSN_MDL_C-Sub_monthly_2015base.csv")

##### CSN & IMPROVE - concentration vs. MDL #####
## CSN
# species_daily = fread("CSN_RFinterpulated_combine_2023.04.csv")
# species_daily = fread("CSN_RFinterpulated_combine_Csubgroup_2023.04.csv") 
species_daily = fread("CSN_RFinterpulated_combine_Csubgroup_2024.04.csv") 

## IMPROVE
species_daily = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE/IMPROVE_interpulation_random-forest_2023.csv")

species_daily$V1 = NULL
species_daily$Date = as.Date(species_daily$Date)
names(species_daily)

# get monthly MDL
### CSN
# csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_MDL_monthly.csv")
# csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_MDL_monthly_2023.02.27.csv")
# species_mdl = fread("CSN_MDL_C-Sub_monthly_2023.05.csv")

# use site-specific either pre-2016 or post-2016 montly median MDL for the whole study period
species_mdl = fread("CSN_MDL_C-Sub_monthly_forPMF_2024.04.csv")
species_mdl_till2015 = fread("CSN_MDL_C-Sub_monthly_2015base.csv")
species_mdl_till2015$V1 = species_mdl_till2015$ClIon = NULL

### IMPROVE
species_mdl = fread("IMPROVE_MDL_monthly_2023.csv")

species_mdl$V1 = species_mdl$ClIon = NULL # imp_mdl$OP = 
names(species_mdl)

# reorder
species_daily = species_daily[with(
  species_daily, 
  order(SiteCode, Date)), ]

# get month, for matching with monthly MDL
species_daily_conc = species_daily
species_daily_conc$month = month(species_daily_conc$Date)
dim(species_daily_conc)

# reorder columns the dataset for matching
species_col_mdl = names(species_mdl)[col_comp(species_mdl, "Ag", "PM25")]
species_col_conc = names(species_daily_conc)[col_comp(species_daily_conc, "Ag", "PM25")]
species_mdl_reag = species_mdl[, ..species_col_mdl]
species_mdl_till2015_reag = species_mdl_till2015[, ..species_col_mdl]

setcolorder(species_mdl_reag, 
            names(species_daily_conc[, ..species_col_conc]))
setcolorder(species_mdl_till2015_reag, 
            names(species_daily_conc[, ..species_col_conc]))
species_mdl_use = 
  data.frame(select(species_mdl, SiteCode, month), 
             species_mdl_reag)
species_mdl_till2015_use = 
  data.frame(select(species_mdl_till2015, month), 
             species_mdl_till2015_reag)

# check if species order match
species_col_mdl_use = names(species_mdl_use)[col_comp(species_mdl_use, "Ag", "PM25")]
summary(species_col_conc == species_col_mdl_use)

# expand MDL file to daily measurement 
# (in case of interpolation, not used original data directly)
species_daily_conc_date = select(species_daily_conc, 
                                 SiteCode, State, Date, month)
species_daily_conc_date$month = as.integer(species_daily_conc_date$month)
unique(subset(species_mdl_use, !(SiteCode %in% unique(species_daily$SiteCode)))$SiteCode)
# 132950002 GA 150030010 HI  20900034 AK 220150008 LA 370670022 NC 
# 530530031 WA 540390011 WV 560210100 WY 60850005 CA, not included in species daily
species_daily_fullMDL = merge(species_daily_conc_date, 
                              species_mdl_use,
                              all.x = TRUE)

####### deal with NA for the two sites of < 12 month MDL, fill based on its own median of pre-2015 records
# reorder rows the dataset for estimation
summary(species_daily_fullMDL) 
species_daily_fullMDL = species_daily_fullMDL[with(
  species_daily_fullMDL, 
  order(SiteCode, Date)), ]

# subset dataset including only sites with missing MDL
species_mdl_withNA =
  subset(species_daily_fullMDL, 
         SiteCode %in% 
           unique(subset(species_daily_fullMDL, is.na(Al))$SiteCode))
site_na_mdls = unique(species_mdl_withNA$SiteCode)

# site-specific median
species_mdl_withNA_median = 
  select(species_mdl_withNA, -State, -Date, -month) %>%
  group_by(SiteCode) %>%
  dplyr::summarize(
    across(where(is.numeric),
           median, na.rm = TRUE),
    .groups = 'drop') %>%
  ungroup()

species_mdl_NA_sitedate =
  select(subset(species_daily_fullMDL, is.na(Al)),
         SiteCode, Date, State, month, Al)
summary(species_mdl_NA_sitedate)

species_mdl_NAreplace =
  merge(select(species_mdl_NA_sitedate, -Al),
        species_mdl_withNA_median)
species_mdl_NAreplace$month = NULL

species_daily_fullMDL$month = species_daily_conc$month = NULL
species_daily_fullMDL =
  rbind(na.omit(species_daily_fullMDL), species_mdl_NAreplace)

# reorder columns the dataset for matching
setcolorder(species_daily_fullMDL, 
            names(species_daily_conc))

dim(species_daily_conc_date)
dim(species_daily_fullMDL)
summary(names(species_daily_conc) == names(species_daily_fullMDL))

# reorder rows the dataset for matching
species_daily_fullMDL = species_daily_fullMDL[with(
  species_daily_fullMDL, 
  order(SiteCode, Date)), ]

species_daily_conc = species_daily_conc[with(
  species_daily_conc, 
  order(SiteCode, Date)), ]

# double check if date & site match
summary(species_daily_fullMDL$SiteCode == species_daily_conc$SiteCode)
summary(species_daily_fullMDL$Date == species_daily_conc$Date)
summary(names(species_daily_fullMDL) == names(species_daily_conc))

# compare concentration and MDL of a given component
cols_to_extract <- setdiff(names(species_daily_conc), 
                           c("SiteCode", "Date", "State"))
# species_daily_conc$OP = NULL # remove later, keep OP
species_conc = species_daily_conc[, ..cols_to_extract]
species_mdl_only = species_daily_fullMDL[, ..cols_to_extract]

species_conc_mdl = data.frame(Map(">", species_conc, species_mdl_only))
setDT(species_conc_mdl)

species_conc[1:3, 1:10]
species_mdl_only[1:3, 1:10]
species_conc_mdl[1:3, 1:10]

# check variable class & dataset dimenssion
sapply(species_conc, class)
sapply(species_mdl_only, class)
sapply(species_conc_mdl, class)
dim(species_conc)
dim(species_mdl_only)
dim(species_conc_mdl)

species_conc_mdl_Site = 
  cbind(select(species_daily_conc, SiteCode, Date, State), 
        species_conc_mdl)

####### For site & date match check, finished!
species_conc_mdl_randomsite = subset(species_conc_mdl_Site, SiteCode == "60371103") # "BADL1", "60371103"
species_mdl_only_randomsite = subset(species_daily_fullMDL, SiteCode == "60371103")
species_conc_randomsite = subset(species_daily_conc, SiteCode == "60371103")

species_conc_mdl_randomsite[1:3, 4:13]
species_mdl_only_randomsite[1:3, 4:13]
species_conc_randomsite[1:3, 4:13]

####### For site & date match check, finished!

# only SiteCode & Date before species
# species_conc_mdl_Site$State = NULL

# write.csv(species_conc_mdl_Site, "CSN_conc_vs_MDL_C-subgroup_corrected_2023.05.csv")
# write.csv(species_conc_mdl_Site, "CSN_conc_vs_MDL_C-subgroup_corrected_2024.02.csv")
# write.csv(species_conc_mdl_Site, "CSN_conc_vs_MDL_C-subgroup_corrected_2024.03.csv")
write.csv(species_conc_mdl_Site, "CSN_conc_vs_MDL_C-subgroup_corrected_2024.04.csv")
write.csv(species_daily_fullMDL, "CSN_MDL_C-Sub_monthly_forPMF_expand_2024.04.csv")

# species_conc_mdl_Site = subset(species_conc_mdl_Site, !is.na(OP))

# write.csv(species_conc_mdl_Site, "IMPROVE_conc_vs_MDL_C-subgroup_corrected_2023.csv")
write.csv(species_conc_mdl_Site, "IMPROVE_conc_vs_MDL_C-subgroup_corrected_2024.csv")


