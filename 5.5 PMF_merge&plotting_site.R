library(sf)
library(readr)
library(scales)
library(dplyr)
library(plyr)
library(tidyr)
library(stringr)
library(deming) # Thiel-Sen
library(gganimate)
library(ggthemes)
library(ggplot2)
library(purrr)
library(ggsci)
library(gridExtra)
library(insight)
library(data.table)
library(patchwork)

setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/")
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/"
getwd()

#### 1. Prepare & merge info for manual source assignment ####

# # extra 5% uncertainty
# data_use = "CSN_Site_15TimesMean"
# data.pre = "CSN_noCsub_15TimesMean_"
# 
# # 0 uncertainty
# data_use = "CSN_Site_15tMean_0unc"
# data.pre = "CSN_noCsub_15tMean_0unc_"

# 0 uncertainty
data_use = "CSN_Site_15t1mdl0unc"
data.pre = "CSN_noCsub_15t1mdl0unc_"

dir_path <- paste0("PMF_NonGUI/", data_use, "/base_DISPres1")

time = Sys.Date()

pdf.series <- c("factor_pairs.pdf", "source_profile.pdf", "overall.pdf", "daily.pdf")
csv.series <- c("source_profile.csv", "overall.csv", "daily.csv", "annual.csv", "month.csv")

###### 1.1. overall contribution, merge & transform, FINISHED ######

csv_overall_list <- list.files(dir_path, pattern = ".*overall\\.csv$", full.names = TRUE)

csv_overall <- 
  do.call(
    rbind, 
    (lapply(
      csv_overall_list, 
      read.csv)))
csv_overall$Dataset = "CSN"
csv_overall$X = NULL


# reoder the columns
# get the rest of the column names, excluding those already in desired_order
adjusted_columns <- c("Dataset", "site.serial", "Factor.No", "Factor")
remaining_columns <- 
  setdiff(
  names(csv_overall), 
  adjusted_columns)


csv_overall <- 
  csv_overall[, c(adjusted_columns, remaining_columns)]

# write.csv(csv_overall, paste0(data_use, "_overall.csv"))
write.csv(csv_overall, paste0(data_use, "_overall", time, ".csv"))

#### monthly contri
csv_month_list <- list.files(dir_path, pattern = ".*month\\.csv$", full.names = TRUE)

csv_month <- 
  do.call(
    rbind, 
    (lapply(
      csv_month_list, 
      fread)))
csv_month$Dataset = "CSN"
csv_month$X = NULL

csv_daily_list <- list.files(dir_path, pattern = ".*daily\\.csv$", full.names = TRUE)

csv_daily <- 
  do.call(
    rbind, 
    (lapply(
      csv_daily_list, 
      fread)))
csv_daily$Dataset = "CSN"
csv_daily$V1 = NULL

csv_month <- 
  csv_daily[, .(
    Concentration = quantile(Concentration, 0.5),
    conc_up = quantile(Concentration, 0.95),
    conc_down = quantile(Concentration, 0.05), 
    Percent = quantile(Percent, 0.5),
    perc_up = quantile(Percent, 0.95),
    perc_down = quantile(Percent, 0.05)
  ), 
  by = .(Dataset, site.serial, Factor.No, Factor, Year, Month, Year_Month, 
         Main_Species, Source.No, Source_reference, Source.No, Factor_source)]


# reoder the columns
# get the rest of the column names, excluding those already in desired_order
adjusted_columns <- c("Dataset", "site.serial", "Factor.No", "Factor", 
                      "Factor_source", "Year", "Month", "Concentration")
remaining_columns <- 
  setdiff(
    names(csv_month), 
    adjusted_columns)

csv_month <- 
  csv_month[, c(adjusted_columns, remaining_columns)]

write.csv(csv_month, paste0(data_use, "_month", ".csv"))
# CSN_Site_15t1mdl0unc_month.csv

# transform the Assigned source data, results into one line for each Dataset-site.serial-Factor.No group
overall_sourceAssigned = subset(csv_overall, Source.No != "F")
overall_sourceAssigned = 
  select(overall_sourceAssigned,
         Dataset, site.serial, Factor.No, Factor, 
         Source_reference, Main_Species, Faraction_conc_contri)

names(overall_sourceAssigned)[5:7] = 
  c("assigned_Source", "assigned_Species", "assigned_Fraction")
tans_sourceAssigned <- 
  overall_sourceAssigned %>%
  group_by(Dataset, site.serial, Factor.No) %>%
  group_modify(~ transform_group(.x, "assigned_Source", "assigned_Species", "assigned_Fraction")) %>%
  ungroup()
head(tans_sourceAssigned)

# transform the To-be-assigned source data, results into one line for each Dataset-site.serial-Factor.No group
overall_toAssign = subset(csv_overall, Source.No == "F")
overall_toAssign = 
  select(overall_toAssign,
         Dataset, site.serial, Factor.No, 
         Factor_source, Main_Species, Faraction_conc_contri)

names(overall_toAssign)[4:6] = 
  c("to_assign_Source", "to_assign_Species", "to_assign_Fraction")
tans_toAssign <- 
  overall_toAssign %>%
  group_by(Dataset, site.serial, Factor.No) %>%
  group_modify(~ transform_group(.x, "to_assign_Source", "to_assign_Species", "to_assign_Fraction")) %>%
  ungroup()
head(tans_toAssign)

# determine if there is Zero-contribution
names(tans_sourceAssigned)[2] = names(tans_toAssign)[2] = "serial.No"
tans_sourceAssigned$assigned_Zero =
  apply(
    tans_sourceAssigned[, grepl("Fraction", names(tans_sourceAssigned))], 
    1, 
    function(x) 
      ifelse(any(x == "0%" & !is.na(x)), 1, 0))

tans_toAssign$toAssign_Zero =
  apply(
    tans_toAssign[, grepl("Fraction", names(tans_toAssign))], 
    1, 
    function(x) 
      ifelse(any(x == "0%" & !is.na(x)), 1, 0))

# determine if all contributions are 0
tans_sourceAssigned$assigned_all_Zero =
  apply(
    tans_sourceAssigned[, grepl("Fraction", names(tans_sourceAssigned))], 
    1, 
    function(x) ifelse(all(x == "0%" & !is.na(x)), 1, 0))

tans_toAssign$toAssign_all_Zero =
  apply(
    tans_toAssign[, grepl("Fraction", names(tans_toAssign))], 
    1, 
    function(x) ifelse(all(x == "0%" & !is.na(x)), 1, 0))


## reorder the columns & rows
tans_sourceAssigned <- tans_sourceAssigned[, reorder_col_number(tans_sourceAssigned)]
tans_toAssign <- tans_toAssign[, reorder_col_number(tans_toAssign)]

tans_sourceAssigned = 
  tans_sourceAssigned[with(tans_sourceAssigned, order(serial.No, Factor.No)), ]

tans_toAssign = 
  tans_toAssign[with(tans_toAssign, order(serial.No, Factor.No)), ]


write.csv(overall_sourceAssigned, paste0(data_use, "_Source_assigned.csv"))
write.csv(overall_toAssign, paste0(data_use, "_Source_to_assign.csv"))

write.csv(tans_sourceAssigned, paste0(data_use, "_Source_assigned_reorder.csv"))
write.csv(tans_toAssign, paste0(data_use, "_Source_to_assign_reorder.csv"))

###### 1.2 other source-related info ######

dropbox_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/"

# site_info_all = read.csv(paste0(dropbox_path, "CSN_NoGUI_NoCsub_15TimesMean_site/CSN_noCsub_15timesMean_PMF_SWB_site.csv"))
site_info_all = read.csv(paste0(dropbox_path, "CSN_NoGUI_NoCsub_15t1mdl0unc_site/CSN_noCsub_15t1mdl0unc_PMF_SWB_site.csv"))

site_geo = read.csv(paste0(dropbox_path, "CSN_IMPROVE_ownPC/CSN_site_info.csv"))
site_geo$SiteCode = as.character(site_geo$SiteCode)

site_geoid = read.csv(paste0(dropbox_path, "CSN_IMPROVE_ownPC/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv"))

cty_cluster_traffic = read.csv("results_R_data/County_cluster_traffic_info.csv")

# site serial
site_code_serial = read.csv("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_IMPROVE_ownPC/CSN_IMPROVE_site.serial.csv")
site_code_serial$X = NULL
site_serial_info = subset(site_code_serial, serial.No %in% site_info_all$serial.No)
dim(site_serial_info)

# PMF performance direct summary
PMF_base_summary = read.csv(
  paste0(dir_path, "/", data.pre, "base_DISP_summary.csv")
)

tans_sourceAssigned = read.csv(paste0(data_use, "_Source_assigned_reorder.csv"))
tans_toAssign = read.csv(paste0(data_use, "_Source_to_assign_reorder.csv"))

site_info_all$X = site_geo$X = cty_cluster_traffic$X = site_geoid$X = 
  PMF_base_summary$X = tans_sourceAssigned$X = tans_toAssign$X = NULL

site_serial = select(site_serial_info, SiteCode, serial.No)
site_serial$SiteCode = as.character(site_serial$SiteCode)
site_geoid = select(site_geoid, SiteCode, geoid)

# edit before merge
names(PMF_base_summary)[3] = "Factor.No"
PMF_base_summary$Dataset = gsub("^(.*)_.*$", "\\1", PMF_base_summary$Dataset)

# repeat to match factor number
site_serial_factor = site_serial[rep(1:nrow(site_serial), each = 9), ]
site_serial_factor$Factor.No = rep(3:11, nrow(site_serial))

col_remove_cty = c("Dataset", "state_abbr", "Longitude", "Latitude",
                   "countyns", "namelsad", "county_name", "geoid")
site_cluster_traffic = select(cty_cluster_traffic, -col_remove_cty)

# merge all listed files, all.x = TRUE
# make sure all listed files are data.frame or all are data.table, otherwise, error in Reduce
list_site_census_source_assign = 
  list(site_serial, site_geo, site_geoid, site_cluster_traffic, 
       PMF_base_summary, tans_toAssign, tans_sourceAssigned)

site_census_source_assign =
  Reduce(function(x, y) 
    merge(x, y, all.x = TRUE), 
    list_site_census_source_assign)

site_census_source_assign = 
  relocate(site_census_source_assign, Dataset, .before = serial.No)
site_census_source_assign = 
  relocate(site_census_source_assign, SiteCode, .before = serial.No)

# merge with converge results
converge_all = read.csv(
  paste0(dir_path, "/", data.pre, "converge_percent.csv")
)

converge_all$X = NULL
names(converge_all)[1:2] = c("serial.No", "Factor.No")

site_census_source_assign = 
  merge(site_census_source_assign, converge_all, all.x = TRUE)

# reorder the file
site_census_source_assign = 
  merge(site_serial_factor, site_census_source_assign, all.x = TRUE)
site_census_source_assign = 
  site_census_source_assign[with(site_census_source_assign, 
                                 order(serial.No, Factor.No)), ]

# in case of dup
site_census_source_assign$DUP =
  duplicated(site_census_source_assign[, 2:3])
summary(site_census_source_assign$DUP)
site_census_source_assign = 
  relocate(site_census_source_assign, DUP, .before = "Dataset")
site_census_source_assign =
  subset(site_census_source_assign, !DUP)

## RMSE & R2, these two variable were not add beofre firstly summarizing CSN_Site_15t1mdl0unc results
# rmse_validate = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15t1mdl0unc/base_DISPres1/CSN_noCsub_15t1mdl0unc_rmse_R2.csv")
# site_pm_rmse =
#   merge(select(site_census_source_assign, serial.No, Factor.No),
#         rmse_validate, all.x = TRUE)
# dim(site_pm_rmse)
# site_pm_rmse = site_pm_rmse[!duplicated(site_pm_rmse), ]
# write.csv(site_pm_rmse, paste0(data_use, "_matched_RMSE_R2_", ".csv"))

write.csv(site_census_source_assign, 
          paste0(data_use, "_PMF_source_census_", time, ".csv"))
# "CSN_Site_15TimesMean_PMF_source_census_2024-03-15.csv"
# "CSN_Site_15t1mdl0unc_PMF_source_census_2024-04-19.csv"

# data_use = "CSN_Site_15TimesMean"
# site_census_source_assign = site_census_source_assign_5unc

# data_use = "CSN_Site_15tMean_0unc"
# site_census_source_assign = site_census_source_assign_0unc


###### 1.3 site-specific performance summary ######

# site_census_source_assign_5unc = read.csv("CSN_Site_15TimesMean_PMF_source_census_2024-03-22.csv")
# site_perform = read.csv("CSN_Site_15tMean_0unc_PMF_source_census_2024-03-22.csv")
site_perform = read.csv("PMF_NonGUI/CSN_Site_15t1mdl0unc/base_DISPres1/CSN_noCsub_15t1mdl0unc_decided_factor_performance_2024.06.csv")
site_perform$X = NULL
dim(site_perform)

site_perform = na.omit(site_perform)
summary(site_perform)

# PMF predicted PM2.5 vs. observations
cor_overall_obs_pmf = 
  calculate_corr_label(
    site_perform, 
    "median_obs_PM2.5", "median_PMF_PM2.5")
cor_overall_obs_pmf

pmf_obs <- 
  ggplot(site_perform, 
         aes(x = median_obs_PM2.5, y = median_PMF_PM2.5)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  scale_x_continuous(limits = c(0, NA), breaks = c(0, 4, 8, 12)) +
  scale_y_continuous(limits = c(0, NA), breaks = c(0, 4, 8, 12)) +
  # geom_text(size = 0,  aes(x = 2, y = 9, label = "R = 0.97; p = 0.00")) + 
  labs(x = format_variable("Observation  µg/m3"),
       y = format_variable("PMF prediction  µg/m3")) +
  theme_bw(base_size = 20)
pmf_obs

# other validation parameter distribution
site_perform_estimated = 
  select(site_perform, 
         SiteCode, serial.No, Factor.No,
         cor_PMF.obs_PM, RMSE_PM2.5,  R2_PM2.5)

# convert to longer version
site_perform_estimated = 
  site_perform_estimated %>%
  pivot_longer(
    cols = cor_PMF.obs_PM:R2_PM2.5,
    names_to = c("Validation_method"),
    values_to = "Performance"
  )
head(site_perform_estimated)

r2_r2 <- 
  ggplot(subset(site_perform_estimated,
                Validation_method != "RMSE_PM2.5" &
                  Performance > 0), 
         aes(x = Validation_method, y = Performance)) +
  geom_jitter(width = 0.2, alpha = 0.25, size = 2)+
  geom_boxplot(outlier.shape = NA, fill = NA,
               linewidth = 0.6, width = 0.5) +
  scale_x_discrete(labels = c("Correlation \ncoefficient", 
                              "Coefficient of \ndetermination")) +
  labs(x = "Validation method") +
  theme_bw(base_size = 20)
r2_r2

rmse_pm <- 
  ggplot(subset(site_perform_estimated,
                Validation_method == "RMSE_PM2.5" &
                  Performance < 6), 
         aes(x = Performance)) +
  geom_histogram(alpha = 0.8) +
  labs(x = format_variable("RMSE  µg/m3"), y = "Count") +
  theme_bw(base_size = 20)
rmse_pm

pm_perform = pmf_obs + r2_r2 + rmse_pm


## Converged percent
# ggplot(site_perform, 
#        aes(x = converge_percent * 100)) + 
#   geom_histogram(alpha = 0.6) +
#   facet_grid(. ~ overall_unc) +
#   scale_x_continuous(breaks = 3:11) +
#   theme_bw(base_size = 16)

###### 1.4 species prediction performance ######
# species_perf_5 = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15TimesMean/base_DISPres1/CSN_noCsub_15TimesMean_PMF_vs_obs.csv")
species_perf_0 = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15tMean_0unc/base_DISPres1/CSN_noCsub_15tMean_0unc_PMF_vs_obs.csv")
species_perf_5$X = species_perf_0$X = NULL
species_perf_0$higher.3.scalRes.per = 
  as.numeric(sub("%$", "", 
                 species_perf_0$higher.3.scalRes.per))
species_perf_5$higher.3.scalRes.per = 
  as.numeric(sub("%$", "", 
                 species_perf_5$higher.3.scalRes.per))

species_perf_5 =
  species_perf_5 %>%
  pivot_longer(
    cols = RMSE:higher.3.scalRes.per,
    names_to = "criteria",
    values_to = "performance"
  )
species_perf_0 =
  species_perf_0 %>%
  pivot_longer(
    cols = RMSE:higher.3.scalRes.per,
    names_to = "criteria",
    values_to = "performance"
  )

species_perf_5$overall_unc = "Extra_overall_5%"
species_perf_0$overall_unc = "No_extra_uncertainty"

species_perform = rbind(species_perf_5, species_perf_0)

ggplot(species_perf_5, 
       aes(x = Species, y = performance)) +
  geom_boxplot() +
  facet_grid(criteria ~ ., scales = "free") +
  theme_bw(base_size = 12)

ggplot(species_perf_0, 
       aes(x = Species, y = performance)) +
  geom_boxplot() +
  facet_grid(criteria ~ ., scales = "free") +
  theme_bw(base_size = 12)






#### 2.0 daily site SA results 2024.04 ####

# source_org = read.csv("CSN_Site_15t1mdl0unc_PMF_decision_2024-04.csv")
# source_org = read.csv("CSN_Site_15t1mdl0unc_PMF_decision_2024-05.csv")
source_org = read.csv("CSN_Site_15t1mdl0unc_PMF_decision_2024-05-30.csv")
source_org$X = NULL
names(source_org)
dim(source_org)

# remove the "assigned_" and re-organize the dataframe
source_org_long = 
  source_org %>%
  pivot_longer(
    cols = starts_with("assigned"),  # selects all columns that start with 'assigned'
    names_to = c(".value", "number"), # '.value' helps in splitting column names based on a pattern
    names_pattern = "assigned_(.*)_(\\d+)$"  # Regex to split the column names into 'assigned_Source', 'assigned_Species', 'assigned_Fraction', and a group number
  )
names(source_org_long)
# dim(source_org_long)
# View(source_org_long)
nrow(subset(source_org_long, Source == "F3-Vehicle"))
nrow(subset(source_org_long, Source == "F7-Industry"))

# remove the mixed for now
source_org_long = subset(source_org_long, !(is.na(Fraction)))
dim(source_org_long)
# double check if the manual match was correct
# select(subset(source_org_long, serial.No == "150"), serial.No, Factor.No, Factor, Fraction, Species, Source)

#  remove the mixed-source factors
source_org_long = 
  subset(source_org_long, 
         !(grepl("+", Source, fixed = T)))

# rename some sources
source_org_long$Source[source_org_long$Source == "F3-Vehicle"] = "F1-Traffic"
source_org_long$Source[source_org_long$Source == "F10-Ship"] = "F1-Traffic"
source_org_long$Source[source_org_long$Source == "Non-tailpipe"] = "F7-Non-tailpipe"

source_org_long$Source[source_org_long$Source == ""] = "F7-Industry"
source_org_long$Source[source_org_long$Source == "F7-Industry"] = "F5-Industry"
source_org_long$Source[source_org_long$Source == "F10-Agriculture"] = "F5-Industry"

source_org_long = plyr::rename(source_org_long, 
                          c("Factor" = "Factor_source",
                            "Source" = "Source_aftermanual"))
unique(source_org_long$Source_aftermanual)
unique(source_org_long$Factor_source)
nrow(subset(source_org_long, Source_aftermanual == "F1-Traffic"))
nrow(subset(source_org_long, Source_aftermanual == "F5-Industry"))

# get fraction numeric
source_org_long$Fraction_nm = as.numeric(sub("%", "", source_org_long$Fraction))
source_org_long$Fraction = NULL
summary(source_org_long$Fraction_nm)

# remove those with NA fraction
source_org_long = subset(source_org_long, !(is.na(Fraction_nm)))
dim(source_org_long)
names(source_org_long)

# check the number of each source
data.frame(table(source_org_long$Source_aftermanual))
# check the number of sites with a given source, which might be two source to one factor
length(unique(subset(source_org_long, Source_aftermanual == "F1-Traffic")$SiteCode))
length(unique(subset(source_org_long, Source_aftermanual == "F5-Industry")$SiteCode))
length(unique(subset(source_org_long, Source_aftermanual == "F2-Secondary Nitrate")$SiteCode))
length(unique(subset(source_org_long, Source_aftermanual == "F3-Secondary Sulfate")$SiteCode))

# generate into to check the source-site match
source_site_match = 
  select(source_org_long, 
         SiteCode, serial.No, Factor.No, 
         Source_aftermanual, Fraction_nm)

# estimate the number of each source of each site
unique_source_count <- 
  source_site_match %>%
  dplyr::group_by(SiteCode, serial.No, Factor.No, Source_aftermanual) %>%
  dplyr::summarize(count_sources = length(Source_aftermanual),
                   Fraction_nm = sum(Fraction_nm),
                   .groups = "drop")

# check the site lacking given sources
source_org_site_source = 
  select(unique_source_count, -Fraction_nm) %>% 
  pivot_wider(names_from = Source_aftermanual, 
              values_from = count_sources)

# # re-arrange columns accordingn to factor F-X
# source_org_site_source <- 
#   source_org_site_source %>%
#   select(SiteCode, serial.No, Factor.No, `F1-Traffic`, `F2-Secondary Nitrate`, `F3-Secondary Sulfate`,
#          `F4-Aged Sea Salt`, `F5-Industry`, `F6-Fresh Sea Salt`, `F7-Non-tailpipe`, 
#          `F8-Biomass`, `F9-Soil/Dust`)

# remove backticks and spaces from column names
names(source_org_site_source) <- gsub("`| ", "", names(source_org_site_source))

# reorder rows by serial.No
source_org_site_source = 
  source_org_site_source[with(source_org_site_source, order(serial.No)), ]


#### 2.1 match with monthly contribution ####

# combine with monthly concentration contribution
month_source = fread("CSN_Site_15t1mdl0unc_month.csv")
month_source$V1 = NULL
names(month_source)[2] = "serial.No"
head(month_source)
unique(month_source$Factor_source)

# should be character(0) here
unique(source_org_long$Factor_source)[!(unique(source_org_long$Factor_source) %in% unique(month_source$Factor_source))]
# probably "Factor10" "Factor11" here
unique(month_source$Factor_source)[!(unique(month_source$Factor_source) %in% unique(source_org_long$Factor_source))]
summary(source_org_long$serial.No %in% month_source$serial.No)

source_org_long$site_factor = 
  paste(source_org_long$serial.No, source_org_long$Factor.No)
month_source$site_factor = 
  paste(month_source$serial.No, month_source$Factor.No)
source_org_long = data.frame(source_org_long)
summary(unique(source_org_long$site_factor) %in% unique(month_source$site_factor))
unique(source_org_long$site_factor)[!(unique(source_org_long$site_factor) %in% unique(month_source$site_factor))]

# manual check if the Factor_source match
select(subset(source_org_long, serial.No == "150"), site_factor, Factor_source, Species, Source_aftermanual)
b = select(subset(month_source, serial.No == "150" & Factor.No == 7), site_factor, Factor_source, Main_Species)
b[!duplicated(b), ]

month_source_cleaning = 
  merge(source_org_long, month_source, 
        by = c("Dataset", "serial.No", "Factor.No", "Factor_source", "site_factor"))
dim(month_source_cleaning)
dim(source_org_long)
dim(month_source)
# check the number of sites with Vehicle same as the decision file
length(unique(subset(month_source_cleaning, Source_aftermanual == "F1-Traffic")$SiteCode))
length(unique(subset(month_source_cleaning, Source_aftermanual == "F5-Industry")$SiteCode))

summary(data.frame(table(source_org_long$serial.No))$Var1 %in% data.frame(table(month_source$serial.No))$Var1)

summary(unique(month_source_cleaning$Factor_source) %in% unique(source_org_long$Factor_source))
month_source_cleaning$site_factor = NULL

# reorder
month_source_cleaning = 
  month_source_cleaning[with(month_source_cleaning,
    order(serial.No, Factor.No, Source_aftermanual, Year, Month)), ]

month_source_cleaning = 
  relocate(month_source_cleaning, Source_aftermanual, .after = Factor.No)

# Check those main_species not match with Species column, from re-assign
month_source_specie_not_match =
  subset(month_source_cleaning,
         Main_Species != Species)
month_source_specie_not_match$Summary.Row.No = 
  month_source_specie_not_match$Longitude = month_source_specie_not_match$Latitude = 
  month_source_specie_not_match$geoid = month_source_specie_not_match$state_name = 
  month_source_specie_not_match$start_date = month_source_specie_not_match$end_date = NULL
# after checking, finding the unmatch is when two factors were assigned to the same source during auto-assignment
# thus, remove

month_source_cleaning_matchSpecies = 
  subset(month_source_cleaning, Main_Species == Species)

dim(month_source_cleaning_matchSpecies)
month_source_cleaning_matchSpecies$Species = NULL

# rename sources, and Aged & Fresh sea salt to Sea Salt
month_source_cleaning_matchSpecies$Source_use = month_source_cleaning_matchSpecies$Source_aftermanual

# replace the source names for new plotting
replacement_sourcename <- 
  c("F7-Non-tailpipe" = "F4-Non-tailpipe",
    "F6-Fresh Sea Salt" = "F6-Salt",
    "F4-Aged Sea Salt" = "F6-Salt",
    "F8-Biomass" = "F7-Biomass",
    "F9-Soil/Dust" = "F8-Soil/Dust")

month_source_cleaning_matchSpecies <- 
  month_source_cleaning_matchSpecies %>%
  mutate(Source_use = 
           ifelse(Source_use %in% names(replacement_sourcename), 
                  replacement_sourcename[Source_use], 
                  Source_use))

# in manual assign, for cases of >1 factors assigned to the same source
# Get the average
month_source_assign <- 
  month_source_cleaning_matchSpecies %>%
  group_by_at(vars(-Fraction_nm, -Concentration, -Factor_source, 
                   -Source_reference, -Source_aftermanual, -number, 
                   -Factor, -Source.No, -Main_Species)) %>%
  dplyr::summarise(
    Fraction_nm = mean(Fraction_nm, na.rm = TRUE),
    Concentration = mean(Concentration, na.rm = TRUE),
    .groups = "drop")
dim(month_source_assign)


month_source_assign_keyinfo = 
  select(month_source_assign, SiteCode, serial.No, Factor.No, 
         State, Latitude, Longitude, geoid,
         Year, Month, Source_use, Fraction_nm, Concentration)

month_source_assign_vehicle = subset(month_source_assign_keyinfo, Source_use == "F1-Traffic")
length(unique(month_source_assign_vehicle$SiteCode))

# write.csv(month_source_cleaning_matchSpecies, paste0(data_use, "_montly_source_assigned_with_2factor_to_one.csv"))
# write.csv(month_source_assign, paste0(data_use, "_montly_source_assigned.csv"))

write.csv(month_source_cleaning_matchSpecies, paste0(data_use, "_montly_source_assigned_with_2factor_to_one_2024-05.csv"))
write.csv(month_source_assign, paste0(data_use, "_montly_source_assigned_2024-05.csv"))

#### 4. plotting - data preparation ####

#### 4.1 annual & monthly data preparation ####

# # many packages disappers, reinstall via devtools, and github user link
# install.packages("devtools")
# devtools::install_github("ropensci/USAboundaries") # , force = TRUE, reinstall even with current version 
# devtools::install_github("ropensci/USAboundariesData") 

library(USAboundaries)
library(USAboundariesData)
library(ggsci)
library(gganimate)

# jco, tron, startrek, rickandmorty, futurama, 
# show color in different palette in ggsci to chose the eight color for sources  
pal_npg(color_npg)  
color_npg = pal_npg()(10)
show_col(pal_npg()(10))
pal_npg()(10)
show_col(pal_jco()(10))
pal_jco()(10)
show_col(pal_tron()(7))
pal_tron()(7)
show_col(pal_startrek()(7))
pal_startrek()(7)
show_col(pal_rickandmorty()(12))
pal_rickandmorty()(12)
show_col(pal_futurama()(12))
pal_futurama()(12)
show_col(pal_flatui()(10))
pal_flatui()(10)

color_source = c(
  "F1-Traffic" = "#C71000FF",         # red 
  "F2-Secondary Nitrate" = "#0073C2FF", # blue
  "F3-Secondary Sulfate" = "#16A085FF", # green 16A085FF 00A087FF
  "F4-Non-tailpipe" = "#F7C530FF",     # yellow FAFD7CFF F7C530FF
  "F5-Industry" = "#E89242FF",         # orange
  "F6-Salt" = "#00B5E2FF",             # cyan 6EE2FFFF  84D7E1FF  00B5E2FF
  "F7-Biomass" = "#8A4198FF",          # purple #8A4198FF violetred
  "F8-Soil/Dust" = "grey40"         # Gray, brown
)
  
# monthly assigned source
# month_source_assign = read.csv("CSN_Site_15t1mdl0unc_montly_source_assigned.csv")
month_source_assign = read.csv("CSN_Site_15t1mdl0unc_montly_source_assigned_2024-05.csv")
month_source_assign$X = NULL

# generate the US county boundary data
us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr[, 13] = NULL # there are two "state_name"
head(us_cty_bdr)
us_cty_bdr_geo = dplyr::select(us_cty_bdr, geoid, stusps, geometry)

# MainStates <- map_data("state")
UScounty <- map_data("county")

us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr <- us_cty_bdr[!(us_cty_bdr$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

# GPS assigned earlier
# cty_rural_urban = read.csv("/Users/TingZhang/Dropbox/HEI_US_PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
# cty_rural_urban$X = cty_rural_urban$X.1 = NULL
# cty_rural_urban = cty_rural_urban[!duplicated(cty_rural_urban$SiteCode), ] 
# 
# month_contri_gps = merge(month_source_assign, 
#                          cty_rural_urban,
#                          by = "SiteCode",
#                          all.x = T)

month_contri_gps = month_source_assign

month_contri_gps$geoid = ifelse(month_contri_gps$geoid < 10000, 
                                 paste0("0", month_contri_gps$geoid), 
                                 month_contri_gps$geoid)


month_source_gps = merge(us_cty_bdr_geo, month_contri_gps)
month_source_gps$Dataset.x = "CSN"

Year_aggregated <- ddply(month_source_gps, 
                         .(Dataset.x, SiteCode, Year, Source_use), 
                         summarise,
                         Longitude = mean(Longitude),
                         Latitude = mean(Latitude),
                         Concentration = median(Concentration),
                         conc_up = median(conc_up),
                         conc_down = median(conc_down),
                         Percent = median(Percent),
                         perc_up = median(perc_up),
                         perc_down = median(perc_down))

year_month_aggregated <- ddply(month_source_gps, 
                               .(Dataset.x, SiteCode, Year, Month, Source_use), 
                               summarise,
                               Longitude = mean(Longitude),
                               Latitude = mean(Latitude),
                               Concentration = mean(Concentration),
                               conc_up = median(conc_up),
                               conc_down = median(conc_down),
                               Percent = median(Percent),
                               perc_up = median(perc_up),
                               perc_down = median(perc_down))

month_aggregated <- ddply(month_source_gps, .(Dataset.x, SiteCode, Month, Source_use), summarise,
                          Longitude = mean(Longitude),
                          Latitude = mean(Latitude),
                          Concentration = mean(Concentration),
                          conc_up = median(conc_up),
                          conc_down = median(conc_down),
                          Percent = median(Percent),
                          perc_up = median(perc_up),
                          perc_down = median(perc_down))

# write.csv(Year_aggregated, "Annual_site_source_contribuion_CSN_2024.01.csv")
# write.csv(year_month_aggregated, "Year-month_site_source_contribuion_CSN_2024.01.csv")
# write.csv(month_aggregated, "Month_site_source_contribuion_CSN_2024.01.csv")

# write.csv(Year_aggregated, paste0(data_use, "_Annual_source_2024.04.csv")) # "CSN_Site_15t1mdl0unc_Annual_source_2024.04.csv"
# write.csv(year_month_aggregated, paste0(data_use, "_Year-month_source_2024.04.csv")) # "CSN_Site_15t1mdl0unc_Year-month_source_2024.04.csv"
# write.csv(month_aggregated, paste0(data_use, "_Month_source_2024.04.csv")) # "CSN_Site_15t1mdl0unc_Month_source_2024.04.csv"

write.csv(Year_aggregated, paste0(data_use, "_Annual_source_2024.05.csv")) # "CSN_Site_15t1mdl0unc_Annual_source_2024.04.csv"
write.csv(year_month_aggregated, paste0(data_use, "_Year-month_source_2024.05.csv")) # "CSN_Site_15t1mdl0unc_Year-month_source_2024.04.csv"
write.csv(month_aggregated, paste0(data_use, "_Month_source_2024.05.csv")) # "CSN_Site_15t1mdl0unc_Month_source_2024.04.csv"

#### 4.2 mapping - annual & map ####

###### 4.2.0. data selection, choose one from data-1 & data-2 ######

color_npg = pal_npg("nrc")(10)

####### data-1, averages of the PMF outputs

# Year_aggregated = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/Annual_site_source_contribuion_CSN_2024.01.csv")
# month_aggregated = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/Month_site_source_contribuion_CSN_2024.01.csv")

# Year_aggregated = read.csv("CSN_Site_15t1mdl0unc_Annual_source_2024.04.csv")
# month_aggregated = read.csv("CSN_Site_15t1mdl0unc_Month_source_2024.04.csv")
Year_aggregated = read.csv("CSN_Site_15t1mdl0unc_Annual_source_2024.05.csv")
month_aggregated = read.csv("CSN_Site_15t1mdl0unc_Month_source_2024.05.csv")
year_month_aggregated = read.csv("CSN_Site_15t1mdl0unc_Year-month_source_2024.05.csv")

Year_aggregated$X = month_aggregated$X = year_month_aggregated$X = NULL

# extract 2011 vehicle data for Sherry Xiaorong
# vehicle_2011_year = 
#   subset(select(Year_aggregated, SiteCode, Year, Source_use, 
#                            Longitude, Latitude, 
#                            Concentration, conc_up, conc_down),
#          Year == 2011 & Source_use == "F1-Traffic")
# head(vehicle_2011_year); dim(vehicle_2011_year)
# write.csv(vehicle_2011_year, "CSN_2011_Vehicle_source_concentration.csv")

## remove other source not to be used
# Year_aggregated_use = subset(Year_aggregated,
#                              !(Source_use %in% c("F10-Industries_2", "Vehic_Biom")))
# 
# Month_aggregated_use = subset(month_aggregated,
#                               !(Source_use %in% c("F10-Industries_2", "Vehic_Biom")))
Year_aggregated_use = Year_aggregated
Month_aggregated_use = month_aggregated
                              
# site_no_source <- 
#   Year_aggregated_use %>%
#   group_by(Source_use) %>%
#   dplyr::summarise(Unique_SiteCode_No = n_distinct(SiteCode))

####### data-2, estimates from linear regression based on PMF outputs

# site_gps = read.csv("CSN_GPS.csv")
# source_contribution = read.csv("CSN_Site_source_contribution_overall_annual_month_2024.01.csv")
# source_contribution$X = NULL
# source_contribution = merge(source_contribution, site_gps)
# colnames(source_contribution)[3]
# source_contribution$Source_use = source_contribution$Source
# 
# source_contribution <- source_contribution %>%
#   mutate(Source_use = case_when(
#     Source == "F1_Vehicle"     ~ "F1-Traffic",
#     Source == "F2_SecNitrate"  ~ "F2-Secondary Nitrate",
#     Source == "F3_SecSulfate"  ~ "F3-Secondary Sulfate",
#     Source == "F4_AgedSeaSalt" ~ "F4-Aged Sea Salt",
#     Source == "F5_Industry" ~ "F5-Industry",
#     Source == "F6_FreshSeaSalt" ~ "F6-Fresh Sea Salt",
#     Source == "F7_NonTailpipe" ~ "F7-Non-Tailpipe",
#     Source == "F8_Biomass" ~ "F8-Biomass",
#     Source == "F9_SoilDust" ~ "F9-Soil/Dust",
#     TRUE                       ~ Source_use  # Keep existing value for other cases
#   ))
# 
# Year_aggregated_use = subset(source_contribution, 
#                              Period %in% 2011:2018)
# colnames(Year_aggregated_use)[ncol(Year_aggregated_use)-3] = "Year"
# Year_aggregated_use$Contribution = Year_aggregated_use$Source.concentration
# Year_aggregated_use$Year = as.integer(Year_aggregated_use$Year)
# 
# Month_aggregated_use = subset(source_contribution, 
#                               !(Period %in% 2011:2018 | Period == "overall"))
# Month_aggregated_use$Contribution = Month_aggregated_use$Source.concentration
# Month_aggregated_use$Month = 
#   as.integer(
#     format(
#       as.Date(
#         paste0(Month_aggregated_use$Period, "-01"), 
#         format = "%Y-%m-%d"), 
#       format = "%m"))
# 
# overall_contri = subset(source_contribution, Period == "overall")
# overall_contri$Contribution = overall_contri$Source.concentration
# 
# site_no_source <- 
#   Year_aggregated_use %>%
#   group_by(Source_use) %>%
#   dplyr::summarise(Unique_SiteCode_No = n_distinct(SiteCode))

###### 4.2.1. annual, month, box & trend plot ######

########### Overall, only for data-2 for now

# # histgram - each source
# 
# ggplot(subset(overall_contri, Contribution>0), 
#        # aes(x = Source.percent)) + # , fill = Source_use
#         aes(x = Contribution)) + # , fill = Source_use
#   geom_histogram(alpha = 0.6, bins = 30) +
#   facet_wrap(~ Source_use, ncol = 3) +
#   # labs(x = "Percent Contribution  %", y = "Count") +
#   labs(x = format_variable("Contribution  µg/m3"), y = "Count") +
#   theme_minimal() +
#   theme(legend.position = "none",
#         strip.background = element_blank(), 
#         strip.text = element_text(color="grey25", size = 15, vjust=0), # facet title
#         axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
#         axis.title.y = element_text(color="grey25", size = 18, vjust=1),
#         plot.margin = unit(c(2,1,2, 2), "lines"),
#         axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
#         axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))
# 
# overall_contri_sum = 
#   ddply(overall_contri, .(Source_use), summarise,
#         mean = round(mean(Contribution), 3), 
#         sd = round(sd(Contribution), 3), 
#         median = round(median(Contribution), 3), 
#         p99th = round(quantile(Contribution, 0.99), 3), 
#         p1th = round(quantile(Contribution, 0.01), 3))
# 
# export_table(overall_contri_sum, format = "md")
# 
# overall_percent_sum = 
#   ddply(overall_contri, .(Source_use), summarise,
#         mean = round(mean(Source.percent), 3), 
#         sd = round(sd(Source.percent), 3), 
#         median = round(median(Source.percent), 3), 
#         p99th = round(quantile(Source.percent, 0.99), 3), 
#         p1th = round(quantile(Source.percent, 0.01), 3))
# 
# export_table(overall_percent_sum, format = "md")

########### Annual
# the median and 99% of data
Year_aggregated_summary = 
  ddply(Year_aggregated_use, 
        .(Source_use, Year),
        summarise,
        conc_Lower = quantile(Concentration, 0.0025, na.rm = T),
        conc_Median = median(Concentration, na.rm = T),
        conc_Upper = quantile(Concentration, 0.9975, na.rm = T),
        perc_Lower = quantile(Percent, 0.0025, na.rm = T),
        perc_Median = median(Percent, na.rm = T),
        perc_Upper = quantile(Percent, 0.9975, na.rm = T))
Year_aggregated_summary$Year = as.integer(Year_aggregated_summary$Year)

# calculate the occurrence of each site in each source
year_month_site = select(Year_aggregated_use, SiteCode, Source_use)
year_month_site = year_month_site[!(duplicated(year_month_site)), ]
ym_site_count = data.frame(table(year_month_site$Source_use))
names(ym_site_count)[1] = c("Source_use")
ym_site_count$ym_site_count = 
  paste0(ym_site_count$Source_use, 
         ", No. of sites: ", 
         ym_site_count$Freq)

# boxplot - annual
conc_annual <-
  ggplot(subset(Year_aggregated_use, 
                Concentration < quantile(Concentration, 0.999)),
         aes(as.factor(Year), Concentration),
         color = Source_use) +
  geom_jitter(aes(color = Source_use), 
              width = 0.18, alpha = 0.15, size = 1.5)+
  geom_boxplot(aes(color = Source_use), 
               outlier.shape = NA, fill = NA,
               linewidth = 0.6, width = 0.5) +
  geom_line(data = Year_aggregated_summary, 
            aes(x = as.factor(Year), y = conc_Median, group = Source_use), 
            color = "grey50", size = 0.5, linetype="dashed") +
  geom_point(data = Year_aggregated_summary, 
             aes(x = as.factor(Year), y = conc_Median), color = "grey50") + 
  facet_grid(Source_use ~.) + # , scales = "free_y"
  geom_vline(data = line_data, aes(xintercept = xintercept), 
             color = "black", linetype = "solid") +
  # scale_y_log10(limits = c(0.1, NA), breaks = c(0.1, 1, 5)) +
  scale_y_continuous(limits = c(0, NA), breaks = function(x) pretty(x, n = 3)) +
  scale_x_discrete(breaks = as.character(2011:2020)) +
  labs(x = "Year", y = format_variable("Concentration µg/m3")) +
  # scale_color_manual(values = c("red", "blue", "purple", "green", "orange", "cyan", "pink", "brown")) +
  scale_color_manual(values = color_source) +
  # c("red", "blue", "purple", "green", "orange", "cyan", "pink", "brown")
  geom_text(data = ym_site_count, size = 5.5,
            aes(x = "2020", y = 3.4, label = ym_site_count), 
            inherit.aes = FALSE, vjust = -0.2, hjust = 1) + # , fontface = "bold"
  # theme_minimal(base_size = 8) +
  theme_classic(base_size = 8) +
  theme(panel.grid = element_line(colour = "white"),
        strip.background = element_blank(), strip.text = element_blank(), # facet title
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(4, "mm"),   
        legend.position = "none",
        axis.title.x = element_text(size = 20, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(size = 20, vjust=2, margin=margin(0,2,0,0)),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1, vjust = 1), 
        axis.text.y = element_text(size = 18, angle = 0, hjust = 0.5))


frac_annual <-
  ggplot(subset(Year_aggregated_use, 
                Percent < quantile(Percent, 0.999)),
         aes(as.factor(Year), Percent),
         color = Source_use) +
  geom_boxplot(aes(color = Source_use), 
               outlier.shape = NA, fill = NA,
               linewidth = 0.6, width = 0.5) +
  geom_jitter(aes(color = Source_use), 
              width = 0.18, alpha = 0.15)+
  geom_line(data = Year_aggregated_summary, 
            aes(x = as.factor(Year), y = perc_Median, 
                group = Source_use), 
            color = "grey50", size = 0.5, linetype="dashed") +
  geom_point(data = Year_aggregated_summary, 
             aes(x = as.factor(Year), y = perc_Median), 
             color = "grey50") + # , color = Source_use, fill = Source_use
  facet_grid(Source_use ~.) + # , scales = "free_y"
  # ylim(0,4) +
  scale_y_continuous(breaks = c(0, 25, 50)) +
  # scale_y_continuous(limits = c(0, NA), breaks = function(x) pretty(x, n = 2)) +
  # scale_x_discrete(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
  labs(x = "Year", y = format_variable("Percent %")) +
  scale_color_manual(values = color_source) + # color_npg[-c(1, 5, 7)]
  geom_text(data = ym_site_count, size = 5.5,
            aes(x = "2020", y = 42, label = ym_site_count), 
            inherit.aes = FALSE, vjust = -0.2, hjust = 1) + # , fontface = "bold"
  # theme_minimal(base_size = 8) +
  theme_classic(base_size = 6) +
  theme(panel.grid = element_line(colour = "white"),
        strip.background = element_blank(), strip.text = element_blank(), # facet title
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(4, "mm"),   
        legend.position = "none",
        axis.title.x = element_text(size = 20, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(size = 20, vjust=2, margin=margin(0,2,0,0)),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1, vjust = 1), 
        axis.text.y = element_text(size = 18, angle = 0, hjust = 0.5))

annual_change = conc_annual + frac_annual
annual_change

# histgram - each source

ggplot(subset(Year_aggregated_use, 
              Concentration > 0), 
       aes(x = Concentration)) + # , fill = Source_use
  geom_histogram(alpha = 0.6, bins = 30) +
  facet_wrap(~ Source_use, ncol = 3) +
  labs(x = format_variable("Concentration  µg/m3"), y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 15, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))
  
annual_Concentration_source = 
  ddply(Year_aggregated_use, .(Source_use), summarise,
        mean = round(mean(Concentration), 3), 
        sd = round(sd(Concentration), 3), 
        median = round(median(Concentration), 3), 
        p975th = round(quantile(Concentration, 0.975), 3), 
        p025th = round(quantile(Concentration, 0.025), 3))

export_table(annual_Concentration_source, format = "md")
  
  
# ribbon range figure
ggplot(Year_aggregated_summary, 
       aes(x = Year, y = conc_Median, color = Source_use, fill = Source_use)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(2011, 2014, 2017)) + 
  #scale_y_continuous(limits = c(0, 4), breaks = c(0, 2, 4)) +  # Set y-axis limits and breaks
  labs(x = "Year", y = format_variable("Concentration µg/m3")) +
  scale_color_manual(values = color_npg) +
  geom_ribbon(aes(ymin = conc_Lower, ymax = conc_Upper), alpha = 0.2, color = NA) +
  facet_wrap(~ Source_use, ncol = 3) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 18, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5))

ggplot(Year_aggregated_summary, 
       aes(x = Year, y = perc_Median, color = Source_use, fill = Source_use)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(2011, 2014, 2017)) + 
  #scale_y_continuous(limits = c(0, 4), breaks = c(0, 2, 4)) +  # Set y-axis limits and breaks
  labs(x = "Year", y = format_variable("Percent %")) +
  scale_color_manual(values = color_npg) +
  geom_ribbon(aes(ymin = perc_Lower, ymax = perc_Upper), alpha = 0.2, color = NA) +
  facet_wrap(~ Source_use, ncol = 3) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 18, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5))

########### Monthly
# the median and 99% of data
Month_aggregated_summary = 
  ddply(Month_aggregated_use, 
        .(Source_use, Month),
        summarise,
        conc_Lower = quantile(Concentration, 0.0025, na.rm = T),
        conc_Median = median(Concentration, na.rm = T),
        conc_Upper = quantile(Concentration, 0.9975, na.rm = T),
        perc_Lower = quantile(Percent, 0.0025, na.rm = T),
        perc_Median = median(Percent, na.rm = T),
        perc_Upper = quantile(Percent, 0.9975, na.rm = T))
Month_aggregated_summary$Month = as.integer(Month_aggregated_summary$Month)

month_conc <-
  ggplot(Month_aggregated_use,
       aes(as.factor(Month), Concentration),
       color = Source_use) +
  geom_jitter(aes(color = Source_use), 
              width = 0.18, alpha = 0.15)+
  geom_boxplot(aes(color = Source_use), 
               outlier.shape = NA, fill = NA,
               linewidth = 0.55, width = 0.5) +
  geom_line(data = Month_aggregated_summary, 
            aes(x = as.factor(Month), y = conc_Median, 
                group = Source_use), 
            color = "grey50", size = 0.5, linetype="dashed") +
  geom_point(data = Month_aggregated_summary, 
             aes(x = as.factor(Month), y = conc_Median), 
             color = "grey50") + # , color = Source_use, fill = Source_use
  facet_grid(Source_use ~., scales = "free_y") + # , scales = "free_y"
  scale_y_continuous(limits = c(0, NA), 
                     breaks = function(x) pretty(x, n = 3)) +
  labs(x = "Month", y = format_variable("Concentration µg/m3")) +
  scale_color_manual(values = color_source) + # color_npg[-c(1, 5, 7)]
  geom_text(data = ym_site_count, size = 5.5,
            aes(x = "12", y = 8.5, label = ym_site_count), 
            inherit.aes = FALSE, vjust = -0.2, hjust = 1) + # , fontface = "bold"
  theme_classic(base_size = 6) +
  theme(panel.grid = element_line(colour = "white"),
        strip.background = element_blank(), strip.text = element_blank(), # facet title
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(4, "mm"),   
        legend.position = "none",
        axis.title.x = element_text(size = 20, vjust=-1, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(size = 20, vjust=2, margin=margin(0,2,0,0)),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = -1), 
        axis.text.y = element_text(size = 18, angle = 0, hjust = 0.5))


month_frac <-
  ggplot(Month_aggregated_use,
         aes(as.factor(Month), Percent),
         color = Source_use) +
  geom_jitter(aes(color = Source_use), 
              width = 0.18, alpha = 0.15)+
  geom_boxplot(aes(color = Source_use), 
               outlier.shape = NA, fill = NA,
               linewidth = 0.55, width = 0.5) +
  geom_point(data = Month_aggregated_summary, 
             aes(x = as.factor(Month), y = perc_Median), 
             color = "grey50") + # , color = Source_use, fill = Source_use
  geom_line(data = Month_aggregated_summary, 
            aes(x = as.factor(Month), y = perc_Median, 
                group = Source_use), 
            color = "grey50", size = 0.5, linetype="dashed") +
  facet_grid(Source_use ~., scales = "free_y") + # , scales = "free_y"
  # scale_y_continuous(breaks = c(0, 30, 60)) +
  scale_y_continuous(limits = c(0, NA), breaks = function(x) pretty(x, n = 3)) +
  labs(x = "Month", y = format_variable("Percent %")) +
  scale_color_manual(values = color_source) + # color_npg[-c(1, 5, 7)]
  geom_text(data = ym_site_count, size = 5.5,
            aes(x = "12", y = 50, label = ym_site_count), 
            inherit.aes = FALSE, vjust = -0.2, hjust = 1) + # , fontface = "bold"
  theme_classic(base_size = 6) +
  theme(panel.grid = element_line(colour = "white"),
        strip.background = element_blank(), strip.text = element_blank(), # facet title
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(4, "mm"),   
        legend.position = "none",
        axis.title.x = element_text(size = 20, vjust=-1, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(size = 20, vjust=2, margin=margin(0,2,0,0)),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = -1), 
        axis.text.y = element_text(size = 18, angle = 0, hjust = 0.5))

month_change = month_conc + month_frac
month_change

# histgram - each source

ggplot(subset(Month_aggregated_use), 
       aes(x = Concentration)) + # , fill = Source_use
  geom_histogram(alpha = 0.6, bins = 30) +
  facet_wrap(~ Source_use, ncol = 3) +
  labs(x = format_variable("Concentration  µg/m3"), y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 15, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))

month_Concentration_source = 
  ddply(Month_aggregated_use, .(Source_use), summarise,
        mean = round(mean(Concentration, na.rm = T), 3), 
        sd = round(sd(Concentration, na.rm = T), 3), 
        median = round(median(Concentration, na.rm = T), 3), 
        p99th = round(quantile(Concentration, 0.99, na.rm = T), 3), 
        p1th = round(quantile(Concentration, 0.01, na.rm = T), 3))

export_table(month_Concentration_source, format = "md")

# ribbon range figure

Month_aggregated_summary = 
  ddply(Month_aggregated_use, 
        .(Source_use, Month),
        summarise,
        conc_Lower = quantile(Concentration, 0.0025, na.rm = T),
        conc_Median = median(Concentration, na.rm = T),
        conc_Upper = quantile(Concentration, 0.9975, na.rm = T),
        perc_Lower = quantile(Percent, 0.0025, na.rm = T),
        perc_Median = median(Percent, na.rm = T),
        perc_Upper = quantile(Percent, 0.9975, na.rm = T))


ggplot(Month_aggregated_summary, 
       aes(x = Month, y = conc_Median, color = Source_use, fill = Source_use)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) + 
  #scale_y_continuous(limits = c(0, 4), breaks = c(0, 2, 4)) +  # Set y-axis limits and breaks
  labs(x = "Month", y = format_variable("Concentration µg/m3")) +
  scale_color_manual(values = color_npg) +
  geom_ribbon(aes(ymin = conc_Lower, ymax = conc_Upper), alpha = 0.2, color = NA) +
  facet_wrap(~ Source_use, ncol = 3) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 18, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5))

ggplot(Month_aggregated_summary, 
       aes(x = Month, y = perc_Median, color = Source_use, fill = Source_use)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) + 
  #scale_y_continuous(limits = c(0, 4), breaks = c(0, 2, 4)) +  # Set y-axis limits and breaks
  labs(x = "Month", y = format_variable("percentration %")) +
  scale_color_manual(values = color_npg) +
  geom_ribbon(aes(ymin = perc_Lower, ymax = perc_Upper), alpha = 0.2, color = NA) +
  facet_wrap(~ Source_use, ncol = 3) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 18, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5))

###### 4.2.1.2. annual, month, accumulated lines ######

# extract gps info
site_long_lat = 
  ddply(Month_aggregated_use, .(SiteCode),
        summarise,
        Longitude = mean(Longitude),
        Latitude = mean(Latitude))

# recorder by longitude
site_long_lat = 
  site_long_lat[with(site_long_lat, order(Longitude)), ]
site_long_lat$long_sequence = 1:nrow(site_long_lat)
# site_long_lat$Longitude = NULL

# merge with montly and annual data
Month_aggregated_use = merge(Month_aggregated_use, site_long_lat)
Year_aggregated_use = merge(Year_aggregated_use, site_long_lat)
year_month_use = merge(year_month_aggregated, site_long_lat)
year_month_conc = select(year_month_use, 
                         long_sequence, Source_use,
                         Year, Month, Concentration)

# ddentify the month when Concentration peaks for each Source_use at each site (long_sequence) in each year
peak_concentration_months <- 
  year_month_conc %>%
  group_by(long_sequence, Source_use, Year) %>%
  dplyr::summarise(Peak_Month = Month[which.max(Concentration)],
            Peak_Concentration = max(Concentration),
            .groups = "drop")

# assign season
peak_concentration_months$Season = 
  ifelse(peak_concentration_months$Peak_Month %in% c(12, 1, 2), "Winter",
         ifelse(peak_concentration_months$Peak_Month %in% c(3, 4, 5), "Spring",
                ifelse(peak_concentration_months$Peak_Month %in% c(6, 7, 8), "Summer", 
                       "Autumn")))

# count the number of occurrences of each season for each source of each site
season_site_counts <- 
  peak_concentration_months %>%
  group_by(Source_use, long_sequence, Season) %>%
  dplyr::summarise(Count = n(),
                   .groups = "drop")

season_site_peak <-
  season_site_counts %>%
  group_by(long_sequence, Source_use) %>%
  dplyr::summarise(Peak_Season = Season[which.max(Count)],
                   Peak_Season_count = Count[which.max(Count)],
                   .groups = "drop")

season_site_peak = merge(site_long_lat, season_site_peak)


ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = season_site_peak, 
             aes(x = Longitude, y = Latitude, 
                 fill = Peak_Season),
             size = 2.5, alpha = 0.6, 
             shape = 21, color = "grey66") +
  # guides(color=guide_legend(title="Slope: µg/m3")) + 
  coord_sf(datum = NA) +
  facet_wrap(~ Source_use, ncol = 3) +
  scale_fill_jama() +
  #facet_wrap(~ source_site_count, labeller = labeller(source_site_count = custom_labeller)) +
  # labs(fill="diff_slope /n (µg/m^3/year)") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10), 
        legend.text = element_text(size = 14), 
        legend.position = c(0.84, 0.2), 
        # legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 0)) 

# count the number of occurrences of each season for each source
season_counts <- 
  peak_concentration_months %>%
  group_by(Source_use, Season) %>%
  dplyr::summarise(Count = n(),
            .groups = "drop")

ggplot(season_counts, 
       aes(x = Source_use, y = Season, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color="grey25", size = 12, vjust=0), # facet title
    axis.title.x = element_text(color="grey25", size = 14, vjust=0),
    axis.title.y = element_text(color="grey25", size = 14, vjust=1),
    axis.text.x = element_text(color="grey25", size = 12, angle = 90, hjust = 0.5, vjust = 0.2),
    axis.text.y = element_text(color="grey25", size = 12, angle = 0, hjust = 0.5))

# count the number of occurrences of each month for each source
month_counts <- 
  peak_concentration_months %>%
  group_by(Source_use, Peak_Month) %>%
  dplyr::summarise(Count = n(),
                   .groups = "drop")

ggplot(month_counts, 
       aes(x = Source_use, y = Peak_Month, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  scale_y_continuous(breaks = 1:12) +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color="grey25", size = 12, vjust=0), # facet title
    axis.title.x = element_text(color="grey25", size = 14, vjust=0),
    axis.title.y = element_text(color="grey25", size = 14, vjust=1),
    axis.text.x = element_text(color="grey25", size = 12, angle = 90, hjust = 0.5, vjust = 0.2),
    axis.text.y = element_text(color="grey25", size = 12, angle = 0, hjust = 0.5))




ggplot(peak_concentration_months, 
       aes(x = Source_use, y = Season, fill = Peak_Concentration)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  facet_wrap(. ~ Year, ncol = 2) +
  labs(title = "Peak Concentration Heatmap",
       x = "Source Use",
       y = "Season",
       fill = "Peak Concentration") +
  theme_minimal() +
  theme(# legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color="grey25", size = 12, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 14, vjust=0),
        axis.title.y = element_text(color="grey25", size = 14, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 12, angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(color="grey25", size = 12, angle = 0, hjust = 0.5))




# Calculate annual total concentration for each site
annual_totals <-
  year_month_conc %>%
  dplyr::group_by(long_sequence, Year) %>%
  dplyr::summarize(
    total_concentration = sum(Concentration, na.rm = TRUE),
    .groups = "drop")

# Calculate annual fraction contribution for each source at each site
annual_fraction <- year_month_conc %>%
  group_by(long_sequence, Year, Source_use) %>%
  summarize(source_concentration = sum(Concentration, na.rm = TRUE)) %>%
  left_join(annual_totals, by = c("long_sequence", "Year")) %>%
  mutate(fraction_contribution = source_concentration / total_concentration) %>%
  select(long_sequence, Year, Source_use, fraction_contribution)

theme_year_month_accu = 
  theme_minimal() +
  theme(# legend.position = "none",
    strip.background = element_blank(), strip.text = element_blank(), # no title for facets
    axis.title.x = element_text(color="grey25", size = 20, vjust=0), 
    axis.title.y = element_text(color="grey25", size = 20, vjust=1),
    plot.margin = unit(c(2,1,2, 2), "lines"),
    axis.text.x = element_text(color="grey25", size = 16, angle = 0, hjust = 0.5, vjust = 0.5), 
    axis.text.y = element_text(color="grey25", size = 16, angle = 0, hjust = 0.5))
  
### plot 1, accumulated Montly contribution of all sources
month_position = 
  data.frame (Month = 1:12, x_position = 63, 
              y_conc_position = 20, y_perc_position = 110)

year_position = 
  data.frame (Year = 2011:2020, x_position = 63, 
              y_conc_position = 12, y_perc_position = 110)

ggplot(Month_aggregated_use, 
       aes(x = long_sequence, y = Concentration, 
           color = Source_use)) +
  geom_bar(stat = "identity", width = 0.15) +
  facet_wrap(~ Month, ncol = 3) +
  scale_color_npg() +
  scale_x_continuous(limits = c(0, NA), 
                     breaks = c(6.7, 25.6, 39.8, 61.5, 88.2, 109.2),
                     labels = c(-120, -100, -90, -85, -80, -75)) +
  labs(x = "Longitude", 
       y = format_variable("Concentration µg/m3"), fill = "Source Use") +
  geom_text(data = month_position, size = 6,
            aes(x = x_position, y = y_conc_position, label = Month), 
            inherit.aes = FALSE) +
  theme_year_month_accu

ggplot(Month_aggregated_use, 
       aes(x = long_sequence, y = Percent, 
           color = Source_use)) +
  geom_bar(stat = "identity", width = 0.15) +
  facet_wrap(~ Month, ncol = 3) +
  scale_color_npg() +
  scale_x_continuous(limits = c(0, NA), 
                     breaks = c(6.7, 25.6, 39.8, 61.5, 88.2, 109.2),
                     labels = c(-120, -100, -90, -85, -80, -75)) +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = c(25, 50, 75, 100)) +
  labs(x = "Longitude", y = "Percent %", fill = "Source Use") +
  geom_text(data = month_position, size = 6,
            aes(x = x_position, y = y_perc_position, label = Month), 
            inherit.aes = FALSE) +
  theme_year_month_accu


ggplot(Year_aggregated_use, 
       aes(x = long_sequence, y = Concentration, 
           color = Source_use)) +
  geom_bar(stat = "identity", width = 0.15) +
  facet_wrap(~ Year, ncol = 2) +
  scale_color_npg() +
  scale_x_continuous(limits = c(0, NA), 
                     breaks = c(6.7, 25.6, 39.8, 61.5, 88.2, 109.2),
                     labels = c(-120, -100, -90, -85, -80, -75)) +
  labs(x = "Longitude", 
       y = format_variable("Concentration µg/m3"), fill = "Source Use") +
  geom_text(data = year_position, size = 6,
            aes(x = x_position, y = y_conc_position, label = Year), 
            inherit.aes = FALSE) +
  theme_year_month_accu

ggplot(Year_aggregated_use, 
       aes(x = long_sequence, y = Percent, 
           color = Source_use)) +
  geom_bar(stat = "identity", width = 0.15) +
  facet_wrap(~ Year, ncol = 2) +
  scale_color_npg() +
  scale_x_continuous(limits = c(0, NA), 
                     breaks = c(6.7, 25.6, 39.8, 61.5, 88.2, 109.2),
                     labels = c(-120, -100, -90, -85, -80, -75)) +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = c(25, 50, 75, 100)) +
  labs(x = "Longitude", y = "Percent %", fill = "Source Use") +
  geom_text(data = year_position, size = 6,
            aes(x = x_position, y = y_perc_position, label = Year), 
            inherit.aes = FALSE) +
  theme_year_month_accu


ggplot(Year_aggregated_use, 
       aes(x = long_sequence, y = Concentration)) +
  geom_bar(stat = "identity", width = 0.15) +
  # facet_wrap(Source_use ~ Year, ncol = 10) +
  facet_grid(Source_use ~ Year) +
  scale_color_npg() +
  geom_errorbar(aes(ymin = conc_down, ymax = conc_up),
                width = 0.15) +
  scale_x_continuous(limits = c(0, NA), 
                     breaks = c(6.7, 25.6, 39.8, 61.5, 88.2, 109.2),
                     labels = c(-120, -100, -90, -85, -80, -75)) +
  labs(x = "Longitude", 
       y = format_variable("Concentration µg/m3"), fill = "Source Use") +
  geom_text(data = year_position, size = 3,
            aes(x = x_position, y = y_conc_position, label = Year),
            inherit.aes = FALSE) +
  theme_minimal() +
  theme(# legend.position = "none",
    panel.spacing.x = unit(0, "cm"), # minimize the space between facets
    strip.background = element_blank(), strip.text = element_blank(), # no title for facets
    axis.title.x = element_text(color="grey25", size = 14, vjust=0), 
    axis.title.y = element_text(color="grey25", size = 14, vjust=1),
    plot.margin = unit(c(2,1,2, 2), "lines"),
    axis.text.x = element_text(color="grey25", size = 12, angle = 90, hjust = 0.5, vjust = 0.5), 
    axis.text.y = element_text(color="grey25", size = 12, angle = 0, hjust = 0.5))

ggplot(subset(Year_aggregated_use, Source_use == "F1-Traffic"), 
       aes(x = long_sequence, y = Concentration)) +
  geom_bar(stat = "identity", width = 0.15) +
  # facet_wrap(Source_use ~ Year, ncol = 2) +
  scale_color_npg() +
  geom_errorbar(aes(ymin = conc_down, ymax = conc_up),
                width = 0.15) +
  scale_x_continuous(limits = c(0, NA), 
                     breaks = c(6.7, 25.6, 39.8, 61.5, 88.2, 109.2),
                     labels = c(-120, -100, -90, -85, -80, -75)) +
  labs(x = "Longitude", 
       y = format_variable("Concentration µg/m3"), fill = "Source Use") +
  geom_text(data = year_position, size = 6,
            aes(x = x_position, y = y_conc_position, label = Year), 
            inherit.aes = FALSE) +
  theme_year_month_accu


conc_range = c(quantile(Month_aggregated_use$Concentration, 0.05), 
               quantile(Month_aggregated_use$Concentration, 0.95))
conc_range =  c(1, 130); midpoint = 65

Month_aggregated_use_order =
  Month_aggregated_use[with(Month_aggregated_use, 
                            order(Source_use, Concentration)), ]

ggplot(Month_aggregated_use_order, 
       aes(x = Month, y = Concentration, 
           color = long_sequence)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_color_gradient2(limits = conc_range,
                       low = "paleturquoise1",  
                       mid = "deepskyblue1",  
                       high = "deepskyblue4", 
                       midpoint = midpoint,
                       oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  scale_x_continuous(breaks = c(1, 4, 7, 10), labels = c(1, 4, 7, 10)) +
  facet_wrap(~ Source_use, ncol = 3) +
  xlim(1,12) +
  labs(x = "Month", 
       y = format_variable("Concentration µg/m3"), fill = "Source Use") +
  theme_minimal() +
  theme(# legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(color="grey25", size = 20), 
    axis.title.x = element_text(color="grey25", size = 20, vjust=0), 
    axis.title.y = element_text(color="grey25", size = 20, vjust=1),
    plot.margin = unit(c(2,1,2, 2), "lines"),
    axis.text.x = element_text(color="grey25", size = 16, angle = 0, hjust = 0.5, vjust = 0.5), 
    axis.text.y = element_text(color="grey25", size = 16, angle = 0, hjust = 0.5))


ggplot(Month_aggregated_use, 
       aes(x = long_sequence, y = Concentration, 
           color = Concentration)) +
  geom_bar(stat = "identity", width = 0.1) +
  facet_wrap(~ Source_use, ncol = 1) +
  # scale_color_npg() +
  geom_text(data = month_position, size = 6,
            aes(x = x_position, y = y_conc_position, label = Month), 
            inherit.aes = FALSE) +
  theme_year_month_accu






###### 4.2.2. Spatial distribution of Changes between 2011 & 2020 - Absolute difference ######

# # Load the dplyr package
# library(dplyr)
# 
# # Filter the dataset for the years 2011 and 2019
# annual_source_selectd <- Year_aggregated %>% 
#   filter(Year %in% c(2011, 2019))
# 
# Concentration_diff <- 
#   annual_source_selectd %>%
#   dplyr::group_by(Source_use, SiteCode) %>%
#   dplyr::summarise(
#     diff_Concentration = ifelse(n() > 1, diff(Concentration), NA),
#     Longitude = last(Longitude),
#     Latitude = last(Latitude),
#     # geoid = last(geoid),
#     # state_abbr = last(state_abbr),
#     # geometry = last(geometry),
#     .groups = 'drop'  # This will automatically ungroup the data
#   )
# 
# 
# Concentration_diff = subset(Concentration_diff, 
#                            !(Source_use %in% c("F10-Industries_2", "Vehic_Biom")))
# 
# # Create the plot
# ggplot() +
#   geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
#   geom_point(data = subset(source9_diff_map, 
#                            !is.na(diff_Concentration)), 
#              aes(x = Longitude, y = Latitude, 
#                  color = diff_Concentration),
#              size = 2.5, alpha = 0.8) +
#   scale_color_gradient2(low = color_npg[2], 
#                         high = color_npg[8], 
#                         midpoint = 0) +
#   coord_sf(datum = NA) +
#   facet_wrap(~ Source_use, 
#              labeller = labeller(Source_use = 
#                                    as_labeller(as.character, 
#                                                default = label_value))) +
#   theme_minimal() +
#   theme(panel.background = element_blank(),
#         strip.text = element_text(color = "black", size = 16))


###### 4.2.3.1 Spatial distribution of Changes between 2011 & 2020 - data preparation - Slope ######

Year_aggregated_use_unique = data.frame(table(Year_aggregated_use$SiteCode, 
                                              Year_aggregated_use$Source_use))
names(Year_aggregated_use_unique)[1:2] = c("SiteCode", "Source_use")
head(Year_aggregated_use_unique)

length(unique(Year_aggregated_use_unique$SiteCode))
Year_aggregated_use$SiteCode = as.factor(Year_aggregated_use$SiteCode)

### Linear Regression, at least 2 groups of data
Year_exclude_2lm = subset(Year_aggregated_use_unique, Freq < 2)
row.names(Year_exclude_2lm)=NULL

length(unique(Year_exclude_2lm$SiteCode))
head(Year_exclude_2lm); summary(Year_exclude_2lm)

# exclude sites with <2 groups of data
Year_aggregated_lm = anti_join(Year_aggregated_use, 
                                      Year_exclude_2lm, 
                                      by = c("SiteCode", "Source_use"))
sapply(Year_aggregated_lm, class)

### Linear Regression, estimate the slope for each site across the study period 
# concentration-based slope
slope_diff_lm <- 
  Year_aggregated_lm %>%
  group_by(SiteCode, Source_use, Longitude, Latitude) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_lm(cur_data(), "Year", "Concentration"),
    .groups = "drop"
  ) %>%
  ungroup()

# percent-based slope
slope_diff_perc_lm <- 
  Year_aggregated_lm %>%
  group_by(SiteCode, Source_use, Longitude, Latitude) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_lm(cur_data(), "Year", "Percent"),
    .groups = "drop"
  ) %>%
  ungroup()

### Thiel-Sen Regression, at least 3 groups of data
Year_exclude_3ts = subset(Year_aggregated_use_unique, Freq < 3)
row.names(Year_exclude_3ts)=NULL

length(unique(Year_exclude_3ts$SiteCode))
head(Year_exclude_3ts); summary(Year_exclude_3ts)
# Year_exclude_3ts$SiteCode = as.integer(Year_exclude_3ts$SiteCode)


# exclude sites with <3 groups of data
Year_aggregated_ts = anti_join(Year_aggregated_use, 
                                      Year_exclude_3ts, 
                                      by = c("SiteCode", "Source_use"))
sapply(Year_aggregated_ts, class)

# check if there is two-line data
# subset(Year_aggregated_ts, SiteCode == 180190010 & Source_use == "F1-Traffic")
# subset(Year_aggregated_use_unique, SiteCode == 180190010 & Source_use == "F1-Traffic")
# subset(Year_aggregated_use, SiteCode == 180190010 & Source_use == "F1-Traffic")

### Thiel-Sen, estimate the slope for each site across the study period 
# concentration-based slope
slope_diff_ts <- 
  Year_aggregated_ts %>%
  group_by(SiteCode, Source_use, Longitude, Latitude) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(cur_data(), "Year", "Concentration"),
    .groups = "drop"
  ) %>%
  ungroup()

# percent-based slope
slope_diff_perc_ts <- 
  Year_aggregated_ts %>%
  group_by(SiteCode, Source_use, Longitude, Latitude) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(cur_data(), "Year", "Percent"),
    .groups = "drop"
  ) %>%
  ungroup()

write.csv(slope_diff_ts, paste0(data_use, "_slopes_Theil-Sen_concentraion_year.csv"))
write.csv(slope_diff_perc_ts, paste0(data_use, "_slopes_Theil-Sen_percent_year.csv"))
write.csv(slope_diff_lm, paste0(data_use, "_slopes_Linear_concentraion_year.csv"))
write.csv(slope_diff_perc_lm, paste0(data_use, "_slopes_Linear_percent_year.csv"))

###### decide whether using Thiel-Sen or lm regression for plotting
### Thiel-Sen regression
slope_diff = slope_diff_ts
slope_diff_perc = slope_diff_perc_ts

### linear regression
slope_diff = slope_diff_lm
slope_diff_perc = slope_diff_perc_lm


# remove potential NA
slope_diff = na.omit(slope_diff)
slope_diff_perc = na.omit(slope_diff_perc)

# calculate the occurrence of each site in each source
source_site_count = as.data.frame(t(table(slope_diff$Source_use)))
source_site_count$Var1 = NULL
colnames(source_site_count)[1] = c("Source_use")
source_site_count$source_site_count = 
  paste0(source_site_count$Source_use, 
         "\nNo. of Sites: ", 
         source_site_count$Freq)

# merge slope diff with site count of each source
slope_diff = merge(slope_diff, source_site_count)
slope_diff_perc = merge(slope_diff_perc, source_site_count)

### check the distribution of slopes from regression
# slopes from concentration
slope_diff_source = 
  ddply(slope_diff, .(Source_use), summarise,
        slope_mean = round(mean(diff_slope), 3), 
        slope_sd = round(sd(diff_slope), 3), 
        slope_median = round(median(diff_slope), 3), 
        slope_975th = round(quantile(diff_slope, 0.975), 3), 
        slope_025th = round(quantile(diff_slope, 0.025), 3))

export_table(slope_diff_source, format = "text")

# slopes from percent
slope_diff_perc_source = 
  ddply(slope_diff_perc, .(Source_use), summarise,
        slope_mean = round(mean(diff_slope), 3), 
        slope_sd = round(sd(diff_slope), 3), 
        slope_median = round(median(diff_slope), 3), 
        slope_975th = round(quantile(diff_slope, 0.975), 3), 
        slope_025th = round(quantile(diff_slope, 0.025), 3))

export_table(slope_diff_perc_source, format = "text")

summary(slope_diff_perc$diff_slope)

###### 4.2.3.2 Spatial distribution of Changes between 2011 & 2020 - Concentration - Slope ######

# detect those with <2 data groups, a regression needs at least 2 groups of data.

ggplot() +
  geom_histogram(data = subset(slope_diff, Source_use == "F3-Secondary Sulfate"), 
                 aes(x = diff_slope)) +
  theme_minimal()

# Create the plot
# slope_range_conc <- quantile(slope_diff$diff_slope, c(0.0025, 0.9975))
# slope_range_conc <- c(-0.6, 0.6)
# slope_range_conc <- c(-0.4, 0.4)
slope_range_conc <- c(-0.2, 0.2)

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = slope_diff, 
             aes(x = Longitude, y = Latitude, 
                 fill = diff_slope),
             size = 2.5, alpha = 0.8, 
             shape = 21, color = "grey66") +
  scale_fill_gradient2(limits = slope_range_conc,
                        low = "#2CA02C",  
                        mid = "ivory",  
                        high = "#D62728", 
                        midpoint = 0,
                        oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  # guides(color=guide_legend(title="Slope: µg/m3")) + 
  coord_sf(datum = NA) +
  facet_wrap(~ source_site_count, 
            labeller = labeller(source_site_count = 
                                   as_labeller(as.character, 
                                              default = label_value))) +
  #facet_wrap(~ source_site_count, labeller = labeller(source_site_count = custom_labeller)) +
  # addline_space, add a break in the text
  labs(fill=addline_space(paste("Changing_rate",  
                                 format_variable("(µg/m3/year)")))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10),
        axis.title = element_text(size = 0),
        legend.position = c(0.85, 0.18),
        legend.text = element_text(size = 14), 
        legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
        legend.spacing.y = unit(1, "cm")) 


# Vehicle special
slope_range_conc <- c(-0.2, 0.2)

ggplot() +
  geom_sf(data = us_states, 
          fill = "grey96", alpha = 0.8,
          color = "grey55", linewidth = 0.6) +
  geom_point(data = subset(slope_diff, Source_use == "F1-Traffic"), 
             aes(x = Longitude, y = Latitude, 
                 fill = diff_slope),
             size = 5, alpha = 0.8, shape = 21) +
  scale_fill_gradient2(limits = slope_range_conc,
                        low = "#2CA02C",  
                        mid = "ivory",  
                        high = "#D62728", 
                        midpoint = 0,
                        oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  # guides(color=guide_legend(title="Slope: µg/m3")) + 
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10), 
        legend.text = element_text(size = 14), 
        # legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 0))


###### 4.2.3.3 Spatial distribution of Changes between 2011 & 2020 - Percent - Slope ######


ggplot() +
  geom_histogram(data = subset(slope_diff_perc, Source_use == "F3-Secondary Sulfate"), 
                 aes(x = diff_slope)) +
  theme_minimal()


# Create the plot
# slope_range_perc <- quantile(slope_diff_perc$diff_slope, c(0.0025, 0.9975))
# slope_range_perc <- c(-0.6, 0.6)
slope_range_perc <- c(-0.4, 0.4)
# slope_range_perc <- c(-0.2, 0.2)

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = slope_diff_perc, 
             aes(x = Longitude, y = Latitude, 
                 fill = diff_slope),
             size = 2.5, alpha = 0.8, 
             shape = 21, color = "grey66") +
  scale_fill_gradient2(limits = slope_range_perc,
                       low = "#2CA02C",  
                       mid = "ivory",  
                       high = "#D62728", 
                       midpoint = 0,
                       oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  # guides(color=guide_legend(title="Slope: µg/m3")) + 
  coord_sf(datum = NA) +
  facet_wrap(~ source_site_count, 
             labeller = labeller(source_site_count = 
                                   as_labeller(as.character, 
                                               default = label_value))) +
  #facet_wrap(~ source_site_count, labeller = labeller(source_site_count = custom_labeller)) +
  labs(fill=addline_space(paste("Changing_rate",  
                                 format_variable("(%/year)"), ""))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10),
        axis.title = element_text(size = 0),
        legend.position = c(0.85, 0.18),
        legend.text = element_text(size = 14), 
        legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
        legend.spacing.y = unit(1, "cm")) 

###### 4.2.3.4 Spatial distribution of Changes between 2011 & 2020 - Linear lm vs. Theil-Sen - Slope ######

# merge slopes based on concentrations from the two method
slope_diff_conc_lm_ts = slope_diff_ts
names(slope_diff_conc_lm_ts)[ncol(slope_diff_conc_lm_ts)] = "conc_slope_ts"

slope_diff_conc_lm_ts = 
  merge(slope_diff_conc_lm_ts, 
        slope_diff_lm, all.x = TRUE)
dim(slope_diff_conc_lm_ts)
names(slope_diff_conc_lm_ts)[ncol(slope_diff_conc_lm_ts)] = "conc_slope_lm"
  
# merge slopes based on percents from the two method
slope_diff_perc_lm_ts = slope_diff_ts
names(slope_diff_perc_lm_ts)[ncol(slope_diff_perc_lm_ts)] = "perc_slope_ts"

slope_diff_perc_lm_ts = 
  merge(slope_diff_perc_lm_ts, 
        slope_diff_lm, all.x = TRUE)
dim(slope_diff_perc_lm_ts)
names(slope_diff_perc_lm_ts)[ncol(slope_diff_perc_lm_ts)] = "perc_slope_lm"

## plot lm vs. theil-sen, concentration
conc_corr_text = 
  calculate_corr_label(
  slope_diff_conc_lm_ts, 
  "conc_slope_ts", "conc_slope_lm")
conc_rmse_text = 
  round(
    rmse(
      slope_diff_conc_lm_ts$conc_slope_ts, 
      slope_diff_conc_lm_ts$conc_slope_lm), 3)

conc_slope_text = 

ggplot(subset(slope_diff_conc_lm_ts,
              conc_slope_ts > 
                quantile(slope_diff_conc_lm_ts$conc_slope_ts, 0.001)), 
       aes(x = conc_slope_ts, y = conc_slope_lm)) +
  geom_point() +
  geom_text(size = 5,
            aes(x = -0.5, y = 0.2, 
                label = ), 
            inherit.aes = FALSE) + 
  geom_abline(slope=1, intercept=0, color = "red") +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16)) 

  
## plot lm vs. theil-sen, percent

ggplot(subset(slope_diff_perc_lm_ts,
              perc_slope_ts > 
                quantile(slope_diff_perc_lm_ts$perc_slope_ts, 0.001)), 
       aes(x = perc_slope_ts, y = perc_slope_lm)) +
  geom_point() +
  geom_text(size = 5,
            aes(x = -0.5, y = 0.2, 
                label = 
                  calculate_corr_label(
                    slope_diff_perc_lm_ts, 
                    "perc_slope_ts", "perc_slope_lm")), 
            inherit.aes = FALSE) + 
  geom_abline(slope=1, intercept=0, color = "red") +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))   


#### 4.3 Spatial & temporal (annual) ####

cty_rural_urban = read.csv("/Users/TingZhang/Dropbox/HEI_US_PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$Longitude = cty_rural_urban$Latitude = NULL
cty_rural_urban = cty_rural_urban[!duplicated(cty_rural_urban$SiteCode), ] 

# match geoid
annual_contri_gps = merge(Year_aggregated_use, 
                          cty_rural_urban,
                          by = "SiteCode",
                          all.x = T)

annual_contri_gps$geoid = ifelse(annual_contri_gps$geoid < 10000, 
                                 paste0("0", annual_contri_gps$geoid), 
                                 annual_contri_gps$geoid)

###### 4.3.1 SP Map - common setting for regions ###### 

# generate the US county boundary data
us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr[, 13] = NULL # there are two "state_name"
head(us_cty_bdr)
us_cty_bdr_geo = dplyr::select(us_cty_bdr, geoid, stusps, geometry)

# MainStates <- map_data("state")
UScounty <- map_data("county")

us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr <- us_cty_bdr[!(us_cty_bdr$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

# merge annual contribution data with geometry
annual_source_gps = merge(annual_contri_gps, us_cty_bdr_geo)

# Define the grouping for the regions using state abbreviations
# # Census Regions and Divisions of the U.S.
# state_regions <- tibble(
#   state_abbr = c(
#     "CT", "ME", "MA", "NH", "RI", "VT", # New England (right, up 1)
#     "NJ", "NY", "PA", # Mid-Atlantic (right, up 2)
#     "DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", # South Atlantic (bottom, right 3) 
#     "AL", "KY", "MS", "TN", # East South Central (bottom, right 2) 
#     "AR", "LA", "OK", "TX", # West South Central (bottom, right 1) 
#     "IL", "IN", "MI", "OH", "WI", # East North Central (top, right 2)
#     "IA", "KS", "MN", "MO", "NE", "ND", "SD", # West North Central (top, right 1)
#     "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", # Mountain (left, up 2)
#     "CA", "OR", "WA" # Pacific (left, up 1)
#   ),
#   region = rep(c("New England", "Mid-Atlantic", "South Atlantic", "East South Central", 
#                  "West South Central", "East North Central", "West North Central",
#                  "Mountain", "Pacific"), c(6, 3, 8, 4, 4, 5, 7, 8, 3))
# )

# based on EPA Regions, but separate Midwest into two nearby area
state_regions <- tibble(
  state_abbr = c(
    "CT", "ME", "MA", "NH", "RI", "VT", # Region 1, New England,  (right, up)
    "NJ", "NY", # Region 2, New York/Jersey (right, bottom)
    "DE", "DC", "MD", "PA", "VA", "WV", # Region 3, Mid-Atlantic, (bottom, right)
    "AL", "FL", "GA", "KY", "MS", "NC", "SC", "TN", # Region 4, Southeast, (bottom, middle)
    "IL", "IN", "MI", "MN", "OH", "WI", "IA", "MO",  # Region 5, East North Central (top, right)
    "AR", "LA", "NM", "OK", "TX", # Region 6, South Central, (bottom, left)
    ### Region 7, Midwest, (top, middle)
    "CO", "MT", "ND", "SD", "UT", "WY", "KS", "NE", # Region 8, Mountains & Plains, (top, left)
    "AZ", "CA", "NV", # Region 9, Pacific Southwest, (bottom, middle)
    "AK", "ID", "OR", "WA" # Region 10, Pacific Northwest,  (bottom, up)
  ),
  region = rep(c("New England", "New York/Jersey", "Mid-Atlantic", "Southeast", 
                 "East North Central", "South Central", #"Midwest",
                 "Mountains & Plains", "Pacific Southwest", "Pacific Northwest"), 
               c(6, 2, 6, 8, 8, 5, 8, 3, 4))
)

us_states_region = merge(us_states, state_regions)

# dissolve states into regions
us_states_region$region <- as.factor(us_states_region$region)


regions_dissolved = 
  us_states_region %>%
  group_by(region) %>%
  # !!!! summarize from dplyr, not plyr !!!!
  dplyr::summarize(geometry = st_union(geometry)) 
class(regions_dissolved)

# Calculate the centroids of each region
region_centroids <- 
  us_states_region %>%
  group_by(region) %>%
  # !!!! summarize from dplyr, not plyr !!!!
  dplyr::summarize(geometry = 
                     st_centroid(st_union(geometry)), 
                   .groups = "keep")

# covert centroids to data.frame with longitude and latitude
centroid_coords <- 
  st_coordinates(region_centroids$geometry)
centroid_df <- data.frame(
  Longitude = centroid_coords[,1],
  Latitude = centroid_coords[,2],
  region = region_centroids$region
)

# get new region_name and reset it for ggtext format setting later in the center map
centroid_df$region_name = as.character(centroid_df$region) # orginal is factor, and can hardly to replace
centroid_df$region_name[centroid_df$region == "East North Central"] = "East North_Central"
centroid_df$region_name[centroid_df$region == "Mountains & Plains"] = "Mountains_& Plains"
centroid_df$region_name[centroid_df$region == "Pacific Northwest"] = "Pacific_Northwest"
centroid_df$region_name[centroid_df$region == "Pacific Southwest"] = "Pacific_Southwest"

# shift the position of text "New England" 
centroid_df$Longitude[centroid_df$region == "New England"] = -69.5
centroid_df$Latitude[centroid_df$region == "New England"] = 45.2
# shift the position of text "New York/Jersey" 
centroid_df$Longitude[centroid_df$region == "New York/Jersey"] = -76.2
centroid_df$Latitude[centroid_df$region == "New York/Jersey"] = 43
centroid_df

# Arrange the plots, set grid_layout

layout_matrix <- rbind(
  c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, NA, NA, NA,  NA, NA, NA, NA, NA, NA),
  c(NA, NA, NA, NA, NA, 7, 7, 7, NA, 5, 5, 5, NA, NA, NA, NA, NA, NA),
  c(NA, NA, NA, NA, NA, 7, 7, 7, NA, 5, 5, 5, NA, NA, NA, NA, NA, NA),
  c(NA, 9, 9, 9, NA, 7, 7, 7, NA, 5, 5, 5, NA, NA, NA, NA, NA, NA),
  c(NA, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, NA, 1, 1, 1, NA),
  c(NA, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, NA, 1, 1, 1, NA),
  c(NA, NA, NA, NA, 10, 10, 10, 10, 10, 10, 10, 10, 10, NA, 1, 1, 1, NA),
  c(NA, 8, 8, 8, 10, 10, 10, 10, 10, 10, 10, 10, 10, NA, NA, NA, NA, NA),
  c(NA, 8, 8, 8, 10, 10, 10, 10, 10, 10, 10, 10, 10, NA, 2, 2, 2, NA),
  c(NA, 8, 8, 8, 10, 10, 10, 10, 10, 10, 10, 10, 10, NA, 2, 2, 2, NA),
  c(NA, NA, NA, NA, 10, 10, 10, 10, 10, 10, 10, 10, 10, NA, 2, 2, 2, NA),
  c(NA, NA, 6, 6, 6, NA, 4, 4, 4, NA, 3, 3, 3, NA, NA, NA, NA, NA),
  c(NA, NA, 6, 6, 6, NA, 4, 4, 4, NA, 3, 3, 3, NA, NA, NA, NA, NA),
  c(NA, NA, 6, 6, 6, NA, 4, 4, 4, NA, 3, 3, 3, NA, NA, NA, NA, NA),
  c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
)

## EPA
# layout_matrix <- rbind(
#   c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, NA, NA, NA,  NA, NA, NA, NA, NA), 
#   c(NA, NA, NA, 8, 8, 8, NA, 7, 7, 7, NA, 5, 5, 5, NA, NA, NA), 
#   c(NA, NA, NA, 8, 8, 8, NA, 7, 7, 7, NA, 5, 5, 5, NA, NA, NA), 
#   c(NA, NA, NA, 8, 8, 8, NA, 7, 7, 7, NA, 5, 5, 5, NA, NA, NA),
#   c(NA, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, NA, NA, NA, NA), 
#   c(NA, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 1, 1, 1, NA), 
#   c(NA, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 1, 1, 1, NA), 
#   c(NA, NA, NA, NA, 11, 11, 11, 11, 11, 11, 11, 11, 11, 1, 1, 1, NA), 
#   c(NA, 9, 9, 9, 11, 11, 11, 11, 11, 11, 11, 11, 11, NA, NA, NA, NA), 
#   c(NA, 9, 9, 9, 11, 11, 11, 11, 11, 11, 11, 11, 11, 2, 2, 2, NA), 
#   c(NA, 9, 9, 9, 11, 11, 11, 11, 11, 11, 11, 11, 11, 2, 2, 2, NA), 
#   c(NA, NA, NA, NA, 11, 11, 11, 11, 11, 11, 11, 11, 11, 2, 2, 2, NA), 
#   c(NA, NA, NA, 6, 6, 6, NA, 4, 4, 4, NA, 3, 3, 3, NA, NA, NA), 
#   c(NA, NA, NA, 6, 6, 6, NA, 4, 4, 4, NA, 3, 3, 3, NA, NA, NA), 
#   c(NA, NA, NA, 6, 6, 6, NA, 4, 4, 4, NA, 3, 3, 3, NA, NA, NA),
#   c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, NA, NA, NA,  NA, NA, NA, NA, NA)
# )


###### 4.3.2 SP Map - basic source data, color define ###### 

# continuous-material.R in ggsci{}, https://github.com/nanxstats/ggsci/blob/master/R/continuous-material.R
show_col(pal_material("indigo")(10))
show_col(pal_material("red")(10))

scale_fill_material
## only color from scale_fill_material (listed in below palette) are available for 
palette = c("red", "pink", "purple", "deep-purple", "indigo", "blue", 
            "light-blue", "cyan", "teal", "green", "light-green", "lime", 
            "yellow", "amber", "orange", "deep-orange", "brown", "grey", 
            "blue-grey")

# color_source = c(
#   "F1-Traffic" = "#C71000FF",         # red 
#   "F2-Secondary Nitrate" = "#0073C2FF", # blue
#   "F3-Secondary Sulfate" = "#16A085FF", # green 16A085FF 00A087FF
#   "F4-Non-tailpipe" = "#F7C530FF",     # yellow FAFD7CFF F7C530FF
#   "F5-Industry" = "#E89242FF",         # orange
#   "F6-Salt" = "#00B5E2FF",             # cyan 6EE2FFFF  84D7E1FF  00B5E2FF
#   "F7-Biomass" = "#8A4198FF",          # purple #8A4198FF violetred
#   "F8-Soil/Dust" = "grey40"         # Gray, brown
# )

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F1-Traffic")
col_singleSource = "red"
col_singleSource_line = "#C71000FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F2-Secondary Nitrate")
col_singleSource = "blue"
col_singleSource_line = "#0073C2FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F3-Secondary Sulfate")
col_singleSource = "green" # green
col_singleSource_line = "#16A085FF"

annual_singleSource =
  subset(annual_source_gps,
         Source_use == "F4-Non-tailpipe")
col_singleSource = "yellow"
col_singleSource_line = "#F7C530FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F5-Industry")
col_singleSource = "orange"
col_singleSource_line = "#E89242FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F6-Salt")
col_singleSource = "cyan"
col_singleSource_line = "#00B5E2FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F7-Biomass")
col_singleSource = "purple"
col_singleSource_line = "#8A4198FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F8-Soil/Dust")
col_singleSource = "grey"
col_singleSource_line = "grey40"


###### 4.3.3.1 SP Map - Concentration - contribution trend estimation ######

# merge annual contribution & GPS data, with the region classification info
annual_singleSource_region = merge(annual_singleSource, state_regions)

# estimate the mean contribution of each region, for central map color fill
annual_singleSource_region_contri = 
  annual_singleSource_region %>%
  group_by(region) %>%
  dplyr::summarize(Concentration = mean(Concentration)) 
annual_singleSource_region_contri$geometry = NULL  

# merge the region mean contribution with geometry of each region
regions_dissolved_annual_singleSource =
  merge(regions_dissolved, annual_singleSource_region_contri)

# get the gps of each site, points on central map
singleSource_allSites = 
  ddply(annual_singleSource_region, 
        .(SiteCode), 
        summarise,
        Longitude = mean(Longitude, na.rm = T),
        Latitude = mean(Latitude, na.rm = T))

# convert single site info into sf file
allSite_sf =
  singleSource_allSites %>%
  st_as_sf(
    coords = c("Longitude", "Latitude"), 
    crs = st_crs(regions_dissolved_annual_singleSource))

# use spatial join to count the number of sites within each region
sites_within_regions =
  st_join(allSite_sf, regions_dissolved_annual_singleSource, join = st_within)
site_counts_sp = 
  data.frame(table(sites_within_regions$region))
names(site_counts_sp)[1] = "region"
site_counts_sp$region_site_count =
  paste0(site_counts_sp$region, ": ",
         site_counts_sp$Freq, " sites")

# Optionally, simplify geometries if they are too complex
# regions_dissolved <- st_simplify(regions_dissolved, preserveTopology = TRUE)

# temporal trends for each region
annual_singleSource_region_plot = 
  ddply(subset(annual_singleSource_region, Concentration>0), 
        .(region, Year),
        summarise,
        med.contri = median(Concentration),
        up.contri = quantile(Concentration, 0.995),
        down.contri = quantile(Concentration, 0.005),
        # up.contri = max(Concentration),
        # down.contri = min(Concentration),
        Longitude = last(Longitude),
        Latitude = last(Latitude),
        geoid = last(geoid),
        state_abbr = last(state_abbr)
  )

# merge with site count
annual_singleSource_region_plot = 
  merge(annual_singleSource_region_plot, site_counts_sp)

# create the data list, one for each region
annual_singleSource_region_split = split(annual_singleSource_region_plot, 
                                         annual_singleSource_region_plot$region)
max.contri.singleSource = max(annual_singleSource_region_plot$up.contri)

###### 4.3.3.2 SP Map - Concentration - plotting ######

# center map
singleSource_map_center <- 
  ggplot() +
  geom_sf(data = us_states_region, 
          fill = NA, color = "lightgrey") +  # Fill color for states without border
  geom_sf(data = regions_dissolved_annual_singleSource, 
          aes(fill = Concentration), 
          color = "white", lwd = 2, alpha = 0.5) +  # Thicker borders for regions
  geom_point(data = singleSource_allSites, 
             aes(x = Longitude, y = Latitude),
             color = "white", alpha = 0.5, size = 1.5) +
  geom_text(data = centroid_df,
           aes(x = Longitude, y = Latitude, label = addline_underline(region_name)),
           size = 5, color = "black") +  # Add region names
  theme_minimal() +
  scale_fill_material(col_singleSource) +
  #scale_alpha(guide = "none")+ # alpha for legend
  theme(legend.position = "bottom") +
  theme_map() +
  guides(fill = guide_colorbar(override.aes = list(alpha = 0.5),
                             title=format_variable("Concentration \n(µg/m3)"))) +
  theme(legend.position = c(0.85, 0.1),  # legend.position="none"
        legend.text = element_text(size = 12), 
        # legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 13, hjust = 0.1), 
        strip.background = element_blank()) # +
# extend margin space to place other figures
# theme(plot.margin = margin(1.5, 1.5, 2, 2, "cm"))
# singleSource_map_center

# create the plot list, one for each region
annual_singleSource_list_plots <- 
  lapply(annual_singleSource_region_split, function(x) {
    ggplot(data = x, aes(x = Year, y = med.contri), fill = NA) + 
      geom_line(color = col_singleSource_line, size = 0.8) +
      geom_point(shape = 3) +
      geom_errorbar(aes(ymin = down.contri, ymax = up.contri), width = 0.2) +
      scale_x_continuous(breaks = c(2011, 2014, 2017, 2020)) +
      scale_y_continuous(limits = c(0, max.contri.singleSource)) +  # Set y-axis limits
      labs(title = unique(x$region_site_count),
           y = format_variable("Concentration µg/m3")) +
      theme_classic() +
      theme(strip.background = element_blank(),
            plot.title = element_text(hjust = 0.5, vjust = 1),
            axis.title.x = element_text(size = 0),
            axis.text.x = element_text(size = 14, hjust = 0.5, vjust = 0), 
            axis.text.y = element_text(size = 14, hjust = 0.5),
            axis.title.y = element_text(size = 14, hjust = 0.5, vjust = 2.5, angle = 90))
  })
# annual_singleSource_list_plots$`East North Central`


# get the single time-seriel plot of each region
New_England_p = annual_singleSource_list_plots$`New England` # 1
New_York_Jersey_p = annual_singleSource_list_plots$`New York/Jersey` # 2
Mid_Atlantic_p = annual_singleSource_list_plots$`Mid-Atlantic` # 3
Southeast_p = annual_singleSource_list_plots$Southeast # 4
East_North_Central_p = annual_singleSource_list_plots$`East North Central` # 5
South_Central_p = annual_singleSource_list_plots$`South Central` # 6
# Midwest_p = annual_singleSource_list_plots$Midwest # 7
Mountains_Plains_p = annual_singleSource_list_plots$`Mountains & Plains` # 8
Pacific_Southwest_p = annual_singleSource_list_plots$`Pacific Southwest` # 9
Pacific_Northwest_p = annual_singleSource_list_plots$`Pacific Northwest` # 10


# Create an empty grob to use as a spacer
spacer_grob <- ggplot() + theme_void()

# Create a list of grobs
grob_list_tempral <- 
  list(
    # time series of each region
    New_England_grob = ggplotGrob(New_England_p), # 1
    # New_England_grob = spacer_grob,  # 1 # only for Aged sea salt
    New_York_Jersey_grob = ggplotGrob(New_York_Jersey_p), # 2
    Mid_Atlantic_grob = ggplotGrob(Mid_Atlantic_p), # 3
    Southeast_grob = ggplotGrob(Southeast_p), # 4
    East_North_Central_grob = ggplotGrob(East_North_Central_p), # 5
    South_Central_grob = ggplotGrob(South_Central_p), # 6
    # Midwest_grob = ggplotGrob(Midwest_p), # 7
    Mountains_Plains_grob = ggplotGrob(Mountains_Plains_p), # 8
    Pacific_Southwest_grob = ggplotGrob(Pacific_Southwest_p), # 9
    Pacific_Northwest_grob = ggplotGrob(Pacific_Northwest_p), # 10
    # central map
    singleSource_map_center_grob = ggplotGrob(singleSource_map_center)
  )

# Arrange the plots, using grid.arrange based on pre-set grid_layout
grid_layout_concentration <- grid.arrange(
  grobs = grob_list_tempral,
  layout_matrix = layout_matrix,
  widths = c(0.2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.3, 1, 1, 1, 0.3),  # Adjust as needed
  heights = c(0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.1)     # Adjust as needed
)

grid_layout_concentration

###### 4.3.4.1 SP Map - Percent - contribution trend estimation ######

# merge annual contribution & GPS data, with the region classification info
annual_singleSource_region = merge(annual_singleSource, state_regions)

# merge annual contribution & GPS data, with the region classification info
annual_singleSource_region_contri = 
  annual_singleSource_region %>%
  group_by(region) %>%
  dplyr::summarize(Percent = mean(Percent)) 
annual_singleSource_region_contri$geometry = NULL  

# merge the region mean contribution with geometry of each region
regions_dissolved_annual_singleSource =
  merge(regions_dissolved, annual_singleSource_region_contri)

# get the gps of each site, points on central map
singleSource_allSites = 
  ddply(annual_singleSource_region, 
        .(SiteCode), 
        summarise,
        Longitude = mean(Longitude, na.rm = T),
        Latitude = mean(Latitude, na.rm = T))

# Optionally, simplify geometries if they are too complex
# regions_dissolved <- st_simplify(regions_dissolved, preserveTopology = TRUE)

# temporal trends for each region

annual_singleSource_region_plot = 
  ddply(subset(annual_singleSource_region, Percent>0), 
        .(region, Year),
        summarise,
        med.contri = median(Percent),
        up.contri = max(Percent),
        down.contri = min(Percent),
        Longitude = last(Longitude),
        Latitude = last(Latitude),
        geoid = last(geoid),
        state_abbr = last(state_abbr)
  )

# merge with site count
annual_singleSource_region_plot = 
  merge(annual_singleSource_region_plot, site_counts_sp)

# create the data list, one for each region
annual_singleSource_region_split = split(annual_singleSource_region_plot, 
                                         annual_singleSource_region_plot$region)
max.contri.singleSource = max(annual_singleSource_region_plot$up.contri)

###### 4.3.4.2 SP Map - Percent - plotting ######

# center map
singleSource_map_center <- 
  ggplot() +
  geom_sf(data = us_states_region, 
          fill = NA, color = "lightgrey") +  # Fill color for states without border
  geom_sf(data = regions_dissolved_annual_singleSource, 
          aes(fill = Percent), 
          color = "white", lwd = 2, alpha = 0.5) +  # Thicker borders for regions
  geom_point(data = singleSource_allSites, 
             aes(x = Longitude, y = Latitude),
             color = "white", alpha = 0.5, size = 1.5) +
  geom_text(data = centroid_df,
            aes(x = Longitude, y = Latitude, label = addline_underline(region_name)),
            size = 5, color = "black") +  # Add region names
  theme_minimal() +
  scale_fill_material(col_singleSource) +
  #scale_alpha(guide = "none")+ # alpha for legend
  theme(legend.position = "bottom") +
  theme_map() +
  guides(fill = guide_colorbar(override.aes = list(alpha = 0.5),
                             title=format_variable("Percent %"))) +
  theme(legend.position = c(0.84, 0.1),  # legend.position="none"
        legend.text = element_text(size = 12), 
        # legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 13, hjust = 0.1), 
        strip.background = element_blank()) # +
# extend margin space to place other figures
# theme(plot.margin = margin(1.5, 1.5, 2, 2, "cm"))
# singleSource_map_center

# scale_fill_gradientn(colors = c("thistle1", "#8A4198FF")) 
# scale_fill_gradientn(colors = c("lightblue2", "#00B5E2FF")) 

# create the plot list, one for each region
annual_singleSource_list_plots <- 
  lapply(annual_singleSource_region_split, function(x) {
    ggplot(data = x, aes(x = Year, y = med.contri), fill = NA) + 
      geom_line(color = col_singleSource_line, size = 0.8) + # 
      geom_point(shape = 3) +
      geom_errorbar(aes(ymin = down.contri, ymax = up.contri), width = 0.2) +
      scale_x_continuous(breaks = c(2011, 2014, 2017, 2020)) +
      scale_y_continuous(limits = c(0, max.contri.singleSource), 
                         breaks = function(x) pretty(x, n = 3)) +  
      labs(title = unique(x$region_site_count),
           y = "Percent %") +
      theme_classic() +
      theme(strip.background = element_blank(),
            plot.title = element_text(hjust = 0.5, vjust = 1),
            axis.title.x = element_text(size = 0),
            axis.text.x = element_text(size = 14, hjust = 0.5, vjust = 0), 
            axis.text.y = element_text(size = 14, hjust = 0.5),
            axis.title.y = element_text(size = 14, hjust = 0.5, vjust = 2.5, angle = 90))
  })
# annual_singleSource_list_plots$`East North Central`

# get the single time-seriel plot of each region
New_England_p = annual_singleSource_list_plots$`New England` # 1
New_York_Jersey_p = annual_singleSource_list_plots$`New York/Jersey` # 2
Mid_Atlantic_p = annual_singleSource_list_plots$`Mid-Atlantic` # 3
Southeast_p = annual_singleSource_list_plots$Southeast # 4
East_North_Central_p = annual_singleSource_list_plots$`East North Central` # 5
South_Central_p = annual_singleSource_list_plots$`South Central` # 6
# Midwest_p = annual_singleSource_list_plots$Midwest # 7
Mountains_Plains_p = annual_singleSource_list_plots$`Mountains & Plains` # 8
Pacific_Southwest_p = annual_singleSource_list_plots$`Pacific Southwest` # 9
Pacific_Northwest_p = annual_singleSource_list_plots$`Pacific Northwest` # 10


# Create an empty grob to use as a spacer
spacer_grob <- ggplot() + theme_void()

# Create a list of grobs
grob_list_tempral <- 
  list(
    # time series of each region
    New_England_grob = ggplotGrob(New_England_p), # 1
    # New_England_grob = spacer_grob,  # 1 # only for Aged sea salt
    New_York_Jersey_grob = ggplotGrob(New_York_Jersey_p), # 2
    Mid_Atlantic_grob = ggplotGrob(Mid_Atlantic_p), # 3
    Southeast_grob = ggplotGrob(Southeast_p), # 4
    East_North_Central_grob = ggplotGrob(East_North_Central_p), # 5
    South_Central_grob = ggplotGrob(South_Central_p), # 6
    # Midwest_grob = ggplotGrob(Midwest_p), # 7
    Mountains_Plains_grob = ggplotGrob(Mountains_Plains_p), # 8
    Pacific_Southwest_grob = ggplotGrob(Pacific_Southwest_p), # 9
    Pacific_Northwest_grob = ggplotGrob(Pacific_Northwest_p), # 10
    # central map
    singleSource_map_center_grob = ggplotGrob(singleSource_map_center)
  )

# Arrange the plots, using grid.arrange based on pre-set grid_layout
grid_layout_fraction <- grid.arrange(
  grobs = grob_list_tempral,
  layout_matrix = layout_matrix,
  widths = c(0.2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.3, 1, 1, 1, 0.3),  # Adjust as needed
  heights = c(0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.1)     # Adjust as needed
)

grid_layout_fraction


#### 4.4 year-month time series ####
`
year_month_aggregated = read.csv("Year-month_site_source_contribuion_CSN_2024.01.csv")
year_month_aggregated$X = NULL
year_month_aggregated$Date = 
  paste0(year_month_aggregated$Year, "-", year_month_aggregated$Month)


as.Date(year_month_aggregated$Date[1], format = "%Y-%m")


#### 4.5 annimation ####

setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_base_DISPres1/")
pathway = "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_base_DISPres1/"

Year_aggregated = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/annual_results/Annual_site_source_contribuion_CSN_2024.01.csv")
Year_aggregated$X = NULL

Month_aggregated = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/annual_results/Month_site_source_contribuion_CSN_2024.01.csv")
Month_aggregated$X = NULL

month_source_gps = subset(month_source_gps,
                          !(Source_use %in% c("F10-Industries_2", "Vehic_Biom")))

for(source.type in unique(month_source_gps$Source_use)){
  
  dir.create(paste0(pathway, "CSN_", source.type))
  
  month_source_plot = subset(month_source_gps, Source_use == source.type)
  month_source_plot$YearMonth <- with(month_source_plot, paste(Year, Month, sep = "-"))
  
  year_month_animation <- 
    ggplot() +
    geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
    geom_point(data = month_source_plot, 
               aes(x = Longitude, y = Latitude, color = Concentration, group = interaction(Year, Month)), 
               size = 4, alpha = 0.8) +
    scale_alpha_continuous(range = c(0.1, 0.99), guide = "legend") +
    scale_color_gradient(low = "white", high = color_npg[2]) +
    coord_sf(datum = NA) +
    theme_minimal() +
    theme(panel.background = element_blank(),
          strip.text = element_text(color = "transparent")) +
    labs(title = paste0(source.type, ' Year: {frame_time}', ' Month: {frame_time}')) +
    transition_manual(frames = interaction(month_source_plot$Year, month_source_plot$Month))
  
  # Render and save the animation
  anim <- animate(year_month_animation, nframes = 100, fps = 10, width = 800, height = 600)
  anim_save(paste0(source.type, "_year_month_change.gif"), anim)
  
  ###### Yearly
  
  year_aggregated_ani = subset(Year_aggregated, Source_use == source.type)
  
  year_animation <- 
    ggplot() +
    geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
    geom_point(data = Year_aggregated_ani, 
               aes(x = Longitude, y = Latitude, color = Concentration, group = Year), 
               size = 4, alpha = 0.8) +
    scale_alpha_continuous(range = c(0.1, 0.99), guide = "legend") +
    scale_color_gradient(low = "white", high = color_npg[2]) +
    coord_sf(datum = NA) +
    theme_minimal() +
    theme(panel.background = element_blank(),
          strip.text = element_text(color = "transparent")) +
    labs(title = paste0(source.type, ' Year: {current_frame$Year}')) +
    transition_manual(frames = month_source_plot$Year)
  
  ###### Monthly
  
  month_aggregated_ani = subset(Month_aggregated, Source_use == source.type)
  
  month_animation <- 
    ggplot() +
    geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
    geom_point(data = month_aggregated_ani, 
               aes(x = Longitude, y = Latitude, color = Concentration, group = Month), 
               size = 4, alpha = 0.8) +
    scale_alpha_continuous(range = c(0.1, 0.99), guide = "legend") +
    scale_color_gradient(low = "white", high = color_npg[2]) +
    coord_sf(datum = NA) +
    theme_minimal() +
    theme(panel.background = element_blank(),
          strip.text = element_text(color = "transparent")) +
    labs(title = paste(source.type, 'Month: {current_frame$Month}')) +
    transition_manual(frames = month_source_plot$Month)
  
  
  # Render and save the animation
  anim_year_month <- animate(year_month_animation, nframes = 100, fps = 10, width = 800, height = 600)
  anim_save(paste0(source.type, "_year_month_change.gif"), anim)
  
  anim_year <- animate(year_animation, nframes = 100, fps = 10, width = 800, height = 600)
  anim_save(paste0(source.type, "_year_month_change.gif"), anim)
  
  anim_month <- animate(month_animation, nframes = 100, fps = 10, width = 800, height = 600)
  anim_save(paste0(source.type, "_year_month_change.gif"), anim)
  
  
}


#### Threshold comparison ####

threh_K = read.csv("/Users/TingZhang/Downloads/Above_thresholds_K.csv")
head(threh_K)

ggplot(threh_K, 
       aes(x = Method, y = Above_thresholds_K)) +
  geom_boxplot()

ggplot(threh_K, 
       aes(x = Above_thresholds_K)) +
  geom_histogram() +
  facet_grid(.~Method) +
  labs(x = format_variable("K Concentration above Thresholds  µg/m3"), y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 15, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))

ddply(threh_K, .(Method), summarise,
      mean_k = mean(Above_thresholds_K),
      median_k = median(Above_thresholds_K))

