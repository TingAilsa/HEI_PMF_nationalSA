library(sf)
library(readr)
library(scales)
library(dplyr)
library(plyr)
library(tidyr)
library(stringr)
library(gganimate)
library(ggthemes)
library(ggplot2)
library(purrr)
library(ggsci)
library(gridExtra)
library(insight)

setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/")
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/"


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
      read.csv)))
csv_month$Dataset = "CSN"
csv_month$X = NULL

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

dropbox_path = "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/"

# site_info_all = read.csv(paste0(dropbox_path, "CSN_NoGUI_NoCsub_15TimesMean_site/CSN_noCsub_15timesMean_PMF_SWB_site.csv"))
site_info_all = read.csv(paste0(dropbox_path, "CSN_NoGUI_NoCsub_15t1mdl0unc_site/CSN_noCsub_15t1mdl0unc_PMF_SWB_site.csv"))

site_geo = read.csv(paste0(dropbox_path, "CSN_IMPROVE_ownPC/CSN_site_info.csv"))
site_geo$SiteCode = as.character(site_geo$SiteCode)

site_geoid = read.csv(paste0(dropbox_path, "CSN_IMPROVE_ownPC/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv"))

cty_cluster_traffic = read.csv("results_R_data/County_cluster_traffic_info.csv")

# site serial
site_code_serial = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_IMPROVE_ownPC/CSN_IMPROVE_site.serial.csv")
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


write.csv(site_census_source_assign, 
          paste0(data_use, "_PMF_source_census_", time, ".csv"))
# "CSN_Site_15TimesMean_PMF_source_census_2024-03-15.csv"
# "CSN_Site_15t1mdl0unc_PMF_source_census_2024-04-19.csv"

# data_use = "CSN_Site_15TimesMean"
# site_census_source_assign = site_census_source_assign_5unc

# data_use = "CSN_Site_15tMean_0unc"
# site_census_source_assign = site_census_source_assign_0unc


###### 1.3 site-specific performance summary ######

site_census_source_assign_5unc = read.csv("CSN_Site_15TimesMean_PMF_source_census_2024-03-22.csv")
site_census_source_assign_0unc = read.csv("CSN_Site_15tMean_0unc_PMF_source_census_2024-03-22.csv")
site_census_source_assign_5unc$X = site_census_source_assign_0unc$X = NULL

site_source_5unc = 
  select(site_census_source_assign_5unc, 
         SiteCode, serial.No, Factor.No, State, 
         Longitude, Latitude, geoid, RuralUrban, 
         median_PMF_PM2.5, median_obs_PM2.5, cor_PMF.obs_PM, 
         converge_percent, DISP_error.code, DISP_qdrop, bs.map, 
         toAssign_Zero, toAssign_all_Zero, 
         assigned_Zero, assigned_all_Zero)
site_source_5unc$overall_unc = "Extra_overall_5%"

site_source_0unc = 
  select(site_census_source_assign_0unc, 
         SiteCode, serial.No, Factor.No, State, 
         Longitude, Latitude, geoid, RuralUrban, 
         median_PMF_PM2.5, median_obs_PM2.5, cor_PMF.obs_PM, 
         converge_percent, DISP_error.code, DISP_qdrop, bs.map, 
         toAssign_Zero, toAssign_all_Zero, 
         assigned_Zero, assigned_all_Zero)
site_source_0unc$overall_unc = "No_extra_uncertainty"

summary(site_source_0unc$SiteCode == site_source_5unc$SiteCode & 
          site_source_0unc$Factor.No == site_source_5unc$Factor.No)
summary(site_source_0unc$median_PMF_PM2.5 == site_source_5unc$median_PMF_PM2.5)

source_perform = rbind(site_source_5unc, site_source_0unc)

# PMF predicted PM2.5
ggplot(source_perform, 
       aes(x = median_obs_PM2.5, y = median_PMF_PM2.5)) +
  geom_point() +
  facet_grid(. ~ overall_unc) +
  theme_bw(base_size = 16)

plot(site_source_0unc$median_PMF_PM2.5, site_source_5unc$median_PMF_PM2.5)
lm_pmf_0ucn_5unc = 
  lm(site_source_0unc$median_PMF_PM2.5 ~ site_source_5unc$median_PMF_PM2.5)
summary(lm_pmf_0ucn_5unc)

# Zero-contributions
ggplot(subset(source_perform,
              assigned_Zero == 1), 
       aes(x = Factor.No)) + 
  geom_histogram(alpha = 0.6) +
  facet_grid(. ~ overall_unc) +
  scale_x_continuous(breaks = 3:11) +
  theme_bw(base_size = 16)
ggplot(subset(source_perform,
              toAssign_Zero == 1), 
       aes(x = Factor.No)) + 
  geom_histogram(alpha = 0.6) +
  facet_grid(. ~ overall_unc) +
  scale_x_continuous(breaks = 3:11) +
  theme_bw(base_size = 16)

ddply(source_perform, .(overall_unc), summarise,
      assigned_all0_row = sum(assigned_all_Zero, na.rm = TRUE), 
      to_assign_all0_row = sum(toAssign_all_Zero, na.rm = TRUE))

# Converged percent

ggplot(source_perform, 
       aes(x = converge_percent * 100)) + 
  geom_histogram(alpha = 0.6) +
  facet_grid(. ~ overall_unc) +
  scale_x_continuous(breaks = 3:11) +
  theme_bw(base_size = 16)

source_perform_converge =
source_perform %>%
  group_by(overall_unc, converge_percent) %>%
  dplyr::summarise(frequency = n()) %>%
  ungroup()

source_perform_converge =
  source_perform_converge %>%
  pivot_wider(
    names_from = overall_unc,
    values_from = frequency
  )

source_low_converge = 
  subset(source_perform, converge_percent < 0.3)

ggplot(source_low_converge, 
       aes(x = median_obs_PM2.5, y = median_PMF_PM2.5)) +
  geom_point() +
  facet_grid(. ~ overall_unc) +
  theme_bw(base_size = 16)

source_low_PMF_pm = 
  subset(source_perform, median_PMF_PM2.5 < 0.2 * median_obs_PM2.5)


###### 1.4 species prediction performance ######
species_perf_5 = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15TimesMean/base_DISPres1/CSN_noCsub_15TimesMean_PMF_vs_obs.csv")
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





####  0. Source assignment results ####

# overall_Assigned = read.csv("results_R_data/Decided_SA.csv")
# org_assigned = read.csv("results_R_data/Source_assigned.csv")
overall_Assigned$X = org_assigned$X = NULL

# there might be some changes in the original manual assign (Source_assigned.csv)
auto_assign = cbind(select(overall_Assigned, 
                           Dataset, Cluster, Factor),
                    overall_Assigned[, 17:40])
manual_assign = overall_Assigned[, 1:15]

# convert the data for plotting
long_auto_assign <- 
  auto_assign %>%
  pivot_longer(
    cols = starts_with("Assigned_"),
    names_to = c(".value", "set"),
    names_pattern = "Assigned_(.*)_([1-8])"
  )
long_auto_assign$final_assign = long_auto_assign$Source_reference

long_manual_assign <- 
  manual_assign %>%
  pivot_longer(
    cols = starts_with("toAssign_"),
    names_to = c(".value", "set"),
    names_pattern = "toAssign_(.*)_([1-3])"
  )
long_manual_assign <- 
  long_manual_assign %>% 
  rename(final_assign = potential_assignment)  

# combine final assignment
source_assign = rbind(long_auto_assign, long_manual_assign)
source_assign$set = NULL

source_assign = source_assign[
  with(source_assign, 
       order(Dataset, Cluster, Factor, Source_reference)), ]
source_assign = na.omit(source_assign)

write.csv(source_assign, "Final_SA_noCsub_25TimesMean_2024.01.csv")

#### 0.1 add Cluster.No and Factor.No  FINISHED ####

# Set the path to the folder containing your CSV files
folder_path <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_base_DISPres1"  # Replace with your folder path

# List all files ending with "annual.csv" or "month.csv"
file_paths <- list.files(folder_path, pattern = "CSN_noCsub_noExtreme_C_[0-9]+_F_[0-9]+_(annual|month)\\.csv", full.names = TRUE)

# Function to process each file
process_file <- function(file_path) {
  # Extract Cluster.No and Factor.No from the file name
  parts <- str_extract_all(file_path, "[0-9]+")[[1]]
  cluster_no <- as.numeric(parts[length(parts) - 1])
  factor_no <- as.numeric(parts[length(parts)])
  
  # Read the file
  data <- read_csv(file_path)
  
  # Add Cluster.No and Factor.No columns
  data <- data %>% 
    mutate(Cluster.No = cluster_no, Factor.No = factor_no)
  
  # Save the file
  write_csv(data, file_path)
}

lapply(file_paths, process_file)

#### 1.0 generate corresponding daily site SA results 2023.12 ####

#csn_cluster = read.csv("/Users/TingZhang/Downloads/CSN_RF_cluster5training.csv")
#colnames(csn_cluster)[2] = "Cluster"

source_assign = read.csv("results_R_data/Final_SA_noCsub_25TimesMean_2024.01.csv")
source_only_assign = select(source_assign, 
                            Dataset, Cluster, Factor,
                            Main_Species, Source_reference, final_assign)
colnames(source_only_assign)[2:3] = c("Cluster.No", "Factor.No")

daily_csn_folder = paste0(data.dir, "PMF_nonGUI_Cluster/CSN_base_DISPres1/")

# a list of unique combinations of Cluster and Factor
combinations <- as.data.frame(unique(source_assign[c("Cluster", "Factor")]))

# read and combine all monthly files
combined_month = 
  read_and_combine_files(
    combinations, 
    "CSN_noCsub_noExtreme_", 
    "_month.csv", 
    daily_csn_folder)

combined_month$Source_reference = combined_month$Source.No = combined_month$...1 = NULL
colnames(combined_month)[7]
colnames(combined_month)[7] = "Source_reference"
# combined_month <- combined_month %>% rename(Source_reference = Factor_source)  

month_source_cleaning = merge(combined_month, source_only_assign, all.x = T)
month_source_cleaning$Source_use = month_source_cleaning$final_assign
unique(month_source_cleaning$Source_use)
unique(month_source_cleaning$Source_reference)
 
month_source_cleaning$Source_use[grepl("Vehicle + biomass", month_source_cleaning$Source_use, fixed = T)] = "Vehic_Biom"
month_source_cleaning$Source_use[grepl("Soil", month_source_cleaning$Source_use, fixed = T)] = "F9-Soil/Dust"
month_source_cleaning$Source_use[grepl("Vehicle", month_source_cleaning$Source_use, fixed = T)] = "F1-Vehicle"
month_source_cleaning$Source_use[grepl("Biomass", month_source_cleaning$Source_use, fixed = T)] = "F8-Biomass"
month_source_cleaning$Source_use[month_source_cleaning$Source_use == "Industry 2"] = "F10-Industries_2"
month_source_cleaning$Source_use[grepl("Industry", month_source_cleaning$Source_use, fixed = T)] = "F5-Industry"
month_source_cleaning$Source_use[grepl("Aged sea", month_source_cleaning$Source_use, fixed = T)] = "F4-Aged Sea Salt"
month_source_cleaning$Source_use[grepl("Fresh Sea", month_source_cleaning$Source_use, fixed = T)] = "F6-Fresh Sea Salt"
month_source_cleaning$Source_use[grepl("peline", month_source_cleaning$Source_use, fixed = T)] = "F7-Non-Tailpipe"

## For those not included in the results, manually assign
source_na = subset(month_source_cleaning, is.na(Source_use))
source_na_unique = 
  ddply(source_na,
        .(Main_Species, Source_reference, Cluster.No, Factor.No, Factor, 
          Dataset, final_assign, Source_use),
        summarise,
        Contribution = median(Contribution))

# Identify the rows where 'Source_use' does not contain "Factor" and 'final_assign' is NA
rows_to_update <- !grepl("Factor", month_source_cleaning$Source_reference, fixed = TRUE) &
  is.na(month_source_cleaning$final_assign)
unique(month_source_cleaning[rows_to_update,]$Source_reference)
# Assign 'Source_reference' values to 'Source_use' for these rows
month_source_cleaning$Source_use[rows_to_update] <- month_source_cleaning$Source_reference[rows_to_update]

rows_to_update_2 <- grepl("Si Al Ti Fe", month_source_cleaning$Main_Species, fixed = TRUE) &
  is.na(month_source_cleaning$Source_use)
View(month_source_cleaning[rows_to_update_2,])
unique(month_source_cleaning[rows_to_update_2,]$Source_reference)
month_source_cleaning$Source_use[rows_to_update_2] <- "F9-Soil/Dust"

# convert those not assigned to "F10-Industries_2"
source_not_assign = subset(month_source_cleaning, is.na(Source_use))
unique(source_not_assign$Main_Species)

month_source_cleaning$Source_use[is.na(month_source_cleaning$Source_use)] = "F10-Industries_2"

write.csv(month_source_cleaning, "Decided_monthly_SA_2024.01.csv")

# combine two or more industry sources
month_assign_industry_combine = month_source_cleaning
month_assign_industry_combine$Source_use[month_assign_industry_combine$Source_use == "F10-Industries_2"] = "F5-Industry"

### TO BE FINISHED

#### check the result for single cluster
select.cluster = 1
month_source_cleaning_1c = subset(month_source_cleaning,
                                Cluster.No == select.cluster)
source_1c_contri = ddply(month_source_cleaning_1c,
                         .(Source_use),
                         summarise,
                         Contribution = mean(Contribution, na.rm = T))
summary(source_1c_contri)

#### 1.0 daily site SA results 2024.04 ####

source_org = read.csv("CSN_Site_15t1mdl0unc_PMF_decision_2024-04.csv")
source_org$X = NULL
names(source_org)

source_org_long = 
  source_org %>%
  pivot_longer(
    cols = starts_with("assigned"),  # selects all columns that start with 'assigned'
    names_to = c(".value", "number"), # '.value' helps in splitting column names based on a pattern
    names_pattern = "assigned_(.*)_(\\d+)$"  # Regex to split the column names into 'assigned_Source', 'assigned_Species', 'assigned_Fraction', and a group number
  )
# View(source_org_long)

# remove the mixed for now
source_org_long = na.omit(source_org_long)

source_org_long = 
  subset(source_org_long, 
         !(Source %in% 
           c("F8-Biomass + F3-Vehicle", "F3-Vehicle + F9-Soil/Dust")))

# rename some sources
source_org_long$Source[source_org_long$Source == "F3-Vehicle"] = "F1-Vehicle"
source_org_long$Source[source_org_long$Source == "F10-Ship"] = "F1-Vehicle"
source_org_long$Source[source_org_long$Source == "Non-tailpipe"] = "F7-Non-tailpipe"

source_org_long$Source[source_org_long$Source == ""] = "F7-Industry"
source_org_long$Source[source_org_long$Source == "F7-Industry"] = "F5-Industry"

source_org_long = plyr::rename(source_org_long, 
                          c("Factor" = "Factor_source",
                            "Source" = "Source_aftermanual"))

unique(source_org_long$Source_aftermanual)
unique(source_org_long$Factor_source)

# get fraction numeric
source_org_long$Fraction_nm = as.numeric(sub("%", "", source_org_long$Fraction))
source_org_long$Fraction = NULL
summary(source_org_long$Fraction_nm)

# combine with monthly concentration contribution
month_source = read.csv("CSN_Site_15t1mdl0unc_month.csv")
month_source$X = NULL
names(month_source)[2] = "serial.No"
head(month_source)
unique(month_source$Factor_source)

summary(unique(source_org_long$Factor_source) %in% unique(month_source$Factor_source))
summary(unique(month_source$Factor_source) %in% unique(source_org_long$Factor_source))
#"Factor10", "Factor11"

source_org_long$site_factor = 
  paste(source_org_long$serial.No, source_org_long$Factor.No)
month_source$site_factor = 
  paste(month_source$serial.No, month_source$Factor.No)
source_org_long = data.frame(source_org_long)

month_source_cleaning = 
  merge(source_org_long, month_source, 
        by = c("Dataset", "serial.No", "Factor.No", "Factor_source", "site_factor"))
dim(month_source_cleaning)
dim(source_org_long)
dim(month_source)
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

month_source_cleaning = subset(month_source_cleaning,
                               Main_Species == Species)
dim(month_source_cleaning)
month_source_cleaning$Species = NULL

# rename sources, and Aged & Fresh sea salt to Sea Salt
month_source_cleaning$Source_use = month_source_cleaning$Source_aftermanual

replacement_sourcename <- 
  c("F7-Non-tailpipe" = "F4-Non-tailpipe",
    "F6-Fresh Sea Salt" = "F6-Sea Salt",
    "F4-Aged Sea Salt" = "F6-Sea Salt",
    "F8-Biomass" = "F7-Biomass",
    "F9-Soil/Dust" = "F8-Soil/Dust")

month_source_cleaning <- 
  month_source_cleaning %>%
  mutate(Source_use = 
           ifelse(Source_use %in% names(replacement_sourcename), 
                  replacement_sourcename[Source_use], 
                  Source_use))

# in manual assign, for cases of >1 factors assigned to the same source
# Get the average
month_source_assign <- 
  month_source_cleaning %>%
  group_by_at(vars(-Fraction_nm, -Concentration, -Factor_source, 
                   -Source_reference, -Source_aftermanual, -number, 
                   -Factor, -Source.No, -Main_Species)) %>%
  dplyr::summarise(
    Fraction_nm = mean(Fraction_nm, na.rm = TRUE),
    Concentration = mean(Concentration, na.rm = TRUE),
    .groups = "drop")
dim(month_source_assign)


write.csv(month_source_cleaning, paste0(data_use, "_montly_source_assigned_with_2factor_to_one.csv"))
write.csv(month_source_assign, paste0(data_use, "_montly_source_assigned.csv"))

#### 4. plotting - data preparation ####

library(USAboundaries)
library(ggsci)
library(gganimate)

color_npg = pal_npg("nrc")(10)

# monthly assigned source
month_source_assign = read.csv("CSN_Site_15t1mdl0unc_montly_source_assigned.csv")
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
# cty_rural_urban = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
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
                         Concentration = mean(Concentration))

year_month_aggregated <- ddply(month_source_gps, 
                               .(Dataset.x, SiteCode, Year, Month, Source_use), 
                               summarise,
                               Longitude = mean(Longitude),
                               Latitude = mean(Latitude),
                               Concentration = mean(Concentration))

month_aggregated <- ddply(month_source_gps, .(Dataset.x, SiteCode, Month, Source_use), summarise,
                          Longitude = mean(Longitude),
                          Latitude = mean(Latitude),
                          Concentration = mean(Concentration))

# write.csv(Year_aggregated, "Annual_site_source_contribuion_CSN_2024.01.csv")
# write.csv(year_month_aggregated, "Year-month_site_source_contribuion_CSN_2024.01.csv")
# write.csv(month_aggregated, "Month_site_source_contribuion_CSN_2024.01.csv")

write.csv(Year_aggregated, paste0(data_use, "_Annual_source_2024.04.csv")) # "CSN_Site_15t1mdl0unc_Annual_source_2024.04.csv"
write.csv(year_month_aggregated, paste0(data_use, "_Year-month_source_2024.04.csv")) # "CSN_Site_15t1mdl0unc_Year-month_source_2024.04.csv"
write.csv(month_aggregated, paste0(data_use, "_Month_source_2024.04.csv")) # "CSN_Site_15t1mdl0unc_Month_source_2024.04.csv"

#### 4.2 mapping - annual & map ####

###### 4.2.0. data selection, choose one from data-1 & data-2 ######

color_npg = pal_npg("nrc")(10)

####### data-1, averages of the PMF outputs

# Year_aggregated = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/Annual_site_source_contribuion_CSN_2024.01.csv")
# month_aggregated = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/Month_site_source_contribuion_CSN_2024.01.csv")

Year_aggregated = read.csv("CSN_Site_15t1mdl0unc_Annual_source_2024.04.csv")
month_aggregated = read.csv("CSN_Site_15t1mdl0unc_Month_source_2024.04.csv")

Year_aggregated$X = month_aggregated$X = NULL

Year_aggregated_use = subset(Year_aggregated, 
                             !(Source_use %in% c("F10-Industries_2", "Vehic_Biom")))

Month_aggregated_use = subset(month_aggregated, 
                              !(Source_use %in% c("F10-Industries_2", "Vehic_Biom")))

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
#     Source == "F1_Vehicle"     ~ "F1-Vehicle",
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
#                              Period %in% 2011:2020)
# colnames(Year_aggregated_use)[ncol(Year_aggregated_use)-3] = "Year"
# Year_aggregated_use$Contribution = Year_aggregated_use$Source.concentration
# Year_aggregated_use$Year = as.integer(Year_aggregated_use$Year)
# 
# Month_aggregated_use = subset(source_contribution, 
#                               !(Period %in% 2011:2020 | Period == "overall"))
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

middle_position = 
  data.frame(
    Year = rep(as.factor(2016), 
               length(unique(Year_aggregated_use$Source_use))),
    Source_use = unique(Year_aggregated_use$Source_use))

# boxplot - annual
ggplot(subset(Year_aggregated_use, 
              Concentration < quantile(Concentration, 0.995)),
       aes(as.factor(Year), Concentration),
       color = Source_use) +
  geom_boxplot(aes(color = Source_use), 
               outlier.shape = NA, 
               linewidth = 0.3, width = 0.5) +
  geom_jitter(aes(color = Source_use), 
              width = 0.15, alpha = 0.15)+
  facet_grid(Source_use ~.) + # , scales = "free_y"
  # ylim(0,4) +
  scale_y_continuous(breaks = c(0, 2, 4)) +
  # scale_x_discrete(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
  labs(x = "Year", y = format_variable("Concentration µg/m3")) +
  scale_color_manual(values = color_npg) + # color_npg[-c(1, 5, 7)]
  geom_text(data = middle_position, size = 5,
            aes(x = Year, y = 3, label = Source_use), 
            inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
  theme_bw() +
  theme(panel.grid = element_line(colour = "white"),
        # plot.title = element_text(hjust = 0.05, vjust = -25, size = 0),
        strip.background = element_blank(), strip.text = element_blank(), # facet title
        legend.position = "none",
        axis.title.x = element_text(color="grey25", size = 18, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1, margin=margin(0,2,0,0)),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 15, angle = 45, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))


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
Year_aggregated_summary = 
  ddply(Year_aggregated_use, 
        .(Source_use, Year),
        summarise,
        Lower = quantile(Concentration, 0.0025),
        Median = median(Concentration),
        Upper = quantile(Concentration, 0.9975))
Year_aggregated_summary$Year = as.integer(Year_aggregated_summary$Year)

ggplot(Year_aggregated_summary, 
       aes(x = Year, y = Median, color = Source_use, fill = Source_use)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(2011, 2014, 2017)) + 
  #scale_y_continuous(limits = c(0, 4), breaks = c(0, 2, 4)) +  # Set y-axis limits and breaks
  labs(x = "Year", y = format_variable("Concentration µg/m3")) +
  scale_color_manual(values = color_npg) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, color = NA) +
  facet_wrap(~ Source_use, ncol = 3) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 18, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 20, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 20, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5))

########### Monthly

middle_position = 
  data.frame(
    Month = rep(as.factor(6), 
               length(unique(Month_aggregated_use$Source_use))),
    Source_use = unique(Month_aggregated_use$Source_use))

contri_range1 <- quantile(Month_aggregated_use$Concentration, 
                          c(0.025, 0.975), na.rm = T)

ggplot(subset(Month_aggregated_use, 
              Concentration < contri_range1[2] &
                Concentration > contri_range1[1]),
       aes(as.factor(Month), Concentration),
       color = Source_use) +
  geom_boxplot(aes(color = Source_use), 
               outlier.shape = NA, 
               linewidth = 0.3, width = 0.5) +
  geom_jitter(aes(color = Source_use), 
              width = 0.15, alpha = 0.15)+
  facet_grid(Source_use ~.) + # , scales = "free_y"
  # ylim(0,4) +
  scale_y_continuous(breaks = c(0,1,2)) +
  # scale_y_continuous(breaks = c(-20, 0, 20)) +
  # scale_x_discrete(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
  labs(x = "Month", y = format_variable("Concentration µg/m3")) +
  scale_color_manual(values = color_npg) + # color_npg[-c(1, 5, 7)]
  geom_text(data = middle_position, size = 5,
            aes(x = Month, y = 2.5, label = Source_use), 
            inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
  theme_bw() +
  theme(panel.grid = element_line(colour = "white"),
        # plot.title = element_text(hjust = 0.05, vjust = -25, size = 0),
        strip.background = element_blank(), strip.text = element_blank(), # facet title
        legend.position = "none",
        axis.title.x = element_text(color="grey25", size = 18, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1, margin=margin(0,2,0,0)),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))

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
        Lower = quantile(Concentration, 0.0025, na.rm = T),
        Median = median(Concentration, na.rm = T),
        Upper = quantile(Concentration, 0.9975, na.rm = T))


ggplot(Month_aggregated_summary, 
       aes(x = Month, y = Median, color = Source_use, fill = Source_use)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) + 
  #scale_y_continuous(limits = c(0, 4), breaks = c(0, 2, 4)) +  # Set y-axis limits and breaks
  labs(x = "Month", y = format_variable("Concentration µg/m3")) +
  scale_color_manual(values = color_npg) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, color = NA) +
  facet_wrap(~ Source_use, ncol = 3) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 18, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 20, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 20, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5))


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


###### 4.2.3. Spatial distribution of Changes between 2011 & 2020 - Slope ######

# detect those with <2 data groups, a regression needs at least 2 groups of data.
Year_aggregated_use_unique = data.frame(table(Year_aggregated_use$SiteCode, 
                                              Year_aggregated_use$Source_use))
colnames(Year_aggregated_use_unique)[1:2] = c("SiteCode", "Source_use")
Year_exclude = subset(Year_aggregated_use_unique, Freq < 2)
row.names(Year_exclude)=NULL
head(Year_exclude)
# Year_exclude$SiteCode = as.integer(Year_exclude$SiteCode)

# exclude sites with <2 groups of data
Year_aggregated_use$SiteCode = as.factor(Year_aggregated_use$SiteCode)
Year_aggregated_use_slope = anti_join(Year_aggregated_use, 
                                      Year_exclude, 
                                      by = c("SiteCode", "Source_use"))
sapply(Year_aggregated_use_slope, class)

# estimate the slope for each site across the study period
slope_diff <- 
  Year_aggregated_use_slope %>%
  group_by(SiteCode, Source_use, Longitude, Latitude) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope(cur_data(), "Year", "Concentration")
    ) %>%
  ungroup()

slope_diff = na.omit(slope_diff)

slope_diff_source = 
  ddply(slope_diff, .(Source_use), summarise,
        slope_mean = round(mean(diff_slope), 3), 
        slope_sd = round(sd(diff_slope), 3), 
        slope_median = round(median(diff_slope), 3), 
        slope_975th = round(quantile(diff_slope, 0.975), 3), 
        slope_025th = round(quantile(diff_slope, 0.025), 3))

export_table(slope_diff_source, format = "md")

ggplot() +
  geom_histogram(data = subset(slope_diff, Source_use == "F3-Secondary Sulfate"), 
                 aes(x = diff_slope)) +
  theme_minimal()

# calculate the occurrence of each site in each source
source_site_count = as.data.frame(t(table(slope_diff$Source_use)))
source_site_count$Var1 = NULL
colnames(source_site_count)[1] = c("Source_use")
source_site_count$Freq[source_site_count$Source_use == "F3-Secondary Sulfate"] = 
  source_site_count$Freq[source_site_count$Source_use == "F2-Secondary Nitrate"] 
source_site_count$source_site_count = 
  paste0(source_site_count$Source_use, 
         "\nNo. of Sites: ", 
         source_site_count$Freq)

slope_diff = merge(slope_diff, source_site_count)


# Create the plot
slope_range1 <- quantile(slope_diff$diff_slope, c(0.0025, 0.9975))
slope_range1 <- c(-0.6, 0.6)
slope_range1 <- c(-0.4, 0.4)

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = slope_diff, 
             aes(x = Longitude, y = Latitude, 
                 color = diff_slope),
             size = 2.5, alpha = 0.8) +
  scale_color_gradient2(limits = slope_range1,
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
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10), 
        legend.text = element_text(size = 14), 
        # legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16))

slope_range2 <- quantile(slope_diff$diff_slope, c(0.0005, 0.9995))
slope_range2 <- c(-2, 2)

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = slope_diff, 
             aes(x = Longitude, y = Latitude, 
                 color = diff_slope),
             size = 2.5, alpha = 0.8) +
  scale_color_gradient2(limits = slope_range2,
                        low = "blue", 
                        high = "red", 
                        midpoint = 0) +
  coord_sf(datum = NA) +
  facet_wrap(~ Source_use, 
             labeller = labeller(Source_use = 
                                   as_labeller(as.character, 
                                               default = label_value))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16))


#### 4.3 Spatial & temporal (annual) ####

cty_rural_urban = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
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

###### 4.3.1 Temporal trends & Map - common setting for regions ###### 

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

annual_source_gps = merge(us_cty_bdr_geo, 
                          annual_contri_gps)

# Define the grouping for the regions using state abbreviations
# Census Regions and Divisions of the U.S.
state_regions <- tibble(
  state_abbr = c(
    "CT", "ME", "MA", "NH", "RI", "VT", # New England (right, up 1)
    "NJ", "NY", "PA", # Mid-Atlantic (right, up 2)
    "DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", # South Atlantic (bottom, right 3) 
    "AL", "KY", "MS", "TN", # East South Central (bottom, right 2) 
    "AR", "LA", "OK", "TX", # West South Central (bottom, right 1) 
    "IL", "IN", "MI", "OH", "WI", # East North Central (top, right 2)
    "IA", "KS", "MN", "MO", "NE", "ND", "SD", # West North Central (top, right 1)
    "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", # Mountain (left, up 2)
    "CA", "OR", "WA" # Pacific (left, up 1)
  ),
  region = rep(c("New England", "Mid-Atlantic", "South Atlantic", "East South Central", 
                 "West South Central", "East North Central", "West North Central",
                 "Mountain", "Pacific"), c(6, 3, 8, 4, 4, 5, 7, 8, 3))
)


census_region = c("New England", "Mid-Atlantic", "South Atlantic", 
                  "East South Central", "West South Central", "East North Central", 
                  "West North Central", "Mountain", "Pacific")

# left, "Pacific" (up, 1) & "Mountain" (down, 2); 
# bottom, "West South Central"(left, 1), "East South Central" (right, 2) & "South Atlantic" (right, 3)
# right, "New England" (up, 1) & "Mid-Atlantic" (down, 2); 
# top "West North Central" (left, 1) & "East North Central" (right, 2)

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
#centroid_df$region_name = centroid_df$region
#centroid_df$region_name[1:2] = c("East North \n Central", "East South \n Central")
#centroid_df$region_name[8:9] = c("West North \n Central", "West South \n Central")

annual_source_gps = merge(annual_contri_gps, us_cty_bdr_geo)

# Arrange the plots, set grid_layout
layout_matrix <- rbind(
  c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, NA, NA, NA,  NA, NA, NA, NA, NA), 
  c(NA, NA, NA, NA, NA, 1, 1, 1, NA, 2, 2, 2, NA, NA, NA, NA, NA), 
  c(NA, NA, NA, NA, NA, 1, 1, 1, NA, 2, 2, 2, NA, NA, NA, NA, NA), 
  c(NA, 3, 3, 3,  NA, 1, 1, 1, NA, 2, 2, 2, NA, NA, NA, NA, NA), 
  c(NA, 3, 3, 3, 10, 10, 10, 10, 10, 10, 10, 10, 10, 5, 5, 5, NA), 
  c(NA, 3, 3, 3, 10, 10, 10, 10, 10, 10, 10, 10, 10, 5, 5, 5, NA), 
  c(NA, NA, NA, NA, 10, 10, 10, 10, 10, 10, 10, 10, 10, 5, 5, 5, NA), 
  c(NA, 4, 4, 4, 10, 10, 10, 10, 10, 10, 10, 10, 10, NA, NA, NA, NA), 
  c(NA, 4, 4, 4, 10, 10, 10, 10, 10, 10, 10, 10, 10, 6, 6, 6, NA), 
  c(NA, 4, 4, 4, 10, 10, 10, 10, 10, 10, 10, 10, 10, 6, 6, 6, NA), 
  c(NA, NA, NA, NA, 10, 10, 10, 10, 10, 10, 10, 10, 10, 6, 6, 6, NA), 
  c(NA, NA, 7, 7, 7, NA, 8, 8, 8, NA, 9, 9, 9, NA, NA, NA, NA), 
  c(NA, NA, 7, 7, 7, NA, 8, 8, 8, NA, 9, 9, 9, NA, NA, NA, NA), 
  c(NA, NA, 7, 7, 7, NA, 8, 8, 8, NA, 9, 9, 9, NA, NA, NA, NA),
  c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, NA, NA, NA,  NA, NA, NA, NA, NA)
)

###### 4.3.2 Temporal trends & Map - basic source data ###### 

# continuous-material.R in ggsci{}, https://github.com/nanxstats/ggsci/blob/master/R/continuous-material.R
show_col(pal_material("indigo")(10))
show_col(pal_material("red")(10))

scale_fill_material
palette = c("red", "pink", "purple", "deep-purple", "indigo", "blue", 
            "light-blue", "cyan", "teal", "green", "light-green", "lime", 
            "yellow", "amber", "orange", "deep-orange", "brown", "grey", 
            "blue-grey")

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F1-Vehicle")
col_singleSource = "red"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F2-Secondary Nitrate")
col_singleSource = "blue"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F3-Secondary Sulfate")
col_singleSource = "purple"

# annual_singleSource = 
#   subset(annual_source_gps, 
#          Source_use == "F4-Aged Sea Salt")
# col_singleSource = "green"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F5-Industry")
col_singleSource = "orange"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F6-Sea Salt")
col_singleSource = "cyan"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F7-Biomass")
col_singleSource = "pink"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F8-Soil/Dust")
col_singleSource = "brown"


###### 4.3.3 Temporal trends & Map - specific source ######

annual_singleSource_region = merge(annual_singleSource, state_regions)

annual_singleSource_region_contri = 
  annual_singleSource_region %>%
  group_by(region) %>%
  dplyr::summarize(Concentration = mean(Concentration)) 
annual_singleSource_region_contri$geometry = NULL  

regions_dissolved_annual_singleSource =
  merge(regions_dissolved, annual_singleSource_region_contri)

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
  ddply(subset(annual_singleSource_region, Concentration>0), 
        .(region, Year),
        summarise,
        med.contri = median(Concentration),
        up.contri = quantile(Concentration, 0.975),
        down.contri = quantile(Concentration, 0.025),
        Longitude = last(Longitude),
        Latitude = last(Latitude),
        geoid = last(geoid),
        state_abbr = last(state_abbr)
  )

# create the data list, one for each region
annual_singleSource_region_split = split(annual_singleSource_region_plot, 
                                         annual_singleSource_region_plot$region)
max.contri.singleSource = max(annual_singleSource_region_plot$up.contri)

###### 4.3.4 arrange with grid.arrange ######

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
  #geom_text(data = centroid_df, 
  #          aes(x = Longitude, y = Latitude, label = region), 
  #          size = 3, color = "black") +  # Add region names
  theme_minimal() +
  scale_fill_material(col_singleSource) +
  #scale_alpha(guide = "none")+ # alpha for legend
  theme(legend.position = "bottom") +
  theme_map() +
  guides(fill = guide_legend(override.aes = list(alpha = 0.5),
                             title=format_variable("Concentration \n (µg/m3)"))) +
  theme(legend.position = c(0.85, 0.1),  # legend.position="none"
        legend.text = element_text(size = 12), 
        # legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 13), 
        strip.background = element_blank()) # +
# extend margin space to place other figures
# theme(plot.margin = margin(1.5, 1.5, 2, 2, "cm"))
singleSource_map_center

# create the plot list, one for each region
annual_singleSource_list_plots <- 
  lapply(annual_singleSource_region_split, function(x) {
    ggplot(data = x, aes(x = Year, y = med.contri), fill = NA) + 
      geom_line(color = col_singleSource) +
      geom_point(shape = 3) +
      geom_errorbar(aes(ymin = down.contri, ymax = up.contri), width = 0.2) +
      scale_x_continuous(breaks = c(2011, 2014, 2017, 2020)) +
      scale_y_continuous(limits = c(0, max.contri.singleSource)) +  # Set y-axis limits
      labs(title = unique(x$region),
           y = format_variable("Concentration µg/m3")) +
      theme_classic() +
      theme(strip.background = element_blank(),
            plot.title = element_text(hjust = 0.5, vjust = -2),
            axis.title.x = element_text(size = 0),
            axis.text.x = element_text(size = 14, hjust = 0.5, vjust = 0), 
            axis.text.y = element_text(size = 14, hjust = 0.5),
            axis.title.y = element_text(size = 14, hjust = 0.5, angle = 90))
  })
# annual_singleSource_list_plots$`East North Central`

East_North_Central_p = annual_singleSource_list_plots$`East North Central`
East_South_Central_p = annual_singleSource_list_plots$`East South Central`
Mid_Atlantic_p = annual_singleSource_list_plots$`Mid-Atlantic`
Mountain_p = annual_singleSource_list_plots$Mountain
New_England_p = annual_singleSource_list_plots$`New England`
Pacific_p = annual_singleSource_list_plots$Pacific
South_Atlantic_p = annual_singleSource_list_plots$`South Atlantic`
West_North_Central_p = annual_singleSource_list_plots$`West North Central`
West_South_Central_p = annual_singleSource_list_plots$`West South Central`


# Create an empty grob to use as a spacer
spacer_grob <- ggplot() + theme_void()

# Create a list of grobs
grob_list_tempral <- 
  list(
    West_North_Central_grob = ggplotGrob(West_North_Central_p),
    East_North_Central_grob = ggplotGrob(East_North_Central_p),
    Pacific_grob = ggplotGrob(Pacific_p),
    Mountain_grob = ggplotGrob(Mountain_p),
    # New_England_grob = ggplotGrob(New_England_p),
    New_England_grob = spacer_grob, # only for Aged sea salt
    Mid_Atlantic_grob = ggplotGrob(Mid_Atlantic_p),
    West_South_Central_grob = ggplotGrob(West_South_Central_p),
    East_South_Central_grob = ggplotGrob(East_South_Central_p),
    South_Atlantic_grob = ggplotGrob(South_Atlantic_p),
    singleSource_map_center_grob = ggplotGrob(singleSource_map_center)
  )

# Arrange the plots, using grid.arrange based on pre-set grid_layout
grid_layout <- grid.arrange(
  grobs = grob_list_tempral,
  layout_matrix = layout_matrix,
  widths = c(0.2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.2),  # Adjust as needed
  heights = c(0.2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.2)     # Adjust as needed
)

grid_layout

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
  geom_violin()

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

