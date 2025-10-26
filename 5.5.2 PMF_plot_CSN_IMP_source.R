# rm(list=ls())

library(sf)
library(readr)
library(scales)
library(dplyr)
library(plyr)
library(tidyr)
library(fst)
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
library(ggpubr)
library(corrplot)
library(psych)


setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/PMF_results/")
data.dir <- "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/PMF_results/"
getwd()

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


#### 0.1 Process initial source data ####
# # Read PMF result combination from CSN & IMPROVE
# pmf_both = fread("CSN_IMPROVE_source_daily_contribution.csv")
# setDT(pmf_both)
# head(pmf_both); dim(pmf_both)
# unique(pmf_both$Source_aftermanual)
# 
# # Change different traffic/sea salt from CSN & IMPROVE to one traffic/sea salt
# pmf_both <-
#   pmf_both %>%
#   mutate(Source_aftermanual =
#            ifelse(grepl("Traffic", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
#                   "F1-Traffic",
#                   Source_aftermanual))
# 
# pmf_both <-
#   pmf_both %>%
#   mutate(Source_aftermanual =
#            ifelse(grepl("Sea Salt", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
#                   "F6-Salt",
#                   Source_aftermanual))
# 
# pmf_both <-
#   pmf_both %>%
#   mutate(Source_aftermanual =
#            ifelse(grepl("Non-tailpipe", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
#                   "F7-Non-tailpipe",
#                   Source_aftermanual))
# 
# pmf_both <-
#   pmf_both %>%
#   mutate(Source_aftermanual =
#            ifelse(grepl("Industry", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
#                   "F5-Industry",
#                   Source_aftermanual))
# 
# pmf_both <-
#   pmf_both %>%
#   mutate(Source_aftermanual =
#            ifelse(grepl("Biomass", Source_aftermanual, fixed = TRUE), # only apply to character, not factor
#                   "F8-Biomass",
#                   Source_aftermanual))
# 
# unique(pmf_both$Source_aftermanual)
# 
# # Get the year, month
# pmf_both$Date = as.Date(pmf_both$Date)
# pmf_both$Year = year(pmf_both$Date)
# head(pmf_both); class(pmf_both)
# 
# # Get the percent contribution
# pmf_both_perc = pmf_both[, .(
#   Source_aftermanual,
#   Concentration,
#   Percent = Concentration / sum(Concentration) * 100
# ), by = .(SiteCode, Date, Year, Latitude, Longitude)]
# head(pmf_both_perc); dim(pmf_both_perc); dim(pmf_both); 
# summary(pmf_both_perc)
# 
# write_fst(pmf_both_perc, "CSN_IMPROVE_daily_SA_conc_perc.fst")

#### 0.2 Prepare data for plotting ####

# "CSN_IMPROVE_Daily_Source_Impacts_2011-20.csv"
# "CSN_IMPROVE_Daily_PM_2011-20.csv"

# pmf_both_perc = read_fst("CSN_IMPROVE_daily_SA_conc_perc.fst")
pmf_both_perc = fread("CSN_IMPROVE_Daily_Source_Impacts_2011-20.csv")
pm_both = fread("CSN_IMPROVE_Daily_PM_2011-20.csv")

table(pmf_both_perc$Dataset, pmf_both_perc$SiteCode)

pmf_both_perc$V1 = NULL
dim(pmf_both_perc); head(pmf_both_perc) # 1585748  8; 1428020   15
head(pm_both)

all_site_annual_pm = 
  ddply(pm_both, .(Year), summarise,
        PM2.5_pred_org_mean = mean(PM2.5_pred_org),
        PM2.5_pred_org_median = median(PM2.5_pred_org),
        PM2.5_obs_mean = mean(PM2.5_obs),
        PM2.5_obs_median = median(PM2.5_obs))

all_site_overall_pm = 
  ddply(pm_both, .(Dataset, SiteCode, serial.No), summarise,
        PM2.5_pred_org_mean = mean(PM2.5_pred_org),
        PM2.5_pred_org_median = median(PM2.5_pred_org),
        PM2.5_obs_mean = mean(PM2.5_obs),
        PM2.5_obs_median = median(PM2.5_obs))
# View(all_site_overall_pm)

annual_site_overall_pm = 
  ddply(pm_both, .(Dataset, SiteCode, serial.No, Year), summarise,
        PM2.5_pred_org_mean = mean(PM2.5_pred_org),
        PM2.5_pred_org_median = median(PM2.5_pred_org),
        PM2.5_obs_mean = mean(PM2.5_obs),
        PM2.5_obs_median = median(PM2.5_obs))
# View(annual_site_overall_pm)

pm_both_prediction_perform = 
  pm_both %>%
  dplyr::group_by(SiteCode) %>%
  dplyr::summarise(
    r = cor(PM2.5_pred_org, PM2.5_obs),
    R2 = 1 - sum((PM2.5_obs - PM2.5_pred_org)^2) / sum((PM2.5_obs - mean(PM2.5_obs))^2),
    RMSE = sqrt(mean((PM2.5_obs - PM2.5_pred_org)^2)),
    MAE = mean(abs(PM2.5_obs - PM2.5_pred_org)),
    MB = mean(PM2.5_pred_org - PM2.5_obs),
    NMB = mean(PM2.5_pred_org - PM2.5_obs) / mean(PM2.5_obs) * 100,
    .groups = "drop"
  )
summary(pm_both_prediction_perform)

# Double check if there are duplicates
head(pmf_both_perc)

subset(pmf_both_perc, serial.No == 1 & Source_aftermanual == "F3-Secondary Sulfate" & Date == as.Date("2011-01-03"))
subset(pmf_both_perc, serial.No == 157 & Source_aftermanual == "F1-Traffic" & Date == as.Date("2011-01-03"))

pmf_both_perc_dup = pmf_both_perc
pmf_both_perc_dup$dup = duplicated(pmf_both_perc_dup)
summary(pmf_both_perc_dup$dup)

pmf_both_perc_dup$dup_2 = 
  duplicated(
    dplyr::select(pmf_both_perc_dup, 
                  SiteCode, Date, Year, Latitude, Longitude, Source_aftermanual))
summary(pmf_both_perc_dup$dup_2)
subset(pmf_both_perc_dup, SiteCode == "482011039" & Date == as.Date("2011-01-09") & Source_aftermanual == "F1-Traffic")

rm(pmf_both_perc_dup)
rm(pmf_both_perc_dup)
gc()

# Get Month and Source info
head(pmf_both_perc); dim(pmf_both_perc)# 1412996  14
length(unique(pmf_both_perc$SiteCode)) # 264
length(unique(pmf_both_perc$Date)) # 1218

pmf_both_perc$Source = 
  gsub("F[0-9]+-", "", pmf_both_perc$Source_aftermanual)
head(pmf_both_perc)

# Get the total contributions from all sources (PM2.5 itself)
pmf_daily_pm =
  pmf_both_perc %>%
  dplyr::group_by(SiteCode, Date, Year, Month) %>%
  dplyr::summarise(
    Concentration = sum(Concentration),
    Percent = sum(Percent),
    .groups = "drop"
  )
head(pmf_daily_pm)
summary(pmf_daily_pm)
subset(pmf_daily_pm, Percent > 200)
subset(pmf_daily_pm, Percent < -50)

pmf_annual_pm =
  pmf_daily_pm %>%
  dplyr::group_by(SiteCode, Year) %>%
  dplyr::summarise(
    Concentration = median(Concentration),
    Percent = median(Percent),
    .groups = "drop"
  )
head(pmf_annual_pm)
summary(pmf_annual_pm)

# nationwide
pmf_daily_pm %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    Concentration = median(Concentration),
    Percent = median(Percent),
    .groups = "drop"
  )

pmf_daily_pm %>%
  dplyr::group_by(SiteCode) %>%
  dplyr::summarise(
    Concentration = median(Concentration),
    Percent = median(Percent),
    .groups = "drop"
  )

# Nationwide source: annual median & range
Year_naiton_annual <- 
  ddply(pmf_both_perc, 
        .(Year, Source, Source_aftermanual), 
        summarise,
        Conc_mean = mean(Concentration),
        Conc_sd = sd(Concentration),
        Conc_min = min(Concentration),
        Conc_max = max(Concentration),
        Perc_mean = mean(Percent),
        Perc_sd = sd(Percent),
        Perc_min = min(Percent),
        Perc_max = max(Percent))

Year_naiton_2011_20 <-
  subset(Year_naiton_annual, Year %in% c(2011, 2020))

pmf_both_conc_sd_absMean =
  dplyr::select(pmf_both_perc, Dataset, SiteCode, Latitude, Longitude, Source, Date, Concentration) %>%
  dplyr::group_by(Dataset, SiteCode, Latitude, Longitude, Source) %>%
  dplyr::summarise(
    sd_conc = sd(Concentration),
    abs_mean_conc = abs(mean(Concentration)),
    diff_conc = diff(range(Concentration)),
    .groups = "drop"
  )
# estimate the coefficient of variation
pmf_both_conc_sd_absMean$cv_conc =
  pmf_both_conc_sd_absMean$sd_conc/pmf_both_conc_sd_absMean$abs_mean_conc
pmf_both_conc_sd_absMean$cvRange_conc =
  pmf_both_conc_sd_absMean$sd_conc/pmf_both_conc_sd_absMean$diff_conc

pmf_both_conc_sd_absMean_source =
  pmf_both_conc_sd_absMean %>%
  dplyr::group_by(Source) %>%
  dplyr::summarise(
    cv_median = median(cv_conc),
    cv_max = max(cv_conc),
    cv_min = min(cv_conc),
    cv_mean = mean(cv_conc),
    cv_sd = sd(cv_conc),
    cvRange_median = median(cvRange_conc),
    cvRange_max = max(cvRange_conc),
    cvRange_min = min(cvRange_conc),
    cvRange_mean = mean(cvRange_conc),
    cvRange_sd = sd(cvRange_conc),
    .groups = "drop"
  )

# Get annual median & range
Year_aggregated_use <- 
  ddply(pmf_both_perc, 
        .(Dataset, State, SiteCode, Year, Source, Source_aftermanual), 
        summarise,
        Longitude = mean(Longitude),
        Latitude = mean(Latitude),
        Concentration = median(Concentration),
        conc_up = quantile(Concentration, 0.975),
        conc_down = quantile(Concentration, 0.025),
        Percent = median(Percent),
        perc_up = quantile(Percent, 0.975),
        perc_down = quantile(Percent, 0.025))
summary(Year_aggregated_use); head(Year_aggregated_use)

summary(subset(Year_aggregated_use, Source_aftermanual == "F1-Traffic"))
summary(subset(Year_aggregated_use, Source_aftermanual == "F8-Biomass"))

head(Year_aggregated_use)
overall_aggregated_use =
  ddply(Year_aggregated_use, 
        .(Dataset, State, SiteCode, Source), 
        summarise,
        Longitude = mean(Longitude),
        Latitude = mean(Latitude),
        Concentration = median(Concentration),
        conc_up = quantile(Concentration, 0.975),
        conc_down = quantile(Concentration, 0.025),
        Percent = median(Percent),
        perc_up = quantile(Percent, 0.975),
        perc_down = quantile(Percent, 0.025))
head(overall_aggregated_use)
summary(overall_aggregated_use)

# write.csv(overall_aggregated_use, 
#           "CSN_IMPROVE_Overall_Source_Impacts_2011-20.csv")


# Get monthly median & range
Month_aggregated_use <- 
  ddply(pmf_both_perc, 
        .(Dataset, SiteCode, Month, Source, Source_aftermanual), 
        summarise,
        Longitude = mean(Longitude),
        Latitude = mean(Latitude),
        Concentration = median(Concentration),
        conc_up = quantile(Concentration, 0.975),
        conc_down = quantile(Concentration, 0.025),
        Percent = median(Percent),
        perc_up = quantile(Percent, 0.975),
        perc_down = quantile(Percent, 0.025))
summary(Month_aggregated_use); head(Month_aggregated_use)
unique(Month_aggregated_use$Source_aftermanual)


# Get DOW median & range
pmf_both_perc$DOW = weekdays(pmf_both_perc$Date)
head(pmf_both_perc)
DOW_aggregated_use <- 
  ddply(pmf_both_perc, 
        .(SiteCode, DOW, Source, Source_aftermanual), 
        summarise,
        Longitude = mean(Longitude),
        Latitude = mean(Latitude),
        Concentration = median(Concentration),
        conc_up = quantile(Concentration, 0.975),
        conc_down = quantile(Concentration, 0.025),
        Percent = median(Percent),
        perc_up = quantile(Percent, 0.975),
        perc_down = quantile(Percent, 0.025))
summary(DOW_aggregated_use); head(DOW_aggregated_use)
unique(DOW_aggregated_use$Source_aftermanual)

########################################
#### 1. DOW, Monthly & annual, temporal trends ####
########################################

###### Trends 1.1: DOW, median line ######

# the median and 99% of data
DOW_aggregated_summary = 
  ddply(DOW_aggregated_use, 
        .(Source_aftermanual, Source, DOW),
        summarise,
        conc_Lower = quantile(Concentration, 0.0025, na.rm = T),
        conc_Median = median(Concentration, na.rm = T),
        conc_Upper = quantile(Concentration, 0.9975, na.rm = T),
        perc_Lower = quantile(Percent, 0.0025, na.rm = T),
        perc_Median = median(Percent, na.rm = T),
        perc_Upper = quantile(Percent, 0.9975, na.rm = T))

# Generate source for plotting
DOW_plot_data = 
  subset(DOW_aggregated_summary, 
         Source_aftermanual %in% 
           c("F1-Traffic", "F2-Secondary Nitrate", "F3-Secondary Sulfate",
             "F8-Biomass", "F8-Soil/Dust"))
head(DOW_plot_data)
# View(DOW_plot_data)

# Make sure DOW is a factor with proper order
DOW_plot_data$DOW <- 
  factor(DOW_plot_data$DOW, 
         levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                    "Thursday", "Friday", "Saturday"))

# Plotting
line_conc_DOW <-
  ggplot(DOW_plot_data,
         aes(x = DOW, y= conc_Median, color = Source, group = Source)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = color_source_noF) +
  scale_x_discrete(
    limits = c("Sunday", "Monday", "Tuesday", "Wednesday", 
               "Thursday", "Friday", "Saturday")) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = format_variable("Concentration µg/m3")) +
  theme_minimal(base_size = 28) + 
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.5),
    axis.title.x = element_text(vjust = 0, hjust = 0.5))
line_conc_DOW

line_perc_DOW <-
  ggplot(DOW_plot_data,
         aes(x = DOW, y= perc_Median, color = Source, group = Source)) +
  geom_line(size = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = color_source_noF) +
  scale_x_discrete(
    limits = c("Sunday", "Monday", "Tuesday", "Wednesday", 
               "Thursday", "Friday", "Saturday")) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = "Percent %") +
  theme_minimal(base_size = 28)  + 
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.5))
# line_perc_DOW

# combine two figures and show the legend once
line_DOW = 
  ggarrange(line_conc_DOW, NULL, line_perc_DOW, 
            widths = c(1, 0.2, 1), # add space between figure by NULL and widths setting
            nrow=1, align = "v", # Align vertically
            common.legend = TRUE, legend="bottom"
  )
line_DOW

###### Trends 1.2: monthly, median line ######

# the median and 99% of data
Month_aggregated_summary = 
  ddply(Month_aggregated_use, 
        .(Source_aftermanual, Source, Month),
        summarise,
        conc_Lower = quantile(Concentration, 0.0025, na.rm = T),
        conc_Median = median(Concentration, na.rm = T),
        conc_Upper = quantile(Concentration, 0.9975, na.rm = T),
        perc_Lower = quantile(Percent, 0.0025, na.rm = T),
        perc_Median = median(Percent, na.rm = T),
        perc_Upper = quantile(Percent, 0.9975, na.rm = T))
Month_aggregated_summary$Month = as.integer(Month_aggregated_summary$Month)

# Generate source for plotting
month_plot_data = 
  subset(Month_aggregated_summary, 
         Source_aftermanual %in% 
           c("F1-Traffic", "F2-Secondary Nitrate", "F3-Secondary Sulfate",
             "F8-Biomass", "F8-Soil/Dust"))
head(month_plot_data)
# View(month_plot_data)

line_conc_Month <-
  ggplot(month_plot_data,
         aes(x = Month, y= conc_Median, color = Source)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = color_source_noF) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = format_variable("Concentration µg/m3")) +
  theme_minimal(base_size = 28) + 
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 0, vjust = 0.5))
line_conc_Month

line_perc_Month <-
  ggplot(month_plot_data,
         aes(x = Month, y= perc_Median, color = Source)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = color_source_noF) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = "Percent %") +
  theme_minimal(base_size = 28)  + 
  theme(
    panel.grid.minor = element_blank(),
    # legend.position = "none",
    axis.text.x = element_text(angle = 0, vjust = 0.5))
line_perc_Month

# combine two figures and show the legend once
line_Month = 
  ggarrange(line_conc_Month, NULL, line_perc_Month, 
            widths = c(1, 0.2, 1), # add space between figure by NULL and widths setting
            nrow=1, align = "v", # Align vertically
            common.legend = TRUE, legend="bottom"
  )
line_Month


###### Trends 1.2: monthly, IMPROVE vs. CSN median line ######

# the median and 99% of data
Month_aggregated_summary_csnimp = 
  ddply(Month_aggregated_use, 
        .(Dataset, Source_aftermanual, Source, Month),
        summarise,
        conc_Lower = quantile(Concentration, 0.0025, na.rm = T),
        conc_Median = median(Concentration, na.rm = T),
        conc_Upper = quantile(Concentration, 0.9975, na.rm = T),
        perc_Lower = quantile(Percent, 0.0025, na.rm = T),
        perc_Median = median(Percent, na.rm = T),
        perc_Upper = quantile(Percent, 0.9975, na.rm = T))
Month_aggregated_summary_csnimp$Month = as.integer(Month_aggregated_summary_csnimp$Month)

# Generate source for plotting
month_plot_data_csnimp = 
  subset(Month_aggregated_summary_csnimp, 
         Source_aftermanual %in% 
           c("F1-Traffic", "F2-Secondary Nitrate", "F3-Secondary Sulfate",
             "F8-Biomass", "F8-Soil/Dust"))
head(month_plot_data_csnimp)
# View(month_plot_data_csnimp)

line_imp_csn_conc_Month <-
  ggplot(month_plot_data_csnimp,
         aes(x = Month, y= conc_Median, color = Source)) +
  geom_line() +
  geom_point() +
  facet_wrap(Dataset ~., scales = "free", ncol = 2) +
  scale_color_manual(values = color_source_noF) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = format_variable("Concentration µg/m3")) +
  theme_minimal(base_size = 28) + 
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 0, vjust = 0.5))
line_imp_csn_conc_Month

line_imp_csn_perc_Month <-
  ggplot(month_plot_data_csnimp,
         aes(x = Month, y= perc_Median, color = Source)) +
  geom_line() +
  geom_point() +
  facet_wrap(Dataset ~., scales = "free", ncol = 2) +
  scale_color_manual(values = color_source_noF) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = "Percent %") +
  theme_minimal(base_size = 28)  + 
  theme(
    panel.grid.minor = element_blank(),
    # legend.position = "none",
    axis.text.x = element_text(angle = 0, vjust = 0.5))
line_imp_csn_perc_Month

# combine two figures and show the legend once
# line_imp_csn_Month = 
#   ggarrange(line_imp_csn_conc_Month, NULL, line_imp_csn_perc_Month, 
#             widths = c(1, 0.05, 1), # add space between figure by NULL and widths setting
#             nrow=1, align = "v", # Align vertically
#             common.legend = TRUE, legend="bottom"
#   )

line_imp_csn_Month =
  ggarrange(line_imp_csn_conc_Month, line_imp_csn_perc_Month,
            ncol = 1, nrow = 2, # Stack plots vertically with 1 column, 2 rows
            align = "v", # Align vertically
            common.legend = TRUE, legend = "bottom"
  )


line_imp_csn_Month = line_imp_csn_conc_Month / line_imp_csn_perc_Month
line_imp_csn_Month

###### Trends 1.2.1: monthly, area ######

# Data for area
Month_aggregated_area = 
  dplyr::select(Month_aggregated_summary, Source, Month, conc_Median)

# re-estimate the percent contribution based on the conc_median of all sites of each Month
Month_aggregated_area_p <- 
  subset(
    Month_aggregated_area,
    Source %in% c("Traffic", "Secondary Nitrate", "Secondary Sulfate",
                  "Industry", "Salt", "Non-tailpipe", "Biomass", "Soil/Dust")) %>%
  group_by(Month) %>%
  dplyr::mutate(
    conc_sum = sum(conc_Median)) %>%
  ungroup() %>%
  dplyr::mutate(
    perc_reestimated = conc_Median / conc_sum * 100)
head(Month_aggregated_area_p)

# re-arrage for checking
Month_aggregated_area_p = 
  Month_aggregated_area_p[with(Month_aggregated_area_p, 
                              order(Month, Source)), ]
head(Month_aggregated_area_p)

# calculate the contributions for January
contrib_Jan <- 
  subset(Month_aggregated_area_p, Month == 1) 

# arrange by conc
contrib_Jan <-
  contrib_Jan%>%
  # arrange(desc(conc_Median))
  arrange(conc_Median)

# arrange by the conc sequence from annual impacts
contrib_Jan$annual_imp = c(1, 5, 2, 3, 6, 4, 8, 7)
contrib_Jan <-
  contrib_Jan%>%
  # arrange(desc(conc_Median))
  arrange(annual_imp)

contrib_Jan

# reorder the factor levels of Source based on contributions in 1
Month_aggregated_area_p$Source =
  factor(Month_aggregated_area_p$Source, 
         levels = contrib_Jan$Source)

area_conc_Month <-
  ggplot(Month_aggregated_area_p, 
         aes(x = Month, y = conc_Median, fill = Source)) +
  geom_area(alpha = 0.8, position = "stack", color = "grey88") +
  scale_fill_manual(values = color_source_noF) +
  scale_x_continuous(breaks = 1:12) +
  labs(y = format_variable("Concentration µg/m3")) +
  theme_minimal(base_size = 32) + # 28
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 0, vjust = 3.5, hjust =0.5),
    axis.text.y = element_text(vjust = 0.5, hjust = -1))
area_conc_Month

###### Trends 1.2.2: monthly, variance ######
Month_variance <- 
  pmf_both_perc %>%
  dplyr::select(-Year, -DOW, -Date) %>%
  dplyr::group_by(SiteCode, Month, Source, Source_aftermanual) %>%
  dplyr::summarise(
    Longitude = mean(Longitude),
    Latitude = mean(Latitude),
    
    var_conc = var(Concentration),
    mean_conc = mean(Concentration),
    sd_conc = sd(Concentration),
    
    var_perc = var(Percent),
    mean_perc = mean(Percent),
    sd_perc = sd(Percent),
    
    month_day = n(),
    CV_conc = (sd_conc/mean_conc) * 100,
    SE_conc = sd_conc/sqrt(month_day),
    CV_perc = (sd_perc/mean_perc) * 100,
    SE_perc = sd_perc/sqrt(month_day),
    .groups = "drop"
  )
head(Month_variance); dim(Month_variance)
names(Month_variance)

month_variance_conc = 
  dplyr::select(Month_variance, 
                Longitude, Latitude, Month, Source, CV_conc, SE_conc) %>%
  pivot_longer(
    cols = CV_conc:SE_conc,
    values_to = "Values",
    names_to = "Variance_Metric"
  ) 

month_variance_conc = 
  month_variance_conc %>%
  subset(!(Source %in% c("OP-rich")) & # , "Non-tailpipe", "Salt"
           Values < quantile(month_variance_conc$Values, 0.9999, na.rm = TRUE))  %>%
  # Clean up the metric names for display
  mutate(Variance_Metric = case_when(
    Variance_Metric == "SE_conc" ~ "Standard Error",
    Variance_Metric == "CV_conc" ~ "Coefficient of Variation (%)",
    TRUE ~ Variance_Metric
  ))
head(month_variance_conc); dim(month_variance_conc)

# month_variance_conc$Month = as.factor(month_variance_conc$Month)

### Standard Error
ggplot(subset(month_variance_conc, 
              Variance_Metric == "Standard Error" & Values < 2),
       aes(x = factor(Month), y = Values, fill = Source)) +
  geom_boxplot() +
  facet_wrap(. ~ Source, ncol = 4) + # scales = "free_y", 
  scale_fill_manual(values = color_source_noF) +
  # scale_x_continuous(breaks = 1:12) +
  # scale_x_discrete(labels = month.abb) +  # Use month abbreviations instead of numbers
  scale_y_continuous(limits = c(0, NA),
                     breaks = function(x) pretty(x, n = 3)) +
  labs(
    x = "Month",
    y = "Standard Error",
    title = "Standard Error by Source"
  ) +
  theme_minimal(base_size = 22) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1)  # Angled text for better readability
  )

### CV
ggplot(subset(month_variance_conc, 
              Variance_Metric == "Coefficient of Variation (%)" & Values < 1000),
       aes(x = factor(Month), y = Values, fill = Source)) +
  geom_boxplot() +
  facet_wrap(. ~ Source, ncol = 4) + # scales = "free_y", 
  scale_fill_manual(values = color_source_noF) +
  geom_hline(yintercept = 100, 
             color = "grey35", size = 0.8, linetype = "dashed") +
  scale_y_continuous(limits = c(0, NA),
                     breaks = function(x) pretty(x, n = 3)) +
  labs(
    x = "Month",
    y = "Coefficient of Variation (%)",
    title = "Coefficient of Variation (%) by Source"
  ) +
  theme_minimal(base_size = 22) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1)  # Angled text for better readability
  )


##### Median by source & Maps
cv_se_median = 
  month_variance_conc %>%
  dplyr::group_by(Longitude, Latitude, Month, Source, Variance_Metric) %>%
  dplyr::summarise(
    Values = median(Values),
    .groups = "drop"
  )
head(cv_se_median)

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(
    data = subset(cv_se_median, 
                  Variance_Metric == "Standard Error"), 
    mapping = aes(              
      x = Longitude, 
      y = Latitude, 
      fill = Values
    ),
    size = 2.5, 
    alpha = 0.9, 
    shape = 21, 
    color = "grey66"
  ) +
  scale_fill_gradient2(limits = c(0.026, 0.141), # 0.25 & 0.75 quantile
                       low = "grey",  
                       # mid = "ivory",  
                       high = "#2CA02C", 
                       # midpoint = 95,
                       oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  facet_wrap(Source ~., ncol = 3) +
  coord_sf(datum = NA) +
  labs(
    title = "Standard Error by Source, median of the montly SE"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    strip.text = element_text(color = "black", size = 16),
    strip.text.y = element_text(size = 10),
    axis.title = element_text(size = 0),
    legend.text = element_text(size = 14), 
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
    legend.spacing.y = unit(1, "cm")
  )

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(
    data = subset(cv_se_median, 
                  Variance_Metric == "Coefficient of Variation (%)"),
    mapping = aes(              
      x = Longitude, 
      y = Latitude, 
      fill = Values
    ),
    size = 2.5, 
    alpha = 0.9, 
    shape = 21, 
    color = "grey66"
  ) +
  scale_fill_gradient2(limits = c(71, 121), # 0.25 & 0.75 quantile
                       low = "grey",  ##2CA02C
                       # mid = "ivory",  
                       high = "#D62728", 
                       # midpoint = 95,
                       oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  facet_wrap(Source ~., ncol = 3) +
  coord_sf(datum = NA) +
  labs(
    title = "Coefficient of Variation (%) by Source, median of the montly CV"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    strip.text = element_text(color = "black", size = 16),
    strip.text.y = element_text(size = 10),
    axis.title = element_text(size = 0),
    legend.text = element_text(size = 14), 
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
    legend.spacing.y = unit(1, "cm")
  )


#### Median by Month of all sites
cv_se_median_month =
  month_variance_conc %>%
  dplyr::group_by(Month, Source, Variance_Metric) %>%
  dplyr::summarise(
    Values = median(Values),
    .groups = "drop"
  )
unique(cv_se_median_month$Source)

ggplot(subset(cv_se_median_month, 
              Source %in% c("Biomass", "Secondary Nitrate", "Secondary Sulfate",
                            "Soil/Dust", "Traffic")),
       aes(x = Month, y= Values, color = Source)) +
  geom_line() +
  geom_point() +
  facet_wrap(.~ Variance_Metric, scales = "free_y") +
  scale_color_manual(values = color_source_noF) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(limits = c(0, NA)) +
  # labs(y = format_variable("Concentration µg/m3")) +
  theme_minimal(base_size = 28) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.spacing = unit(25, "mm"), 
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, vjust = 0.5))

###### Trends 1.3: Annual, area ######

## the median and 99% of data
# Use the median/averages all site first, then year
Year_aggregated_summary =
  Year_aggregated_use %>%
  subset(Source != "OP-rich") %>%
  dplyr::group_by(Source_aftermanual, Source, Year) %>%
  dplyr::summarise(
    conc_Lower = quantile(Concentration, 0.0025),
    conc_Median = median(Concentration),
    conc_Mean = mean(Concentration),
    conc_Upper = quantile(Concentration, 0.9975),
    perc_Lower = quantile(Percent, 0.0025),
    perc_Median = median(Percent),
    perc_Mean = mean(Percent),
    perc_Upper = quantile(Percent, 0.9975),
    .groups = "drop"
  )

# # Use the overall median/averages of all sites all day instead of site first, then year
# Year_aggregated_summary = 
#   pmf_both_perc %>%
#   subset(Source != "OP-rich") %>%
#   dplyr::group_by(Source_aftermanual, Source, Year) %>%
#   dplyr::summarise(
#     conc_Lower = quantile(Concentration, 0.0025),
#     conc_Median = median(Concentration),
#     conc_Mean = mean(Concentration),
#     conc_Upper = quantile(Concentration, 0.9975),
#     perc_Lower = quantile(Percent, 0.0025),
#     perc_Median = median(Percent),
#     perc_Mean = mean(Percent),
#     perc_Upper = quantile(Percent, 0.9975),
#     .groups = "drop"
#   )
# Year_aggregated_summary$Year = as.integer(Year_aggregated_summary$Year)
head(Year_aggregated_summary)

Year_aggregated_area = 
  dplyr::select(Year_aggregated_summary, Source, Year, conc_Median, conc_Mean)

# Year_aggregated_area = 
#   dplyr::select(Year_aggregated_summary, Source, Year, conc_Median)
head(Year_aggregated_area)
ddply(Year_aggregated_area, .(Year), summarise,
      conc_mean_sum = sum(conc_Mean), conc_median_sum = sum(conc_Median))

# re-estimate the percent contribution based on the conc_median of all sites of each year
Year_aggregated_area_p <- 
  Year_aggregated_area %>%
  group_by(Year) %>%
  dplyr::mutate(
    conc_sum = sum(conc_Mean)) %>%
  ungroup() %>%
  dplyr::mutate(
    perc_reestimated = conc_Mean / conc_sum * 100)

# re-arrage for checking
Year_aggregated_area_p = 
  Year_aggregated_area_p[with(Year_aggregated_area_p, 
                            order(Year, Source)), ]
head(Year_aggregated_area_p)

# calculate the contributions for 2011
contrib_2011 <- 
  subset(Year_aggregated_area_p, Year == 2011) %>%
  # arrange(desc(conc_Median))
  arrange(conc_Median)

# reorder the factor levels of Source based on contributions in 2011
Year_aggregated_area_p$Source =
  factor(Year_aggregated_area_p$Source, 
         levels = contrib_2011$Source)

area_conc_Year <-
  ggplot(Year_aggregated_area_p, 
         aes(x = Year, y = conc_Median, fill = Source)) +
  geom_area(alpha = 0.8, position = "stack", color = "grey88") +
  scale_fill_manual(values = color_source_noF) +
  scale_x_continuous(breaks = 2011:2020) +
  labs(y = format_variable("Concentration µg/m3")) +
  theme_minimal(base_size = 28) + # 32
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text.y = element_text(vjust = 0.5, hjust = 0))
area_conc_Year

area_perc_Year <-
  ggplot(Year_aggregated_area_p, 
         aes(x = Year, y = perc_reestimated, fill = Source)) +
  geom_area(alpha = 0.8, position = "stack", color = "grey88") +
  scale_fill_manual(values = color_source_noF) +
  scale_x_continuous(breaks = 2011:2020) +
  labs(y = "Percent %") +
  theme_minimal(base_size = 28)  + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust = 0.5))
# theme(legend.position = "bottom")

# combine two figures and show the legend once
area_Year = 
  ggarrange(area_conc_Year, NULL, area_perc_Year, 
            widths = c(1, 0.1, 1), # add space between figure by NULL and widths setting
            nrow=1, align = "v", # Align vertically
            common.legend = TRUE, legend="bottom"
  )
area_Year



########################################
############# Annual Change, mapping ############
########################################

#### 2. Thiel-Sen, site level ####

# Detect the appearance times of each site
Year_aggregated_use_unique = data.frame(table(Year_aggregated_use$SiteCode, 
                                              Year_aggregated_use$Source_aftermanual))
names(Year_aggregated_use_unique)[1:2] = c("SiteCode", "Source_aftermanual")
head(Year_aggregated_use_unique)

# Exclude those with <3 appearances
Year_aggregated_ts <- 
  subset(Year_aggregated_use,
         SiteCode %in% 
           unique(subset(Year_aggregated_use_unique, Freq >= 3)$SiteCode))
length(unique(Year_aggregated_ts$SiteCode))
length(unique(Year_aggregated_use$SiteCode))

###### 2.1 Thiel-Sen, the slope for each site across the study period ######
### Thiel-Sen, estimate the slope for each site across the study period 
# concentration-based slope
slope_diff_conc_ts <- 
  Year_aggregated_ts %>%
  dplyr::group_by(SiteCode, Source_aftermanual, Longitude, Latitude) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(cur_data(), "Year", "Concentration"),
    .groups = "drop"
  ) %>%
  ungroup()

# percent-based slope
slope_diff_perc_ts <- 
  Year_aggregated_ts %>%
  group_by(SiteCode, Source_aftermanual, Longitude, Latitude) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(cur_data(), "Year", "Percent"),
    .groups = "drop"
  ) %>%
  ungroup()

slope_diff_conc = slope_diff_conc_ts
slope_diff_perc = slope_diff_perc_ts
summary(slope_diff_conc); summary(slope_diff_perc)

###### 2.2 Dominant source by site, mapping ######
# Group by Longitude and Latitude, then find the row with max Concentration in each group
domain_source_year <- 
  subset(Year_aggregated_ts, Source != "OP-rich") %>% 
  group_by(SiteCode, Year, Longitude, Latitude) %>%
  slice_max(order_by = Concentration, n = 1) %>%
  ungroup()
head(domain_source_year)

domain_source_year %>%
  dplyr::group_by(Source) %>%
  dplyr::summarize(
    total_count = n(),
    count_frac = total_count/length(unique(domain_source_year$SiteCode)) * 100
  )
# write.csv(domain_source_year, "Dominant_source_site_annual_CSN-IMPROVE.csv")

ggplot() +
  geom_sf(data = us_states, fill = "grey98", alpha = 0.8) +
  geom_point(
    data = subset(domain_source_year, Year != 2020),  # moved data argument inside geom_point
    mapping = aes(              # properly wrapped in mapping argument
      x = Longitude, 
      y = Latitude, 
      fill = Source
    ),
    size = 2, 
    alpha = 0.8, 
    shape = 21, 
    color = "grey66"
  ) +
  facet_wrap(Year~., ncol = 3) +
  scale_fill_manual(values = color_source_noF) +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    strip.text = element_text(color = "black", size = 16),
    strip.text.y = element_text(size = 10),
    axis.title = element_text(size = 0),
    legend.text = element_text(size = 14), 
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
    legend.spacing.y = unit(1, "cm")
  )

ggplot() +
  geom_sf(data = us_states, fill = "grey98", alpha = 0.8) +
  geom_point(
    data = subset(domain_source_year, Year %in% c(2011, 2019)),  # moved data argument inside geom_point
    mapping = aes(              # properly wrapped in mapping argument
      x = Longitude, 
      y = Latitude, 
      fill = Source
    ),
    size = 2, 
    alpha = 0.8, 
    shape = 21, 
    color = "grey66"
  ) +
  facet_wrap(Year~., ncol = 2) +
  scale_fill_manual(values = color_source_noF) +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    strip.text = element_text(color = "black", size = 16),
    strip.text.y = element_text(size = 10),
    axis.title = element_text(size = 0),
    legend.text = element_text(size = 14), 
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
    legend.spacing.y = unit(1, "cm")
  )

# Check examples of adjacent sites with different dominant source 
domain_source_year_IN =
  subset(domain_source_year, State == "IN") # Indianan
View(domain_source_year_IN)

domain_source_year_NYC =
  subset(domain_source_year, SiteCode %in% c("360551007", "360610134", "360810124"))
View(domain_source_year_NYC)


# Count of dominate source by year

domain_source_year_conc =
  domain_source_year %>%
  dplyr::select(Year, Source, Concentration, Percent) %>%
  dplyr::group_by(Year, Source) %>%
  dplyr::summarise(
    count = n(),
    Concentration_mean = mean(Concentration), 
    Concentration_sd = sd(Concentration), 
    Percent_mean = mean(Percent), 
    Percent_sd = sd(Percent), 
    .groups = "drop"
  )
View(domain_source_year_conc)

domain_traffic_2019 = unique(subset(domain_source_year, Year == 2019 & Source == "Traffic")$SiteCode)
domain_traffic_2020 = unique(subset(domain_source_year, Year == 2020 & Source == "Traffic")$SiteCode)
domain_dust_2019 = unique(subset(domain_source_year, Year == 2019 & Source == "Soil/Dust")$SiteCode)
domain_dust_2020 = unique(subset(domain_source_year, Year == 2020 & Source == "Soil/Dust")$SiteCode)

domain_traffic_2019[!(domain_traffic_2019 %in% domain_traffic_2020)]
new_site_dust_domain_20 = domain_dust_2020[!(domain_dust_2020 %in% domain_dust_2019)]

# Sites dominated by dust in 2020, the main sources for these sites were:
table(subset(domain_source_year, Year == 2019 & 
               SiteCode %in% new_site_dust_domain_20)$Source)
table(subset(domain_source_year, Year == 2018 & 
               SiteCode %in% new_site_dust_domain_20)$Source)
table(subset(domain_source_year, Year == 2017 & 
               SiteCode %in% new_site_dust_domain_20)$Source)
table(subset(domain_source_year, Year == 2016 & 
               SiteCode %in% new_site_dust_domain_20)$Source)
table(subset(domain_source_year, Year == 2015 & 
               SiteCode %in% new_site_dust_domain_20)$Source)
table(subset(domain_source_year, Year == 2014 & 
               SiteCode %in% new_site_dust_domain_20)$Source)
table(subset(domain_source_year, Year == 2013 & 
               SiteCode %in% new_site_dust_domain_20)$Source)
table(subset(domain_source_year, Year == 2012 & 
               SiteCode %in% new_site_dust_domain_20)$Source)
table(subset(domain_source_year, Year == 2011 & 
               SiteCode %in% new_site_dust_domain_20)$Source)


all_site_year <- 
  domain_source_year_conc %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    total_site_count = sum(count),
    .groups = "drop"
  )

domain_source_year_conc = join(domain_source_year_conc, all_site_year)
domain_source_year_conc$fraction = 
  round(
    domain_source_year_conc$count/domain_source_year_conc$total_site_count*100, 1)

# calculate the occurrence of each site in each source
source_site_count = as.data.frame(t(table(slope_diff_conc$Source_aftermanual)))
source_site_count$Var1 = NULL
colnames(source_site_count)[1] = c("Source_aftermanual")

source_site_count$Source_name = 
  sapply(as.character(source_site_count$Source_aftermanual), function(x) strsplit(x, "-")[[1]][2])

source_site_count$source_site_count = 
  paste0(source_site_count$Source_name, 
         "\nNo. of Sites: ", 
         source_site_count$Freq)

# merge slope diff with site count of each source
slope_diff_conc = merge(slope_diff_conc, source_site_count)
slope_diff_perc = merge(slope_diff_perc, source_site_count)

# Rename sources
slope_diff_conc$Source_name[slope_diff_conc$Source_name == "Non"] = "Non-tailpipe"
head(slope_diff_conc); summary(slope_diff_conc)
unique(slope_diff_conc$Source_name)

slope_diff_perc$Source_name[slope_diff_perc$Source_name == "Non"] = "Non-tailpipe"
head(slope_diff_perc); summary(slope_diff_perc)
unique(slope_diff_perc$Source_name)

### check the distribution of slopes from regression
# slopes from concentration
slope_diff_conc_source = 
  ddply(slope_diff_conc, .(Source_aftermanual), summarise,
        slope_mean = round(mean(diff_slope), 3), 
        slope_sd = round(sd(diff_slope), 3), 
        slope_median = round(median(diff_slope), 3), 
        slope_95th = round(quantile(diff_slope, 0.95), 3), 
        slope_05th = round(quantile(diff_slope, 0.05), 3))

export_table(slope_diff_conc_source, format = "text")

# slopes from percent
slope_diff_perc_source = 
  ddply(slope_diff_perc, .(Source_aftermanual), summarise,
        slope_mean = round(mean(diff_slope), 3), 
        slope_sd = round(sd(diff_slope), 3), 
        slope_median = round(median(diff_slope), 3), 
        slope_95th = round(quantile(diff_slope, 0.95), 3), 
        slope_05th = round(quantile(diff_slope, 0.05), 3))

export_table(slope_diff_perc_source, format = "text")

summary(slope_diff_perc$diff_slope)

###### 2.2.1 Dominant source by site, Traffic ######
domain_year_nonBBSS =
  domain_source_year %>%
  subset(!(Source %in% c("Biomass", "Secondary Sulfate")))
head(domain_year_nonBBSS)

cty_rural_urban = read.csv("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nationwide_SA/data/intermediate/pmf/PMF_progress_files/CSN_IMPROVE/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$Longitude = cty_rural_urban$Latitude = NULL
cty_rural_urban = cty_rural_urban[!duplicated(cty_rural_urban$SiteCode), ] 
head(cty_rural_urban)


domain_source_year_cty =
  merge(domain_source_year, cty_rural_urban, all.x = TRUE, by = "SiteCode")
domain_year_nonBBSS_cty =
  merge(domain_year_nonBBSS, cty_rural_urban, all.x = TRUE, by = "SiteCode")
summary(domain_year_nonBBSS_cty)
table(domain_year_nonBBSS_cty$RuralUrban)
table(domain_year_nonBBSS_cty$state_abbr)

domain_year_traf_cty = 
  subset(domain_year_nonBBSS_cty, Source == "Traffic")
head(domain_year_traf_cty)
table(domain_year_traf_cty$RuralUrban)
table(domain_year_traf_cty$state_abbr)

domain_year_ind_cty = 
  subset(domain_year_nonBBSS_cty, Source == "Industry")
head(domain_year_ind_cty)
table(domain_year_ind_cty$RuralUrban)
table(domain_year_ind_cty$state_abbr)

domain_year_dust_cty = 
  subset(domain_year_nonBBSS_cty, Source == "Soil/Dust")
head(domain_year_dust_cty)
table(domain_year_dust_cty$RuralUrban)
table(domain_year_dust_cty$state_abbr)

domain_year_sn_cty = 
  subset(domain_year_nonBBSS_cty, Source == "Secondary Nitrate")
head(domain_year_sn_cty)
table(domain_year_sn_cty$RuralUrban)
table(domain_year_sn_cty$state_abbr)


table(domain_year_traf_cty$RuralUrban)
table(domain_year_ind_cty$RuralUrban)
table(domain_year_sn_cty$RuralUrban)

table(subset(domain_year_traf_cty, Year == 2011)$RuralUrban)
table(subset(domain_year_traf_cty, Year == 2019)$RuralUrban)

table(subset(domain_year_ind_cty, Year == 2011)$RuralUrban)
table(subset(domain_year_ind_cty, Year == 2019)$RuralUrban)

table(subset(domain_year_dust_cty, Year == 2011)$RuralUrban)
table(subset(domain_year_dust_cty, Year == 2019)$RuralUrban)

table(subset(domain_year_sn_cty, Year == 2011)$RuralUrban)
table(subset(domain_year_sn_cty, Year == 2019)$RuralUrban)

table(subset(domain_source_year_cty, Source == "Secondary Sulfate" & Year == 2011)$RuralUrban)
table(subset(domain_source_year_cty, Source == "Secondary Sulfate" & Year == 2019)$RuralUrban)
table(subset(domain_source_year_cty, Source == "Secondary Sulfate" & Year == 2020)$RuralUrban)

table(subset(domain_source_year_cty, Source == "Biomass" & Year == 2011)$RuralUrban)
table(subset(domain_source_year_cty, Source == "Biomass" & Year == 2019)$RuralUrban)
table(subset(domain_source_year_cty, Source == "Biomass" & Year == 2020)$RuralUrban)

table(subset(domain_source_year_cty, Year == 2011)$RuralUrban)
table(subset(domain_source_year_cty, Year == 2019)$RuralUrban)
table(subset(domain_source_year_cty, Year == 2020)$RuralUrban)

county_year =
  domain_source_year_cty %>%
  dplyr::group_by(Year, RuralUrban) %>%
  dplyr::summarise(
    county_year = n(),
    .groups = "drop"
  )

county_source =
  domain_source_year_cty %>%
  dplyr::group_by(Year, Source) %>%
  dplyr::summarise(
    county_source = n(),
    .groups = "drop"
  )

source_cty = 
  domain_source_year_cty %>%
  dplyr::group_by(Source, Year, RuralUrban) %>%
  dplyr::summarise(
    county_count = n(),
    .groups = "drop"
  )

source_cty_ruralurban =
  join(source_cty, county_year)
source_cty_ruralurban = 
  join(source_cty_ruralurban, county_source)

source_cty_ruralurban$year_county_percent = 
  round(source_cty_ruralurban$county_count/source_cty_ruralurban$county_year * 100, 1)
source_cty_ruralurban$source_county_percent = 
  round(source_cty_ruralurban$county_count/source_cty_ruralurban$county_source * 100, 1)
head(source_cty_ruralurban)

# write.csv(source_cty_ruralurban, "CSN_IMPROVE_source_dominate_site_county_info.csv")

ggplot(source_cty_ruralurban,
       aes(x = as.factor(Year), y = year_county_percent, fill = Source)) +
  geom_col(width = 0.7) +
  facet_wrap(RuralUrban ~., scales = "free", ncol = 3) +
  geom_text(aes(label = county_count), 
            position = position_stack(vjust = 0.5),
            color = "white", 
            size = 3) +
  scale_fill_manual(values = color_source_noF) +
  labs(x = "Year", 
       y = "Percent (%)") +
  theme_base(base_size = 20)   + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5))
  
ggplot(subset(source_cty_ruralurban, Source != "Industry"),
       aes(x = as.factor(Year), y = county_count, fill = RuralUrban)) +
  geom_col(width = 0.7) +
  facet_wrap(Source ~., scales = "free", ncol = 3) +
  geom_text(aes(label = source_county_percent), 
            position = position_stack(vjust = 0.5),
            color = "white", 
            size = 3) +
  # scale_fill_manual(values = color_source_noF) +
  labs(x = "Year", 
       y = "County Count") +
  theme_base(base_size = 20)   + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5))


ggplot() +
  geom_sf(data = us_states, fill = "grey98", alpha = 0.8) +
  geom_point(
    data = subset(domain_year_nonBBSS, Year != 2020),  # moved data argument inside geom_point
    mapping = aes(              # properly wrapped in mapping argument
      x = Longitude, 
      y = Latitude, 
      fill = Source
    ),
    size = 2, 
    alpha = 0.8, 
    shape = 21, 
    color = "grey66"
  ) +
  facet_wrap(Year~., ncol = 3) +
  scale_fill_manual(values = color_source_noF) +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    strip.text = element_text(color = "black", size = 16),
    strip.text.y = element_text(size = 10),
    axis.title = element_text(size = 0),
    legend.text = element_text(size = 14), 
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
    legend.spacing.y = unit(1, "cm")
  )

###### 2.3 Thiel-Sen, plotting, source slope, bar figure ######
slope_diff_conc$Class = "Concentration"
slope_diff_perc$Class = "Percent"

# # Extract Source_name
# slope_diff_conc$Source_name = 
#   sapply(slope_diff_conc$Source_aftermanual, function(x) strsplit(x, "-")[[1]][2])
# slope_diff_perc$Source_name = 
#   sapply(slope_diff_perc$Source_aftermanual, function(x) strsplit(x, "-")[[1]][2])

# Main trends for different sources
slope_diff_conc$Trend = "Mild"
slope_diff_conc$Trend[
  slope_diff_conc$Source_name %in% c("Biomass")] = "B-Increase"
slope_diff_conc$Trend[
  slope_diff_conc$Source_name %in% c("Secondary Sulfate", "Secondary Nitrate")] = "B-Reduce"

head(slope_diff_conc)

slope_diff_perc$Trend = "Mild"
slope_diff_perc$Trend[
  slope_diff_perc$Source_name %in% c("Biomass", "Traffic", "Soil/Dust")] = "B-Increase" # , "Soil/Dust"
slope_diff_perc$Trend[
  slope_diff_perc$Source_name %in% c("Secondary Sulfate", "Salt")] = "B-Reduce"

# Combine dataset
slope_diff_site = 
  rbind(subset(slope_diff_conc, 
               diff_slope < quantile(slope_diff_conc$diff_slope, 0.995) &
                 diff_slope > quantile(slope_diff_conc$diff_slope, 0.005)), 
        subset(slope_diff_perc, 
               diff_slope < quantile(slope_diff_perc$diff_slope, 0.995) &
                 diff_slope > quantile(slope_diff_perc$diff_slope, 0.005)))

slope_diff_site_1 = 
  merge(
    dplyr::select(
    subset(slope_diff_conc, 
           diff_slope < quantile(slope_diff_conc$diff_slope, 0.995) &
             diff_slope > quantile(slope_diff_conc$diff_slope, 0.005)),
    Source_aftermanual, diff_slope, Longitude, Latitude) %>%
      plyr::rename(c("diff_slope" = "diff_slope_conc")), 
    dplyr::select(
      subset(slope_diff_perc, 
             diff_slope < quantile(slope_diff_perc$diff_slope, 0.995) &
               diff_slope > quantile(slope_diff_perc$diff_slope, 0.005)),
      Source_aftermanual, diff_slope, Longitude, Latitude) %>%
      plyr::rename(c("diff_slope" = "diff_slope_frac"))
  )
head(slope_diff_site_1)

slope_diff_site_range <-
  slope_diff_site_1 %>%
  dplyr::group_by(Source_aftermanual) %>%
  dplyr::summarize(
    diff_slope_conc_995 = round(quantile(diff_slope_conc, 0.995), 3),
    diff_slope_conc_med = round(median(diff_slope_conc), 3),
    diff_slope_conc_005 = round(quantile(diff_slope_conc, 0.005), 3),
    diff_slope_conc_mean = round(mean(diff_slope_conc), 3),
    diff_slope_conc_sd = round(sd(diff_slope_conc), 3),
    diff_slope_conc_max = round(max(diff_slope_conc), 3),
    diff_slope_conc_min = round(min(diff_slope_conc), 3),
    diff_slope_frac_995 = round(quantile(diff_slope_frac, 0.995), 3),
    diff_slope_frac_med = round(median(diff_slope_frac), 3),
    diff_slope_frac_005 = round(quantile(diff_slope_frac, 0.005), 3),
    diff_slope_frac_mean = round(mean(diff_slope_frac), 3),
    diff_slope_frac_max = round(max(diff_slope_frac), 3),
    diff_slope_frac_min = round(min(diff_slope_frac), 3),
    diff_slope_frac_sd = round(sd(diff_slope_frac), 3),
    total_count = n()
  )

head(slope_diff_site_range)
# View(slope_diff_site_range)

slope_diff_site_range_conc <-
  select(slope_diff_site_range, 
         Source_aftermanual, 
         diff_slope_conc_995, diff_slope_conc_med, diff_slope_conc_005, 
         diff_slope_conc_mean, diff_slope_conc_sd)
slope_diff_site_range_frac <-
  select(slope_diff_site_range, 
         Source_aftermanual, 
         diff_slope_frac_995, diff_slope_frac_med, diff_slope_frac_005, 
         diff_slope_frac_mean, diff_slope_frac_sd)
# View(slope_diff_site_range_conc)
View(slope_diff_site_range_frac)

# Check the frequency of positive and negative trends
# how many sites experience increase/decrease in source-specific contributions
neg_pos_perc_freq <- 
  slope_diff_perc %>%
  dplyr::group_by(Source_name) %>%
  dplyr::summarize(
    negative_count = sum(diff_slope < 0, na.rm = TRUE),
    positive_count = sum(diff_slope > 0, na.rm = TRUE),
    zero_count = sum(diff_slope == 0, na.rm = TRUE),
    # total_count = n(),
    total_count = first(Freq),
    .groups = "drop"
  )
head(neg_pos_perc_freq)

neg_pos_perc_freq$negative_frac = neg_pos_perc_freq$negative_count/neg_pos_perc_freq$total_count*100
neg_pos_perc_freq$positive_frac = neg_pos_perc_freq$positive_count/neg_pos_perc_freq$total_count*100

subset(neg_pos_perc_freq, 
       Source_name %in% 
         c("Biomass", "Industry", "Salt", "Soil/Dust",
           "Secondary Nitrate", "Secondary Sulfate", "Traffic"))

# Ranges of diff_slope

ggplot(data = subset(slope_diff_site, 
                     !(Source_name %in% c("OP"))), # , "Non-tailpipe", "Salt"
       aes(x = Source_name, y = diff_slope, fill = Trend)) +
  geom_jitter(width = 0.18, alpha = 0.35, color = "gray50") +
  geom_hline(yintercept=0, linetype='dashed', col = "gray25")+
  geom_boxplot(outlier.shape = NA, 
               linewidth = 0.6, width = 0.5, alpha = 0.65) +
  facet_grid(Class ~., scales = "free") +
  scale_fill_manual(values = c("#D95F02", "#1B9E77", "gray45")) + # ("#D95F02", "#2CA02C", "gray45") ("#E41A1C", "#377EB8", "gray45")
  theme_base() +
  theme(axis.text.x = element_text( color = "gray25", size = 20, 
                                    angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = "gray25", size = 20),
        axis.title = element_text(color = "gray25", size = 24),
        strip.text = element_text(color = "gray30", size = 14, face = "bold"), 
        strip.background = element_blank(), # Remove frame lines and background
        panel.spacing.y = unit(0.8, "cm"), # Change space between facet panels
        legend.position = "none")
  

###### 2.4 Thiel-Sen, plotting, spatial distribution ######
# Create the plot
slope_range_conc <- c(-0.2, 0.2)

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = subset(slope_diff_conc, 
                           !(Source_name %in% 
                               c("OP"))), # , "Non-tailpipe", "Salt"
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
  facet_wrap(~ source_site_count, # ~ source_site_count, # source_row, 
             labeller = labeller(source_site_count =
                                   as_labeller(as.character,
                                               default = label_value))) +
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


slope_range_perc <- c(-0.4, 0.4)
# slope_range_perc <- c(-0.2, 0.2)

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = subset(slope_diff_perc, 
                           # !(Source_aftermanual %in% c("F4-Non-tailpipe", "F10-OP-rich"))), 
                           !(Source_name %in% 
                               c("OP"))), # , "Non-tailpipe", "Salt"
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


###### 2.5 Thiel-Sen, BB, sulfate, & other sources: spatial distribution ######
# Group sources as BB, sulfate & others
Year_aggregated_newClass = Year_aggregated_ts
Year_aggregated_newClass$Source_group = Year_aggregated_newClass$Source
Year_aggregated_newClass$Source_group[!(Year_aggregated_newClass$Source %in% c("Biomass", "Secondary Sulfate"))] = "Other Sources"
Year_aggregated_newClass$Source_group[Year_aggregated_newClass$Source == "Biomass"] = "Biomass Burning"
Year_aggregated_newClass$Source_group[Year_aggregated_newClass$Source == "Secondary Sulfate"] = "Sulfate"
table(Year_aggregated_newClass$Source_group)

Year_aggregated_newClass =
  dplyr::select(
    Year_aggregated_newClass, 
    Dataset, State, SiteCode, Year, Source_group, Longitude, Latitude,
    Concentration, Percent) %>%
  dplyr::group_by(
    Dataset, State, SiteCode, Year, Source_group, Longitude, Latitude) %>%
  dplyr::summarise(
    Concentration = sum(Concentration),
    Percent = sum(Percent),
    .groups = "drop"
  )

### Thiel-Sen, estimate the slope for each site across the study period 
# concentration-based slope
slope_diff_conc_ts_newClass <- 
  Year_aggregated_newClass %>%
  dplyr::group_by(SiteCode, Source_group, Longitude, Latitude) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(cur_data(), "Year", "Concentration"),
    .groups = "drop"
  ) %>%
  ungroup()

# percent-based slope
slope_diff_perc_ts_newClass <- 
  Year_aggregated_newClass %>%
  group_by(SiteCode, Source_group, Longitude, Latitude) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(cur_data(), "Year", "Percent"),
    .groups = "drop"
  ) %>%
  ungroup()

# Create the plot
slope_range_conc <- c(-0.2, 0.2)

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = slope_diff_conc_ts_newClass, 
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
  # facet_wrap(~ source_site_count, # ~ source_site_count, # source_row, 
  #            labeller = labeller(source_site_count =
  #                                  as_labeller(as.character,
  #                                              default = label_value))) +
  facet_wrap(~ Source_group) +
  # addline_space, add a break in the text
  labs(fill=addline_space(paste("Changing_rate",
                                format_variable("(µg/m3/year)")))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10),
        axis.title = element_text(size = 0),
        # legend.position = c(0.85, 0.18),
        legend.text = element_text(size = 14), 
        legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
        legend.spacing.y = unit(1, "cm")) 


slope_range_perc <- c(-0.4, 0.4)
# slope_range_perc <- c(-0.2, 0.2)

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = slope_diff_perc_ts_newClass, 
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
  # facet_wrap(~ source_site_count, # ~ source_site_count, # source_row, 
  #            labeller = labeller(source_site_count =
  #                                  as_labeller(as.character,
  #                                              default = label_value))) +
  facet_wrap(~ Source_group) +
  #facet_wrap(~ Source_group, labeller = labeller(source_site_count = custom_labeller)) +
  labs(fill=addline_space(paste("Changing_rate",  
                                format_variable("(%/year)"), ""))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10),
        axis.title = element_text(size = 0),
        # legend.position = c(0.85, 0.18),
        legend.text = element_text(size = 14), 
        legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
        legend.spacing.y = unit(1, "cm")) 


#### 3. Spatial & temporal (annual) ####

cty_rural_urban = read.csv("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nationwide_SA/data/intermediate/pmf/PMF_progress_files/CSN_IMPROVE/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
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

month_contri_gps = merge(Month_aggregated_use, 
                         cty_rural_urban,
                         by = "SiteCode",
                         all.x = T)

month_contri_gps$geoid = ifelse(month_contri_gps$geoid < 10000, 
                                paste0("0", month_contri_gps$geoid), 
                                month_contri_gps$geoid)


###### 3.1 SP Map - common setting for regions ###### 

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

# merge annual/monthly contribution data with geometry
annual_source_gps = merge(annual_contri_gps, us_cty_bdr_geo)
summary(annual_source_gps); head(annual_source_gps)
class(annual_source_gps)

month_source_gps = merge(month_contri_gps, us_cty_bdr_geo)
summary(month_source_gps); head(month_source_gps)
class(month_source_gps)

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


###### 3.1.1 Merge daily source impact with regions ######
head(state_regions)
head(pmf_both_perc)

# extract state region match info
state_regions_use = state_regions
state_regions_use = 
  plyr::rename(state_regions_use,
               c("state_abbr" = "State",
                 "region" = "Region"))
head(state_regions_use)

# Change full spelling state names to abbreviations
pmf_both_perc$State = 
  sapply(pmf_both_perc$State, 
         state_convert_to_abbrev)

# check the results
unique(pmf_both_perc$State)
unique(state_regions_use$State)

length(unique(pmf_both_perc$State))
length(unique(state_regions_use$State))

unique(state_regions_use$State)[!(unique(pmf_both_perc$State) %in% unique(state_regions_use$State))]
unique(state_regions_use$State)[!(unique(state_regions_use$State) %in% unique(pmf_both_perc$State))]

# merge with daily source impacts
pmf_both_perc_region = merge(pmf_both_perc, state_regions_use)
head(pmf_both_perc_region)
dim(pmf_both_perc_region); dim(pmf_both_perc)

write_fst(pmf_both_perc_region, "CSN_IMPROVE_Daily_Source_Impacts_region_2011-20.fst")


pmf_region_conc_sd_absMean =
  dplyr::select(pmf_both_perc_region, Dataset, Region, SiteCode, Latitude, Longitude, Source, Date, Concentration) %>%
  dplyr::group_by(Region, Source) %>%
  dplyr::summarise(
    sd_conc = sd(Concentration),
    abs_mean_conc = abs(mean(Concentration)),
    diff_conc = diff(range(Concentration)),
    .groups = "drop"
  )

# estimate the coefficient of variation
pmf_region_conc_sd_absMean$cv_conc =
  pmf_region_conc_sd_absMean$sd_conc/pmf_region_conc_sd_absMean$abs_mean_conc
pmf_region_conc_sd_absMean$cvRange_conc =
  pmf_region_conc_sd_absMean$sd_conc/pmf_region_conc_sd_absMean$diff_conc

pmf_region_conc_sd_absMean_source =
  pmf_region_conc_sd_absMean %>%
  dplyr::group_by(Source) %>%
  dplyr::summarise(
    cv_median = median(cv_conc),
    cv_max = max(cv_conc),
    cv_min = min(cv_conc),
    cv_mean = mean(cv_conc),
    cv_sd = sd(cv_conc),
    cvRange_median = median(cvRange_conc),
    cvRange_max = max(cvRange_conc),
    cvRange_min = min(cvRange_conc),
    cvRange_mean = mean(cvRange_conc),
    cvRange_sd = sd(cvRange_conc),
    .groups = "drop"
  )

###### 3.1.2 Annual Theil-Sen trend for each source in each area ######
# select columns to use
annual_source_gps_conc_perc = 
  dplyr::select(annual_source_gps, Dataset.x, SiteCode, Year, 
                Source_aftermanual, Source, Longitude, Latitude, Concentration, Percent)
names(annual_source_gps_conc_perc)[1] = "Dataset"
us_states_region_sf = 
  dplyr::select(us_states_region, region, state_abbr, geometry)
head(annual_source_gps_conc_perc)

# convert to sf
annual_source_gps_conc_perc_sf = 
  st_as_sf(annual_source_gps_conc_perc, coords = c("Longitude", "Latitude"), crs = 4326)
  
# Perform the spatial join
annual_source_site <- 
  annual_source_gps_conc_perc_sf %>%
  st_join(us_states_region_sf, join = st_within)

# Site by state
site_state = 
  dplyr::select(annual_source_site, SiteCode, state_abbr)
site_state = site_state[!duplicated(site_state), ]
subset(site_state, state_abbr == "TX")
site_state_count = data.frame(table(site_state$state_abbr))

site_state_source = 
  dplyr::select(annual_source_site, SiteCode, state_abbr, Source_aftermanual, Source)
head(site_state_source)
site_state_source = site_state_source[!duplicated(site_state_source), ]
subset(site_state_source, state_abbr == "TX" & Source_aftermanual == "F1-Traffic")
site_state_source_count = data.frame(table(site_state_source$state_abbr))

site_state_2011 = 
  dplyr::select(annual_source_site, Year, SiteCode, state_abbr, Source_aftermanual, Source) %>%
  subset(Year == 2011)
site_state_2011 = site_state_2011[!duplicated(site_state_2011), ]
subset(site_state_2011, state_abbr == "TX" & Source_aftermanual == "F1-Traffic")
site_state_2011_count = data.frame(table(site_state_2011$state_abbr))

##### 3.1.3 Annual regional, national source contribution estimation ######

# Get regional median
annual_source_region <-
  annual_source_site %>%
  group_by(region, Source, Source_aftermanual, Year) %>%
  dplyr::summarise(
    Concentration = median(Concentration),
    Percent = median(Percent),
    conc_999 = quantile(Concentration, 0.999),
    conc_001 = quantile(Concentration, 0.001),
    perc_999 = quantile(Percent, 0.999),
    perc_001 = quantile(Percent, 0.001),
    .groups = "drop")
head(annual_source_region); dim(annual_source_region)

source_region <-
  annual_source_site %>%
  group_by(region, Source, Source_aftermanual) %>%
  dplyr::summarise(
    Concentration = median(Concentration),
    Percent = median(Percent),
    conc_999 = quantile(Concentration, 0.999),
    conc_001 = quantile(Concentration, 0.001),
    perc_999 = quantile(Percent, 0.999),
    perc_001 = quantile(Percent, 0.001),
    .groups = "drop") %>%
  st_drop_geometry()

source_region_wd =
  dplyr::select(source_region, region, Source, Concentration) %>%
  pivot_wider(
    names_from = "Source",
    values_from = "Concentration"
  )


source_region_dataset <-
  annual_source_site %>%
  group_by(Dataset, region, Source, Source_aftermanual) %>%
  dplyr::summarise(
    Concentration = median(Concentration),
    Percent = median(Percent),
    conc_999 = quantile(Concentration, 0.999),
    conc_001 = quantile(Concentration, 0.001),
    perc_999 = quantile(Percent, 0.999),
    perc_001 = quantile(Percent, 0.001),
    .groups = "drop") %>%
  st_drop_geometry()

source_region_dataset_wd =
  dplyr::select(source_region_dataset, 
                Dataset, region, Source, Concentration) %>%
  pivot_wider(
    names_from = "Source",
    values_from = "Concentration"
  )

subset(annual_source_region, Source == "Secondary Sulfate" & Year == 2011)
subset(annual_source_region, Source == "Biomass" & Year == 2011)

annual_source_nation <-
  annual_source_site %>%
  st_drop_geometry() %>%
  group_by(Source_aftermanual, Source, Year) %>%
  dplyr::summarise(
    Concentration = median(Concentration),
    Percent = median(Percent),
    Conc_mean = mean(Concentration),
    Perc_mean = mean(Percent),
    conc_999 = quantile(Concentration, 0.999),
    conc_001 = quantile(Concentration, 0.001),
    perc_999 = quantile(Percent, 0.999),
    perc_001 = quantile(Percent, 0.001),
    .groups = "drop")
# View(annual_source_nation)

subset(annual_source_nation, Source_aftermanual == "F3-Secondary Sulfate")
subset(annual_source_nation, Source_aftermanual == "F8-Biomass")

annual_source_nation %>%
  st_drop_geometry() %>%
  dplyr::select(Source_aftermanual, Year, Concentration) %>%
  tidyr::pivot_wider(
    names_from = "Year",
    values_from = "Concentration"
  )

annual_source_nation %>%
  st_drop_geometry() %>%
  dplyr::select(Source_aftermanual, Year, Percent) %>%
  tidyr::pivot_wider(
    names_from = "Year",
    values_from = "Percent"
  )

nation_slope_diff_ts <- 
  annual_source_nation %>%
  group_by(Source_aftermanual) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope_conc = 
      round(get_slope_ts(pick(everything()), "Year", "Concentration"), 3),
    diff_slope_perc = 
      round(get_slope_ts(pick(everything()), "Year", "Percent"), 2),
    .groups = "drop"
  ) %>%
  ungroup()
View(nation_slope_diff_ts)

annual_dataset_source_nation <-
  annual_source_site %>%
  st_drop_geometry() %>%
  group_by(Dataset, Source_aftermanual, Source, Year) %>%
  dplyr::summarise(
    Concentration = median(Concentration),
    Percent = median(Percent),
    Conc_mean = mean(Concentration),
    Perc_mean = mean(Percent),
    conc_999 = quantile(Concentration, 0.999),
    conc_001 = quantile(Concentration, 0.001),
    perc_999 = quantile(Percent, 0.999),
    perc_001 = quantile(Percent, 0.001),
    .groups = "drop")

nation_dataset_slope_diff_ts <-
  annual_dataset_source_nation %>%
  group_by(Dataset, Source_aftermanual) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope_conc = 
      round(get_slope_ts(pick(everything()), "Year", "Concentration"), 3),
    diff_slope_perc = 
      round(get_slope_ts(pick(everything()), "Year", "Percent"), 2),
    .groups = "drop"
  ) %>%
  ungroup()
View(nation_dataset_slope_diff_ts)


###### 3.1.2.2 Monthly Theil-Sen trend for each source in each area ######
# select columns to use
month_source_gps_conc_perc = 
  dplyr::select(month_source_gps, SiteCode, Month, 
                Source_aftermanual, Source, Longitude, Latitude, Concentration, Percent)
us_states_region_sf = 
  dplyr::select(us_states_region, region, state_abbr, geometry)

# convert to sf
month_source_gps_conc_perc_sf = 
  st_as_sf(month_source_gps_conc_perc, coords = c("Longitude", "Latitude"), crs = 4326)

# Perform the spatial join
month_source_site <- 
  month_source_gps_conc_perc_sf %>%
  st_join(us_states_region_sf, join = st_within)

# Site by state
site_state = 
  dplyr::select(month_source_site, SiteCode, state_abbr)
site_state = site_state[!duplicated(site_state), ]
subset(site_state, state_abbr == "TX")
site_state_count = data.frame(table(site_state$state_abbr))

site_state_source = 
  dplyr::select(month_source_site, SiteCode, state_abbr, Source_aftermanual, Source)
head(site_state_source)
site_state_source = site_state_source[!duplicated(site_state_source), ]
subset(site_state_source, state_abbr == "TX" & Source_aftermanual == "F1-Traffic")
site_state_source_count = data.frame(table(site_state_source$state_abbr))

site_state_jan = 
  dplyr::select(month_source_site, Month, SiteCode, state_abbr, Source_aftermanual, Source) %>%
  subset(Month == 1)
site_state_jan = site_state_jan[!duplicated(site_state_jan), ]
subset(site_state_jan, state_abbr == "TX" & Source_aftermanual == "F1-Traffic")
site_state_jan_count = data.frame(table(site_state_jan$state_abbr))

##### 3.1.3.2 Monthly regional, national source contribution estimation & dominant sources ######

# Get regional median
month_source_region <-
  month_source_site %>%
  group_by(region, Source, Source_aftermanual, Month) %>%
  dplyr::summarise(
    Concentration = median(Concentration),
    Percent = median(Percent),
    conc_999 = quantile(Concentration, 0.999),
    conc_001 = quantile(Concentration, 0.001),
    perc_999 = quantile(Percent, 0.999),
    perc_001 = quantile(Percent, 0.001),
    .groups = "drop")
head(month_source_region); dim(month_source_region)
# View(month_source_region)

# Contributions of sources ranked by concentration
month_ranked_sources_region <- 
  month_source_region %>%
  group_by(Month, region) %>%
  arrange(desc(Concentration)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# View top source for each region-month
month_top_sources_region <- 
  month_ranked_sources_region %>%
  filter(rank == 1)
month_top_sources_region = month_top_sources_region[with(month_top_sources_region, order(Month, region)), ]
# View(month_top_sources_region)

subset(month_source_region, Source == "Secondary Sulfate" & Month == 1)
subset(month_source_region, Source == "Biomass" & Month == 1)
subset(month_top_sources_region, region == "Southeast")
subset(month_top_sources_region, region == "Mid-Atlantic")


month_source_nation <-
  month_source_site %>%
  st_drop_geometry() %>%
  group_by(Source_aftermanual, Source, Month) %>%
  dplyr::summarise(
    Concentration = median(Concentration),
    Percent = median(Percent),
    Conc_mean = mean(Concentration),
    Perc_mean = mean(Percent),
    conc_999 = quantile(Concentration, 0.999),
    conc_001 = quantile(Concentration, 0.001),
    perc_999 = quantile(Percent, 0.999),
    perc_001 = quantile(Percent, 0.001),
    .groups = "drop")
# View(month_source_nation)

subset(month_source_nation, Source_aftermanual == "F3-Secondary Sulfate")
subset(month_source_nation, Source_aftermanual == "F8-Biomass")

# Contributions of sources ranked by concentration
month_ranked_sources_nation <- 
  month_source_nation %>%
  group_by(Month) %>%
  arrange(desc(Concentration)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# View top source for each nation-month
month_top_sources_nation <- 
  month_ranked_sources_nation %>%
  filter(rank == 1)
month_top_sources_nation = month_top_sources_nation[with(month_top_sources_nation, order(Month)), ]
# View(month_top_sources_nation)

# Assign multipolygon geometry
month_top_sources_region_newGeo = 
  merge(st_drop_geometry(month_top_sources_region), 
        regions_dissolved, by = "region")
head(month_top_sources_region_newGeo)
st_geometry(month_top_sources_region_newGeo) <- "geometry"
# class(month_top_sources_region_newGeo); head(month_top_sources_region_newGeo)

month_top_sources_region_dominate = 
  as.data.frame(
    table(
      month_top_sources_region_newGeo$region, 
      month_top_sources_region_newGeo$Source))
names(month_top_sources_region_dominate) = c("region", "Source", "Freq_dominant_months")

month_top_sources_region_dominate = 
  month_top_sources_region_dominate %>%
  pivot_wider(
    names_from = Source,
    values_from = Freq_dominant_months
  )
names(month_top_sources_region_dominate) = 
  c("region", "Biomass", "OP_rich", "Sec_nitrate", "Sulafte", "Soil_dust")
head(month_top_sources_region_dominate)

ggplot() +
  geom_sf(data = us_states_region, 
          fill = NA, color = "lightgrey") +  # Fill color for states without border
  geom_sf(data = na.omit(month_top_sources_region_newGeo), 
          aes(fill = Source), 
          color = "white", lwd = 1, alpha = 0.5) +  # Thicker borders for regions
  geom_text(data = centroid_df,
            aes(x = Longitude, y = Latitude, label = addline_underline(region_name)),
            size = 2.5, color = "black") +  # Add region names
  facet_wrap(Month~., ncol = 4) +
  theme_minimal(base_size = 20) +
  scale_fill_manual(values = color_source_noF) + 
  theme_map() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 20, face = "bold"),
        legend.position = "bottom")


##### 3.1.3.3 Monthly regional, source-specific check ######
View(month_source_region)
month_region_bb = 
  subset(month_source_region, Source == "Biomass")
month_region_bb$Concentration = round(month_region_bb$Concentration, 2)
month_region_bb_w =
  dplyr::select(month_region_bb, region, Month, Concentration) %>%
  st_drop_geometry() %>%
  pivot_wider(
    names_from = Month,
    values_from = Concentration
  ) %>%
  na.omit()
month_region_bb_w$`NA` = NULL
View(month_region_bb_w)


##### 3.1.4 Thiel-Sen ######
### Thiel-Sen, for site 
# concentration-based slope
# region_slope_diff_conc_ts <- 
#   annual_source_region %>%
#   group_by(region, Source_aftermanual) %>% # Dataset.x, 
#   dplyr::summarize(
#     # `cur_data()` was deprecated in dplyr 1.1.0., use pick() instead
#     diff_slope = get_slope_ts(cur_data(), "Year", "Concentration"), 
#     .groups = "drop"
#   ) %>%
#   ungroup()

region_slope_diff_conc_ts <- 
  annual_source_region %>%
  group_by(region, Source_aftermanual) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(pick(everything()), "Year", "Concentration"),
    .groups = "drop"
  ) %>%
  ungroup()

# # Overall 
# subset(region_slope_diff_conc_ts, Source_aftermanual == "F3-Secondary Sulfate")
# subset(region_slope_diff_conc_ts, Source_aftermanual == "F8-Biomass")
# 
# 
# subset(region_slope_diff_conc_ts, Source_aftermanual == "F1-Traffic")
# subset(region_slope_diff_conc_ts, Source_aftermanual == "F2-Secondary Nitrate")
# subset(region_slope_diff_conc_ts, Source_aftermanual == "F5-Industry")
# subset(region_slope_diff_conc_ts, Source_aftermanual == "F6-Salt")
# subset(region_slope_diff_conc_ts, Source_aftermanual == "F4-Non-tailpipe")
# subset(region_slope_diff_conc_ts, Source_aftermanual == "F8-Soil/Dust")
# subset(region_slope_diff_conc_ts, Source_aftermanual == "F9-OP-rich")


# percent-based slope
region_slope_diff_perc_ts <- 
  annual_source_region %>%
  group_by(region, Source_aftermanual) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(pick(everything()), "Year", "Percent"),
    .groups = "drop"
  ) %>%
  ungroup()

subset(region_slope_diff_perc_ts, Source_aftermanual == "F3-Secondary Sulfate")
subset(region_slope_diff_perc_ts, Source_aftermanual == "F8-Biomass")


# subset(region_slope_diff_perc_ts, Source_aftermanual == "F1-Traffic")
# subset(region_slope_diff_perc_ts, Source_aftermanual == "F2-Secondary Nitrate")
# subset(region_slope_diff_perc_ts, Source_aftermanual == "F5-Industry")
# subset(region_slope_diff_perc_ts, Source_aftermanual == "F6-Salt")
# subset(region_slope_diff_perc_ts, Source_aftermanual == "F7-Non-tailpipe")
# subset(region_slope_diff_perc_ts, Source_aftermanual == "F9-Soil/Dust")
# subset(region_slope_diff_perc_ts, Source_aftermanual == "F10-OP-rich")

region_slope_diff_conc_ts_1 = region_slope_diff_conc_ts
region_slope_diff_perc_ts_1 = region_slope_diff_perc_ts

# Rbind
region_slope_diff_conc_ts$Class = "Concentration"
region_slope_diff_perc_ts$Class = "Percent"

region_slope_diff = rbind(region_slope_diff_conc_ts, region_slope_diff_perc_ts)
head(region_slope_diff)

region_slope_diff_conc_ts_use = 
  st_drop_geometry(region_slope_diff_conc_ts) %>%
  dplyr::select(-Class) %>%
  dplyr::mutate(diff_slope = round(diff_slope, 4)) %>%
  tidyr::pivot_wider(
    names_from = "Source_aftermanual",
    values_from = "diff_slope"
  )
# View(region_slope_diff_conc_ts_use)

region_slope_diff_perc_ts_use = 
  st_drop_geometry(region_slope_diff_perc_ts) %>%
  dplyr::select(-Class) %>%
  dplyr::mutate(diff_slope = round(diff_slope, 4)) %>%
  tidyr::pivot_wider(
    names_from = "Source_aftermanual",
    values_from = "diff_slope"
  )
# View(region_slope_diff_perc_ts_use)

# Merge
names(region_slope_diff_conc_ts_1)[3] = "diff_slope_conc"
names(region_slope_diff_perc_ts_1)[3] = "diff_slope_frac"
region_slope_diff_1 = 
  merge(
    st_drop_geometry(region_slope_diff_conc_ts_1), 
    st_drop_geometry(region_slope_diff_perc_ts_1))
region_slope_diff_1 = na.omit(region_slope_diff_1)
# View(region_slope_diff_1)

region_slope_diff_1 %>%
  dplyr::group_by(Source_aftermanual) %>%
  dplyr::summarize(
    diff_slope_conc_max = max(diff_slope_conc),
    diff_slope_conc_min = min(diff_slope_conc),
    diff_slope_frac_max = max(diff_slope_frac),
    diff_slope_frac_min = min(diff_slope_frac)
  )

# write.csv(region_slope_diff, "CSN_IMPROVE_region_Theil-Sen.csv")
# write.csv(region_slope_diff_1, "CSN_IMPROVE_region_Theil-Sen_another_view.csv") # To use for table in supplement

write.csv(region_slope_diff, "CSN_IMPROVE_region_Theil-Sen_2025.05.csv")
write.csv(region_slope_diff_1, "CSN_IMPROVE_region_Theil-Sen_another_view_2025.05.csv") # To use for table in supplement

###### 3.2. Dominant source by region - annual ###### 

# Get the dataset to use
names(annual_source_region)

annual_source_region$Source = 
  gsub("F[0-9]+-", "", annual_source_region$Source_aftermanual)
annual_source_region$Source_aftermanual = NULL

annual_source_region_noOP = 
  subset(annual_source_region, !(Source %in% c("OP-rich"))) 
  # dplyr::mutate(
  #   Source = gsub("Secondary Nitrate", "Secondary_Nitrate", Source),
  #   Source = gsub("Secondary Sulfate", "Secondary_Sulfate", Source),
  #   Source = gsub("Soil/Dust", "Soil_Dust", Source),
  #   Source = gsub("Non-tailpipe", "Non_tailpipe", Source)) 
head(annual_source_region_noOP)
unique(annual_source_region_noOP$Source)

# Get the dominated source by year by region
domain_source_region <-
  annual_source_region_noOP %>%
  dplyr::group_by(region, Year) %>%
  slice_max(order_by = Concentration, n = 1) %>%
  dplyr::select(region, Year, Source, Concentration, Percent, conc_999, conc_001, perc_999, perc_001) %>%
  ungroup() %>%
  st_drop_geometry() %>%# remove multipoint geometry
  na.omit()
head(domain_source_region)

subset(annual_source_region_noOP, Source == "Biomass") %>% st_drop_geometry()
subset(annual_source_region_noOP, Source == "Secondary Sulfate") %>% st_drop_geometry()

subset(annual_source_region_noOP, Source == "Biomass" & Year == 2011) %>% st_drop_geometry()
subset(annual_source_region_noOP, Source == "Secondary Sulfate" & Year == 2011) %>% st_drop_geometry()

total_PM_region <-
  annual_source_region_noOP %>%
  dplyr::group_by(region, Year) %>%
  dplyr::summarise(Concentration = sum(Concentration)) %>%
  ungroup() %>%
  st_drop_geometry() # remove multipoint geometry
head(total_PM_region)

# Assign multipolygon geometry
domain_source_region = merge(domain_source_region, regions_dissolved)
st_geometry(domain_source_region) <- "geometry"
class(domain_source_region); head(domain_source_region)

total_PM_region = merge(total_PM_region, regions_dissolved)
st_geometry(total_PM_region) <- "geometry"
class(total_PM_region); head(total_PM_region)

ggplot() +
  geom_sf(data = us_states_region, 
          fill = NA, color = "lightgrey") +  # Fill color for states without border
  geom_sf(data = na.omit(subset(domain_source_region, 
                                !(Year %in% c(2020)))), 
          aes(fill = Source), 
          color = "white", lwd = 2, alpha = 0.5) +  # Thicker borders for regions
  geom_text(data = centroid_df,
            aes(x = Longitude, y = Latitude, label = addline_underline(region_name)),
            size = 3, color = "black") +  # Add region names
  facet_wrap(Year~., ncol = 3) +
  theme_minimal(base_size = 20) +
  scale_fill_manual(values = color_source_noF) + 
  theme_map() +
  guides(fill = guide_colorbar(override.aes = list(alpha = 0.5),
                               title=format_variable("Concentration \n(µg/m3)"))) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 20, face = "bold"),
        legend.position = "bottom")


ggplot() +
  geom_sf(data = us_states_region, 
          fill = NA, color = "lightgrey") +  # Fill color for states without border
  geom_sf(data = na.omit(subset(total_PM_region, 
                                !(Year %in% c(2020)))), 
          aes(fill = Concentration), 
          color = "white", lwd = 2, alpha = 0.5) +  # Thicker borders for regions
  geom_text(data = centroid_df,
            aes(x = Longitude, y = Latitude, label = addline_underline(region_name)),
            size = 3, color = "black") +  # Add region names
  facet_wrap(Year~., ncol = 3) +
  theme_minimal(base_size = 20) +
  # scale_fill_manual(values = color_source_noF) + 
  theme_map() +
  guides(fill = guide_colorbar(override.aes = list(alpha = 0.5),
                               title=format_variable("Concentration \n(µg/m3)"))) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 20, face = "bold"),
        legend.position = "bottom")


###### 3.3 Annual, site-region, rural urban ###### 

annual_allSource_region = merge(annual_source_gps, state_regions)
head(annual_allSource_region); dim(annual_allSource_region) # 14542  23

annual_allSource_region = plyr::rename(
  annual_allSource_region,
  c("Dataset.x" = "Dataset")
)

annual_site_source_region =
  st_drop_geometry(annual_allSource_region) %>%
  dplyr::select(Dataset, SiteCode, Longitude, Latitude, Year, 
         Source, Concentration, Percent, RuralUrban, state_name, region)

ggplot(subset(annual_site_source_region, 
              Source != "OP-rich" & 
                Concentration < quantile(annual_site_source_region$Concentration, 0.999)),
       aes(x = Source, y = Concentration, fill = RuralUrban)) +
  geom_boxplot() +
  labs(y = paste("Concentration, ",format_variable("µg/m3"), "")) +
  theme_base(base_size = 24) +
  scale_fill_manual(values = c("#D95F02", "#1B9E77", "gray45")) + # ("#D95F02", "#2CA02C", "gray45") ("#E41A1C", "#377EB8", "gray45")
  theme(axis.text.x = element_text( color = "gray25", 
                                    angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = "gray25"),
        axis.title = element_text(color = "gray25"),
        strip.text = element_text(color = "gray30", face = "bold"), 
        strip.background = element_blank(), # Remove frame lines and background
        panel.spacing.y = unit(0.8, "cm"), # Change space between facet panels
        legend.position = "bottom")

ggplot(subset(annual_site_source_region, Source != "OP-rich"),
       aes(x = Source, y = Percent, fill = RuralUrban)) +
  geom_boxplot() +
  labs(y = paste("Percent, ",format_variable("%"), "")) +
  theme_base(base_size = 24) +
  scale_fill_manual(values = c("#D95F02", "#1B9E77", "gray45")) + # ("#D95F02", "#2CA02C", "gray45") ("#E41A1C", "#377EB8", "gray45")
  theme(axis.text.x = element_text( color = "gray25", 
                                    angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = "gray25"),
        axis.title = element_text(color = "gray25"),
        strip.text = element_text(color = "gray30", face = "bold"), 
        strip.background = element_blank(), # Remove frame lines and background
        panel.spacing.y = unit(0.8, "cm"), # Change space between facet panels
        legend.position = "bottom")

# Check mid-Atlantic
annual_site_source_midAtlantic = 
  subset(annual_site_source_region, region == "Mid-Atlantic")
annual_site_midAtlantic =
  annual_site_source_midAtlantic %>%
  select(Dataset, SiteCode, state_name, region, RuralUrban)
annual_site_midAtlantic = annual_site_midAtlantic[!duplicated(annual_site_midAtlantic), ]
annual_site_midAtlantic
table(annual_site_midAtlantic$RuralUrban)

# Check Southeast
annual_site_source_Southeast = 
  subset(annual_site_source_region, region == "Southeast")
annual_site_Southeast =
  annual_site_source_Southeast %>%
  select(Dataset, SiteCode, state_name, region, RuralUrban)
annual_site_Southeast = annual_site_Southeast[!duplicated(annual_site_Southeast), ]
annual_site_Southeast
table(annual_site_Southeast$RuralUrban)

# Check Pacific Northwest
annual_site_source_pacificNW = 
  subset(annual_site_source_region, region == "Pacific Northwest")
annual_site_pacificNW =
  annual_site_source_pacificNW %>%
  select(Dataset, SiteCode, state_name, region, RuralUrban)
annual_site_pacificNW = annual_site_pacificNW[!duplicated(annual_site_pacificNW), ]
annual_site_pacificNW
table(annual_site_pacificNW$RuralUrban)

###### 3.3.1 Annual rural urban, Theil-Sen, across all rural or urban sites ###### 

# concentration-based slope
ruralurban_slope_diff_conc_ts <- 
  subset(annual_site_source_region, Source != "OP-rich") %>%
  group_by(RuralUrban, Source) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(cur_data(), "Year", "Concentration"),
    .groups = "drop"
  ) %>%
  ungroup()
ruralurban_slope_diff_conc_ts$Class = "Concentration"

ruralurban_slope_diff_perc_ts <- 
  subset(annual_site_source_region, Source != "OP-rich") %>%
  group_by(RuralUrban, Source) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(cur_data(), "Year", "Percent"),
    .groups = "drop"
  ) %>%
  ungroup()
ruralurban_slope_diff_perc_ts$Class = "Percent"

ruralurban_slope_diff_ts = 
  rbind(ruralurban_slope_diff_conc_ts, ruralurban_slope_diff_perc_ts)

ggplot(ruralurban_slope_diff_conc_ts,
       aes(x = Source, y = diff_slope, fill = RuralUrban)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y = paste("Changing Rate, ",format_variable("µg/m3/year"), "")) +
  theme_base(base_size = 24) +
  scale_fill_manual(values = c("#D95F02", "#1B9E77", "gray45")) + # ("#D95F02", "#2CA02C", "gray45") ("#E41A1C", "#377EB8", "gray45")
  theme(axis.text.x = element_text( color = "gray25", 
                                    angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = "gray25"),
        axis.title = element_text(color = "gray25"),
        strip.text = element_text(color = "gray30", face = "bold"), 
        strip.background = element_blank(), # Remove frame lines and background
        panel.spacing.y = unit(0.8, "cm"), # Change space between facet panels
        legend.position = "bottom")

ggplot(ruralurban_slope_diff_perc_ts,
       aes(x = Source, y = diff_slope, fill = RuralUrban)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y = paste("Changing Rate, ",format_variable("%/year"), "")) +
  theme_base(base_size = 24) +
  scale_fill_manual(values = c("#D95F02", "#1B9E77", "gray45")) + # ("#D95F02", "#2CA02C", "gray45") ("#E41A1C", "#377EB8", "gray45")
  theme(axis.text.x = element_text( color = "gray25", 
                                    angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = "gray25"),
        axis.title = element_text(color = "gray25"),
        strip.text = element_text(color = "gray30", face = "bold"), 
        strip.background = element_blank(), # Remove frame lines and background
        panel.spacing.y = unit(0.8, "cm"), # Change space between facet panels
        legend.position = "bottom")

###### 3.3.2 Annual rural urban, Theil-Sen, grouped by rural, mix, or urban ###### 

# Site info, GPS assigned earlier
cty_rural_urban = fread("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Intp_IMPROVE_CSN/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$V1 = NULL
cty_rural_urban = cty_rural_urban[!duplicated(cty_rural_urban$SiteCode), ]
head(cty_rural_urban); dim(cty_rural_urban)

cty_rural_urban_site =
  dplyr::select(cty_rural_urban, SiteCode, RuralUrban, state_abbr)
head(cty_rural_urban_site)

slope_diff_conc_cty =
  join(slope_diff_conc, cty_rural_urban_site)
head(slope_diff_conc_cty); dim(slope_diff_conc_cty)

slope_diff_perc_cty =
  join(slope_diff_perc, cty_rural_urban_site)
head(slope_diff_perc_cty); dim(slope_diff_perc_cty)

ggplot(subset(slope_diff_conc_cty, 
              Source_name != "OP" &
                diff_slope > quantile(slope_diff_conc_cty$diff_slope, 0.0025) &
                diff_slope < quantile(slope_diff_conc_cty$diff_slope, 0.9975)),
       aes(x = Source_name, y = diff_slope, fill = RuralUrban)) +
  geom_boxplot() +
  labs(y = paste("Changing Rate, ",format_variable("µg/m3/year"), "")) +
  theme_base(base_size = 24) +
  scale_fill_manual(values = c("#D95F02", "#1B9E77", "gray45")) + # ("#D95F02", "#2CA02C", "gray45") ("#E41A1C", "#377EB8", "gray45")
  theme(axis.text.x = element_text( color = "gray25", 
                                    angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = "gray25"),
        axis.title = element_text(color = "gray25"),
        strip.text = element_text(color = "gray30", face = "bold"), 
        strip.background = element_blank(), # Remove frame lines and background
        panel.spacing.y = unit(0.8, "cm"), # Change space between facet panels
        legend.position = "bottom")

ggplot(subset(slope_diff_perc_cty, 
              Source_name != "OP" &
                diff_slope > quantile(slope_diff_perc_cty$diff_slope, 0.0025) &
                diff_slope < quantile(slope_diff_perc_cty$diff_slope, 0.9975)),
       aes(x = Source_name, y = diff_slope, fill = RuralUrban)) +
  geom_boxplot() +
  labs(y = paste("Changing Rate, ",format_variable("%/year"), "")) +
  theme_base(base_size = 24) +
  scale_fill_manual(values = c("#D95F02", "#1B9E77", "gray45")) + # ("#D95F02", "#2CA02C", "gray45") ("#E41A1C", "#377EB8", "gray45")
  theme(axis.text.x = element_text( color = "gray25", 
                                    angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = "gray25"),
        axis.title = element_text(color = "gray25"),
        strip.text = element_text(color = "gray30", face = "bold"), 
        strip.background = element_blank(), # Remove frame lines and background
        panel.spacing.y = unit(0.8, "cm"), # Change space between facet panels
        legend.position = "bottom")


###### 3.4.0 SP Map - basic source data, color define ###### 

# continuous-material.R in ggsci{}, https://github.com/nanxstats/ggsci/blob/master/R/continuous-material.R
show_col(pal_material("indigo")(10))
show_col(pal_material("red")(10))

annual_singleSource = 
  subset(annual_source_gps, 
         Source_aftermanual == "F1-Traffic")
col_singleSource = "red"
col_singleSource_line = "#C71000FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_aftermanual == "F1-Traffic-Diesel")
col_singleSource = "#FF95A8FF"
col_singleSource_line = "#FF95A8FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_aftermanual == "F1-Traffic-Gasoline")
col_singleSource = "#FF410DFF"
col_singleSource_line = "#FF410DFF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_aftermanual == "F1-Traffic-ResOil")
col_singleSource = "#FB6467FF"
col_singleSource_line = "#FB6467FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_aftermanual == "F2-Secondary Nitrate")
col_singleSource = "blue"
col_singleSource_line = "#0073C2FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_aftermanual == "F3-Secondary Sulfate")
col_singleSource = "green" # green
col_singleSource_line = "#16A085FF"

annual_singleSource =
  subset(annual_source_gps,
         Source_aftermanual == "F4-Non-tailpipe")
col_singleSource = "yellow"
col_singleSource_line = "#F7C530FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_aftermanual == "F5-Industry")
col_singleSource = "orange"
col_singleSource_line = "#E89242FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_aftermanual == "F6-Salt")
col_singleSource = "cyan"
col_singleSource_line = "#00B5E2FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_aftermanual == "F8-Biomass")
col_singleSource = "purple"
col_singleSource_line = "#8A4198FF"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_aftermanual == "F8-Soil/Dust")
col_singleSource = "grey"
col_singleSource_line = "grey40"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_aftermanual == "F9-OP-rich")
col_singleSource = "#5A9599FF"
col_singleSource_line = "#5A9599FF"


###### 3.4.1 SP Map - Concentration - contribution trend estimation ######

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

a = subset(annual_singleSource_region_plot, region == "East North Central")

###### 3.4.2 SP Map - Concentration - plotting ######

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
  # scale_fill_material(col_singleSource, limits = c(0, NA)) +
  scale_fill_gradient(low = "white", high = col_singleSource_line, limits = c(0, NA)) + # fill starts from 0
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

# Create a blank plot function
blank_plot <- function() {
  ggplot() + 
    geom_blank() + 
    theme_void()
}

# Initialize a list of grobs with considering the potential missing ones
grob_list_tempral <- list(
  New_England_grob = if(!is.null(New_England_p)) 
    ggplotGrob(New_England_p) else ggplotGrob(blank_plot()), # 1
  New_York_Jersey_grob = if(!is.null(New_York_Jersey_p)) 
    ggplotGrob(New_York_Jersey_p) else ggplotGrob(blank_plot()), # 2
  Mid_Atlantic_grob = if(!is.null(Mid_Atlantic_p)) 
    ggplotGrob(Mid_Atlantic_p) else ggplotGrob(blank_plot()), # 3
  Southeast_grob = if(!is.null(Southeast_p)) 
    ggplotGrob(Southeast_p) else ggplotGrob(blank_plot()), # 4
  East_North_Central_grob = if(!is.null(East_North_Central_p)) 
    ggplotGrob(East_North_Central_p) else ggplotGrob(blank_plot()), # 5
  South_Central_grob = if(!is.null(South_Central_p)) 
    ggplotGrob(South_Central_p) else ggplotGrob(blank_plot()), # 6
  #Midwest_grob = if(!is.null(Midwest_p)) ggplotGrob(Midwest_p) else ggplotGrob(blank_plot()), # 7
  Mountains_Plains_grob = if(!is.null(Mountains_Plains_p)) 
    ggplotGrob(Mountains_Plains_p) else ggplotGrob(blank_plot()), # 8
  Pacific_Southwest_grob = if(!is.null(Pacific_Southwest_p)) 
    ggplotGrob(Pacific_Southwest_p) else ggplotGrob(blank_plot()), # 9
  Pacific_Northwest_grob = if(!is.null(Pacific_Northwest_p)) 
    ggplotGrob(Pacific_Northwest_p) else ggplotGrob(blank_plot()), # 10
  singleSource_map_center_grob = ggplotGrob(singleSource_map_center) # central map
)

# Arrange the plots, using grid.arrange based on pre-set grid_layout
grid_layout_concentration <- grid.arrange(
  grobs = grob_list_tempral,
  layout_matrix = layout_matrix,
  widths = c(0.2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.3, 1, 1, 1, 0.3),  # Adjust as needed
  heights = c(0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.1)     # Adjust as needed
)

grid_layout_concentration

###### 3.5.1 SP Map - Percent - contribution trend estimation ######

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

a = subset(annual_singleSource_region_plot, region == "East North Central")


###### 3.5.2 SP Map - Percent - plotting ######

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
  # scale_fill_material(col_singleSource, limits = c(0, NA)) +
  scale_fill_gradient(low = "white", high = col_singleSource_line, limits = c(0, NA)) + # fill starts from 0
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

# Create a blank plot function
blank_plot <- function() {
  ggplot() + 
    geom_blank() + 
    theme_void()
}

# Initialize a list of grobs with considering the potential missing ones
grob_list_tempral <- list(
  New_England_grob = if(!is.null(New_England_p)) 
    ggplotGrob(New_England_p) else ggplotGrob(blank_plot()), # 1
  New_York_Jersey_grob = if(!is.null(New_York_Jersey_p)) 
    ggplotGrob(New_York_Jersey_p) else ggplotGrob(blank_plot()), # 2
  Mid_Atlantic_grob = if(!is.null(Mid_Atlantic_p)) 
    ggplotGrob(Mid_Atlantic_p) else ggplotGrob(blank_plot()), # 3
  Southeast_grob = if(!is.null(Southeast_p)) 
    ggplotGrob(Southeast_p) else ggplotGrob(blank_plot()), # 4
  East_North_Central_grob = if(!is.null(East_North_Central_p)) 
    ggplotGrob(East_North_Central_p) else ggplotGrob(blank_plot()), # 5
  South_Central_grob = if(!is.null(South_Central_p)) 
    ggplotGrob(South_Central_p) else ggplotGrob(blank_plot()), # 6
  #Midwest_grob = if(!is.null(Midwest_p)) ggplotGrob(Midwest_p) else ggplotGrob(blank_plot()), # 7
  Mountains_Plains_grob = if(!is.null(Mountains_Plains_p)) 
    ggplotGrob(Mountains_Plains_p) else ggplotGrob(blank_plot()), # 8
  Pacific_Southwest_grob = if(!is.null(Pacific_Southwest_p)) 
    ggplotGrob(Pacific_Southwest_p) else ggplotGrob(blank_plot()), # 9
  Pacific_Northwest_grob = if(!is.null(Pacific_Northwest_p)) 
    ggplotGrob(Pacific_Northwest_p) else ggplotGrob(blank_plot()), # 10
  singleSource_map_center_grob = ggplotGrob(singleSource_map_center) # central map
)

# Arrange the plots, using grid.arrange based on pre-set grid_layout
grid_layout_fraction <- grid.arrange(
  grobs = grob_list_tempral,
  layout_matrix = layout_matrix,
  widths = c(0.2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.3, 1, 1, 1, 0.3),  # Adjust as needed
  heights = c(0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.1)     # Adjust as needed
)

grid_layout_fraction


#### 4. Correlations ####
head(pmf_both_perc); sapply(pmf_both_perc, class)
unique(pmf_both_perc$Source)

# Get data for use & change some names
pmf_both_perc_coor_use = 
  subset(pmf_both_perc, !(Source %in% c("OP-rich"))) %>%
  dplyr::select(SiteCode, Date, Source, Concentration) %>%
  dplyr::mutate(
    Source = gsub("Secondary Nitrate", "Secondary_Nitrate", Source),
    Source = gsub("Secondary Sulfate", "Secondary_Sulfate", Source),
    Source = gsub("Soil/Dust", "Soil_Dust", Source),
    Source = gsub("Non-tailpipe", "Non_tailpipe", Source)) 
head(pmf_both_perc_coor_use)

# Check the duplicates
pmf_both_perc_coor_use$dup =
  duplicated(
    dplyr::select(pmf_both_perc_coor_use, SiteCode, Date, Source)) 
summary(pmf_both_perc_coor_use$dup)
pmf_both_perc_coor_use$dup = NULL

# pmf_perc_coor_dup = subset(pmf_both_perc_coor_use, dup)
# unique(pmf_perc_coor_dup$SiteCode)
# unique(pmf_perc_coor_dup$Date)

# Change to format for plotting
pmf_perc_coor_plot <-
  pmf_both_perc_coor_use %>%
  pivot_wider(
    names_from = Source, 
    values_from = Concentration)

head(pmf_perc_coor_plot)
sapply(pmf_perc_coor_plot, class)

# Drop SiteCode and Date for correlation
pmf_perc_conc_coor_num <- 
  dplyr::select(pmf_perc_coor_plot, -SiteCode, -Date)

# Calculate correlation, relevance, and number of complete cases matrix
pmf_perc_conc_cor <- 
  psych::corr.test(pmf_perc_conc_coor_num, use = "pairwise.complete.obs")
pmf_perc_conc_cor_matrix <- pmf_perc_conc_cor$r
pmf_perc_conc_p_matrix <- pmf_perc_conc_cor$p
pmf_perc_conc_n_matrix <- pmf_perc_conc_cor$n

# The mask for significant correlations (p < 0.05)
pmf_perc_conc_p_mask <- pmf_perc_conc_p_matrix > 0.05

# Set up color palette
col_palette <- 
  colorRampPalette(c("#4477AA", "white", "#EE6677"))(100)

# Create and display correlation plot, only show those with p < 0.05
corrplot(pmf_perc_conc_cor_matrix, 
         method = "color",
         type = "upper",
         diag = TRUE,
         tl.col = "black",
         tl.srt = 45,
         col = col_palette,
         # p.mat = pmf_perc_conc_p_mask,      # Add p-value matrix
         sig.level = 0.05,      # Significance level
         insig = "blank",       # Hide insignificant correlations
         addCoef.col = "black", 
         number.cex = 0.9,
         tl.cex = 0.9,
         cl.pos = "n"  # Remove the legend/colorbar
         )

# Optional: Corrplot showing the number of observations used
# This can help identify correlations based on too few data points
corrplot(pmf_perc_conc_n_matrix, 
         method = "color",
         type = "upper",
         is.corr = FALSE,  # This is not a correlation matrix
         tl.col = "black",
         tl.srt = 45,
         col = colorRampPalette(c("white", "darkgreen"))(100),
         addCoef.col = "black",
         number.cex = 0.8,
         tl.cex = 0.9,
         cl.pos = "n"  # Remove the legend/colorbar
         )

#### 5. Species trends ####
csn_imp_species = 
  read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/PMF_results/CSN_IMPROVE_Species_Csub.fst")

head(csn_imp_species); dim(csn_imp_species)
table(csn_imp_species$Dataset)
length(unique(csn_imp_species$SiteCode))
length(unique(subset(csn_imp_species, Dataset == "CSN")$SiteCode))
length(unique(subset(csn_imp_species, Dataset == "IMPROVE")$SiteCode))

# Get year & month
csn_imp_species$Year = year(csn_imp_species$Date)
csn_imp_species$Month = month(csn_imp_species$Date)

######  Species: overall annual change ######  
csn_imp_species_long =
  csn_imp_species %>%
  dplyr::group_by(Dataset, Year) %>%
  pivot_longer(
    cols = Al:PM25,
    names_to = "Species",
    values_to = "Concentration"
  )
head(csn_imp_species_long)

# Get overall annual averages
csn_imp_species_ann = 
  dplyr::select(csn_imp_species_long, -Dataset, -State, -Date, -SiteCode, -Month) %>%
  dplyr::group_by(Year, Species) %>%
  dplyr::summarise(
    conc_mean = mean(Concentration),
    conc_median = median(Concentration),
    conc_025th = quantile(Concentration, 0.025),
    conc_975th = quantile(Concentration, 0.975),
    .groups = "drop"
  )
head(csn_imp_species_ann)

csn_imp_species_ann_dataset = 
  dplyr::select(csn_imp_species_long, -State, -Date, -SiteCode, -Month) %>%
  dplyr::group_by(Dataset, Year, Species) %>%
  dplyr::summarise(
    conc_mean = mean(Concentration),
    conc_median = median(Concentration),
    conc_025th = quantile(Concentration, 0.025),
    conc_975th = quantile(Concentration, 0.975),
    .groups = "drop"
  )
head(csn_imp_species_ann_dataset)


# Reorder
csn_imp_species_ann =
  csn_imp_species_ann[with(csn_imp_species_ann, order(Species, Year)), ]
# View(csn_imp_species_ann)

csn_imp_species_ann_dataset =
  csn_imp_species_ann_dataset[with(csn_imp_species_ann_dataset, order(Species, Dataset, Year)), ]
# View(csn_imp_species_ann_dataset)


######  Main Species plot ######  
csn_imp_species_ann_main =
  subset(csn_imp_species_ann, Species %in% c("EC", "OC", "NO3Ion", "SO4Ion", "PM25"))
csn_imp_species_ann_main$Type = "Carbonaceous"
csn_imp_species_ann_main$Type[csn_imp_species_ann_main$Species %in% c("NO3Ion", "SO4Ion")] = "Ions"
csn_imp_species_ann_main$Type[csn_imp_species_ann_main$Species %in% c("PM25")] = "PM25"
# View(csn_imp_species_ann_main)
head(csn_imp_species_ann_main)

csn_imp_main_species_ts <- 
  csn_imp_species_ann_main %>%
  group_by(Type, Species) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(pick(everything()), "Year", "conc_mean"),
    .groups = "drop"
  ) %>%
  ungroup()
csn_imp_main_species_ts
summary(csn_imp_main_species_ts)

######  Species: annual Theil-Sen trends ######  
csn_imp_species_ts <- 
  csn_imp_species_ann %>%
  group_by(Species) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = round(get_slope_ts(pick(everything()), "Year", "conc_mean"), 3),
    .groups = "drop"
  ) %>%
  ungroup()
# View(csn_imp_species_ts)

csn_imp_species_ts_dataset <- 
  csn_imp_species_ann_dataset %>%
  group_by(Dataset, Species) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = round(get_slope_ts(pick(everything()), "Year", "conc_mean"), 3),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Dataset,
    values_from = diff_slope
  )
# View(csn_imp_species_ts_dataset)

csn_imp_species_ts_dataset_main <-
  subset(csn_imp_species_ts_dataset, 
         Species %in% c("Al", "Ca", "Cl", "K", "Na", "Fe", "Si", "NO3Ion", "SO4Ion",
                        "EC", "EC1", "EC2", "EC3", "OC", "OC1", "OC2", "OC3", "OC4", "OP", "PM25"))

csn_imp_species_dataset <- 
  csn_imp_species_ann_dataset %>%
  group_by(Dataset, Species) %>% # Dataset.x, 
  dplyr::summarize(
    conc_mean = mean(conc_mean),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Dataset,
    values_from = conc_mean
  )

csn_imp_species_dataset_main <-
  subset(csn_imp_species_dataset, 
         Species %in% c("Al", "Ca", "Cl", "K", "Na", "Fe", "Si", "NO3Ion", "SO4Ion",
                        "EC", "EC1", "EC2", "EC3", "OC", "OC1", "OC2", "OC3", "OC4", "OP", "PM25"))

######  Species: co-located site comparison ######  
# Get sitecode of the co-located sites
near_csn_in_imp = read.csv( "/Users/TingZhang/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE/The closest sampling points of CSN in IMPROVE.csv"); near_csn_in_imp$X = NULL
nearest_csn_in_imp = 
  subset(near_csn_in_imp, distance < 0.004) %>%
  dplyr::select(csn.site, imp.site)
nearest_csn_in_imp$Group = 1:nrow(nearest_csn_in_imp)
nearest_csn_in_imp =
  nearest_csn_in_imp %>%
  plyr::rename(
    c("csn.site" = "CSN",
      "imp.site" = "IMPROVE")) %>%
  mutate_all(as.character) %>%
  pivot_longer(
    cols = CSN:IMPROVE,
    names_to = "Dataset",
    values_to = "SiteCode"
  )
nearest_csn_in_imp

coloc_species = 
  merge(csn_imp_species, nearest_csn_in_imp)

coloc_species_long =
  coloc_species %>%
  pivot_longer(
    cols = Al:PM25,
    names_to = "Species",
    values_to = "Concentrations"
  )
head(coloc_species_long)

# add protocol change time points, Kaur_2024_AST
coloc_species_long$ion_protocol[
  coloc_species_long$Date <= as.Date("2015-10-31")] = "RTI-pre"
coloc_species_long$ion_protocol[
  coloc_species_long$Date >= as.Date("2015-11-01") & 
    coloc_species_long$Date < as.Date("2018-09-30")] = "DRI"
coloc_species_long$ion_protocol[
  coloc_species_long$Date >= as.Date("2018-10-01")] = "RTI-post"

coloc_species_long$C_protocol = "Original"
coloc_species_long$C_protocol[
  coloc_species_long$Date >= as.Date("2018-10-01")] = "IMPROVE_A"

coloc_species_csn = subset(coloc_species_long, Dataset == "CSN")
coloc_species_imp = subset(coloc_species_long, Dataset == "IMPROVE")
coloc_species_csn =
  plyr::rename(
    coloc_species_csn,
    c("SiteCode" = "CSN_SiteCode",
      "Concentrations" = "CSN_Concentrations")
  ) %>%
  dplyr::select(-State, -Dataset)
coloc_species_imp =
  plyr::rename(
    coloc_species_imp,
    c("SiteCode" = "IMPROVE_SiteCode",
      "Concentrations" = "IMPROVE_Concentrations")
  ) %>%
  dplyr::select(-State, -Dataset)

coloc_species_csn_imp =
  merge(coloc_species_csn, coloc_species_imp,
        by = c("Date", "Year", "Month", "Group", "Species", "ion_protocol", "C_protocol"))
head(coloc_species_csn_imp)

coloc_ion_csn_imp = 
  subset(coloc_species_csn_imp, Species %in% c("NO3Ion", "SO4Ion"))
coloc_c_csn_imp = 
  subset(coloc_species_csn_imp, Species %in% c("OC", "EC"))

# Scatter plot with trend lines
ggplot(coloc_ion_csn_imp) +
  geom_point(aes(x = CSN_Concentrations,
                 y = IMPROVE_Concentrations, 
                 color = ion_protocol),
             alpha = 0.55) +
  geom_smooth(aes(x = CSN_Concentrations,
                  y = IMPROVE_Concentrations,
                  color = ion_protocol),
              method = "lm", 
              se = TRUE,  # Set to FALSE if you don't want confidence intervals
              alpha = 0.0,  # Transparency for confidence band
              size = 1) +  # Line thickness
  facet_wrap(.~ Species, scales = "free") +
  scale_color_manual(values = c("#FF6347", "#2E8B57", "#4682B4")) +
  geom_abline(slope = 1, intercept = 0, 
              color = "grey40", linetype = "dashed", size = 1.2) +
  labs(x = format_variable("CSN Concentrations µg/m3"), 
       y = format_variable("IMPROVE Concentrations µg/m3")) +
  theme_base(base_size = 24)


ggplot(coloc_c_csn_imp) +
  geom_point(aes(x=CSN_Concentrations,
                 y=IMPROVE_Concentrations, 
                 color=C_protocol),
             alpha = 0.55) +
  theme_base(base_size = 24)

# Check the points not following 1:1 line
coloc_ion_csn_imp_not11NO3 = 
  subset(coloc_ion_csn_imp, 
       Species == "NO3Ion" & 
         CSN_Concentrations > 2 & IMPROVE_Concentrations < 1)
unique(coloc_ion_csn_imp_not11NO3$CSN_SiteCode)

coloc_ion_csn_imp_not11SO4 = 
  subset(coloc_ion_csn_imp, 
         Species == "SO4Ion" & 
           CSN_Concentrations > 3 & IMPROVE_Concentrations < 2.5)
unique(coloc_ion_csn_imp_not11SO4$CSN_SiteCode)


######  Species: Check for ions ######  

# Check for ions
csn_imp_species_ion = 
  dplyr::select(csn_imp_species, Dataset, SiteCode, Date, Year, NO3Ion, SO4Ion)
head(csn_imp_species_ion)

csn_imp_species_ion$ion_protocol[
  csn_imp_species_ion$Date <= as.Date("2015-10-31")] = "RTI-pre"
csn_imp_species_ion$ion_protocol[
  csn_imp_species_ion$Date >= as.Date("2015-11-01") & 
    csn_imp_species_ion$Date < as.Date("2018-09-30")] = "DRI"
csn_imp_species_ion$ion_protocol[
  csn_imp_species_ion$Date >= as.Date("2018-10-01")] = "RTI-post"


ddply(csn_imp_species_ion, .(ion_protocol), summarise,
      NO3_mean = mean(NO3Ion), NO3_sd = sd(NO3Ion),
      SO4_mean = mean(SO4Ion), SO4_sd = sd(SO4Ion))

csn_imp_species_ion_long =
  csn_imp_species_ion %>%
  dplyr::group_by(Dataset, Year) %>%
  pivot_longer(
    cols = NO3Ion:SO4Ion,
    names_to = "Species",
    values_to = "Concentration"
  )
head(csn_imp_species_ion_long)

csn_imp_species_ion_ann = 
  dplyr::select(csn_imp_species_ion_long, -Date, -SiteCode) %>%
  dplyr::group_by(Dataset, Year, Species) %>%
  dplyr::summarise(
    conc_median = median(Concentration),
    conc_025th = quantile(Concentration, 0.025),
    conc_975th = quantile(Concentration, 0.975),
    .groups = "drop"
  )

head(csn_imp_species_ion_ann)


ggplot(csn_imp_species_ion_ann,
       aes(x = Year, y= conc_median, color = Dataset)) +
  geom_line(size = 0.8) +
  geom_point(size = 2.5) +
  geom_errorbar(data = csn_imp_species_ion_ann,
                aes(ymin = conc_025th, 
                    ymax = conc_975th, color = Dataset), 
                width = 0.3, alpha = 0.6) +
  facet_grid(Species~., scales = "free_y") + 
  # scale_color_manual(values = color_source_noF) +
  scale_x_continuous(breaks = 2011:2020) +
  labs(y = format_variable("Concentration µg/m3")) +
  theme_minimal(base_size = 28) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    # legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust = 0.5))


### Get the ratios of annual ion conc vs. year 2011
csn_imp_species_ion_2011 <-
  subset(csn_imp_species_ion_ann, Year == 2011) %>%
  dplyr::select(-conc_025th, -conc_975th) %>%
  plyr::rename(c("Year" = "Base_Year",
                 "conc_median" = "Base_conc_median"))
head(csn_imp_species_ion_2011)

csn_imp_species_ion_restYear <-
  csn_imp_species_ion_ann %>%
  dplyr::select(-conc_025th, -conc_975th)
head(csn_imp_species_ion_restYear)

csn_imp_species_ion_comp <-
  merge(csn_imp_species_ion_restYear, csn_imp_species_ion_2011)
head(csn_imp_species_ion_comp)

csn_imp_species_ion_comp$Ratio = 
  csn_imp_species_ion_comp$conc_median/csn_imp_species_ion_comp$Base_conc_median

# Plot the ratio
ggplot(csn_imp_species_ion_comp,
       aes(x = Year, y= Ratio, color = Dataset)) +
  geom_line(size = 0.8) +
  geom_point(size = 2.5) +
  facet_grid(Species ~.) + 
  geom_hline(yintercept = 0.5, 
             color = "grey45", size = 0.6, linetype = "dashed") +
  geom_hline(yintercept = 1, 
             color = "grey45", size = 0.6, linetype = "dashed") +
  scale_color_manual(values = c("royalblue3", "#ff7f0e")) + #
  # scale_color_manual(values = color_source_noF) +
  scale_x_continuous(breaks = 2011:2020) +
  scale_y_continuous(limits = c(0, NA),
                     breaks = function(x) pretty(x, n = 3)) +
  labs(y = "Ratios (vs. Year 2011)") +
  theme_base(base_size = 28) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, vjust = 0.5, color = "grey25"))


######  Species: ion distributions - map ###### 
# Site GPS
cty_rural_urban = fread("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Intp_IMPROVE_CSN/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$V1 = NULL
cty_rural_urban = cty_rural_urban[!duplicated(cty_rural_urban$SiteCode), ]
head(cty_rural_urban); dim(cty_rural_urban)
cty_rural_urban_geo = 
  select(cty_rural_urban, SiteCode, Longitude, Latitude)

# Get site level mean species conc
csn_imp_species_site =
  csn_imp_species_long %>%
  dplyr::group_by(Dataset, State, SiteCode, Species) %>%
  dplyr::summarise(
    Concentration.mean = mean(Concentration),
    Concentration.median = median(Concentration),
    Concentration.sd = ifelse(n() == 1, 0, sd(Concentration)), 
    .groups = "drop"
  )
head(csn_imp_species_site)
summary(csn_imp_species_site)
# subset(csn_imp_species_long, SiteCode == "10730023" & Species == "Al")
# sd(subset(csn_imp_species_long, SiteCode == "11130001" & Species == "As")$Concentration)
sapply(csn_imp_species_site, class)

# Merge with GPS
csn_imp_species_site_gps =
  merge(csn_imp_species_site, cty_rural_urban_geo, by = "SiteCode")
head(csn_imp_species_site_gps)
unique(csn_imp_species_site_gps$Species)

# Select species for mapping
species_site_gps_carbon = 
  subset(csn_imp_species_site_gps,
         Species %in% c("EC", "EC1", "EC2", "OC", "OC1", "OC2", "OC3", "OP"))
species_site_gps_ion = 
  subset(csn_imp_species_site_gps,
         Species %in% c("NO3Ion", "SO4Ion"))
species_site_gps_element = 
  subset(csn_imp_species_site_gps,
         Species %in% c("Al", "Ca", "Fe", "Si", "Na", "K", "Cl", "Zn"))


#### Mapping
# Carbons
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = species_site_gps_carbon, 
             aes(x = Longitude, y = Latitude, 
                 fill = Concentration.mean),
             size = 2.5, alpha = 0.8, 
             shape = 21, color = "grey66") +
  scale_fill_gradient2(limits = c(quantile(species_site_gps_carbon$Concentration.mean, 0.05),
                                  quantile(species_site_gps_carbon$Concentration.mean, 0.95)),
                       low = "#2CA02C",  
                       mid = "ivory",  
                       high = "#D62728", 
                       midpoint = 0,
                       oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  # guides(color=guide_legend(title="Slope: µg/m3")) + 
  coord_sf(datum = NA) +
  facet_wrap(~ Species) +
  # addline_space, add a break in the text
  labs(fill=addline_space(paste("Carbonaceous groups",
                                format_variable("(µg/m3)")))) +
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

# Ions
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = species_site_gps_ion, 
             aes(x = Longitude, y = Latitude, 
                 fill = Concentration.mean),
             size = 2.5, alpha = 0.8, 
             shape = 21, color = "grey66") +
  scale_fill_gradient2(limits = c(quantile(species_site_gps_ion$Concentration.mean, 0.05),
                                  quantile(species_site_gps_ion$Concentration.mean, 0.95)),
                       low = "green",  # ivory
                       high = "#D62728", 
                       oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  coord_sf(datum = NA) +
  facet_wrap(~ Species) +
  # addline_space, add a break in the text
  labs(fill=addline_space(paste("Ions", format_variable("(µg/m3)")))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10),
        axis.title = element_text(size = 0),
        # legend.position = c(0.85, 0.18),
        legend.text = element_text(size = 14), 
        legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
        legend.spacing.y = unit(1, "cm")) 


# Elements
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = species_site_gps_element, 
             aes(x = Longitude, y = Latitude, 
                 fill = Concentration.mean),
             size = 2.5, alpha = 0.8, 
             shape = 21, color = "grey66") +
  scale_fill_gradient2(limits = c(quantile(species_site_gps_element$Concentration.mean, 0.05),
                                  quantile(species_site_gps_element$Concentration.mean, 0.95)),
                       low = "green",  # ivory
                       high = "#D62728", 
                       oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  coord_sf(datum = NA) +
  facet_wrap(~ Species) +
  # addline_space, add a break in the text
  labs(fill=addline_space(paste("Elements", format_variable("(µg/m3)")))) +
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

#### 5.2 Species vs. Source ####

csn_imp_species = 
  read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/PMF_results/CSN_IMPROVE_Species_Csub.fst")
pmf_source = 
  read.fst("/Users/TingZhang/Dropbox/HEI_US_PMF/PMF_results/CSN_IMPROVE_Daily_Source_Impacts_region_2011-20.fst")
site_region = dplyr::select(pmf_source, SiteCode, Region)
head(csn_imp_species); head(pmf_source)

species_source = 
  merge(csn_imp_species, pmf_source, by = c("Dataset", "SiteCode", "Date", "State"))
head(species_source)

species_source_wide = 
  dplyr::select(species_source, -Source_aftermanual, -Percent) %>%
  pivot_wider(
    names_from = "Source",
    values_from = "Concentration"
  )

names(species_source_wide)[52:59]
names(species_source_wide)[52:59] =
  c("Non_tailpipe", "Sulfate", "Salt", "Soil_Dust", "Traffic", 
    "Sec_Nitrate", "Industry", "OP_rich")
head(species_source_wide)

######  Species-Source: correlations ###### 
# Drop SiteCode and Date for correlation
species_source_main_num <- 
  dplyr::select(species_source_wide,  Ca, Si, Fe, K, Na, Ni, Cl, EC, OC, OP, NO3Ion, SO4Ion, PM25, Biomass:Industry)
head(species_source_main_num)

# Calculate correlation, relevance, and number of complete cases matrix
species_source_main_cor <- 
  psych::corr.test(species_source_main_num, use = "na.or.complete") #  "pairwise.complete.obs"
species_source_main_cor_matrix <- species_source_main_cor$r
species_source_main_p_matrix <- species_source_main_cor$p
species_source_main_n_matrix <- species_source_main_cor$n

# The mask for significant correlations (p < 0.05)
species_source_main_p_mask <- species_source_main_p_matrix > 0.05

# Set up color palette
col_palette <- 
  colorRampPalette(c("#4477AA", "white", "#EE6677"))(100)

# Create and display correlation plot, only show those with p < 0.05
corrplot(species_source_main_cor_matrix, 
         method = "color",
         type = "upper",
         diag = TRUE,
         tl.col = "black",
         tl.srt = 45,
         col = col_palette,
         # p.mat = species_source_main_p_mask,      # Add p-value matrix
         sig.level = 0.05,      # Significance level
         insig = "blank",       # Hide insignificant correlations
         addCoef.col = "black", 
         number.cex = 0.9,
         tl.cex = 0.9,
         cl.pos = "n"  # Remove the legend/colorbar
)

# Optional: Corrplot showing the number of observations used
# This can help identify correlations based on too few data points
corrplot(species_source_main_n_matrix, 
         method = "color",
         type = "upper",
         is.corr = FALSE,  # This is not a correlation matrix
         tl.col = "black",
         tl.srt = 45,
         col = colorRampPalette(c("white", "darkgreen"))(100),
         addCoef.col = "black",
         number.cex = 0.8,
         tl.cex = 0.9,
         cl.pos = "n"  # Remove the legend/colorbar
)  
  

######  Species trend nationwide ######  
species_nation_conc = 
  dplyr::select(species_source_wide,
                Dataset:PM25, Region, Year) %>%
  pivot_longer(
    cols = Al:PM25,
    names_to = "Species",
    values_to = "Concentration"
  ) %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(
    conc_mean = mean(Concentration),
    conc_median = median(Concentration),
    .groups = "drop"
  )

species_nation_ann = 
  dplyr::select(species_source_wide,
                Dataset:PM25, Region, Year) %>%
  pivot_longer(
    cols = Al:PM25,
    names_to = "Species",
    values_to = "Concentration"
  ) %>%
  dplyr::group_by(Species, Year) %>%
  dplyr::summarise(
    conc_mean = mean(Concentration),
    conc_median = median(Concentration),
    .groups = "drop"
  )

species_nation_ts <- 
  species_nation_ann %>%
  group_by(Species) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = round(get_slope_ts(pick(everything()), "Year", "conc_mean"), 3),
    .groups = "drop"
  ) %>%
  ungroup()

species_nation_ts_main =
  subset(species_nation_ts, 
         Species %in% c("Si", "Fe", "Ni", "K", "Cl", "OC", "EC", "SO4Ion", "NO3Ion", "PM25"))
species_nation_ts_main_wd =
  species_nation_ts_main %>%
  pivot_wider(
    names_from = Species,
    values_from = diff_slope
  )

######  Species trend at regional ######  
species_region_conc = 
  dplyr::select(species_source_wide,
                Dataset:PM25, Region, Year) %>%
  pivot_longer(
    cols = Al:PM25,
    names_to = "Species",
    values_to = "Concentration"
  ) %>%
  dplyr::group_by(Region, Species) %>%
  dplyr::summarise(
    conc_mean = mean(Concentration),
    conc_median = median(Concentration),
    .groups = "drop"
  )

species_region_ann = 
  dplyr::select(species_source_wide,
                Dataset:PM25, Region, Year) %>%
  pivot_longer(
    cols = Al:PM25,
    names_to = "Species",
    values_to = "Concentration"
  ) %>%
  dplyr::group_by(Region, Species, Year) %>%
  dplyr::summarise(
    conc_mean = mean(Concentration),
    conc_median = median(Concentration),
    .groups = "drop"
  )

species_region_ts <- 
  species_region_ann %>%
  group_by(Region, Species) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = round(get_slope_ts(pick(everything()), "Year", "conc_mean"), 3),
    .groups = "drop"
  ) %>%
  ungroup()

species_region_ts_main =
  subset(species_region_ts, Species %in% c("Si", "Fe", "Ni", "K", "Cl", "OC", "EC", "SO4Ion", "NO3Ion"))
species_region_ts_main_wd =
  species_region_ts_main %>%
  pivot_wider(
    names_from = Species,
    values_from = diff_slope
  )

######  Species trend at regional and by dataset ######  
species_region_dataset_ann = 
  dplyr::select(species_source_wide,
                Dataset:PM25, Region, Year) %>%
  pivot_longer(
    cols = Al:PM25,
    names_to = "Species",
    values_to = "Concentration"
  ) %>%
  dplyr::group_by(Dataset, Region, Species, Year) %>%
  dplyr::summarise(
    conc_mean = mean(Concentration),
    conc_median = median(Concentration),
    .groups = "drop"
  )

species_region_dataset_ts <- 
  species_region_dataset_ann %>%
  group_by(Dataset, Region, Species) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = round(get_slope_ts(pick(everything()), "Year", "conc_mean"), 3),
    .groups = "drop"
  ) %>%
  ungroup()

species_region_dataset_ts_main =
  subset(species_region_dataset_ts, 
         Species %in% c("Si", "Fe", "Ni", "K", "Cl", "OC", "EC", "SO4Ion", "NO3Ion"))
species_region_dataset_ts_main_wd =
  species_region_dataset_ts_main %>%
  pivot_wider(
    names_from = Species,
    values_from = diff_slope
  )

######  Species-Source: Mass Reconstruction ###### 
## Chow_2015_AQAH_Mass reconstruction PM2.5.pdf
# based on chemical mass balance

species_source_mr = species_source_wide

## Get OC/EC ratio primary
# set the primary not the minimum, but 5th percentile of each ratios of the site
species_source_mr$oc_ec_ratio = 
  species_source_mr$OC/species_source_mr$EC
# summary(species_source_mr$oc_ec_ratio)
species_source_mr <- 
  species_source_mr %>%
  dplyr::group_by(SiteCode) %>%
  dplyr::mutate(
    oc_ec_ratio_primary = 
      quantile(oc_ec_ratio, 0.025, na.rm = TRUE)) %>%
  ungroup()
summary(species_source_mr$oc_ec_ratio_primary)

# species_source_mr$EC_new = 
#   species_source_mr$EC1 + species_source_mr$EC2 +
#   species_source_mr$EC3 - species_source_mr$OP
# species_source_mr$OC_new = 
#   species_source_mr$OC1 + species_source_mr$OC2 +
#   species_source_mr$OC3 + species_source_mr$OC4 +
#   species_source_mr$OP
# summary(species_source_mr$EC_new)
# summary(species_source_mr$OC_new)
# 
# species_source_mr$oc_ec_ratio_new = 
#   species_source_mr$OC_new/species_source_mr$EC_new
# species_source_mr <- 
#   species_source_mr %>%
#   dplyr::group_by(SiteCode) %>%
#   dplyr::mutate(
#     oc_ec_ratio_primary_new = 
#       quantile(oc_ec_ratio_new, 0.025, na.rm = TRUE)) %>%
#   ungroup()
# summary(species_source_mr$oc_ec_ratio_primary_new)

species_source_mr$mr_soa = 
  species_source_mr$OC - 
  species_source_mr$EC * species_source_mr$oc_ec_ratio_primary
summary(species_source_mr$mr_soa)
quantile(species_source_mr$mr_soa, 0.026)
species_source_mr$mr_soa[species_source_mr$mr_soa<=0] = NA

species_source_mr$mr_soil = 
  2.2*species_source_mr$Al + 2.49*species_source_mr$Si +
  1.63*species_source_mr$Ca + 2.42*species_source_mr$Fe +
  1.94*species_source_mr$Ti

species_source_mr$mr_sea_salt =
  species_source_mr$Na * 3.27

species_source_mr$mr_sulfate =
  species_source_mr$SO4Ion * 1.375
species_source_mr$mr_nitrate =
  species_source_mr$NO3Ion *1.3

species_source_mr$mr_trace =
  species_source_mr$Br + species_source_mr$As +
  species_source_mr$Cr + species_source_mr$Cu +
  species_source_mr$Mn + species_source_mr$Ni +
  species_source_mr$Rb + species_source_mr$Sr +
  species_source_mr$V + species_source_mr$Zn

species_source_mr_use =
  dplyr::select(species_source_mr, Biomass:mr_trace)
names(species_source_mr_use)
species_source_mr_use =
  dplyr::select(species_source_mr_use, -oc_ec_ratio, -oc_ec_ratio_primary)
summary(species_source_mr_use)

# Calculate correlation, relevance, and number of complete cases matrix
species_source_mr_cor <- 
  psych::corr.test(species_source_mr_use, use = "pairwise") #  "pairwise.complete.obs"
species_source_mr_cor_matrix <- species_source_mr_cor$r
species_source_mr_p_matrix <- species_source_mr_cor$p
species_source_mr_n_matrix <- species_source_mr_cor$n

# The mask for significant correlations (p < 0.05)
species_source_mr_p_mask <- species_source_mr_p_matrix > 0.05

# Set up color palette
col_palette <- 
  colorRampPalette(c("#4477AA", "white", "#EE6677"))(100)

# Create and display correlation plot, only show those with p < 0.05
corrplot(species_source_mr_cor_matrix, 
         method = "color",
         type = "upper",
         diag = TRUE,
         tl.col = "black",
         tl.srt = 45,
         col = col_palette,
         # p.mat = species_source_mr_p_mask,      # Add p-value matrix
         sig.level = 0.05,      # Significance level
         insig = "blank",       # Hide insignificant correlations
         addCoef.col = "black", 
         number.cex = 0.9,
         tl.cex = 0.9,
         cl.pos = "n"  # Remove the legend/colorbar
)

# Optional: Corrplot showing the number of observations used
# This can help identify correlations based on too few data points
corrplot(species_source_mr_n_matrix, 
         method = "color",
         type = "upper",
         is.corr = FALSE,  # This is not a correlation matrix
         tl.col = "black",
         tl.srt = 45,
         col = colorRampPalette(c("white", "darkgreen"))(100),
         addCoef.col = "black",
         number.cex = 0.8,
         tl.cex = 0.9,
         cl.pos = "n"  # Remove the legend/colorbar
)  


species_source_mr_sp_overall =
  dplyr::select(species_source_mr, Latitude, Longitude, mr_soa:mr_trace) %>%
  dplyr::group_by(Latitude, Longitude) %>%
  dplyr::summarise(
    SOA = mean(mr_soa, na.rm = TRUE),
    Soil = mean(mr_soil, na.rm = TRUE),
    Sea_salt = mean(mr_sea_salt, na.rm = TRUE),
    Sulfate = mean(mr_sulfate, na.rm = TRUE),
    Nitrate = mean(mr_nitrate, na.rm = TRUE),
    Trace_element = mean(mr_trace, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = SOA:Trace_element,
    names_to = "Reconstructed_source",
    values_to = "Concentrations"
  )
head(species_source_mr_sp_overall)

conc_range = 
  quantile(species_source_mr_sp_overall$Concentrations, 0.1):
  quantile(species_source_mr_sp_overall$Concentrations, 0.9)
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = species_source_mr_sp_overall, 
             aes(x = Longitude, y = Latitude, 
                 fill = Concentrations),
             size = 2.5, alpha = 0.8, 
             shape = 21, color = "grey66") +
  scale_fill_gradient2(limits = conc_range,
                       low = "#2CA02C",  
                       mid = "ivory",
                       high = "#D62728", 
                       # midpoint = 0,
                       oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  # guides(color=guide_legend(title="Slope: µg/m3")) + 
  coord_sf(datum = NA) +
  # facet_wrap(~ source_site_count, # ~ source_site_count, # source_row, 
  #            labeller = labeller(source_site_count =
  #                                  as_labeller(as.character,
  #                                              default = label_value))) +
  facet_wrap(~ Reconstructed_source) +
  # addline_space, add a break in the text
  labs(fill=addline_space(paste("Concentrations",
                                format_variable("(µg/m3)")))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10),
        axis.title = element_text(size = 0),
        # legend.position = c(0.85, 0.18),
        legend.text = element_text(size = 14), 
        legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
        legend.spacing.y = unit(1, "cm")) 



#### 6. all site PM ####
`
# All observations, after interpolation
site_species_daily = read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/PMF_results/CSN_IMPROVE_Species_Csub.fst")
site_species_daily$V1 = NULL
head(site_species_daily); dim(site_species_daily)

# Get PM2.5 data
site_pm_daily = 
  dplyr::select(site_species_daily, Dataset, SiteCode, Date, State, PM25)
site_pm_daily$Year = year(site_pm_daily$Date)
site_pm_daily$Month = month(site_pm_daily$Date)

# Annual PM
site_pm_annual = 
  site_pm_daily %>%
  dplyr::group_by(Dataset, State, SiteCode, Year) %>%
  dplyr::summarise(
    PM25_med = median(PM25),
    PM25_mean = mean(PM25),
    .groups = "drop"
  )
head(site_pm_annual)

# Overall PM for each site
site_pm_overal = 
  site_pm_daily %>%
  dplyr::group_by(Dataset, State, SiteCode) %>%
  dplyr::summarise(
    PM25_med = median(PM25),
    PM25_mean = mean(PM25),
    .groups = "drop"
  )
head(site_pm_overal)

# Overall PM, nationwide
pm_overal_annual = 
  site_pm_daily %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    PM25_med = median(PM25),
    PM25_mean = mean(PM25),
    .groups = "drop"
  )
head(pm_overal_annual)

summary(site_pm_daily)

# Site info, GPS assigned earlier
cty_rural_urban = fread("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Intp_IMPROVE_CSN/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$V1 = NULL
cty_rural_urban = cty_rural_urban[!duplicated(cty_rural_urban$SiteCode), ]
head(cty_rural_urban); dim(cty_rural_urban)

cty_rural_urban_use =
  dplyr::select(cty_rural_urban, -geoid, -Dataset)
head(cty_rural_urban_use)

# Merge with site_perform
site_geo_annual = merge(cty_rural_urban_use, site_pm_annual)
summary(site_geo_annual)

site_geo_overal = merge(cty_rural_urban_use, site_pm_overal)
summary(site_geo_overal)

# All 2011-2020, by year
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = site_geo_annual,
             aes(x = Longitude, y = Latitude, 
                 size = PM25_med), # size = PM25_med, 
             alpha = 0.7, size = 2.5,
             shape = 21, color = "grey66") +
  scale_fill_gradient2(
    limits = c(2.5, 10),
    breaks = c(2.5, 5, 7.5, 10),
    low = "grey", # #2CA02C 
    # mid = "ivory",  
    high = "#D62728", 
    # midpoint = 0,
    oob = scales::squish) + # oob = scales::squish, show the extreme values outside of range.
  coord_sf(datum = NA) +
  facet_wrap(. ~ Year, ncol = 4) +
  labs(fill=addline_space(paste("Concentration",  
                                format_variable("µg/m3"), ""))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10),
        axis.title = element_text(size = 0),
        # legend.position = c(0.85, 0.18),
        legend.text = element_text(size = 14), 
        legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
        legend.spacing.y = unit(1, "cm")) 

# Overall
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = site_geo_overal,
             aes(x = Longitude, y = Latitude, 
                 size = PM25_med, shape = Dataset, color = Dataset, group = Dataset), # size = PM25_med, 
             alpha = 0.5) +
  coord_sf(datum = NA) +
  scale_color_manual(values = c("royalblue3", "#ff7f0e")) + 
  labs(color=addline_space(paste("Concentration",  
                                format_variable("µg/m3"), ""))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10),
        axis.title = element_text(size = 0),
        # legend.position = c(0.85, 0.18),
        legend.text = element_text(size = 14), 
        legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
        legend.spacing.y = unit(1, "cm")) 

# Two year comparison
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = subset(site_geo_annual, Year %in% c(2011, 2019)),
             aes(x = Longitude, y = Latitude, 
                 size = PM25_med, shape = Dataset, color = Dataset, group = Dataset), # size = PM25_med, 
             alpha = 0.6) +
  coord_sf(datum = NA) +
  facet_wrap(. ~ Year, ncol = 2) +
  scale_color_manual(values = c("royalblue3", "#ff7f0e")) + 
  labs(fill=addline_space(paste("Concentration",  
                                format_variable("µg/m3"), ""))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.text.y = element_text(size = 10),
        axis.title = element_text(size = 0),
        # legend.position = c(0.85, 0.18),
        legend.text = element_text(size = 14), 
        legend.key.size = unit(1.5, "lines"), # adjust the size of the legend keys
        legend.title = element_text(size = 16, hjust = 0.1, vjust = 3),
        legend.spacing.y = unit(1, "cm")) 



#### Dominant source & non-attaintant ####
domain_source_year = fread("Dominant_source_site_annual_CSN-IMPROVE.csv")
non_attainment_cty = fread("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/EPA_52_Non-attainment_counties_NAAQS_by2032.csv")
non_attainment_cty$Attainment = "FALSE"
cty_rural_urban = fread("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nationwide_SA/data/intermediate/pmf/PMF_progress_files/CSN_IMPROVE/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
domain_source_year$V1 = non_attainment_cty$V1 = cty_rural_urban$V1 = NULL

cty_rural_urban = cty_rural_urban[!duplicated(cty_rural_urban$SiteCode), ] 
cty_rural_urban = 
  plyr::rename(cty_rural_urban, 
               c("state_name" = "State",
                 "namelsad" = "County"))
cty_rural_urban = dplyr::select(cty_rural_urban, SiteCode, State, County)
cty_rural_urban[, County := gsub(" County", "", County)] # remove "County" in the column

head(cty_rural_urban); head(non_attainment_cty); head(domain_source_year)

non_attainment_cty = 
  merge(non_attainment_cty, cty_rural_urban,
        by = c("State", "County"), all.y = TRUE)
non_attainment_cty = subset(non_attainment_cty, !is.na(State))
non_attainment_cty$Attainment[is.na(non_attainment_cty$Attainment)] = "TRUE"
non_attainment_cty$Attainment = as.logical(non_attainment_cty$Attainment)
summary(non_attainment_cty$Attainment) # 47 vs. 280

all_attain_cty_domain_source =
  merge(domain_source_year, non_attainment_cty, by = "SiteCode")
non_attain_cty_domain_source = 
  subset(all_attain_cty_domain_source, Attainment == FALSE)
attain_cty_domain_source = 
  subset(all_attain_cty_domain_source, Attainment == TRUE)

# 36 vs. 255
length(unique(all_attain_cty_domain_source$SiteCode)) # 255
length(unique(non_attain_cty_domain_source$SiteCode)) # 36

non_attain_cty_domain_source_sum = 
  data.table(table(non_attain_cty_domain_source$SiteCode, 
                   non_attain_cty_domain_source$Source)) %>%
  subset(N > 0)

non_attain_cty_domain_source_year = 
  data.table(table(non_attain_cty_domain_source$Year, 
                 non_attain_cty_domain_source$Source))
names(non_attain_cty_domain_source_year) =
  c("Year", "Source", "Count") 
subset(non_attain_cty_domain_source_year, Year == 2019)
View(non_attain_cty_domain_source_year)
table(non_attain_cty_domain_source$Year)
# biomass, sulfate, nitrate, traffic, dust, 11, 10, 1, 3, and 0 in 2011, 22, 5, 0, 6, and 0 in 2019
# bb+ss, 21 (21/26=81%) in 2011, 27 (27/34=79%) in 2019
# bb+ss+sn, 22 (22/26=85%) in 2011, 27 (27/34=79%) in 2019

non_attain_cty_domain_source_year_table <-
  non_attain_cty_domain_source_year %>%
  pivot_wider(
    names_from = "Source",
    values_from = "Count"
  )
# View(non_attain_cty_domain_source_year_table)

attain_cty_domain_source_year = 
  data.table(table(attain_cty_domain_source$Year, 
                   attain_cty_domain_source$Source))
names(attain_cty_domain_source_year) =
  c("Year", "Source", "Count")
table(attain_cty_domain_source$Year)
# biomass, sulfate, nitrate, traffic, dust, 43, 123, 2, 18 in 2011, 106, 61, 1, 23, 7 in 2019, dust, 19 in 2020
# bb+ss, 166 (166/192 = 86.4%) in 2011, 167 (167/198=84%) in 2019
# bb+ss+sn, 168 (168/192 = 88%) in 2011, 168 (168/198=85%) in 2019

attain_cty_domain_source_year_table <-
  attain_cty_domain_source_year %>%
  pivot_wider(
    names_from = "Source",
    values_from = "Count"
  )

table(non_attain_cty_domain_source$Source)/nrow(non_attain_cty_domain_source)
table(attain_cty_domain_source$Source)/nrow(attain_cty_domain_source)


