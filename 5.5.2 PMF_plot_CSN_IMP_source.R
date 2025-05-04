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


csn_org = fread("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/CSN_Site_15t1mdl0unc_DN_covertBack_daily.csv")
imp_org = fread("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/IMPROVE_Site_15t1mdlVNi_DN_covertBack_daily.csv")
head(csn_org); dim(imp_org)




#### 0.1 Process initial source data ####
# # Read PMF result combination from CSN & IMPROVE
# pmf_both = read.csv("CSN_IMPROVE_source_daily_contribution.csv")
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

pmf_both_perc = read_fst("CSN_IMPROVE_daily_SA_conc_perc.fst")
dim(pmf_both_perc); head(pmf_both_perc) # 1585748  8

# avoid duplicates
pmf_both_perc = 
  pmf_both_perc %>%
  dplyr::group_by(SiteCode, Date, Year, Latitude, Longitude, Source_aftermanual) %>%
  dplyr::summarise(
    Concentration = mean(Concentration),
    Percent = mean(Percent),
    .groups = "drop"
  )
dim(pmf_both_perc); head(pmf_both_perc) # 1386953  8

# Get Month and Source info
pmf_both_perc$Month = month(pmf_both_perc$Date)
head(pmf_both_perc); dim(pmf_both_perc)
length(unique(pmf_both_perc$SiteCode))
length(unique(pmf_both_perc$Date))

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
subset(pmf_daily_pm, Percent < 50)

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

# Get annual median & range
Year_aggregated_use <- 
  ddply(pmf_both_perc, 
        .(SiteCode, Year, Source, Source_aftermanual), 
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

# Get monthly median & range
Month_aggregated_use <- 
  ddply(pmf_both_perc, 
        .(SiteCode, Month, Source, Source_aftermanual), 
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
             "F8-Biomass", "F9-Soil/Dust"))
head(DOW_plot_data)
View(DOW_plot_data)

# Make sure DOW is a factor with proper order
DOW_plot_data$DOW <- 
  factor(DOW_plot_data$DOW, 
         levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                    "Thursday", "Friday", "Saturday"))

# Plotting
line_conc_DOW <-
  ggplot(DOW_plot_data,
         aes(x = DOW, y= conc_Median, color = Source, group = Source)) +
  geom_line(size = 0.8) +
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
    axis.title.x = element_text(vjust = -1, hjust = 0))
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
             "F8-Biomass", "F9-Soil/Dust"))
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
    legend.position = "none",
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
  subset(Month_aggregated_area_p, Month == 1) %>%
  # arrange(desc(conc_Median))
  arrange(conc_Median)

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
  theme_minimal(base_size = 28) + 
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

### SE
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
  theme_minimal(base_size = 36) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 1)  # Angled text for better readability
  )

### CV
ggplot(subset(month_variance_conc, 
              Variance_Metric == "Coefficient of Variation (%)"),
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
  theme_minimal(base_size = 36) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 1)  # Angled text for better readability
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

# the median and 99% of data
Year_aggregated_summary = 
  ddply(Year_aggregated_use, 
        .(Source_aftermanual, Source, Year),
        summarise,
        conc_Lower = quantile(Concentration, 0.0025, na.rm = T),
        conc_Median = median(Concentration, na.rm = T),
        conc_Upper = quantile(Concentration, 0.9975, na.rm = T),
        perc_Lower = quantile(Percent, 0.0025, na.rm = T),
        perc_Median = median(Percent, na.rm = T),
        perc_Upper = quantile(Percent, 0.9975, na.rm = T))
Year_aggregated_summary$Year = as.integer(Year_aggregated_summary$Year)
head(Year_aggregated_summary)

Year_aggregated_area = 
  dplyr::select(Year_aggregated_summary, Source, Year, conc_Median)

# re-estimate the percent contribution based on the conc_median of all sites of each year
Year_aggregated_area_p <- 
  subset(
    Year_aggregated_area,
    Source %in% c("Traffic", "Secondary Nitrate", "Secondary Sulfate",
                  "Industry", "Salt", "Non-tailpipe", "Biomass", "Soil/Dust")) %>%
  group_by(Year) %>%
  dplyr::mutate(
    conc_sum = sum(conc_Median)) %>%
  ungroup() %>%
  dplyr::mutate(
    perc_reestimated = conc_Median / conc_sum * 100)

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
  theme_minimal(base_size = 28) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text.y = element_text(vjust = 0.5, hjust = 100))
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
write.csv(domain_source_year, "Dominant_source_site_annual_CSN-IMPROVE.csv")

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
    diff_slope_conc_995 = quantile(diff_slope_conc, 0.995, na.rm = TRUE),
    diff_slope_conc_med = median(diff_slope_conc, na.rm = TRUE),
    diff_slope_conc_005 = quantile(diff_slope_conc, 0.005, na.rm = TRUE),
    diff_slope_conc_mean = mean(diff_slope_conc, na.rm = TRUE),
    diff_slope_conc_sd = sd(diff_slope_conc, na.rm = TRUE),
    diff_slope_conc_max = max(diff_slope_conc, na.rm = TRUE),
    diff_slope_conc_min = min(diff_slope_conc, na.rm = TRUE),
    diff_slope_frac_995 = quantile(diff_slope_frac, 0.995, na.rm = TRUE),
    diff_slope_frac_med = median(diff_slope_frac, na.rm = TRUE),
    diff_slope_frac_005 = quantile(diff_slope_frac, 0.005, na.rm = TRUE),
    diff_slope_frac_mean = mean(diff_slope_frac, na.rm = TRUE),
    diff_slope_frac_max = max(diff_slope_frac, na.rm = TRUE),
    diff_slope_frac_min = min(diff_slope_frac, na.rm = TRUE),
    diff_slope_frac_sd = sd(diff_slope_frac, na.rm = TRUE),
    total_count = n()
  )

head(slope_diff_site_range)
# View(slope_diff_site_range)

# Check the frequency of positive and negative trends
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
        # legend.position = c(0.85, 0.18),
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

# merge annual contribution data with geometry
annual_source_gps = merge(annual_contri_gps, us_cty_bdr_geo)
summary(annual_source_gps); head(annual_source_gps)
class(annual_source_gps)

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

###### 3.1.2 Annual Theil-Sen trend for each source in each area ######
# select columns to use
annual_source_gps_conc_perc = 
  dplyr::select(annual_source_gps, SiteCode, Year, 
                Source_aftermanual, Longitude, Latitude, Concentration, Percent)
us_states_region_sf = 
  dplyr::select(us_states_region, region, state_abbr, geometry)

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
  dplyr::select(annual_source_site, SiteCode, state_abbr, Source_aftermanual)
site_state_source = site_state_source[!duplicated(site_state_source), ]
subset(site_state_source, state_abbr == "TX" & Source_aftermanual == "F1-Traffic")
site_state_source_count = data.frame(table(site_state_source$state_abbr))

site_state_2011 = 
  dplyr::select(annual_source_site, Year, SiteCode, state_abbr, Source_aftermanual) %>%
  subset(Year == 2011)
site_state_2011 = site_state_2011[!duplicated(site_state_2011), ]
subset(site_state_2011, state_abbr == "TX" & Source_aftermanual == "F1-Traffic")
site_state_2011_count = data.frame(table(site_state_2011$state_abbr))

# Get regional median
annual_source_region <-
  annual_source_site %>%
  group_by(region, Source_aftermanual, Year) %>%
  dplyr::summarise(Concentration = median(Concentration),
            Percent = median(Percent),
            .groups = "drop")
head(annual_source_region); dim(annual_source_region)

### Thiel-Sen, for site 
# concentration-based slope
region_slope_diff_conc_ts <- 
  annual_source_region %>%
  group_by(region, Source_aftermanual) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(cur_data(), "Year", "Concentration"),
    .groups = "drop"
  ) %>%
  ungroup()

# Overall 
subset(region_slope_diff_conc_ts, Source_aftermanual == "F3-Secondary Sulfate")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F8-Biomass")


subset(region_slope_diff_conc_ts, Source_aftermanual == "F1-Traffic")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F2-Secondary Nitrate")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F5-Industry")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F6-Salt")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F7-Non-tailpipe")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F9-Soil/Dust")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F10-OP-rich")


# percent-based slope
region_slope_diff_perc_ts <- 
  annual_source_region %>%
  group_by(region, Source_aftermanual) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(cur_data(), "Year", "Percent"),
    .groups = "drop"
  ) %>%
  ungroup()

subset(region_slope_diff_perc_ts, Source_aftermanual == "F3-Secondary Sulfate")
subset(region_slope_diff_perc_ts, Source_aftermanual == "F8-Biomass")


subset(region_slope_diff_perc_ts, Source_aftermanual == "F1-Traffic")
subset(region_slope_diff_perc_ts, Source_aftermanual == "F2-Secondary Nitrate")
subset(region_slope_diff_perc_ts, Source_aftermanual == "F5-Industry")
subset(region_slope_diff_perc_ts, Source_aftermanual == "F6-Salt")
subset(region_slope_diff_perc_ts, Source_aftermanual == "F7-Non-tailpipe")
subset(region_slope_diff_perc_ts, Source_aftermanual == "F9-Soil/Dust")
subset(region_slope_diff_perc_ts, Source_aftermanual == "F10-OP-rich")

region_slope_diff_conc_ts_1 = region_slope_diff_conc_ts
region_slope_diff_perc_ts_1 = region_slope_diff_perc_ts

# Rbind
region_slope_diff_conc_ts$Class = "Concentration"
region_slope_diff_perc_ts$Class = "Percent"

region_slope_diff = rbind(region_slope_diff_conc_ts, region_slope_diff_perc_ts)
head(region_slope_diff)

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

write.csv(region_slope_diff, "CSN_IMPROVE_region_Theil-Sen.csv")
write.csv(region_slope_diff_1, "CSN_IMPROVE_region_Theil-Sen_another_view.csv")

###### 3.2. Dominant source by region ###### 

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
  dplyr::select(region, Year, Source, Concentration) %>%
  ungroup() %>%
  st_drop_geometry() # remove multipoint geometry
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


###### 3.3.0 SP Map - basic source data, color define ###### 

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
         Source_aftermanual == "F7-Non-tailpipe")
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
         Source_aftermanual == "F9-Soil/Dust")
col_singleSource = "grey"
col_singleSource_line = "grey40"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_aftermanual == "F10-OP-rich")
col_singleSource = "#5A9599FF"
col_singleSource_line = "#5A9599FF"


###### 3.3.1 SP Map - Concentration - contribution trend estimation ######

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

###### 3.3.2 SP Map - Concentration - plotting ######

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

###### 3.4.1 SP Map - Percent - contribution trend estimation ######

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


###### 3.4.2 SP Map - Percent - plotting ######

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
         p.mat = pmf_perc_conc_p_mask,      # Add p-value matrix
         sig.level = 0.05,      # Significance level
         insig = "blank",       # Hide insignificant correlations
         addCoef.col = "black", 
         number.cex = 0.9,
         tl.cex = 0.9,
         cl.pos = "n"  # Remove the legend/colorbar
         )

# Optional: You can also create a corrplot showing the number of observations used
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

#### Species plotting ####
csn_imp_species = 
  read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/PMF_results/CSN_IMPROVE_Species_Csub.fst")
head(csn_imp_species); dim(csn_imp_species)

csn_imp_species_ion = 
  dplyr::select(csn_imp_species, Dataset, SiteCode, Date, NO3Ion, SO4Ion)
head(csn_imp_species_ion)
csn_imp_species_ion$Year = year(csn_imp_species_ion$Date)

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


#### 5. all site PM ####
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

# Overall PM
site_pm_overal = 
  site_pm_daily %>%
  dplyr::group_by(Dataset, State, SiteCode) %>%
  dplyr::summarise(
    PM25_med = median(PM25),
    PM25_mean = mean(PM25),
    .groups = "drop"
  )
head(site_pm_overal)


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
# biomass, sulfate, traffic, 7, 15, 4 in 2011, 13, 8, 8, in 2019
# bb+ss, 20 (55.5%) in 2011, 23 (63.9%) in 2019

attain_cty_domain_source_year = 
  data.table(table(attain_cty_domain_source$Year, 
                   attain_cty_domain_source$Source))
names(attain_cty_domain_source_year) =
  c("Year", "Source", "Count")
# biomass, sulfate, traffic, 42, 129, 7 in 2011, 101, 71, 13, in 2019
# bb+ss, 143 (56.1%) in 2011, 200 (78.4%) in 2019

table(non_attain_cty_domain_source$Source)/nrow(non_attain_cty_domain_source)
table(attain_cty_domain_source$Source)/nrow(attain_cty_domain_source)


