library(sf)
<<<<<<< HEAD
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
=======
library(dplyr)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(gganimate)
library(ggplot2)
library(purrr)
library(readr)
library(ggsci)
>>>>>>> origin/main

setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/")
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/"

#####  0. Source assignment results ######

overall_Assigned = read.csv("results_R_data/Decided_SA.csv")
org_assigned = read.csv("results_R_data/Source_assigned.csv")
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

<<<<<<< HEAD
=======
library(readr)
library(stringr)

>>>>>>> origin/main
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

#### 1.0 generate corresponding daily site SA results ####

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

month_source_assign = merge(combined_month, source_only_assign, all.x = T)
month_source_assign$Source_use = month_source_assign$final_assign
unique(month_source_assign$Source_use)
unique(month_source_assign$Source_reference)
 
month_source_assign$Source_use[grepl("Vehicle + biomass", month_source_assign$Source_use, fixed = T)] = "Vehic_Biom"
month_source_assign$Source_use[grepl("Soil", month_source_assign$Source_use, fixed = T)] = "F9-Soil/Dust"
month_source_assign$Source_use[grepl("Vehicle", month_source_assign$Source_use, fixed = T)] = "F1-Vehicle"
month_source_assign$Source_use[grepl("Biomass", month_source_assign$Source_use, fixed = T)] = "F8-Biomass"
month_source_assign$Source_use[month_source_assign$Source_use == "Industry 2"] = "F10-Industries_2"
month_source_assign$Source_use[grepl("Industry", month_source_assign$Source_use, fixed = T)] = "F5-Industry"
month_source_assign$Source_use[grepl("Aged sea", month_source_assign$Source_use, fixed = T)] = "F4-Aged Sea Salt"
month_source_assign$Source_use[grepl("Fresh Sea", month_source_assign$Source_use, fixed = T)] = "F6-Fresh Sea Salt"
<<<<<<< HEAD
month_source_assign$Source_use[grepl("peline", month_source_assign$Source_use, fixed = T)] = "F7-Non-Tailpipe"

## For those not included in the results, manually assign
source_na = subset(month_source_assign, is.na(Source_use))
source_na_unique = 
  ddply(source_na,
        .(Main_Species, Source_reference, Cluster.No, Factor.No, Factor, 
          Dataset, final_assign, Source_use),
        summarise,
        Contribution = median(Contribution))

# Identify the rows where 'Source_use' does not contain "Factor" and 'final_assign' is NA
rows_to_update <- !grepl("Factor", month_source_assign$Source_reference, fixed = TRUE) &
  is.na(month_source_assign$final_assign)
unique(month_source_assign[rows_to_update,]$Source_reference)
# Assign 'Source_reference' values to 'Source_use' for these rows
month_source_assign$Source_use[rows_to_update] <- month_source_assign$Source_reference[rows_to_update]

rows_to_update_2 <- grepl("Si Al Ti Fe", month_source_assign$Main_Species, fixed = TRUE) &
  is.na(month_source_assign$Source_use)
View(month_source_assign[rows_to_update_2,])
unique(month_source_assign[rows_to_update_2,]$Source_reference)
month_source_assign$Source_use[rows_to_update_2] <- "F9-Soil/Dust"

# convert those not assigned to "F10-Industries_2"
source_not_assign = subset(month_source_assign, is.na(Source_use))
unique(source_not_assign$Main_Species)

month_source_assign$Source_use[is.na(month_source_assign$Source_use)] = "F10-Industries_2"

write.csv(month_source_assign, "Decided_monthly_SA_2024.01.csv")

# combine two or more industry sources
month_assign_industry_combine = month_source_assign
month_assign_industry_combine$Source_use[month_assign_industry_combine$Source_use == "F10-Industries_2"] = "F5-Industry"

### TO BE FINISHED

#### check the result for single cluster
select.cluster = 1
month_source_assign_1c = subset(month_source_assign,
                                Cluster.No == select.cluster)
source_1c_contri = ddply(month_source_assign_1c,
                         .(Source_use),
                         summarise,
                         Contribution = mean(Contribution, na.rm = T))
summary(source_1c_contri)

=======
month_source_assign$Source_use[grepl("peline", month_source_assign$Source_use, fixed = T)] = "F7-Non-Pipeline"

month_source_assign$Source_use[is.na(month_source_assign$Source_use)] = "F6-Fresh Sea Salt"

write.csv(month_source_assign, "Decided_monthly_SA_2024.01.csv")

>>>>>>> origin/main
#### 1.1 plotting - data preparation ####
library(USAboundaries)
library(ggsci)
library(gganimate)

color_npg = pal_npg("nrc")(10)

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

cty_rural_urban = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$X.1 = NULL
cty_rural_urban = cty_rural_urban[!duplicated(cty_rural_urban$SiteCode), ] 

month_contri_gps = merge(month_source_assign, 
                         cty_rural_urban,
                         by = "SiteCode",
                         all.x = T)


month_contri_gps$geoid = ifelse(month_contri_gps$geoid < 10000, 
                                 paste0("0", month_contri_gps$geoid), 
                                 month_contri_gps$geoid)


month_source_gps = merge(us_cty_bdr_geo, month_contri_gps)
<<<<<<< HEAD
month_source_gps$Dataset.x = "CSN"

Year_aggregated <- ddply(month_source_gps, 
                         .(Dataset.x, SiteCode, Year, Source_use), 
                         summarise,
=======

Year_aggregated <- ddply(month_source_gps, .(Dataset.x, SiteCode, Year, Source_use), summarise,
>>>>>>> origin/main
                         Longitude = mean(Longitude),
                         Latitude = mean(Latitude),
                         Contribution = mean(Contribution))

<<<<<<< HEAD
year_month_aggregated <- ddply(month_source_gps, 
                               .(Dataset.x, SiteCode, Year, Month, Source_use), 
                               summarise,
                               Longitude = mean(Longitude),
                               Latitude = mean(Latitude),
                               Contribution = mean(Contribution))

=======
>>>>>>> origin/main
month_aggregated <- ddply(month_source_gps, .(Dataset.x, SiteCode, Month, Source_use), summarise,
                          Longitude = mean(Longitude),
                          Latitude = mean(Latitude),
                          Contribution = mean(Contribution))

<<<<<<< HEAD
write.csv(Year_aggregated, "Annual_site_source_contribuion_CSN_2024.01.csv")
write.csv(year_month_aggregated, "Year-month_site_source_contribuion_CSN_2024.01.csv")
write.csv(month_aggregated, "Month_site_source_contribuion_CSN_2024.01.csv")

#### 1.2 mapping - annual & map ####

###### 1.2.0. data selection, choose one from data-1 & data-2 ######

color_npg = pal_npg("nrc")(10)

####### data-1, averages of the PMF outputs

Year_aggregated = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/Annual_site_source_contribuion_CSN_2024.01.csv")
month_aggregated = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/Month_site_source_contribuion_CSN_2024.01.csv")
Year_aggregated$X = month_aggregated$X = NULL

Year_aggregated_use = subset(Year_aggregated, 
                             !(Source_use %in% c("F10-Industries_2", "Vehic_Biom")))

Month_aggregated_use = subset(month_aggregated, 
                              !(Source_use %in% c("F10-Industries_2", "Vehic_Biom")))

site_no_source <- 
  Year_aggregated_use %>%
  group_by(Source_use) %>%
  dplyr::summarise(Unique_SiteCode_No = n_distinct(SiteCode))

####### data-2, estimates from linear regression based on PMF outputs

site_gps = read.csv("CSN_GPS.csv")
source_contribution = read.csv("CSN_Site_source_contribution_overall_annual_month_2024.01.csv")
source_contribution$X = NULL
source_contribution = merge(source_contribution, site_gps)
colnames(source_contribution)[3]
source_contribution$Source_use = source_contribution$Source

source_contribution <- source_contribution %>%
  mutate(Source_use = case_when(
    Source == "F1_Vehicle"     ~ "F1-Vehicle",
    Source == "F2_SecNitrate"  ~ "F2-Secondary Nitrate",
    Source == "F3_SecSulfate"  ~ "F3-Secondary Sulfate",
    Source == "F4_AgedSeaSalt" ~ "F4-Aged Sea Salt",
    Source == "F5_Industry" ~ "F5-Industry",
    Source == "F6_FreshSeaSalt" ~ "F6-Fresh Sea Salt",
    Source == "F7_NonTailpipe" ~ "F7-Non-Tailpipe",
    Source == "F8_Biomass" ~ "F8-Biomass",
    Source == "F9_SoilDust" ~ "F9-Soil/Dust",
    TRUE                       ~ Source_use  # Keep existing value for other cases
  ))

Year_aggregated_use = subset(source_contribution, 
                             Period %in% 2011:2020)
colnames(Year_aggregated_use)[ncol(Year_aggregated_use)-3] = "Year"
Year_aggregated_use$Contribution = Year_aggregated_use$Source.concentration
Year_aggregated_use$Year = as.integer(Year_aggregated_use$Year)

Month_aggregated_use = subset(source_contribution, 
                              !(Period %in% 2011:2020 | Period == "overall"))
Month_aggregated_use$Contribution = Month_aggregated_use$Source.concentration
Month_aggregated_use$Month = 
  as.integer(
    format(
      as.Date(
        paste0(Month_aggregated_use$Period, "-01"), 
        format = "%Y-%m-%d"), 
      format = "%m"))

overall_contri = subset(source_contribution, Period == "overall")
overall_contri$Contribution = overall_contri$Source.concentration

site_no_source <- 
  Year_aggregated_use %>%
  group_by(Source_use) %>%
  dplyr::summarise(Unique_SiteCode_No = n_distinct(SiteCode))

###### 1.2.1. annual, month, box & trend plot ######

########### Overall, only for data-2 for now

# histgram - each source

ggplot(subset(overall_contri, Contribution>0), 
       # aes(x = Source.percent)) + # , fill = Source_use
        aes(x = Contribution)) + # , fill = Source_use
  geom_histogram(alpha = 0.6, bins = 30) +
  facet_wrap(~ Source_use, ncol = 3) +
  # labs(x = "Percent Contribution  %", y = "Count") +
  labs(x = format_variable("Contribution  µg/m3"), y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 15, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))

overall_contri_sum = 
  ddply(overall_contri, .(Source_use), summarise,
        mean = round(mean(Contribution), 3), 
        sd = round(sd(Contribution), 3), 
        median = round(median(Contribution), 3), 
        p99th = round(quantile(Contribution, 0.99), 3), 
        p1th = round(quantile(Contribution, 0.01), 3))

export_table(overall_contri_sum, format = "md")

overall_percent_sum = 
  ddply(overall_contri, .(Source_use), summarise,
        mean = round(mean(Source.percent), 3), 
        sd = round(sd(Source.percent), 3), 
        median = round(median(Source.percent), 3), 
        p99th = round(quantile(Source.percent, 0.99), 3), 
        p1th = round(quantile(Source.percent, 0.01), 3))

export_table(overall_percent_sum, format = "md")

########### Annual

middle_position = 
  data.frame(
    Year = rep(as.factor(2016), 
               length(unique(Year_aggregated_use$Source_use))),
    Source_use = unique(Year_aggregated_use$Source_use))

# boxplot - annual
ggplot(subset(Year_aggregated_use, 
              Contribution < quantile(Contribution, 0.995)),
       aes(as.factor(Year), Contribution),
       color = Source_use) +
  geom_boxplot(aes(color = Source_use), 
               outlier.shape = NA, 
               linewidth = 0.3, width = 0.5) +
  geom_jitter(aes(color = Source_use), 
              width = 0.15, alpha = 0.15)+
  facet_grid(Source_use ~.) + # , scales = "free_y"
  # ylim(0,4) +
  scale_y_continuous(breaks = c(0, 1, 2)) +
  # scale_x_discrete(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
  xlab("Year") +
  scale_color_manual(values = color_npg) + # color_npg[-c(1, 5, 7)]
  geom_text(data = middle_position, size = 5,
            aes(x = Year, y = 2, label = Source_use), 
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
              Contribution > 0), 
       aes(x = Contribution)) + # , fill = Source_use
  geom_histogram(alpha = 0.6, bins = 30) +
  facet_wrap(~ Source_use, ncol = 3) +
  labs(x = format_variable("Contribution  µg/m3"), y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 15, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))
  
annual_contribution_source = 
  ddply(Year_aggregated_use, .(Source_use), summarise,
        mean = round(mean(Contribution), 3), 
        sd = round(sd(Contribution), 3), 
        median = round(median(Contribution), 3), 
        p975th = round(quantile(Contribution, 0.975), 3), 
        p025th = round(quantile(Contribution, 0.025), 3))

export_table(annual_contribution_source, format = "md")
  
  
# ribbon range figure
Year_aggregated_summary = 
  ddply(Year_aggregated_use, 
        .(Source_use, Year),
        summarise,
        Lower = quantile(Contribution, 0.0025),
        Median = median(Contribution),
        Upper = quantile(Contribution, 0.9975))
Year_aggregated_summary$Year = as.integer(Year_aggregated_summary$Year)

ggplot(Year_aggregated_summary, 
       aes(x = Year, y = Median, color = Source_use, fill = Source_use)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(2011, 2014, 2017)) + 
  #scale_y_continuous(limits = c(0, 4), breaks = c(0, 2, 4)) +  # Set y-axis limits and breaks
  labs(y = "Contribution") + #  µg/m^3
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

contri_range1 <- quantile(Month_aggregated_use$Contribution, 
                          c(0.025, 0.975), na.rm = T)

ggplot(subset(Month_aggregated_use, 
              Contribution < contri_range1[2] &
                Contribution > contri_range1[1]),
       aes(as.factor(Month), Contribution),
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
  xlab("Month") +
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
       aes(x = Contribution)) + # , fill = Source_use
  geom_histogram(alpha = 0.6, bins = 30) +
  facet_wrap(~ Source_use, ncol = 3) +
  labs(x = format_variable("Contribution  µg/m3"), y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 15, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))

month_contribution_source = 
  ddply(Month_aggregated_use, .(Source_use), summarise,
        mean = round(mean(Contribution, na.rm = T), 3), 
        sd = round(sd(Contribution, na.rm = T), 3), 
        median = round(median(Contribution, na.rm = T), 3), 
        p99th = round(quantile(Contribution, 0.99, na.rm = T), 3), 
        p1th = round(quantile(Contribution, 0.01, na.rm = T), 3))

export_table(month_contribution_source, format = "md")

# ribbon range figure

Month_aggregated_summary = 
  ddply(Month_aggregated_use, 
        .(Source_use, Month),
        summarise,
        Lower = quantile(Contribution, 0.0025, na.rm = T),
        Median = median(Contribution, na.rm = T),
        Upper = quantile(Contribution, 0.9975, na.rm = T))


ggplot(Month_aggregated_summary, 
       aes(x = Month, y = Median, color = Source_use, fill = Source_use)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) + 
  #scale_y_continuous(limits = c(0, 4), breaks = c(0, 2, 4)) +  # Set y-axis limits and breaks
  labs(y = "Contribution") + #  µg/m^3
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


###### 1.2.2. Spatial distribution of Changes between 2011 & 2020 - Absolute difference ######

# Load the dplyr package
library(dplyr)

# Filter the dataset for the years 2011 and 2019
annual_source_selectd <- Year_aggregated %>% 
  filter(Year %in% c(2011, 2019))

contribution_diff <- 
  annual_source_selectd %>%
  dplyr::group_by(Source_use, SiteCode) %>%
  dplyr::summarise(
    diff_contribution = ifelse(n() > 1, diff(Contribution), NA),
    Longitude = last(Longitude),
    Latitude = last(Latitude),
    # geoid = last(geoid),
    # state_abbr = last(state_abbr),
    # geometry = last(geometry),
    .groups = 'drop'  # This will automatically ungroup the data
  )


contribution_diff = subset(contribution_diff, 
                           !(Source_use %in% c("F10-Industries_2", "Vehic_Biom")))

# Create the plot
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = subset(source9_diff_map, 
                           !is.na(diff_contribution)), 
             aes(x = Longitude, y = Latitude, 
                 color = diff_contribution),
             size = 2.5, alpha = 0.8) +
  scale_color_gradient2(low = color_npg[2], 
                        high = color_npg[8], 
                        midpoint = 0) +
  coord_sf(datum = NA) +
  facet_wrap(~ Source_use, 
             labeller = labeller(Source_use = 
                                   as_labeller(as.character, 
                                               default = label_value))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16))


###### 1.2.3. Spatial distribution of Changes between 2011 & 2020 - Slope ######

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
    diff_slope = get_slope(cur_data(), "Year", "Contribution")
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
slope_range1 <- c(-0.3, 0.3)

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = slope_diff, 
             aes(x = Longitude, y = Latitude, 
                 color = diff_slope),
             size = 2.5, alpha = 0.8) +
  scale_color_gradient2(limits = slope_range1,
                        low = color_npg[2], 
                        mid = "ivory", # cornsilk1, honeydew
                        high = color_npg[8], 
                        midpoint = 0) +
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


#### 1.3 Spatial & temporal (annual) ####

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

###### 1.3.1 Temporal trends & Map - common setting for regions ###### 

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

###### 1.3.2 Temporal trends & Map - basic source data ###### 

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

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F4-Aged Sea Salt")
col_singleSource = "green"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F5-Industry")
col_singleSource = "orange"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F6-Fresh Sea Salt")
col_singleSource = "cyan"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F8-Biomass")
col_singleSource = "pink"

annual_singleSource = 
  subset(annual_source_gps, 
         Source_use == "F9-Soil/Dust")
col_singleSource = "brown"


###### 1.3.3 Temporal trends & Map - specific source ######

annual_singleSource_region = merge(annual_singleSource, state_regions)

annual_singleSource_region_contri = 
  annual_singleSource_region %>%
  group_by(region) %>%
  dplyr::summarize(Contribution = mean(Contribution)) 
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
  ddply(subset(annual_singleSource_region, Contribution>0), 
        .(region, Year),
        summarise,
        med.contri = median(Contribution),
        up.contri = quantile(Contribution, 0.975),
        down.contri = quantile(Contribution, 0.025),
        Longitude = last(Longitude),
        Latitude = last(Latitude),
        geoid = last(geoid),
        state_abbr = last(state_abbr)
  )

# create the data list, one for each region
annual_singleSource_region_split = split(annual_singleSource_region_plot, 
                                         annual_singleSource_region_plot$region)
max.contri.singleSource = max(annual_singleSource_region_plot$up.contri)

###### 1.3.4 arrange with grid.arrange ######

# center map
singleSource_map_center <- 
  ggplot() +
  geom_sf(data = us_states_region, 
          fill = NA, color = "lightgrey") +  # Fill color for states without border
  geom_sf(data = regions_dissolved_annual_singleSource, 
          aes(fill = Contribution), 
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
                             title=format_variable("Contribution \n (µg/m3)"))) +
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
           y = "Contribution") +
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

#### 1.4 year-month time series ####
`
year_month_aggregated = read.csv("Year-month_site_source_contribuion_CSN_2024.01.csv")
year_month_aggregated$X = NULL
year_month_aggregated$Date = 
  paste0(year_month_aggregated$Year, "-", year_month_aggregated$Month)


as.Date(year_month_aggregated$Date[1], format = "%Y-%m")


#### 1.5 annimation ####
=======
#### 1.1 plotting - annimation ####
>>>>>>> origin/main

setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_base_DISPres1/")
pathway = "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_base_DISPres1/"

<<<<<<< HEAD
Year_aggregated = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/annual_results/Annual_site_source_contribuion_CSN_2024.01.csv")
Year_aggregated$X = NULL

Month_aggregated = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/annual_results/Month_site_source_contribuion_CSN_2024.01.csv")
Month_aggregated$X = NULL

month_source_gps = subset(month_source_gps,
                          !(Source_use %in% c("F10-Industries_2", "Vehic_Biom")))

for(source.type in unique(month_source_gps$Source_use)){
  
  dir.create(paste0(pathway, "CSN_", source.type))
  
=======
for(source.type in unique(month_source_gps$Source_use)){
  
  dir.create(paste0(pathway, "CSN_", source.type))

>>>>>>> origin/main
  month_source_plot = subset(month_source_gps, Source_use == source.type)
  month_source_plot$YearMonth <- with(month_source_plot, paste(Year, Month, sep = "-"))
  
  year_month_animation <- 
    ggplot() +
    geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
    geom_point(data = month_source_plot, 
               aes(x = Longitude, y = Latitude, color = Contribution, group = interaction(Year, Month)), 
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
  
<<<<<<< HEAD
  year_aggregated_ani = subset(Year_aggregated, Source_use == source.type)
  
=======
  year_aggregated_ani = subset(year_aggregated, Source_use == source.type)
   
>>>>>>> origin/main
  year_animation <- 
    ggplot() +
    geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
    geom_point(data = Year_aggregated_ani, 
               aes(x = Longitude, y = Latitude, color = Contribution, group = Year), 
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
  
<<<<<<< HEAD
  month_aggregated_ani = subset(Month_aggregated, Source_use == source.type)
=======
  month_aggregated_ani = subset(month_aggregated, Source_use == source.type)
>>>>>>> origin/main
  
  month_animation <- 
    ggplot() +
    geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
    geom_point(data = month_aggregated_ani, 
               aes(x = Longitude, y = Latitude, color = Contribution, group = Month), 
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
<<<<<<< HEAD
  
  anim_year <- animate(year_animation, nframes = 100, fps = 10, width = 800, height = 600)
  anim_save(paste0(source.type, "_year_month_change.gif"), anim)
  
=======

  anim_year <- animate(year_animation, nframes = 100, fps = 10, width = 800, height = 600)
  anim_save(paste0(source.type, "_year_month_change.gif"), anim)

>>>>>>> origin/main
  anim_month <- animate(month_animation, nframes = 100, fps = 10, width = 800, height = 600)
  anim_save(paste0(source.type, "_year_month_change.gif"), anim)
  
  
}

<<<<<<< HEAD

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
  labs(x = format_variable("K Contribution above Thresholds  µg/m3"), y = "Count") +
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
=======
#### 1.2 mapping - 2011 vs. 2019 ####

Year_aggregated


#### 1.3 mapping - annual & map ####

annual_Biomass = subset(Year_aggregated, 
                        Source_use == "F8-Biomass")
annual_FreshSeaSalt = subset(Year_aggregated, 
                             Source_use == "F6-Fresh Sea Salt")
annual_AgedSeaSalt = subset(Year_aggregated, 
                            Source_use == "F4-Aged Sea Salt")
annual_SedNitrate = subset(Year_aggregated, 
                           Source_use == "F2-Secondary Nitrate")
annual_SedSulfate = subset(Year_aggregated, 
                           Source_use == "F3-Secondary Sulfate")
annual_SoilDust = subset(Year_aggregated, 
                         Source_use == "F9-Soil/Dust")
annual_Vehicle = subset(Year_aggregated, 
                        Source_use == "F1-Vehicle")
annual_Industry = subset(Year_aggregated, 
                         Source_use == "F5-Industry")


###### 1.3.1. annual, month, box ######

color_npg = pal_npg("nrc")(10)

Year_aggregated_use = subset(Year_aggregated, 
                             Source_use %in% c("F2-Secondary Nitrate", "F3-Secondary Sulfate", "F4-Aged Sea Salte", 
                                               "F6-Fresh Sea Salt", "F8-Biomass", "F9-Soil/Dust",
                                               "F5-Industry", "F1-Vehicle", "F7-Non-Pipeline"))
middle_position = 
  data.frame(
    Year = rep(as.factor(2016), 
               length(unique(Year_aggregated_use$Source_use))),
    Source_use = unique(Year_aggregated_use$Source_use))


ggplot(subset(Year_aggregated_use, 
              Contribution < quantile(Contribution, 0.995)),
       aes(as.factor(Year), Contribution),
       color = Source_use) +
  geom_boxplot(aes(color = Source_use), 
               outlier.shape = NA, 
               linewidth = 0.3, width = 0.5) +
  geom_jitter(aes(color = Source_use), 
              width = 0.15, alpha = 0.15)+
  facet_grid(Source_use ~.) + # , scales = "free_y"
  # ylim(0,4) +
  scale_y_continuous(breaks = c(0, 1, 2)) +
  # scale_x_discrete(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
  xlab("Year") +
  scale_color_manual(values = color_npg) + # color_npg[-c(1, 5, 7)]
  geom_text(data = middle_position, size = 5.5,
            aes(x = Year, y = 2, label = Source_use), 
            inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
  theme_bw() +
  theme(panel.grid = element_line(colour = "white"),
        # plot.title = element_text(hjust = 0.05, vjust = -25, size = 0),
        strip.background = element_blank(), strip.text = element_blank(), # facet title
        legend.position = "none",
        axis.title.x = element_text(color="grey25", size = 20, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 20, vjust=1, margin=margin(0,2,0,0)),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 18, angle = 45, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5))


Month_aggregated_use = subset(month_aggregated, 
                             Source_use %in% c("F2-Secondary Nitrate", "F3-Secondary Sulfate", "F4-Aged Sea Salte", 
                                               "F6-Fresh Sea Salt", "F8-Biomass", "F9-Soil/Dust",
                                               "F5-Industry", "F1-Vehicle", "F7-Non-Pipeline"))
middle_position = 
  data.frame(
    Month = rep(as.factor(6), 
               length(unique(Month_aggregated_use$Source_use))),
    Source_use = unique(Month_aggregated_use$Source_use))


ggplot(subset(Month_aggregated_use, 
              Contribution < quantile(Contribution, 0.995)),
       aes(as.factor(Month), Contribution),
       color = Source_use) +
  geom_boxplot(aes(color = Source_use), 
               outlier.shape = NA, 
               linewidth = 0.3, width = 0.5) +
  geom_jitter(aes(color = Source_use), 
              width = 0.15, alpha = 0.15)+
  facet_grid(Source_use ~.) + # , scales = "free_y"
  # ylim(0,4) +
  scale_y_continuous(breaks = c(0, 1, 2)) +
  # scale_x_discrete(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
  xlab("Month") +
  scale_color_manual(values = color_npg) + # color_npg[-c(1, 5, 7)]
  geom_text(data = middle_position, size = 5.5,
            aes(x = Month, y = 2.5, label = Source_use), 
            inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
  theme_bw() +
  theme(panel.grid = element_line(colour = "white"),
        # plot.title = element_text(hjust = 0.05, vjust = -25, size = 0),
        strip.background = element_blank(), strip.text = element_blank(), # facet title
        legend.position = "none",
        axis.title.x = element_text(color="grey25", size = 20, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 20, vjust=1, margin=margin(0,2,0,0)),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 18, angle = 45, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5))


###### 1.3.2. Spatial distribution of Changes between 2011 & 2020 ######

# Load the dplyr package
library(dplyr)

# Filter the dataset for the years 2011 and 2019
annual_source_selectd <- Year_aggregated %>% 
  filter(Year %in% c(2011, 2019))

# Calculate the differences in Contribution for each Source_use and SiteCode
contribution_diff <- 
  annual_source_selectd %>%
  dplyr::group_by(Source_use, SiteCode) %>%
  dplyr::summarise(
    diff_contribution = ifelse(n() > 1, diff(Contribution), NA),
    Longitude = last(Longitude),
    Latitude = last(Latitude),
    # geoid = last(geoid),
    # state_abbr = last(state_abbr),
    # geometry = last(geometry),
    .groups = 'drop'  # This will automatically ungroup the data
  )


contribution_diff = subset(contribution_diff, 
                           Source_use %in% c("F2-Secondary Nitrate", "F3-Secondary Sulfate", "F4-Aged Sea Salte", 
                                             "F6-Fresh Sea Salt", "F8-Biomass", "F9-Soil/Dust",
                                             "F5-Industry", "F1-Vehicle", "F7-Non-Pipeline"))

contribution_diff$col = contribution_diff$row = 1

contribution_diff$row[
  contribution_diff$Source_use %in%
    c("F6-Fresh Sea Salt", "F8-Biomass", "F9-Soil/Dust")] = 2

contribution_diff$row[
  contribution_diff$Source_use %in%
    c("F5-Industry", "F1-Vehicle", "F7-Non-Pipeline")] = 3

contribution_diff$col[
  contribution_diff$Source_use %in%
    c("F3-Secondary Sulfate", "F8-Biomass", "F1-Vehicle")] = 2

contribution_diff$col[
  contribution_diff$Source_use %in%
    c("F4-Aged Sea Salt", "F9-Soil/Dust", "F7-Non-Pipeline")] = 3

# Create a new variable to uniquely identify each facet
contribution_diff <- contribution_diff %>%
  mutate(facet_id = interaction(col, row, lex.order = TRUE))

contribution_diff = na.omit(contribution_diff)

contribution_diff <- contribution_diff %>%
  mutate(facet_id = interaction(Source_use, col, row, lex.order = TRUE))

# Create the plot
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = subset(contribution_diff, 
                           !is.na(diff_contribution)), 
             aes(x = Longitude, y = Latitude, 
                 color = diff_contribution),
             size = 2.5, alpha = 0.8) +
  scale_color_gradient2(low = color_npg[2], 
                        high = color_npg[8], 
                        midpoint = 0) +
  coord_sf(datum = NA) +
  facet_wrap(~ facet_id, 
             labeller = labeller(facet_id = 
                                   as_labeller(as.character, 
                                               default = label_value))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16))

>>>>>>> origin/main

