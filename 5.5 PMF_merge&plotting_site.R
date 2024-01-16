library(sf)
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

library(readr)
library(stringr)

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
month_source_assign$Source_use[grepl("peline", month_source_assign$Source_use, fixed = T)] = "F7-Non-Pipeline"

month_source_assign$Source_use[is.na(month_source_assign$Source_use)] = "F6-Fresh Sea Salt"

write.csv(month_source_assign, "Decided_monthly_SA_2024.01.csv")

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

Year_aggregated <- ddply(month_source_gps, .(Dataset.x, SiteCode, Year, Source_use), summarise,
                         Longitude = mean(Longitude),
                         Latitude = mean(Latitude),
                         Contribution = mean(Contribution))

month_aggregated <- ddply(month_source_gps, .(Dataset.x, SiteCode, Month, Source_use), summarise,
                          Longitude = mean(Longitude),
                          Latitude = mean(Latitude),
                          Contribution = mean(Contribution))

#### 1.1 plotting - annimation ####

setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_base_DISPres1/")
pathway = "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_base_DISPres1/"

for(source.type in unique(month_source_gps$Source_use)){
  
  dir.create(paste0(pathway, "CSN_", source.type))

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
  
  year_aggregated_ani = subset(year_aggregated, Source_use == source.type)
   
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
  
  month_aggregated_ani = subset(month_aggregated, Source_use == source.type)
  
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

  anim_year <- animate(year_animation, nframes = 100, fps = 10, width = 800, height = 600)
  anim_save(paste0(source.type, "_year_month_change.gif"), anim)

  anim_month <- animate(month_animation, nframes = 100, fps = 10, width = 800, height = 600)
  anim_save(paste0(source.type, "_year_month_change.gif"), anim)
  
  
}

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


