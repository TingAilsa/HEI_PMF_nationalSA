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
head(pmf_both_perc); dim(pmf_both_perc)

# Get annual median & range
Year_aggregated_use <- 
  ddply(pmf_both_perc, 
        .(SiteCode, Year, Source_aftermanual), 
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

#### 1. Thiel-Sen, site level ####

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

###### 1.1 Thiel-Sen, the slope for each site across the study period ######
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

###### 1.2 Thiel-Sen, plotting, source slope, bar figure ######
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
  

###### 1.3 Thiel-Sen, plotting, spatial distribution ######
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


#### 2. Spatial & temporal (annual) ####

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

###### 2.1 SP Map - common setting for regions ###### 

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

###### 2.1.2 Annual Theil-Sen trend for each source in each area ######
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

subset(region_slope_diff_conc_ts, Source_aftermanual == "F3-Secondary Sulfate")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F8-Biomass")

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

# Rbind
region_slope_diff_conc_ts$Class = "Concentration"
region_slope_diff_perc_ts$Class = "Percent"

region_slope_diff = rbind(region_slope_diff_conc_ts, region_slope_diff_perc_ts)
head(region_slope_diff)

write.csv(region_slope_diff, "CSN_IMPROVE_region_Theil-Sen.csv")

###### 2.2 SP Map - basic source data, color define ###### 

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


###### 2.3.1 SP Map - Concentration - contribution trend estimation ######

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

###### 2.3.2 SP Map - Concentration - plotting ######

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

###### 2.4.1 SP Map - Percent - contribution trend estimation ######

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


###### 2.4.2 SP Map - Percent - plotting ######

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





