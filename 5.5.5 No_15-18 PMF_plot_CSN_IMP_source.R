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


# pmf_both_perc = read_fst("CSN_IMPROVE_daily_SA_conc_perc.fst")
pmf_both_perc = fread("CSN_IMPROVE_Daily_Source_Impacts_2011-20.csv")

pmf_both_perc_no1518 = subset(
  pmf_both_perc,
  Date > as.Date("2018-09-30") | Date < as.Date("2015-11-01")
)

#### 1. Annual averages ####
pmf_site_no1518_ann =
  pmf_both_perc_no1518 %>%
  dplyr::group_by(Dataset, SiteCode, serial.No, Factor.No,
                  State, Latitude, Longitude, geoid, Source_aftermanual, Year) %>%
  dplyr::summarise(
    Concentration = mean(Concentration),
    Percent = mean(Percent),
    .groups = "drop"
  )

pmf_all_no1518_ann =
  pmf_both_perc_no1518 %>%
  dplyr::group_by(Dataset, Source_aftermanual, Year) %>%
  dplyr::summarise(
    Concentration = mean(Concentration),
    Percent = mean(Percent),
    .groups = "drop"
  )

pmf_all_ann =
  pmf_both_perc %>%
  dplyr::group_by(Dataset, Source_aftermanual, Year) %>%
  dplyr::summarise(
    Concentration = mean(Concentration),
    Percent = mean(Percent),
    .groups = "drop"
  )

nation_slope_ts <- 
  subset(pmf_all_ann, Dataset == "CSN") %>%
  group_by(Source_aftermanual) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(pick(everything()), "Year", "Concentration"),
    .groups = "drop"
  ) %>%
  ungroup()

nation_slope_no1518_ts <- 
  subset(pmf_all_no1518_ann, Dataset == "CSN") %>%
  group_by(Source_aftermanual) %>% # Dataset.x, 
  dplyr::summarize(
    diff_slope = get_slope_ts(pick(everything()), "Year", "Concentration"),
    .groups = "drop"
  ) %>%
  ungroup()

###### 2.1.2 Annual Theil-Sen trend for each source in each area ######
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

us_states_region_sf = 
  dplyr::select(us_states_region, region, state_abbr, geometry)

# convert to sf
pmf_site_no1518_ann_sf = 
  st_as_sf(pmf_site_no1518_ann, coords = c("Longitude", "Latitude"), crs = 4326)

# Perform the spatial join
annual_source_site <- 
  pmf_site_no1518_ann_sf %>%
  st_join(us_states_region_sf, join = st_within)

# Site by state
site_state = 
  dplyr::select(annual_source_site, SiteCode, state_abbr)
site_state = site_state[!duplicated(site_state), ]
subset(site_state, state_abbr == "TX")
site_state_count = data.frame(table(site_state$state_abbr))

site_state_Source_aftermanual = 
  dplyr::select(annual_source_site, SiteCode, state_abbr, Source_aftermanual)
site_state_Source_aftermanual = site_state_source[!duplicated(site_state_source), ]
subset(site_state_source, state_abbr == "TX" & Source_aftermanual == "F1-Traffic")
site_state_source_count = data.frame(table(site_state_source$state_abbr))

site_state_2011 = 
  dplyr::select(annual_source_site, Year, SiteCode, state_abbr, Source_aftermanual) %>%
  subset(Year == 2011)
site_state_2011 = site_state_2011[!duplicated(site_state_2011), ]
subset(site_state_2011, state_abbr == "TX" & Source_aftermanual == "F1-Traffic")
site_state_2011_count = data.frame(table(site_state_2011$state_abbr))

##### 2.1.3 Annual regional, national source contribution estimation ######

# Get regional median
annual_source_region <-
  annual_source_site %>%
  group_by(region, Source_aftermanual, Year) %>%
  dplyr::summarise(
    Concentration = median(Concentration),
    Percent = median(Percent),
    conc_999 = quantile(Concentration, 0.999),
    conc_001 = quantile(Concentration, 0.001),
    perc_999 = quantile(Percent, 0.999),
    perc_001 = quantile(Percent, 0.001),
    .groups = "drop")
head(annual_source_region); dim(annual_source_region)

# unique(annual_source_region$Source_aftermanual)
subset(annual_source_region, Source_aftermanual == "F3-Secondary Sulfate" & Year == 2011)
subset(annual_source_region, Source_aftermanual == "F8-Biomass" & Year == 2011)

annual_source_nation <-
  annual_source_site %>%
  st_drop_geometry() %>%
  group_by(Source_aftermanual, Year) %>%
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


##### 2.1.4 Thiel-Sen ######
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

# Overall 
subset(region_slope_diff_conc_ts, Source_aftermanual == "F3-Secondary Sulfate")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F8-Biomass")


subset(region_slope_diff_conc_ts, Source_aftermanual == "F1-Traffic")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F2-Secondary Nitrate")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F5-Industry")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F6-Salt")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F4-Non-tailpipe")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F8-Soil/Dust")
subset(region_slope_diff_conc_ts, Source_aftermanual == "F9-OP-rich")


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
View(region_slope_diff_perc_ts_use)


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




