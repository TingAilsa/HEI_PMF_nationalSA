library(fst)
library(magrittr)
library(base)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)
library(sf)
library(lubridate)
library(ggplot2)
library(viridis)
library(USAboundaries)

## Extract long & points with the continental US
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]
us_states <- st_set_geometry(us_states, "geometry")

cmaq_sulfate = read.fst("/Users/TingZhang/Downloads/CMAQ_Sulfate_2011-01_2011-12.fst")
head(cmaq_sulfate); dim(cmaq_sulfate); sapply(cmaq_sulfate, class)

cmaq_sulfate_month_all = cmaq_sulfate
cmaq_sulfate_month_all$month = month(cmaq_sulfate_month_all$Date)
cmaq_sulfate_month = 
  cmaq_sulfate_month_all %>%
  group_by(month, Longitude, Latitude) %>%
  dplyr::summarise(
    NH3 = median(NH3),
    SO2 = median(SO2),
    O3 = median(O3),
    PM25_TOT_EGU = median(PM25_TOT_EGU)
  )
head(cmaq_sulfate_month); dim(cmaq_sulfate_month)

cmaq_sulfate_month_use = subset(cmaq_sulfate_month, month == 7)

cmaq_sulfate_avg = 
  cmaq_sulfate %>%
  group_by(Longitude, Latitude) %>%
  dplyr::summarise(
    NH3 = median(NH3),
    SO2 = median(SO2),
    O3 = median(O3),
    PM25_TOT_EGU = median(PM25_TOT_EGU)
  )
head(cmaq_sulfate_avg); dim(cmaq_sulfate_avg)

cmaq_rds_by_egu_plot <-
  ggplot() +
  geom_point(data = cmaq_sulfate_avg,
             aes(x = Longitude, y = Latitude, color = PM25_TOT_EGU),
             size = 0.35, alpha = 0.8) +
  geom_sf(data = us_states,
          fill = NA, color = "grey70", size = 0.3) +
  # facet_wrap(~cmaq_variable, ncol = 3) +
  scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Median CMAQ PM25_TOT_EGU in 2011") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 19),
    plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
    # plot.subtitle = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 19)
  )
cmaq_rds_by_egu_plot

cmaq_sulfate_month =
  subset(cmaq_sulfate, )
cmaq_daily_by_egu_plot <-
  ggplot() +
  geom_point(data = cmaq_sulfate_month_use,
             aes(x = Longitude, y = Latitude, color = PM25_TOT_EGU),
             size = 0.35, alpha = 0.8) +
  geom_sf(data = us_states,
          fill = NA, color = "grey70", size = 0.3) +
  # facet_wrap(~cmaq_variable, ncol = 3) +
  scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Median CMAQ PM25_TOT_EGU in 2011") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 19),
    plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
    # plot.subtitle = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 19)
  )
cmaq_daily_by_egu_plot


