library(base)
library(stringr)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)
library(sf)
library(lubridate)
library(fst)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(USAboundaries)
library(viridis)
library(ggh4x)


setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Other_analyses")
getwd()


#### Plot for air pollutant emission in US ####

us_ap = read.csv("US_national_Air pollutant emission_2007-2022.csv")
head(us_ap); sapply(us_ap, class)

us_ap_use = 
  dplyr::select(us_ap, Year, SO2, NOx, NH3) %>% # PM2.5, 
  pivot_longer(
    cols = SO2:NH3,
    values_to = "Emission",
    names_to = "Air_Pollutant"
  )
head(us_ap_use)

ggplot(us_ap_use, 
       aes(x = Year, y = Emission, color = Air_Pollutant)) +
  geom_line() +
  geom_point() +
  labs(title = "US air pollutant emission by year",
       y = "Emissions (thousands of tons)") +
  scale_x_continuous(
    limits = c(2011, 2020),
    breaks = function(x) pretty(x, n = 4)) +
  scale_y_continuous(
    limits = c(0, NA), 
    breaks = function(x) pretty(x, n = 4)) +
  theme_bw(base_size = 20) +
  theme(plot.background = element_rect(color = NA),
        panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text( size = 20),
        # axis.title.y = element_text( size = 0),
        axis.text.x = element_text( angle = 0, hjust = 0.5, vjust = 1))





