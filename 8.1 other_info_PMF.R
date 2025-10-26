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

us_ap = read.csv("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Other_analyses/US_national_Air pollutant emission_2007-2022.csv")
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


#### Source contribution and meteorology, gridMET data ####
# setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds")
# getwd()
# 
# # Extract gridMET data from inputs for ML modeling
# sulfate_met = 
#   read_fst("machine_learning_source_input/Sulfate_ML_input_only_PMF_sites_2011-2020.fst")
# 
# sulfate_met = 
#   dplyr::select(sulfate_met, 
#                 Date, Longitude, Latitude, SiteCode, tmmn:land_type)
# head(sulfate_met)
# write_fst(sulfate_met, "machine_learning_source_input/PMF_site_gridMET_2011-202.fst")

# gridMET
pmf_met = read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/PMF_site_gridMET_2011-202.fst")

# PMF source
pmf_source = 
  read.fst("/Users/TingZhang/Dropbox/HEI_US_PMF/PMF_results/CSN_IMPROVE_Daily_Source_Impacts_region_2011-20.fst")
pmf_source =
  plyr::rename(pmf_source,
               c("Latitude" = "lat",
                 "Longitude" = "long"))

# Merge
pmf_source_met =
  merge(
    dplyr::select(pmf_source, -Percent, -Source), 
    pmf_met, 
    by = c("Date", "SiteCode"))
head(pmf_source_met)

pmf_source_met_wd =
  pmf_source_met %>%
  pivot_wider(
    names_from = "Source_aftermanual",
    values_from = "Concentration"
  )
head(pmf_source_met_wd); names(pmf_source_met_wd)
names(pmf_source_met_wd)[22:30]
names(pmf_source_met_wd)[22:30] =
  c("Industry", "Soil_Dust", "Sec_Nitrate", "Traffic", "Sulfate", 
    "Salt", "Biomass", "Non_tailpipe", "OP_rich")

#### Dataset for corrplot, correlation analyses
# Drop SiteCode and Date for correlation
pmf_source_met_num <- 
  dplyr::select(pmf_source_met_wd, tmmn:th, Industry:Non_tailpipe)
head(pmf_source_met_num)

# Calculate correlation, relevance, and number of complete cases matrix
pmf_source_met_cor <- 
  psych::corr.test(pmf_source_met_num, use = "na.or.complete") #  "pairwise.complete.obs"
pmf_source_met_cor_matrix <- pmf_source_met_cor$r
pmf_source_met_p_matrix <- pmf_source_met_cor$p
pmf_source_met_n_matrix <- pmf_source_met_cor$n

# The mask for significant correlations (p < 0.05)
pmf_source_met_p_mask <- pmf_source_met_p_matrix > 0.05

# Set up color palette
col_palette <- 
  colorRampPalette(c("#4477AA", "white", "#EE6677"))(100)

# Create and display correlation plot, only show those with p < 0.05
corrplot(pmf_source_met_cor_matrix, 
         method = "color",
         type = "upper",
         diag = TRUE,
         tl.col = "black",
         tl.srt = 45,
         col = col_palette,
         # p.mat = pmf_source_met_p_mask,      # Add p-value matrix
         sig.level = 0.05,      # Significance level
         insig = "blank",       # Hide insignificant correlations
         addCoef.col = "black", 
         number.cex = 0.9,
         tl.cex = 0.9,
         cl.pos = "n"  # Remove the legend/colorbar
)

# Optional: Corrplot showing the number of observations used
# This can help identify correlations based on too few data points
corrplot(pmf_source_met_n_matrix, 
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


