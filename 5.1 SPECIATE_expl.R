##clear environment
# rm(list=ls())

##set working directory
# setwd("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/SPECIATE_2018")
# getwd()
# data.dir <- "/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/SPECIATE_2018"

setwd("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/SPECIATE_2018")
getwd()
data.dir <- "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/SPECIATE_2018"

##packages in need
require(tidyr) # separate{tidyr}, gather{tidyr}, spread{tidyr},  spread is VIP function, str_split_fixed{stringr} is better than separate
require(stats) # aggregate{stats}
require(scales) # percent{}
require(stringr) # str_split_fixed, separate one column into multiple
require(dplyr)
require(plyr)
require(lubridate)
library(data.table)
library(ggrepel)

library(ggpubr)
library(gridExtra) #grid.arrange{}
library(grid) #textGrob{}
library(ggplot2)
library(ggsci)

#### FINISHED -- data match and extract -- FINISHED ####
pm_whole_speciate = read.csv("SPECIATE_PM2.5ProfileSimplified.csv")
pm_whole_speciate = select(pm_whole_speciate, 
                           PROFILE_CODE, PROFILE_NAME,
                           TOTAL, TEST_YEAR, REGION,
                           SPECIES_ID, SPECIES_NAME, 
                           WEIGHT_PERCENT, UNCERTAINTY_PERCENT,
                           CAS, CAS.no.hyphen)

# check name of OC/EC subgroups in dataset, match by name
species.include = unique(pm_whole_speciate$SPECIES_NAME)
species.include[grepl("Element", species.include, fixed=T)]
species.include[grepl("Organic", species.include, fixed=T)]
species.include[grepl("Organic P", species.include, fixed=T)]

# check name of ions
species.include[grepl("Potassium", species.include, fixed=T)]
species.include[grepl("Ammonium", species.include, fixed=T)]
species.include[grepl("Sodium", species.include, fixed=T)]
species.include[grepl("Chloride", species.include, fixed=T)]
species.include[grepl("Nitrate", species.include, fixed=T)]
species.include[grepl("Sulfate", species.include, fixed=T)]
species.include[grepl("Carbonate", species.include, fixed=T)]

# match with CompName set for CSN/IMPROVE dataset
species_name = read.csv("Species_name_CompName.csv")

pm_whole_speciate = merge(pm_whole_speciate, 
                          species_name,
                          all.x = T)
# reorder
pm_whole_speciate = pm_whole_speciate[with(pm_whole_speciate, 
                                           order(PROFILE_CODE, REGION, 
                                                 Class, Sequence, CompName)), ]

# separate into OC-domain speciate & not-OC domain
pm_OC_speciate = subset(pm_whole_speciate, 
                        is.na(CompName))
pm_csn_speciate = subset(pm_whole_speciate, 
                         !is.na(CompName))
write.csv(pm_OC_speciate, "SPECIATE_PM2.5_species_OC_domain.csv")
write.csv(pm_csn_speciate, "SPECIATE_PM2.5_species_common_CSN.csv")


#### START -- check source profile ####
pm_csn_speciate = read.csv("SPECIATE_PM2.5_species_common_CSN.csv")
pm_csn_speciate$X = NULL

# below steps can ensure the order of labels in x-axis follow the preset order 
# when reorder, no considering of region, only following class & CompName
pm_csn_speciate = pm_csn_speciate[with(pm_csn_speciate, 
                                           order(Class, Sequence, CompName)), ]
pm_csn_speciate$CompName <- factor(pm_csn_speciate$CompName, 
                                   levels = unique(pm_csn_speciate$CompName))

profile.included = unique(pm_csn_speciate$PROFILE_NAME)
length(profile.included)

#### Plot theme & function for font of ions ####
# function to format ion variables
# \u208+#, subscript#; \u00B+#, superscript#; \u207A, \u207B, superscript +/-

# Here name the ions with "Ion" instead of adding symbols inside cause this cause confusion for gsub
format_variable <- function(variable) {
  variable <- gsub("ClIon", "Cl\u207B", variable)
  variable <- gsub("NO3Ion", "NO\u2083\u207B", variable)
  variable <- gsub("SO4Ion", "SO\u2084\u00B2\u207B", variable)
  variable <- gsub("CO3Ion", "CO\u2083\u00B2\u207B", variable)
  variable <- gsub("NH4Ion", "NH\u2084\u207A", variable)
  variable <- gsub("NaIon", "Na\u207A", variable)
  variable <- gsub("KIon", "K\u207A", variable)
  variable <- gsub("PM25", "PM\u2082.\u2085", variable)
  variable <- gsub("m3", "µm\u00B3", variable)
  return(variable)
}

# test the function
variable = c("ClIon", "NO3Ion", "SO4Ion", "CO3Ion", "NH4Ion", 
             "NaIon", "KIon", "Cl", "Na", "K")
format_variable(variable)

# format_variable <- function(variable) {
#   variable <- gsub("Cl-", "Cl\u207B", variable)
#   variable <- gsub("NO3-", "NO\u2083\u207B", variable)
#   variable <- gsub("SO4(2-)", "SO\u2084\u00B2\u207B", variable)
#   variable <- gsub("CO3(2-)", "CO\u2083\u00B2\u207B", variable)
#   variable <- gsub("NH4+", "NH\u2084\u207A", variable)
#   variable <- gsub("Na+", "Na\u207A", variable)
#   variable <- gsub("K+", "K\u207A", variable)
#   return(variable)
# }
# variable = c("Cl-", "NO3-", "SO4(2-)", "CO3(2-)", "NH4+", "Na+", "K+", "Cl", "Na", "K")
# format_variable(variable)


# to display the super-/sub-scripts in figure, need to define the fonts that support the Unicode
# for example, theme(axis.text.x = element_text(family = "Arial Unicode MS"))

# to make it easier, define the font in theme for all text with theme_set
theme.species = theme_bw() +
  theme(axis.title.y.right = element_blank(),
        panel.spacing = unit(10, "mm"),   
        panel.background = element_rect(fill = NA, color = NA),
        legend.background = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="grey", size=16),
        axis.title.x = element_text(color="grey25", size = 16, vjust=-2,
                                    family = "Arial Unicode MS"), # , margin=margin(0,0,0,300)
        axis.title.y = element_text(color="grey25", size = 16, vjust=2,
                                    family = "Arial Unicode MS"), # , margin=margin(0,2,0,0)
        plot.title=element_text(size=rel(2), family = "Arial Unicode MS"),  
        axis.text.x = element_text(color="grey25", size = 14, angle = 90, 
                                   hjust = 0.5, vjust = 0.5,
                                   family = "Arial Unicode MS"), 
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.y = element_text(color="grey25", size = 14, angle = 0, 
                                   hjust = 0.5,
                                   family = "Arial Unicode MS"))

theme.species.up = theme_bw() +
  theme(axis.title.y.right = element_blank(),
        panel.spacing = unit(10, "mm"),   
        panel.background = element_rect(fill = NA, color = NA),
        legend.background = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="grey", size=16),
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), 
        plot.title=element_text(size=rel(2), family = "Arial Unicode MS"),  
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.y = element_text(color="grey25", size = 14, angle = 0, 
                                   hjust = 0.5,
                                   family = "Arial Unicode MS"))

theme.species.bottom = theme_bw() +
  theme(axis.title.y.right = element_blank(),
        panel.spacing = unit(10, "mm"),   
        panel.background = element_rect(fill = NA, color = NA),
        legend.background = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="grey", size=16),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(color="grey25", size = 16, vjust=-2,
                                    family = "Arial Unicode MS"), # , margin=margin(0,0,0,300)
        plot.title = element_blank(), 
        axis.text.x = element_text(color="grey25", size = 14, angle = 90, 
                                   hjust = 0.5, vjust = 0.5,
                                   family = "Arial Unicode MS"), 
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.y = element_text(color="grey25", size = 14, angle = 0, 
                                   hjust = 0.5,
                                   family = "Arial Unicode MS"))


#### Wildfire ####
# detect those with Wildfire

profile.Wildfire = profile.included[grepl("Wildfire", 
                                          profile.included, 
                                          fixed=T)]
                                    
csn_speciate_wildfire = subset(pm_csn_speciate, 
                               grepl("Wildfire", PROFILE_NAME, fixed=T))

ggplot(csn_speciate_wildfire, 
              aes(x = CompName, 
                  y = WEIGHT_PERCENT, 
                  color = Class)) +
  geom_vline(xintercept = c("K", "OC"), color = "aliceblue", size = 7) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color = "black", size=0.5, alpha=0.4) +
  scale_color_nejm() + 
  ggtitle("SPECIATE_Wildfire") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  theme(legend.position = c(0.5, 1.05), legend.direction = "horizontal")


#### Biomass ####
# detect those with biomass burning
profile.included[grepl("Wood", profile.included, fixed=T)]
# "Wood Stoves"  "Wood Combustion"  "Wood-fired"  "Wood Fired" "Woodstove" "Wood Burning"
profile.included[grepl("Biomass", profile.included, fixed=T)]
# "Biomass Burning"
profile.included[grepl("Grass", profile.included, fixed=T)]
profile.included[grepl("Forest", profile.included, fixed=T)]
profile.included[grepl("Fire", profile.included, fixed=T)]
# including oil fired power plant
profile.included[grepl("Burning", profile.included, fixed=T)]
# "Agricultural Burning", "Vegetative Burning" "Field Burning" "Slash Burning"
profile.included[grepl("Harvest Burning", profile.included, fixed=T)]
profile.included[grepl("Fireplaces", profile.included, fixed=T)]

profile.biomass = profile.included[(
  grepl("Wood", profile.included, fixed=T) &
    (grepl("Combustion", profile.included, fixed=T) |
       grepl("ove", profile.included, fixed=T) |
       grepl("ire", profile.included, fixed=T) |
       grepl("Burning", profile.included, fixed=T))) |
    grepl("Biomass", profile.included, fixed=T) |
    grepl("Grass", profile.included, fixed=T) |
    grepl("Forest", profile.included, fixed=T) |
    grepl("Agricultural Burning", profile.included, fixed=T) |
    grepl("Vegetative Burning", profile.included, fixed=T) |
    grepl("Field Burning", profile.included, fixed=T) |
    grepl("Slash Burning", profile.included, fixed=T) |
    grepl("Harvest Burning", profile.included, fixed=T) |
    grepl("Fireplaces", profile.included, fixed=T)]

csn_speciate_Biomass = subset(pm_csn_speciate, 
                              PROFILE_NAME %in% profile.biomass)
unique(csn_speciate_Biomass$CompName)

biomass_whole <- ggplot(csn_speciate_Biomass, 
                        aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(xintercept = c("EC1", "OC", "OC1"), 
             color = "aliceblue", size = 5) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color = "black", size=1, alpha=0.3) +
  scale_color_nejm() + 
  ggtitle("speciate_Biomass") +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species.up +
  theme(plot.margin=unit(c(1,1,0,1.15), "cm"), 
        legend.position = c(0.5, 1.07), legend.direction = "horizontal")
biomass_whole
# plot.margin, top, right, bottom, left

biomass_zoom <- ggplot(csn_speciate_Biomass, 
                       aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(xintercept = c("K", "KIon"), 
             color = "aliceblue", size = 5) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color = "black", size=1, alpha=0.3) +
  scale_color_nejm() + 
  ylim(0, 10) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species.bottom +
  theme(plot.margin=unit(c(0.2,1,1,1), "cm"),
        legend.position = "none")
biomass_zoom

# combine two plot
# grid.arrange(biomass_whole, biomass_zoom, ncol = 1, heights = c(1, 1), left = "Weight Percent")

# Combine the two plots
biomass_combine <- ggarrange(
  biomass_whole + rremove("ylab") + rremove("xlab"), 
  biomass_zoom + rremove("ylab") + rremove("xlab"), # remove axis labels from plots
  ncol = 1, nrow = 2, # common.legend = T, 
  # align = "hv", # graphs be horizontally "h", vertically "v", or both "hv" or none aligned
  font.label = list(size = 10, color = "black", face = "bold", 
                    family = "Arial Unicode MS", position = "top"))

# Set xlab & ylab
biomass_combine <- annotate_figure(
  biomass_combine, 
  left = textGrob("Weight Percent", rot = 90, vjust = 2, gp = gpar(cex = 1.3)),
  bottom = textGrob("PM"[2.5]~"Species", vjust = -1.5, gp = gpar(cex = 1.3)))
biomass_combine

# Save the modified plot
png("speciate_Biomass_combine.png",
    width = 32, height = 18, 
    units = "cm", res = 300,
    bg = "transparent") # somehow the output plot has no transparent background
print(biomass_combine)
dev.off()

#### SeaSalt ####
# detect those with SeaSalt burning
profile.included[grepl("Sea Salt", profile.included, fixed=T)]
# "Sea Salt - Composite"  "Draft Sea Salt - Simplified" "Sea Salt - Simplified"
profile.included[grepl("Marine Aerosol", profile.included, fixed=T)]

profile.SeaSalt = profile.included[
  grepl("Sea Salt", profile.included, fixed=T) |
    grepl("Marine Aerosol", profile.included, fixed=T)]

csn_speciate_SeaSalt = subset(pm_csn_speciate, 
                              PROFILE_NAME %in% profile.SeaSalt)
# subset(pm_csn_speciate, PROFILE_CODE == "91176")
unique(csn_speciate_SeaSalt$CompName)

ggplot(csn_speciate_SeaSalt, 
       aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color="black", size=1, alpha=0.3) +
  scale_color_nejm() + 
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  ggtitle("SPECIATE_SeaSalt") +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  theme(legend.position = "none")

csn_speciate_SeaSalt_use = select(csn_speciate_SeaSalt,
                                  SPECIES_NAME, PROFILE_CODE, PROFILE_NAME, 
                                  SPECIES_ID, WEIGHT_PERCENT, CompName)


#### Nitrate, Sulfate ####
# detect those with Nitrate burning
profile.included[grepl("Nitrate", profile.included, fixed=T)]
# Ammonium Nitrate series
profile.included[grepl("Sulfate", profile.included, fixed=T)]
# Ammonium Sulfate series
profile.included[grepl("Second", profile.included, fixed=T)]
# mostly secondary metal production or so

profile.Nitrate = profile.included[grepl("Nitrate", profile.included, fixed=T)]
profile.Sulfate = profile.included[grepl("Sulfate", profile.included, fixed=T)]

csn_speciate_Nitrate = subset(pm_csn_speciate, 
                              PROFILE_NAME %in% profile.Nitrate)
csn_speciate_Sulfate = subset(pm_csn_speciate, 
                              PROFILE_NAME %in% profile.Sulfate)
csn_speciate_Nitrate$Type = "Nitrate"
csn_speciate_Sulfate$Type = "Sulfate"
csn_speciate_NitrateSulfate = rbind(csn_speciate_Nitrate, csn_speciate_Sulfate)

unique(csn_speciate_NitrateSulfate$CompName)

ggplot(csn_speciate_NitrateSulfate, 
       aes(CompName, WEIGHT_PERCENT, color = Type)) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color="black", size=2.5, alpha=0.3) +
  facet_grid(.~ Type, scales = "free")+
  scale_color_nejm() + 
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  ggtitle("SPECIATE_NitrateSulfate") +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  theme(legend.position = "none")


#### Transportation - Vehicle Exhaust ####
# detect those with Vehicle
profile.included[grepl("Vehicle", profile.included, fixed=T)]
# Multiple
profile.included[grepl("Traffic", profile.included, fixed=T)]
profile.included[grepl("transit", profile.included, fixed=T)]
# profile.included[grepl("Exhaust", profile.included, fixed=T)], include aircraft
profile.included[grepl("Transportation", profile.included, fixed=T)]
profile.included[grepl("LDDV", profile.included, fixed=T)]
profile.included[grepl("HDDV", profile.included, fixed=T)]
# light-duty diesel vehicles (LDDVs), HDDV, heavy
# profile.included[grepl("Road", profile.included, fixed=T)], mostly dust
profile.included[grepl("Moto", profile.included, fixed=T)]
profile.included[grepl("Truck", profile.included, fixed=T)]
profile.included[grepl("Diesel", profile.included, fixed=T)]
profile.included[grepl("Gasoline", profile.included, fixed=T)]
# here the LA92 UDC cycle is from light duty vehicle
profile.included[grepl("CNG", profile.included, fixed=T)]
# CNG, Natural gas, Out of Contract Rates (OCR) and Deemed Rates (DR)

profile.Vehicle = profile.included[
  grepl("Diesel", profile.included, fixed=T) |
    grepl("Vehicle", profile.included, fixed=T) |
    grepl("Gasoline", profile.included, fixed=T) |
    grepl("Traffic", profile.included, fixed=T) |
    grepl("transit", profile.included, fixed=T) |
    grepl("Transportation", profile.included, fixed=T) |
    grepl("LDDV", profile.included, fixed=T) |
    grepl("HDDV", profile.included, fixed=T) |
    grepl("Moto", profile.included, fixed=T) |
    grepl("Truck", profile.included, fixed=T) |
    grepl("CNG", profile.included, fixed=T)]

csn_speciate_Vehicle = subset(pm_csn_speciate, 
                              PROFILE_NAME %in% profile.Vehicle)
unique(csn_speciate_Vehicle$CompName)

csn_speciate_Vehicle$Type = "NoInfo"
csn_speciate_Vehicle$Type[
  grepl("Diesel", csn_speciate_Vehicle$PROFILE_NAME, fixed=T)] = 
  "Diesel"
csn_speciate_Vehicle$Type[
  grepl("Gasoline", csn_speciate_Vehicle$PROFILE_NAME, fixed=T) |
    grepl("CNG", csn_speciate_Vehicle$PROFILE_NAME, fixed=T)] =
  "Gasoline"

# Define the position of vlines for different sub-facet
Vehicle_vlines <- data.frame(
  Type = rep(c("Diesel", "Gasoline"), each = 4),
  xintercept = rep(c("EC", "EC2", "OC", "OC1"), 2))

ggplot(csn_speciate_Vehicle, 
       aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(data = Vehicle_vlines, aes(xintercept = xintercept), 
             color = "aliceblue", size = 5) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color="black", size=0.5, alpha=0.3) +
  facet_grid(Type ~.) + 
  scale_color_nejm() + 
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  ggtitle("SPECIATE_Vehicle") +
  ylim(0, 100) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  theme(legend.position = c(0.5, 1.04), legend.direction = "horizontal") +
  # below is to minimize the space between facets
  theme(panel.spacing.y = unit(0, "cm")) 

ggplot(csn_speciate_Vehicle, 
       aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(xintercept = c("Fe", "Zn", "ClIon"), 
             color = "oldlace", size = 5) +  
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color="black", size=0.5, alpha=0.3) +
  facet_grid(Type ~.) + 
  scale_color_nejm() + 
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  ggtitle("SPECIATE_Vehicle") +
  ylim(0, 5) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  theme(legend.position = c(0.5, 1.04), legend.direction = "horizontal") +
  # below is to minimize the space between facets
  theme(panel.spacing.y = unit(0, "cm")) 


#### Transportation - ship & plane ####
# detect those with ship & plane
profile.included[grepl("Aircraft", profile.included, fixed=T)]
profile.included[grepl("Plane", profile.included, fixed=T)] # character(0)
profile.included[grepl("Vessel", profile.included, fixed=T)]
profile.included[grepl("Ship", profile.included, fixed=T)] # character(0)
profile.included[grepl("Boat", profile.included, fixed=T)] # character(0)

profile.AircraftVessel = profile.included[
  grepl("Aircraft", profile.included, fixed=T) |
    grepl("Vessel", profile.included, fixed=T)]

profile.Aircraft = profile.included[grepl("Aircraft", profile.included, fixed=T)]
profile.Vessel = profile.included[grepl("Vessel", profile.included, fixed=T)]

csn_speciate_Aircraft = subset(pm_csn_speciate, 
                              PROFILE_NAME %in% profile.Aircraft)
csn_speciate_Vessel = subset(pm_csn_speciate, 
                              PROFILE_NAME %in% profile.Vessel)
csn_speciate_Aircraft$Type = "Aircraft"
csn_speciate_Vessel$Type = "Vessel"
csn_speciate_AircraftVessel = rbind(csn_speciate_Aircraft, csn_speciate_Vessel)

unique(csn_speciate_AircraftVessel$CompName)

# Define the position of vlines for different sub-facet
AircraftVessel_vlines <- data.frame(
  Type = c(rep("Aircraft", 2),  
           rep("Vessel", 3)),
  xintercept = c("SO4Ion", "EC", 
                 "V", "SO4Ion", "OC"))

ggplot(csn_speciate_AircraftVessel,
       aes(CompName, WEIGHT_PERCENT, color = Type)) +
  geom_vline(data = AircraftVessel_vlines, aes(xintercept = xintercept), 
             color = "aliceblue", size = 9) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color="black", size=2.5, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  ggtitle("SPECIATE_AircraftVessel") +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  theme(legend.position = "none")


#### Transportation - tire & brake ####
# detect those with Vehicle tire & brake
profile.included[grepl("Tire", profile.included, fixed=T)]
profile.included[grepl("Brake", profile.included, fixed=T)]

profile.TireBrake = profile.included[
  grepl("Tire", profile.included, fixed=T) |
    grepl("Brake", profile.included, fixed=T)]

profile.Tire = profile.included[
  grepl("Tire", profile.included, fixed=T) &
    !(grepl("Burning", profile.included, fixed=T))]
profile.Tire.Burn = profile.included[
  grepl("Tire Burning", profile.included, fixed=T)]
profile.Brake = profile.included[
  grepl("Brake", profile.included, fixed=T)]

csn_speciate_Tire = subset(pm_csn_speciate, 
                               PROFILE_NAME %in% profile.Tire)
csn_speciate_Tire_Burn = subset(pm_csn_speciate, 
                                PROFILE_NAME %in% profile.Tire.Burn)
csn_speciate_Brake = subset(pm_csn_speciate, 
                             PROFILE_NAME %in% profile.Brake)
csn_speciate_Tire$Type = "Tire"
csn_speciate_Tire_Burn$Type = "Tire_Burn"
csn_speciate_Brake$Type = "Brake"
csn_speciate_TireBrake = rbind(csn_speciate_Tire, csn_speciate_Brake)
csn_speciate_TireBrake = rbind(csn_speciate_TireBrake, csn_speciate_Tire_Burn)

unique(csn_speciate_TireBrake$CompName)

# Define the position of vlines for different sub-facet
TireBrake_vlines <- data.frame(
  Type = c("Brake", "Brake", "Brake", 
           "Brake", "Brake", "Brake", 
           "Tire", "Tire"),
  xintercept = c("Ba", "Fe", "Mg", 
                 "Si", "EC", "OC",
                 "EC", "OC"))

ggplot(csn_speciate_TireBrake, 
       aes(CompName, WEIGHT_PERCENT, color = Type)) +
  geom_vline(data = TireBrake_vlines, 
             aes(xintercept = xintercept), 
             color = "aliceblue", size = 6) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color="black", size=1.2, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  ggtitle("SPECIATE_TireBrake") +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  theme(panel.spacing.y = unit(0.2, "cm")) +
  theme(legend.position = "none")


#### Fuel Type ####
# detect PROFILE_NAME with different fuel
profile.included[grepl("Diesel", profile.included, fixed=T)]
profile.included[grepl("Oil", profile.included, fixed=T)]
profile.included[grepl("Kerosene", profile.included, fixed=T)]
profile.included[grepl("Bunker", profile.included, fixed=T)]
profile.included[grepl("Fuel", profile.included, fixed=T)]
profile.included[grepl("Gas", profile.included, fixed=T)]
profile.included[grepl("Coal", profile.included, fixed=T)]
profile.included[grepl("Biomass", profile.included, fixed=T)]
profile.included[grepl("Lignite", profile.included, fixed=T)]
profile.included[grepl("Bituminous", profile.included, fixed=T)]
profile.included[grepl("Coke Calciner", profile.included, fixed=T)]
profile.included[grepl("Coke", profile.included, fixed=T)]
profile.included[grepl("Peat", profile.included, fixed=T)]

profile.Fuel = profile.included[
  grepl("Diesel", profile.included, fixed=T) |
    grepl("Gas", profile.included, fixed=T) |
    grepl("Coal", profile.included, fixed=T) |
    grepl("Oil", profile.included, fixed=T) |
    grepl("Kerosene", profile.included, fixed=T) |
    grepl("Lignite", profile.included, fixed=T) |
    grepl("Bituminous", profile.included, fixed=T) |
    grepl("Coke Calciner", profile.included, fixed=T) |
    grepl("Peat", profile.included, fixed=T)]

profile.Diesel = profile.included[
  grepl("Diesel", profile.included, fixed=T)]
profile.Gasoline = profile.included[
  grepl("Gas", profile.included, fixed=T)]
profile.Coal = profile.included[
  grepl("Coal", profile.included, fixed=T) |
    grepl("Lignite", profile.included, fixed=T) |
    grepl("Bituminous", profile.included, fixed=T) |
    grepl("Coke Calciner", profile.included, fixed=T) |
    grepl("Peat", profile.included, fixed=T)]
profile.Oil = profile.included[
  grepl("Oil", profile.included, fixed=T) |
    grepl("Kerosene", profile.included, fixed=T)]

# extract data sets of different fuels
csn_speciate_Diesel = subset(pm_csn_speciate, 
                             PROFILE_NAME %in% profile.Diesel)
csn_speciate_Gasoline = subset(pm_csn_speciate, 
                               PROFILE_NAME %in% profile.Gasoline)
csn_speciate_Coal = subset(pm_csn_speciate, 
                           PROFILE_NAME %in% profile.Coal)
csn_speciate_Oil = subset(pm_csn_speciate, 
                               PROFILE_NAME %in% profile.Oil)

# assign fuel type
csn_speciate_Diesel$Type = "Diesel"
csn_speciate_Gasoline$Type = "Gasoline"
csn_speciate_Coal$Type = "Coal"
csn_speciate_Oil$Type = "Oil"
csn_speciate_Oil$Type[grepl("Heavy", 
                            csn_speciate_Oil$PROFILE_NAME, 
                            fixed=T)] = "Heavy Oil"

csn_speciate_Fuel = do.call("rbind", 
                            list(csn_speciate_Diesel, 
                                 csn_speciate_Coal, 
                                 csn_speciate_Oil,
                                 csn_speciate_Gasoline))

# Define the position of vlines for different sub-facet
Fuel_vlines_C <- data.frame(
  Type = c(rep("Coal", 9),
           rep("Diesel", 8),
           rep("Gasoline", 8),
           rep("Oil", 5)),
  xintercept = c("SO4Ion", "Ca", "Fe", "Si", 
                 "EC", "OC", "OC1", "OC2", "OC3", 
                 "SO4Ion", "EC", "EC1", 
                 "EC2", "OC", "OC1", "OC2", "OC3", 
                 "SO4Ion", "EC", "EC1", 
                 "EC2", "OC", "OC1", "OC2", "OC3", 
                 "SO4Ion", "EC", "OC", "OC3", "OC4"))

ggplot(csn_speciate_Fuel, 
       aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(data = Fuel_vlines_C, 
             aes(xintercept = xintercept), 
             color = "gray88", size = 6) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=1, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  ggtitle("SPECIATE_Fuel_all") +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = c(0, 25, 50, 75)) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  theme(panel.spacing.y = unit(0, "cm")) +
  theme(legend.position = c(0.5, 1.03), legend.direction = "horizontal")

# Define the position of vlines for different sub-facet
Fuel_vlines_Earth <- data.frame(
  Type =rep(c("Coal", "Gasoline", "Oil"), 6),
  xintercept = rep(c("Al", "Ca", "Fe", 
                     "Na", "S", "Si"), 3))
Fuel_vlines_other <- data.frame(
  Type = c(rep("Coal", 7),
           rep("Diesel", 3),
           rep("Gasoline", 5),
           rep("Oil", 7)),
  xintercept = c("Ba", "Cu","La", "Mg", 
                 "Ni", "V", "Zn",
                 "Ba", "W", "Zn",
                 "Ba", "Cu","Mg", 
                 "Pb", "Zn",
                 "Ba", "Cu","La", "Mg", 
                 "Ni", "V", "Zn"))

ggplot(csn_speciate_Fuel, 
       aes(CompName, WEIGHT_PERCENT, color = Class)) +
#  geom_vline(data = Fuel_vlines_C, aes(xintercept = xintercept), 
#             color = "gray88", size = 5) +
  geom_vline(data = Fuel_vlines_Earth, 
             aes(xintercept = xintercept), 
             color = "aliceblue", size = 5) +
  geom_vline(data = Fuel_vlines_other, 
             aes(xintercept = xintercept), 
             color = "oldlace", size = 5) +
  geom_vline(xintercept = "OC4", 
             color = "gray88", size = 6) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=1, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  ggtitle("SPECIATE_Fuel_zoom") +
  scale_y_continuous(limits = c(0, 5), breaks = c(0, 2, 4)) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  theme(panel.spacing.y = unit(0, "cm")) +
  theme(legend.position = c(0.5, 1.05), 
        legend.direction = "horizontal")

#### Residential/indoor Coal/Oil vs. Other situation ####
profile.included[grepl("Resident", profile.included, fixed=T)]
profile.included[grepl("Indoor", profile.included, fixed=T)]
profile.included[grepl("Home", profile.included, fixed=T)]

csn_speciate_CoalOil = do.call("rbind", 
                            list(csn_speciate_Coal, 
                                 csn_speciate_Oil))

csn_speciate_CoalOil$Res.Other = "Other."
csn_speciate_CoalOil$Res.Other[grepl("Resident", 
                                     csn_speciate_CoalOil$PROFILE_NAME, 
                                     fixed=T)] = "Res."

csn_speciate_CoalOil$Fuel = "Oil"
csn_speciate_CoalOil$Fuel[csn_speciate_CoalOil$PROFILE_NAME %in% 
                            profile.Coal] = "Coal"

csn_speciate_CoalOil$Type = paste0(csn_speciate_CoalOil$Res.Other, 
                                   csn_speciate_CoalOil$Fuel)


ggplot(csn_speciate_CoalOil, 
       aes(CompName, WEIGHT_PERCENT, color = Class)) +
#  geom_vline(data = Fuel_vlines_C, aes(xintercept = xintercept), 
#             color = "gray88", size = 6) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=1, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  ggtitle("SPECIATE_Fuel_Residential.vs.Not") +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75)) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  theme(panel.spacing.y = unit(0, "cm")) +
  theme(legend.position = c(0.6, 1.03), legend.direction = "horizontal")

#### Other burning: fly ash & cigarette smoke ####
profile.included[grepl("Fly Ash", profile.included, fixed=T)]
profile.included[grepl("Prescribed Burning", profile.included, fixed=T)]
profile.included[grepl("Cigarette", profile.included, fixed=T)]

profile.OtherBurn = profile.included[
  grepl("Fly Ash", profile.included, fixed=T) | 
    grepl("Cigarette", profile.included, fixed=T) | 
    (grepl("Prescribed Burning", profile.included, fixed=T) &
       !grepl("Forest", profile.included, fixed=T))]

csn_speciate_OtherBurn = subset(pm_csn_speciate, 
                             PROFILE_NAME %in% profile.OtherBurn)
csn_speciate_OtherBurn$Type = "Fly Ash"
csn_speciate_OtherBurn$Type[grepl("Prescribed", 
                                  csn_speciate_OtherBurn$PROFILE_NAME, 
                                  fixed=T)] = "PrescriBn"
csn_speciate_OtherBurn$Type[grepl("Cigarette", 
                                  csn_speciate_OtherBurn$PROFILE_NAME, 
                                  fixed=T)] = "Cigarette"

OtherBurn_vlines_main <- data.frame(
  Type = c(rep("Fly Ash", 8), 
           "PrescriBn"),
  xintercept = c("Al", "Ca", "Fe", 
                 "Mg", "Si", "Zn", 
                 "SO4Ion", "EC",
                 "EC"))

OtherBurn_vlines_C <- data.frame(
  Type = c("Cigarette",
           "Fly Ash", "Fly Ash", 
           "PrescriBn", "PrescriBn"),
  xintercept = c("EC",
                 "EC", "OC",
                 "EC", "OC"))

OtherBurn_whole <- ggplot(csn_speciate_OtherBurn, 
                       aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(data = OtherBurn_vlines_main, aes(xintercept = xintercept), 
             color = "gray88", size = 6) +
  geom_vline(data = OtherBurn_vlines_C, aes(xintercept = xintercept), 
             color = "oldlace", size = 6) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=1.5, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ggtitle("SPECIATE_OtherBurn") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  # scale_y_continuous(limits = c(0, 30), breaks = c(0, 10, 20)) +
  theme.species.up +
  # plot.margin is set so that upper and below plots has the same size
  theme(plot.margin=unit(c(1, 1, 0, 0.9), "cm"), 
        legend.position = c(0.5, 1.05), legend.direction = "horizontal",
        panel.spacing.y = unit(0, "cm"))
# plot.margin, top, right, bottom, left
OtherBurn_whole

OtherBurn_vlines_zoom <- data.frame(Type = c("Fly Ash", "Fly Ash",  
                                             "Fly Ash", "Fly Ash",
                                             "Fly Ash", "Fly Ash",  
                                             "Fly Ash", "Fly Ash",
                                             "PrescriBn", "PrescriBn"),
                                    xintercept = c("Ba", "Sr", "Ti", "K",
                                                   "EC1", "EC2", 
                                                   "OC", "OC2", 
                                                   "Cd", "K"))

OtherBurn_zoom <- ggplot(subset(csn_speciate_OtherBurn,
                                Type != "Cigarette"), 
                      aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(data = OtherBurn_vlines_zoom, aes(xintercept = xintercept), 
             color = "gray88", size = 5) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=1.5, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ggtitle("SPECIATE_OtherBurn") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  scale_y_continuous(limits = c(0, 4), breaks = c(0, 1, 2, 3)) +
  theme.species.bottom +
  theme(plot.margin=unit(c(0.2, 1, 1, 1.15), "cm"),
        legend.position = "none",
        panel.spacing.y = unit(0, "cm"))
OtherBurn_zoom

# Combine the two plots
OtherBurn_combine <- ggarrange(OtherBurn_whole + rremove("ylab") + rremove("xlab"), 
                            OtherBurn_zoom + rremove("ylab") + rremove("xlab"), # remove axis labels from plots
                            ncol = 1, nrow = 2, # common.legend = T, 
                            heights = c(2.5, 2),
                            # align = "hv", # graphs be horizontally "h", vertically "v", or both "hv" or none aligned
                            font.label = list(size = 10, color = "black", face = "bold", 
                                              family = "Arial Unicode MS", position = "top"))

# Set xlab & ylab
OtherBurn_combine <- annotate_figure(OtherBurn_combine, 
                                  left = textGrob("Weight Percent", rot = 90, vjust = 2, gp = gpar(cex = 1.3)),
                                  bottom = textGrob("PM"[2.5]~"Species", vjust = -1.5, gp = gpar(cex = 1.3)))
OtherBurn_combine 

#### Industry ####
# detect those with Industry
profile.till.Industry = profile.included[! (profile.included %in% profile.Wildfire |
                                              profile.included %in% profile.biomass |
                                              profile.included %in% profile.SeaSalt |
                                              profile.included %in% profile.Nitrate |
                                              profile.included %in% profile.Sulfate |
                                              profile.included %in% profile.Vehicle |
                                              profile.included %in% profile.AircraftVessel |
                                              profile.included %in% profile.TireBrake |
                                              profile.included %in% profile.Fuel |
                                              profile.included %in% profile.OtherBurn)]

profile.included[grepl("Industry", profile.included, fixed=T)]
# Multiple
profile.included[grepl("Lead", profile.included, fixed=T) & 
                   !grepl("Vehicle", profile.included, fixed=T) & 
                   !grepl("Gasoline", profile.included, fixed=T)]
profile.included[grepl("Welding Fume", profile.included, fixed=T)]
profile.included[grepl("Gas-fired", profile.included, fixed=T)]
# profile.included[grepl("Truck", profile.included, fixed=T)]
profile.included[grepl("Metal", profile.included, fixed=T)]
profile.included[grepl("Aluminum", profile.included, fixed=T)]
profile.included[grepl("Copper", profile.included, fixed=T)]
profile.included[grepl("Steel", profile.included, fixed=T)]
profile.included[grepl("Phospho", profile.included, fixed=T)]
profile.included[grepl("Kiln", profile.included, fixed=T)]
profile.included[grepl("Antimony", profile.included, fixed=T)]
profile.included[grepl("Zinc", profile.included, fixed=T)]
profile.included[grepl("Iron", profile.included, fixed=T)]
profile.included[grepl("Sodium", profile.included, fixed=T)]
# profile.included[grepl("Lime", profile.included, fixed=T)]
profile.included[grepl("Petroleum", profile.included, fixed=T)]

Industry.words = c("Manufactu", "Manuf", "Shred", "Making", "Gas-fired", 
                   "boiler", "Boiler", "heater", "Desulfurization", "Production", 
                   "Steel", "Brick Grinding and Screening", "Cupola", 
                   "Furnace", "Product", "Brick Making", "Charcoal Making", 
                   "Mills", "Catalytic Crack", "Coating", "Aluminum", 
                   "Copper", "Sandblast", "Fabrication", "Mining", "Welding",
                   "Phospho", "Kiln", "Antimony", "Zinc", "Iron", "Sodium",
                   "Dryer", "Heat", "Industry", "Gypsum", "Sulfite", "Cooler",
                   "Petroleum", "hosphate", "Hogged Fuel Boiler", "Tar Pot",
                   "Recovery Boiler", "Handling")

profile.Industry.1 = profile.included[grepl("Lead", profile.included, fixed=T) & 
                                        !grepl("Vehicle", profile.included, fixed=T) & 
                                        !grepl("Gasoline", profile.included, fixed=T)]

# all those included in Industry.words
profile.Industry.2 = grep(paste0(Industry.words, collapse = "|"), 
                          profile.included, value = T)

# Get the combined profile.Industry
profile.Industry = append(profile.Industry.1, profile.Industry.2)

# Get the dataset including all sources belong to Industry
csn_speciate_Industry = subset(pm_csn_speciate, 
                               PROFILE_NAME %in% profile.Industry)
unique(csn_speciate_Industry$CompName)
dim(csn_speciate_Industry)

ggplot(csn_speciate_Industry, aes(CompName, WEIGHT_PERCENT, color = Class)) +
    geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color="black", size=0.5, alpha=0.3) +
  scale_color_nejm() + 
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  ggtitle("SPECIATE_Industry") +
  ylim(0, 100) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  theme(legend.position = c(0.5, 1.05), legend.direction = "horizontal")

ggplot(csn_speciate_Industry, aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color="black", size=0.5, alpha=0.3) +
  scale_color_nejm() + 
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  ggtitle("SPECIATE_Industry") +
  ylim(0, 5) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  theme(legend.position = c(0.5, 1.05), legend.direction = "horizontal")


Industry_whole <- ggplot(csn_speciate_Industry, 
                        aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(xintercept = c("Cs", "Na", "Pb", "Zn", "OC"), 
             color = "aliceblue", size = 6) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color = "black", size=1, alpha=0.3) +
  scale_color_nejm() + 
  ggtitle("SPECIATE_Industry") +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species.up +
  # plot.margin is set so that upper and below plots has the same size
  theme(plot.margin=unit(c(1, 1, 0, 1.4), "cm"), 
        legend.position = c(0.5, 1.07), legend.direction = "horizontal")
Industry_whole
# plot.margin, top, right, bottom, left

Industry_zoom <- ggplot(csn_speciate_Industry, 
                       aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(xintercept = c("Al", "Ca", "Fe", "K", "Na", "S", 
                            "Si", "ClIon", "EC", "OC"), 
             color = "aliceblue", size = 6) +
  geom_vline(xintercept = c("Ss", "Cd", "Ce", "Cu", "Sb", "Pb", "Zn", "Sn"), 
             color = "oldlace", size = 6, alpla = 0.1) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color = "black", size=1, alpha=0.3) +
  scale_color_nejm() + 
  ylim(0, 10) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species.bottom +
  theme(plot.margin=unit(c(0.2,1,1,1), "cm"),
        legend.position = "none")
Industry_zoom

# combine two plot
# grid.arrange(Industry_whole, Industry_zoom, ncol = 1, heights = c(1, 1), left = "Weight Percent")

# Combine the two plots
Industry_combine <- ggarrange(Industry_whole + rremove("ylab") + rremove("xlab"), 
                             Industry_zoom + rremove("ylab") + rremove("xlab"), # remove axis labels from plots
                             ncol = 1, nrow = 2, # common.legend = T, 
                             # align = "hv", # graphs be horizontally "h", vertically "v", or both "hv" or none aligned
                             font.label = list(size = 10, color = "black", face = "bold", 
                                               family = "Arial Unicode MS", position = "top"))

# Set xlab & ylab
Industry_combine <- annotate_figure(Industry_combine, 
                                   left = textGrob("Weight Percent", rot = 90, vjust = 2, gp = gpar(cex = 1.3)),
                                   bottom = textGrob("PM"[2.5]~"Species", vjust = -1.5, gp = gpar(cex = 1.3)))
Industry_combine

# Save the modified plot
png("SPECIATE_Industry_combine.png",
    width = 32, height = 18, 
    units = "cm", res = 300,
    bg = "transparent") # somehow the output plot has no transparent background
print(Industry_combine)
dev.off()

#### Industry - metals ####
# extract those directly related to metals
csn_speciate_IndustryMetal = subset(csn_speciate_Industry,
                                     grepl("Lead", profile.included, fixed=T) |
                                       grepl("Aluminum", profile.included, fixed=T) |
                                       grepl("Copper", profile.included, fixed=T) |
                                      grepl("Steel", profile.included, fixed=T))
dim(csn_speciate_IndustryMetal)

subset(csn_speciate_Industry,
       grepl("Lead", profile.included, fixed=T))

# Assign metal type
csn_speciate_IndustryMetal$Metals = "Lead"
csn_speciate_IndustryMetal$Metals[grepl("Aluminum", 
                                        csn_speciate_IndustryMetal$PROFILE_NAME, 
                                        fixed=T)] = "Aluminum"
csn_speciate_IndustryMetal$Metals[grepl("Copper", 
                                        csn_speciate_IndustryMetal$PROFILE_NAME, 
                                        fixed=T)] = "Copper"
csn_speciate_IndustryMetal$Metals[grepl("Steel", 
                                        csn_speciate_IndustryMetal$PROFILE_NAME, 
                                        fixed=T)] = "Steel"

# plotting
ggplot(csn_speciate_IndustryMetal, 
       aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(xintercept = c("Cs", "Na", "Pb", "Zn", "OC"), 
             color = "aliceblue", size = 6) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color = "black", size=1.5, alpha=0.3) +
  facet_grid(Metals ~., scales = "free")+
  scale_color_nejm() + 
  ggtitle("SPECIATE_Industry_Metals") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  # plot.margin is set so that upper and below plots has the same size
  theme(legend.position = c(0.5, 1.05), legend.direction = "horizontal",
        panel.spacing.y = unit(0, "cm"))
# plot.margin, top, right, bottom, left


#### Industry - processing type ####
profile.included[grepl("Gas-fired", profile.included, fixed=T)]
profile.included[grepl("Manufacturing", profile.included, fixed=T)]
profile.included[grepl("Making", profile.included, fixed=T)]
profile.included[grepl("Production", profile.included, fixed=T)]
profile.included[grepl("Shredding", profile.included, fixed=T)]
profile.included[grepl("Grinding", profile.included, fixed=T)]
profile.included[grepl("Desulfurization", profile.included, fixed=T)]
profile.included[grepl("Catalytic Cracking", profile.included, fixed=T)]
profile.included[grepl("Coating", profile.included, fixed=T)]

# extract those directly related to industry methods
csn_speciate_shred = subset(csn_speciate_Industry,
                            grepl("Shredding", profile.included, fixed=T) |
                              grepl("Grinding", profile.included, fixed=T))
csn_speciate_desulfur = subset(csn_speciate_Industry,
                               grepl("Desulfurization", profile.included, fixed=T))
csn_speciate_catalytic = subset(csn_speciate_Industry,
                                grepl("Catalytic Cracking", profile.included, fixed=T))
csn_speciate_coat = subset(csn_speciate_Industry,
                           grepl("Coating", profile.included, fixed=T))
csn_speciate_weld = subset(csn_speciate_Industry,
                           grepl("Welding", profile.included, fixed=T))


# Assign method type
csn_speciate_shred$Type = "Shredding"
csn_speciate_desulfur$Type = "Desulfur"
csn_speciate_catalytic$Type = "CatalyCrack"
csn_speciate_coat$Type = "Coating"
csn_speciate_weld$Type = "Welding"

csn_speciate_IndustryMethod = do.call("rbind", 
                                      list(csn_speciate_shred, 
                                           csn_speciate_desulfur, 
                                           csn_speciate_catalytic,
                                           csn_speciate_coat,
                                           csn_speciate_weld))


# plotting
ggplot(csn_speciate_IndustryMethod, 
       aes(CompName, WEIGHT_PERCENT, color = Class)) +
#  geom_vline(xintercept = c("Cs", "Na", "Pb", "Zn", "OC"), 
#             color = "aliceblue", size = 7) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(color = "black", size=1.5, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ggtitle("SPECIATE_Industry_Process") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species +
  # plot.margin is set so that upper and below plots has the same size
  theme(legend.position = c(0.7, 1.03), legend.direction = "horizontal",
        panel.spacing.y = unit(0, "cm"),
        strip.text.y = element_text(size = 13))
# plot.margin, top, right, bottom, left


#### Agriculture ####
profile.included[grepl("Fertilizer", profile.included, fixed=T)]
profile.included[grepl("Agricult", profile.included, fixed=T)]
profile.included[grepl("Food & Ag", profile.included, fixed=T)]
profile.included[grepl("Feed And Grain", profile.included, fixed=T)]
profile.included[grepl("Farm", profile.included, fixed=T)]
profile.included[grepl("Food And Agriculture", profile.included, fixed=T)]

profile.Agriculture = profile.included[grepl("Fertilizer", profile.included, fixed=T) |
                                         grepl("Agriculture Soil", profile.included, fixed=T) |
                                         (grepl("Agricultural", profile.included, fixed=T) &
                                            grepl("Burning", profile.included, fixed=T)) |
                                         grepl("Food & Ag", profile.included, fixed=T) |
                                         grepl("Food And Agriculture", profile.included, fixed=T) |
                                         grepl("Feed And Grain", profile.included, fixed=T)]

# extract those directly related to Agriculture
csn_speciate_Fertilizer = subset(pm_csn_speciate,
                            grepl("Fertilizer", profile.Agriculture, fixed=T))
csn_speciate_AgricSoil = subset(pm_csn_speciate,
                               grepl("Agriculture Soil", profile.Agriculture, fixed=T))
csn_speciate_AgricBurn = subset(pm_csn_speciate,
                                grepl("Agricultural", profile.Agriculture, fixed=T) &
                                  grepl("Burning", profile.Agriculture, fixed=T))
csn_speciate_FoodGrain = subset(pm_csn_speciate,
                              grepl("Food", profile.Agriculture, fixed=T) |
                                grepl("Feed And Grain", profile.Agriculture, fixed=T))

# Assign method type
csn_speciate_Fertilizer$Type = "Fertilizer"
csn_speciate_AgricSoil$Type = "AgricultSoil"
csn_speciate_AgricBurn$Type = "AgricultBrun"
csn_speciate_FoodGrain$Type = "FoodGrain"

csn_speciate_Agriculture = do.call("rbind", 
                                   list(csn_speciate_Fertilizer, 
                                        csn_speciate_AgricSoil, 
                                        csn_speciate_AgricBurn,
                                        csn_speciate_FoodGrain))

Agriculture_vlines <- 
  data.frame(Type = c("AgricultBrun", "AgricultBrun", "AgricultBrun", 
                      "AgricultBrun", "AgricultBrun", "AgricultBrun", 
                      "AgricultBrun", 
                      "AgricultSoil", "AgricultSoil",
                      "AgricultSoil", "AgricultSoil",
                      "Fertilizer", "Fertilizer", "Fertilizer", 
                      "Fertilizer", "Fertilizer", "Fertilizer",
                      "FoodGrain", "FoodGrain", "FoodGrain",
                      "FoodGrain", "FoodGrain", "FoodGrain",
                      "FoodGrain", "FoodGrain", "FoodGrain"),
             xintercept = c("Al", "Fe","Si", "EC", 
                            "EC2", "OC", "OC2", 
                            "Fe", "OC", "SO4Ion", "EC2", 
                            "SO4Ion", "EC", "EC2",
                            "OC", "OC3","OC4",
                            "Fe","Si", "SO4Ion",
                            "EC", "EC2","OC", 
                            "OC1", "OC2", "OC3"))

# plotting
ggplot(csn_speciate_Agriculture, 
       aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(data = Agriculture_vlines, aes(xintercept = xintercept), 
             color = "gray88", size = 6) +
  geom_vline(xintercept = c("EC2", "OC2"), color = "aliceblue", size = 6) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=1.5, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ggtitle("SPECIATE_Agriculture") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  scale_y_continuous(limits = c(0, 80), breaks = c(0, 20, 40, 60)) +
  theme.species +
  # plot.margin is set so that upper and below plots has the same size
  theme(legend.position = c(0.5, 1.03), legend.direction = "horizontal",
        panel.spacing.y = unit(0, "cm"))
# plot.margin, top, right, bottom, left


#### Waste Combustion ####
profile.included[grepl("Waste", profile.included, fixed=T)]
profile.included[grepl("Garbage", profile.included, fixed=T)]
profile.included[grepl("Sludge Incineration", profile.included, fixed=T)]
profile.included[grepl("Sludge", profile.included, fixed=T)]
profile.included[grepl("Incineration", profile.included, fixed=T)]
profile.included[grepl("Municipal Incinerator", profile.included, fixed=T)]

profile.Waste = profile.included[(grepl("Waste", profile.included, fixed=T) &
                                    !grepl("Copper", profile.included, fixed=T)) |
                                   grepl("Garbage", profile.included, fixed=T) |
                                   grepl("Sludge", profile.included, fixed=T) |
                                   grepl("Incineration", profile.included, fixed=T) |
                                   grepl("Municipal Incinerator", profile.included, fixed=T)]

# extract those directly related to Waste
csn_speciate_Waste = subset(pm_csn_speciate, PROFILE_NAME %in% profile.Waste)
csn_speciate_Waste$Type = "Waste"
csn_speciate_Waste$Type[grepl("Sludge Incineration", 
                              csn_speciate_Waste$PROFILE_NAME, fixed=T)] = "SewageSludge"

Waste_vlines_IonCsub <- 
  data.frame(Type = c("Waste", "Waste", 
                      "Waste", "Waste"),
             xintercept = c("ClIon", "SO4Ion", "EC", "OC"))

Waste_whole <- ggplot(csn_speciate_Waste, 
                      aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(data = Waste_vlines_IonCsub, 
             aes(xintercept = xintercept), 
             color = "gray88", size = 7) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=1.5, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ggtitle("SPECIATE_Waste_Burning") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  scale_y_continuous(limits = c(0, 80), breaks = c(0, 20, 40, 60)) +
  theme.species.up +
  # plot.margin is set so that upper and below plots has the same size
  theme(plot.margin=unit(c(1, 1, 0, 0.9), "cm"), 
        legend.position = c(0.5, 1.07), legend.direction = "horizontal",
        panel.spacing.y = unit(0, "cm"))
# plot.margin, top, right, bottom, left
Waste_whole

Waste_vlines_others <- 
  data.frame(Type = c("Waste", "Waste", "Waste", 
                      "Waste", "Waste", "Waste", 
                      "SewageSludge", "SewageSludge", "SewageSludge", 
                      "SewageSludge", "SewageSludge", "SewageSludge", 
                      "SewageSludge", "SewageSludge"),
             xintercept = c("Fe", "Hg", "Pb", 
                            "Si", "Sn", "Zn",
                            "Fe", "Cd", "Cu", "Cr", 
                            "P", "Pb", "Sn", "Zn"))

Waste_zoom <- ggplot(csn_speciate_Waste, 
                     aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(xintercept = c("Al", "Ca", "Na", "Mg", "K", "S", "Si"),
             color = "gray88", size = 7) +
  geom_vline(data = Waste_vlines_others,
             aes(xintercept = xintercept), 
             color = "aliceblue", size = 7) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=1.5, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ggtitle("SPECIATE_Waste_Burning") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  scale_y_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8)) +
  theme.species.bottom +
  theme(plot.margin=unit(c(0.2, 1, 1, 1.15), "cm"),
        legend.position = "none",
        panel.spacing.y = unit(0, "cm"))
Waste_zoom

# Combine the two plots
Waste_combine <- ggarrange(
  Waste_whole + rremove("ylab") + rremove("xlab"), 
  Waste_zoom + rremove("ylab") + rremove("xlab"), # remove axis labels from plots
  ncol = 1, nrow = 2, # common.legend = T, 
  # align = "hv", # graphs be horizontally "h", vertically "v", or both "hv" or none aligned
  font.label = list(size = 10, color = "black", face = "bold", 
                    family = "Arial Unicode MS", position = "top"))

# Set xlab & ylab
Waste_combine <- annotate_figure(
  Waste_combine, 
  left = textGrob("Weight Percent", rot = 90, vjust = 2, gp = gpar(cex = 1.3)),
  bottom = textGrob("PM"[2.5]~"Species", vjust = -1.5, gp = gpar(cex = 1.3)))
Waste_combine

#### Cooking ####
profile.Industry.Waste = profile.till.Industry[! (
  profile.till.Industry %in% profile.Industry |
    profile.till.Industry %in% profile.Agriculture |
    profile.till.Industry %in% profile.Waste)]

# Charbroiling
profile.included[grepl("Charbroiler", profile.included, fixed=T)] #Charbroiler
profile.included[grepl("Charbroiling", profile.included, fixed=T)]
profile.included[grepl("Frying", profile.included, fixed=T)]
# profile.included[grepl("Food", profile.included, fixed=T)] #"FoodGrainriculture"
profile.included[grepl("Cook", profile.included, fixed=T)]
profile.included[grepl("Residen", profile.included, fixed=T)]

profile.Cook = profile.included[
  grepl("Charbroil", profile.included, fixed=T) |
    grepl("charbroil", profile.included, fixed=T) |
    grepl("Frying", profile.included, fixed=T) |
    (grepl("Cook", profile.included, fixed=T) &
       !grepl("Wood", profile.included, fixed=T))]

# extract data sets of different fuels
csn_speciate_Charbroil = subset(pm_csn_speciate, 
                                grepl("Charbroil", PROFILE_NAME, fixed=T) |
                                  grepl("charbroil", PROFILE_NAME, fixed=T))
csn_speciate_Fry = subset(pm_csn_speciate, 
                          grepl("Frying", PROFILE_NAME, fixed=T))
csn_speciate_OtherCook = subset(pm_csn_speciate, 
                                grepl("Cook", PROFILE_NAME, fixed=T) &
                                  !grepl("Wood", PROFILE_NAME, fixed=T))

# assign Cook type
csn_speciate_Charbroil$Type = "Charbroil"
csn_speciate_Fry$Type = "Fry"
csn_speciate_OtherCook$Type = "OtherCook"

csn_speciate_Cook = do.call("rbind", 
                            list(csn_speciate_Charbroil, 
                                 csn_speciate_OtherCook,
                                 csn_speciate_Fry))


# Define the position of vlines for different sub-facet
Cook_vlines_C <- data.frame(
  Type = c("Charbroil", "Fry", 
           rep("OtherCook", 5)),
  xintercept = c("OC", "OC", 
                 "EC", "EC1", 
                 "OC", "OC2", "OC3"))

Cook_whole <- ggplot(csn_speciate_Cook, 
                      aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(data = Cook_vlines_C, 
             aes(xintercept = xintercept), 
             color = "gray88", size = 7) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=1, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  ggtitle("SPECIATE_Cook_all") +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75)) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species.up +
  # plot.margin is set so that upper and below plots has the same size
  theme(plot.margin=unit(c(1, 1, 0, 0.9), "cm"), 
        legend.position = c(0.5, 1.05), legend.direction = "horizontal",
        panel.spacing.y = unit(0, "cm"))
Cook_whole

Cook_zoom <- ggplot(csn_speciate_OtherCook, 
                     aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=1, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  ggtitle("SPECIATE_Cook_zoom") +
  scale_y_continuous(limits = c(0, 5), breaks = c(0, 2, 4)) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme.species.bottom +
  theme(plot.margin=unit(c(0.2, 1, 1, 1.15), "cm"),
        legend.position = "none",
        panel.spacing.y = unit(0, "cm"))
Cook_zoom

# Combine the two plots
Cook_combine <- ggarrange(
  Cook_whole + rremove("ylab") + rremove("xlab"), 
  Cook_zoom + rremove("ylab") + rremove("xlab"), # remove axis labels from plots
  ncol = 1, nrow = 2, # common.legend = T, 
  heights = c(2, 1), 
  # align = "hv", # graphs be horizontally "h", vertically "v", or both "hv" or none aligned
  font.label = list(size = 10, color = "black", face = "bold", 
                    family = "Arial Unicode MS", position = "top"))

# Set xlab & ylab
Cook_combine <- annotate_figure(
  Cook_combine, 
  left = textGrob("Weight Percent", rot = 90, vjust = 2, gp = gpar(cex = 1.3)),
  bottom = textGrob("PM"[2.5]~"Species", vjust = -1.5, gp = gpar(cex = 1.3)))
Cook_combine


#### Dust ####
profile.Industry.Waste[! (profile.Industry.Waste %in% profile.Cook)]

profile.included[grepl("Soil", profile.included, fixed=T)]
profile.included[grepl("Dust", profile.included, fixed=T)]
profile.included[grepl("Road Dust", profile.included, fixed=T)]

profile.Dust = profile.Industry.Waste[grepl("Dust", profile.Industry.Waste, fixed=T)]                                
profile.Construction = profile.Industry.Waste[
  grepl("Construction", profile.Industry.Waste, fixed=T) |
    grepl("Excavation", profile.Industry.Waste, fixed=T) |
    grepl("Sanding", profile.Industry.Waste, fixed=T) |
    grepl("Gravel", profile.Industry.Waste, fixed=T) |
    grepl("Sawdust", profile.Industry.Waste, fixed=T) |
    grepl("Cement", profile.Industry.Waste, fixed=T) |
    grepl("Marble", profile.Industry.Waste, fixed=T) |
    grepl("Cement", profile.Industry.Waste, fixed=T) |
    grepl("Sawdust", profile.Industry.Waste, fixed=T) |
    grepl("Asphalt Roofing", profile.Industry.Waste, fixed=T)]

profile.DustCons = append(profile.Dust, profile.Construction)

# extract data sets of different Dust
csn_speciate_DustCons = subset(pm_csn_speciate, 
                               PROFILE_NAME %in% profile.DustCons)

# classify into more subgroups
csn_speciate_DustCons$Type = "Industry" 
# c("Sodium", "Ion", "Gypsum Pile", "Industrial Dust", "Limestone") 
csn_speciate_DustCons$Type[grepl("Road Dust", 
                             csn_speciate_DustCons$PROFILE_NAME, 
                             fixed=T)] = "OtherRoad"
csn_speciate_DustCons$Type[grepl("Unpaved Road", 
                             csn_speciate_DustCons$PROFILE_NAME, 
                             fixed=T)] = "UnpavedRd"
csn_speciate_DustCons$Type[grepl("Paved Road", 
                             csn_speciate_DustCons$PROFILE_NAME, 
                             fixed=T)] = "PavedRd"
csn_speciate_DustCons$Type[grepl("Soil Dust", 
                             csn_speciate_DustCons$PROFILE_NAME, 
                             fixed=T)] = "SoilDust"
csn_speciate_DustCons$Type[csn_speciate_DustCons$PROFILE_NAME %in% 
                             profile.Construction] = "Construction"

DustCons_vlines_C <- data.frame(Type = c("Construction", "Construction",
                                         "PavedRd", "PavedRd", 
                                         "UnpavedRd", "UnpavedRd"),
                                xintercept = rep(c("OC4", "OC3"), 3))

# plotting
ggplot(csn_speciate_DustCons, 
       aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(data = DustCons_vlines_C, 
             aes(xintercept = xintercept), 
             color = "oldlace", size = 7) +
  geom_vline(xintercept = c("Al", "Ca", "Fe", "K", "Si", "OC"), 
             color = "gray88", size = 6) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=1.5, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ggtitle("SPECIATE_Dust") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  scale_y_continuous(limits = c(0, 20), breaks = c(0, 5, 10, 15)) +
  theme.species +
  # plot.margin is set so that upper and below plots has the same size
  theme(legend.position = c(0.5, 1.03), legend.direction = "horizontal",
        panel.spacing.y = unit(0, "cm"),
        strip.text.y = element_text(size = 12))
# plot.margin, top, right, bottom, left

#### Soil ####
profile.Industry.Waste[! (profile.Industry.Waste %in% profile.Cook |
                            profile.Industry.Waste %in% profile.Dust)]

profile.included[grepl("Soil", profile.included, fixed=T)]
profile.included[grepl("Crust", profile.included, fixed=T)]

profile.Soil = profile.Industry.Waste[
  !grepl("Dust", profile.Industry.Waste, fixed=T) &
    (grepl("Soil", profile.Industry.Waste, fixed=T) |
       grepl("Crust", profile.Industry.Waste, fixed=T))]                                

# extract data sets of different Dust
csn_speciate_Soil = subset(pm_csn_speciate, 
                           PROFILE_NAME %in% profile.Soil)

# classify into more subgroups
csn_speciate_Soil$Type = "Industry"
csn_speciate_Soil$Type[csn_speciate_Soil$PROFILE_NAME %in% 
                         c("Agriculture", "Dairy", "Mill", "Farm")] = "Farm"
csn_speciate_Soil$Type[grepl("Desert", 
                             csn_speciate_Soil$PROFILE_NAME, 
                             fixed=T)] = "Desert"
csn_speciate_Soil$Type[grepl("Local", 
                             csn_speciate_Soil$PROFILE_NAME, 
                             fixed=T)] = "Local"
csn_speciate_Soil$Type[grepl("Volcanic", 
                             csn_speciate_Soil$PROFILE_NAME, 
                             fixed=T)] = "Volcanic"
csn_speciate_Soil$Type[grepl("Urban", 
                             csn_speciate_Soil$PROFILE_NAME, 
                             fixed=T)] = "Urban"

# plotting
ggplot(csn_speciate_Soil, 
       aes(CompName, WEIGHT_PERCENT, color = Class)) +
  #  geom_vline(data = Agriculture_vlines, aes(xintercept = xintercept), 
  #             color = "gray88", size = 6) +
  geom_vline(xintercept = c("Al", "Ca", "Fe", "K", "Si", "OC"), 
             color = "gray88", size = 6) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=1.5, alpha=0.3) +
  facet_grid(Type ~., scales = "free")+
  scale_color_nejm() + 
  ggtitle("SPECIATE_Soil") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 10, 20)) +
  theme.species +
  # plot.margin is set so that upper and below plots has the same size
  theme(legend.position = c(0.5, 1.03), legend.direction = "horizontal",
        panel.spacing.y = unit(0, "cm"),
        strip.text.y = element_text(size = 12))
# plot.margin, top, right, bottom, left

#### Other ####
profile.Industry.Waste[! (profile.Industry.Waste %in% profile.Cook |
                            profile.Industry.Waste %in% profile.DustCons |
                            profile.Industry.Waste %in% profile.Soil)]

csn_speciate_NotClass = subset(
  pm_csn_speciate, 
  PROFILE_NAME %in% 
    profile.Industry.Waste[! (
      profile.Industry.Waste %in% profile.Cook |
        profile.Industry.Waste %in% profile.DustCons |
        profile.Industry.Waste %in% profile.Soil)])

#### Data summary ####
# add fake variable to match with other dataset
csn_speciate_Biomass$Type = csn_speciate_SeaSalt$Type = 
  csn_speciate_Industry$Type = csn_speciate_NotClass$Type = "None"

# ADD Column to distinguish the sources
csn_speciate_Biomass$Source = "Biomass"
csn_speciate_SeaSalt$Source = "SeaSalt"
csn_speciate_NitrateSulfate$Source = "NitrateSulfate"
csn_speciate_Vehicle$Source = "Vehicle"
csn_speciate_AircraftVessel$Source = "AircraftVessel"
csn_speciate_TireBrake$Source = "TireBrake"
csn_speciate_Fuel$Source = "Fuel"
csn_speciate_OtherBurn$Source = "OtherBurn"
csn_speciate_Industry$Source = "Industry"
csn_speciate_Agriculture$Source = "Agriculture"
csn_speciate_Waste$Source = "Waste"
csn_speciate_Cook$Source = "Cook"
csn_speciate_Soil$Source = "Soil"
csn_speciate_DustCons$Source = "DustCons"
csn_speciate_NotClass$Source = "NotClass"

csn_speciate_list <- list(csn_speciate_Biomass, 
                          csn_speciate_SeaSalt, 
                          csn_speciate_NitrateSulfate,
                          csn_speciate_Vehicle,
                          csn_speciate_AircraftVessel, 
                          csn_speciate_TireBrake, 
                          csn_speciate_Fuel,
                          csn_speciate_OtherBurn,
                          csn_speciate_Industry, 
                          csn_speciate_Agriculture, 
                          csn_speciate_Waste,
                          csn_speciate_Cook,
                          csn_speciate_Soil, 
                          csn_speciate_DustCons,
                          csn_speciate_NotClass)

# check if sharing same row number
length(unique(lapply(csn_speciate_list, 
                     function(x) sort(toupper(dim(x)[1]))))) == 1
# check if sharing same column number
length(unique(lapply(csn_speciate_list, 
                     function(x) sort(toupper(dim(x)[2]))))) == 1
# check if sharing same column names
length(unique(lapply(csn_speciate_list, 
                     function(x) sort(toupper(names(x)))))) == 1

# combine all source datasets
speciate_source = do.call("rbind", 
                       csn_speciate_list)

# check component info
speciate_comp = unique(speciate_source$CompName)
length(speciate_comp)

# calculate quantiles & other summary statistics for each variable
sapply(speciate_source, class)

# create a complete dataset of Source-CompName combinations
speciate_source_complete = expand.grid(
  Source = unique(speciate_source$Source), 
  CompName = unique(speciate_source$CompName))
speciate_source_complete = join(speciate_source_complete, 
                                speciate_source)

# get the statistical summary
speciate_source_comp_sum = 
  speciate_source_complete %>%
  dplyr::group_by(Source, CompName) %>%
  dplyr::summarise(Weight_05 = quantile(WEIGHT_PERCENT, 0.05, na.rm = T),
                   Weight_10 = quantile(WEIGHT_PERCENT, 0.10, na.rm = T),
                   Weight_25 = quantile(WEIGHT_PERCENT, 0.25, na.rm = T),
                   Weight_50 = quantile(WEIGHT_PERCENT, 0.50, na.rm = T),
                   Weight_75 = quantile(WEIGHT_PERCENT, 0.75, na.rm = T),
                   Weight_90 = quantile(WEIGHT_PERCENT, 0.90, na.rm = T),
                   Weight_95 = quantile(WEIGHT_PERCENT, 0.95, na.rm = T),
                   Weight_mean = mean(WEIGHT_PERCENT, na.rm = T),
                   Weight_sd = sd(WEIGHT_PERCENT, na.rm = T))

write.csv(speciate_source_comp_sum, "SPECIATE_Source_Profile_quantile.csv")
unique(speciate_source_comp_sum$Source)

speciate_source_comp_sum_1 = na.omit(speciate_source_comp_sum)

spc_NitrateSulfate = subset(speciate_source_comp_sum_1, Source == "NitrateSulfate")
spc_SeaSalt = subset(speciate_source_comp_sum_1, Source == "SeaSalt")
spc_Biomass = subset(speciate_source_comp_sum_1, Source == "Biomass")
spc_Vehicle = subset(speciate_source_comp_sum_1, Source == "Vehicle")
spc_TireBrake = subset(speciate_source_comp_sum_1, Source == "TireBrake")
spc_Fuel = subset(speciate_source_comp_sum_1, Source == "Fuel")
spc_Industry = subset(speciate_source_comp_sum_1, Source == "Industry")
spc_Agriculture = subset(speciate_source_comp_sum_1, Source == "Agriculture")
spc_Waste = subset(speciate_source_comp_sum_1, Source == "Waste")
spc_Cook = subset(speciate_source_comp_sum_1, Source == "Cook")
spc_Soil = subset(speciate_source_comp_sum_1, Source == "Soil")
spc_DustCons = subset(speciate_source_comp_sum_1, Source == "DustCons")
spc_OtherBurn = subset(speciate_source_comp_sum_1, Source == "OtherBurn")

speciate_source_comp_med = select(speciate_source_comp_sum_1, 
                                  Source, CompName, Weight_50)
speciate_source_comp_med$class = "Element"
speciate_source_comp_med$class[
  grepl("Ion", speciate_source_comp_med$CompName, fixed = T)] = "Ion"
speciate_source_comp_med$class[
  grepl("OC", speciate_source_comp_med$CompName, fixed = T) |
    grepl("EC", speciate_source_comp_med$CompName, fixed = T)] = "OC.EC"

###### Top_5species, Top_5_element_species ######

# Top 5 species for each source
Top_5species_source = 
  speciate_source_comp_med %>%
  group_by(Source) %>%
  arrange(desc(Weight_50)) %>%
  slice_head(n = 5)

Top_5species_source = 
  select(Top_5species_source, 
         Source, CompName)

Top_5species_source$Species.No = 
  c(rep(paste0("species", 1:5), 2),
    paste0("species", 1:4),
    rep(paste0("species", 1:5), 12))

Top_5species_source =
  spread(Top_5species_source, 
         Source, 
         CompName)

# Top 5 element species for each source
Top_5Element_source = 
  subset(speciate_source_comp_med, 
         !(class %in% c("Ion", "OC.EC"))) %>%
  group_by(Source) %>%
  arrange(desc(Weight_50)) %>%
  slice_head(n = 5)

Top_5Element_source = 
  select(Top_5Element_source, 
         Source, CompName)

Top_5Element_source$Species.No = 
  c(rep(paste0("species", 1:5), 2),
    "species1",
    rep(paste0("species", 1:5), 12))

Top_5Element_source =
  spread(Top_5Element_source, 
         Source, 
         CompName)

# Top 5 species when there is no C-subgroups for each source
Top_5noCsub_source = 
  subset(speciate_source_comp_med, 
         !(CompName %in% 
             c("OC1", "OC2", "OC3", "OC4", 
               "EC1", "EC2", "EC3"))) %>%
  group_by(Source) %>%
  arrange(desc(Weight_50)) %>%
  slice_head(n = 5)

Top_5noCsub_source = 
  select(Top_5noCsub_source, 
         Source, CompName)

Top_5noCsub_source$Species.No = 
  c(rep(paste0("species", 1:5), 2),
    paste0("species", 1:4),
    rep(paste0("species", 1:5), 12))

Top_5noCsub_source =
  spread(Top_5noCsub_source, 
         Source, 
         CompName)

write.csv(Top_5species_source, "SPECIATE_Top_5_All_species_source.csv")
write.csv(Top_5Element_source, "SPECIATE_Top_5_Element_species_source.csv")
write.csv(Top_5noCsub_source, "SPECIATE_Top_5_noCsub_species_source.csv")

#### Data summary - Plotting #### 
# below steps can ensure the order of labels in x-axis follow the preset order 
speciate_source = speciate_source[with(speciate_source, 
                                       order(Class, Sequence, CompName)), ]
speciate_source$CompName <- factor(speciate_source$CompName, 
                                   levels = unique(speciate_source$CompName))
# plotting
source.Combustion = c("Biomass", "Vehicle", 
                      "Fuel", "Waste") #"OtherBurn", 
Combust_whole <- ggplot(subset(speciate_source, 
                               Source %in% source.Combustion), 
                        aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=0.8, alpha=0.3) +
  facet_grid(Source ~., scales = "free")+
  scale_color_nejm() + 
  ggtitle("SPECIATE_Main-Combustion_all") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  scale_y_continuous(limits = c(0, 90), breaks = c(0, 30, 60)) +
  theme.species +
  # plot.margin is set so that upper and below plots has the same size
  theme(legend.position = c(0.55, 1.03), 
        legend.direction = "horizontal",
        panel.spacing.y = unit(0, "cm"),
        strip.text.y = element_text(size = 12))
# plot.margin, top, right, bottom, left
Combust_whole

comb_whole_vlines_C <- 
  data.frame(Source = c(rep("Biomass", 7), 
                        rep("Fuel", 7), 
                        rep("Vehicle", 8), 
                        rep("Waste",2)),
             xintercept = c("EC", "EC1", "EC2", "OC1", "OC2", "OC3", "OC4", 
                            "EC", "EC1", "EC2", "OC", "OC2", "OC3", "OC4",
                            "EC", "EC1", "EC2", "OC", "OC1", "OC2", "OC3", "OC4", 
                            "EC", "OC"))

comb_whole_vlines_Ion <- 
  data.frame(Source = c(rep("Biomass", 2), 
                        rep("Fuel", 2), 
                        rep("Vehicle", 2), 
                        rep("Waste",1)),
             xintercept = c("SO4Ion", "KIon", 
                            "SO4Ion", "NH4Ion", 
                            "SO4Ion", "NH4Ion",  
                            "ClIon"))

comb_whole_vlines_element <- 
  data.frame(Source = c(rep("Biomass", 1), 
                        rep("Fuel", 9), 
                        rep("Vehicle", 8), 
                        rep("Waste", 11)),
             xintercept = c("K", 
                            "Al", "Ba", "Ca", "Fe", "K", "Na", "S", "Si", "Zn",
                            "Al", "Ba", "Ca", "Fe", "Na", "S", "Si", "Zn",
                            "Al", "Ca", "Fe", "Hg", "K", "Na", "Pb", "S", "Si", "Sn", "Zn"))

comb_whole_vlines_element <- 
  data.frame(Source = c(rep("Biomass", 1), 
                      rep("Fuel", 4), 
                      rep("Vehicle", 3), 
                      rep("Waste", 6)),
             xintercept = c("K", 
                            "Ba", "Ca", "K", "Zn",
                            "Ba", "Ca", "Zn",
                            "Ca", "Hg", "K", "Pb", "Sn", "Zn"))
  
Combust_zoom <- ggplot(subset(speciate_source, 
                              Source %in% source.Combustion), 
                       aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_vline(data = comb_whole_vlines_C, 
             aes(xintercept = xintercept), 
             color = "oldlace", size = 7) +
  geom_vline(data = comb_whole_vlines_Ion, 
             aes(xintercept = xintercept), 
             color = "aliceblue", size = 7) +
  geom_vline(data = comb_whole_vlines_element, 
             aes(xintercept = xintercept), 
             color = "gray92", size = 7) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=0.5, alpha=0.3, color = "black") +
  facet_grid(Source ~., scales = "free")+
  scale_color_nejm() + 
  ggtitle("SPECIATE_Main-Combustion_zoom") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  scale_y_continuous(limits = c(0, 5), breaks = c(0, 1, 2, 3, 4)) +
  theme.species +
  # plot.margin is set so that upper and below plots has the same size
  theme(legend.position = c(0.55, 1.03), 
        legend.direction = "horizontal",
        panel.spacing.y = unit(0, "cm"),
        strip.text.y = element_text(size = 12))
Combust_zoom
  

source.otherMain = c("Industry", "Agriculture", "Soil", "Cook", 
                     "DustCons", "TireBrake")
OtherMain_whole <- ggplot(subset(speciate_source, 
                                 Source %in% source.otherMain), 
                          aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=0.8, alpha=0.3) +
  facet_grid(Source ~., scales = "free")+
  scale_color_nejm() + 
  ggtitle("SPECIATE_Main-Other_all") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  scale_y_continuous(limits = c(0, 90), breaks = c(0, 30, 60)) +
  theme.species +
  # plot.margin is set so that upper and below plots has the same size
  theme(legend.position = c(0.55, 1.03), 
        legend.direction = "horizontal",
        panel.spacing.y = unit(0, "cm"),
        strip.text.y = element_text(size = 12))
# plot.margin, top, right, bottom, left
OtherMain_whole

OtherMain_zoom <- ggplot(subset(speciate_source, 
                                Source %in% source.otherMain), 
                         aes(CompName, WEIGHT_PERCENT, color = Class)) +
  geom_boxplot(fill = NA, outlier.colour = NA) +
  geom_jitter(size=0.8, alpha=0.3) +
  facet_grid(Source ~., scales = "free")+
  scale_color_nejm() + 
  ggtitle("SPECIATE_Main-Other_zoom") +
  ylab("Weight Percent") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  scale_y_continuous(limits = c(0, 5), breaks = c(0, 1, 2, 3, 4)) +
  theme.species +
  # plot.margin is set so that upper and below plots has the same size
  theme(legend.position = c(0.55, 1.03), 
        legend.direction = "horizontal",
        panel.spacing.y = unit(0, "cm"),
        strip.text.y = element_text(size = 12))
OtherMain_zoom

#### tutorial - get multiples quantiles & other summary for multiple variables ####

# define quantile levels & df
quantiles <- c(0.05, 0.1, 0.25, 0.5, 
               0.75, 0.9, 0.95)
 
df <- data.frame(weight1 = rnorm(1000, 50, 10),
                 weight2 = rnorm(1000, 4928, 23),
                 weight3 = rnorm(1000, 7900, 789),
                 class = sample(LETTERS[1:5], 1000, replace = TRUE),
                 profile = sample(c("A", "B"), 1000, replace = TRUE))

# group by class and profile, calculate summary statistics
summary_df <- 
  df %>%
  dplyr::group_by(class, profile) %>%
  # In case there is no row for some class-profile groups, fill the gap
  # complete(class, profile, fill = list(weight = NA)) %>%
  dplyr::summarise(across(.cols = everything(), 
                          .fns = list(mean = mean, 
                                      sd = sd, 
                                      quantile = ~quantile(., quantiles))))
summary_df
