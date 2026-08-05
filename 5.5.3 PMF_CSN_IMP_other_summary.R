
library(dplyr)
library(plyr)
library(tidyr)
library(base)
library(data.table)
library(fst)
library(ggplot2)
library(ggthemes)
library(ggh4x)

#### Working directory & site, factor ####
########## CSN
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15t1mdl0unc_DN/")
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15t1mdl0unc_DN/"
dataset = "CSN"; c_data = "noCsub"; data_setting = "15t1mdl0unc"; pmf_method = "DN_PMF"
getwd()

# Decided Factor.No for each site.No
site_factor = fread("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/CSN_Site_15t1mdl0unc_DN_PMF_decision_2024-06-30.csv")
site_factor = dplyr::select(site_factor, SiteCode, serial.No,	Factor.No)
head(site_factor); dim(site_factor)

## SWB extract
# Species applied
species_swb = fread(paste0(dataset, "_", c_data, "_", data_setting, "_PMF_SWB_site.csv"))
# Count number of species used for PMF
start_species <- which(colnames(species_swb) == "Al")
end_species <- which(colnames(species_swb) == "SO4Ion")
species_swb[, species_count := rowSums(!is.na(.SD)), 
            .SDcols = names(species_swb)[start_species:end_species]]

# Extract columns used to count extremes
species_swb_extreme = 
  dplyr::select(species_swb, Dataset, serial.No, site.row, extreme_rowNo_remain, 
                extreme_rowNo_replace, extreme_rowNo_remove, row_count_org, species_count)
write.csv(species_swb_extreme, paste0(dataset, "_", c_data, "_", data_setting, "_PMF_extreme_handling.csv"))
# "CSN_noCsub_15t1mdl0unc_PMF_SWB_site.csv"
head(species_swb)
species_applied = read.csv(paste0(dataset, "_", c_data, "_", data_setting, "_PMF_all_species.csv")) 
# "CSN_noCsub_15t1mdl0unc_PMF_all_species.csv"

setDT(species_applied)
# View(species_applied)
species_applied_list = names(species_applied)
print(species_applied_list)

species_swb_long = 
  dplyr::select(species_swb, serial.No:PM2.5) %>%
  pivot_longer(
    cols = Al:PM2.5,
    names_to = "Species",
    values_to = "SWB"
  ) %>%
  na.omit()
head(species_swb_long); dim(species_swb_long)

########## IMPROVE
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/IMPROVE_Site_Csub_15t1mdlVNi_DN/")
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/IMPROVE_Site_Csub_15t1mdlVNi_DN"
dataset = "IMPROVE"; c_data = "Csub"; data_setting = "15t1mdlVNi"; pmf_method = "DN_PMF"
getwd()

# Decided Factor.No for each site.No
site_factor = fread("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/IMPROVE_Site_15t1mdlVNi_DN_PMF_2024-07-23.csv")
site_factor = dplyr::select(site_factor, SiteCode, serial.No,	Factor.No)
head(site_factor); dim(site_factor)

## SWB extract
# Species applied
species_swb = fread(paste0(dataset, "_", c_data, "_", data_setting, "_PMF_SWB_site.csv"))

# Count number of species used for PMF
start_species <- which(colnames(species_swb) == "Al")
end_species <- which(colnames(species_swb) == "NO3Ion")
species_swb[, species_count := rowSums(!is.na(.SD)), 
            .SDcols = names(species_swb)[start_species:end_species]]

# Extract columns used to count extremes
species_swb_extreme = 
  dplyr::select(species_swb, Dataset, serial.No, site.row, extreme_rowNo_remain, 
                extreme_rowNo_replace, extreme_rowNo_remove, row_count_org, species_count)
head(species_swb_extreme)
write.csv(species_swb_extreme, paste0(dataset, "_", c_data, "_", data_setting, "_PMF_extreme_handling.csv"))

# "CSN_noCsub_15t1mdl0unc_PMF_SWB_site.csv"
head(species_swb)
species_applied = read.csv(paste0(dataset, "_", c_data, "_", data_setting, "_PMF_all_species.csv")) 
# "CSN_noCsub_15t1mdl0unc_PMF_all_species.csv"

setDT(species_applied)
# View(species_applied)
species_applied_list = names(species_applied)
print(species_applied_list)

species_swb_long = 
  dplyr::select(species_swb, serial.No:PM2.5) %>%
  pivot_longer(
    cols = Al:PM2.5,
    names_to = "Species",
    values_to = "SWB"
  ) %>%
  na.omit()
head(species_swb_long); dim(species_swb_long)
summary(species_swb_long)


#### Species residual ####

# Path for residual files
dir_path = paste0(getwd(), "/base_DISPres1/")
print(dir_path)

# File list
residual_list <- list.files(dir_path, pattern = ".*residual\\.csv$", full.names = TRUE)

# Combine files and add serial.No, Factor.No
species_residual <- 
  process_and_merge(residual_list, species_applied_list)

species_residual$Dataset = dataset
head(species_residual); dim(species_residual)
summary(species_residual)

# Extract the used Factor.No for each site
species_residual_use =
  plyr::join(species_residual, site_factor)
species_residual_use = subset(species_residual_use, !is.na(SiteCode))
summary(species_residual_use)

write_fst(species_residual_use, 
          paste0(dataset, "_Site_", c_data, "_", data_setting, "_DN_PMF_Species_Residuals.fst"))

#### Species Performace, R2, RMSE, etc. ####
species_perform_list <- list.files(dir_path, pattern = ".*PMF_vs_obs\\.csv$", full.names = TRUE)
species_perform_all = 
  do.call(
    rbind, 
    (lapply(
      species_perform_list, 
      read.csv))
  )
species_perform_all$X = NULL
species_perform_all = 
  plyr::rename(species_perform_all, 
               c("site.serial" = "serial.No",
                 "factor.No" = "Factor.No"))
head(species_perform_all); dim(species_perform_all)

# Only keep the site.No-Factor.No combinations used 
species_perform_use = 
  plyr::join(species_perform_all, site_factor)
species_perform_use = subset(species_perform_use, !is.na(SiteCode))
summary(species_perform_use)
length(unique(site_factor$serial.No)); length(unique(species_perform_use$serial.No))

# Merge with SWB info
length(unique(species_swb_long$serial.No))
species_perform_swb = 
  plyr::join(species_perform_use, species_swb_long) %>%
  na.omit()
summary(species_perform_swb)
length(unique(species_perform_swb$serial.No))
species_perform_swb$Dataset = dataset
head(species_perform_swb)

write_fst(species_perform_swb, 
          paste0(dataset, "_Site_", c_data, "_", data_setting, "_DN_PMF_Species_performance.fst"))

###### Plotting for both CSN & IMPROVE ######

species_perform_swb_csn = 
  read_fst("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15t1mdl0unc_DN/CSN_Site_noCsub_15t1mdl0unc_DN_PMF_Species_performance.fst")
species_perform_swb_imp = 
  read_fst("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/IMPROVE_Site_Csub_15t1mdlVNi_DN/IMPROVE_Site_Csub_15t1mdlVNi_DN_PMF_Species_performance.fst")
species_perform_swb_both = rbind(species_perform_swb_csn, species_perform_swb_imp)
head(species_perform_swb_both)
summary(species_perform_swb_both)
summary(subset(species_perform_swb_both, SWB == 1))

species_perform_swb_long =
  dplyr::select(species_perform_swb_both, 
                Dataset, serial.No, Factor.No, Species, RMSE, 
                cor_pearson, cor_spearman, Q_Qexp_ratio, 
                mean_obs, mean_pmf, SWB) %>%
  pivot_longer(
    cols = RMSE:mean_pmf,
    names_to = "Metrics",
    values_to = "Values"
  )
head(species_perform_swb_long)  


species_perform_swb_both_plot = species_perform_swb_both
species_perform_swb_both_plot$mean_obs[species_perform_swb_both_plot$mean_obs > 2] = NA
species_perform_swb_both_plot$mean_pmf[species_perform_swb_both_plot$mean_pmf > 2] = NA
species_perform_swb_both_plot$Q_Qexp_ratio[species_perform_swb_both_plot$Q_Qexp_ratio > 20] = NA
species_perform_swb_both_plot$RMSE[species_perform_swb_both_plot$RMSE > 2] = NA
species_perform_swb_both_plot$SWB[species_perform_swb_both_plot$SWB == 0] = "Weak"
species_perform_swb_both_plot$SWB[species_perform_swb_both_plot$SWB == 1] = "Strong"

species_perform_swb_long_plot =
  dplyr::select(species_perform_swb_both_plot, 
                Dataset, serial.No, Factor.No, Species, RMSE, 
                cor_pearson, cor_spearman, Q_Qexp_ratio, 
                mean_obs, mean_pmf, SWB) %>%
  pivot_longer(
    cols = RMSE:mean_pmf,
    names_to = "Metrics",
    values_to = "Values"
  )
head(species_perform_swb_long_plot)  


ggplot(species_perform_swb_long_plot,
       aes(x = SWB, y = Values, color = Dataset)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, NA), breaks = function(x) pretty(x, n = 3)) +
  scale_color_manual(values = c("royalblue3", "#ff7f0e")) + 
  facet_wrap(Metrics ~., scales = "free", ncol = 2) +
  xlab("Strong Weak Classification") +
  theme_base(base_size = 28) + 
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major = element_blank(),
    panel.spacing = unit(10, "mm"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, vjust = 0.5, color = "grey25"),
    axis.title.x = element_text(hjust = 0.5, vjust = -2, color = "grey25"))


#### Handling extreme values in Species ####

extreme_csn = 
  fread("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15t1mdl0unc_DN/CSN_noCsub_15t1mdl0unc_PMF_extreme_handling.csv")
extreme_imp = 
  fread("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/IMPROVE_Site_Csub_15t1mdlVNi_DN/IMPROVE_Csub_15t1mdlVNi_PMF_extreme_handling.csv")
extreme_handle = rbind(extreme_csn, extreme_imp)
extreme_handle$V1 = NULL
extreme_handle$total_species_points = extreme_handle$row_count_org * extreme_handle$species_count
head(extreme_handle); dim(extreme_handle)

colSums(dplyr::select(extreme_handle, site.row:total_species_points))
summary(dplyr::select(extreme_handle, site.row:total_species_points))


#### Element contribution in each source factor ####
###### Data Preparation ######
setwd("/Users/ztttttt/Dropbox/GMU_computer/HEI HAQ PMF/PMF_Results")
dir <- "/Users/ztttttt/Dropbox/GMU_computer/HEI HAQ PMF/PMF_Results"

# source profile including element contribution from all factor analyses
source_profile_csn = fread("CSN_Site_15t1mdl0unc_DN_source_profile_all.csv")
source_profile_csn = 
  dplyr::select(source_profile_csn, 
                Dataset, site.serial, Factor.No, Species, Concentration, Percent,
                disp_conc_down, disp_conc_mean, disp_conc_up, 
                sequence, class, Factor_source)

names(source_profile_csn)[2]
names(source_profile_csn)[2] = "serial.No"
names(source_profile_csn)
head(source_profile_csn); dim(source_profile_csn)

source_profile_imp = fread("IMPROVE_Site_15t1mdlVNi_DN_source_profile_2024-07-12.csv")
source_profile_imp = 
  dplyr::select(source_profile_imp, 
                Dataset, site.serial, Factor.No, Species, Concentration, Percent,
                disp_conc_down, disp_conc_mean, disp_conc_up, 
                sequence, class, Factor_source)

names(source_profile_imp)[2]
names(source_profile_imp)[2] = "serial.No"
names(source_profile_imp)
head(source_profile_imp); dim(source_profile_imp)

# extract the data from used factor for each site
# source_decision = fread("CSN_Site_15t1mdl0unc_DN_unique_source_match.csv") 
# source_decision = fread("IMPROVE_Site_15t1mdlVNi_DN_source_profile.csv") 

source_decision = 
  dplyr::select(source_decision,
                SiteCode, serial.No, Factor.No, State, Latitude, Longitude, 
                geoid, Main_Species, Factor_source, Source_aftermanual)
source_decision = source_decision[!duplicated(source_decision), ]
head(source_decision)

# Extract element contribution for each site
# source_profile_all=source_profile_imp; source_profile_all=source_profile_csn
dim(source_decision); dim(source_profile_all)
head(source_decision); head(source_profile_all)
sapply(source_profile_all, class)
sapply(source_decision, class)
element_contri =
  merge(source_decision, source_profile_all,
        by = c("serial.No", "Factor.No", "Factor_source"), all.x = TRUE)

dim(element_contri); dim(source_decision); dim(source_profile_all)

element_contri =
  subset(element_contri, Species != "PM2.5") %>%
  dplyr::select(-disp_conc_down, -disp_conc_mean, -disp_conc_up)

# Estimate source concentration
element_contri[, Souce_conc := sum(Concentration), 
           by = .(serial.No, Factor.No, Factor_source)]
# Fraction of each species in source factor
element_contri$species_frac_source = 
  round(element_contri$Concentration / element_contri$Souce_conc * 100, 2)
# View(element_contri)
# write.csv(element_contri, "CSN_Site_15t1mdl0unc_DN_source_species_fraction_2026.csv")
# write.csv(element_contri, "IMPROVE_Site_15t1mdlVNi_DN_source_species_fraction_2026.csv")

head(element_contri)

## Mean species profile for all sites
element_contri_csn = fread("CSN_Site_15t1mdl0unc_DN_source_species_fraction_2026.csv"); element_contri_csn$V1 = NULL
element_contri_imp = fread("IMPROVE_Site_15t1mdlVNi_DN_source_species_fraction_2026.csv"); element_contri_imp$V1 = NULL

dim(element_contri_csn); dim(element_contri_imp)
head(element_contri_csn); head(element_contri_imp)

# Combine them and create a file with speiece from csn, improve, and both
element_contri_both = rbind(element_contri_csn, element_contri_imp)
element_contri_both$Dataset = "Both"

element_contri = rbind(element_contri_both, element_contri_csn)
element_contri = rbind(element_contri, element_contri_imp)

# Show all 
sort(unique(element_contri$Species))

# Group those with the key element to element species, for example, K-ion to K
element_contri$Species[element_contri$Species == "KIon"] = "K"
element_contri$Species[element_contri$Species == "NaIon"] = "Na"
sort(unique(element_contri$Species))

# Update source names
sort(unique(element_contri$Source_aftermanual))

element_contri_1 <- 
  element_contri %>%
  dplyr::mutate(
    Source_aftermanual = case_when(
      Source_aftermanual == "F1-Traffic" ~ "Traffic\n Exhaust",
      Source_aftermanual == "F2-Secondary Nitrate" ~ "Secondary\n Nitrate",
      Source_aftermanual == "F3-Secondary Sulfate" ~ "Sulfate",
      Source_aftermanual == "F4-Non-tailpipe" ~ "Non-\n tailpipe",
      Source_aftermanual == "F5-Industry" ~ "Industry",
      Source_aftermanual == "F6-Fresh Sea Salt" ~ "Salt",
      Source_aftermanual == "F8-Biomass Burning" ~ "Biomass\n Burning/\n SOA",
      Source_aftermanual == "F8-Biomass" ~ "Biomass\n Burning/\n SOA",
      Source_aftermanual == "F9-Soil/Dust" ~ "Dust",
      TRUE ~ Source_aftermanual  # Keep original value if no match
    ))

element_contri_1 = 
  subset(element_contri_1, Source_aftermanual != "OP-rich")
sort(unique(element_contri_1$Source_aftermanual))

# Define the sequence of sources for later plotting
source_sequence <- 
  c("Traffic\n Exhaust", "Secondary\n Nitrate", "Sulfate", "Non-\n tailpipe",
    "Industry", "Salt", "Biomass\n Burning/\n SOA", "Dust", "OP-rich")
element_contri_1 <-
  element_contri_1 %>%
  mutate(Source_aftermanual = factor(Source_aftermanual, levels = source_sequence))

###### Plot1. conc contribution ######

ggplot(data = subset(element_contri_1, Dataset == "Both"),
       aes(x = reorder(Species, sequence))) +
  # Bar plot for Concentration
  geom_boxplot(aes(y = Concentration),
               alpha = 0.6, outlier.shape = NA,
               fill = "skyblue2", color = "grey30",
               linewidth = 0.4) +
  facet_grid(Source_aftermanual ~ ., scale = "free_y") +
  scale_y_log10(
    name = format_variable("Concentration µg/m3"),
    breaks = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01, 1e+00, 1e+01),
    labels = c(expression(10^"-5"), expression(""),
               expression(10^"-3"), expression(""),
               expression(10^"-1"), expression(""),
               expression(10^"1")),
    sec.axis = sec_axis(
      transform = ~.,
      breaks = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01, 1e+00),
      name = "Explained Variance %",
      labels = c(0, "", 40, "", 80, "")
    )
  ) +
  xlab(format_variable("PM25 Species")) +
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme_bw() +
  theme_text_speciesName +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linetype = "dashed"),
    
    # Strip text visible, centered, clean background
    strip.background = element_rect(fill = "grey95", color = "grey70"),
    strip.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5,
                                size = 9, face = "bold"),
    
    # Axis
    axis.text.x  = element_text(angle = 90, hjust = 1),
    axis.title   = element_text(size = 10),
    
    # Legend
    legend.position   = "top",
    legend.title      = element_blank(),
    legend.text       = element_text(size = 9),
    legend.key.size   = unit(0.4, "cm"),
    
    plot.margin = margin(8, 8, 8, 8)
  )
 
###### Plot2. conc and species explained variation ######

# Estimate the mean and median contributions
element_contri_species =
  element_contri_1 %>%
  dplyr::group_by(Dataset, Source_aftermanual, Species) %>%
  dplyr::summarise(
    conc_median = median(Concentration),
    conc_mean = mean(Concentration),
    conc_sd = sd(Concentration),
    frac_mean = mean(Percent),
    frac_sd = sd(Percent),
    souce_conc_mean = mean(Souce_conc),
    souce_conc_sd = sd(Souce_conc),
    species_frac_source_mean = mean(species_frac_source),
    species_frac_source_sd = sd(species_frac_source),
    species_frac_source_median = median(species_frac_source),
    species_frac_source_995 = quantile(species_frac_source, 0.995),
    species_frac_source_005 = quantile(species_frac_source, 0.005),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(., 3)))
# View(element_contri_species)

# Convert 0 to 1e-10 for columns to be used for y-axis, there is log transfer later
element_contri_species <- 
  element_contri_species %>%
  mutate(across(conc_median:species_frac_source_005, # Percent.up
                ~replace(., . == 0, 1e-5)))

##### Convert percent values to make the scale pattern similar to log concentration
# set the Percent == 0 to a low value before log
element_contri_species$species_frac_source_median[element_contri_species$species_frac_source_median == 0] = 1e-05

## get only dataset including both networks
species_contri_both <- 
  element_contri_species %>%
  filter(Dataset == "Both")

# Define the scaling factor for dual axis mapping
# Map species_frac_source_median (0-1 or 0-100%) onto the log concentration axis
# We need to find the range of conc_median to set up the mapping
conc_range <- range(species_contri_both$conc_median, na.rm = TRUE)
frac_range <- range(c(species_contri_both$species_frac_source_005, 
                      species_contri_both$species_frac_source_995), na.rm = TRUE)

# Log-scale mapping: map fraction [0,1] -> log space of concentration
# Choose anchor points manually based on your data range
log_min <- log10(0.00001)
log_max <- log10(max(species_contri_both$conc_median, na.rm = TRUE))

frac_min <- 0
frac_max <- 100 

# Forward transform: fraction -> concentration scale
frac_to_conc <- function(f) {
  10^(log_min + (f - frac_min) / (frac_max - frac_min) * (log_max - log_min))
}

# Inverse transform: concentration -> fraction (for sec.axis)
conc_to_frac <- function(y) {
  frac_min + (log10(y) - log_min) / (log_max - log_min) * (frac_max - frac_min)
}

# Mapped values for right axis
species_contri_both <- species_contri_both %>%
  mutate(
    frac_median_mapped  = frac_to_conc(species_frac_source_median),
    frac_995_mapped     = frac_to_conc(species_frac_source_995),
    frac_005_mapped     = frac_to_conc(species_frac_source_005)
  )

# Define fraction breaks to display on right axis (adjust as needed)
frac_breaks <- c(0, 40, 80, 100)
# conc_at_frac_breaks <- frac_to_conc(frac_breaks)
# conc_at_frac_breaks

# Add sequence
species_sequence <-
  dplyr::select(element_contri_1, Species, sequence)
species_sequence = species_sequence[!duplicated(species_sequence), ]

species_contri_both = 
  join(species_contri_both, species_sequence)

# Plot
med_species_profile_p <- 
  ggplot(species_contri_both, 
         aes(x = reorder(Species, sequence))) +
  
  # Left axis: bar for conc_median
  geom_rect(aes(
    xmin = as.numeric(reorder(Species, sequence)) - 0.3,
    xmax = as.numeric(reorder(Species, sequence)) + 0.3,
    ymin = 1e-5,
    ymax = conc_median
  ), 
  fill = "lightblue2", alpha = 0.8) +
  
  # Right axis: point + errorbar for species fraction
  geom_point(aes(y = frac_median_mapped),
             color = "grey25", shape = 15, size = 1.5) +
  geom_errorbar(aes(ymin = frac_005_mapped,
                    ymax = frac_995_mapped),
                width = 0.3, color = "grey25") +
  
  # Dual y-axis
  scale_y_continuous(
    name   = "Concentration (µg/m³)",
    trans  = "log10",
    breaks = c(1e-4, 1e-2, 1e0, 1e1),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits = c(1e-5, max(species_contri_both$conc_median, na.rm = TRUE)),
    sec.axis = sec_axis(
      transform = ~ conc_to_frac(.),
      name      = "Explained Variance (%)",
      breaks    = frac_breaks,
      labels    = paste0(frac_breaks, "%")
    )
  ) +
  
  facet_grid(Source_aftermanual ~ ., scales = "free_x", space = "free_x", switch = "y") +
  
  scale_fill_brewer(palette = "Set1") +
  xlab("Species") +
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme_bw() +
  theme_text_speciesName +
  theme(
    # axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
    axis.ticks.x    = element_line(),
    panel.grid = element_line(colour = "white"),
    plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
    strip.background = element_blank(), 
    strip.text = element_blank(),
    legend.position = "none"
  )

med_species_profile_p
  
### Make an error bar  
df <- data.frame(
  x     = 1,
  y_mid = 0.5,
  y_low = 0.1,
  y_up  = 0.9
)

ggplot(df, aes(x = x, y = y_mid)) +
  geom_point(shape = 15, size = 30, color = "black") +
  geom_errorbar(aes(ymin = y_low, ymax = y_up),
                width = 0.5, color = "black", linewidth = 10) +
  xlim(0.5, 1.5) +
  theme_void()


# K in BB
bb_contri =
  subset(element_contri, Source_aftermanual == "F8-Biomass") # Species == "KIon"
# View(bb_contri)
head(bb_contri)

# Check OC/EC ratio
bb_contri_C = 
  subset(bb_contri, Species %in% c("OC", "EC")) %>%
  dplyr::select(serial.No, Factor.No, Species, Concentration) %>%
  dplyr::group_by(serial.No, Factor.No, Species) %>%
  dplyr::summarise( 
    Concentration = mean(Concentration),
    .groups = "drop") %>%
  pivot_wider(
    names_from = Species,
    values_from = Concentration
  )
bb_contri_C$OC_EC_ratio =
  round(bb_contri_C$OC/bb_contri_C$EC, 3) 
bb_contri_C = 
  bb_contri_C %>%
  subset(!is.infinite(OC_EC_ratio))
summary(bb_contri_C$OC_EC_ratio)
mean(bb_contri_C$OC_EC_ratio)
sd(bb_contri_C$OC_EC_ratio)
quantile(bb_contri_C$OC_EC_ratio, 0.1)
quantile(bb_contri_C$OC_EC_ratio, 0.9)
quantile(bb_contri_C$OC_EC_ratio, 0.025)
quantile(bb_contri_C$OC_EC_ratio, 0.975)
# 3.4 (1.5-17.0)

bb_contri_species =
  subset(element_contri_species, Source_aftermanual == "F8-Biomass")


