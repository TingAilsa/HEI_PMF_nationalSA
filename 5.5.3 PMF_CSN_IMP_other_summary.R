
library(dplyr)
library(plyr)
library(tidyr)
library(base)
library(data.table)
library(fst)
library(ggplot2)
library(ggthemes)

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

