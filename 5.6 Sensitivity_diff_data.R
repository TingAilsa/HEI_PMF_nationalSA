library(sf)
library(scales)
library(dplyr)
library(plyr)
library(tidyr)
library(stringr)
library(gganimate)
library(ggthemes)
library(ggplot2)
library(purrr)
library(readr)
library(ggsci)
library(gridExtra)
library(insight)
library(purrr)

setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/")
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/"

#### 1. Data Preparation FINISHED ####

########   source assignment results 
data_folders = c("CSN_Cluster_25TimesMean", "CSN_Cluster_noSeason99", "CSN_Site_25TimesMean")
file_endings <- c("_overall.csv", "_annual.csv", "_month.csv") # Replace with your actual file endings
result_path <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/results_R_data"
 
########   combine outputs

for (data_folder in data_folders){
  # path and endings for combine
  folder_path <- 
    paste0("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/",
           data_folder)  
  
  # loop through each file ending and combine
  for (ending in file_endings) {
    pmf_files <- list.files(folder_path, 
                            pattern = ending, 
                            full.names = TRUE)
    combined_df <- do.call(rbind, lapply(pmf_files, read.csv))
    combined_df$X = NULL
    
    new_file_name <- paste0(data_folder, ending)
    write.csv(combined_df, 
              file.path(result_path, new_file_name), 
              row.names = FALSE)
  }
}

##### Source assignment
source_assign = read.csv("results_R_data/Final_SA_noCsub_25TimesMean_2024.01.csv")
source_assign$X = NULL
colnames(source_assign)[2:3] = c("Cluster.No", "Factor.No")

source_assign$Source_use = source_assign$final_assign
source_assign$Source_use[grepl("Vehicle + biomass", source_assign$Source_use, fixed = T)] = "Vehic_Biom"
source_assign$Source_use[grepl("Soil", source_assign$Source_use, fixed = T)] = "F9-Soil/Dust"
source_assign$Source_use[grepl("Vehicle", source_assign$Source_use, fixed = T)] = "F1-Vehicle"
source_assign$Source_use[grepl("Biomass", source_assign$Source_use, fixed = T)] = "F8-Biomass"
source_assign$Source_use[source_assign$Source_use == "Industry 2"] = "F10-Industries_2"
source_assign$Source_use[grepl("Industry", source_assign$Source_use, fixed = T)] = "F5-Industry"
source_assign$Source_use[grepl("Aged sea", source_assign$Source_use, fixed = T)] = "F4-Aged Sea Salt"
source_assign$Source_use[grepl("Fresh Sea", source_assign$Source_use, fixed = T)] = "F6-Fresh Sea Salt"
source_assign$Source_use[grepl("peline", source_assign$Source_use, fixed = T)] = "F7-Non-Tailpipe"

write.csv(source_assign, "Final_SA_noCsub_25TimesMean_2024.01.29.csv")

##### Dual source assign to the same factor
dual_file_name = "CSN_base_DISP_summary.csv"

for (data_folder in data_folders){
  pre.fix = sub("^CSN_", "", data_folder)
  
  dual_cluster = 
    read.csv(paste0(
      "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/",
      data_folder, "/",
      dual_file_name))
  
  dual_cluster$Cluster = sub("_.*", "", dual_cluster$Cluster)
  
  dual_cluster = 
    select(dual_cluster,
           Cluster, Factor, DISP_qdrop, Name.of.source.with.multiple.factors.)
  colnames(dual_cluster) = 
    c("Cluster.No", "Factor.No", "DISP_qdrop", "dual_source")
  dual_cluster$Cluster.No = as.integer(dual_cluster$Cluster.No)
  
  dual_cluster = 
    subset(dual_cluster,
           !(dual_source== "<NA>"))
  
  dual_cluster$PMFdata = pre.fix
  dual_file = paste0(data_folder, "_dual_source.csv")
  
  write.csv(dual_cluster, 
            file.path(result_path, dual_file),
            row.names = FALSE)
}

dual_files <- list.files(result_path, 
                         pattern = "_dual_source.csv", 
                         full.names = TRUE)
combined_dual <- do.call(rbind, lapply(dual_files, read.csv))
write.csv(combined_dual, 
          file.path(result_path, "CSN_all_datasets_dual_source.csv"),
          row.names = FALSE)

#### 2. Check dual-assigned sources ####

############### read datasets
combined_dual = read.csv("results_R_data/CSN_all_datasets_dual_source.csv")
combined_dual$Factor_source = combined_dual$dual_source

source_assign = read.csv("Final_SA_noCsub_25TimesMean_2024.01.29.csv")
source_assign$X = NULL
source_assign = plyr::rename(
  source_assign, 
  c("Main_Species" = "Main_Species_reference"))

source_assign_use = select(source_assign, 
                           Dataset, Cluster.No, Factor.No, 
                           Source_use, Main_Species_reference)
source_assign_use$Factor_source = source_assign_use$Source_use

annual_Cluster_25TimesMean = read.csv("results_R_data/CSN_Cluster_25TimesMean_annual.csv")
annual_Cluster_25TimesMean$PMFdata = "Cluster_25TimesMean"

annual_Cluster_noSeason99 = read.csv("results_R_data/CSN_Cluster_noSeason99_annual.csv")
annual_Cluster_noSeason99$PMFdata = "Cluster_noSeason99"

annual_Site_25TimesMean = read.csv("results_R_data/CSN_Site_25TimesMean_annual.csv")
annual_Site_25TimesMean$PMFdata = "Site_25TimesMean"
annual_Site_25TimesMean$Cluster.No <- sub("_.*", "", annual_Site_25TimesMean$cluster.site)
annual_Site_25TimesMean$Cluster.No = as.integer(annual_Site_25TimesMean$Cluster.No)
annual_Site_25TimesMean$cluster.site = NULL
# reorder columns in annual_Site_25TimesMean to match annual_Cluster_25TimesMean
annual_Site_25TimesMean <- 
  annual_Site_25TimesMean %>% 
  select(all_of(names(annual_Cluster_25TimesMean)))

############### prepare file for dual source result comparison
# somehow, sometimes there is error when running using "command+return", 
# but not when click the green "Run" button above
# https://stackoverflow.com/questions/46171362/rmarkdown-error-attempt-to-use-zero-length-variable-name
overall_Cluster_25TimesMean = 
  ddply(annual_Cluster_25TimesMean, 
        .(PMFdata, SiteCode, Cluster.No, Factor.No, 
          Factor, Main_Species, Factor_source),
        summarise,
        Normalize_contri = median(Normalize_contri))

overall_Cluster_noSeason99 = 
  ddply(annual_Cluster_noSeason99, 
        .(PMFdata, SiteCode, Cluster.No, Factor.No, 
          Factor, Main_Species, Factor_source),
        summarise,
        Normalize_contri = median(Normalize_contri))

overall_Site_25TimesMean = 
  ddply(annual_Site_25TimesMean, 
        .(PMFdata, SiteCode, Cluster.No, Factor.No, 
          Factor, Main_Species, Factor_source),
        summarise,
        Normalize_contri = median(Normalize_contri))


overall_Cluster_25TimesMean_auto = 
  merge(overall_Cluster_25TimesMean, 
        source_assign_use)
overall_Cluster_noSeason99_auto = 
  merge(overall_Cluster_noSeason99, 
        source_assign_use)
overall_Site_25TimesMean_auto = 
  merge(overall_Site_25TimesMean, 
        source_assign_use)

###### data for mannual selection

overall_Cluster_25TimesMean_dual = 
  merge(overall_Cluster_25TimesMean_auto,
        combined_dual)
overall_Cluster_25TimesMean_dual = 
  overall_Cluster_25TimesMean_dual[
    with(overall_Cluster_25TimesMean_dual, 
         order(Cluster.No, Factor.No, SiteCode, Factor, Factor_source)), ]
overall_Cluster_25TimesMean_dual = 
  merge(overall_Cluster_25TimesMean_dual, 
        source_assign_use)
overall_Cluster_25TimesMean_dual = 
  overall_Cluster_25TimesMean_dual[
    !duplicated(overall_Cluster_25TimesMean_dual), ]
write.csv(overall_Cluster_25TimesMean_dual, 
          file.path(result_path, "overall_Cluster_25TimesMean_dual.csv"))

overall_Cluster_noSeason99_dual = 
  merge(overall_Cluster_noSeason99_auto,
        combined_dual)
overall_Cluster_noSeason99_dual = 
  overall_Cluster_noSeason99_dual[
    with(overall_Cluster_noSeason99_dual, 
         order(Cluster.No, Factor.No, SiteCode, Factor, Factor_source)), ]
overall_Cluster_noSeason99_dual = 
  merge(overall_Cluster_noSeason99_dual, 
        source_assign_use)
overall_Cluster_noSeason99_dual = 
  overall_Cluster_noSeason99_dual[
    !duplicated(overall_Cluster_noSeason99_dual), ]
write.csv(overall_Cluster_noSeason99_dual, 
          file.path(result_path, "overall_Cluster_noSeason99_dual.csv"))

overall_Site_25TimesMean_dual = 
  merge(overall_Site_25TimesMean_auto,
        combined_dual)
overall_Site_25TimesMean_dual = 
  overall_Site_25TimesMean_dual[
    with(overall_Site_25TimesMean_dual, 
         order(Cluster.No, Factor.No, SiteCode, Factor, Factor_source)), ]
overall_Site_25TimesMean_dual = 
  overall_Site_25TimesMean_dual[
    !duplicated(overall_Site_25TimesMean_dual), ]
write.csv(overall_Site_25TimesMean_dual, 
          file.path(result_path, "overall_Site_25TimesMean_dual.csv"))


#### 3. Source comparison - sensitivity ####

###### 3.1 Exclude the wrongly dual assigned sources ######

########## manual dual assign results
Cluster_25TimesMean_dual = read.csv("results_R_data/overall_Cluster_25TimesMean_dual.csv")
Cluster_noSeason99_dual = read.csv("results_R_data/overall_Cluster_noSeason99_dual.csv")
Site_25TimesMean_dual = read.csv("results_R_data/overall_Site_25TimesMean_dual.csv")

########## exclude the wrongly dual assigned sources
### Cluster_25TimesMean
Cluster_25TimesMean_dual_excl = 
  select(subset(Cluster_25TimesMean_dual, Use == 0),
         Cluster.No, Factor.No, Factor, SiteCode, Source_use)

Cluster_25TimesMean_dual_excl = 
  Cluster_25TimesMean_dual_excl[
    !duplicated(Cluster_25TimesMean_dual_excl), ]

# identify common columns
common_cols_Cluster_25TimesMean <- 
  intersect(names(overall_Cluster_25TimesMean_auto), 
            names(Cluster_25TimesMean_dual_excl))

Cluster_25TimesMean_nodual = 
  anti_join(overall_Cluster_25TimesMean_auto, 
            Cluster_25TimesMean_dual_excl, by = 
              common_cols_Cluster_25TimesMean)

### Cluster_noSeason99
Cluster_noSeason99_dual_excl = 
  select(subset(Cluster_noSeason99_dual, Use == 0),
         Cluster.No, Factor.No, Factor, SiteCode, Source_use)

Cluster_noSeason99_dual_excl = 
  Cluster_noSeason99_dual_excl[
    !duplicated(Cluster_noSeason99_dual_excl), ]

# identify common columns
common_cols_Cluster_noSeason99 <- 
  intersect(names(overall_Cluster_noSeason99_auto), 
            names(Cluster_noSeason99_dual_excl))

Cluster_noSeason99_nodual = 
  anti_join(overall_Cluster_noSeason99_auto, 
            Cluster_noSeason99_dual_excl, by = 
              common_cols_Cluster_noSeason99)


### Site_25TimesMean
Site_25TimesMean_dual_excl = 
  select(subset(Site_25TimesMean_dual, Use == 0),
         Cluster.No, Factor.No, Factor, SiteCode, Source_use)

Site_25TimesMean_dual_excl = 
  Site_25TimesMean_dual_excl[
    !duplicated(Site_25TimesMean_dual_excl), ]

# identify common columns
common_cols_Site_25TimesMean <- 
  intersect(names(overall_Site_25TimesMean_auto), 
            names(Site_25TimesMean_dual_excl))

Site_25TimesMean_nodual = 
  anti_join(overall_Site_25TimesMean_auto, 
            Site_25TimesMean_dual_excl, by = 
              common_cols_Site_25TimesMean)

########## rename for comparison

Cluster_25TimesMean_to_comp = 
  plyr::rename(
    Cluster_25TimesMean_nodual, 
    c("Normalize_contri" = 
        "Normalize_contri_Cluster_25TimesMean",
      "Main_Species" =
        "Main_Species_Cluster_25TimesMean"))
Cluster_25TimesMean_to_comp = 
  select(Cluster_25TimesMean_to_comp,
         Cluster.No, Factor.No, SiteCode, 
         Factor_source, Main_Species_Cluster_25TimesMean, 
         Normalize_contri_Cluster_25TimesMean)


Cluster_noSeason99_to_comp = 
  plyr::rename(
    Cluster_noSeason99_nodual, 
    c("Normalize_contri" = 
        "Normalize_contri_Cluster_noSeason99",
      "Main_Species" =
        "Main_Species_Cluster_noSeason99"))
Cluster_noSeason99_to_comp = 
  select(Cluster_noSeason99_to_comp,
         Cluster.No, Factor.No, SiteCode, 
         Factor_source, Main_Species_Cluster_noSeason99, 
         Normalize_contri_Cluster_noSeason99)


Site_25TimesMean_to_comp = 
  plyr::rename(
    Site_25TimesMean_nodual, 
    c("Normalize_contri" = 
        "Normalize_contri_Site_25TimesMean",
      "Main_Species" =
        "Main_Species_Site_25TimesMean"))
Site_25TimesMean_to_comp = 
  select(Site_25TimesMean_to_comp,
         Cluster.No, Factor.No, SiteCode, 
         Factor_source, Main_Species_Site_25TimesMean, 
         Normalize_contri_Site_25TimesMean)

###### 3.2.1 Comparison tedious way - Clusters - extreme thresholds ######

cluster_extrem_threshold_comp = 
  merge(Cluster_25TimesMean_to_comp, Cluster_noSeason99_to_comp)
cluster_extrem_threshold_comp$low25_high99 =
  ifelse(cluster_extrem_threshold_comp$Normalize_contri_Cluster_25TimesMean <
           cluster_extrem_threshold_comp$Normalize_contri_Cluster_noSeason99,
         1, 0)
summary(cluster_extrem_threshold_comp)
quantile(cluster_extrem_threshold_comp$Normalize_contri_Cluster_25TimesMean,
         c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975))
quantile(cluster_extrem_threshold_comp$Normalize_contri_Cluster_noSeason99,
         c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), na.rm = TRUE)
cor.test(cluster_extrem_threshold_comp$Normalize_contri_Cluster_25TimesMean,
    cluster_extrem_threshold_comp$Normalize_contri_Cluster_noSeason99,
    method = "spearm")
cor.test(cluster_extrem_threshold_comp$Normalize_contri_Cluster_25TimesMean,
         cluster_extrem_threshold_comp$Normalize_contri_Cluster_noSeason99,
         method = "pearson")
low_sea_salt =
  subset(cluster_extrem_threshold_comp,
         low25_high99 == 1 &
           Factor_source == "F6-Fresh Sea Salt" &
           Normalize_contri_Cluster_noSeason99 < 0.5)

# calculate correlation for each Factor_source
corr_labels_extreme <- 
  cluster_extrem_threshold_comp %>%
  group_by(Factor_source) %>%
  nest() %>%
  dplyr::mutate(
    label = 
      map_chr(data, 
              ~calculate_corr_label(
                .x, 
                "Normalize_contri_Cluster_25TimesMean", 
                "Normalize_contri_Cluster_noSeason99"))) %>%
  select(-data)

# Merge the labels back to the original data
cluster_extrem_threshold_comp <- 
  left_join(cluster_extrem_threshold_comp, 
            corr_labels_extreme, 
            by = "Factor_source")


ggplot(cluster_extrem_threshold_comp,
       aes(x = Normalize_contri_Cluster_25TimesMean,
           y = Normalize_contri_Cluster_noSeason99)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, color = "red") +
  facet_wrap(~ Factor_source, ncol = 3) +
  geom_text(aes(label = label), 
            x = Inf, y = Inf, 
            hjust = 1, vjust = 1, 
            check_overlap = TRUE) +
  # labs(x = "Percent Contribution  %", y = "Count") +
  labs(x = "Thresholds-25*mean, Contribution", 
       y = "Thresholds-seasonal 99th percentile, Contribution") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 15, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))


###### 3.2.2 Comparison tedious way - Cluster vs. Sites ######

site_cluster_comp = 
  merge(Cluster_25TimesMean_to_comp, Site_25TimesMean_to_comp)

# calculate correlation for each Factor_source
corr_labels_site_cluster <- 
  site_cluster_comp %>%
  group_by(Factor_source) %>%
  nest() %>%
  dplyr::mutate(
    label = 
      map_chr(data, 
              ~calculate_corr_label(
                .x, 
                "Normalize_contri_Cluster_25TimesMean", 
                "Normalize_contri_Site_25TimesMean"))) %>%
  select(-data)

# Merge the labels back to the original data
site_cluster_comp <- 
  left_join(site_cluster_comp, 
            corr_labels_site_cluster, 
            by = "Factor_source")

ggplot(site_cluster_comp,
       aes(x = Normalize_contri_Cluster_25TimesMean,
           y = Normalize_contri_Site_25TimesMean)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, color = "red") +
  facet_wrap(~ Factor_source, ncol = 3) +
  geom_text(aes(label = label), 
            x = Inf, y = Inf, 
            hjust = 1, vjust = 1, 
            check_overlap = TRUE) +
  # labs(x = "Percent Contribution  %", y = "Count") +
  labs(x = "Multi-site PMF based, Contribution", 
       y = "Single-site PMF based, Contribution") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 15, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))

summary(site_cluster_comp)

plot(site_cluster_comp$Normalize_contri_Cluster_25TimesMean, 
     site_cluster_comp$Normalize_contri_Site_25TimesMean)


#### 4. Source comparison - 2 source contribution estimation methods ####

###### 4.1 25timesMean single site ######

site_overall = read.csv("results_R_data/CSN_Site_25TimesMean_overall.csv")

# calculate correlation for each Factor_source
corr_labels_site_cluster <- 
  site_overall %>%
  group_by(Factor_source) %>%
  nest() %>%
  dplyr::mutate(
    label = 
      map_chr(data, 
              ~calculate_corr_label(
                .x, 
                "Factor.contribution", 
                "Fractrion_conc_based"))) %>%
  select(-data)

corr_labels_site_cluster = 
  subset(corr_labels_site_cluster,
         !(grepl("Factor", Factor_source, fixed = T)))

site_overall_6source = merge(site_overall, corr_labels_site_cluster)

ggplot(site_overall_6source,
       aes(x = Factor.contribution,
           y = Fractrion_conc_based)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, color = "red") +
  facet_wrap(~ Factor_source, ncol = 3) +
  geom_text(aes(label = label), 
            x = Inf, y = Inf, 
            hjust = 1, vjust = 1, 
            check_overlap = TRUE) +
  # labs(x = "Percent Contribution  %", y = "Count") +
  labs(x = "Overall Normalized Contribution_OLS MLR", 
       y = "Concentration Contribution_PMF MatrixBB") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 15, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))

###### 4.1 25timesMean cluster ######

cluster_overall = read.csv("results_R_data/CSN_Cluster_25TimesMean_overall.csv")

# calculate correlation for each Factor_source
corr_labels_cluster_cluster <- 
  cluster_overall %>%
  group_by(Factor_source) %>%
  nest() %>%
  dplyr::mutate(
    label = 
      map_chr(data, 
              ~calculate_corr_label(
                .x, 
                "Factor.contribution", 
                "Fractrion_conc_based"))) %>%
  select(-data)

corr_labels_cluster_cluster = 
  subset(corr_labels_cluster_cluster,
         !(grepl("Factor", Factor_source, fixed = T)))

cluster_overall_6source = merge(cluster_overall, corr_labels_cluster_cluster)

ggplot(cluster_overall_6source,
       aes(x = Factor.contribution,
           y = Fractrion_conc_based)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, color = "red") +
  facet_wrap(~ Factor_source, ncol = 3) +
  geom_text(aes(label = label), 
            x = Inf, y = Inf, 
            hjust = 1, vjust = 1, 
            check_overlap = TRUE) +
  # labs(x = "Percent Contribution  %", y = "Count") +
  labs(x = "Overall Normalized Contribution_OLS MLR", 
       y = "Concentration Contribution_PMF MatrixBB") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(color="grey25", size = 15, vjust=0), # facet title
        axis.title.x = element_text(color="grey25", size = 18, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 18, vjust=1),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))
