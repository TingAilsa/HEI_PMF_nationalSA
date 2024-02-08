##clear environment
# rm(list=ls())

getwd()
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/results_R_data"

####### Libraries to use ####### 
library(tidyr)
library(dplyr)
library(plyr)
library(base)
library(lubridate)
library(data.table)
library(ggplot2)
library(ggsci)
library(ggpattern)
library(ggthemes)
library(scales)
library(extrafont) 

## extrafont{}, help make non-standard fonts accessible to the plotting devices
font_import()
loadfonts(device = "pdf")

# library(gridExtra)

####### Read & process other files to use ####### 
# Directory containing the CSV files you want to read
# csv_folder <- "/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_txt_noCsub_noExtreme/other_files/"

#### 1.1 CSN noCsub noExtreme - no extreme of any species#### 

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_base_DISPres1/")

cluster_info_all = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/PMF_NoGUI_NoCsub_NoExtreme_cluster/CSN_noCsub_noExtreme_PMF_CMD_StrongWeakBad_Cluster.csv")
species_class = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_Species_class_sub.csv")
cluster_info_all$X = species_class$X = NULL

noCsub_noExtreme = "CSN_NoGUI_NoCsub_NoExtreme_cluster"
data.prefix = "CSN_noCsub_noExtreme_"
disp.prefix = "CSN_"

#### 1.2 CSN noCsub 25TimesMean #### 

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_Cluster_25TimesMean/")

cluster_info_all = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_NoGUI_NoCsub_25TimesMean_cluster/CSN_noCsub_25timesMean_PMF_CMD_StrongWeakBad_Cluster.csv")
species_class = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_Species_class_sub.csv")
cluster_info_all$X = species_class$X = NULL

noCsub_noExtreme = "CSN_NoGUI_NoCsub_25TimesMean_cluster"
data.prefix = "CSN_noCsub_25TimesMean_"
disp.prefix = "CSN_"

#### 1.3 CSN noCsub noSeason99 #### 

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_Cluster_noSeason99/")

cluster_info_all = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_NoGUI_NoCsub_noSeason99_cluster/CSN_noCsub_noSeason99_PMF_CMD_StrongWeakBad_Cluster.csv")
species_class = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_Species_class_sub.csv")
cluster_info_all$X = species_class$X = NULL

noCsub_noExtreme = "CSN_NoGUI_NoCsub_noSeason99_cluster"
data.prefix = "CSN_noCsub_noSeason99_"
disp.prefix = "CSN_"


#### 1.4 IMPROVE noCsub noExtreme - no extreme of any species#### 
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/IMPROVE_base_DISPres1")

cluster_info_all = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_data_prepare/IMPROVE_noCsub_noExtreme_PMF_CMD_StrongWeakBad_Cluster.csv")
species_class = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_Species_class_sub.csv")
cluster_info_all$X = species_class$X = NULL

noCsub_noExtreme = "IMPROVE_NoGUI_NoCsub_NoExtreme_cluster"
data.prefix = "IMPROVE_noCsub_noExtreme_"
disp.prefix = "IMPROVE_"

#### 1.N shared process #### 

cluster_info_all = plyr::rename(
  cluster_info_all, 
  c("K." = "KIon",
    "Na." = "NaIon", 
    "NH4." = "NH4Ion",
    "NO3" = "NO3Ion",
    "SO4" = "SO4Ion",
    "PM25" = "PM2.5"))

source_cluster = paste0("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/", 
                        noCsub_noExtreme, "_SiteDate")

########################################################
######## 2. Process the PMF non-GUI outputs & Plot ####### 
########################################################

correl_r_p_summary = NULL
summary_base = NULL

for (cluster.No in 1:25) { # 1:25
  for (factor.No in 6:11) { # 6:11, 6:6
    
    cluster.factor.pre = paste0("C_", cluster.No, "_F_", factor.No, "_")
    name.prefix = paste0(data.prefix, cluster.factor.pre)
    
    tryCatch({
      ####### Read the results from bash script ####### 
      # Access the input file name passed as an argument
      folder_path <- paste0("Cluster_", cluster.No, "/Factor_", factor.No, "/")
      base_output = readLines(paste0(folder_path, 
                                     data.prefix, 
                                     cluster.factor.pre,
                                     "base.txt")) # "_base.txt"
      disp_output = readLines(paste0(folder_path, 
                                     disp.prefix, 
                                     cluster.factor.pre,
                                     "DISPres1.txt"))
      
      # Find the number of task when the value of Qm is the lowest
      lowest_Qm_taskNo = lowest_Qm_task(base_output)$lowest_Qm_task
      lowest_Qm = lowest_Qm_task(base_output)$lowest_Qm
      
      Factor.serial = paste0("Factor", 1:factor.No)
      
      ####### Cluster-specific species strong, weak, bad #######
      cluster_info = subset(cluster_info_all, 
                            Finaly.Decision == cluster.No)
      cluster.data.row = cluster_info$cluster.row
      
      # detect the column range for PM species & PM2.5
      col_comp_all = col_comp(cluster_info, "Al", "PM2.5")
      
      ## Select weak & strong variables by the value
      cluster.weak.strong = strong_weak(cluster_info, "Al", "PM2.5")
      cluster.w.s.count = length(cluster.weak.strong)
      
      #cluster.strong = strong_species(cluster_info, "Al", "PM2.5")
      #cluster.str.count = length(cluster.strong)
      
      #cluster.weak = weak_species(cluster_info, "Al", "PM2.5")
      #cluster.weak.count = length(cluster.weak)
      
      
      ####### Extract base info from PMF CMD outputs & match with date, PM #######
      
      # Fitted G vs. reference G & Time series
      base_info = base_results(base_output, 
                               lowest_Qm_taskNo, 
                               cluster.data.row)
      # base_G_cor = base_info$base_G_cor
      base_ts = base_info$base_ts
      base_contri = base_info$base_contri
      base_ts_conc = base_info$base_ts_conc
      base_conc_fraction = base_info$base_fraction
      base_conc_fraction = as.data.frame(t(base_conc_fraction))
      colnames(base_conc_fraction)[1] = "Fractrion_conc_based"
      base_conc_fraction$Faraction_conc_contri = 
        paste0(round(base_conc_fraction$Fractrion_conc_based*100, 1),
               "%")
      base_conc_fraction$Factor = rownames(base_conc_fraction)
      
      # assign species names
      base_contri$Species = cluster.weak.strong
      base_percent = conc_percent_contri(base_contri)
      
      base_nmContri_plot = gather(base_contri,
                                  "Factor", 
                                  "Normalized_Contri", 
                                  -Species)
      
      base_percent_plot = gather(base_percent,
                                 "Factor", 
                                 "Percent", 
                                 -Species)
      
      # Extract site & date info
      site_date_cluster = read.csv(
        file.path(
          source_cluster,
          paste0(data.prefix, "C_", cluster.No, "_SiteDate.csv")),
        header = T)
      
      # normalized and overall contributions based on OLS MLR
      time_series_analyses = time_series(base_ts, site_date_cluster)
      base_ts_nmContri_gather = time_series_analyses$base_ts_nmContri_gather
      base_ts_nmContri_spread = time_series_analyses$base_ts_nmContri_spread
      ts_PM_lm_beta = time_series_analyses$ts_PM_lm_beta
      cor_PM_sumSpecies = time_series_analyses$cor_PM_sumSpecies
      base_ts_nmContri_all = time_series_analyses$base_ts_nmContri_all
      
      # pivot data from wide to long
      base_ts_nmContri_long <-
        base_ts_nmContri_all %>% 
        pivot_longer(
          cols = Factor.serial[1]:Factor.serial[length(Factor.serial)],
          names_to = c("Factor"),
          # names_pattern = "new_?(.*)_(.)(.*)",
          values_to = "Normalized_Contri"
        )
      
      # Get annual and seasonal contributions
      base_ts_nmContri_gather <- 
        base_ts_nmContri_gather %>%
        mutate(Year = year(Date),
               Season = case_when(
                 month(Date) %in% c(12, 1, 2) ~ "Winter",
                 month(Date) %in% c(3, 4, 5) ~ "Spring",
                 month(Date) %in% c(6, 7, 8) ~ "Summer",
                 month(Date) %in% c(9, 10, 11) ~ "Fall"
               ),
               Month = month(Date))
      
      ts_annual_nmContri = ddply(base_ts_nmContri_gather, 
                                 .(Year, SiteCode, Factor),
                                 summarise,
                                 Normalize_contri = median(Normalize_contri))
      
      ts_season_nmContri = ddply(base_ts_nmContri_gather, 
                                 .(Season, SiteCode, Factor),
                                 summarise,
                                 Normalize_contri = median(Normalize_contri))
      
      ts_month_nmContri = ddply(base_ts_nmContri_gather, 
                                .(Year, Month, SiteCode, Factor),
                                summarise,
                                Normalize_contri = median(Normalize_contri))
      
      ####### Extract DISP info from PMF CMD, and validation results #######
      
      # DISP results - Credible intervals
      disp_down_conc = disp_analysis(disp_output)$disp_down
      disp_up_conc = disp_analysis(disp_output)$disp_up
      disp_down_conc$Species = disp_up_conc$Species = cluster.weak.strong
      
      disp_down_percent = conc_percent_contri(disp_down_conc)
      disp_down_percent_plot = gather(disp_down_percent,
                                     "Factor", 
                                     "Percent.down", 
                                     -Species)
      
      disp_up_percent = conc_percent_contri(disp_up_conc)
      disp_up_percent_plot = gather(disp_up_percent,
                                   "Factor", 
                                   "Percent.up", 
                                   -Species)
      
      # DISP results - overall validation of results
      disp.error.code = disp_analysis(disp_output)[[1]]
      disp.qdrop = disp_analysis(disp_output)[[2]]
      
      
      ####### Source Assignment & Match #######
      #main3_species = Nmain_Species(base_percent, 3)
      main5_species = Nmain_Species(base_percent, 5)
      
      # Source detection according to info in recent publications
      main_source = source_ref(base_percent, 5)
      main_source$Source.No = sapply(strsplit(main_source$Source_reference, "-"), "[", 1)
      main_source$Factor_source = main_source$Factor
      main_source$Factor_source[main_source$Source_reference != "F-"] = 
        main_source$Source_reference[main_source$Source_reference != "F-"]
      
      # replace colnames in base_ts by the exact source
      colnames(base_ts_nmContri_spread)[1:factor.No] = main_source$Factor_source
      base_ts_nmContri_spread$Date = NULL
      base_ts_nmContri_spread = site_color(base_ts_nmContri_spread)
      
      # merge datasets for source profile plotting
      base_disp_nmContri_to_merge <- 
        list(base_nmContri_plot, base_percent_plot, 
             disp_down_percent_plot, disp_up_percent_plot, 
             species_class, main_source)
      
      nmContri_percent_bsDisp <- 
        Reduce(function(x, y) 
          merge(x, y), 
          base_disp_nmContri_to_merge)
      
      # match time-series normalized contribution data with source info
      ts_nmContri_plot = merge(base_ts_nmContri_gather, main_source)
      ts_annual_nmContri = merge(ts_annual_nmContri, main_source)
      ts_season_nmContri = merge(ts_season_nmContri, main_source)
      ts_month_nmContri = merge(ts_month_nmContri, main_source)
      lm_beta_plot = merge(ts_PM_lm_beta, main_source)
      base_ts_nmContri_long_plot = merge(base_ts_nmContri_long, main_source)
      
      # match overall contribution based on normalized data with that from mass concentration
      lm_beta_plot = merge(lm_beta_plot, base_conc_fraction)
      lm_beta_plot$Fractrion_conc_based = 100 * lm_beta_plot$Fractrion_conc_based
      
      ####### Number of Factor & Factor_source #######
      Factor_source_count = 
        ddply(ts_annual_nmContri, 
              .(Factor_source, Factor), 
              summarise,
              Normalize_contri = mean(Normalize_contri))
      
      Factor_source_match = 
        data.frame(
          table(
            Factor_source_count$Factor_source))
      
      colnames(Factor_source_match) = 
        c("Factor_source", "Corresponding_factor")
     
      source_2more_factor = 
        as.character(
          Factor_source_match$Factor_source[
            Factor_source_match$Corresponding_factor > 
              1])
      
      count_not_intepretate <- 
        sum(grepl("Factor", 
                  Factor_source_match$Factor_source, 
                  ignore.case = TRUE))

      # Replace character(0) with NA or another placeholder
      source_2more_factor <- if(length(source_2more_factor) == 0) NA else source_2more_factor

      
      ####### Plotting #######
      
      #### Source Profile - Concentration & percent Normalize_contri #### 
      nmContri_percent_bsDisp_use = nmContri_percent_bsDisp
      nmContri_percent_bsDisp_use = merge(nmContri_percent_bsDisp_use, lm_beta_plot)
      # nmContri_percent_bsDisp_use = subset(nmContri_percent_bsDisp, Source.No != "F")
      
      # Convert 0 to 1e-10 for columns to be used for y-axis, there is log transfer later
      nmContri_percent_bsDisp_use <- 
        nmContri_percent_bsDisp_use %>%
        mutate(across(Normalized_Contri:Percent.up, 
                      ~replace(., . == 0, 1e-10)))
      
      # Convert percent values to make the scale pattern similar to log Normalize_contri
      # nmContri_percent_bsDisp_use$LogNormalized_Contri <- log10(nmContri_percent_bsDisp_use$Normalized_Contri)
      nmContri_percent_bsDisp_use$Trans.Percent = 
        trans_log(nmContri_percent_bsDisp_use$Percent, log(1e-05))
      nmContri_percent_bsDisp_use$Trans.Percent.down = 
        trans_log(nmContri_percent_bsDisp_use$Percent.down, log(1e-05))
      nmContri_percent_bsDisp_use$Trans.Percent.up = 
        trans_log(nmContri_percent_bsDisp_use$Percent.up, log(1e-05))
      
      # Correcting any inverted values
      inverted_rows <- nmContri_percent_bsDisp_use$Trans.Percent.up < nmContri_percent_bsDisp_use$Trans.Percent.down
      # Swapping the values using a temporary variable
      temp <- nmContri_percent_bsDisp_use$Trans.Percent.up[inverted_rows]
      nmContri_percent_bsDisp_use$Trans.Percent.up[inverted_rows] <- nmContri_percent_bsDisp_use$Trans.Percent.down[inverted_rows]
      nmContri_percent_bsDisp_use$Trans.Percent.down[inverted_rows] <- temp
      
      #### Step trying to identify the trans for sec.axis
      #trans.num = log(numbers + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)
      #y.values = exp(trans.num)
      #trans.y = log(y.values + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)
      
      #log(exp(log(c(1e-10, 10, 20, 50, 100) + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)) + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)
      #log(exp(log(c(min(nmContri_percent_bsDisp_use$Percent), max(nmContri_percent_bsDisp_use$Percent)) + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)) + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)
      
      # Define the position of the factor/source name
      # find the middle position of the factor/source names on x-axis
      middle_positions <- 
        nmContri_percent_bsDisp_use %>%
        dplyr::group_by(Factor_source) %>%
        dplyr::summarize(middle = custom_median(as.numeric(sequence)),
                         Factor_nm_contr = paste(unique(Factor.contr), 
                                                 collapse = ", "),
                         Factor_conc_fr = paste(unique(Faraction_conc_contri), 
                                                collapse = ", ")) %>%
        dplyr::arrange(Factor_source)
      
      middle_species = 
        nmContri_percent_bsDisp_use$Species[
          nmContri_percent_bsDisp_use$sequence == 
            middle_positions$middle[1]]
        
      middle_positions$Species = middle_species[1]
      middle_positions$Factor_source_contr = 
        paste0(middle_positions$Factor_source, ", ", 
               "nm_contri ", middle_positions$Factor_nm_contr, ", ",
               "conc_fr ", middle_positions$Factor_conc_fr)
      
      # Set the breaks
      log_breaks = 
        log(exp(log(c(1e-10, 10, 20, 50, 100) + 1) / 
                log(100) * 
                (log(1e-01) - log(1e-05)) + 
                log(1e-05)) + 1) / log(100) * 
        (log(1e-01) - log(1e-05)) + 
        log(1e-05)
      
      # Create the plot
      conc_percent_source <- 
        ggplot(nmContri_percent_bsDisp_use,
               aes(x = reorder(Species, sequence), 
                   group = Factor_source)) +
        # Bar plot for Concentration
        geom_bar(aes(y = Normalized_Contri, fill = Factor_source), 
                 stat = "identity", width = 0.6, alpha = 0.8) +
        # Point plot for transformed Percent
        geom_point(aes(y = exp(Trans.Percent)), color = "black", shape = 15) +
        geom_errorbar(aes(ymin = exp(Trans.Percent.down), 
                          ymax = exp(Trans.Percent.up)), 
                      width = 0.4) +
        facet_grid(Factor_source ~ ., switch = "y") +
        ggtitle(paste0(paste0(disp.prefix, cluster.factor.pre), # "\n",
                       ", Error.Code = ", disp.error.code, 
                       ", DISP.Qdrop = ", disp.qdrop)) + 
        scale_y_log10(
          name = "Normalized Contribution",
          limits = c(1e-05, 1e-01),
          breaks = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01),
          labels = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01),
          sec.axis = sec_axis(
            trans = ~ log(. + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05),
            name = "% of Species",
            breaks = log_breaks,
            labels = c(0, 10, 20, 50, 100)
          )
        ) +
        scale_y_continuous(trans = mylog_trans(base=10, from=-5),
                           limits = c(1E-5, max(nmContri_percent_bsDisp_use$Normalized_Contri))) +
        scale_fill_npg() +
        xlab(format_variable("PM25 Species")) +
        scale_x_discrete(labels = function(x) format_variable(x)) +
        geom_text(data = middle_positions, size = 4,
                  aes(x = Species, y = 1e-01, label = Factor_source_contr), 
                  inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        theme_bw() +
        theme_text_speciesName +
        theme(
          panel.grid = element_line(colour = "white"),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          legend.position = "none"
        )

      # conc_percent_source
        
      #### Time-series factor percent contribution #### 
      
      #### Daily
      # daily_plot_use = subset(ts_plot, Source.No != "F")
      daily_plot_use = ts_nmContri_plot
      
      middle_positions$middle_day = median(daily_plot_use$Date)
      middle_positions$middle_month = month(median(daily_plot_use$Date))
      middle_positions$middle_year = year(median(daily_plot_use$Date))
      
      daily_oneSite_nmContri <- 
        ggplot(subset(daily_plot_use,
                      SiteCode = unique(daily_plot_use$SiteCode)[2]), 
               # ggplot(daily_plot_use,
               aes(x = Date, y = Normalize_contri, 
                   group = Factor_source, color = Factor_source)) +
        facet_grid(Factor_source ~., scales = "free_y") +
        geom_line(linewidth = 0.3, alpha = 0.8)+
        geom_point(size = 0.3, alpha = 0.4) +
        scale_color_npg() +
        geom_text(data = middle_positions, size = 4,
                  aes(x = middle_day, y = 4, label = Factor_source_contr), 
                  inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        theme_ts +
        theme(
          panel.grid = element_line(colour = "white"),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          legend.position = "none"
        )
      
      #### Annual
      # annual_plot_use = subset(ts_annual_nmContri, Source.No != "F")
      annual_plot_use = ts_annual_nmContri
      
      annual_cluster <- 
        #  ggplot(subset(annual_plot_use,
        #                SiteCode = unique(annual_plot_use$SiteCode)[2]), 
        ggplot(annual_plot_use,
               aes(x = as.factor(Year), y = Normalize_contri, 
                   color = Factor_source)) +
        facet_grid(Factor_source ~., scales = "free_y") +
        geom_boxplot(linewidth = 0.3, width = 0.5, alpha = 0.8) +
        scale_color_npg() +
        theme_ts +
        theme(
          panel.grid = element_line(colour = "white"),
          legend.position = "none"
        )
      
      #### Month
      # month_plot_use = subset(ts_month_nmContri, Source.No != "F")
      month_plot_use = ts_month_nmContri
      month_plot_use =  ddply(month_plot_use, 
                              .(Month, SiteCode, Factor, Main_Species, 
                                Source_reference, Source.No, Factor_source),
                              summarise,
                              Normalize_contri = median(Normalize_contri))
      
      month_cluster <- 
        #  ggplot(subset(month_plot_use,
        #                SiteCode = unique(month_plot_use$SiteCode)[2]), 
        ggplot(month_plot_use,
               aes(x = as.factor(Month), y = Normalize_contri, 
                   color = Factor_source)) +
        facet_grid(Factor_source ~., scales = "free_y") +
        geom_boxplot(linewidth = 0.3, width = 0.5, alpha = 0.8) +
        scale_color_npg() +
        theme_ts +
        theme(
          panel.grid = element_line(colour = "white"),
          legend.position = "none"
        )
      
      #### Overall factor percent contribution #### 
      # lm_beta_plot_use = subset(lm_beta_plot, Source.No != "F")
      lm_beta_plot_use = lm_beta_plot
      
      lm_beta_nm_contri = select(lm_beta_plot, 
                                 Factor, Factor_source, 
                                 Factor.contribution, Factor.contr)
      lm_beta_nm_contri$Fraction_data = "NM_contribution"
      
      lm_beta_conc_fr = select(lm_beta_plot, 
                               Factor, Factor_source, 
                               Fractrion_conc_based, Faraction_conc_contri)
      lm_beta_conc_fr$Fraction_data = "Concentration_base"
      
      colnames(lm_beta_conc_fr)[3:4] = colnames(lm_beta_nm_contri)[3:4]
      contri_fraction = rbind(lm_beta_conc_fr, lm_beta_nm_contri)
      
      overall_contri_2df <-
        ggplot(contri_fraction, 
               aes(x = Factor, y = Factor.contribution)) +
        facet_grid(Fraction_data ~. , 
                   switch = "y", scales = "free_y") + 
        geom_bar_pattern(aes(fill = Factor),
                         stat = "identity",
                         pattern_color = "white",
                         pattern_fill = "white",
                         # alpha = 0.8,
                         width = 0.3)  +
        geom_text(aes(label = paste(Factor_source, Factor.contr)), 
                  size = 6, angle = 90, hjust = 0, vjust = -3,
                  position = position_stack(vjust = 0)) + # start from same bottom level
        scale_fill_npg() +
        scale_y_continuous(position = "right") +
        # labs(y = "Fraction (%)") +
        theme_bw() +
        theme(panel.grid.major.x = element_line(colour="grey60", linetype="dashed"),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              strip.background = element_blank(), 
              strip.text = element_text(size = 20),
              legend.position = "none",
              axis.text.x = element_text(color="grey25", size = 16, angle = 90, hjust = 0.5, vjust = 0.5), 
              axis.text.y = element_text(color="grey25", size = 16, angle = 90, hjust = 5, vjust = -0.5),
              axis.title.x = element_text(color="grey25", size = 22, angle = 180, hjust = 0.5),
              axis.title.y = element_text(color = "grey25", size = 0, angle = 27, vjust = 3, hjust = 1.1))
      
      overall_contri <-
        ggplot(lm_beta_plot_use, 
               aes(x = Factor, y = Factor.contribution)) +
        geom_bar_pattern(aes(fill = Factor),
                         stat = "identity",
                         pattern_color = "white",
                         pattern_fill = "white",
                         # alpha = 0.8,
                         width = 0.3)  +
        geom_text(aes(label = paste(Factor_source, Factor.contr)), 
                  size = 6, angle = 90, hjust = 0, vjust = -3,
                  position = position_stack(vjust = 0)) + # start from same bottom level
        scale_fill_npg() +
        scale_y_continuous(position = "right") +
        theme_bw() +
        theme(panel.grid.major.x = element_line(colour="grey60", linetype="dashed"),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              legend.position = "none",
              axis.text.x = element_text(color="grey25", size = 16, angle = 90, hjust = 0.5, vjust = 0.5), 
              axis.text.y = element_text(color="grey25", size = 16, angle = 90, hjust = 5, vjust = -0.5),
              axis.title.x = element_text(color="grey25", size = 22, angle = 180, hjust = 0.5),
              axis.title.y = element_text(color="grey25", size = 0, angle = -90, vjust = -1))
      
      ####### Pairs: between-factor scattor & correlation & summary form #######
      
      # Use captured pch and col outside the function 
      point_char = 19
      point_size = 1
      point_alpha = 0.3
      point_col <- base_ts_nmContri_spread$color
      base_contribute_pair = base_ts_nmContri_spread[, 1:factor.No]
      
      # Initialize an empty data frame to store results
      correl_r_p <- 
        data.frame(Var1 = character(), Var2 = character(), 
                   Correlation = numeric(), P_Value = numeric(), 
                   stringsAsFactors = FALSE)
      
      
      # Iterate over all pairs of variables
      for (i in 1:(ncol(base_contribute_pair) - 1)) {
        for (j in (i + 1):ncol(base_contribute_pair)) {
          corr_result <- cor.test(base_contribute_pair[[i]], 
                                      base_contribute_pair[[j]])

          correl_r_p <- rbind(correl_r_p, 
                              c(colnames(base_contribute_pair)[i], 
                                colnames(base_contribute_pair)[j], 
                                round(corr_result$estimate, digits=3), 
                                round(corr_result$p.value, digits=5)))
        }
      }
      
      colnames(correl_r_p) <- c("Var1", "Var2", "Correlation", "P_Value")
      correl_r_p$cluster.No = cluster.No
      correl_r_p$factor.No = factor.No
      correl_r_p$Dataset = disp.prefix
      
      correl_r_percentile <- quantile(as.numeric(correl_r_p$Correlation), 
                                      probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))
      
      correl_r_p_summary = rbind(correl_r_p_summary, correl_r_p)
      
      base_oneFactor = data.frame(Dataset = disp.prefix, Cluster = cluster.No, Factor = factor.No, 
                                  Qmin = lowest_Qm, Qmin_task_No = lowest_Qm_taskNo, 
                                  DISP_error.code = disp.error.code, DISP_qdrop = disp.qdrop, 
                                  Correlation.between.factors.Min = correl_r_percentile[1],
                                  Correlation.between.factors.10th = correl_r_percentile[2],
                                  Correlation.between.factors.25th = correl_r_percentile[3],
                                  Correlation.between.factors.50th = correl_r_percentile[4],
                                  Correlation.between.factors.75th = correl_r_percentile[5],
                                  Correlation.between.factors.90th = correl_r_percentile[6],
                                  Correlation.between.factors.Max = correl_r_percentile[7],
                                  With.multiple.factors.assigned.to.one.source = length(unique(source_2more_factor)), 
                                  Name.of.source.with.multiple.factors. = source_2more_factor,
                                  Number.of.factors.not.intepretated.Before.screening = count_not_intepretate)
      
      summary_base = rbind(summary_base, base_oneFactor[1, ])
      
      write.csv(summary_base, paste0(disp.prefix, "base_DISP_summary.csv"))
      write.csv(correl_r_p_summary, paste0(disp.prefix, "base_factor_correlation.csv"))
      
      # Pair plotting
      pdf(paste0(name.prefix, "factor_pairs.pdf"))
      
      unique_sites <- unique(base_ts_nmContri_spread$SiteCode)
      unique_colors <- unique(base_ts_nmContri_spread$color)
      
      # par(oma = c(4, 4, 6, 10), mar = c(4, 4, 2, 2)) 
      pairs(base_ts_nmContri_spread[, 1:factor.No], 
            upper.panel = panel.scatter,
            lower.panel = panel.corr,
            main = name.prefix)
      
      par(xpd=TRUE)
      
      legend(0, 0.2, ## "bottomright", 
             as.vector(unique_sites),  
             fill=unique_colors,
             cex = 0.7, 
             # pt.cex = 0.5,
             x.intersp = 0.3,
             y.intersp = 0.7,  
             # text.width = 1,
             bg = "transparent")
      
      #legend(x = "center", 
      #       # inset = c(-0.3, 0),  # Adjust the inset for precise positioning
      #       legend = unique_sites, col = unique_colors, 
      #       pch = point_char, cex = point_size)
      
      
      dev.off()
      
      ####### Daily source-specific contribution vs. PM2.5 #######
      
      # calculate correlation for each Factor_source
      corr_labels_cluster_PM <- 
        base_ts_nmContri_long_plot %>%
        group_by(Factor_source) %>%
        nest() %>%
        dplyr::mutate(
          label = 
            map_chr(data, 
                    ~calculate_corr_label(
                      .x, 
                      "PM2.5", 
                      "Normalized_Contri"))) %>%
        select(-data)
      
      corr_labels_cluster_PM = 
        subset(corr_labels_cluster_PM,
               !(grepl("Factor", Factor_source, fixed = T)))
      
      base_ts_nmContri_long_plot = 
        merge(base_ts_nmContri_long_plot, corr_labels_cluster_PM)
      
      PM_source_daily<-
        ggplot(base_ts_nmContri_long_plot,
               aes(x = PM2.5,
                   y = Normalized_Contri)) +
        geom_point() +
        geom_abline(slope=1, intercept=0, color = "red") +
        facet_wrap(~ Factor_source, ncol = 3) +
        geom_text(aes(label = label), 
                  x = Inf, y = Inf, 
                  hjust = 1, vjust = 1, 
                  check_overlap = TRUE) +
        # labs(x = "Percent Contribution  %", y = "Count") +
        labs(x = format_variable("Daily PM25 µg/m3"), 
             y = "Normalized Contribution") +
        theme_minimal() +
        theme(legend.position = "none",
              strip.background = element_blank(), 
              strip.text = element_text(color="grey25", size = 15, vjust=0), # facet title
              axis.title.x = element_text(color="grey25", size = 18, vjust=0, family = "Arial Unicode MS"), 
              axis.title.y = element_text(color="grey25", size = 18, vjust=1, family = "Arial Unicode MS"),
              plot.margin = unit(c(2,1,2, 2), "lines"),
              axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
              axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))
      
      
      ####### Output files #######
      
      ggsave(paste0(name.prefix, "daily.pdf"), plot = daily_oneSite_nmContri, width = 6, height = 7.5)
      ggsave(paste0(name.prefix, "month.pdf"), plot = month_cluster, width = 6, height = 7.5)
      ggsave(paste0(name.prefix, "annual.pdf"), plot = annual_cluster, width = 6, height = 7.5)
      ggsave(paste0(name.prefix, "overall.pdf"), plot = overall_contri_2df, width = 12, height = 9)
      ggsave(paste0(name.prefix, "source-PM.pdf"), plot = PM_source_daily, width = 9, height = 7)
      
      nmContri_percent_bsDisp_output = nmContri_percent_bsDisp
      ts_nmContri_plot_output = ts_nmContri_plot
      lm_beta_plot_output = lm_beta_plot
      lm_beta_plot_output
      
      nmContri_percent_bsDisp_output$Cluster.No = ts_nmContri_plot_output$Cluster.No = lm_beta_plot_output$Cluster.No = cluster.No
      nmContri_percent_bsDisp_output$Factor.No = ts_nmContri_plot_output$Factor.No = lm_beta_plot_output$Factor.No = factor.No
      
      ts_annual_nmContri$Cluster.No = ts_month_nmContri$Cluster.No = cluster.No
      ts_annual_nmContri$Factor.No = ts_month_nmContri$Factor.No = factor.No

      write.csv(nmContri_percent_bsDisp_output, paste0(name.prefix, "source_profile.csv"))
      write.csv(ts_nmContri_plot_output, paste0(name.prefix, "daily.csv"))
      write.csv(ts_annual_nmContri, paste0(name.prefix, "annual.csv"))
      write.csv(ts_month_nmContri, paste0(name.prefix, "month.csv"))
      write.csv(lm_beta_plot_output, paste0(name.prefix, "overall.csv"))
      
      
      ggsave(paste0(name.prefix, "source_profile.pdf"), plot = conc_percent_source, width = 6, height = 7.5)
      
    }, error=function(e) {
      print(paste("Error at iteration", i, ":", e$message))
    })
  }
}


