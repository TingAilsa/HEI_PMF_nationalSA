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
library(purrr)
library(patchwork)
library(Metrics)

####### 1. Read & process other files to use ####### 

#### 1.1 CSN 25TimesMean noCsub #### 

##set working directory
#setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_25TimesMean_2024.01_selected/")
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_25TimesMean/base_DISPres1/")
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_25TimesMean/base_DISP_BS_sum/"

site_info_all = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_NoGUI_NoCsub_25TimesMean_Site/CSN_noCsub_25timesMean_PMF_CMD_StrongWeakBad_Site.csv")
species_class = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/Progress data/CSN_Species_class_sub.csv")
species_class$Species[nrow(species_class)] = "PM2.5"

site_info_all$X = species_class$X = NULL

noCsub_noExtreme = "CSN_NoGUI_NoCsub_25TimesMean_Site"
data.prefix = "CSN_noCsub_25TimesMean_"
pm.prefix = "CSN_noCsub_25TimesMean_"
disp.prefix = "CSN_"

#### 1.2 CSN 15TimesMean noCsub, overall uncertainty = 5% #### 

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15TimesMean/base_DISPres1/")
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15TimesMean/base_DISP_BS_sum/"

site_info_all = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_NoGUI_NoCsub_15TimesMean_Site/CSN_noCsub_15timesMean_PMF_SWB_site.csv")
species_class = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/Progress data/CSN_Species_class_sub.csv")
species_class$Species[nrow(species_class)] = "PM2.5"

site_info_all$X = species_class$X = NULL

noCsub_noExtreme = "CSN_NoGUI_NoCsub_15TimesMean_Site"
data.prefix = "CSN_noCsub_15TimesMean_"
pm.prefix = "CSN_noCsub_15TimesMean_"
disp.prefix = "CSN_"


#### 1.3 CSN 15TimesMean noCsub, overall uncertainty = 0% #### 

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15tMean_0unc/base_DISPres1/")
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15tMean_0unc/base_DISP_BS_sum/"

site_info_all = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_NoGUI_NoCsub_15TimesMean_Site/CSN_noCsub_15timesMean_PMF_SWB_site.csv")
species_class = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/Progress data/CSN_Species_class_sub.csv")
species_class$Species[nrow(species_class)] = "PM2.5"

site_info_all$X = species_class$X = NULL

noCsub_noExtreme = "CSN_NoGUI_NoCsub_15TimesMean_Site"
data.prefix = "CSN_noCsub_15tMean_0unc_"
pm.prefix = "CSN_noCsub_15TimesMean_"
disp.prefix = "CSN_"

#### 1.4 CSN 15t1mdl0unc noCsub, overall uncertainty = 0% #### 

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15t1mdl0unc/base_DISPres1/")
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15t1mdl0unc/base_DISP_BS_sum/"

site_info_all = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_NoGUI_NoCsub_15t1mdl0unc_Site/CSN_noCsub_15t1mdl0unc_PMF_SWB_site.csv")
species_class = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/Progress data/CSN_Species_class_sub.csv")
species_class$Species[nrow(species_class)] = "PM2.5"

site_info_all$X = species_class$X = NULL

noCsub_noExtreme = "CSN_NoGUI_NoCsub_15t1mdl0unc_Site"
data.prefix = "CSN_noCsub_15t1mdl0unc_"
pm.prefix = "CSN_noCsub_15t1mdl0unc_"
disp.prefix = "CSN_"

#### 1.N shared process #### 

site_info_all = plyr::rename(
  site_info_all, 
  c("K." = "KIon",
    "Na." = "NaIon", 
    "NH4." = "NH4Ion",
    "NH4+" = "NH4Ion",
    "NO3" = "NO3Ion",
    "SO4" = "SO4Ion",
    "PM25" = "PM2.5"))

source_cmd_pm = paste0("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/", 
                        noCsub_noExtreme)

########## single site list defination
### ONLY use when randomly select some sites for sensitivity analyses of the multi-site result
# site_info_all$cluster_site = 
#   paste0(site_info_all$Finaly.Decision, 
#          "_", 
#          site_info_all$SiteCode)
# 
# site_serial_Nos <- c("1_10732003", "1_391510017", "10_421010055", "11_270530963", 
#                    "12_450790007", "12_540390020", "13_540511002", "14_160010010", 
#                    "15_180890022", "16_420450109", "17_360551007", "18_371190041", 
#                    "19_550090005", "2_420950025", "20_201730010", "21_220330009", 
#                    "22_500070012", "23_360050110", "24_460990008", "24_490050007", 
#                    "25_420030064", "3_320030540", "4_320310031", "5_130210007", 
#                    "6_60731022", "7_120110034", "7_60670006", "8_380171004", 
#                    "9_530530031", "9_550790010")
### ONLY use when randomly select some sites for sensitivity analyses of the multi-site result


### for ALL sites
site_info_all =
  site_info_all[with(site_info_all, order(serial.No)), ]
site_info_all$serial.No =
  ifelse(site_info_all$serial.No < 100,
         sprintf("%03d", site_info_all$serial.No),
         as.character(site_info_all$serial.No))
site_serial_Nos = unique(site_info_all$serial.No)


########################################################
######## 2. Process the PMF non-GUI outputs & Plot ####### 
########################################################

correl_r_p_summary = NULL
summary_base = NULL
pred_obs_compare_summary = NULL

for (site.serial in site_serial_Nos) { # 1:25
  for (factor.No in 3:11) { # 5:11
    
    # site.serial.factor.pre = paste0("C_", site.serial, "_F_", factor.No, "_")
    site.serial.factor.pre = paste0("S_", site.serial, "_F_", factor.No, "_")
    name.prefix = paste0(data.prefix, site.serial.factor.pre)
    
    tryCatch({
      ####### Read the results from bash script ####### 
      # Access the input file name passed as an argument
      folder_path <- paste0("S_", site.serial, "/Factor_", factor.No, "/") # "C_"
      base_output = readLines(paste0(folder_path, 
                                     data.prefix, 
                                     site.serial.factor.pre,
                                     "base.txt")) # "_base.txt"
      disp_output = readLines(paste0(folder_path, 
                                     disp.prefix, 
                                     site.serial.factor.pre,
                                     "DISPres1.txt"))
      bs_output = readLines(paste0(folder_path, 
                                   data.prefix, 
                                   site.serial.factor.pre,
                                   "BS_.txt"))
      base_report = readLines(paste0(folder_path, 
                                     data.prefix, 
                                     site.serial.factor.pre,
                                     "base_PMFreport.txt"))
      
      # Bootstrap mapping rate
      bs_map_fra = bs_map(bs_output, 50, factor.No, 0.6) # 0.6 is the threshold or r
      bs_map_fraction = bs_map_fra$percent_less_than_0_6
      bs_overall_map = bs_map_fra$BS_overall
      
      # Find the number of task when the value of Qm is the lowest
      lowest_Qm_taskNo = lowest_Qm_task(base_report)$lowest_Qm_task
      lowest_Qm = round(lowest_Qm_task(base_report)$lowest_Qm, 0)
      # converge condition in the 20 base runs
      converge_percent = lowest_Qm_task(base_report)$converge_percent
      
      Factor.serial = paste0("Factor", 1:factor.No)
      
      ####### Cluster-specific species strong, weak, bad #######
      site_info = subset(site_info_all, 
                            serial.No == site.serial)
      site.data.row = site_info$site.row
      site_code = site_info$SiteCode
      strong.species.count = length(strong_species(site_info, "Al", "PM2.5"))
      
      # detect the column range for PM species & PM2.5
      col_comp_all = col_comp(site_info, "Al", "PM2.5")
      
      ## Select weak & strong variables by the value
      strong_weak_assign = strong_weak(site_info, "Al", "PM2.5")
      strong_weak_df = strong_weak_assign$strong_weak_col
      site.weak.strong = strong_weak_assign$strong_weak_species
      species.weak.strong.count = length(site.weak.strong) - 1
      
      Q.exp = 
        site.data.row*strong.species.count - 
        ((factor.No*site.data.row) + 
           (factor.No*strong.species.count))
        
      ####### Extract base info from PMF CMD outputs & match with date, PM #######
      
      # extract df from base outputs
      base_info = base_results(base_output, 
                               lowest_Qm_taskNo, 
                               site.data.row)
      
      # base_G_cor = base_info$base_G_cor
      base_ts = base_info$base_ts
      base_contri = base_info$base_contri
      base_ts_conc = base_info$base_ts_conc
      base_conc_fraction = base_info$base_fraction
      base_conc_fraction = as.data.frame(t(base_conc_fraction))
      predict_daily_species_conc = base_info$predict_daily_species_conc
      
      colnames(base_conc_fraction)[1] = "Fractrion_conc_based"
      base_conc_fraction$Faraction_conc_contri = 
        paste0(round(base_conc_fraction$Fractrion_conc_based*100, 1),
               "%")
      base_conc_fraction$Factor = rownames(base_conc_fraction)
      
      # assign species names 
      base_contri$Species = site.weak.strong
      colnames(predict_daily_species_conc)[-1] = 
        site.weak.strong
      
      # get concentration fraction contribution of each species
      base_percent = conc_percent_contri(base_contri)
      
      base_conc_plot = gather(base_contri,
                              "Factor", 
                              "Concentration", 
                              -Species)
      
      base_percent_plot = gather(base_percent,
                                 "Factor", 
                                 "Percent", 
                                 -Species)
      
      # Extract site & date info
      # site_date_PM = read.csv(
      #   file.path(
      #     paste0(source_cmd_pm, "_SiteDate"), # "_SiteDate_2024.01"
      #     paste0(pm.prefix, "S_", site.serial, "_PM_Date.csv")), 
      #   header = T)

      site_date_PM_species = read.csv(
        file.path(
          source_cmd_pm,
          paste0(pm.prefix, "S_", site.serial, "_CMD.csv")), #"_CMD.csv", ".csv"
        header = T)
      site_date_PM_species$X = NULL
      
      start.date = site_date_PM_species$Date[1]
      end.date = site_date_PM_species$Date[nrow(site_date_PM_species)]
      
      site_date_PM_species_conc <- 
        site_date_PM_species %>%
        # remove columns with "unc_" in their names
        select(-contains("unc_")) %>%
        # rename columns that start with "conc_"
        rename_with(~gsub("^conc_", "", .x), starts_with("conc_"))

      site_date_PM_species_unc <- 
        site_date_PM_species %>%
        # remove columns with "conc_" in their names
        select(-contains("conc_")) %>%
        # rename columns that start with "unc_"
        rename_with(~gsub("^unc_", "", .x), starts_with("unc_"))
      
      site_date_PM_species_conc = plyr::rename(
        site_date_PM_species_conc, 
        c("K." = "KIon",
          "Na." = "NaIon", 
          "NH4." = "NH4Ion",
          "NO3" = "NO3Ion",
          "SO4" = "SO4Ion",
          "PM25" = "PM2.5"))
      
      site_date_PM_species_unc = plyr::rename(
        site_date_PM_species_unc, 
        c("K." = "KIon",
          "Na." = "NaIon", 
          "NH4." = "NH4Ion",
          "NO3" = "NO3Ion",
          "SO4" = "SO4Ion",
          "PM25" = "PM2.5"))
        
      #### estimate the uncertainty scaled residual
      daily_species_scale_residual = 
        (site_date_PM_species_conc[, site.weak.strong] - 
            predict_daily_species_conc[, site.weak.strong]) /
           site_date_PM_species_unc[, site.weak.strong]

      # remove PM2.5 data
      daily_species_scale_residual_noPM = 
        daily_species_scale_residual[-ncol(daily_species_scale_residual)]
      
      # Q_true & Q_robust
      daily_species_scale_residual_noPM_robust = daily_species_scale_residual_noPM
      daily_species_scale_residual_noPM_robust[abs(daily_species_scale_residual_noPM_robust) > 4] <- 0
      
      Q.true = round(sum(daily_species_scale_residual_noPM^2), 0)
      Q.robust = round(sum(daily_species_scale_residual_noPM_robust^2), 0)
      
      # Daily Q/Qexp ratio
      daily_Q_Qexp = 
        data.frame(site_date_PM_species_conc$Date, 
                   rowSums(daily_species_scale_residual_noPM^2)/species.weak.strong.count)
      colnames(daily_Q_Qexp) = c("Date", "Q_Qexp_ratio")
      daily_Q_Qexp$higher.3.scalRes.fraction = 
        rowSums(abs(daily_species_scale_residual_noPM) > 3) / species.weak.strong.count
      daily_Q_Qexp$higher.3.scalRes.per = 
        paste0(round(daily_Q_Qexp$higher.3.scalRes.fraction * 100, 2), "%")
      
      # Species-specific Q/Qexp ratio
      species_Q_Qexp = data.frame(colSums(daily_species_scale_residual^2)/(Q.exp/strong.species.count))
      species_Q_Qexp$Species = row.names(species_Q_Qexp)
      colnames(species_Q_Qexp)[1] = "Q_Qexp_ratio"
      species_Q_Qexp$higher.3.scalRes.fraction = 
        colSums(abs(daily_species_scale_residual) > 3) / site.data.row
      species_Q_Qexp$higher.3.scalRes.per = 
        paste0(round(species_Q_Qexp$higher.3.scalRes.fraction * 100, 2), "%")
      
      # normalized and overall contributions based on OLS MLR
      site_date_PM = select(site_date_PM_species_conc,
                            Date, PM2.5) # SiteCode, State, 
      time_series_analyses = time_series(base_ts, site_date_PM, factor.No)
      # base_ts_nmContri_gather = time_series_analyses$base_ts_nmContri_gather
      base_ts_nmContri_spread = time_series_analyses$base_ts_nmContri_spread
      ts_PM_lm_beta = time_series_analyses$ts_PM_lm_beta
      cor_PM_sumSpecies = time_series_analyses$cor_PM_sumSpecies
      # base_ts_nmContri_all = time_series_analyses$base_ts_nmContri_all
      
      base_ts_conc_all = cbind(base_ts_conc, site_date_PM)
      base_ts_conc_all$Date = as.Date(base_ts_conc_all$Date)
      base_ts_conc_all$SerialNumber = NULL
      # colnames(base_ts_conc_all)[ncol(base_ts_conc_all)] = "PM2.5"
      
      # estimate the PMF explained PM2.5 and the observations
      ts_conc_2PM = base_ts_conc_all
      ts_conc_2PM$PMF_PM2.5 = rowSums(ts_conc_2PM[, Factor.serial], na.rm = TRUE)
      median_PMF_PM2.5 = median(ts_conc_2PM$PMF_PM2.5)
      median_obs_PM2.5 = median(ts_conc_2PM$PM2.5)
      cor.PMF.obs.PM = cor(ts_conc_2PM$PMF_PM2.5, ts_conc_2PM$PM2.5)
      
      #### time-series concentration contribution estimation
      
      # pivot data from wide to long
      base_ts_conc_long <-
        base_ts_conc_all %>% 
        pivot_longer(
          cols = Factor.serial[1]:Factor.serial[factor.No],
          names_to = c("Factor"),
          # names_pattern = "new_?(.*)_(.)(.*)",
          values_to = "Concentration"
        )
      
      # Get annual and seasonal contributions
      base_ts_conc_long <- 
        base_ts_conc_long %>%
        mutate(Year = year(Date),
               Season = case_when(
                 month(Date) %in% c(12, 1, 2) ~ "Winter",
                 month(Date) %in% c(3, 4, 5) ~ "Spring",
                 month(Date) %in% c(6, 7, 8) ~ "Summer",
                 month(Date) %in% c(9, 10, 11) ~ "Fall"
               ),
               Month = month(Date))
      
      ts_annual_conc = ddply(base_ts_conc_long, 
                             .(Year, Factor), # SiteCode, 
                             summarise,
                             Concentration = median(Concentration))
      
      ts_season_conc = ddply(base_ts_conc_long, 
                             .(Season, Factor), #SiteCode, 
                             summarise,
                             Concentration = median(Concentration))
      
      ts_month_conc = ddply(base_ts_conc_long, 
                            .(Year, Month, Factor), #SiteCode,
                            summarise,
                            Concentration = median(Concentration))
      
      ####### Extract DISP info from PMF CMD, and validation results #######
      
      # DISP results - Credible intervals
      disp_down_conc = disp_analysis(disp_output)$disp_down
      disp_up_conc = disp_analysis(disp_output)$disp_up
      disp_down_conc$Species = disp_up_conc$Species = site.weak.strong
      
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
      base_ts_nmContri_spread$SiteCode = site_code
      base_ts_nmContri_spread = site_color(base_ts_nmContri_spread)
      
      # merge datasets for source profile plotting
      base_disp_conc_to_merge <- 
        list(base_conc_plot, base_percent_plot, 
             disp_down_percent_plot, disp_up_percent_plot, 
             species_class, main_source)
      
      conc_percent_bsDisp <- 
        Reduce(function(x, y) 
          merge(x, y), 
          base_disp_conc_to_merge)
      
      # match time-series normalized contribution data with source info
      ts_conc_plot = merge(base_ts_conc_long, main_source)
      ts_annual_conc = merge(ts_annual_conc, main_source)
      ts_season_conc = merge(ts_season_conc, main_source)
      ts_month_conc = merge(ts_month_conc, main_source)
      lm_beta_plot = merge(ts_PM_lm_beta, main_source)
      base_ts_conc_long_plot = merge(base_ts_conc_long, main_source)
      
      # reorder the file 
      ts_conc_plot = 
        ts_conc_plot[with(ts_conc_plot,
                          order(Date, Factor)), ]
      
      # match overall contribution based on normalized data with that from mass concentration
      lm_beta_plot = merge(lm_beta_plot, base_conc_fraction)
      lm_beta_plot$Fractrion_conc_based = 100 * lm_beta_plot$Fractrion_conc_based
      
      cor.two.fraction.contri = cor.test(lm_beta_plot$Factor.contribution,
                                         lm_beta_plot$Fractrion_conc_based,
                                         method = "pearson")$estimate
      
      ####### Number of Factor & Factor_source #######
      Factor_source_count = 
        ddply(ts_annual_conc, 
              .(Factor_source, Factor), 
              summarise,
              Concentration = median(Concentration))
      
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
      
      ##########################################
      ####### PLOT #######
      ##########################################
      
      #### Source Profile - Concentration & percent Normalize_contri #### 
      conc_percent_bsDisp_use = conc_percent_bsDisp
      conc_percent_bsDisp_use = merge(conc_percent_bsDisp_use, lm_beta_plot)
      # conc_percent_bsDisp_use = subset(conc_percent_bsDisp, Source.No != "F")
      
      # Convert 0 to 1e-10 for columns to be used for y-axis, there is log transfer later
      conc_percent_bsDisp_use <- 
        conc_percent_bsDisp_use %>%
        mutate(across(Concentration:Percent.up, 
                      ~replace(., . == 0, 1e-10)))
      
      # Convert percent values to make the scale pattern similar to log Normalize_contri
      # conc_percent_bsDisp_use$LogConcentration <- log10(conc_percent_bsDisp_use$Concentration)
      conc_percent_bsDisp_use$Trans.Percent = 
        trans_log(conc_percent_bsDisp_use$Percent, log(1e-00), log(1e-05))
      conc_percent_bsDisp_use$Trans.Percent.down = 
        trans_log(conc_percent_bsDisp_use$Percent.down, log(1e-00), log(1e-05))
      conc_percent_bsDisp_use$Trans.Percent.up = 
        trans_log(conc_percent_bsDisp_use$Percent.up, log(1e-00), log(1e-05))
      
      #### Step trying to identify the trans for sec.axis
      #numbers = 60
      #trans.num = log(numbers + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)
      #trans.num = trans_log(numbers, log(1e-05))
      #y.values = exp(trans.num)
      #trans.y = log(y.values + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)
      #trans.y = trans_log(y.values, log(1e-05))
      
      #log(exp(log(c(1e-10, 10, 20, 50, 100) + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)) + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)
      #log(exp(log(c(min(conc_percent_bsDisp_use$Percent), max(conc_percent_bsDisp_use$Percent)) + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)) + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)
      
      
      # Correcting any inverted values
      inverted_rows <- conc_percent_bsDisp_use$Trans.Percent.up < conc_percent_bsDisp_use$Trans.Percent.down
      # Swapping the values using a temporary variable
      temp <- conc_percent_bsDisp_use$Trans.Percent.up[inverted_rows]
      conc_percent_bsDisp_use$Trans.Percent.up[inverted_rows] <- conc_percent_bsDisp_use$Trans.Percent.down[inverted_rows]
      conc_percent_bsDisp_use$Trans.Percent.down[inverted_rows] <- temp
      
      # find the middle position of the factor/source names on x-axis
      middle_positions <- 
        conc_percent_bsDisp_use %>%
        dplyr::group_by(Factor) %>%
        dplyr::summarize(Factor_source = Factor_source[1],
                         middle = custom_median(as.numeric(sequence)),
                         Factor_nm_contr = paste(unique(Factor.contr), 
                                                 collapse = ", "),
                         Factor_conc_fr = paste(unique(Faraction_conc_contri), 
                                                collapse = ", ")) %>%
        dplyr::arrange(Factor)
      
      middle_species = 
        conc_percent_bsDisp_use$Species[
          conc_percent_bsDisp_use$sequence == 
            middle_positions$middle[1]]
      
      middle_positions$Species = middle_species[1]
      
      ### add BS results
      bs_map_fraction_source = 
        merge(bs_map_fraction, 
              select(lm_beta_plot, 
                     Factor, Factor_source))
      middle_positions = merge(middle_positions, bs_map_fraction_source)
      
      ### Mark on figure
      # middle_positions$Factor_source_contr = 
      #   paste0(middle_positions$Factor_source, ", ", 
      #          "nm_contri ", middle_positions$Factor_nm_contr, ", ",
      #          "conc_fr ", middle_positions$Factor_conc_fr)
      middle_positions$Factor_source_contr = 
        paste0(middle_positions$Factor_source, ", ", 
               "conc_fr ", middle_positions$Factor_conc_fr,
               ", BS_map ", middle_positions$BS_map_fra)

      # Set the breaks
      y_breaks = c(1, 6, 16, 41, 101)
      trans_y_breaks = trans_log(y_breaks, log(1e-00), log(1e-05))
      log_breaks = trans_log(exp(trans_y_breaks), log(1e-00), log(1e-05))

      # Create the plot
      source_profile <- 
        ggplot(conc_percent_bsDisp_use,
               aes(x = reorder(Species, sequence), 
                   group = Factor_source)) +
        # Bar plot for Concentration
        geom_bar(aes(y = Concentration, fill = Factor_source), 
                 stat = "identity", width = 0.6, alpha = 0.8) +
        # Point plot for transformed Percent
        geom_point(aes(y = exp(Trans.Percent)), 
                   color = "grey25", shape = 15, size = 1) +
        geom_errorbar(aes(ymin = exp(Trans.Percent.down), 
                          ymax = exp(Trans.Percent.up)), 
                      width = 0.3, color = "grey25") +
        facet_grid(Factor ~ ., switch = "y") +
        ggtitle(paste0(paste0(disp.prefix, site.serial.factor.pre), # "\n",
                       ", Error.Code = ", disp.error.code, 
                       ", DISP.Qdrop = ", disp.qdrop, 
                       ", BS.map = ", paste0(bs_overall_map*100, "%"), "\n",
                       "Estimated Q.true = ", Q.true,
                       ", Q.robust = ", Q.robust,
                       ", nonGUI.Qmin = ", lowest_Qm)) + 
        scale_y_continuous(
          name = format_variable("Concentration µg/m3"),
          # limits = c(1e-05, max(conc_percent_bsDisp$Concentration)),
          breaks = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01, 1e-00),
          labels = c(expression(10^"-5"), expression(10^"-4"), 
                     expression(10^"-3"), expression(10^"-2"), 
                     expression(10^"-1"), expression(10^"0")), # label_scientific(),
          sec.axis = sec_axis(
            # trans = ~ log(. + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05), # when uplimit is log(1e-01)
            trans = ~ (log(. + 1)) / log(101) * (log(1e-01) - log(1e-05)) + log(1e-05),
            name = "% of Species",
            breaks = log_breaks,
            labels = c(0, 5, 15, 40, 100)
          ),
          trans = mylog_trans(base=10, from=-5),
          limits = c(1E-5, max(conc_percent_bsDisp$Concentration))
        ) +
        scale_fill_npg() +
        xlab(format_variable("PM25 Species")) +
        ylab(format_variable("Concentration µg/m3")) +
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
      
      # source_profile
      
      
      #### Time-series factor percent Normalize_contri #### 
      
      #### Daily
      # daily_plot_use = subset(ts_conc_plot, Source.No != "F")
      daily_plot_use = ts_conc_plot
      
      # set gap in figure if no data for >15 days
      daily_plot_use <- 
        daily_plot_use %>%
        arrange(Date) %>%
        mutate(gap = c(0, diff(as.numeric(Date))) > 15, # calculate gaps, convert Date to numeric for diff
               group = cumsum(gap)) # Cumulative sum to create a new group after each gap
      
      # define the x-y position of ggtext
      middle_positions$middle_day = median(daily_plot_use$Date)
      middle_positions$middle_month = month(median(daily_plot_use$Date))
      middle_positions$middle_year = year(median(daily_plot_use$Date))
      
      text_y_position =
        ddply(daily_plot_use, 
              .(Factor),
              summarise,
              text_y_day = quantile(Concentration, 0.997),
              text_y_mon = quantile(Concentration, 0.55),
              text_y_yr = quantile(Concentration, 0.55))
      
      middle_positions_ts = 
        merge(middle_positions, text_y_position)
      
      daily_conc_plot <- 
        ggplot(daily_plot_use, 
               aes(x = Date, y = Concentration, 
                   group = Factor_source, color = Factor_source)) + # group = 1
        geom_line(aes(group = group), linewidth = 0.3, alpha = 0.6) +
        geom_point(size = 0.7) +
        labs(x = "Date", y = format_variable("Concentration µg/m3")) +
        facet_grid(Factor ~., scales = "free_y") +
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 3)) + # y start from 0, and number of breaks, pretty function
        scale_color_npg() +
        geom_text(data = middle_positions_ts, size = 4,
                  aes(x = middle_day, y = text_y_day, label = Factor_source_contr), 
                  inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        theme_base() +
        theme(
          panel.grid = element_line(colour = "white"),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          legend.position = "none"
        )
      
      # daily_conc_plot
      
      #### Annual
      # annual_plot_use = subset(ts_annual_conc, Source.No != "F")
      annual_plot_use = ts_annual_conc
      
      annual_conc_plot <- 
        #  ggplot(subset(annual_plot_use,
        #                SiteCode = unique(annual_plot_use$SiteCode)[2]), 
        ggplot(annual_plot_use,
               aes(x = Year, y = Concentration, 
                   group = Factor_source, color = Factor_source)) +
        labs(x = "Year", y = format_variable("Concentration µg/m3")) +
        facet_grid(Factor ~., scales = "free_y") +
        geom_line(linewidth = 0.3, alpha = 0.4)+
        geom_point(size = 1.5, alpha = 0.8) +
        scale_x_continuous(breaks = 
                             pretty(annual_plot_use$Year, # pretty{base}
                                    n = length(unique(annual_plot_use$Year)), 
                                    min.n = 1)) +
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 3)) + 
        geom_text(data = middle_positions_ts, size = 4,
                  aes(x = middle_year, y = text_y_yr, label = Factor_source_contr), 
                  inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        scale_color_npg() +
        theme_base() +
        theme(
          panel.grid = element_line(colour = "white"),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          legend.position = "none"
        )
      
      # annual_conc_plot
      
      #### Month
      # month_plot_use = subset(ts_month_conc, Source.No != "F")
      month_plot_use = ts_month_conc
      month_plot_use =  ddply(month_plot_use, 
                              .(Month, Factor, Main_Species, # SiteCode, 
                                Source_reference, Source.No, Factor_source),
                              summarise,
                              Concentration = median(Concentration))
      
      month_conc_plot <- 
        #  ggplot(subset(month_plot_use,
        #                SiteCode = unique(month_plot_use$SiteCode)[2]), 
        ggplot(month_plot_use,
               aes(x = Month, y = Concentration, 
                   group = Factor_source, color = Factor_source)) +
        labs(x = "Month", y = format_variable("Concentration µg/m3")) +
        facet_grid(Factor ~., scales = "free_y") +
        geom_line(linewidth = 0.3, alpha = 0.4)+
        geom_point(size = 1.5, alpha = 0.8) +
        scale_x_continuous(breaks = 
                             pretty(month_plot_use$Month, # pretty{base}
                                    n = length(unique(month_plot_use$Month)), 
                                    min.n = 1)) +
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 3)) + 
        geom_text(data = middle_positions_ts, size = 4,
                  aes(x = middle_month, y = text_y_mon, label = Factor_source_contr), 
                  inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        scale_color_npg() +
        theme_base() +
        theme(
          panel.grid = element_line(colour = "white"),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          legend.position = "none"
        )
      
      # month_conc_plot
      
      #### year_month
      # get year & month
      base_ts_conc_ym_conc = ts_conc_plot
      base_ts_conc_ym_conc$Year = year(base_ts_conc_ym_conc$Date)
      base_ts_conc_ym_conc$Month = month(base_ts_conc_ym_conc$Date)
      # check the output
      # select(subset(base_ts_conc_ym_conc, 
      #               Factor == "Factor2" & Year == 2016 & Month == 2),
      #        Date, Concentration, Year, Month, Factor_source)
      
      # estimate the concentration by year-month group
      base_ts_conc_ym_conc = 
        base_ts_conc_ym_conc %>%
        dplyr::group_by(Factor, Year, Month) %>%
        dplyr::summarise(
          conc_med = quantile(Concentration, 0.5),  
          conc_up = quantile(Concentration, 0.95), 
          conc_down = quantile(Concentration, 0.05), 
          # drops the grouping so the result is ungrouped
          .groups = 'drop')
      
      # get the Year_Month date
      base_ts_conc_ym_conc = 
        base_ts_conc_ym_conc %>%
        mutate(Year_Month = 
                 as.Date(
                   paste(Year, Month, "01", sep = "-")))
      head(base_ts_conc_ym_conc)
      
      # plot the change
      ym_conc_plot <-
        ggplot(base_ts_conc_ym_conc, 
               aes(x = Year_Month, 
                   y = conc_med, 
                   group = Factor, color = Factor)) + # group = 1
        geom_errorbar(aes(ymin = conc_down, ymax = conc_up),
                      width = 0.15) +
        geom_point(size = 1.2) +
        # the preset format_variable function, for sub-/super- scripts
        labs(x = "Date", y = format_variable("Concentration µg/m3")) +
        facet_grid(Factor ~., scales = "free_y") +
        # pretty function, y start from 0, and 3 breaks
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 3)) + 
        # Setting breaks every Month  
        # scale_x_date(date_labels = "%Y-%m", date_breaks = "3 Month") + 
        scale_color_npg() + # ggsci
        theme_base() +
        theme(
          panel.grid = element_line(colour = "white"),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "none"
        )
      
      # ym_conc_plot

      #### Overall factor percent Normalize_contri #### 
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
      
      ### fraction contribution estimated based on lm and concentration contribution
      # overall_contri_2df <-
      #   ggplot(contri_fraction, 
      #          aes(x = Factor, y = Factor.contribution)) +
      #   facet_grid(Fraction_data ~. , 
      #              switch = "y", scales = "free_y") + 
      #   geom_bar_pattern(aes(fill = Factor),
      #                    stat = "identity",
      #                    pattern_color = "white",
      #                    pattern_fill = "white",
      #                    # alpha = 0.8,
      #                    width = 0.3)  +
      #   geom_text(aes(label = paste(Factor_source, Factor.contr)), 
      #             size = 6, angle = 90, hjust = 0, vjust = -3,
      #             position = position_stack(vjust = 0)) + # start from same bottom level
      #   scale_fill_npg() +
      #   scale_y_continuous(position = "right") +
      #   # labs(y = "Fraction (%)") +
      #   theme_bw() +
      #   theme(panel.grid.major.x = element_line(colour="grey60", linetype="dashed"),
      #         panel.grid.minor.x = element_blank(),
      #         panel.grid.major.y = element_blank(),
      #         strip.background = element_blank(), 
      #         strip.text = element_text(size = 20),
      #         legend.position = "none",
      #         axis.text.x = element_text(color="grey25", size = 16, angle = 90, hjust = 0.5, vjust = 0.5), 
      #         axis.text.y = element_text(color="grey25", size = 16, angle = 90, hjust = 5, vjust = -0.5),
      #         axis.title.x = element_text(color="grey25", size = 22, angle = 180, hjust = 0.5),
      #         axis.title.y = element_text(color = "grey25", size = 0, angle = 27, vjust = 3, hjust = 1.1))
      
      overall_contri <-
        ggplot(lm_beta_plot_use, 
               aes(x = Factor, y = Fractrion_conc_based)) +
        geom_bar_pattern(aes(fill = Factor_source),
                         stat = "identity",
                         pattern_color = "white",
                         pattern_fill = "white",
                         # alpha = 0.8,
                         width = 0.3)  +
        geom_text(aes(label = paste(Factor_source, Faraction_conc_contri)), 
                  size = 6, angle = 90, hjust = 0, vjust = -3,
                  position = position_stack(vjust = 0)) + # start from same bottom level
        scale_fill_npg() +
        scale_y_continuous(position = "right") +
        theme_base() +
        theme(panel.grid.major.x = element_line(colour="grey60", linetype="dashed"),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              legend.position = "none",
              axis.text.x = element_text(color="grey25", size = 16, angle = 90, hjust = 0.5, vjust = 0.5), 
              axis.text.y = element_text(color="grey25", size = 16, angle = 90, hjust = 5, vjust = -0.5),
              axis.title.x = element_text(color="grey25", size = 22, angle = 180, hjust = 0.5),
              axis.title.y = element_text(color="grey25", size = 0, angle = -90, vjust = -1))
      
      ####### G-space plot, pairs: between-factor scattor & correlation & summary form #######
      
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
      correl_r_p$site.serial = site.serial
      
      correl_r_p$factor.No = factor.No
      correl_r_p$Dataset = disp.prefix
      
      correl_r_percentile <- quantile(as.numeric(correl_r_p$Correlation), 
                                      probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))
      
      correl_r_p_summary = rbind(correl_r_p_summary, correl_r_p)
      
      # Pair plotting
      pdf(paste0(name.prefix, "G-space.pdf"))
      
      # par(oma = c(4, 4, 6, 10), mar = c(4, 4, 2, 2)) 
      pairs(base_ts_nmContri_spread[, 1:factor.No], 
            upper.panel = panel.scatter,
            lower.panel = panel.corr,
            main = name.prefix)
      
      dev.off()
      
      
      ####### Daily Specific-specific concentration - prediction vs. observation #######
      
      predict_daily_species_conc$Date = site_date_PM_species_conc$Date
      predict_daily_species_conc = 
        predict_daily_species_conc %>%
        relocate(Date, .before = Serial.No) %>%
        select(-Serial.No)
      
      predict_daily_species_long =
        predict_daily_species_conc %>% 
        pivot_longer(
          cols = Al:PM2.5,
          names_to = c("Species"),
          values_to = "Concentration"
        )
      predict_daily_species_long$Data = "PMF_Predictions"
      
      obs_daily_species_long =
        site_date_PM_species_conc %>% 
        pivot_longer(
          cols = Al:PM2.5,
          names_to = c("Species"),
          values_to = "Concentration"
        )
      obs_daily_species_long$Data = "Observations"
      
      obs_daily_species_long$SiteCode = obs_daily_species_long$State = NULL
      # summary(predict_daily_species_long$Date == obs_daily_species_long$Date) 
      # summary(predict_daily_species_long$Species == obs_daily_species_long$Species)
      
      ### RMSE estimation
      
      rmse <- function(actual, predicted) {
        sqrt(mean((predicted - actual) ^ 2))
      }
      
      pred_obs_species_wide =
        cbind(select(predict_daily_species_long, -Data), 
              select(obs_daily_species_long, -c("Data", "Date", "Species")))
      names(pred_obs_species_wide)[3:4] = 
        c("conc_predict", "conc_obs")
      
      pred_obs_compare  =
        pred_obs_species_wide %>%
        dplyr::group_by(Species) %>%
        dplyr::summarise(
          RMSE = rmse(conc_predict, conc_obs),
          R2 = 1 - sum((conc_obs - conc_predict)^2) / 
            sum((conc_obs - mean(conc_obs))^2),
          cor_pearson = cor(conc_obs, conc_predict, method = "pearson"),
          cor_spearman = cor(conc_obs, conc_predict, method = "spearman"),
          mean_obs = mean(conc_obs), 
          sd_obs = sd(conc_obs), 
          mean_pmf = mean(conc_predict), 
          sd_pmf = sd(conc_predict))
      
      # extract info for PM2.5
      RMSE_PM2.5 = pred_obs_compare$RMSE[pred_obs_compare$Species == "PM2.5"]
      R2_PM2.5 = pred_obs_compare$R2[pred_obs_compare$Species == "PM2.5"]
      
      ### time-series plotting
      pred_obs_species_conc = 
        rbind(predict_daily_species_long, obs_daily_species_long)
      pred_obs_species_conc$Date = as.Date(pred_obs_species_conc$Date)
      
      ### time-series
      # set gap in figure if no data for >7 days
      pred_obs_species_conc <- 
        pred_obs_species_conc %>%
        arrange(Date) %>%
        mutate(gap = c(0, diff(as.numeric(Date))) > 7, # calculate gaps, convert Date to numeric for diff
               group = cumsum(gap)) # Cumulative sum to create a new group after each gap

      pred_obs_species_position <-
        ddply(pred_obs_species_conc, .(Species),
              summarise,
              y_position = quantile(Concentration, 0.999),
              x_position = median(Date))
      
      pred_obs_species_med <-
        ddply(subset(pred_obs_species_conc,
                     Data == "Observations"), 
              .(Species),
              summarise,
              median_conc = signif(median(Concentration), 2),
              mean_conc = signif(mean(Concentration), 2))
      pred_obs_species_med = 
        merge(pred_obs_species_med, pred_obs_compare)
      pred_obs_species_med$Species_conc =
        paste0(pred_obs_species_med$Species,
               ": Obs-median=", pred_obs_species_med$median_conc,
               ", RMSE=", signif(pred_obs_species_med$RMSE, 2),
               ", R^2=", signif(pred_obs_species_med$R2, 2))
      
      pred_obs_species_position = 
        join(pred_obs_species_position, pred_obs_species_med)
      
      pred_obs_species_conc_plot <-
        ggplot(pred_obs_species_conc, 
               aes(x = Date, y = Concentration, 
                   group = group, color = Data)) + # group = 1
        geom_line(linewidth = 0.2, alpha = 0.6) +
        geom_point(size = 0.7) +
        labs(x = "Date", y = format_variable("Concentration µg/m3")) +
        facet_wrap(Species ~., scales = "free_y", ncol = 3) + # ncol, only for facet_wrap, not facet_grid
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 3)) +  # reduce the number of breaks
        scale_color_manual(values = c("#377eb8", "#ff7f00")) + # c("steelblue", "brown2"), c("#377eb8", "#ff7f00")
        geom_text(data = pred_obs_species_position, size = 3.8,
                  aes(x = x_position, y = y_position, label = Species_conc), 
                  inherit.aes = FALSE) + 
        theme_base() +
        theme(
          panel.grid = element_line(colour = "white"),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          legend.position = c(0.9, 0.02), legend.background = element_blank(),
          legend.title = element_text(size = 0),
          axis.text = element_text(size = 11, color = "grey25")
        )
      
      
    ##### predictions vs. observations, scatter
    species_conc_pred_vs_obs = 
      pred_obs_species_conc %>%
        pivot_wider(
          names_from = Data, 
          values_from = Concentration)
      
    species_pred_vs_obs_position =
      ddply(species_conc_pred_vs_obs,
            .(Species),
            summarise,
            x_position = max(Observations)*0.9,
            y_position = max(PMF_Predictions)*0.9)
    
    species_pred_vs_obs_plot <-
      ggplot(species_conc_pred_vs_obs,
             aes(x = Observations,
                 y = PMF_Predictions)) +
      geom_point(size = 1, color = "grey35", alpha = 0.8) +
      geom_abline(slope=1, intercept=0, color = "red") +
      facet_wrap(~ Species, scales = "free", ncol = 5) +
      geom_text(data = species_pred_vs_obs_position, size = 4,
                aes(x = x_position, y = y_position, label = Species), 
                inherit.aes = FALSE) + 
      labs(x = format_variable("Observations µg/m3"), 
           y = format_variable("PMF_Predictions µg/m3")) +
      theme_base() +
      theme(strip.background = element_blank(), strip.text = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 0.5, size = 12),
            axis.text.y = element_text(size = 12))
    # species_pred_vs_obs_plot
    
      ####### Daily Source-specific PM2.5 - prediction vs. observation #######
      
      # calculate correlation for each Factor_source
      corr_labels_site_PM <- 
        base_ts_conc_long_plot %>%
        group_by(Factor_source) %>%
        nest() %>%
        dplyr::mutate(
          label = 
            map_chr(data, 
                    ~calculate_corr_label(
                      .x, 
                      "PM2.5", 
                      "Concentration"))) %>%
        select(-data)
      
      corr_labels_site_PM = 
        subset(corr_labels_site_PM,
               !(grepl("Factor", Factor_source, fixed = T)))
      
      base_ts_conc_long_plot = 
        merge(base_ts_conc_long_plot, corr_labels_site_PM)
      
      PM_source_daily<-
        ggplot(base_ts_conc_long_plot,
               aes(x = PM2.5,
                   y = Concentration)) +
        geom_point() +
        geom_abline(slope=1, intercept=0, color = "red") +
        facet_wrap(~ Factor_source, ncol = 3) +
        geom_text(aes(label = label), 
                  x = Inf, y = Inf, 
                  hjust = 2, vjust = 2, 
                  check_overlap = TRUE) +
        # labs(x = "Percent Contribution  %", y = "Count") +
        labs(x = format_variable("Daily PM25 µg/m3"), 
             y = format_variable("Concentration µg/m3")) +
        theme_bw() +
        theme(legend.position = "none",
              strip.background = element_blank(), 
              strip.text = element_text(color="grey25", size = 15, vjust=0), # facet title
              axis.title.x = element_text(color="grey25", size = 18, vjust=0, family = "Arial Unicode MS"), 
              axis.title.y = element_text(color="grey25", size = 18, vjust=1, family = "Arial Unicode MS"),
              plot.margin = unit(c(2,1,2, 2), "lines"),
              axis.text.x = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5, vjust = 0.5), 
              axis.text.y = element_text(color="grey25", size = 15, angle = 0, hjust = 0.5))
      # PM_source_daily
      
      ####### Daily & species-specific scaled residuals #######
      
      daily_species_scale_residual_long =
        data.frame(Date = site_date_PM_species_conc$Date, daily_species_scale_residual) %>% 
        pivot_longer(
          cols = Al:PM2.5,
          names_to = c("Species"),
          # names_pattern = "new_?(.*)_(.)(.*)",
          values_to = "Scaled_residuals"
        )
      
      # merge & show the setting of strong_weak classification
      daily_species_scale_residual_long =
        merge(daily_species_scale_residual_long, strong_weak_df)
      daily_species_scale_residual_long$Species_sw = 
        paste0(daily_species_scale_residual_long$Species, "-",
               daily_species_scale_residual_long$strong_weak)
      
      # library(ggh4x), change strip color
      # strip <- strip_themed(background_x = elem_list_rect(fill = c("gold", "tomato")
      # facet_wrap2()
      
      ### histogram for all species
      all_species_residual <-
        ggplot(daily_species_scale_residual_long, 
             aes(x = Scaled_residuals)) +
        geom_histogram(binwidth = 0.5, alpha = 0.8) +
        facet_wrap(~ Species_sw, ncol = 4) + # labeller = as_labeller(format_variable)
        geom_vline(xintercept = c(-3, 3), color = "cyan3", linewidth = 0.5) +
        scale_x_continuous(breaks = function(x) unique(c(-3, 3, pretty(x)))) +
        ggtitle("Original Scale") +
        # theme(strip.text = element_text(family = "Arial Unicode MS")) +
        theme_bw()
      
      all_species_residual_zoom <-
        all_species_residual +
        # not ylim directly, now keep the bars of which the values > y up limit
        coord_cartesian(ylim = c(0, 50)) + # ylim(0, 50) +
        ggtitle("Zoom-in Scale")
      
      all_species_residual_plot <- 
        all_species_residual + 
        all_species_residual_zoom + 
        plot_layout(ncol = 2)
      
      ### time series of daily Q/Qexp
      # set gap in date if there is no data for > 15 days
      daily_Q_Qexp$Date = as.Date(daily_Q_Qexp$Date)
      daily_Q_Qexp <- 
        daily_Q_Qexp %>%
        arrange(Date) %>%
        mutate(gap = c(0, diff(as.numeric(Date))) > 15, # calculate gaps, convert Date to numeric for diff
               group = cumsum(gap)) # Cumulative sum to create a new group after each gap
      
      daily_Q_Qexp_plot <-
        ggplot(daily_Q_Qexp, 
               aes(x = Date, y = Q_Qexp_ratio)) + # group = 1
        geom_line(aes(group = group), color = "lightblue") +
        geom_point(color = "slategray", size = 1.2) +
        labs(y = "Q/Qexp Ratio") +
        theme_bw()  +
        theme(axis.title.x = element_text(size = 0), 
              axis.title.y = element_text(color="grey25", size = 14, vjust=1),
              axis.text.x = element_text(color="grey25", size = 12), 
              axis.text.y = element_text(color="grey25", size = 12))

      ### species-specific Q/Qexp
      species_Q_Qexp = join(species_Q_Qexp, species_class)
      species_Q_Qexp_plot <-
        ggplot(species_Q_Qexp, 
               aes(x = reorder(Species, sequence), 
                   y = Q_Qexp_ratio)) +
          geom_hline(yintercept = 1, color = "brown2", linetype = "dashed", linewidth = 0.5) +
          geom_bar(stat = "identity", fill = "steelblue", width = 0.4) +
          scale_x_discrete(labels = function(x) format_variable(x)) +
          labs(y = "Q/Qexp Ratio") +
          theme_bw()+
          theme(axis.title.x = element_text(size = 0), 
                axis.title.y = element_text(color="grey25", size = 14, family = "Arial Unicode MS"),
                axis.text.x = element_text(color="grey25", size = 12, family = "Arial Unicode MS",
                                           angle = 90, hjust = 0, vjust = 0.3), 
                axis.text.y = element_text(color="grey25", size = 12, family = "Arial Unicode MS"))
        
      Q_Qexp_plot = daily_Q_Qexp_plot / species_Q_Qexp_plot # patchwork{}
      
      pred_obs_compare_resd =
        merge(pred_obs_compare, 
              select(species_Q_Qexp, 
                     Species, Q_Qexp_ratio, higher.3.scalRes.per))

      pred_obs_compare_resd = 
        data.frame(site.serial = site.serial, factor.No = factor.No, 
                   pred_obs_compare_resd)
      
      pred_obs_compare_summary =
        rbind(pred_obs_compare_summary, pred_obs_compare_resd)
      
      ####### output data summary #######
      
      base_oneFactor = data.frame(Dataset = disp.prefix, serial.No = site.serial, Factor = factor.No, 
                                  start_date = start.date, end_date = end.date,
                                  median_PMF_PM2.5 = median_PMF_PM2.5, 
                                  median_obs_PM2.5 = median_obs_PM2.5, 
                                  cor_PMF.obs_PM = cor.PMF.obs.PM,
                                  RMSE_PM2.5 = RMSE_PM2.5, R2_PM2.5 = R2_PM2.5, 
                                  Correlation.lm.conc.fraction = cor.two.fraction.contri,
                                  Qmin = lowest_Qm, Qmin_task_No = lowest_Qm_taskNo, 
                                  Q.exp = Q.exp, Q.true = Q.true, Q.robust = Q.robust,
                                  converge_percent = converge_percent,
                                  count.strong.species = strong.species.count, 
                                  count.all.species = species.weak.strong.count,
                                  day.count = site.data.row,
                                  DISP_error.code = disp.error.code, DISP_qdrop = disp.qdrop, 
                                  bs.map = bs_overall_map, 
                                  Correlation.between.factors.Min = correl_r_percentile[1],
                                  Correlation.between.factors.10th = correl_r_percentile[2],
                                  Correlation.between.factors.25th = correl_r_percentile[3],
                                  Correlation.between.factors.50th = correl_r_percentile[4],
                                  Correlation.between.factors.75th = correl_r_percentile[5],
                                  Correlation.between.factors.90th = correl_r_percentile[6],
                                  Correlation.between.factors.Max = correl_r_percentile[7],
                                  Multiple.factors.assigned.to.one.source = length(unique(source_2more_factor)), 
                                  Source.with.multiple.factors. = source_2more_factor,
                                  Number.of.factors.not.intepretated.Before.screening = count_not_intepretate)
      
      summary_base = rbind(summary_base, base_oneFactor[1, ])
      
      write.csv(summary_base, paste0(data.prefix, "base_DISP_summary.csv"))
      write.csv(correl_r_p_summary, paste0(data.prefix, "base_factor_correlation.csv"))
      write.csv(pred_obs_compare_summary, paste0(data.prefix, "PMF_vs_obs.csv"))
      
      ####### Output files #######
      
      ggsave(paste0(name.prefix, "daily.pdf"), plot = daily_conc_plot, width = 4.8, height = 6)
      ggsave(paste0(name.prefix, "month.pdf"), plot = month_conc_plot, width = 4.8, height = 6)
      ggsave(paste0(name.prefix, "annual.pdf"), plot = annual_conc_plot, width = 4.8, height = 6)
      ggsave(paste0(data.pre, factor.no, "_year_month_conc.pdf"), plot = ym_conc_plot, width = 4.8, height = 6)
      ggsave(paste0(name.prefix, "overall.pdf"), plot = overall_contri, width = 9, height = 5)
      ggsave(paste0(name.prefix, "source-PM.pdf"), plot = PM_source_daily, width = 9, height = 7)
      ggsave(paste0(name.prefix, "source_profile.pdf"), plot = source_profile, width = 5.8, height = 6, 
             device = cairo_pdf) # device = cairo_pdf, save the special fonts (super-/sub- script) in pdf
      ggsave(paste0(name.prefix, "Q_Qexp.pdf"), plot = Q_Qexp_plot, width = 9, height = 5, 
             device = cairo_pdf) # download xquartz for use of device = cairo_pdf.
      ggsave(paste0(name.prefix, "species_residual.pdf"), plot = all_species_residual_plot, width = 14.5, height = 9)
      ggsave(paste0(name.prefix, "obs_pred_ts.pdf"), plot = pred_obs_species_conc_plot, width = 14.5, height = 9)
      ggsave(paste0(name.prefix, "obs_vs_pred.pdf"), plot = species_pred_vs_obs_plot, width = 9, height = 9)
      
      conc_percent_bsDisp_output = conc_percent_bsDisp
      ts_conc_plot_output = ts_conc_plot
      lm_beta_plot_output = lm_beta_plot
      lm_beta_plot_output
      
      conc_percent_bsDisp_output$site.serial = ts_conc_plot_output$site.serial = lm_beta_plot_output$site.serial = site.serial
      conc_percent_bsDisp_output$Factor.No = ts_conc_plot_output$Factor.No = lm_beta_plot_output$Factor.No = factor.No
      
      ts_annual_conc$site.serial = ts_month_conc$site.serial = site.serial
      ts_annual_conc$Factor.No = ts_month_conc$Factor.No = factor.No
      
      write.csv(conc_percent_bsDisp_output, paste0(name.prefix, "source_profile.csv"))
      write.csv(ts_conc_plot_output, paste0(name.prefix, "daily.csv"))
      write.csv(ts_annual_conc, paste0(name.prefix, "annual.csv"))
      write.csv(ts_month_conc, paste0(name.prefix, "month.csv"))
      write.csv(lm_beta_plot_output, paste0(name.prefix, "overall.csv"))
      
      write.csv(daily_species_scale_residual, paste0(name.prefix, "species_residual.csv"))
      write.csv(daily_Q_Qexp, paste0(name.prefix, "daily_Q_Qexp.csv"))
      write.csv(species_Q_Qexp, paste0(name.prefix, "species_Q_Qexp.csv"))

    }, error=function(e) {
      print(paste("Error at iteration", site.serial, "factor", factor.No, ":", e$message))
    })
  }
}

#### 3. summary the fraction of converged runs in base model ####

converge_percent_summary = NULL

for (site.serial in site_serial_Nos) { # 1:25
  for (factor.No in 3:11) { # 5:11
    
    site.serial.factor.pre = paste0("S_", site.serial, "_F_", factor.No, "_")
    name.prefix = paste0(data.prefix, site.serial.factor.pre)
    folder_path <- paste0("S_", site.serial, "/Factor_", factor.No, "/") # "C_"
    
    tryCatch({
      
      base_report = readLines(paste0(folder_path, 
                                     data.prefix, 
                                     site.serial.factor.pre,
                                     "base_PMFreport.txt"))
      
      # converge condition 
      converge_percent = lowest_Qm_task(base_report)$converge_percent
      
      converge_percent_info = data.frame(
        site.serial = site.serial, factor.No = factor.No, 
        converge_percent = converge_percent)
      converge_percent_summary = 
        rbind(converge_percent_summary, converge_percent_info)
      
      write.csv(converge_percent_summary, paste0(data.prefix, "converge_percent.csv"))
      
    }, error = function(e) {
      
      print(paste("Error at iteration", site.serial, "factor", factor.No, ":", e$message))
    })
  }
}


#### merge with site geographic info ####

setwd("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/")
getwd()

# 15timesMean
# site_info_all = read.csv("CSN_NoGUI_NoCsub_15TimesMean_Site/CSN_noCsub_15timesMean_PMF_CMD_StrongWeakBad_Site.csv")
# PMF_base_summary = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15TimesMean/base_DISPres1/CSN_base_DISP_summary.csv")

# 15t1mdl0unc
site_info_all = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_NoGUI_NoCsub_15t1mdl0unc_Site/CSN_noCsub_15t1mdl0unc_PMF_SWB_site.csv")
PMF_base_summary = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15t1mdl0unc/base_DISPres1/CSN_noCsub_15t1mdl0unc_base_DISP_summary.csv")

# geographic info
site_geo = read.csv("CSN_IMPROVE_ownPC/CSN_site_info.csv")
site_geoid = read.csv("CSN_IMPROVE_ownPC/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_cluster_traffic = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/results_R_data/County_cluster_traffic_info.csv")

# site serial
site_code_serial = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_IMPROVE_ownPC/CSN_IMPROVE_site.serial.csv")
site_code_serial$V1 = NULL
site_serial_info = subset(site_code_serial,
                          serial.No %in% site_info_all$serial.No)
dim(site_serial_info)

site_info_all$X = site_geo$X = cty_cluster_traffic$X = site_geoid$X = PMF_base_summary$X = NULL

site_serial = select(site_serial_info, 
                     SiteCode, serial.No)
site_geoid = select(site_geoid,
                    SiteCode, geoid)
col_remove_cty = c("Dataset", "state_abbr", "Longitude", "Latitude",
                   "countyns", "namelsad", "county_name", "geoid")
site_cluster_traffic = select(cty_cluster_traffic, -col_remove_cty)

site_census = merge(site_serial, site_geo)
site_census$SiteCode = as.character(site_census$SiteCode)

site_census = merge(site_census, site_geoid, all.x = TRUE)
site_census = merge(site_census, site_cluster_traffic, all.x = TRUE)
# sapply(site_census, class)

site_census_pmf = merge(summary_base, cty_cluster_traffic, all.x = TRUE)

