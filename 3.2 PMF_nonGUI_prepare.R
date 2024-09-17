##clear environment
# rm(list=ls())

##set working directory
# setwd("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE")
# getwd()
# data.dir <- "/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE"

setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_IMPROVE_ownPC")
getwd()
data.dir <- "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_IMPROVE_ownPC"

library(tidyverse)
library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2) 
library(base)
library(ggrepel)
library(missForest)


####################################################################
#### Non-GUI-1 - ALL values - Origin_MDL ####
####################################################################
###### read data files ######

######### Above MDL or not
#### CSN
# gui_conc_mdl_cluster = fread("CSN_concentration_vs_MDL_C-subgroup_2023.04.csv") # original MDL in CSN
# gui_conc_mdl_cluster = fread("CSN_conc_vs_MDL_C-subgroup_corrected_2023.05.csv") # original MDL in CSN
gui_conc_mdl_cluster = fread("CSN_conc_vs_MDL_C-subgroup_corrected_2024.02.csv") # original MDL in CSN
gui_conc_mdl_cluster$V1 = NULL

#### IMPROVE
gui_conc_mdl_cluster = fread("IMPROVE_conc_vs_MDL_C-subgroup_corrected_2023.csv") # original MDL 
gui_conc_mdl_cluster$V1 = NULL

gui_conc_mdl_cluster$Date = as.Date(gui_conc_mdl_cluster$Date)

######### Signal-to-noise ratio
#### CSN
# cluster_snr = read.csv("CSN_cluster_SNR_for_PMF.csv") # No carbon subgroups
# cluster_snr = read.csv("CSN_cluster_SNR_for_PMF_C-subgroup.csv") # carbonaceous substances
cluster_snr = read.csv("CSN_cluster_SNR_PMF_C-subgroup_PM_corrected.csv")

#### IMPROVE
cluster_snr = read.csv("IMPROVE_cluster_SNR_PMF_C-subgroup_PM_corrected.csv") # carbonaceous substances

cluster_snr$X = NULL

#########  OOB & Missing rate
OOB_comb_avg = read.csv("CSN_OOBerror_random-forest_for_PMF.csv")
miss_comb_avg = read.csv("CSN_Missing_Qualifier_interpolation_for_PMF.csv")
OOB_comb_avg$X = miss_comb_avg$X = NULL

#########  conc & unc
#### CSN
# conc_pmf = read.csv("CSN_concentration_AQS.PM_PMF_C-subgroup_2023.04.csv")
# unc_pmf = read.csv("CSN_uncertainty_AQS.PM_PMF_C-subgroup_2023.04.csv")

# conc_pmf = fread("CSN_concentration_AQS.PM_PMF_C-sub_2023.05.csv")
# unc_pmf = fread("CSN_uncertainty_AQS.PM_PMF_C-sub_2023.05.csv")

conc_pmf = fread("CSN_concentration_AQS.PM_PMF_C-sub_2024.02.csv")
unc_pmf = fread("CSN_uncertainty_AQS.PM_PMF_C-sub_2024.02.csv")
pm_cluster = read.csv("CSN_RF_cluster5training.csv")

#### IMPROVE
conc_pmf = fread("IMPROVE_PMF_C-subgroup_PM_corrected_2023.csv")
unc_pmf = fread("IMPROVE_PMF_C-subgroup_corrected_2023.csv")
pm_cluster = read.csv("IMPROVE_RF_cluster_SiteLevel.csv")

conc_pmf$V1 = unc_pmf$V1 = NULL
pm_cluster$X = NULL

conc_pmf$Date = as.Date(conc_pmf$Date)
unc_pmf$Date = as.Date(unc_pmf$Date)

conc_pmf = merge(conc_pmf, pm_cluster)
unc_pmf = merge(unc_pmf, pm_cluster)

# the sequence may change after merging, reorder to ensure the sequence for those need row-by-row process
conc_pmf = conc_pmf[with(
  conc_pmf, 
  order(State, SiteCode, Date)), ]
unc_pmf = unc_pmf[with(
  unc_pmf, 
  order(State, SiteCode, Date)), ]
gui_conc_mdl_cluster = gui_conc_mdl_cluster[with(
  gui_conc_mdl_cluster, 
  order(State, SiteCode, Date)), ]

##### if using total OC&EC 
OC.EC.sub = c("EC1", "EC2", "EC3",
              "OC1", "OC2", "OC3", "OC4")
conc_pmf[ , OC.EC.sub] <- list(NULL)
unc_pmf[ , OC.EC.sub] <- list(NULL)
OOB_comb_avg[ , OC.EC.sub] <- list(NULL)
miss_comb_avg[ , OC.EC.sub] <- list(NULL)
cluster_snr[ , OC.EC.sub] <- list(NULL)
gui_conc_mdl_cluster[ , OC.EC.sub] <- list(NULL)

# create a dataframe to save the decision of weak, bad, or strong 
species.name = 
  setdiff(names(conc_pmf),
          c("SiteCode", "Date", "X", "V1", 
            "State", "Final.Decision"))
Finaly.Decision = 1:25
cmd_species_class_cluster = data.frame(
  matrix(ncol = length(species.name), 
         nrow = length(Finaly.Decision)))
colnames(cmd_species_class_cluster) = species.name
# add PM2.5 and other variables
cmd_species_class_cluster$PM25 = 0
cmd_species_class_cluster$Finaly.Decision = Finaly.Decision
cmd_species_class_cluster$sum.weak.good = 0
cmd_species_class_cluster$style.weak.good = 0
cmd_species_class_cluster$cluster.row = 0

# define the subfolder to save cluster files
# Clusterfolder <- "PMF_NoGUI_cluster"
# Clusterfolder <- "PMF_NoGUI_NoCsub_cluster" ## if no OC EC subgroups 
# Clusterfolder <- "PMF_NoGUI_cluster_originPM"

# extract concentration & uncertainty of single cluster for PMF
for( i in 1:25){
  # conc & unc for each cluster
  conc_cluster = subset(conc_pmf, Final.Decision == i)
  unc_cluster = subset(unc_pmf, Final.Decision == i)
  dim(unc_cluster)
  
  cluster.No = i
  conc_cluster$Final.Decision = unc_cluster$Final.Decision = NULL
  
  # site included in the cluster
  sites.in.cluster = unique(conc_cluster$SiteCode)
  
  # extract the conc_vs._mdl data of selected cluster(s)
  conc_mdl_cluster = subset(gui_conc_mdl_cluster,
                            SiteCode %in% sites.in.cluster)
  
  conc_mdl_cluster$State = NULL
  
  # double check if row and columns are matched
  summary(conc_mdl_cluster$Date == unc_cluster$Date &
            conc_mdl_cluster$SiteCode == unc_cluster$SiteCode)
  
  ## checking the ratio of concentrations above MDL for PMF resetting for selected cluster
  conc_cluster_aboveMDL = data.frame(round(sapply(
    conc_mdl_cluster[, 3:ncol(conc_mdl_cluster)], sum)/
      nrow(conc_mdl_cluster)*100, 0))
  
  conc_cluster_aboveMDL$CompName = rownames(conc_cluster_aboveMDL)
  colnames(conc_cluster_aboveMDL)[1] = "Percent"
  
  # conc_cluster_aboveMDL$Percent[conc_cluster_aboveMDL$CompName == "S"] = 
  #  conc_site_aboveMDL$Percent[conc_site_aboveMDL$CompName == "SO4"] 
  
  # Create factor levels for CompName in the order we want them to appear
  conc_cluster_aboveMDL$CompName <- factor(conc_cluster_aboveMDL$CompName, 
                                           levels = unique(conc_cluster_aboveMDL$CompName))
  
  # plot the percent of below_MDL_concentrations for each species
  # ggplot(conc_cluster_aboveMDL, aes(CompName, Percent)) +
  # ggplot(conc_cluster_aboveMDL, aes(CompName, Percent)) +
  #   geom_point() +
  #   geom_hline(yintercept = 10, color = "red") +
  #   geom_hline(yintercept = 20, color = "orange", linetype='dotted') +
  #   annotate("text", x = 15, y = 20, label = "Weak  ↓", vjust = -0.5, col = "orange") +
  #   annotate("text", x = 15, y =10, label = "Bad  ↓", vjust = -0.5, col = "red") +
  #   ylab("Percent of below-CSN_MDL concentration") +
  #   theme_bw()
  
  # get the SNR signal-to-noise ratio 
  snr_selected_cluster = subset(cluster_snr, 
                                Final.Decision == i)
  snr_selected_cluster$Final.Decision = NULL
  
  ###### Cluster-bad&weak 1. Strict SNR & Strict MDL - selected ######
  # generate the weak & bad species based on the conc_vs_mdl 
  cluster.species.weak.Pmdl = conc_cluster_aboveMDL$CompName[
    conc_cluster_aboveMDL$Percent <= 50 &
      conc_cluster_aboveMDL$Percent > 20]
  cluster.species.bad.Pmdl = conc_cluster_aboveMDL$CompName[
    conc_cluster_aboveMDL$Percent <= 20]
  # change to character
  cluster.species.weak.Pmdl = as.character(cluster.species.weak.Pmdl)
  cluster.species.bad.Pmdl = as.character(cluster.species.bad.Pmdl)
  
  # generate the weak & bad species based on SNR 
  cluster.species.bad.snr = colnames(snr_selected_cluster)[which(
    colSums(snr_selected_cluster) < 0.2)]
  cluster.species.weak.snr = colnames(snr_selected_cluster)[which(
    colSums(snr_selected_cluster) >= 0.2 &
      colSums(snr_selected_cluster) < 2)]
  
  ###### Cluster-bad&weak 2. Combine the rate need interpolation & OBB error ######
  # Extract data for cluster
  cluster_oob = subset(OOB_comb_avg, 
                       SiteCode %in% sites.in.cluster)
  cluster_miss = subset(miss_comb_avg, 
                        SiteCode %in% sites.in.cluster)
  
  # get cluster average
  cluster_oob = data.frame(sapply(cluster_oob, mean))
  colnames(cluster_oob)[1] = "OOB"
  
  cluster_miss = data.frame(sapply(cluster_miss, mean))
  colnames(cluster_miss)[1] = "Miss.Flag"
  
  cluster_oob_miss = cbind(cluster_oob, cluster_miss)
  cluster_oob_miss$CompName = row.names(cluster_oob_miss)
  # only keep rows for CompName
  cluster_oob_miss = cluster_oob_miss[2:nrow(cluster_oob_miss), ]
  
  row.names(cluster_oob_miss) = NULL
  
  # bad, weak
  cluster.oob.miss.weak = cluster_oob_miss$CompName[
    (cluster_oob_miss$Miss.Flag <= 15 & 
       cluster_oob_miss$Miss.Flag > 10 &
       cluster_oob_miss$OOB <= 10 &
       cluster_oob_miss$OOB > 5) | 
      (cluster_oob_miss$Miss.Flag <= 10 &
         cluster_oob_miss$Miss.Flag > 5 &
         cluster_oob_miss$OOB > 8)]
  cluster.oob.miss.bad = cluster_oob_miss$CompName[
    cluster_oob_miss$Miss.Flag > 15 |
      (cluster_oob_miss$Miss.Flag <= 15 & 
         cluster_oob_miss$Miss.Flag > 10 &
         cluster_oob_miss$OOB > 5)]
  
  ###### Cluster-bad&weak 3. other criteria ######
  
  # combine the "weak" & "bad" list
  cluster.species.bad = append(cluster.species.bad.snr, 
                               cluster.species.bad.Pmdl)
  cluster.species.bad = append(cluster.species.bad, 
                               cluster.oob.miss.bad)
  
  cluster.species.weak = append(cluster.species.weak.snr, 
                                cluster.species.weak.Pmdl)
  cluster.species.weak = append(cluster.species.weak, 
                                cluster.oob.miss.weak)
  
  # remove PM25, which potentially exists in the weak/bad list
  cluster.species.bad = cluster.species.bad[! cluster.species.bad %in% "PM25"]
  cluster.species.weak = cluster.species.weak[! cluster.species.weak %in% "PM25"]
  
  # if OC/EC/OPC is in bad, force remove to weak
  oc.ec = c("EC", "EC1", "EC2", "EC3", 
            "OC", "OC1", "OC2", 
            "OC3", "OC4", "OP")
  
  oc.ec.bad = cluster.species.bad[cluster.species.bad %in% oc.ec]
  cluster.species.bad = cluster.species.bad[! cluster.species.bad %in% oc.ec]
  cluster.species.weak = append(cluster.species.weak, 
                                oc.ec.bad)
  
  # if a species exist both in "bad" & "weak", define as "WEAK"
  cluster.species.bad = cluster.species.bad[! (
    cluster.species.bad %in% cluster.species.weak)]
  
  # add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
  cluster.species.bad = append(c("S", "Na", "K"), 
                               cluster.species.bad) 
  # in case any of this three is in weak, if so, remove
  cluster.species.weak = cluster.species.weak[! (
    cluster.species.weak %in% c("S", "Na", "K"))]
  
  # add Al, Mg, Na into weak, if there are not in bad, due to the lower reliability in XRF test
  Al.Mg.weak = c("Al", "Mg")
  # determine if Al or Mg is in bad, if so, remove the one in "Al.Mg.weak"
  Al.Mg.weak = Al.Mg.weak[! (
    Al.Mg.weak %in% cluster.species.bad)]
  # add the rest in Al.Mg.weak to "weak"
  cluster.species.weak = append(Al.Mg.weak, 
                                cluster.species.weak) 
  # "Na", "K" were removed, and use Na+, K+ instead
  
  # remove the duplicated
  # cluster.species.weak = cluster.species.weak[!duplicated(cluster.species.weak)]
  
  # remove the duplicated strings from the character vector
  cluster.species.bad = unique(
    unlist(
      strsplit(
        cluster.species.bad, " ")))
  cluster.species.weak = unique(
    unlist(
      strsplit(
        cluster.species.weak, " ")))
  
  ###### Cluster-bad&weak 4. Distribution of values above MDL ######
  # species stay in "bad" and related to higher Pmdl
  cluster.species.bad.check = cluster.species.bad[
    cluster.species.bad %in% 
      cluster.species.bad.Pmdl]
  
  # remove those with >95% of value below MDL
  cluster.species.bad.check = 
    unique(
      subset(conc_cluster_aboveMDL, 
             CompName %in% cluster.species.bad.check &
               Percent > 5)$CompName)
  
  # cluster.species.bad.check = c("Ag", "Ca")
  if(length(cluster.species.bad.check) > 0){
    # files for those with above MDL concentrations
    conc_cluster_mdl = conc_cluster[, 4:ncol(conc_cluster)] *
      conc_mdl_cluster[, 3:ncol(conc_mdl_cluster)]
    
    # subset those grouped as bad
    conc_cluster_bad = conc_cluster_mdl %>%
      select(all_of(cluster.species.bad.check))
    
    # replace 0 by NA
    conc_cluster_bad <-
      data.frame(
        apply(
          conc_cluster_bad, 
          2, 
          function(x) 
            replace(x, x == 0, NA)))
    
    # for concentrations above MDL, check the 10th vs. 90th percentile ratio
    # for some random comparison, ratio < 0.1 for strong species and < 0.2 for weak 
    # or, change to compare mean vs. sd? if mean < sd, or if mean > sd
    conc_cluster_bad_mean = 
      conc_cluster_bad %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          list(
            p = ~mean(., na.rm = T)
          )
        ))
    
    conc_cluster_bad_sd = 
      conc_cluster_bad %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          list(
            p = ~sd(., na.rm = T)
          )
        ))
    
    bad_sd_mean = conc_cluster_bad_sd/conc_cluster_bad_mean
    colnames(bad_sd_mean) = cluster.species.bad.check
    
    # Detect the species for which the sd > mean
    bad_comp <- which(colSums(bad_sd_mean > 1) > 0)
    cluster.species.bad.remove <- colnames(bad_sd_mean)[bad_comp]
    
  } else{
    cluster.species.bad.remove = NA
  }
  
  # remove the species with very scatter distribution from bad and add it into weak 
  cluster.species.bad = 
    cluster.species.bad[
      !(cluster.species.bad %in% 
          cluster.species.bad.remove)]
  
  cluster.species.weak = append(cluster.species.weak, 
                                cluster.species.bad.remove)
  cluster.species.weak = cluster.species.weak[
    !is.na(cluster.species.weak)]
  
  # arrange in alphabetic order 
  cluster.species.bad = sort(cluster.species.bad)
  cluster.species.weak = sort(cluster.species.weak)
  cluster.species.weak = append(cluster.species.weak, "PM25")
  cluster.species.strong = unique(
    subset(conc_cluster_aboveMDL, 
           !(CompName %in% cluster.species.bad |
               CompName %in% cluster.species.weak))$CompName)
  cluster.species.strong = cluster.species.strong[! cluster.species.strong %in% "PM25"]
  cluster.species.strong = as.character(cluster.species.strong)
  
  ###### Cluster- final 1: files of conc & unc for CMD ######
  # remove species marked as bad
  conc_cluster_pmf = conc_cluster[ ,-which(
    names(conc_cluster) %in% cluster.species.bad)]
  unc_cluster_pmf = unc_cluster[ ,-which(
    names(conc_cluster) %in% cluster.species.bad)]
  
  # for those marked as weak, set the uncertainty 3 times as much
  # unc_cluster_pmf_1 = unc_cluster_pmf
  
  unc_cluster_pmf[names(unc_cluster_pmf) %in% cluster.species.weak] = 
    3 * unc_cluster_pmf[names(unc_cluster_pmf) %in% cluster.species.weak]
  
  # for PM2.5, set the uncertainty 3*3 times as much
  unc_cluster_pmf$PM25 = 3 * 3 * conc_cluster_pmf$PM25
  
  #head(unc_cluster_pmf_1$Ca); head(unc_cluster_pmf$Ca); head(conc_cluster_pmf$Ca)
  #head(unc_cluster_pmf_1$Al); head(unc_cluster_pmf$Al); head(conc_cluster_pmf$Al)
  #head(unc_cluster_pmf_1$PM25); head(unc_cluster_pmf$PM25); head(conc_cluster_pmf$PM25)
  
  # conc_cluster.1 = conc_cluster; unc_cluster.1 = unc_cluster
  # add info into column names before combination
  colnames(conc_cluster_pmf) = paste0("conc_", colnames(conc_cluster_pmf))
  colnames(unc_cluster_pmf) = paste0("unc_", colnames(unc_cluster_pmf))
  
  # reorder before combine
  conc_cluster_pmf = conc_cluster_pmf[with(conc_cluster_pmf,
                                           order(conc_SiteCode, conc_Date)), ]
  unc_cluster_pmf = unc_cluster_pmf[with(unc_cluster_pmf,
                                         order(unc_SiteCode, unc_Date)), ]
  
  # combining conc & unc files
  cmd_cluster_conc_unc = cbind(conc_cluster_pmf, unc_cluster_pmf)
  
  # Interleave columns of concentration and uncertainty files
  ## generate a new vector to index the columns of cbind file
  interleave.col.order <- 
    rep(1:ncol(unc_cluster_pmf), each = 2) + 
    (0:1) * ncol(unc_cluster_pmf)
  ## reorder the cbind file
  cmd_cluster_interleave = cmd_cluster_conc_unc[interleave.col.order]
  
  # only keep one Date & cluster column
  cmd_cluster_interleave$conc_Date = 
    cmd_cluster_interleave$conc_State = 
    cmd_cluster_interleave$conc_SiteCode = NULL
  colnames(cmd_cluster_interleave)[1:3] = 
    c("SiteCode", "Date", "State")
  head(cmd_cluster_interleave)
  
  cmd_cluster_interleave = 
    cmd_cluster_interleave[
      with(cmd_cluster_interleave, 
           order(SiteCode, Date)), ]
  
  write.csv(cmd_cluster_interleave, 
            paste0(Clusterfolder, "/CSN_C_", 
                   cluster.No, "_PMF_CMD.csv"))
  
  write.csv(cmd_cluster_interleave, 
            paste0(Clusterfolder, "/CSN_noCsub_C_", 
                   cluster.No, "_PMF_CMD.csv"))
  
  ###### Cluster- final 2: files of weak, bad, strong for CMD ######
  cmd_species_class_cluster[i, cluster.species.strong] <- 1
  cmd_species_class_cluster[i, cluster.species.weak] <- 0
  cmd_species_class_cluster[i, cluster.species.bad] <- NA
  
  cmd_species_class_cluster$sum.weak.good[i] = 
    length(cluster.species.weak) +
    length(cluster.species.strong) 
  
  cmd_species_class_cluster$cluster.row[i] = nrow(conc_cluster)
  # detect the species for None-GUI PMF, thus, weak and strong species
  nonGUI_disp_species <- 
    cmd_species_class_cluster[
      i, 
      c(cluster.species.strong, cluster.species.weak)]
  species.name.use = 
    species.name[
      species.name %in% 
        c(cluster.species.strong, cluster.species.weak)]
  
  # add PM25
  species.name.use = append(species.name.use, "PM25")
  
  nonGUI_disp_species <- nonGUI_disp_species[, species.name.use]
  cmd_species_class_cluster$style.weak.good[i] <- 
    paste0("/", nonGUI_disp_species, collapse = "")
  
  write.csv(cmd_species_class_cluster, 
            paste0(Clusterfolder, "/CSN_PMF_CMD_StrongWeakBad_Cluster.csv"))
  
  write.csv(cmd_species_class_cluster, 
            paste0(Clusterfolder, "/CSN_noCsub_PMF_CMD_StrongWeakBad_Cluster.csv"))
}


################################################################################
#### Non-GUI-2 - NO EXTREMES - Origin_MDL ####
################################################################################

##### Read data files #####

########## read.1 OOB & Missing rate ##########
#### CSN
# OOB_comb_avg = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_OOBerror_random-forest_for_PMF.csv")
# miss_comb_avg = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_Missing_Qualifier_interpolation_for_PMF.csv")
# OOB_comb_avg = fread("CSN_OOBerror_random-forest_for_PMF.csv")
# miss_comb_avg = fread("CSN_Missing_Qualifier_interpolation_for_PMF.csv")

# OOB_comb_avg = fread("CSN_OOBerror_random-forest_for_PMF.csv")
# miss_comb_avg = fread("CSN_Missing_Qualifier_interpolation_for_PMF.csv")

OOB_comb_avg = fread("CSN_OOBerror_random-forest_for_PMF_2024.csv")
miss_comb_avg = fread("CSN_Missing_Qualifier_interpolation_for_PMF_2024.csv")

#### IMPROVE
OOB_comb_avg_org = read.csv("IMPROVE_OOBerror_random-forest.csv")
# add new column into data.table OOB_comb_avg_org
oob_sitecode_row = 
  append("SiteCode",
         names(OOB_comb_avg_org)[2:ncol(OOB_comb_avg_org)])
OOB_comb_avg_org = rbind(OOB_comb_avg_org, oob_sitecode_row, fill = TRUE)
OOB_comb_avg_org$X = NULL
OOB_comb_avg_org = OOB_comb_avg_org[-nrow(OOB_comb_avg_org), ] 

# t-convert
OOB_comb_avg = data.table(t(OOB_comb_avg_org))
dim(OOB_comb_avg)

# change colnames
names(OOB_comb_avg) = OOB_comb_avg_org[, 1]
OOB_comb_avg = 
  relocate(OOB_comb_avg, Variables, .before = "PM2.5")
OOB_comb_avg = OOB_comb_avg[-1, ]
names(OOB_comb_avg)[1] = "SiteCode"

miss_comb_avg = fread("IMPROVE_Missing_Qualifier_interpolation_All.csv")


### CSN & IMPROVE
OOB_comb_avg$V1 = miss_comb_avg$V1 = NULL

OOB_comb_avg = species_col_reorder(OOB_comb_avg)
miss_comb_avg = species_col_reorder(miss_comb_avg)

OOB_comb_avg = relocate(OOB_comb_avg, SiteCode, .before = Al) # .before = Ag
miss_comb_avg = relocate(miss_comb_avg, SiteCode, .before = Al) # .before = Ag

# convert character to numeric
OOB_comb_avg = 
  OOB_comb_avg %>%
  dplyr::mutate(
    dplyr::across(Al:PM25,
                  as.numeric))

summary(names(OOB_comb_avg) == names(miss_comb_avg))
names(miss_comb_avg)
summary(OOB_comb_avg)
summary(miss_comb_avg)

########## read.2 concentration ##########
# uncertainties would be re-estimated after removing and replacing the extreme data points

#### CSN
# conc_pmf = fread("CSN_concentration_AQS.PM_PMF_C-sub_2023.05.csv")
# conc_pmf = fread("CSN_concentration_AQS.PM_PMF_C-sub_2024.02.csv")
# conc_pmf = fread("CSN_concentration_AQS.PM_PMF_C-sub_2024.03.csv")
conc_pmf = fread("CSN_RFinterpulated_combine_Csubgroup_2024.04.csv")

#### IMPROVE
# conc_pmf = fread("IMPROVE_PMF_C-subgroup_PM_corrected_2023.csv")
conc_pmf = fread("IMPROVE_interpulation_random-forest_2023.csv") # 2024.03 data
summary(conc_pmf$EC1)
summary(conc_pmf$EC1 - conc_pmf$OP)

conc_pmf$V1 = NULL #conc_pmf$Final.Decision = 
nrow(conc_pmf); names(conc_pmf)
# conc_pmf$Date = as.Date(conc_pmf$Date)

conc_pmf = species_col_reorder(conc_pmf)
conc_pmf = relocate(conc_pmf, SiteCode, .before = Date)

# reorder to ensure the sequence for those need row-by-row process
conc_pmf = 
  conc_pmf[with(
  conc_pmf, 
  order(SiteCode, Date)), ]
summary(conc_pmf)

########## read.3 test uncertainty, error_fraction  ##########

# comp_error_fraction = fread("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/CSN_k_Error Fraction.csv")
comp_error_fraction = fread("CSN_k_Error-Fraction_2023.04.csv")
comp_error_fraction$data = NULL

# set EF of subgroups to the same of OC, EC
comp_error_fraction$EC3 = comp_error_fraction$EC2 = comp_error_fraction$EC1 = comp_error_fraction$EC
comp_error_fraction$OC4 = comp_error_fraction$OC3 = comp_error_fraction$OC2 = comp_error_fraction$OC1 = comp_error_fraction$OC
comp_error_fraction$OP = comp_error_fraction$EC
comp_error_fraction = species_col_reorder(comp_error_fraction)

summary(comp_error_fraction)

########## read.4 site_code_serial  ##########

##### VC for dispersion normalization, based on ERA5 data 
# dn_vc = fread("Nearest_ERA5_Wind_BLH_VC_CSN&IMPROVE.csv")
# dn_vc$V1 = NULL
# # dn_vc$Date = as.Date(dn_vc$Date)
# dn_vc_use = select(dn_vc,
#                    Dataset, SiteCode, Date, VC_coef)
# dn_vc_use =
#   dn_vc_use[
#     with(dn_vc_use,
#       order(Dataset, SiteCode, Date)), ]

#### get SiteCode & site.serial matching list for both CSN & IMPROVE datasets

# site_code_serial = select(dn_vc_use, Dataset, SiteCode)
# site_code_serial = unique(site_code_serial)
# site_code_serial$serial.No = 1:nrow(site_code_serial)
# 
# site_code_serial$serial.No =
#   ifelse(site_code_serial$serial.No < 100,
#          sprintf("%03d", site_code_serial$serial.No),
#          as.character(site_code_serial$serial.No))
# write.csv(site_code_serial, "CSN_IMPROVE_site.serial.csv")

site_code_serial_all = fread("CSN_IMPROVE_site.serial.csv"); site_code_serial_all$V1 = NULL

# add 0 to force the serial.No being three digits
site_code_serial_all$serial.No =
  ifelse(site_code_serial_all$serial.No < 100,
         sprintf("%03d", site_code_serial_all$serial.No),
         as.character(site_code_serial_all$serial.No))

## CSN
site_code_serial = subset(site_code_serial_all, Dataset == "EPACSN") 

## IMPROVE
site_code_serial = subset(site_code_serial_all, Dataset == "IMPAER") 

dim(site_code_serial)

# exclude sites that only include in conc_pmf
all_sites = unique(conc_pmf$SiteCode)
site_list = subset(site_code_serial,
                   SiteCode %in% all_sites)
length(unique(site_list$serial.No))
length(all_sites)

## get variables used for manual info check ahead
# imp_meta_sites = fread("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/R - original IMPROVE/IMPROVE metadata 196 sample sites info 2010-20.csv")
# imp_meta_sites_use = select(imp_meta_sites,
#                             SiteCode, State, Site, AQSCode, Latitude, Longitude, 
#                             StartDate, EndDate, LocDesc)
# imp_meta_sites_use = subset(imp_meta_sites_use,
#                             SiteCode %in% unique(site_list$SiteCode))
# write.csv(imp_meta_sites_use, "IMPROVE_168_site_info.csv")

## CSN
all_sites[13] %in% site_code_serial$SiteCode # 60731018
site60731018 = subset(conc_pmf,SiteCode==60731018)
all_sites = all_sites[all_sites != 60731018]



########## read.4 conc vs. MDL  ##########

### CSN
# species_conc_above_mdl = fread("CSN_conc_vs_MDL_C-subgroup_corrected_2024.03.csv")
species_conc_above_mdl = fread("CSN_conc_vs_MDL_C-subgroup_corrected_2024.04.csv")

### IMPROVE
# species_conc_above_mdl = fread("IMPROVE_conc_vs_MDL_C-subgroup_corrected_2024.csv")
species_conc_above_mdl = fread("IMPROVE_conc_vs_MDL_C-subgroup_corrected_2024.06.csv")

dim(species_conc_above_mdl)
al_conc_above_mdl = select(species_conc_above_mdl, SiteCode, Date, Al)

species_conc_above_mdl$V1 = NULL # imp_mdl$OP = 
# species_conc_above_mdl$Date = as.Date(species_conc_above_mdl$Date)

species_conc_above_mdl = 
  species_conc_above_mdl[with(
    species_conc_above_mdl, 
    order(SiteCode, Date)), ]

summary(species_conc_above_mdl$Date == conc_pmf$Date & 
          species_conc_above_mdl$SiteCode == conc_pmf$SiteCode)
summary(species_conc_above_mdl)
a = subset(species_conc_above_mdl, is.na(Al))

########## read.5 mdl  ##########
### CSN
# csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_MDL_monthly.csv")
# csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_MDL_monthly_2023.02.27.csv")
species_daily_fullMDL = fread("CSN_MDL_C-Sub_monthly_forPMF_expand_2024.04.csv")

### IMPROVE
species_daily_fullMDL = fread("IMPROVE_MDL_C-Sub_daily_forPMF_expand_2024.06.csv")

species_daily_fullMDL$V1 = NULL # 

species_daily_fullMDL$Date = as.Date(species_daily_fullMDL$Date)

species_daily_fullMDL = 
  species_daily_fullMDL[with(
    species_daily_fullMDL, 
    order(SiteCode, Date)), ]

summary(species_daily_fullMDL$Date == conc_pmf$Date & 
          species_daily_fullMDL$SiteCode == conc_pmf$SiteCode)

########## read.6 the marked Interpolated points  ##########

### CSN
#species_NA_intp = fread("CSN_TF_logical_InterpolatedOrNot.csv")
# species_NA_intp = fread("CSN_TF_logical_InterpolatedOrNot_C-subgroup.csv")
species_NA_intp = fread("CSN_TF_logical_InterpolatedOrNot_C-subgroup_2024.04.csv")
# species_NA_intp$K = NULL

### IMPROVE
# species_NA_intp = fread("IMPROVE_TF_logical_InterpolatedOrNot_allSpecies.csv")
species_NA_intp = fread("IMPROVE_TF_logical_InterpolatedOrNot_allSpecies_2024.06.csv")

species_NA_intp = species_col_reorder(species_NA_intp)
species_NA_intp$V1 = NULL
dim(species_NA_intp)

species_NA_intp = relocate(species_NA_intp, SiteCode, .before = Date)

species_NA_intp = 
  species_NA_intp[
    with(
      species_NA_intp, 
      order(SiteCode, Date)), ]

# double check if rows & columns match
summary(species_NA_intp$SiteCode == conc_pmf$SiteCode)
summary(species_NA_intp$Date == conc_pmf$Date)
summary(names(species_NA_intp) == names(conc_pmf))

########## read.7 excluding species not to be used  ##########

OC.EC.sub = c("EC1", "EC2", "EC3", "OP",
              "OC1", "OC2", "OC3", "OC4")
# OC.EC.sub = c("OC", "EC")

## CSN, Br too noisy
species_exclude <- c(OC.EC.sub, c("Ag", "Br", "Na", "K", "Rb", "S", "Zr", "ClIon")) # not used for SA, or colinearity

## IMPROVE, Br too noisy
species_exclude <- c(OC.EC.sub, c("P", "Br", "Rb", "Zr", "ClIon", "NO2Ion")) # not used for SA, or colinearity


conc_pmf[ , species_exclude] <- list(NULL)
OOB_comb_avg[ , species_exclude] <- list(NULL)
miss_comb_avg[ , species_exclude] <- list(NULL)
species_conc_above_mdl[ , species_exclude] <- list(NULL)
species_NA_intp[ , species_exclude] <- list(NULL)
species_daily_fullMDL[ , species_exclude] <- list(NULL)

##### define the species_source_groups ##### 

species_source_groups = 
  list(earth_elements = c("Al", "Fe", "Ca", "Si", "Ti", "AlIon", "FeIon", "CaIon", "Mg", "MgIon"),
       resuspend_dust = c("Ba", "Br", "Pb", "Ti", "Fe"),
       # coke_produce = c("Al", "Fe", "Ca", "Si", "K", "AlIon", "FeIon", "CaIon", "KIon", "NO3Ion", "SO4Ion"), # Luo_2023_IJEST
       construction = c("Ca", "Mg", "CaIon", "MgIon"), # Peilin Chen_2023, or only Ca, Liu_2017_EP_PMF
       fugtive_dust = c("Ti", "K", "KIon"), # Kotchenruther_2016_AE
       # High Ca/Al ratio was a good marker for urban fugitive dust from Asian dust (Sun_2019_STE).
       # fugitive dust in agricultural production is mainly soil dust particles, agricultural harvesting machinery (Liu_2022_Agriculture)
       sec_nitrate = c("NH4Ion", "NO3Ion"),
       sulfate = c("NH4Ion", "SO4Ion"),
       fresh_sea_salt = c("NaIon", "ClIon", "Na", "Cl", "Sr", "Br"), #Louie_2005_STE, Hadley_2017_AE
       aged_sea_salt = c("SO4Ion", "NO3Ion", "Mg", "MgIon", "NaIon", "ClIon", "Na", "Cl", "NO3Ion"),
       galvanizing = c("Pb", "Zn"), # Dai_2023_EP
       ferros_metal = c("Fe", "Zn", "Mn"), # Yang_2023_Atmosphere
       non_ferros_metal = c("Cu", "Cr", "Ni", "Pb"), # Yang_2023_Atmosphere
       # Singh_2022_AAQR OC/EC ratio, Masiol_2017_AE, Maiti_2006_EMA, Vassilev_2010_Fuel
       biomass = c("OC", "K", "KIon", "Br", "Cl", "ClIon", "S", "SO4Ion", "ammSO4", "Mn", "P", "Ti", "Na", "Zn", "Pb", "Ni", "Cu"), 
       heavy_oil = c("Ni", "V"), # Ni:V ratio, Kotchenruther_2013_AE_PM, SPECIATE, Hadley_2017_AE
       coal_burn = c("S", "SO4Ion", "ammSO4", "EC", "OC", "As", "Se", "Pb", "Cd", "Sb", "Cl", "ClIon"), # Xie_2022_EP, Tao_2017_STE
       firework = c("K", "Pb", "Cu", "Sr", "As", "Ba", "Na", "KIon", "NaIon", "Mg", "MgIon", "OC", "EC", "NO3Ion", "SO4Ion"), # Phil, slides, natural relationships
       non_tailpipe = c("Fe", "Cu", "Zn", "Pb", "Mn", "Ba", "Sb", "Al", "Cr"), # Hasheminassab_2014_ACP, also , EC/OC gas/diesel; Nanjing_Zheng_2019; Park_STOTEN_2022_Beijing-Seoul
       vehicle = c("OC", "EC", "Fe", "Zn", "NO3Ion", "ammNO3"), # Dai_2023_EP, Nanjing_Zheng_2019;  Wong_2020_ACP, OC/EC ratios, diesel vehicles (high NOx) are 0.5, gasoline vehicles are 1.8–2.2
       airport = c("OC", "Ca", "Al", "Fe", "CaIon", "FeIon", "EC", "Mg", "MgIon"), #Yin_2024_STE, PNC airport China, OC/EC 2.5–3.5.
       lubricating_oils =c("Fe", "Cu", "Zn"), # Sahu_2011_STE
       op_rich = c("OP", "S", "SOIon", "ammSO4", "NH4Ion", "H"),
       # steel_industry = c("Mn"), metal_industry = c("Pb"), coal_burn = c("Se"), # Rahman_2022_EI
       heavy_metal_g1 = c("As", "Cu", "Pb", "Co"), # https://www.britannica.com/science
       heavy_metal_g2 = c("Ce", "Cu", "Pb"), 
       heavy_metal_g3 = c("Cs", "Al", "Si", "Rb"), 
       heavy_metal_g4 = c("Fe", "Ni", "Co"), # heat-resistant metal
       heavy_metal_g5 = c("In", "Bi", "Sn", "Pb"), # low melting alloys 
       heavy_metal_g6 = c("Rb", "As", "Hg"), 
       heavy_metal_g7 = c("Sb", "Cu", "Pb", "Hg")
  )
 

##### backup before species transfer for C-sub or Ions in IMPROVE #####
conc_pmf_bk = conc_pmf
OOB_comb_avg_bk = OOB_comb_avg
miss_comb_avg_bk = miss_comb_avg
species_NA_intp_bk = species_NA_intp
species_conc_above_mdl_bk = species_conc_above_mdl
species_daily_fullMDL_bk = species_daily_fullMDL

# conc_pmf = conc_pmf_bk
# OOB_comb_avg = OOB_comb_avg_bk
# miss_comb_avg = miss_comb_avg_bk
# species_NA_intp = species_NA_intp_bk
# species_conc_above_mdl = species_conc_above_mdl_bk
# species_daily_fullMDL = species_daily_fullMDL_bk


##### process C-sub inputs, EC1 = EC1-OP ##### 

##### NOT USE FOR NOW, lots of negative EC1 (8%) in IMPROVE after this step
##### against the positive input criteria

# # no change for species_daily_fullMDL
# conc_pmf$EC1 = conc_pmf$EC1 - conc_pmf$OP
# nrow(subset(conc_pmf, EC1 <0))
# 
# OOB_comb_avg$EC1 = (OOB_comb_avg$EC1 + OOB_comb_avg$OP)/2
# miss_comb_avg$EC1 = (miss_comb_avg$EC1 + miss_comb_avg$OP)/2
# 
# species_NA_intp$EC1 = species_NA_intp$EC1 * species_NA_intp$OP
# species_conc_above_mdl$EC1 = species_conc_above_mdl$EC1 * species_conc_above_mdl$OP
# 
# dim(conc_pmf); dim(OOB_comb_avg); dim(miss_comb_avg)
# dim(species_NA_intp); dim(species_conc_above_mdl); dim(species_daily_fullMDL)

##### IMPROVE N/S & ion species ##### 

# use NO3-, SO4(2-)
ions_exclude <- c("ammNO3", "ammSO4", "S") 

# use ammNO3 and ammSO4
ions_exclude <- c("NO3Ion", "SO4Ion", "S") 
comp_error_fraction$ammNO3 = comp_error_fraction$NO3Ion
comp_error_fraction$ammSO4 = comp_error_fraction$SO4Ion
comp_error_fraction = comp_error_fraction %>% select(-PM25, everything(), PM25) # Place PM2.5 at the end

# use NO3- and S element
ions_exclude <- c("ammNO3", "ammSO4", "SO4Ion") 

### exclude the species 
conc_pmf[ , ions_exclude] <- list(NULL)
OOB_comb_avg[ , ions_exclude] <- list(NULL)
miss_comb_avg[ , ions_exclude] <- list(NULL)
species_conc_above_mdl[ , ions_exclude] <- list(NULL)
species_NA_intp[ , ions_exclude] <- list(NULL)
species_daily_fullMDL[ , ions_exclude] <- list(NULL)


# extract species colnames, those after excluding "SiteCode", "State" & "Date"
species_columns = 
  setdiff(names(conc_pmf), 
          c("SiteCode", "Date", "State"))

# if include more species than in conc_pmf, exclude them
comp_error_fraction = comp_error_fraction[, ..species_columns]
summary(names(comp_error_fraction) == names(conc_pmf)[4:ncol(conc_pmf)])


##### choose the Folder ##### 
# ONLY apply this to SITEs, and combine site data if running the cluster analyses

###### define the subfolder to save cluster files
dropbox_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_PMF"

# #### CSN, extreme, mean*25 as threshold of outlier, remove all above threshold
# Clusterfolder <- "PMF_NoGUI_NoCsub_NoExtreme_cluster" ## if no OC EC subgroups, no extreme values
# GUI.cluster.folder <- "PMF_GUI_NoCsub_NoExtreme_cluster"
# prefix = "CSN_noCsub_noExtreme_C_"
# 
# #### CSN, extreme, mean*25 as threshold of outlier, only remove those with high K on July.4th
# Clusterfolder <- "CSN_NoGUI_NoCsub_25TimesMean_cluster" ## if no OC EC subgroups, no extreme values
# GUI.cluster.folder <- "CSN_GUI_NoCsub_25TimesMean_cluster"
# prefix = "CSN_noCsub_25timesMean_C_"
# 
# #### CSN, extreme, mean*15 as threshold of outlier, remove those with high K, and replace those with single species high by median
# nonGUI.site.folder <- "CSN_NoGUI_NoCsub_15TimesMean_Site" ## if no OC EC subgroups, no extreme values 
# GUI.site.folder <- "CSN_GUI_NoCsub_15TimesMean_Site"
# prefix = "CSN_noCsub_15timesMean_S_"
# 
# #### CSN, extreme, mean*15, Median MDL, one MDL across whole period
# nonGUI.site.folder <- "CSN_NoGUI_NoCsub_15Times1MDL_Site" ## if no OC EC subgroups, no extreme values 
# GUI.site.folder <- "CSN_GUI_NoCsub_15Times1MDL_Site"
# prefix = "CSN_noCsub_15times1MDL_S_"
# 
# #### CSN, extreme, mean*15, uncertainty of the interpolated values, 4.5 times as high, earlier, 1.5 times
# nonGUI.site.folder <- "CSN_NoGUI_NoCsub_15Times45unc_Site" ## if no OC EC subgroups, no extreme values 
# GUI.site.folder <- "CSN_GUI_NoCsub_15Times45unc_Site"
# prefix = "CSN_noCsub_15Times45unc_S_"
# 
# #### CSN, extreme, mean*15, 4.5 times uncertainty for interpolated values, match AQS before interpolation
# nonGUI.site.folder <- "CSN_NoGUI_NoCsub_15t45uncAQS_Site" ## if no OC EC subgroups, no extreme values 
# GUI.site.folder <- "CSN_GUI_NoCsub_15t45uncAQS_Site"
# prefix = "CSN_noCsub_15t45uncAQS_S_"

#### CSN, extreme, mean*15, MDL from either 2011.01-2015.11, or post-2016 median, 4.5 times uncertainty for interpolated values, match AQS before interpolation
# nonGUI.site.folder <- "CSN_NoGUI_NoCsub_15t1mdl0unc_Site" ## if no OC EC subgroups, no extreme values 
# GUI.site.folder <- "CSN_GUI_NoCsub_15t1mdl0unc_Site"
# prefix = "CSN_noCsub_15t1mdl0unc_S_"

# #### CSN, extreme, mean*15, MDL from either 2011015, or post-2016 median, 4.5 times uncertainty, match AQS PM2.5
# # always V, Ni, (higher uncertainty if they were grouped as Bad according to current criteria
# nonGUI.site.folder <- "CSN_NoGUI_NoCsub_15t1mdlVNi_Site" ## if no OC EC subgroups, no extreme values 
# GUI.site.folder <- "CSN_GUI_NoCsub_15t1mdlVNi_Site"
# prefix = "CSN_noCsub_15t1mdlVNi_S_"
# # 
# #### CSN, extreme, mean*15, MDL from either 2011015, or post-2016 median, 4.5 times uncertainty, match AQS PM2.5
# # always V, Ni, (higher uncertainty if they were grouped as Bad according to current criteria
# nonGUI.site.folder <- "CSN_NoGUI_Csub_15t1mdlVNi_Site" ## if no OC EC subgroups, no extreme values
# GUI.site.folder <- "CSN_GUI_Csub_15t1mdlVNi_Site"
# prefix = "CSN_Csub_15t1mdlVNi_S_"

# #### IMPROVE, extreme, mean*15, MDL from random forest, 4.5 times uncertainty
# # always V, Ni, (higher uncertainty if they were grouped as Bad according to current criteria
# nonGUI.site.folder <- "IMPROVE_NoGUI_Csub_15t1mdlVNi_Site" ## if no OC EC subgroups, no extreme values 
# GUI.site.folder <- "IMPROVE_GUI_Csub_15t1mdlVNi_Site"
# prefix = "IMPROVE_Csub_15t1mdlVNi_S_"

# #### IMPROVE, extreme, mean*15, MDL from random forest, 4.5 times uncertainty, use ammSO4 & ammNO3
# # always V, Ni, (higher uncertainty if they were grouped as Bad according to current criteria
# nonGUI.site.folder <- "IMPROVE_NoGUI_Csub_15tAmmIonVNi_Site" ## if no OC EC subgroups, no extreme values 
# GUI.site.folder <- "IMPROVE_GUI_Csub_15tAmmIonVNi_Site"
# prefix = "IMPROVE_Csub_15tAmmIonVNi_S_"

#### IMPROVE, extreme, mean*15, MDL from random forest, 4.5 times uncertainty, use ammSO4 & ammNO3
# always V, Ni, (higher uncertainty if they were grouped as Bad according to current criteria
nonGUI.site.folder <- "IMPROVE_NoGUI_NoCsub_15tAmmIonVNi_Site" ## if no OC EC subgroups, no extreme values
GUI.site.folder <- "IMPROVE_GUI_NoCsub_15tAmmIonVNi_Site"
prefix = "IMPROVE_NoCsub_15tAmmIonVNi_S_"

# #### CSN, extreme, season 99th as threshold of outlier
# Clusterfolder <- "CSN_NoGUI_NoCsub_99season_cluster"
# GUI.cluster.folder <- "CSN_GUI_NoCsub_99season_cluster"
# prefix = "CSN_noCsub_noSeason99_C_"
# 
# #### IMPROVE, extreme, mean*25
# Clusterfolder <- "IMPROVE_noGUI_NoCsub_NoExtreme_cluster"
# GUI.cluster.folder <- "IMPROVE_GUI_NoCsub_NoExtreme_cluster"
# prefix = "IMPROVE_noCsub_noExtreme_C_"

##### create Dataset/Folder to save results ##### 

###### create folder for out puts if not exist
file.path(dropbox_path, GUI.site.folder)

if (!dir.exists(file.path(dropbox_path, GUI.site.folder))) {
  dir.create(file.path(dropbox_path, GUI.site.folder))
}

if (!dir.exists(file.path(dropbox_path, nonGUI.site.folder))) {
  dir.create(file.path(dropbox_path, nonGUI.site.folder))
}

###### a dataframe to save the decision of weak, bad, or strong 

species.name = 
  setdiff(names(conc_pmf),
          c("SiteCode", "Date", "X", "V1", 
            "State", "Final.Decision"))

cmd_species_class_site = 
  data.frame(
    matrix(ncol = length(species.name), 
           nrow = nrow(site_list)))
names(cmd_species_class_site) = species.name

# add PM2.5 and other variables
cmd_species_class_site = 
  data.frame(site_list, 
             cmd_species_class_site, 
             sum.weak.good = NA, style.weak.good = NA, 
             site.row = NA, extreme_rowNo_remain = NA, 
             extreme_rowNo_replace = NA, extreme_rowNo_remove = NA, 
             row_count_org = NA, original_PM_weak = NA,
             species.bad.to.weak = NA)


# dataset = "CSN"
dataset = "IMPROVE"
cmd_species_class_site$Dataset = dataset

cmd_species_class_site[, species_exclude] <- list(NULL)
cmd_species_class_site[, ions_exclude] <- list(NULL)
head(cmd_species_class_site)
dim(cmd_species_class_site)

### Others
# extreme_K = NULL
# thresholds_Seasonal99th = NULL
thresholds_TimesMean = NULL
extreme_events_remove = extreme_events_remain = extreme_events_replace = NULL

##### START the LOOP ##### 

# all_sites[!(all_sites %in% OOB_comb_avg$SiteCode)]
prefix_swb = sub("S_$", "", str_extract(prefix,  "^(.*?)S_"))
# prefix_swb = "CSN_noCsub_15t45uncAQS"

# avoid saving numeric as txt in csv files, if ending "options(scipen=0)"
options(scipen=999)


# supplement CSN sites with only data from 2016 or lack 12 months of MDL untill 2015, included in new datasets
# site_from2016_lack15mdl = 
#   c(11130003, 60731022, 180190010, 320310031, 340230011, 
#     380150003, 420710012, 490050007, 540390020, 120110034, 530330030)



# extract concentration & uncertainty of single cluster for PMF
for( site_code in unique(site_list$SiteCode)){ 
  # site_from2016_lack15mdl,  # site_code = unique(site_list$SiteCode)[40] #, "261630001"
  # site_code = unique(site_list$SiteCode)[7] #, 7-"BIBE1", 1-"ACAD1"
  
  tryCatch({
    
    # generate site.serial and corresponding datasets
    site.serial = site_list$serial.No[site_list$SiteCode == site_code]
    conc_site = subset(conc_pmf, SiteCode == site_code)
    row_count_org = nrow(conc_site)
    # summary(conc_site$EC1)
    # summary(conc_site)
    
    species_NA_intp_site = 
      subset(species_NA_intp, SiteCode == site_code &
               Date %in% conc_site$Date)
    conc_above_mdl_site = 
      subset(species_conc_above_mdl, SiteCode == site_code &
               Date %in% conc_site$Date)
    species_fullMDL_site =
      subset(species_daily_fullMDL, SiteCode == site_code &
               Date %in% conc_site$Date)
    
    # summary(species_NA_intp_site$Date == conc_site$Date)
    # summary(conc_above_mdl_site$Date == conc_site$Date)
    # summary(species_fullMDL_site$Date == conc_site$Date)
    # summary(names(species_fullMDL_site) == names(conc_above_mdl_site))
    # summary(names(species_fullMDL_site) == names(species_NA_intp_site))
    # summary(names(species_fullMDL_site) == names(conc_site))
    
    dim(conc_site)
    dim(species_NA_intp_site)
    
    ##### Extremes 1.1 - N*mean #####
    conc_site_forExe = conc_site
    # summary(conc_site_forExe)
    cols_comp = col_comp(conc_site_forExe, "Al", "PM25")
    mean_conc_site = conc_site_forExe[, lapply(.SD, mean), .SDcols = cols_comp]
    
    # med_conc_site*25 < max_conc_site
    
    # # mean value and 25 times were selected due to the data observation.
    # # however, for some site, mean * 20 may alreay miss some very high values
    # # for earth elements (Si, Al), mean * 20 may filter too many values
    # UPdate 2024.02, 1-if extreme values are detected in multiple featured species of a given source, then keep the extremes
    # UPdate 2024.02, 2-with threshold 25, extremes in many other sources were kept, influence the overall performance
    # UPdate 2024.02, 3-after checking more source peaks, using the value of 15
    times_value = 15
    extreme_conc_site = mean_conc_site * times_value
    
    # identify rows in conc_site_forExe with values higher than the extreme values
    # rows_to_remove_25mean <- apply(conc_site_forExe[, 4:ncol(conc_site_forExe)], 1, 
    #                        function(row) 
    #                          any(row > extreme_conc_site))
    
    # expand the extreme rows to the same as conc_site
    extreme_conc_site_exp = 
      extreme_conc_site %>% 
      slice(rep(1:n(), 
                each = nrow(conc_site_forExe)))
    
    # 1/3 of extreme to detect other high concentrations
    second_extreme_conc_site_exp = extreme_conc_site_exp/3
    
    thresholds_TimesMean = rbind(thresholds_TimesMean, extreme_conc_site)
    
    # write.csv(thresholds_TimesMean, "CSN_thresholds_25TimesMean.csv")
    # write.csv(thresholds_TimesMean, "CSN_site_thresholds_15TimesMean.csv")
    # write.csv(thresholds_TimesMean, "CSN_site_thresholds_pre15MDL_AQSmatchFirst_15TimesMean.csv")
    # write.csv(thresholds_TimesMean, "CSN_site_thresholds_pre15MDL_AQSmatchFirst_15TimesMean_supple.csv")
    
    write.csv(thresholds_TimesMean, "IMPROVE_site_thresholds_15TimesMean.csv")
    
    ##### Extremes 2 - decisions on how to deal with extremes #####
    
    # identify rows in conc_site with values higher than the extreme values
    conc_site_forExe_species = conc_site_forExe[, ..species_columns]
    
    # summary(names(extreme_conc_site_exp) == names(conc_site_forExe_species))
    # summary(names(second_extreme_conc_site_exp) == names(conc_site_forExe_species))
    
    conc_site_extreme_comp = 
      data.frame(conc_site_forExe_species > extreme_conc_site_exp)
    second_conc_site_extreme_comp = 
      data.frame(conc_site_forExe_species > second_extreme_conc_site_exp)
    
    # ### ONLY WHEN USING C-Subgroups!!! 
    # # special for EC1, C-sub are not included in source apportionment
    # Csub_col_to_false <- c("EC1", "EC2", "EC3", "OP", "OC1", "OC2", "OC3", "OC4")
    # conc_site_extreme_comp[, Csub_col_to_false] =
    #   lapply(conc_site_extreme_comp[, Csub_col_to_false], 
    #          function(x) FALSE)
    # second_conc_site_extreme_comp[, Csub_col_to_false] =
    #   lapply(second_conc_site_extreme_comp[, Csub_col_to_false], 
    #          function(x) FALSE)
    
    # extract days with extremes and high values (extreme/3)
    day_extreme = 
      conc_site_extreme_comp[rowSums(conc_site_extreme_comp) >= 1, ]
    day_high = 
      second_conc_site_extreme_comp[rowSums(second_conc_site_extreme_comp) >= 1, ]
    dim(day_extreme); dim(day_high)
    
    day_extreme$row_serial = row.names(day_extreme)
    day_high$row_serial = row.names(day_high)
    day_high = subset(day_high, row_serial %in% day_extreme$row_serial)
    
    # dim(day_extreme); dim(day_high)
    
    ##### detect days with extremes and whether the extreme is only for one species or there are covarying species
    ## do not consider the K-extreme days for now
    
    extreme_site <- data.table(
      SiteCode = character(0),
      serial.No = character(0),
      day_extreme_rows = integer(0),
      Date = as.Date(character(0)),
      extreme_species = character(0),
      co_detected_species_high = character(0),
      potential_source = character(0)
    )
    
    for (exe.row in 1:nrow(day_extreme)) {
      day_extreme_rows = as.integer(day_extreme$row_serial[exe.row])
      
      # species with extreme values in day_extreme & covarying species in day_high, if any
      species_extreme_true <- 
        names(day_extreme[exe.row, ])[day_extreme[exe.row, ] == TRUE]
      species_high_true <- names(day_high[exe.row,])[day_high[exe.row,] == TRUE]
      
      # check for matching source groups among species_extreme_true and species_high_true
      matched_species <- list()
      
      for (source_group in names(species_source_groups)) {
        
        # identify if any species in species_extreme_true & species_high_true be in this source_group
        species_in_group_extreme <- 
          species_extreme_true[
            species_extreme_true %in% 
              species_source_groups[[source_group]]]
        
        species_in_group_high <- 
          species_high_true[
            species_high_true %in% 
              species_source_groups[[source_group]]]
        
        if (length(species_in_group_extreme) > 0) {
          matched_species[[source_group]] <- 
            list(species_extreme = species_in_group_extreme, 
                 species_high = species_in_group_high)
        }
        
        # save the information for each source group where a match was found
        
        for (matched_source_group in names(matched_species)) {
          species_high = matched_species[[matched_source_group]]$species_high
          species_extreme = matched_species[[matched_source_group]]$species_extreme
          species_high_not_extreme = species_high[!(species_high %in% species_extreme)]
          
          # combine all species, if more than one, into a single string
          species_extreme_combined <- 
            paste(species_extreme, collapse = ", ")
          species_high_not_extreme_combined <- 
            paste(species_high_not_extreme, collapse = ", ")
          
          extreme_event_daily <-
            data.frame(
              # cluster.No = cluster.No,
              SiteCode = site_code,
              serial.No = site.serial,
              day_extreme_rows = day_extreme_rows, 
              Date = as.Date(conc_site_forExe$Date[day_extreme_rows]),
              extreme_species = species_extreme_combined, 
              co_detected_species_high = species_high_not_extreme_combined,
              potential_source = matched_source_group)
          
          extreme_site <- rbind(extreme_site, extreme_event_daily)
        }
      }
    }
    
    # remove duplicates if any
    extreme_site_use <- unique(extreme_site)
    
    # detect days potentially related BB & firework
    extreme_site_use$real_bb_firework = FALSE
    
    extreme_site_use$real_bb_firework[
      grepl("K", extreme_site_use$extreme_species)] = TRUE
    
    # remove the case when "K" is not in species_extreme and no covarying species, they are supposed to be replaced 
    extreme_site_use$extreme_species_count = 
      lengths(strsplit(extreme_site_use$extreme_species, ",\\s*"))
    extreme_site_use$covarying_species_count = 
      lengths(strsplit(extreme_site_use$co_detected_species_high, ",\\s*"))
    
    
    extreme_day_remove =
      subset(extreme_site_use, real_bb_firework)
    extreme_day_remove = 
      unique(
        select(
          extreme_day_remove,
          SiteCode, serial.No, 
          day_extreme_rows, Date, extreme_species, co_detected_species_high))
    
    extreme_day_replace = 
      subset(extreme_site_use, 
             extreme_species_count == 1 &
               covarying_species_count == 0 &
               (! Date %in% extreme_day_remove$Date))
    extreme_day_replace = 
      unique(
        select(
          extreme_day_replace,
          SiteCode, serial.No, 
          day_extreme_rows, Date, extreme_species, co_detected_species_high))
    
    extreme_day_remain =
      subset(extreme_site_use, 
             !(Date %in% extreme_day_replace$Date |
                 Date %in% extreme_day_remove$Date))
    extreme_day_remain = 
      unique(
        select(
          extreme_day_remain,
          SiteCode, serial.No, 
          day_extreme_rows, Date, extreme_species, co_detected_species_high))
    
    # extreme days to remove (firework or biomass)  
    rows_to_remove = 
      unique(extreme_day_remove$day_extreme_rows)
    # extreme days to keep (days with other source detected) 
    rows_to_remain = 
      unique(extreme_day_remain$day_extreme_rows)
    # extreme days to replace (no source detected)
    rows_to_replace = 
      unique(extreme_day_replace$day_extreme_rows)
    
    rowNo_replace = length(rows_to_replace)
    rowNo_remove = length(rows_to_remove)
    rowNo_remain = length(rows_to_remain)
    rowNo_after_remove = nrow(conc_site) - rowNo_remove
    
    # check if all rows with extreme(s) are classified
    # rowNo_replace + rowNo_remove + rowNo_remain == nrow(day_extreme)
    # length(unique(append(append(rows_to_remove, rows_to_remain), rows_to_replace))) == nrow(day_extreme)
    
    # combine for later check if the handling with extremes looks fine or not
    extreme_events_remove = rbind(extreme_events_remove, extreme_day_remove)
    extreme_events_remain = rbind(extreme_events_remain, extreme_day_remain)
    extreme_events_replace = rbind(extreme_events_replace, extreme_day_replace)
    
    ##### Extremes 3 - use random-forest to interpolate extremes to be replaced #####
    
    day_extreme_replace = 
      subset(day_extreme,
             row_serial %in% rows_to_replace)
    
    # for each species column, update conc_site and species_NA_intp_site where day_extreme_replace is TRUE
    # i.e, set point-to-be-replaced as NA
    conc_site_1 = conc_site
    
    for (species_col in species_columns) {
      rows_to_update <- 
        as.numeric(
          day_extreme_replace$row_serial[
            day_extreme_replace[[species_col]] == TRUE])
      
      conc_site_forExe[rows_to_update, (species_col) := NA]
      # extra points that are from interpolation
      species_NA_intp_site[rows_to_update, (species_col) := TRUE]
    }
    
    ## log all value to avoid negative interpolation 
    conc_site_noExe = conc_site[!as.integer(rows_to_remove), ]
    
    site_log = 
      cbind(conc_site_noExe[, 1:3],
            conc_site_noExe[, 4:ncol(conc_site_noExe)] %>% 
              dplyr::select(where(is.numeric)) %>%
              log())
    
    # replace NaN and Inf with NA in imp_mdl_other_log
    # when running missForest directly, error like "NA/NaN/Inf in foreign function call (arg 1)"
    sapply(site_log, function(x) any(is.infinite(x)))
    sapply(site_log, function(x) any(is.nan(x)))
    sapply(site_log, function(x) any(is.na(x)))
    
    site_log <- 
      site_log[, lapply(.SD, function(x) {
        x[is.nan(x)] <- NA  # Replace NaN with NA
        x[is.infinite(x)] <- NA  # Replace Inf and -Inf with NA
        return(x)
      })]
    
    #interpolation with missForest, using random forest, option "variablewise = T"
    site_intp_rf_mf = 
      missForest(site_log[, ..species_columns], 
                 variablewise = T)
    site_rf_conc = 
      cbind(site_log[, 1:3], 
            exp(site_intp_rf_mf$ximp))
    
    ##### Extremes 4 - remove rows that needs to be removed #####
    species_NA_site = species_NA_intp_site[!as.integer(rows_to_remove), ]
    species_fullMDL_site = species_fullMDL_site[!as.integer(rows_to_remove), ]
    
    #### set the MDL as the median across whole period
    # median_mdl = 
    #   species_fullMDL_site[, 
    #                        lapply(.SD, median), 
    #                        .SDcols = 3:ncol(species_fullMDL_site)]
    # species_fullMDL_site =
    #   cbind(select(species_fullMDL_site, SiteCode, Date), 
    #         median_mdl[rep(1:nrow(median_mdl), 
    #                        nrow(species_fullMDL_site)), ])
    # 
    summary(site_rf_conc$Date == species_NA_site$Date &
              species_fullMDL_site$SiteCode == species_NA_site$SiteCode)
    
    ##### Uncertainty estimation  ##### 
    
    conc_above_mdl_site <- conc_above_mdl_site[!as.integer(rows_to_remove), ]
    
    summary(conc_above_mdl_site$Date == site_rf_conc$Date)
    
    # expand error_fraction file to one with same row number as concentration file
    comp_ef <- 
      comp_error_fraction[1][
        rep(1, nrow(conc_above_mdl_site)), 
        .SD] # .SD selects the data for each repetition.
    
    # estimate the uncertainty 
    # EPA PMF5.0 user guide, page 16-17, function 5-1 & 5-2
    conc_above_mdl_species = conc_above_mdl_site[, ..species_columns]
    conc_rf_species = site_rf_conc[, ..species_columns]
    species_MDL_site = species_fullMDL_site[, ..species_columns]
    
    conc_rf_pmf = 
      conc_above_mdl_species * conc_rf_species +
      (!conc_above_mdl_species) * species_MDL_site * 0.5
    unc_rf_pmf = 
      conc_above_mdl_species * (((species_MDL_site / 2)^2 + 
                                   (comp_ef * conc_rf_species)^2)^0.5) +
      (!conc_above_mdl_species) * 5/6 * species_MDL_site
    
    # conc_above_mdl_species[1:5, 4:10]; conc_rf_species[1:5, 4:10]; species_MDL_site[1:5, 4:10]; comp_ef[1:5, 4:10]
    # conc_rf_pmf[1:5, 4:10]; unc_rf_pmf[1:5, 4:10]
    
    # PM2.5 unc
    conc_rf_pmf$PM25 = site_rf_conc$PM25
    unc_rf_pmf$PM25 = 3 * site_rf_conc$PM25
    
    # extra uncertainty for the interpolated points
    species_NA_intp_unc = species_NA_site[, 4:ncol(species_NA_site)]
    # species_NA_intp_unc[species_NA_intp_unc == TRUE] <- 1.5
    species_NA_intp_unc[species_NA_intp_unc == TRUE] <- 4.5
    species_NA_intp_unc[species_NA_intp_unc == FALSE] <- 1
    
    summary(names(unc_rf_pmf) == names(species_NA_intp_unc))
    
    unc_rf_pmf = unc_rf_pmf * species_NA_intp_unc
    
    # insert date info
    conc_rf_pmf <- cbind(site_rf_conc[, 1:3], conc_rf_pmf) # site_rf_conc[, 1:4]
    unc_rf_pmf <- cbind(site_rf_conc[, 1:3], unc_rf_pmf) # site_rf_conc[, 1:4]
    
    ##### Re-estimate signal-to-noise SNR after removing extremes #####
    conc.col = ncol(conc_rf_pmf)
    
    # according to EPA PMF 5.0 User Guide.pdf, function 5-3 & 5-4 from page 23
    conc_unc_diff_site = 
      (conc_rf_pmf[, ..species_columns] - 
         unc_rf_pmf[, ..species_columns])/
      unc_rf_pmf[, ..species_columns] * conc_above_mdl_species
    
    conc_unc_diff_site$PM25 = -999
    # conc_unc_diff_site[1:5, 4:10]
    
    # get the SNR signal-to-noise ratio 
    snr_selected_site = 
      colSums(conc_unc_diff_site)/
      nrow(conc_unc_diff_site)
    # convert the result to a one-row data.frame for later match
    snr_selected_site = data.frame(t(snr_selected_site))  
    
    ##### Bad-weak-strong 1. Strict SNR & Strict MDL - selected #####
    
    # SWB, Strong, Weak, or Bad classification
    
    # estimate the percent of species-specific fration of above MDL
    conc_rf_pmf_aboveMDL = data.frame(colSums(conc_above_mdl_species))
    conc_rf_pmf_aboveMDL$CompName = row.names(conc_rf_pmf_aboveMDL)
    names(conc_rf_pmf_aboveMDL)[1] = "above_MDL_count"
    conc_rf_pmf_aboveMDL$Percent = 
      conc_rf_pmf_aboveMDL$above_MDL_count * 100 / rowNo_after_remove
    
    # generate the weak & bad species based on the conc_vs_mdl 
    site.species.weak.Pmdl = conc_rf_pmf_aboveMDL$CompName[
      conc_rf_pmf_aboveMDL$Percent <= 50 &
        conc_rf_pmf_aboveMDL$Percent > 20]
    site.species.bad.Pmdl = conc_rf_pmf_aboveMDL$CompName[
      conc_rf_pmf_aboveMDL$Percent <= 20]
    # change to character
    site.species.weak.Pmdl = as.character(site.species.weak.Pmdl)
    site.species.bad.Pmdl = as.character(site.species.bad.Pmdl)
    
    # generate the weak & bad species based on SNR 
    site.species.bad.snr = colnames(snr_selected_site)[which(
      colSums(snr_selected_site) < 0.2)]
    site.species.weak.snr = colnames(snr_selected_site)[which(
      colSums(snr_selected_site) >= 0.2 &
        colSums(snr_selected_site) < 2)]
    
    ###### Bad-weak-strong 2. Combine the rate need interpolation & OBB error ######
    # Extract data for cluster
    site_oob = subset(OOB_comb_avg, 
                      SiteCode == site_code)
    site_miss = subset(miss_comb_avg, 
                       SiteCode == site_code)
    
    # get cluster average
    site_oob = data.frame(sapply(site_oob, mean))
    colnames(site_oob)[1] = "OOB"
    
    site_miss = data.frame(sapply(site_miss, mean))
    colnames(site_miss)[1] = "Miss.Flag"
    
    site_oob_miss = cbind(site_oob, site_miss)
    site_oob_miss$CompName = row.names(site_oob_miss)
    # only keep rows for CompName
    site_oob_miss = site_oob_miss[2:nrow(site_oob_miss), ]
    
    row.names(site_oob_miss) = NULL
    
    # bad, weak
    site.oob.miss.weak = site_oob_miss$CompName[
      (site_oob_miss$Miss.Flag <= 15 & 
         site_oob_miss$Miss.Flag > 10 &
         site_oob_miss$OOB <= 10 &
         site_oob_miss$OOB > 5) | 
        (site_oob_miss$Miss.Flag <= 10 &
           site_oob_miss$Miss.Flag > 5 &
           site_oob_miss$OOB > 8)]
    site.oob.miss.bad = site_oob_miss$CompName[
      site_oob_miss$Miss.Flag > 15 |
        (site_oob_miss$Miss.Flag <= 15 & 
           site_oob_miss$Miss.Flag > 10 &
           site_oob_miss$OOB > 5)]
    
    # for site need no interpolation for missing values, site.oob.miss.weak(bad) could be NA
    # ifelse{} only show the same type/length, so cannot be used here
    site.oob.miss.weak = 
      if(all(is.na(site.oob.miss.weak))) {
        character(0)
      } else {
        site.oob.miss.weak
      }
    site.oob.miss.bad = 
      if(all(is.na(site.oob.miss.bad))) {
        character(0)
      } else {
        site.oob.miss.bad
      }
    
    ###### Bad-weak-strong 3. other criteria ######
    
    # combine the "weak" & "bad" list
    site.species.bad = append(site.species.bad.snr, 
                              site.species.bad.Pmdl)
    site.species.bad = append(site.species.bad, 
                              site.oob.miss.bad)
    
    site.species.weak = append(site.species.weak.snr, 
                               site.species.weak.Pmdl)
    site.species.weak = append(site.species.weak, 
                               site.oob.miss.weak)
    
    # remove PM25, which potentially exists in the weak/bad list
    site.species.bad = site.species.bad[! site.species.bad %in% "PM25"]
    # site.species.weak = site.species.weak[! site.species.weak %in% "PM25"]
    
    # if OC/EC/OPC is in bad, force remove to weak
    oc.ec = c("EC", "EC1", "EC2", # "EC3", EC3 is almost always bad, then not reassign it as weak
              "OC", "OC1", "OC2", 
              "OC3", "OC4", "OP")
    
    oc.ec.bad = site.species.bad[site.species.bad %in% oc.ec]
    site.species.bad = site.species.bad[! site.species.bad %in% oc.ec]
    site.species.weak = append(site.species.weak, 
                               oc.ec.bad)
    
    # add below species into weak, if they are in bad, for chemical mass balance (Fe, Ca, etc.), or for special source (Ni, V)
    species.balance.source = 
      c("Ni", "V") # if adding "Al", "Ca", "Fe", "Si", "Ti", then force mass closure, but now give up
    species.balance.source.bad = 
      site.species.bad[site.species.bad %in% species.balance.source]
    site.species.bad = site.species.bad[! site.species.bad %in% species.balance.source]
    site.species.weak = append(site.species.weak, 
                               species.balance.source.bad)
    
    # combine all potential species that are removed from "bad" to "weak"
    species.bad.to.weak = append(oc.ec.bad, species.balance.source.bad)
    
    # remove the duplicated 
    species.bad.to.weak = species.bad.to.weak[!duplicated(species.bad.to.weak)]
    
    # if a species exist both in "bad" & "weak", define as "WEAK"
    site.species.bad = site.species.bad[! (
      site.species.bad %in% site.species.weak)]
    
    # ##### For CSN
    # # add "S", "Na", and "K" into bad to exclude co-linear effects with SO4, Na+, K+
    # site.species.bad = append(c("S", "Na", "K"), 
    #                              site.species.bad) 
    # # in case any of this three is in weak, if so, remove
    # site.species.weak = site.species.weak[! (
    #   site.species.weak %in% c("S", "Na", "K"))]
    
    # ##### For IMPROVE
    # # add "S into bad to exclude co-linear effects with SO4, Na+, K+
    # site.species.bad = append(c("S"), 
    #                              site.species.bad) 
    # # in case "S  is in weak, if so, remove
    # site.species.weak = site.species.weak[! (
    #   site.species.weak %in% c("S"))]
    
    
    # add Al, Mg, Na into weak, if they are not in bad, due to the lower reliability in XRF test
    if("NaIon" %in% species_columns){
      Al.Mg.weak = c("Al", "Mg")
    } else {
      Al.Mg.weak = c("Al", "Mg", "Na")
    }
    # determine if Al or Mg or Na is in bad, if so, remove the rest to "Al.Mg.weak"
    Al.Mg.weak = Al.Mg.weak[! (
      Al.Mg.weak %in% site.species.bad)]
    # add the rest in Al.Mg.weak to "weak"
    site.species.weak = append(Al.Mg.weak, 
                               site.species.weak) 
    # In CSN, "Na", "K" were removed, and use Na+, K+ instead
    
    # remove the duplicated
    # site.species.weak = site.species.weak[!duplicated(site.species.weak)]
    
    # remove the duplicated strings from the character vector
    site.species.bad = unique(
      unlist(
        strsplit(
          site.species.bad, " ")))
    site.species.weak = unique(
      unlist(
        strsplit(
          site.species.weak, " ")))
    
    ###### Bad-weak-strong 4. Distribution of values above MDL ######
    # species stay in "bad" and related to higher Pmdl
    site.species.bad.check = site.species.bad[
      site.species.bad %in% 
        site.species.bad.Pmdl]
    
    # remove those with >95% of value below MDL
    site.species.bad.check = 
      unique(
        subset(conc_rf_pmf_aboveMDL, 
               CompName %in% site.species.bad.check &
                 Percent > 5)$CompName)
    
    # site.species.bad.check = c("Ag", "Ca")
    if(length(site.species.bad.check) > 0){
      # files for those with above MDL concentrations
      conc_rf_pmf_mdl = 
        conc_rf_pmf[, 4:ncol(conc_rf_pmf)] * conc_above_mdl_species
      
      # subset those grouped as bad
      conc_rf_pmf_bad = conc_rf_pmf_mdl %>%
        select(all_of(site.species.bad.check))
      
      # replace 0 by NA
      conc_rf_pmf_bad <-
        data.frame(
          apply(
            conc_rf_pmf_bad, 
            2, 
            function(x) 
              replace(x, x == 0, NA)))
      
      # for concentrations above MDL, check the 10th vs. 90th percentile ratio
      # for some random comparison, r < 0.1 for strong species and < 0.2 for weak 
      # or, change to compare mean vs. sd? if mean < sd, or if mean > sd, already
      conc_rf_pmf_bad_mean = 
        conc_rf_pmf_bad %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::everything(),
            list(
              p = ~mean(., na.rm = T)
            )
          ))
      
      conc_rf_pmf_bad_sd = 
        conc_rf_pmf_bad %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::everything(),
            list(
              p = ~sd(., na.rm = T)
            )
          ))
      
      bad_sd_mean = conc_rf_pmf_bad_sd/conc_rf_pmf_bad_mean
      colnames(bad_sd_mean) = site.species.bad.check
      
      # Detect the species for which the sd > mean
      bad_comp <- which(colSums(bad_sd_mean > 1) > 0)
      site.species.bad.remove <- colnames(bad_sd_mean)[bad_comp]
      
    } else{
      site.species.bad.remove = NA
    }
    
    # remove the species with very scatter distribution from bad and add it into weak 
    site.species.bad = 
      site.species.bad[
        !(site.species.bad %in% 
            site.species.bad.remove)]
    
    site.species.weak = 
      append(site.species.weak, 
             site.species.bad.remove)
    site.species.weak = 
      site.species.weak[
        !is.na(site.species.weak)]
    
    # mark sites where PM25 is already "Weak" according to above criteria
    original_PM_weak = "No"
    if (sum(grepl("PM25", site.species.weak)) == 1)  {
      site.species.weak = site.species.weak[! site.species.weak %in% "PM25"]
      original_PM_weak = "Yes"
    }
    
    # arrange in alphabetic order 
    site.species.bad = sort(site.species.bad)
    site.species.weak = sort(site.species.weak)
    
    site.species.weak = append(site.species.weak, "PM25")
    site.species.strong = unique(
      subset(conc_rf_pmf_aboveMDL, 
             !(CompName %in% site.species.bad |
                 CompName %in% site.species.weak))$CompName)
    site.species.strong = site.species.strong[! site.species.strong %in% "PM25"]
    site.species.strong = as.character(site.species.strong)
    
    
    ##### Output 1:  GUI #####
    # remove species marked as bad
    # Check if site.species.bad is NULL or not
    if (is.null(site.species.bad)) {
      conc_rf_pmf_gui <- conc_rf_pmf
      unc_rf_pmf_gui <- unc_rf_pmf
    } else {
      conc_rf_pmf_gui <- conc_rf_pmf[, !site.species.bad, with=FALSE]
      unc_rf_pmf_gui <- unc_rf_pmf[, !site.species.bad, with=FALSE]
    }
    
    conc_rf_pmf_gui$State = unc_rf_pmf_gui$State = 
      conc_rf_pmf_gui$SiteCode = unc_rf_pmf_gui$SiteCode = NULL
    
    # extra uncertainty for species that were in bad but removed to weak
    if(!identical(species.bad.to.weak, character(0))){ 
      unc_rf_pmf_gui[, (species.bad.to.weak) := 
                       lapply(.SD, function(x) x * 3), 
                     .SDcols = species.bad.to.weak]
    } else {species.bad.to.weak = NA}
    
    # summary(select(unc_rf_pmf_gui, Al, Ni, Pb, Ti, V, Zn)); summary(select(unc_rf_pmf, Al, Ni, Pb, Ti, V, Zn))
    
    write.csv(conc_rf_pmf_gui,
              file = file.path(
                dropbox_path, GUI.site.folder, 
                paste0(prefix, 
                       site.serial, "_conc.csv")),
              row.names = FALSE)
    write.csv(unc_rf_pmf_gui,
              file = file.path(
                dropbox_path, GUI.site.folder, 
                paste0(prefix, 
                       site.serial, "_unc.csv")),
              row.names = FALSE)
    
    # siteRow_in_site = data.frame(table(conc_rf_pmf_gui$SiteCode))
    # colnames(siteRow_in_site) = c("SiteCode", "Freq")
    
    # write.csv(siteRow_in_site,
    #           paste0(GUI.site.folder, "/", prefix, 
    #                  site.serial, "_date_number_of_site.csv"))
    
    # for those marked as weak, set the uncertainty 3 times as much
    # unc_rf_pmf_gui_1 = unc_rf_pmf_gui
    
    
    ######### output 2.1: non-GUI, conc & unc #########
    
    unc_rf_pmf_cmd = unc_rf_pmf_gui
    conc_rf_pmf_cmd = conc_rf_pmf_gui
    
    # unc_rf_pmf_cmd[names(unc_rf_pmf_cmd) %in% site.species.weak] = 
    #   3 * unc_rf_pmf_cmd[names(unc_rf_pmf_cmd) %in% site.species.weak]
    unc_rf_pmf_cmd[, (site.species.weak) := 
                     lapply(.SD, 
                            function(x) 
                              x * 3), 
                   .SDcols = site.species.weak]
    
    # for PM2.5, set the uncertainty 3*3 times as much
    # unc_rf_pmf_cmd$PM25 = 3 * unc_rf_pmf_cmd$PM25, done earlier
    
    # unc_rf_pmf_cmd[1:5, 1:10]; conc_rf_pmf_cmd[1:5, 1:10]
    
    # add info into column names before combination
    names(conc_rf_pmf_cmd) = paste0("conc_", names(conc_rf_pmf_cmd))
    names(unc_rf_pmf_cmd) = paste0("unc_", names(unc_rf_pmf_cmd))
    
    # summary(conc_rf_pmf_cmd$conc_Date == unc_rf_pmf_cmd$unc_Date)
    
    # combining conc & unc files
    cmd_conc_unc = cbind(conc_rf_pmf_cmd, unc_rf_pmf_cmd)
    
    # Interleave columns of concentration and uncertainty files
    ## generate a new vector to index the columns of cbind file
    interleave.col.order <- 
      rep(1:ncol(unc_rf_pmf_cmd), each = 2) + 
      (0:1) * ncol(unc_rf_pmf_cmd)
    
    ## reorder the cbind file
    # cmd_input_interleave = cmd_conc_unc[interleave.col.order] # for data.frame only
    cmd_input_interleave = 
      as.data.table(
        as.matrix(
          cmd_conc_unc)[, 
                        interleave.col.order])
    
    # only keep one Date & cluster column
    cmd_input_interleave$unc_Date = NULL
    names(cmd_input_interleave)[1] = "Date"
    # head(cmd_input_interleave)
    
    cmd_input_interleave = 
      cmd_input_interleave[
        with(cmd_input_interleave, 
             order(Date)), ]
    
    write.csv(cmd_input_interleave, 
              file = file.path(
                dropbox_path, nonGUI.site.folder, 
                paste0(prefix, site.serial, "_CMD.csv")),
              row.names = FALSE)
    
    ###### output 2.2: non-GUI, weak, bad or strong ######
    # get the corresponding row number
    cmd_class_rowNo = 
      as.integer(
        row.names(
          cmd_species_class_site[
            cmd_species_class_site$serial.No == site.serial, ]))
    
    # assign values
    cmd_species_class_site[cmd_class_rowNo, site.species.strong] <- 1
    cmd_species_class_site[cmd_class_rowNo, site.species.weak] <- 0
    cmd_species_class_site[cmd_class_rowNo, site.species.bad] <- NA
    
    cmd_species_class_site$sum.weak.good[cmd_class_rowNo] = 
      length(site.species.weak) +
      length(site.species.strong) 
    
    # adding "," into species.bad.to.weak so as to add it into the dataframe
    if(length(species.bad.to.weak) > 1) {
      species.bad.to.weak = paste(species.bad.to.weak, collapse = ", ")
    } else{species.bad.to.weak = species.bad.to.weak}
    
    # add other information into the summary table
    cmd_species_class_site$site.row[cmd_class_rowNo] = rowNo_after_remove
    cmd_species_class_site$extreme_rowNo_remain[cmd_class_rowNo] = rowNo_remain
    cmd_species_class_site$extreme_rowNo_replace[cmd_class_rowNo] = rowNo_replace
    cmd_species_class_site$extreme_rowNo_remove[cmd_class_rowNo] = rowNo_remove
    cmd_species_class_site$row_count_org[cmd_class_rowNo] = row_count_org
    cmd_species_class_site$original_PM_weak[cmd_class_rowNo] = original_PM_weak
    cmd_species_class_site$SiteCode[cmd_class_rowNo] = site_code
    cmd_species_class_site$species.bad.to.weak[cmd_class_rowNo] = species.bad.to.weak
    cmd_species_class_site[(cmd_class_rowNo-1):cmd_class_rowNo, ]
    
    # detect the species for None-GUI PMF, thus, weak and strong species
    nonGUI_disp_species <- 
      cmd_species_class_site[
        cmd_class_rowNo, 
        c(site.species.strong, site.species.weak)]
    species.name.use = species.name[
      species.name %in% 
        c(site.species.strong, site.species.weak)]
    
    # add PM25
    # species.name.use = append(species.name.use, "PM25")
    
    nonGUI_disp_species <- nonGUI_disp_species[, species.name.use]
    cmd_species_class_site$style.weak.good[cmd_class_rowNo] <- 
      paste0("/", nonGUI_disp_species, collapse = "")
    # cmd_species_class_site[cmd_class_rowNo, ]
    
    write.csv(cmd_species_class_site, 
              file = file.path(
                dropbox_path, nonGUI.site.folder, 
                paste0(prefix_swb, "PMF_SWB_site.csv")), # PMF_SWB_site_supple.csv
              row.names = FALSE)
    
    write.csv(extreme_events_remove, 
              file = file.path(
                dropbox_path, nonGUI.site.folder, 
                paste0(prefix_swb, "extreme_remove.csv")), # extreme_remove_supple.csv
              row.names = FALSE)
    
    write.csv(extreme_events_remain, 
              file = file.path(
                dropbox_path, nonGUI.site.folder, 
                paste0(prefix_swb, "extreme_remain.csv")), # extreme_remain_supple.csv
              row.names = FALSE)
    
    write.csv(extreme_events_replace, 
              file = file.path(
                dropbox_path, nonGUI.site.folder, 
                paste0(prefix_swb, "extreme_replace.csv")), # extreme_replace_supple.csv
              row.names = FALSE)
  }, error=function(e) {
    print(paste("Error at SiteCode", site_code, site.serial, ":", e$message))
  })
}



#### Copy files to Dropbox ####

# # #### CSN, extreme, mean*25 as threshold of outlier
# # Clusterfolder <- "CSN_NoGUI_NoCsub_25TimesMean_cluster" 
# Sitefolder = "CSN_NoGUI_NoCsub_25TimesMean_Site"
# prefix = "CSN_noCsub_25timesMean_C_"
# 
# # #### CSN, extreme, mean*15 as threshold of outlier
# Sitefolder = "CSN_NoGUI_NoCsub_15TimesMean_site"
# prefix = "CSN_noCsub_15timesMean_C_"

# #### IMPROVE, extreme, mean*15 as threshold of outlier
Sitefolder = "IMPROVE_NoGUI_Csub_15t1mdlVNi_Site"
prefix = "IMPROVE_Csub_15t1mdlVNi_S_"

# dropbox_path = "/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/National_SA_PMF"
dropbox_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_PMF"
# cluster_folder = paste0(dropbox_path, "/", Clusterfolder)
site_folder = paste0(dropbox_path, "/", Sitefolder)

###### for cluster
# create the destination folder if it does not exist
if (!dir.exists(cluster_folder)) {
  dir.create(cluster_folder, recursive = TRUE)
}

# cluster_files <- list.files(paste0(data.dir, "/", Clusterfolder), full.names = TRUE)
# # copy each file to the destination folder
# for (cluster_file in cluster_files) {
#   file.copy(cluster_file, cluster_folder)
# }

###### for site
# create the destination folder if it does not exist
if (!dir.exists(site_folder)) {
  dir.create(site_folder, recursive = TRUE)
}

site_files <- list.files(paste0(data.dir, "/", Sitefolder), full.names = TRUE)
# copy each file to the destination folder
for (site_file in site_files) {
  file.copy(site_file, site_folder)
}

#### Copy files to OneDrive for backup ####

dropbox_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_PMF"
onedrive_path = "/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nationwide_SA/data/pmf/inputs"


#### Dispersion Normalization ####

##### VC for dispersion normalization, based on ERA5 data 

#### data to be normalized
# Sitefolder = "CSN_NoGUI_NoCsub_15TimesMean"
# Sitefolder = "CSN_NoGUI_NoCsub_15t1mdl0unc"
# Sitefolder = "CSN_NoGUI_NoCsub_15t1mdlVNi"
# Sitefolder = "CSN_NoGUI_Csub_15t1mdlVNi"

# Sitefolder = "IMPROVE_NoGUI_Csub_15t1mdlVNi"
# Sitefolder = "IMPROVE_NoGUI_Csub_15tAmmIonVNi"
Sitefolder = "IMPROVE_NoGUI_NoCsub_15tAmmIonVNi"


# define the folder of output results
dropbox_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_PMF"
file_folder = paste0(dropbox_path, "/", Sitefolder, "_Site")

# create folder for new data
dn_site_folder = paste0(Sitefolder, "_DN_Site")

file.path(dropbox_path, dn_site_folder)
if (!dir.exists(file.path(dropbox_path, dn_site_folder))) {
  dir.create(file.path(dropbox_path, dn_site_folder))
}

# read dispersion normalization data
dn_vc = fread("Nearest_ERA5_Wind_BLH_VC_CSN&IMPROVE.csv")
site_code_serial = fread("CSN_IMPROVE_site.serial.csv"); site_code_serial$V1 = NULL

# get VC coefficient
# here, the VC_coef is already the VC_daily/VC_mean, but some days have been removed
# thus, we re-estimated the VC_coef for each site
dn_vc$V1 = NULL
# dn_vc$Date = as.Date(dn_vc$Date)

# reorder the dataframe
dn_vc = 
  dn_vc[
    with(dn_vc,
         order(Dataset, SiteCode, Date)), ]

dn_vc_site = join(dn_vc, site_code_serial)
dn_vc_site$serial.No =  
ifelse(dn_vc_site$serial.No < 100,
       sprintf("%03d", dn_vc_site$serial.No),
       as.character(dn_vc_site$serial.No))

# list all files used for DN
nongui_org_csvs <- 
  list.files(file_folder, 
             pattern = ".*CMD\\.csv$", full.names = TRUE)

# nongui_org_csvs <- 
#   list.files(file_folder, 
#              pattern = ".*[0-9]\\.csv$", full.names = TRUE)

# create a file to store updated VC_coef (after removing some days of records)
site_vc_all = NULL

# conduct DN
for (nongui_org_csv in nongui_org_csvs) { # [140:length(nongui_org_csvs)]
  # site_serial = sub(".*_S_([0-9]+)_.*", "\\1", basename(nongui_org_csv))
  file_name = basename(nongui_org_csv)
  dn_file_name = paste0(sub("\\..*", "", file_name), "_DN.csv")
  site_serial = regmatches(file_name, 
                           regexpr("(?<=_S_)(\\d{3})(?=_cmd)?", 
                                   file_name, 
                                   perl = TRUE))
  nongui_conc_unc = read.csv(nongui_org_csv)
  nongui_conc_unc$Date = as.Date(nongui_conc_unc$Date)
  # nongui_conc_unc$Date = as.Date(nongui_conc_unc$Date, format = "%m/%d/%y")
  # head(nongui_conc_unc)
  nongui_conc_unc$X = NULL
  
  # extract VC for the site
  site_dn_vc = subset(dn_vc_site, serial.No == site_serial) 
    
  # exclude potentially removed dates
  site_dn_vc = 
    subset(site_dn_vc, Date %in% nongui_conc_unc$Date)
  site_dn_vc = data.frame(site_dn_vc)
  site_dn_vc = site_dn_vc[!duplicated(site_dn_vc), ] 
  
  # date format
  site_dn_vc$Date = as.Date(site_dn_vc$Date)
  
  # estimated VC mean of the whole study period
  site_vc_mean = mean(site_dn_vc$VC.daily, na.rm = TRUE)
  
  # update the VC_coef
  site_dn_vc$VC.mean = site_vc_mean
  site_dn_vc$VC_coef = site_dn_vc$VC.daily/site_dn_vc$VC.mean
  
  # DN for each cluster/site
  nongui_conc_unc_dn = nongui_conc_unc
  for (col in names(nongui_conc_unc)) {
    # normalize each column in turn
    if (!(col %in% c("SiteCode", "Date", "State"))) {
      nongui_conc_unc_dn[[col]] =
        nongui_conc_unc[[col]] * site_dn_vc$VC_coef
    }
  }
  
  write.csv(nongui_conc_unc_dn, 
            file.path(dropbox_path, dn_site_folder, dn_file_name),
            row.names = FALSE)
  
  # Upated VC coefficient file
  site_vc_all = rbind(site_vc_all, site_dn_vc)
  
  write.csv(site_vc_all, 
            file.path(dropbox_path, dn_site_folder, 
                      "Nearest_ERA5_Wind_BLH_VC_CSN_2024.07.csv"),
            row.names = FALSE)
}


#### map the spatial distribution of VC_coefficient
library(usmap)
library(USAboundaries)

site_geoid = read.csv("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Intp_IMPROVE_CSN/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
site_geoid$X = NULL
site_geoid = site_geoid[!duplicated(site_geoid), ] 
site_vc_all_plot = site_vc_all[!duplicated(site_vc_all), ] 
dim(site_vc_all_plot)

geoid_dn_coef = 
  join(site_vc_all_plot, 
        select(site_geoid, 
               Dataset, SiteCode, Longitude, Latitude, geoid, RuralUrban, state_abbr))
dim(geoid_dn_coef)

geoid_dn_coef$year = year(geoid_dn_coef$Date)
geoid_dn_coef$month = month(geoid_dn_coef$Date)

geoid_dn_coef = 
  geoid_dn_coef %>%
  mutate(year_month = 
           as.Date(
             paste(year, month, "01", sep = "-")))
head(geoid_dn_coef)

ggplot(geoid_dn_coef,
       aes(x = as.factor(year_month), y = VC_coef)) +
  geom_boxplot() +
  theme_minimal()

ggplot(geoid_dn_coef,
       aes(x = as.factor(Date), y = VC_coef)) +
  geom_boxplot() +
  theme_minimal()
 
sapply(geoid_dn_coef, class)
site_geoid_dn_coef = 
  geoid_dn_coef %>% 
  group_by(state_abbr, SiteCode) %>%
  dplyr::summarise(
    VC_coef_median = median (VC_coef),
    VC_coef_mean = mean (VC_coef),
    Longitude = median (Longitude),
    Latitude = median (Latitude),
    geoid = median (geoid))

UScounty <- map_data("county")
ggplot(site_geoid_dn_coef, 
       aes(Longitude, Latitude, color= VC_coef_median)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  theme_void()

#### Explore the removed/replaced data ####
extreme_remove = 
  read.csv(file.path(
  dropbox_path, nonGUI.site.folder, 
  paste0(prefix_swb, "extreme_remove.csv")))

extreme_replace = 
  read.csv(file.path(
    dropbox_path, nonGUI.site.folder, 
    paste0(prefix_swb, "extreme_replace.csv")))
freq_replace = data.frame(table(extreme_replace$extreme_species))
names(freq_replace)[1] = "Species"

species_class = read.csv("/Users/TingZhang/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_Species_class_sub.csv")
species_class$Species[nrow(species_class)] = "PM2.5"

freq_replace = merge(freq_replace, species_class, all.x = TRUE)

ggplot(freq_replace,
       aes(x = reorder(Species, sequence))) +
  geom_bar(aes(y = Freq), 
           stat = "identity", width = 0.6, alpha = 0.8) +
  scale_x_discrete(labels = function(x) format_variable(x)) +
  xlab(format_variable("PM25 Species")) +
  ylab(format_variable("Frequency")) + 
  theme_bw() +
  theme_text_speciesName
  







