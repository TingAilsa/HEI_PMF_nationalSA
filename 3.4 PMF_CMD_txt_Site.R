##clear environment
# rm(list=ls())

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_no_GUI/iniparams_templates")
getwd()
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp"

##packages in need
library(tidyr) 
library(stats) 
library(ggplot2)
library(scales) 
library(dplyr)
library(plyr)
library(stringr)
# library(file.copy)

#### A. create cluster & sub-factor folders for sites, DONE!!!  ####

# midfix = "25TimesMean"
# midfix = "noSeason99"

dropbox_path = "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/"
dropbox_site = paste0(dropbox_path, "CSN_NoGUI_NoCsub_", midfix, "_Site")

# create folder to hold the site-specific subfolders
# new_cluster_folder = paste0(data.dir, "/", "CSN_CMD_noCsub_", midfix, "_Site")
# dir.create(file.path(new_cluster_folder), showWarnings = FALSE)

# dropbox_sitedate = paste0(dropbox_path, "CSN_NoGUI_NoCsub_", midfix, "_Site_SiteDate")
# dir.create(file.path(dropbox_sitedate), showWarnings = FALSE)

#  all CSV files in the dropbox_site folder
# site_files <- list.files(path = dropbox_site, pattern = ".*_C_.*_.*\\.csv", full.names = TRUE)
site_files <- list.files(path = dropbox_site, pattern = ".*_S_.*\\.csv", full.names = TRUE)

########### ONLY used when selecting the some Cluster_Site groups for sensitivity analyses in 2024.01
# extract the pattern (ClusterNo_SiteCode) "_.*_.*" between "C_" and ".csv"
ClusterNo_SiteCode_pattern <- 
  sapply(site_files, function(file) {
  matches <- regmatches(file, regexpr("(?<=_C_).*(?=\\.csv)", file, perl = TRUE))
  if(length(matches) > 0) return(matches)
  return(NA)
})

# Format as B_AAAAAs=("B_12345" "B_67890" "B_ABCDE" ...)
cluster_sites <- paste('cluster_sites=("', 
                       paste(ClusterNo_SiteCode_pattern, collapse='" "'), 
                       '")', 
                       sep='')
print(cluster_sites)
# output ignoring "\" and use for Slurm scripts
cat(cluster_sites)
########### ONLY used when selecting the some Cluster_Site groups for sensitivity analyses in 2024.01

# Process each file
for (site_file in site_files) {
  # Extract cluster_site number from the file name
  #cluster_site <- str_extract(basename(site_file), "C_.*(?=\\.csv)")
  cluster_site <- str_extract(basename(site_file), "S_.*(?=\\.csv)")
  
  # Create a new folder for cluster_site 
  cluster_site_folder <- file.path(new_cluster_folder, cluster_site)
  
  if (!dir.exists(cluster_site_folder)) {
    dir.create(cluster_site_folder, recursive = TRUE)
  }
  
  # create subfolders Factor_6 to Factor_11 under cluster_site
  for (i in 5:11) {
    subfolder <- file.path(cluster_site_folder, paste0("Factor_", i))
    if (!dir.exists(subfolder)) {
      dir.create(subfolder, recursive = TRUE)
    }
  }
  
  # prepare files for non-GUI PMF & site-date-PM2.5 info for later analysis 
  site_file_nonGUI = read.csv(site_file)
  
  colnames(site_file_nonGUI)[1] = "SerialNumber"
  site_PM_date = select(site_file_nonGUI, 
                        SerialNumber, SiteCode, Date, State, conc_PM25)
  colnames(site_PM_date)[ncol(site_PM_date)] = "PM25"
  
  site_file_nonGUI$SiteCode = site_file_nonGUI$Date = site_file_nonGUI$State = NULL
  
  # create the output file paths
  path_PM_date <- file.path(dropbox_sitedate, 
                            paste0("CSN_noCsub_", midfix, "_", cluster_site, "_PM_Date.csv"))
  path_nonGUI <- file.path(cluster_site_folder, 
                           paste0("CSN_noCsub_", midfix, "_", cluster_site, ".csv"))
  
  # Save site_PM_date to dropbox_sitedate folder
  write.csv(site_PM_date, path_PM_date, row.names = FALSE)
  
  # Save site_file_nonGUI to cluster_site_folder
  write.csv(site_file_nonGUI, path_nonGUI, row.names = FALSE)
}


#### B1 start. read iniparams files  ####
base_par_org = readLines("iniparams_base_1.txt")
BS_par_org = readLines("iniparams_BS_2.txt")
DISP_par_org = readLines("iniparams_DISP_3.txt")
before_BS_DISP_par_org = readLines("iniparams_BS_PHASE_of_BSDISP_4.txt")
BS_DISP_par_org = readLines("iniparams_BS-DISP_5.txt")

factor.number = c(5:11)

#### B2. edit and output new iniparams.txt ####

### 25TimesMean, CSN
site_sum = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_NoGUI_NoCsub_25TimesMean_Site/CSN_noCsub_25timesMean_PMF_CMD_StrongWeakBad_Site.csv")
site_folder_pathway = paste0(data.dir,"/CSN_CMD_noCsub_25TimesMean_Site")
name.prefix = "CSN_noCsub_25TimesMean_" # prefix in names for input/output files

### 99thSeasonal, CSN
site_sum = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_NoGUI_NoCsub_99season_Site/CSN_noCsub_noSeason99_PMF_CMD_StrongWeakBad_Site.csv")
site_folder_pathway = paste0(data.dir,"/CSN_CMD_noCsub_noSeason99_Site")
name.prefix = "CSN_noCsub_noSeason99_" # prefix in names for input/output files

site_sum$X = NULL
site_sum$serial.No =
  ifelse(site_sum$serial.No < 100,
         sprintf("%03d", site_sum$serial.No),
         as.character(site_sum$serial.No))

site_sum_serial = select(site_sum, serial.No, SiteCode)
site_sum_serial = site_sum_serial[with(site_sum_serial, order(serial.No)), ]

site_folder <- list.dirs(site_folder_pathway, recursive = FALSE, full.names = TRUE)
cluster_site_select <- basename(site_folder)

for(cluster_site in cluster_site_select){
  
  ######### sensitivity analysis of individual site selection 
  ## data of selected cluster
  cluster.No = str_extract(cluster_site, "(?<=C_)[A-Za-z0-9]+(?=_)")
  site.code <- sub(".*_", "", cluster_site)
  site_info = subset(site_sum, 
                     Finaly.Decision == cluster.No & SiteCode == site.code)
  
  ######### all single site analysis
  site.code = sub(".*_", "", cluster_site)
  site_info = subset(site_sum, 
                     serial.No == site.code)
  
  # specific info to use for the site
  site.row = site_info$site.row
  variable.NO = site_info$sum.weak.good
  ## weak0 or strong1 for PM2.5 species 
  wead.strong.assign = site_info$style.weak.good
  
  species_col = col_comp(site_info, "Al", "PM25")
  rowSums(site_info[, species_col], na.rm = T)
  rowSums(site_info[, species_col] == 0, na.rm = T)
  rowSums(site_info[, species_col] == 1, na.rm = T)
  
  # change to the style used for txt
  # remove the first "/", and change the rest to "\t"
  wead.strong.assign = gsub("/", "\\\t", 
                            sub("^/", "", wead.strong.assign))
  
  # replace the number of factors to use and name of output files
  for (j in factor.number){
    base_par = base_par_org
    BS_par = BS_par_org
    DISP_par = DISP_par_org
    before_BS_DISP_par = before_BS_DISP_par_org
    BS_DISP_par = BS_DISP_par_org
    
    # output path
    # path.CF = paste0(site_folder_pathway, cluster_site, "/Factor_", j, "/")
    path.CF = paste0(site_folder_pathway, "/", cluster_site, "/Factor_", j, "/")
    
    # replace with the row, variable, & factor number 
    base_par = row_var_factor(base_par, site.row, variable.NO, j)
    BS_par = row_var_factor(BS_par, site.row, variable.NO, j)
    DISP_par = row_var_factor(DISP_par, site.row, variable.NO, j)
    before_BS_DISP_par = row_var_factor(before_BS_DISP_par, site.row, variable.NO, j)
    BS_DISP_par = row_var_factor(BS_DISP_par, site.row, variable.NO, j)
    
    ##### B1: iniparams for Base run #####
    # create and replace the names for base run input & output files
    
    base.input = paste0(name.prefix, cluster_site, ".csv")
    output.pre = paste0(name.prefix, cluster_site, "_F_", j)
    
    base.output = paste0(output.pre, "_")
    
    base_par = base_input_output(base_par, base.input, base.output)
    
    write.table(base_par, 
                file = paste0(path.CF, "iniparams_base_", 
                              cluster_site,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    ##### B2: iniparams common input for BS (bootstrap) & DISP files #####
    # create and replace the name of input file
    BS.DISP.input.1 = base.input
    BS.DISP.input.2 = paste0(output.pre, "_.dat")
    
    BS.output = paste0(output.pre, "_BS_")
    DISP.output = paste0(output.pre, "_DISP_")
    before_BS_DISP.output = paste0(output.pre, "_before_BS_DISP_")
    BS_DISP.output = paste0(output.pre, "_BS_DISP_")
    
    BS_par = bs_disp_input_output(BS_par, 
                                  BS.DISP.input.1, 
                                  BS.DISP.input.2, 
                                  BS.output)
    
    DISP_par = bs_disp_input_output(DISP_par, 
                                    BS.DISP.input.1, 
                                    BS.DISP.input.2, 
                                    DISP.output)
    DISP_par = dispbcmask_rp(DISP_par, 
                             wead.strong.assign, 
                             j)
    
    before_BS_DISP_par = bs_disp_input_output(before_BS_DISP_par, 
                                              BS.DISP.input.1, 
                                              BS.DISP.input.2, 
                                              before_BS_DISP.output)
    
    BS_DISP_par = bs_disp_input_output(BS_DISP_par, 
                                       BS.DISP.input.1, 
                                       BS.DISP.input.2, 
                                       BS_DISP.output)
    
    ##### B3: output BS (bootstrap) & DISP files #####
    write.table(BS_par, 
                file = paste0(path.CF,"iniparams_BS_", 
                              cluster_site,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    write.table(DISP_par, 
                file = paste0(path.CF, "iniparams_DISP_", 
                              cluster_site,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    write.table(before_BS_DISP_par, 
                file = paste0(path.CF, "iniparams_BS_DISP_before_", 
                              cluster_site,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    write.table(BS_DISP_par, 
                file = paste0(path.CF, "iniparams_BS_DISP_", 
                              cluster_site,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
  }
}
