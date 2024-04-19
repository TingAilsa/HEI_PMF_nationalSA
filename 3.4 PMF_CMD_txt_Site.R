##clear environment
# rm(list=ls())

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_non-GUI_files/iniparams_templates")
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

#### A. create cluster & sub-factor folders for sites  ####

# midfix = "15tMean_0unc"
# midfix = "15TimesMean"
# midfix = "15tMean"
# midfix = "25TimesMean"
# midfix = "noSeason99"
# midfix = "15t1mdl0unc"

dropbox_path = "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/"
dropbox_site = paste0(dropbox_path, "CSN_NoGUI_NoCsub_", midfix, "_Site")

# create folder to hold the site-specific subfolders
new_file_folder = paste0(data.dir, "/", "CSN_CMD_noCsub_", midfix, "_Site")
dir.create(file.path(new_file_folder), showWarnings = FALSE)

###### post-process script updates, no longer needed
# dropbox_sitedate = paste0(dropbox_path, "CSN_NoGUI_NoCsub_", midfix, "_Site_SiteDate")
# dir.create(file.path(dropbox_sitedate), showWarnings = FALSE)

#  all CSV files in the dropbox_site folder
# site_files <- list.files(path = dropbox_site, pattern = ".*_C_.*_.*\\.csv", full.names = TRUE)
site_files <- list.files(path = dropbox_site, pattern = ".*_S_.*\\.csv", full.names = TRUE)

########### ONLY used when selecting the some site_serial groups for sensitivity analyses in 2024.01
# # extract the pattern (ClusterNo_SiteCode) "_.*_.*" between "C_" and ".csv"
# ClusterNo_SiteCode_pattern <- 
#   sapply(site_files, function(file) {
#   matches <- regmatches(file, regexpr("(?<=_C_).*(?=\\.csv)", file, perl = TRUE))
#   if(length(matches) > 0) return(matches)
#   return(NA)
# })
# 
# # Format as B_AAAAAs=("B_12345" "B_67890" "B_ABCDE" ...)
# site_serials <- paste('site_serials=("', 
#                        paste(ClusterNo_SiteCode_pattern, collapse='" "'), 
#                        '")', 
#                        sep='')
# print(site_serials)
# # output ignoring "\" and use for Slurm scripts
# cat(site_serials)
########### ONLY used when selecting the some site_serial groups for sensitivity analyses in 2024.01

# create folder to save iniparams.txt files and copy csv files for non-GUI
for (site_file in site_files) {
  # Extract site_cmd number from the file name
  #site_cmd <- str_extract(basename(site_file), "C_.*(?=\\.csv)")
  # site_cmd <- str_extract(basename(site_file), "S_.*(?=\\.csv)")
  site_cmd <- sub(".*_(S[_-]\\d+)_.*", "\\1", basename(site_file))
  
  # Create a new folder for site_cmd 
  site_cmd_folder <- file.path(new_file_folder, site_cmd)
  
  if (!dir.exists(site_cmd_folder)) {
    dir.create(site_cmd_folder, recursive = TRUE)
  }
  
  # create subfolders Factor_6 to Factor_11 under site_cmd
  for (i in 3:11) {
    subfolder <- file.path(site_cmd_folder, paste0("Factor_", i))
    if (!dir.exists(subfolder)) {
      dir.create(subfolder, recursive = TRUE)
    }
  }
  
  ##### not working! There shall be no "date" format info in the input csv
  # file.copy(from = paste0(dropbox_site, "/", basename(site_file)),
  #           to = paste0(site_cmd_folder, "/", basename(site_file)))
  
  
  # prepare files for non-GUI PMF & site-date-PM2.5 info for later analysis
  site_file_nonGUI = read.csv(site_file)

  colnames(site_file_nonGUI)[1] = "SerialNumber"
  site_file_nonGUI$SerialNumber = 1:nrow(site_file_nonGUI)
  # site_PM_date = select(site_file_nonGUI,
  #                       SerialNumber, SiteCode, Date, State, conc_PM25)
  # colnames(site_PM_date)[ncol(site_PM_date)] = "PM25"
  # 
  # site_file_nonGUI$SiteCode = site_file_nonGUI$Date = site_file_nonGUI$State = NULL

  # create the output file paths
  # path_PM_date <- file.path(dropbox_sitedate,
  #                           paste0("CSN_noCsub_", midfix, "_", site_cmd, "_PM_Date.csv"))
  path_nonGUI <- file.path(site_cmd_folder,
                           paste0("CSN_noCsub_", midfix, "_", site_cmd, ".csv"))

  # Save site_PM_date to dropbox_sitedate folder
  # write.csv(site_PM_date, path_PM_date, row.names = FALSE)

  # Save site_file_nonGUI to site_cmd_folder
  write.csv(site_file_nonGUI, path_nonGUI, row.names = FALSE)
}


# files_to_remove <- list.files(path = new_file_folder, pattern = "_CMD\\.csv$", recursive = TRUE, full.names = TRUE)
# file.remove(files_to_remove)

#### B1 start. read iniparams files  ####
base_par_org = readLines("iniparams_base_1.txt")
BS_par_org = readLines("iniparams_BS_2.txt")
DISP_par_org = readLines("iniparams_DISP_3.txt")
before_BS_DISP_par_org = readLines("iniparams_BS_PHASE_of_BSDISP_4.txt")
BS_DISP_par_org = readLines("iniparams_BS-DISP_5.txt")

factor.number.series = c(3:11)

#### B2. edit and output new iniparams.txt ####

### 15TimesMean, CSN
site_sum = read.csv(file.path(dropbox_site, "CSN_noCsub_15t1mdl0unc_PMF_SWB_site.csv"))
site_sum = subset(read.csv(file.path(dropbox_site, "CSN_noCsub_15t1mdl0unc_PMF_SWB_site_supple.csv")), !is.na(PM25)) # sites only data from 2016, or lack 12-month MDL till 2015.11
site_folder_pathway = paste0(data.dir,"/CSN_CMD_noCsub_15t1mdl0unc_Site")
name.prefix = "CSN_noCsub_15t1mdl0unc_"
name.prefix.csv = "CSN_noCsub_15t1mdl0unc_"

# ### 15TimesMean, CSN
# site_sum = read.csv(file.path(dropbox_site, "CSN_noCsub_15timesMean_PMF_SWB_site.csv")
# # site_folder_pathway = paste0(data.dir,"/CSN_CMD_noCsub_15TimesMean_Site")
# # name.prefix = "CSN_noCsub_15TimesMean_" # prefix in names for input/output files
# site_folder_pathway = paste0(data.dir,"/CSN_CMD_noCsub_15tMean_0unc_Site")
# name.prefix = "CSN_noCsub_15tMean_0unc_"
# name.prefix.csv = "CSN_noCsub_15TimesMean_"

# ### 25TimesMean, CSN
# site_sum = read.csv(file.path(dropbox_site, "CSN_noCsub_25timesMean_PMF_CMD_StrongWeakBad_Site.csv")
# site_folder_pathway = paste0(data.dir,"/CSN_CMD_noCsub_25TimesMean_Site")
# name.prefix = "CSN_noCsub_25TimesMean_" # prefix in names for input/output files
# 
# ### 99thSeasonal, CSN
# site_sum = read.csv(file.path(dropbox_site, "CSN_noCsub_noSeason99_PMF_CMD_StrongWeakBad_Site.csv")
# site_folder_pathway = paste0(data.dir,"/CSN_CMD_noCsub_noSeason99_Site")
# name.prefix = "CSN_noCsub_noSeason99_" # prefix in names for input/output files

site_sum$X = NULL
site_sum$serial.No =
  ifelse(site_sum$serial.No < 100,
         sprintf("%03d", site_sum$serial.No),
         as.character(site_sum$serial.No))

site_sum_serial = select(site_sum, serial.No, SiteCode)
site_sum_serial = site_sum_serial[with(site_sum_serial, order(serial.No)), ]

site_folder <- list.dirs(site_folder_pathway, recursive = FALSE, full.names = TRUE)
site_folder_use <- basename(site_folder)

site_folder_use = paste0("S_", site_sum$serial.No )

#### B2.1 start the loop for iniparams.txt ####
for(site_serial in site_folder_use){
  # site_serial = site_folder_use[1]
  
  ######### 1. sensitivity analysis of individual site selection 
  ## data of selected cluster
  # cluster.No = str_extract(site_serial, "(?<=C_)[A-Za-z0-9]+(?=_)")
  # site.code <- sub(".*_", "", site_serial)
  # site_info = subset(site_sum, 
  #                    Finaly.Decision == cluster.No & SiteCode == site.code)
  
  ######### 2. all single site analysis
  site.code = sub(".*_", "", site_serial)
  site_info = subset(site_sum, 
                     serial.No == site.code)
  
  # specific info to use for the site
  site.row = site_info$site.row
  variable.NO = site_info$sum.weak.good
  ## weak0 or strong1 for PM2.5 species 
  weak.strong.assign = site_info$style.weak.good
  
  species_col = col_comp(site_info, "Al", "PM25")
  rowSums(site_info[, species_col], na.rm = T)
  rowSums(site_info[, species_col] == 0, na.rm = T)
  rowSums(site_info[, species_col] == 1, na.rm = T)
  
  # change to the style used for txt
  # remove the first "/", and change the rest to "\t"
  weak.strong.assign = gsub("/", "\\\t", 
                            sub("^/", "", weak.strong.assign))
  
  # replace the number of factors to use and name of output files
  for (factor.No in factor.number.series){
    base_par = base_par_org
    BS_par = BS_par_org
    DISP_par = DISP_par_org
    before_BS_DISP_par = before_BS_DISP_par_org
    BS_DISP_par = BS_DISP_par_org
    
    # output path
    # path.CF = paste0(site_folder_pathway, site_serial, "/Factor_", factor.No, "/")
    path.CF = paste0(site_folder_pathway, "/", site_serial, "/Factor_", factor.No, "/")
    overall.unc = 0 # default, overall.unc = 0.05
    
    # replace with the row, variable, & factor number 
    base_par = row_var_factor(base_par, site.row, variable.NO, factor.No, overall.unc)
    BS_par = row_var_factor(BS_par, site.row, variable.NO, factor.No, overall.unc)
    DISP_par = row_var_factor(DISP_par, site.row, variable.NO, factor.No, overall.unc)
    before_BS_DISP_par = row_var_factor(before_BS_DISP_par, site.row, variable.NO, factor.No, overall.unc)
    BS_DISP_par = row_var_factor(BS_DISP_par, site.row, variable.NO, factor.No, overall.unc)
    
    ##### B1: iniparams for Base run #####
    # create and replace the names for base run input & output files
    
    base.input = paste0(name.prefix.csv, site_serial, ".csv")
    output.pre = paste0(name.prefix, site_serial, "_F_", factor.No)
    
    base.output = paste0(output.pre, "_")
    
    base_par = base_input_output(base_par, base.input, base.output)
    
    write.table(base_par, 
                file = paste0(path.CF, "iniparams_base_", 
                              site_serial,
                              "_F_", factor.No, ".txt"), 
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
                             weak.strong.assign, 
                             factor.No)
    
    before_BS_DISP_par = bs_disp_input_output(before_BS_DISP_par, 
                                              BS.DISP.input.1, 
                                              BS.DISP.input.2, 
                                              before_BS_DISP.output)
    
    BS_DISP_par = bs_disp_input_output(BS_DISP_par, 
                                       BS.DISP.input.1, 
                                       BS.DISP.input.2, 
                                       BS_DISP.output)
    
    # BS_par[29:40]; DISP_par[29:40]; before_BS_DISP_par[29:40]; BS_DISP_par[29:40]; 
    
    ##### B3: output BS (bootstrap) & DISP files #####
    write.table(BS_par, 
                file = paste0(path.CF,"iniparams_BS_", 
                              site_serial,
                              "_F_", factor.No, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    write.table(DISP_par, 
                file = paste0(path.CF, "iniparams_DISP_", 
                              site_serial,
                              "_F_", factor.No, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    write.table(before_BS_DISP_par, 
                file = paste0(path.CF, "iniparams_BS_DISP_before_", 
                              site_serial,
                              "_F_", factor.No, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    write.table(BS_DISP_par, 
                file = paste0(path.CF, "iniparams_BS_DISP_", 
                              site_serial,
                              "_F_", factor.No, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
  }
}
