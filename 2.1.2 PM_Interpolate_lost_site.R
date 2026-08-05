##packages in need
library(tidyr) # separate{tidyr}, gather{tidyr}, spread{tidyr},  spread is VIP function, str_split_fixed{stringr} is better than separate
library(ggplot2)
library(scales) # percent{}
library(dplyr)
library(plyr)
library(imputeTS) #na_ma, na_interpolation ect.
library(mice) # using Markov Chain Monte Carlo simulation to impute the missing entries
library(tibble)
library(missForest) # implementation of random forest algorithm
library(ggsci)
library(ggrepel)
library(data.table)
library(readxl)

### 1. original site
# IMPROVE, 196
# CSN, 156

### 2. Site Before interpolation
imp_miss = fread("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/IMPROVE_Component_with_missing.csv")
csn_miss = fread("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE/CSN_daily_with_missing_2026.03")

length(unique(imp_miss$SiteCode)) # 196
length(unique(csn_miss$SiteCode)) # 146

before_intp_site_list =
  c(rbind(unique(imp_miss$SiteCode), unique(csn_miss$SiteCode)))
before_intp_site_list = before_intp_site_list[!duplicated(before_intp_site_list)]
length(before_intp_site_list) # 342

### 3. Site after interpolation
species_daily_imp = fread("/Users/ztttttt/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE/IMPROVE_interpulation_random-forest_2023.csv") # in fact, updated data from 2024.03, before HEI audit
species_daily_csn = fread("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_IMPROVE_ownPC/CSN_RFinterpulated_combine_Csubgroup_2024.04.csv")

length(unique(species_daily_imp$SiteCode)) # 169
length(unique(species_daily_csn$SiteCode)) # 139

after_intp_site_list = 
  unique(
    c(unique(species_daily_imp$SiteCode), 
      unique(species_daily_csn$SiteCode))
    )
after_intp_site_list = after_intp_site_list[!duplicated(after_intp_site_list)]
length(after_intp_site_list) # 308

## Get list of sites excluded in the interpolation process
# due to not in mainland US, all NAs in species concentrations
miss_site_in_intp_list = 
  before_intp_site_list[!(before_intp_site_list %in% after_intp_site_list)]
miss_site_in_intp_list = miss_site_in_intp_list[!duplicated(miss_site_in_intp_list)]
miss_site_in_intp_list = sort(miss_site_in_intp_list)
length(miss_site_in_intp_list) # 34

miss_site_in_intp_list # 34
# [1] "20900035"  "220150008" "370670022" "530530031" "540390011" "550790010" "60850005"  "ADPI1"     "AREN1"    
# [10] "CADI1"     "CHER1"     "DENA1"     "DETR1"     "GAAR1"     "HACR1"     "HALE1"     "HAVO1"     "KPBO1"    
# [19] "LIVO1"     "MAKA1"     "MALO1"     "MALO2"     "MKGO1"     "NEYO1"     "RENO1"     "RENO2"     "RENO3"    
# [28] "SIKE1"     "SIME1"     "TOOL1"     "TRCR1"     "TUXE1"     "VIIS1"     "YOSEX"    

### 4. Site after Non-GUI file preparation
csn_imp_nonGui <-
  read_excel("/Users/ztttttt/Dropbox/GMU_computer/HEI HAQ PMF/PMF_Results/CSN_IMPROVE_reason_exclude_factor.xlsx",
             sheet = 1, n_max = 2449, 
             col_types = c(SiteCode = "text")) # Excel infers column type from the first few rows
csn_imp_nonGui$SiteCode[1116]

csn_imp_nonGui_site = 
  dplyr::select(csn_imp_nonGui, Dataset, SiteCode, serial.No)
csn_imp_nonGui_site = csn_imp_nonGui_site[!duplicated(csn_imp_nonGui_site), ]
table(csn_imp_nonGui_site$Dataset) # CSN 138 IMPROVE 168, 306

length(unique(csn_imp_nonGui_site$serial.No)) # 306
after_nonGui_site = unique(csn_imp_nonGui_site$SiteCode)
length(after_nonGui_site) # 306

## Get list of sites excluded when preparing Non-GUI files
non_gui_exclude_site = after_intp_site_list[!(after_intp_site_list %in% after_nonGui_site)]
non_gui_exclude_site # "MOOS1" "60731018"

