library(dplyr)
library(plyr)
library(tidyr)
library(base)
library(data.table)
library(fst)
library(ggplot2)
library(ggthemes)


setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results")
getwd()


csn_pmf_use = fread("CSN_Site_15t1mdl0unc_DN_covertBack_daily.csv")
imp_pmf_use = fread("IMPROVE_Site_15t1mdlVNi_DN_covertBack_daily.csv")
head(csn_pmf_use); head(imp_pmf_use)


##### CSN: match with source profile & daily contribution #####
# select variables to match
source_org_unique = select(source_org_long, 
                           SiteCode, serial.No, Factor.No, 
                           State, Latitude, Longitude, geoid, Species,
                           Factor_source, Source_aftermanual, Fraction_nm)
names(source_org_unique)[8] = "Main_Species"

###### source profile
csv_source_profile = fread("CSN_Site_15t1mdl0unc_source_profile_2024-04-19.csv") # not updated to DN version!!!

csv_source_profile = select(csv_source_profile,
                            site.serial, Factor.No, Factor_source, Species, 
                            Concentration, Percent, 
                            disp_conc_down, disp_conc_mean, disp_conc_up, Main_Species)
names(csv_source_profile)[1] = "serial.No"

# daily contribution
csv_daily = fread("CSN_Site_15t1mdl0unc_DN_covertBack_daily.csv")

csv_daily$V1 = NULL
csv_daily_use = select(csv_daily,
                       site.serial, Factor.No, Date,
                       Factor_source, Main_Species, Concentration, Percent)
names(csv_daily_use)[1] = "serial.No"

# use site.serial & factor.NO combinations to match
source_profile_use = 
  merge(csv_source_profile, source_org_unique, all.x = TRUE)
source_profile_use = na.omit(source_profile_use)
summary(source_profile_use)
dim(source_profile_use); dim(source_org_unique); dim(csv_source_profile)
names(source_profile_use)[ncol(source_profile_use)] = "Fraction_source"

daily_contri_use =
  merge(csv_daily_use, 
        select(source_org_unique, -Fraction_nm))
dim(csv_daily_use); dim(source_org_unique); dim(daily_contri_use)
summary(daily_contri_use)
head(daily_contri_use)

# check if the merge is correct
# source_profile_use1= select(source_profile_use, serial.No, Factor.No, 
#                             Factor_source, Source_aftermanual)
# source_profile_use1 = source_profile_use1[!duplicated(source_profile_use1), ]

write.csv(source_profile_use, "CSN_Site_15t1mdl0unc_DN_source_profile.csv")
write.csv(daily_contri_use, "CSN_Site_15t1mdl0unc_DN_source_daily_contribution.csv")


##### IMPROVE: match with source profile & daily contribution #####
# select variables to match
source_org_unique = select(source_org_long, 
                           SiteCode, serial.No, Factor.No, 
                           State, Latitude, Longitude, geoid, Species,
                           Factor_source, Source_aftermanual, Fraction_nm)
names(source_org_unique)[8] = "Main_Species"

###### source profile
csv_source_profile = fread("IMPROVE_Site_15tAmmIonVNi_source_profile_2024-04-19.csv") # not updated to DN version!!!

csv_source_profile = select(csv_source_profile,
                            site.serial, Factor.No, Factor_source, Species, 
                            Concentration, Percent, 
                            disp_conc_down, disp_conc_mean, disp_conc_up, Main_Species)
names(csv_source_profile)[1] = "serial.No"

# daily contribution
csv_daily = fread("IMPROVE_Site_15tAmmIonVNi_DN_covertBack_daily.csv")

csv_daily$V1 = NULL
csv_daily_use = select(csv_daily,
                       site.serial, Factor.No, Date,
                       Factor_source, Main_Species, Concentration, Percent)
names(csv_daily_use)[1] = "serial.No"

# use site.serial & factor.NO combinations to match
source_profile_use = 
  merge(csv_source_profile, source_org_unique, all.x = TRUE)
source_profile_use = na.omit(source_profile_use)
summary(source_profile_use)
dim(source_profile_use); dim(source_org_unique); dim(csv_source_profile)
names(source_profile_use)[ncol(source_profile_use)] = "Fraction_source"

daily_contri_use =
  merge(csv_daily_use, 
        select(source_org_unique, -Fraction_nm))
dim(csv_daily_use); dim(source_org_unique); dim(daily_contri_use)
summary(daily_contri_use)
head(daily_contri_use)

# check if the merge is correct
# source_profile_use1= select(source_profile_use, serial.No, Factor.No, 
#                             Factor_source, Source_aftermanual)
# source_profile_use1 = source_profile_use1[!duplicated(source_profile_use1), ]

write.csv(source_profile_use, "IMPROVE_Site_15tAmmIonVNi_DN_source_profile.csv")
write.csv(daily_contri_use, "IMPROVE_Site_15tAmmIonVNi_DN_source_daily_contribution.csv")

