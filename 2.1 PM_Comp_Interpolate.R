##clear environment
# rm(list=ls())

##set working directory - for IMPROVE
# setwd("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE")
# getwd()
# data.dir <- "Users/ztttttt/Documents/HEI PMF/R - original IMPROVE"

setwd("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE")
getwd()
data.dir <- "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE"


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

########## START - check the overall missing rate - JUMP this part later #########
csn_daily_bef = fread("CSN_Component_with_missing_Before_2015_2023.02.csv")
csn_daily_aft = fread("CSN_Component_with_missing_After_2015_2023.02.csv")
# csn_daily$Accept.PM2.5 = NULL # after 2015

csn_daily_bef$X = NULL
csn_daily_bef$Date = as.Date(csn_daily_bef$Date)
sapply(csn_daily_bef, class)

csn_daily_aft$X = NULL
csn_daily_aft$Date = as.Date(csn_daily_aft$Date)
sapply(csn_daily_aft, class)


csn_bfr_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC.unadjusted.88", "EC.unadjusted.88", "OPC.unadjusted.88",
                   "Cl.", "Accept.PM2.5")
# "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
# "K.", "Na.", "NH4.", "Sb", "Sn", 
# "Rb", "Zr", "Cl.")
# Above sepecies were kept since 2023.04

csn_aft_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC1.88", "OC2.88", "OC3.88", "OC4.88",
                   "EC1.88", "EC2.88", "EC3.88", 
                   "EC1", "EC2", "EC3", 
                   "EC.unadjusted.88", "EC.TOR.unadjust.88", "EC.88",
                   "OC.unadjusted.88", "OC.TOR.unadjusted.88", "OC", 
                   "OPC.88", "OP.TOR.unadjusted.88", "OPC.unadjusted.88",
                   "Cl.", "PM2.5RC")
# "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
# "K.", "Na.", "NH4.", "Sb", "Sn", 
# "Rb", "Zr", "Cl.")
# Above sepecies were kept since 2023.04

csn_daily_bef[ ,csn_bfr_remove] <- list(NULL)
csn_daily_aft[ ,csn_aft_remove] <- list(NULL)

# remove those with NA only
csn_daily_bef_noAllNA = 
  subset(csn_daily_bef, 
         rowSums(is.na(csn_daily_bef[, 4:ncol(csn_daily_bef)])) != 
           ncol(csn_daily_bef[, 4:ncol(csn_daily_bef)]))
csn_daily_aft_noAllNA = 
  subset(csn_daily_aft, 
         rowSums(is.na(csn_daily_aft[, 4:ncol(csn_daily_aft)])) != 
           ncol(csn_daily_aft[, 4:ncol(csn_daily_aft)]))

# get the variable used for OC, EC
csn_daily_bef_noAllNA = plyr::rename(csn_daily_bef_noAllNA, 
                                     c("EC.TOR.unadjust.88" = "EC", 
                                       "OC.TOR.unadjusted.88" = "OC",
                                       "OP.TOR.unadjusted.88" = "OP"))

csn_daily_aft_noAllNA = plyr::rename(csn_daily_aft_noAllNA, 
                                     c("EC.TOR.88" = "EC", 
                                       "OC.88" = "OC",
                                       "OPC.TOR.88" = "OP"))

# get common colnames
daily_common_cols <- intersect(names(csn_daily_bef_noAllNA), 
                               names(csn_daily_aft_noAllNA))

# Subset data frames using common rownames and column names
daily_commonRowCol_bef <- csn_daily_bef_noAllNA[, daily_common_cols]
daily_commonRowCol_aft <- csn_daily_aft_noAllNA[, daily_common_cols]

# change variable column to rownames
daily_commonRowCol_bef$Variables = row.names(daily_commonRowCol_bef)
daily_commonRowCol_aft$Variables = row.names(daily_commonRowCol_aft)

# combine two dataframe & remove rownames
csn_daily_comb = rbind(daily_commonRowCol_bef, daily_commonRowCol_aft)

# estimate the overall NA rate
sum(is.na(csn_daily_comb)) *100 /
  (ncol(csn_daily_comb) * nrow(csn_daily_comb))

# NA rate of each row
csn_daily_comb$NA.rate = 
  rowSums(is.na(csn_daily_comb)) * 100 / 
  (ncol(csn_daily_comb) - 4)

# NA rate of each row by year & month
csn_daily_comb$year = format(csn_daily_comb$Date, "%Y")
csn_daily_comb$month = format(csn_daily_comb$Date, "%m")
ddply(csn_daily_comb, .(year), summarise, NA.rate = mean(NA.rate))
ddply(csn_daily_comb, .(month), summarise, NA.rate = mean(NA.rate))

# NA rate of each species
csn_daily_NA_comp = data.frame(
  colSums(is.na(csn_daily_comb)) * 100 /
    nrow(csn_daily_comb))

colnames(csn_daily_NA_comp) = "NA.rate.Component"
csn_daily_NA_comp$CompName = rownames(csn_daily_NA_comp)

# NA rate of each state & plot
csn_daily_NA_State = ddply(csn_daily_comb, .(State), summarise,
                           NA.rate = mean(NA.rate))
colnames(csn_daily_NA_State)[1] = "state_abbr"

library(usmap)
library(USAboundaries)

# get US states
us_states <- USAboundaries::us_states()
us_states <- us_states[us_states$state_abbr %in% 
                         csn_daily_NA_State$state_abbr,]
us_states_csn_NA = merge(us_states, csn_daily_NA_State)
us_states_csn_NA = select(us_states_csn_NA, 
                          state_abbr, geoid, 
                          NA.rate)

plot(us_states_csn_NA)

########### END - check the overall missing rate - JUMP this part later ##########


####################################################################################
##### 11111. CSN - Fill the Missing, till & after 2015 separately #####
####################################################################################
#### generate basic data for filling ####
# csn_daily = fread("CSN_Component_with_missing_Before_2015.csv")
# csn_daily = fread("CSN_Component_with_missing_After_2015.csv")
# csn_daily = fread("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_Component_with_missing_After_2015.csv")
csn_daily = fread("CSN_Component_with_missing_Before_2015_2023.02.csv")
csn_daily = fread("CSN_Component_with_missing_After_2015_2023.02.csv")
# csn_daily$Accept.PM2.5 = NULL # after 2015

csn_daily$V1 = NULL
csn_daily$Date = as.Date(csn_daily$Date)
sapply(csn_daily, class)

aqs_PM25 = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_IMPROVE_ownPC/EPA_CSN_AQS_daily_PM25.csv")
aqs_PM25$V1 = NULL
aqs_PM25$Date = as.Date(aqs_PM25$Date)
sapply(aqs_PM25, class)

# match PM2.5
csn_daily = merge(csn_daily, aqs_PM25)

# compare
csn_daily_PM = select(csn_daily, Accept.PM2.5, PM25_AQS)
summary(csn_daily_PM)
csn_daily_PM = subset(csn_daily_PM, !is.na(Accept.PM2.5))
summary(csn_daily_PM)

plot(csn_daily_PM$Accept.PM2.5, csn_daily_PM$PM25_AQS)
plot(csn_daily_PM$Accept.PM2.5, csn_daily_PM$PM25_AQS, 
     ylim = c(0,100), xlim = c(0,100))
cor(csn_daily_PM$Accept.PM2.5, csn_daily_PM$PM25_AQS, method = "spearman")
cor(csn_daily_PM$Accept.PM2.5, csn_daily_PM$PM25_AQS, method = "pearson")
boxplot(csn_daily_PM$Accept.PM2.5, csn_daily_PM$PM25_AQS)
boxplot(csn_daily_PM$Accept.PM2.5, csn_daily_PM$PM25_AQS, 
        ylim = c(0,20))
# detect those with large difference


# numeric will be calculated later, which we wants to avoid
csn_daily$SiteCode = as.character(csn_daily$SiteCode)

# reorder the dataframe
csn_daily = csn_daily[with(csn_daily, order(State, SiteCode, Date)), ]

# calculate the overall percentages of NA for selected species
## !!! until 2015
site_day_NA_count = ddply(csn_daily, 
                          .(SiteCode), 
                          summarise,
                          count = length(Date),
                          Mg.NA = sum(is.na(Mg)),
                          EC.NA = sum(is.na(EC.TOR.unadjust.88)),
                          OC.NA = sum(is.na(OC.TOR.unadjusted.88)),
                          SO4.NA = sum(is.na(SO4)))

## !!! after 2015
site_day_NA_count = ddply(csn_daily, 
                          .(SiteCode), 
                          summarise,
                          count = length(Date),
                          Mg.NA = sum(is.na(Mg)),
                          EC.NA = sum(is.na(EC.TOR.88)),
                          OC.NA = sum(is.na(OC.88)),
                          SO4.NA = sum(is.na(SO4)))

# count the percent of NA in representative PM species from each type
site_day_NA_count$Mg.NA.per = round(site_day_NA_count$Mg.NA/site_day_NA_count$count, 3)
site_day_NA_count$EC.NA.per = round(site_day_NA_count$EC.NA/site_day_NA_count$count, 3)
site_day_NA_count$OC.NA.per = round(site_day_NA_count$OC.NA/site_day_NA_count$count, 3)
site_day_NA_count$SO4.NA.per = round(site_day_NA_count$SO4.NA/site_day_NA_count$count, 3)

# check the sites with high percentage NAs
site.lots.NA = site_day_NA_count$SiteCode[
  site_day_NA_count$Mg.NA.per > 0.5 & 
    site_day_NA_count$SO4.NA.per > 0.5 &
    site_day_NA_count$EC.NA.per > 0.5 & 
    site_day_NA_count$OC.NA.per > 0.5]
length(site.lots.NA)

csn_daily_halfNA = subset(csn_daily, SiteCode %in% site.lots.NA)
# subset(csn_daily, SiteCode == site.lots.NA[1])[1:12, 2:5]
# subset(csn_daily, SiteCode == site.lots.NA[34])[1:110, 2:5]

# plot their distribution
csn_meta_sites = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/R - original CSN/CSN metadata sample sites 2010-20 use.csv")
csn_sites = select(csn_meta_sites, SiteCode, State, Latitude,  Longitude)
csn_sites$High.P.NA = "N"
csn_sites$High.P.NA[csn_sites$SiteCode %in% site.lots.NA] = "Y"

# only keep the sites used
csn_sites = subset(csn_sites, 
                   SiteCode %in% 
                     unique(csn_daily$SiteCode))
table(csn_sites$High.P.NA)

# mainland US
csn_sites = subset(csn_sites,
                   Latitude < 50 & Latitude > 20 &
                     Longitude > -130 & Longitude < -60)
length(unique(csn_sites$SiteCode))
# site 20900035 is in AK, not included

UScounty <- map_data("county")

ggplot(subset(csn_sites), # , Longitude > -130 & Latitude > 20
       aes(Longitude, Latitude, group= High.P.NA)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.8, aes(shape = High.P.NA, color = High.P.NA)) +
  scale_color_npg() +
  theme_bw()

# csn_miss = subset(csn_daily, !(SiteCode %in% site.lots.NA))
## 135 out of 136 sites left for data before 2015, records in 2022
## 146 out of 149 sites left for data after 2015, records in 2022
## after considering qualifier data in 2023.02, MANY MORE sites excluded
## thus, remove this analysis to the interpolation for each site from 2023.02
## from now on, code in PM_Comp_Interpolate


#### filling the NAs for each site (logged, no negative)- na.omit & check Mix Error ####
csn_daily = subset(csn_daily, SiteCode != "20900035")
n.site = length(unique(csn_daily$SiteCode))
csn_miss = as.data.frame(csn_daily)
csn_miss$PM25_AQS = NULL

# create data.frame to store results
running_avg_sum = linear_sum = mice_sum = rf_sum = NULL
mix_error_intp_pstv_summary = NULL

# create data.frame with the first column being the colnames of csn_miss for matching data
csn_var = data.frame(colnames(csn_miss))
colnames(csn_var)[1] = "Variables"
p_miss_summary = csn_var
rf_vw_oob_summary = data.frame(csn_var[4:nrow(csn_var), ])
colnames(rf_vw_oob_summary)[1] = "Variables"
# a = data.frame(csn_var[4:nrow(csn_var), ])

#### start interpolating ####
for (i in 1:n.site){ 
  # 12, 20, 89, 101-2, 124 (before_2015 data)
  # 3, 13, 35, 47, 69, 74, 110, 142 (after_2015 data)

  ###### 1.prepare data set - remove those with only NA & log ######
  site.study = unique(csn_miss$SiteCode)[i]
  site_single = subset(csn_miss, SiteCode == site.study)
  row.all = nrow(site_single)

  ## remove the rows where all component concentrations are NAs
  col.withAllNA = ncol(site_single)
  cols.comp = 4:col.withAllNA # columns for PM/components
  col.component = ncol(site_single[, cols.comp]) # the code does not work for data.table
  
  # a = is.na(site_single[, cols.comp])
  # rowSums(a)
  
  site_single_noAllNA = subset(site_single, 
                               rowSums(is.na(site_single[, cols.comp])) != 
                                 col.component)
  row.No = nrow(site_single_noAllNA)
  
  ## detect the percent of missing concentration values for each component
  p_miss_with_neg <- data.frame(
    unlist(
      lapply(
        site_single_noAllNA, 
        function(x) 
          sum(is.na(x))))/row.No)
  colnames(p_miss_with_neg) = site.study
  
  ## summarise the missing values for each site
  p_miss_summary = cbind(p_miss_summary, p_miss_with_neg)

  ## substitute the negative or 0 with 0.000005 before interpolation
  ## 0.000009 was selected cause the present lowest positive value is 0.00001
  ## these value will be set to 1/2 MDL before PMF analysis 

  ## log all value to avoid negative interpolation 
  site_single_log = cbind(select(site_single_noAllNA,
                                 SiteCode, Date, State),
                          site_single_noAllNA %>% 
                            dplyr::select(where(is.numeric)) %>%
                            log())
  
  ##### 2.prepare data for interpolation Method Comparison - mix error #####
  # total NA percentage in components
  na.count = sum(is.na(site_single_log[ ,cols.comp]))
  na.percent = na.count/(row.No * col.component)
  
  # site_single_no_NA$Accept.PM2.5 = NA
  # site_single_no_NA = 
  #   relocate(site_single_no_NA, Accept.PM2.5, .after = State)
  # 
  # random set a dataframe with same percent of NAs as the original dataset
  if(sum(is.na(site_single$Accept.PM2.5)) == row.all){
    site_single_log$Accept.PM2.5 = NULL
    site_single_no_NA = na.omit(site_single_log)
    site_single_no_NA = na.omit(site_single_log_noPM)
    
    cols.comp.use = cols.comp[1:(col.component-1)]
    
  } else{
    site_single_no_NA = na.omit(site_single_log)
    cols.comp.use = cols.comp
  }
  
  site_single_rdm_NA = cbind(site_single_no_NA[ ,1:3], 
                             prodNA(site_single_no_NA[ ,cols.comp.use], 
                                    noNA = na.percent))
  
  ##### 3.Interpolation - Method Comparison #####
  #### interpolation, below is set for the randomly set NA, for method performance comparison
  sgl_intp_rdNA_running_avg = sgl_intp_rdNA_linear = site_single_rdm_NA
  
  for(j in cols.comp.use){
    # interpolation1: linear  
    sgl_intp_rdNA_linear[, j] = na_interpolation(site_single_rdm_NA[, j]) 
    # interpolation2: running window average 
    sgl_intp_rdNA_running_avg[, j] = na_ma(site_single_rdm_NA[, j], 
                                           weighting = "simple", k = 4) 
  }
  
  # interpolation3: mice, using MCMC, multivariate interpolation by chained equation
  sgl_intp_rdNA_mice <- mice(site_single_rdm_NA[, cols.comp.use], 
                             maxit=0, m = 50,
                             remove.collinear = F)
  # sgl_intp_rdNA_mice$loggedEvents
  # collinear exists in the dataset, mice will automatically remove NO3 & SO4 as a result
  # https://stefvanbuuren.name/fimd/sec-toomany.html
  
  # fills in the missing data, and returns the completed data
  sgl_intp_rdNA_mice_dslist <- complete(sgl_intp_rdNA_mice, "all")
  
  ## calculate the average of all interpolations in the array
  sgl_intp_rdNA_mice_avg = data.frame(aaply(
    laply(
      sgl_intp_rdNA_mice_dslist, as.matrix),  
    c(2, 3), 
    mean))
  sgl_intp_rdNA_mice_avg_use = cbind(site_single_rdm_NA[, 1:3], 
                                     sgl_intp_rdNA_mice_avg)
  
  #interpolation4: missForest, using random forest
  sgl_intp_rdNA_rf_mf = missForest(site_single_rdm_NA[, cols.comp.use])
  sgl_intp_rdNA_rf = cbind(site_single_rdm_NA[, 1:3], 
                           data.frame(sgl_intp_rdNA_rf_mf$ximp))
  
  me.running.avg = mixError(sgl_intp_rdNA_running_avg[, cols.comp.use], 
                            site_single_rdm_NA[, cols.comp.use], 
                            site_single_no_NA[, cols.comp.use]) 
  me.linear = mixError(sgl_intp_rdNA_linear[, cols.comp.use], 
                       site_single_rdm_NA[, cols.comp.use], 
                       site_single_no_NA[, cols.comp.use]) 
  me.mice = mixError(sgl_intp_rdNA_mice_avg_use[, cols.comp.use], 
                     site_single_rdm_NA[, cols.comp.use], 
                     site_single_no_NA[, cols.comp.use]) 
  me.rf = mixError(sgl_intp_rdNA_rf[, cols.comp.use], 
                   site_single_rdm_NA[, cols.comp.use], 
                   site_single_no_NA[, cols.comp.use]) 
  
  mix_error_intp = data.frame(State = site_single_rdm_NA$State[1], 
                              SiteCode = site.study, 
                              percent.NA = na.percent, 
                              row.total = row.all, 
                              row.not.all.NA = row.No, 
                              row.no.NA = nrow(site_single_rdm_NA), 
                              me.running.avg = me.running.avg, 
                              me.linear = me.linear, 
                              me.mice = me.mice, 
                              me.rf = me.rf)
  rownames(mix_error_intp)[1] = i
  mix_error_intp_pstv_summary = rbind(mix_error_intp_pstv_summary, 
                                      mix_error_intp[1, ])
  
  
  ##### 4.Interpolation - generate data for future analyses #####
  sgl_intp_running_avg_pro = sgl_intp_linear_pro = site_single_log
  
  for(k in cols.comp.use){
    # interpolation1: linear  
    sgl_intp_linear_pro[, k] = na_interpolation(site_single_log[, k]) 
    # interpolation2: running window average 
    sgl_intp_running_avg_pro[, k] = na_ma(site_single_log[, k], 
                                          weighting = "simple", k = 4) 
  }
  # covert logged concentrations to original
  sgl_intp_linear = cbind(sgl_intp_linear_pro[, 1:3], 
                          exp(sgl_intp_linear_pro[, cols.comp.use]))
  sgl_intp_running_avg = cbind(sgl_intp_running_avg_pro[, 1:3], 
                               exp(sgl_intp_running_avg_pro[, cols.comp.use]))
  
  # interpolation3: mice, using MCMC, multiple interpolation
  sgl_intp_mice_org <- mice(site_single_log[, cols.comp.use], 
                            maxit=0, m = 50, 
                            remove.collinear = F)
  sgl_intp_mice_dslist <- complete(sgl_intp_mice_org, "all")
  
  ## get the average of data.frame in the array
  sgl_intp_mice_avg = data.frame(aaply(
    laply(sgl_intp_mice_dslist, as.matrix),  
    c(2, 3), mean))
  sgl_intp_mice = cbind(site_single_log[, 1:3], 
                        exp(sgl_intp_mice_avg))
  
  #interpolation4: missForest, using random forest, option "variablewise = T"
  sgl_intp_rf_mf = missForest(site_single_log[, cols.comp.use], 
                              variablewise = T)
  sgl_intp_rf = cbind(site_single_log[, 1:3], 
                      exp(sgl_intp_rf_mf$ximp))
  
  
  # the OOB error for each PM species & interpolated values combine
  rf_vw_oob = data.frame(sgl_intp_rf_mf$OOBerror)
  colnames(rf_vw_oob) = paste0("X", site.study)
  
  if(sum(is.na(site_single$Accept.PM2.5)) == row.all){
    # insert first row of NA
    rf_vw_oob = 
      rbind(data.frame(lapply(rf_vw_oob, function(x) NA)), rf_vw_oob)
    
    # insert Accept.PM2.5 column of NA
    sgl_intp_running_avg = 
      sgl_intp_running_avg %>%
      dplyr::mutate(Accept.PM2.5 = NA) %>%
      relocate(Accept.PM2.5, .after = State)
    
    sgl_intp_linear = 
      sgl_intp_linear %>%
      dplyr::mutate(Accept.PM2.5 = NA) %>%
      relocate(Accept.PM2.5, .after = State)
    
    sgl_intp_mice = 
      sgl_intp_mice %>%
      dplyr::mutate(Accept.PM2.5 = NA) %>%
      relocate(Accept.PM2.5, .after = State)
    
    sgl_intp_rf = 
      sgl_intp_rf %>%
      dplyr::mutate(Accept.PM2.5 = NA) %>%
      relocate(Accept.PM2.5, .after = State)
  } 
  
  rf_vw_oob_summary = cbind(rf_vw_oob_summary, rf_vw_oob[, 1])
  running_avg_sum = rbind(running_avg_sum, sgl_intp_running_avg)
  linear_sum = rbind(linear_sum, sgl_intp_linear)
  mice_sum = rbind(mice_sum, sgl_intp_mice)
  rf_sum = rbind(rf_sum, sgl_intp_rf)
}

rownames(p_miss_summary) = 1:nrow(p_miss_summary)

#### output interpolation results ####

# out put results for data until 2015
write.csv(p_miss_summary, "CSN_Missing_Rate_Site_until_2015_2023.03.csv")
write.csv(mix_error_intp_pstv_summary, "CSN_interpulation_Mix_Error_until_2015_2023.03.csv")
write.csv(rf_vw_oob_summary, "CSN_OOBerror_random-forest_until_2015_2023.03.csv")

write.csv(running_avg_sum, "CSN_interpulation_running-average_until_2015_2023.03.csv")
write.csv(linear_sum, "CSN_interpulation_linear_until_2015_2023.03.csv")
write.csv(mice_sum, "CSN_interpulation_multi-mice_until_2015_2023.03.csv")
write.csv(rf_sum, "CSN_interpulation_random-forest_until_2015_2023.03.csv")

# out put results for data from 2016
write.csv(p_miss_summary, "CSN_Missing_Rate_Site_from_2016_2023.03.csv")
write.csv(mix_error_intp_pstv_summary, "CSN_interpulation_Mix_Error_from_2016_2023.03.csv")
write.csv(rf_vw_oob_summary, "CSN_OOBerror_random-forest_from_2016_2023.03.csv")

write.csv(running_avg_sum, "CSN_interpulation_running-average_from_2016_2023.03.csv")
write.csv(linear_sum, "CSN_interpulation_linear_from_2016_2023.03.csv")
write.csv(mice_sum, "CSN_interpulation_multi-mice_from_2016_2023.03.csv")
write.csv(rf_sum, "CSN_interpulation_random-forest_from_2016_2023.03.csv")

# ### to find out how was mixError calculated, the dataset was generated from the loop above
# # based on IMPROVE data
# rf_intp_example = sgl_intp_rdNA_rf[, 5:col.withAllNA]
# rdm_NA_example = site_single_rdm_NA[, 5:col.withAllNA]
# site_noNa_example = site_single_no_NA[, 5:col.withAllNA]
# 
# mixError(rf_intp_example, rdm_NA_example, site_noNa_example) 
# varClass(rf_intp_example)
# 
# mis <- is.na(rdm_NA_example)
# sqrt(mean((rf_intp_example[mis] - site_noNa_example[mis])^{2}) / stats::var(site_noNa_example[mis]))
# 
# # nrmse, a function in package missForest, and internally used by mixError{missForest} for numeric variables 
# nrmse <- function(xcsn, xmis, xtrue){
#   mis <- is.na(xmis)
#   sqrt(mean((xcsn[mis] - xtrue[mis])^{2}) / stats::var(xtrue[mis]))
# }
# 
# nrmse(rf_intp_example, rdm_NA_example, site_noNa_example)

#### plot: Mix Erro distribution ####
ME_summary_bef = read.csv("CSN_interpulation_Mix_Error_until_2015_2023.03.csv")
ME_summary_aft = read.csv("CSN_interpulation_Mix_Error_from_2016_2023.03.csv")

## data for plotting
ME_summary_bef$X = NULL
colnames(ME_summary_bef)[7:10]
colnames(ME_summary_bef)[7:10] = c("Running_Avg", "Linear", "Multiple", "Random_Forest")
ME_bef_plot = select(ME_summary_bef, SiteCode, Running_Avg, Linear, Multiple, Random_Forest)
ME_bef_plot = gather(ME_bef_plot, "Interpolations", "NRMSE", -SiteCode)
# ME_bef_plot = gather(ME_bef_plot, "Interpolations", "Mix_Error", -SiteCode)
ME_bef_plot$Year = "2011-15"

ME_summary_aft$X = NULL
colnames(ME_summary_aft)[7:10]
colnames(ME_summary_aft)[7:10] = c("Running_Avg", "Linear", "Multiple", "Random_Forest")
ME_aft_plot = select(ME_summary_aft, SiteCode, Running_Avg, Linear, Multiple, Random_Forest)
ME_aft_plot = gather(ME_aft_plot, "Interpolations", "NRMSE", -SiteCode)
# ME_aft_plot = gather(ME_aft_plot, "Interpolations", "Mix_Error", -SiteCode)
ME_aft_plot$Year = "2016-20"

ME_summary = rbind(ME_summary_bef, ME_summary_aft)
ME_plot = rbind(ME_bef_plot, ME_aft_plot)

## plotting
theme.mix.err = theme(axis.title.y.right = element_blank(),
                      panel.spacing = unit(10, "mm"),   
                      legend.background = element_blank(),
                      strip.text = element_text(face="bold", size=rel(1.5)),
                      strip.background = element_rect(fill="lightblue", colour="grey", linewidth=16),
                      axis.title.x = element_text(color="grey25", size = 20, vjust=-2), #, margin=margin(0,0,0,300)
                      axis.title.y = element_text(color="grey25", size = 20, vjust=2), #, margin=margin(0,2,0,0)
                      plot.title=element_text(size=rel(2)), 
                      axis.text.x = element_text(color="grey25", size = 18, angle = 30, hjust = 0.5, vjust = 0.5), plot.margin = unit(c(2,1,2, 2), "lines"),
                      axis.text.y = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5))

ggplot(ME_plot, aes(Interpolations, NRMSE, fill = Interpolations)) +
  geom_boxplot() +
  geom_jitter(color="black", size=1.1, alpha=0.5) +
  facet_wrap(Year ~.) + 
  scale_fill_npg() + 
  ylim(0, 0.65) +
  theme_bw() +
  theme(legend.position = "none") +
  theme.mix.err

#### plot: OOB (out-of-bag) Error distribution ####
OOB_summary_bef = read.csv("CSN_OOBerror_random-forest_until_2015_2023.03.csv")
OOB_summary_aft = read.csv("CSN_OOBerror_random-forest_from_2016_2023.03.csv")
OOB_summary_bef$X = OOB_summary_aft$X = NULL

# convert values in first column to row names
rownames(OOB_summary_bef) <- OOB_summary_bef[,1]
rownames(OOB_summary_aft) <- OOB_summary_aft[,1]
# Changed selected to OC, EC & OP for PMF

# check the potential duplicates
"EC" %in% rownames(OOB_summary_bef)
"OC" %in% rownames(OOB_summary_bef)
"OP" %in% rownames(OOB_summary_bef)

"EC" %in% rownames(OOB_summary_aft)
"OC" %in% rownames(OOB_summary_aft)
"OP" %in% rownames(OOB_summary_aft)

# "OC" already existed in rownames after 2015, so delete it first
OOB_summary_aft = 
  OOB_summary_aft[!(
    row.names(OOB_summary_aft) == "OC"),]

# select rows used as OC, EC & OP for PMF
dup_row_bef = c("EC.TOR.unadjust.88", 
                "OC.TOR.unadjusted.88", 
                "OP.TOR.unadjusted.88",
                "Accept.PM2.5")
dup_row_aft = c("EC.TOR.88", 
                "OC.88", 
                "OPC.TOR.88",
                "PM2.5RC")

# Identify the rows to rename
row_to_change_bef <- 
  rownames(OOB_summary_bef) %in% 
  dup_row_bef
summary(row_to_change_bef)

row_to_change_aft <- 
  rownames(OOB_summary_aft) %in% 
  dup_row_aft
summary(row_to_change_aft)

# Rename the rows
rownames(OOB_summary_bef)[row_to_change_bef] <- c("PM25", "EC", "OC", "OP")
rownames(OOB_summary_aft)[row_to_change_aft] <- c("EC", "OC", "OP","PM25")

# get common rownames & colnames
obb_common_rows <- intersect(row.names(OOB_summary_bef), 
                             row.names(OOB_summary_aft))

obb_common_cols <- intersect(names(OOB_summary_bef), 
                             names(OOB_summary_aft))

# Subset data frames using common rownames and column names
OOB_commonRowCol_bef <- OOB_summary_bef[obb_common_rows, 
                                        obb_common_cols]
OOB_commonRowCol_aft <- OOB_summary_aft[obb_common_rows, 
                                        obb_common_cols]

# change variable column to rownames
OOB_commonRowCol_bef$Variables = row.names(OOB_commonRowCol_bef)
OOB_commonRowCol_aft$Variables = row.names(OOB_commonRowCol_aft)

# combine two dataframe & remove rownames
OOB_plot = rbind(OOB_commonRowCol_bef, OOB_commonRowCol_aft)
rownames(OOB_plot) <- NULL

# gather dataframe 
OOB_plot = gather(OOB_plot, 
                  "SiteCode", "OOB_error",
                  -Variables)
colnames(OOB_plot)[1] = "Species"

# the 1st appearance of sitecode-variable group is from "2011-15" and 2nd "2016-20"
OOB_plot$Year = "2011-15"
OOB_plot$Year[duplicated(OOB_plot[, 1:2])] = "2016-20"

# group species
ions = c("Na.", "K.", "NH4.", "NO3", "SO4")
OOB_plot$class = "Element"
OOB_plot$class[OOB_plot$Species %in% ions] = "Ion"
OOB_plot$class[grepl("OC", OOB_plot$Species, fixed = T) |
                 grepl("EC", OOB_plot$Species, fixed = T) |
                 grepl("OP", OOB_plot$Species, fixed = T)] = "OC.EC"

## plotting
theme.obb = theme(axis.title.y.right = element_blank(),
                      panel.spacing = unit(10, "mm"),   
                      legend.background = element_blank(),
                      strip.text = element_text(face="bold", size=rel(1.5)),
                      strip.background = element_rect(fill="lightblue", colour="grey", size=16),
                      axis.title.x = element_text(color="grey25", size = 16, vjust=-2, margin=margin(0,0,0,300)), 
                      axis.title.y = element_text(color="grey25", size = 16, vjust=2, margin=margin(0,2,0,0)),
                      plot.title=element_text(size=rel(2)), 
                      axis.text.x = element_text(color="grey25", size = 14, angle = 90, hjust = 0.5, vjust = 0.5), plot.margin = unit(c(2,1,2, 2), "lines"),
                      axis.text.y = element_text(color="grey25", size = 14, angle = 0, hjust = 0.5))

# facet_grid with "free" space
ggplot(OOB_plot, aes(Species, OOB_error)) +
  geom_boxplot() +
  geom_jitter(color="black", size=1, alpha=0.5) +
  facet_grid(Year ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("CSN_OOB_Error") +
  theme_bw() +
  theme.obb

# exclude OC/EC subgroups
ggplot(
  subset(
    OOB_plot, 
    !(grepl("88", OOB_plot$Species, fixed = T))), 
  aes(Species, OOB_error)) +
  geom_boxplot() +
  geom_jitter(color="black", size=1, alpha=0.5) +
  facet_grid(Year ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("CSN_OOB_Error") +
  theme_bw() +
  theme.obb

OOB_final_plot = 
  subset(
    OOB_plot, 
    !(grepl("88", OOB_plot$Species, fixed = T)) &
      !(grepl("PM", OOB_plot$Species, fixed = T)))
colnames(OOB_final_plot)[1] = "Species"

OOB_final_plot$Species[OOB_final_plot$Species == "K."] = "KIon"
OOB_final_plot$Species[OOB_final_plot$Species == "Na."] = "NaIon"
OOB_final_plot$Species[OOB_final_plot$Species == "NH4."] = "NH4Ion"
OOB_final_plot$Species[OOB_final_plot$Species == "NO3"] = "NO3Ion"
OOB_final_plot$Species[OOB_final_plot$Species == "SO4"] = "SO4Ion"


ggplot(OOB_final_plot, 
  aes(Species, OOB_error)) +
  geom_boxplot() +
  geom_jitter(color="black", size=1, alpha=0.5) +
  facet_grid(Year ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("CSN_OOB_Error") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme_bw() +
  theme(panel.spacing.x = unit(0.2, "cm")) +
  theme(panel.spacing.y = unit(0.2, "cm")) +
  theme(axis.title.y.right = element_blank(),
        panel.spacing = unit(10, "mm"),   
        legend.background = element_blank(),
        strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="grey", size=16),
        axis.title.x = element_text(color="grey25", size = 16, vjust=-2, 
                                    margin=margin(0,0,0,300),
                                    family = "Arial Unicode MS"), 
        axis.title.y = element_text(color="grey25", size = 16, vjust=2, 
                                    margin=margin(0,2,0,0), 
                                    family = "Arial Unicode MS"),
        plot.title=element_text(size=rel(2)), 
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 14, angle = 90, 
                                   hjust = 0.5, vjust = 0.5,
                                   family = "Arial Unicode MS"), 
        axis.text.y = element_text(color="grey25", size = 14, angle = 0, 
                                   hjust = 0.5, family = "Arial Unicode MS"))

ggplot(OOB_final_plot, 
       aes(Species, OOB_error, 
           color = class)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(size=1, alpha=0.3) +
  facet_grid(. ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("CSN_OOB_Error") +
  scale_color_nejm() + 
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme_bw() +
  theme(panel.spacing.x = unit(0.2, "cm")) +
  theme(axis.title.y.right = element_blank(),
        panel.spacing = unit(10, "mm"),   
        legend.background = element_blank(),
        strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="grey", size=16),
        axis.title.x = element_text(color="grey25", size = 16, vjust=-2, 
                                    margin=margin(0,0,0,300),
                                    family = "Arial Unicode MS"), 
        axis.title.y = element_text(color="grey25", size = 16, vjust=2, 
                                    margin=margin(0,2,0,0), 
                                    family = "Arial Unicode MS"),
        plot.title=element_text(size=rel(2)), 
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 14, angle = 90, 
                                   hjust = 0.5, vjust = 0.5,
                                   family = "Arial Unicode MS"), 
        axis.text.y = element_text(color="grey25", size = 14, angle = 0, 
                                   hjust = 0.5, family = "Arial Unicode MS"))

# Calculate percentiles and corresponding values
percentiles <- seq(0, 100, 10)
percentiles = append(c(95, 98), percentiles)
values <- quantile(OOB_plot$OOB_error, 
                   probs = percentiles/100)

values <- quantile(subset(OOB_plot, Variables != "PM25")$OOB_error, 
                   probs = percentiles/100)

# Plot values at each percentile
ggplot(data.frame(percentiles, values), 
       aes(x = percentiles, y = values)) +
  geom_line(linetype = "dashed", color = "burlywood3") +
  geom_text(aes(label = round(values, 2)), vjust = -0.1, size = 5) +
  scale_x_continuous(breaks = percentiles) +
  labs(x = "Percentile", y = "Value") +
  ylim(0, 21) +
  theme_bw() +
  theme.obb

## combine two dataframe & remove rownames to save
OOB_RowCol_bef_t = data.frame(t(OOB_commonRowCol_bef))
OOB_RowCol_bef_t = subset(OOB_RowCol_bef_t, 
                          row.names(OOB_RowCol_bef_t) != 
                            "Variables")
OOB_RowCol_bef_t$Year = "2011-15"

OOB_RowCol_aft_t = data.frame(t(OOB_commonRowCol_aft))
OOB_RowCol_aft_t = subset(OOB_RowCol_aft_t, 
                          row.names(OOB_RowCol_aft_t) != 
                            "Variables")
OOB_RowCol_aft_t$Year = "2016-20"

# change row.names to SiteCode
OOB_RowCol_bef_t$SiteCode = row.names(OOB_RowCol_bef_t)
OOB_RowCol_aft_t$SiteCode = row.names(OOB_RowCol_aft_t)

OOB_comb = rbind(OOB_RowCol_bef_t, OOB_RowCol_aft_t)

# remove "X" in SiteCode
OOB_comb$SiteCode = gsub("X", "", OOB_comb$SiteCode)
row.names(OOB_comb) = NULL

# Remove variables not needed for PMF (C-subgroups)
csn_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
               "OC1.unadjusted.88", "OC2.unadjusted.88", 
               "OC3.unadjusted.88", "OC4.unadjusted.88",
               "OC.unadjusted.88", "EC.unadjusted.88", "OPC.unadjusted.88",
               "OP")
OOB_comb[ ,csn_remove] <- list(NULL)

# Move grouped columns to the right of the dataframe
# match(), select columns that match a specific pattern in their names
# everything(), include all remaining columns not selected by matches()
OC.EC = c("EC", "OC")
ions = c("Na.", "K.", "NH4.", "NO3", "SO4")
OOB_comb = OOB_comb %>%
  select(!(matches(OC.EC)), 
         everything())
OOB_comb = OOB_comb %>%
  select(!(matches(ions)), 
         everything())

# OOB_comb$PM25 = -999

# Replace other columns
OOB_comb = OOB_comb %>% relocate(PM25, .after = SO4)
OOB_comb = OOB_comb %>% relocate(Year, .after = PM25)
OOB_comb = OOB_comb %>% relocate(SiteCode, .before = Ag)

write.csv(OOB_comb, "CSN_OOBerror_random-forest_All.csv")

#### Plot: Missing percent file merge ####
miss_bef = read.csv("CSN_Missing_Rate_Site_until_2015_2023.03.csv")
miss_aft = read.csv("CSN_Missing_Rate_Site_from_2016_2023.03.csv")
miss_bef$X = miss_aft$X = NULL

# convert values in first column to row names
rownames(miss_bef) <- miss_bef[,1]
rownames(miss_aft) <- miss_aft[,1]

# check the potential duplicates
"EC" %in% rownames(miss_bef)
"OC" %in% rownames(miss_bef)
"OP" %in% rownames(miss_bef)

"EC" %in% rownames(miss_aft)
"OC" %in% rownames(miss_aft)
"OP" %in% rownames(miss_aft)

# "OC" already existed in rownames after 2015, so delete it first
miss_aft = 
  miss_aft[!(
    row.names(miss_aft) == "OC"),]

# select rows used as OC, EC & OP for PMF
dup_row_bef = c("EC.TOR.unadjust.88", 
                "OC.TOR.unadjusted.88", 
                "OP.TOR.unadjusted.88",
                "Accept.PM2.5")
dup_row_aft = c("EC.TOR.88", 
                "OC.88", 
                "OPC.TOR.88",
                "PM2.5RC")

# Identify the rows to rename
row_to_change_bef <- 
  rownames(miss_bef) %in% 
  dup_row_bef
summary(row_to_change_bef)

row_to_change_aft <- 
  rownames(miss_aft) %in% 
  dup_row_aft
summary(row_to_change_aft)

# Rename the rows
rownames(miss_bef)[row_to_change_bef] <- c("PM25", "EC", "OC", "OP")
rownames(miss_aft)[row_to_change_aft] <- c("EC", "OC", "OP","PM25")

# get common rownames & colnames
obb_common_rows <- intersect(row.names(miss_bef), 
                             row.names(miss_aft))

obb_common_cols <- intersect(names(miss_bef), 
                             names(miss_aft))

# Subset data frames using common rownames and column names
miss_commonRowCol_bef <- miss_bef[obb_common_rows, 
                                  obb_common_cols]
miss_commonRowCol_aft <- miss_aft[obb_common_rows, 
                                  obb_common_cols]

## combine two dataframe & remove rownames to save
miss_RowCol_bef_t = data.frame(t(miss_commonRowCol_bef))
miss_RowCol_bef_t = subset(miss_RowCol_bef_t, 
                          row.names(miss_RowCol_bef_t) != 
                            "Variables")
miss_RowCol_bef_t$Year = "2011-15"

miss_RowCol_aft_t = data.frame(t(miss_commonRowCol_aft))
miss_RowCol_aft_t = subset(miss_RowCol_aft_t, 
                          row.names(miss_RowCol_aft_t) != 
                            "Variables")
miss_RowCol_aft_t$Year = "2016-20"

# change row.names to SiteCode
miss_RowCol_bef_t$SiteCode = row.names(miss_RowCol_bef_t)
miss_RowCol_aft_t$SiteCode = row.names(miss_RowCol_aft_t)

miss_comb = rbind(miss_RowCol_bef_t, miss_RowCol_aft_t)

# remove "X" in SiteCode
miss_comb$SiteCode = gsub("X", "", miss_comb$SiteCode)
row.names(miss_comb) = NULL

# Remove variables not needed for PMF (C-subgroups)
csn_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
               "OC1.unadjusted.88", "OC2.unadjusted.88", 
               "OC3.unadjusted.88", "OC4.unadjusted.88",
               "OC.unadjusted.88", "EC.unadjusted.88", "OPC.unadjusted.88",
               "OP")
miss_comb[ ,csn_remove] <- list(NULL)

# Move grouped columns to the right of the dataframe
# match(), select columns that match a specific pattern in their names
# everything(), include all remaining columns not selected by matches()
OC.EC = c("EC", "OC")
ions = c("Na.", "K.", "NH4.", "NO3", "SO4")
miss_comb = miss_comb %>%
  select(!(matches(OC.EC)), 
         everything())
miss_comb = miss_comb %>%
  select(!(matches(ions)), 
         everything())

# Replace other columns
miss_comb = miss_comb %>% relocate(PM25, .after = SO4)
miss_comb = miss_comb %>% relocate(Year, .after = PM25)
miss_comb = miss_comb %>% relocate(SiteCode, .before = Ag)

miss_comb$State = miss_comb$Date = NULL

# change numemic rate to the XX in XX%
miss_comb[, 2:(ncol(miss_comb)-1)] = lapply(
  miss_comb[, 2:(ncol(miss_comb)-1)], 
  as.numeric)
# multiple 100
miss_comb[, 2:(ncol(miss_comb)-1)] = miss_comb[, 2:(ncol(miss_comb)-1)]*100

write.csv(miss_comb, "CSN_Missing_Qualifier_interpolation_All.csv")

# combine two dataframe & remove rownames
miss_commonRowCol_bef$Variables = rownames(miss_commonRowCol_bef)
miss_commonRowCol_aft$Variables = rownames(miss_commonRowCol_aft)
miss_plot = rbind(miss_commonRowCol_bef, miss_commonRowCol_aft)
rownames(miss_plot) <- NULL

# gather dataframe 
miss_plot = gather(miss_plot, 
                  "SiteCode", "miss_error",
                  -Variables)
colnames(miss_plot)[1] = "Species"

# the 1st appearance of sitecode-variable group is from "2011-15" and 2nd "2016-20"
miss_plot$Year = "2011-15"
miss_plot$Year[duplicated(miss_plot[, 1:2])] = "2016-20"

# group species
miss_plot$class = "Element"
miss_plot$class[miss_plot$Species %in% ions] = "Ion"
miss_plot$class[grepl("OC", miss_plot$Species, fixed = T) |
                 grepl("EC", miss_plot$Species, fixed = T) |
                 grepl("OP", miss_plot$Species, fixed = T)] = "OC.EC"

# facet_grid with "free" space
ggplot(miss_plot, aes(Species, miss_error)) +
  geom_boxplot() +
  geom_jitter(color="black", size=1, alpha=0.5) +
  facet_grid(Year ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("CSN_miss_Error") +
  theme_bw() +
  theme.obb

# exclude OC/EC subgroups
ggplot(
  subset(
    miss_plot, 
    !(grepl("88", miss_plot$Species, fixed = T))), 
  aes(Species, miss_error)) +
  geom_boxplot() +
  geom_jitter(color="black", size=1, alpha=0.5) +
  facet_grid(Year ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("CSN_miss_qualifier_interpolation") +
  theme_bw() +
  # ylim(0, 0.5) +
  ylim(0, 0.2) +
  theme.obb

# Calculate percentiles and corresponding values
percentiles <- seq(0, 100, 10)
percentiles = append(c(95, 98), percentiles)
miss_values <- quantile(miss_plot$miss_error, 
                   probs = percentiles/100)*100

miss_values <- quantile(subset(miss_plot, Variables != "PM25")$miss_error, 
                   probs = percentiles/100)*100

# Plot miss_values at each percentile
ggplot(data.frame(percentiles, miss_values), 
       aes(x = percentiles, y = miss_values)) +
  geom_line(linetype = "dashed", color = "burlywood3") +
  geom_text(aes(label = round(miss_values, 2)), vjust = -0.1, size = 5) +
  scale_x_continuous(breaks = percentiles) +
  labs(x = "Percentile", y = "Value") +
  ylim(0, 21) +
  ggtitle("CSN_miss_qualifier_interpolation") +
  theme_bw() +
  theme.obb


missing_final_plot = 
  subset(
    miss_plot, 
    !(grepl("88", miss_plot$Species, fixed = T)) &
      !(grepl("PM", miss_plot$Species, fixed = T)) &
      !(grepl("State", miss_plot$Species, fixed = T)) &
      !(grepl("SiteCode", miss_plot$Species, fixed = T)) &
      !(grepl("Date", miss_plot$Species, fixed = T)))

missing_final_plot$Species[missing_final_plot$Species == "K."] = "KIon"
missing_final_plot$Species[missing_final_plot$Species == "Na."] = "NaIon"
missing_final_plot$Species[missing_final_plot$Species == "NH4."] = "NH4Ion"
missing_final_plot$Species[missing_final_plot$Species == "NO3"] = "NO3Ion"
missing_final_plot$Species[missing_final_plot$Species == "SO4"] = "SO4Ion"

ggplot(missing_final_plot, 
       aes(Species, miss_error, 
           color = class)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(size=1, alpha=0.3) +
  facet_grid(. ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("CSN_OOB_Error") +
  scale_color_nejm() + 
  xlab(format_variable("PM25 Species")) +
  ylab(format_variable("Missing rate")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme_bw() +
  theme(panel.spacing.x = unit(0.2, "cm")) +
  theme(axis.title.y.right = element_blank(),
        panel.spacing = unit(10, "mm"),   
        legend.background = element_blank(),
        strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="grey", size=16),
        axis.title.x = element_text(color="grey25", size = 16, vjust=-2, 
                                    margin=margin(0,0,0,300),
                                    family = "Arial Unicode MS"), 
        axis.title.y = element_text(color="grey25", size = 16, vjust=2, 
                                    margin=margin(0,2,0,0), 
                                    family = "Arial Unicode MS"),
        plot.title=element_text(size=rel(2)), 
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 14, angle = 90, 
                                   hjust = 0.5, vjust = 0.5,
                                   family = "Arial Unicode MS"), 
        axis.text.y = element_text(color="grey25", size = 14, angle = 0, 
                                   hjust = 0.5, family = "Arial Unicode MS"))

####################################################################################
##### 22222. IMPROVE - Fill the Missing, till & after 2015 separately #####
####################################################################################
#### generate basic data for filling ####
imp_daily = fread("IMPROVE_Component_with_missing.csv")

imp_daily$V1 = NULL
imp_daily$Date = as.Date(imp_daily$Date)
sapply(imp_daily, class)
imp_daily = subset(imp_daily, Date > as.Date("2010-12-31"))

# remove OPC subgroup
colnames(imp_daily)[35:48] # OPC sub-group
colnames(imp_daily)[36:47] # OPC sub-group
imp_daily_opt_sub = imp_daily[ ,36:47]
imp_daily = cbind(imp_daily[,1:35], imp_daily[,48:ncol(imp_daily)])

# calculate the overall percentages of NA for selected species
site_day_NA_count = ddply(imp_daily, 
                          .(SiteCode), 
                          summarise,
                          count = length(Date),
                          Mg.NA = sum(is.na(Mg)),
                          EC.NA = sum(is.na(EC3)),
                          OC.NA = sum(is.na(OC3)),
                          SO4.NA = sum(is.na(SO4)))

# count the percent of NA in representative PM species from each type
site_day_NA_count$Mg.NA.per = round(site_day_NA_count$Mg.NA/site_day_NA_count$count, 3)
site_day_NA_count$EC.NA.per = round(site_day_NA_count$EC.NA/site_day_NA_count$count, 3)
site_day_NA_count$OC.NA.per = round(site_day_NA_count$OC.NA/site_day_NA_count$count, 3)
site_day_NA_count$SO4.NA.per = round(site_day_NA_count$SO4.NA/site_day_NA_count$count, 3)

# check the sites with high percentage NAs
site.lots.NA = site_day_NA_count$SiteCode[
  site_day_NA_count$Mg.NA.per > 0.5 & 
    site_day_NA_count$SO4.NA.per > 0.5 &
    site_day_NA_count$EC.NA.per > 0.5 & 
    site_day_NA_count$OC.NA.per > 0.5]
length(site.lots.NA)

imp_daily_halfNA = subset(imp_daily, SiteCode %in% site.lots.NA)


# plot their distribution
imp_meta_sites = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/R - original IMPROVE/IMPROVE metadata 192 sample sites info 2010-20.csv")
imp_sites = select(imp_meta_sites, Code, State, Latitude,  Longitude)
names(imp_sites)[1] = "SiteCode"
imp_sites$High.P.NA = "N"
imp_sites$High.P.NA[imp_sites$SiteCode %in% site.lots.NA] = "Y"

# only keep the sites used
imp_sites = subset(imp_sites, 
                   SiteCode %in% 
                     unique(imp_daily$SiteCode))
table(imp_sites$High.P.NA)

# mainland US
imp_sites = subset(imp_sites,
                   Latitude < 50 & Latitude > 20 &
                     Longitude > -130 & Longitude < -60)
length(unique(imp_sites$SiteCode))


UScounty <- map_data("county")

ggplot(subset(imp_sites), # , Longitude > -130 & Latitude > 20
       aes(Longitude, Latitude, group= High.P.NA)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.8, aes(shape = High.P.NA, color = High.P.NA)) +
  scale_color_npg() +
  theme_bw()


#### filling the NAs for each site (logged, no negative)- na.omit & check Mix Error ####
# imp_miss = subset(imp_daily, !(SiteCode %in% site.lots.NA))
imp_miss = data.frame(imp_daily)

# exclude states not located in the "Continental and mainland USA"
imp_miss = imp_miss[with(imp_miss, order(State, SiteCode, Date)), ]
imp_miss = subset(imp_miss, !(State %in% c("AK", "HI", "PR", "VI"))) 

# site almost all NA
imp_miss = subset(imp_miss, 
                  !(SiteCode %in% c("DETR1", "RENO1", "RENO2", "RENO3")))

# remove those not directly detected, except of RC.PM2.5, ammoniaSO4, ammoniaNO3, OC & EC
imp_miss$SeaSalt = imp_miss$Soil = imp_miss$PM10 = imp_miss$RC.PM10 = 
  imp_miss$site.date.qualifier = imp_miss$OC_UCD = imp_miss$EC_UCD = 
  imp_miss$OMC = imp_miss$CM_calculated = imp_miss$TC =
  imp_miss$RC.PM2.5 = NULL
imp_miss$OPT = NULL # OPT is deleted cause it is not used to calculated OC & EC


## remove the rows where all component concentrations are NAs
col.withAllNA = ncol(imp_miss)
cols.comp = 4:col.withAllNA # columns for PM/components
col.component = ncol(imp_miss[, cols.comp])

imp_miss_noAllNA = subset(imp_miss, 
                          rowSums(is.na(imp_miss[, cols.comp])) != 
                            col.component)
n.site = length(unique(imp_miss_noAllNA$SiteCode))

imp_miss_noAllNA = 
  relocate(imp_miss_noAllNA, PM2.5, .after = SiteCode)
names(imp_miss_noAllNA)

# create data.frame to store results
running_avg_sum = linear_sum = mice_sum = rf_sum = NULL
mix_error_intp_pstv_summary = NULL

# create data.frame with the first column being the colnames of imp_miss_noAllNA for matching data
imp_var = data.frame(colnames(imp_miss_noAllNA))
colnames(imp_var)[1] = "Variables"
p_miss_summary = imp_var
rf_vw_oob_summary = data.frame(imp_var[4:nrow(imp_var), ])
names(rf_vw_oob_summary)[1] = "Variables"

#### start interpolating ####
for (i in 1:n.site){ 
  
  ###### 1.prepare data set - remove those with only NA & log ######
  site.study = unique(imp_miss_noAllNA$SiteCode)[i]
  site_single_noAllNA = subset(imp_miss_noAllNA, SiteCode == site.study)
  # dim(site_single_noAllNA)
  # head(site_single_noAllNA[, 1:10])
  
  row.No = nrow(site_single_noAllNA)
  
  ## detect the percent of missing concentration values for each component
  p_miss_with_neg <- data.frame(
    unlist(
      lapply(
        site_single_noAllNA, 
        function(x) 
          sum(is.na(x))))/row.No)
  colnames(p_miss_with_neg) = site.study
  
  ## summarise the missing values for each site
  p_miss_summary = cbind(p_miss_summary, p_miss_with_neg)
  
  ## log all value to avoid negative interpolation 
  site_single_log = cbind(select(site_single_noAllNA,
                                 SiteCode, Date, State),
                          site_single_noAllNA %>% 
                            dplyr::select(where(is.numeric)) %>%
                            log())
  
  ##### 2.prepare data for interpolation Method Comparison - mix error #####
  # total NA percentage in components
  na.count = sum(is.na(site_single_log[ ,cols.comp]))
  na.percent = na.count/(row.No * col.component)
  
  # random set a dataframe with same percent of NAs as the original dataset
  # no extra PM2.5 dataset for IMPROVE, thus, if lack PM2.5, only abundon
  site_single_no_NA = na.omit(site_single_log)
  cols.comp.use = cols.comp
  
  site_single_rdm_NA = cbind(site_single_no_NA[ ,1:3], 
                             prodNA(site_single_no_NA[ ,cols.comp.use], 
                                    noNA = na.percent))
  
  ##### 3.Interpolation - Method Comparison #####
  #### interpolation, below is set for the randomly set NA, for method performance comparison
  sgl_intp_rdNA_running_avg = sgl_intp_rdNA_linear = site_single_rdm_NA
  
  for(j in cols.comp.use){
    # interpolation1: linear  
    sgl_intp_rdNA_linear[, j] = na_interpolation(site_single_rdm_NA[, j]) 
    # interpolation2: running window average 
    sgl_intp_rdNA_running_avg[, j] = na_ma(site_single_rdm_NA[, j], 
                                           weighting = "simple", k = 4) 
  }
  
  # interpolation3: mice, using MCMC, multivariate interpolation by chained equation
  sgl_species = site_single_rdm_NA[, cols.comp.use]
  summary(sgl_species)
  
  sgl_intp_rdNA_mice <- mice(site_single_rdm_NA[, cols.comp.use], 
                             maxit=0, m = 50,
                             remove.collinear = F)
  # sgl_intp_rdNA_mice$loggedEvents
  # collinear exists in the dataset, mice will automatically remove NO3 & SO4 as a result
  # https://stefvanbuuren.name/fimd/sec-toomany.html
  
  # fills in the missing data, and returns the completed data
  sgl_intp_rdNA_mice_dslist <- complete(sgl_intp_rdNA_mice, "all")
  
  ## calculate the average of all interpolations in the array
  sgl_intp_rdNA_mice_avg = data.frame(aaply(
    laply(
      sgl_intp_rdNA_mice_dslist, as.matrix),  
    c(2, 3), 
    mean))
  sgl_intp_rdNA_mice_avg_use = cbind(site_single_rdm_NA[, 1:3], 
                                     sgl_intp_rdNA_mice_avg)
  
  #interpolation4: missForest, using random forest
  sgl_intp_rdNA_rf_mf = missForest(site_single_rdm_NA[, cols.comp.use])
  sgl_intp_rdNA_rf = cbind(site_single_rdm_NA[, 1:3], 
                           data.frame(sgl_intp_rdNA_rf_mf$ximp))
  
  me.running.avg = mixError(sgl_intp_rdNA_running_avg[, cols.comp.use], 
                            site_single_rdm_NA[, cols.comp.use], 
                            site_single_no_NA[, cols.comp.use]) 
  me.linear = mixError(sgl_intp_rdNA_linear[, cols.comp.use], 
                       site_single_rdm_NA[, cols.comp.use], 
                       site_single_no_NA[, cols.comp.use]) 
  me.mice = mixError(sgl_intp_rdNA_mice_avg_use[, cols.comp.use], 
                     site_single_rdm_NA[, cols.comp.use], 
                     site_single_no_NA[, cols.comp.use]) 
  me.rf = mixError(sgl_intp_rdNA_rf[, cols.comp.use], 
                   site_single_rdm_NA[, cols.comp.use], 
                   site_single_no_NA[, cols.comp.use]) 
  
  mix_error_intp = data.frame(State = site_single_rdm_NA$State[1], 
                              SiteCode = site.study, 
                              percent.NA = na.percent, 
                              row.total = row.all, 
                              row.not.all.NA = row.No, 
                              row.no.NA = nrow(site_single_rdm_NA), 
                              me.running.avg = me.running.avg, 
                              me.linear = me.linear, 
                              me.mice = me.mice, 
                              me.rf = me.rf)
  rownames(mix_error_intp)[1] = i
  mix_error_intp_pstv_summary = rbind(mix_error_intp_pstv_summary, 
                                      mix_error_intp[1, ])
  
  
  ##### 4.Interpolation - generate data for future analyses #####
  sgl_intp_running_avg_pro = sgl_intp_linear_pro = site_single_log
  
  for(k in cols.comp.use){
    # interpolation1: linear  
    sgl_intp_linear_pro[, k] = na_interpolation(site_single_log[, k]) 
    # interpolation2: running window average 
    sgl_intp_running_avg_pro[, k] = na_ma(site_single_log[, k], 
                                          weighting = "simple", k = 4) 
  }
  # covert logged concentrations to original
  sgl_intp_linear = cbind(sgl_intp_linear_pro[, 1:3], 
                          exp(sgl_intp_linear_pro[, cols.comp.use]))
  sgl_intp_running_avg = cbind(sgl_intp_running_avg_pro[, 1:3], 
                               exp(sgl_intp_running_avg_pro[, cols.comp.use]))
  
  # interpolation3: mice, using MCMC, multiple interpolation
  sgl_intp_mice_org <- mice(site_single_log[, cols.comp.use], 
                            maxit=0, m = 50, 
                            remove.collinear = F)
  sgl_intp_mice_dslist <- complete(sgl_intp_mice_org, "all")
  
  ## get the average of data.frame in the array
  sgl_intp_mice_avg = data.frame(aaply(
    laply(sgl_intp_mice_dslist, as.matrix),  
    c(2, 3), mean))
  sgl_intp_mice = cbind(site_single_log[, 1:3], 
                        exp(sgl_intp_mice_avg))
  
  #interpolation4: missForest, using random forest, option "variablewise = T"
  sgl_intp_rf_mf = missForest(site_single_log[, cols.comp.use], 
                              variablewise = T)
  sgl_intp_rf = cbind(site_single_log[, 1:3], 
                      exp(sgl_intp_rf_mf$ximp))
  
  
  # the OOB error for each PM species & interpolated values combine
  rf_vw_oob = data.frame(sgl_intp_rf_mf$OOBerror)
  rf_vw_oob_summary = cbind(rf_vw_oob_summary, rf_vw_oob[, 1])
  running_avg_sum = rbind(running_avg_sum, sgl_intp_running_avg)
  linear_sum = rbind(linear_sum, sgl_intp_linear)
  mice_sum = rbind(mice_sum, sgl_intp_mice)
  rf_sum = rbind(rf_sum, sgl_intp_rf)
}

rownames(p_miss_summary) = 1:nrow(p_miss_summary)

#### output interpolation results ####

# out put results for data until 2015
write.csv(p_miss_summary, "IMPROVE_Missing_Rate_Site-level.csv")
write.csv(mix_error_intp_pstv_summary, "IMPROVE_interpulation_Mix_Error.csv")
write.csv(rf_vw_oob_summary, "IMPROVE_OOBerror_random-forest.csv")

# write.csv(running_avg_sum, "IMPROVE_interpulation_running-6-average_afterLog.csv")
# write.csv(linear_sum, "IMPROVE_interpulation_linear_afterLog.csv")
# write.csv(mice_sum, "IMPROVE_interpulation_multi-mice_afterLog.csv")
# write.csv(rf_sum, "IMPROVE_interpulation_random-forest_afterLog.csv")

write.csv(running_avg_sum, "IMPROVE_interpulation_running-average_2023.csv")
write.csv(linear_sum, "IMPROVE_interpulation_linear_2023.csv")
write.csv(mice_sum, "IMPROVE_interpulation_multi-mice_2023.csv")
write.csv(rf_sum, "IMPROVE_interpulation_random-forest_2023.csv")

#### plot: Mix Erro distribution ####
ME_summary = read.csv("IMPROVE_interpulation_Mix_Error.csv")
ME_plot = select(ME_summary, SiteCode, Running_Avg, Linear, Multiple, Random_Forest)

## plotting
theme.mix.err = theme(axis.title.y.right = element_blank(),
                      panel.spacing = unit(10, "mm"),   
                      legend.background = element_blank(),
                      strip.text = element_text(face="bold", size=rel(1.5)),
                      strip.background = element_rect(fill="lightblue", colour="grey", linewidth=16),
                      axis.title.x = element_text(color="grey25", size = 20, vjust=-2), #, margin=margin(0,0,0,300)
                      axis.title.y = element_text(color="grey25", size = 20, vjust=2), #, margin=margin(0,2,0,0)
                      plot.title=element_text(size=rel(2)), 
                      axis.text.x = element_text(color="grey25", size = 18, angle = 30, hjust = 0.5, vjust = 0.5), plot.margin = unit(c(2,1,2, 2), "lines"),
                      axis.text.y = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5))

ggplot(ME_plot, aes(Interpolations, NRMSE, fill = Interpolations)) +
  geom_boxplot() +
  geom_jitter(color="black", size=1.1, alpha=0.5) +
  facet_wrap(Year ~.) + 
  scale_fill_npg() + 
  ylim(0, 0.65) +
  theme_bw() +
  theme(legend.position = "none") +
  theme.mix.err

#### plot: OOB (out-of-bag) Error distribution ####
OOB_summary = read.csv("IMPROVE_OOBerror_random-forest.csv")

# gather dataframe 
OOB_plot = gather(OOB_summary, 
                  "SiteCode", "OOB_error",
                  -Variables)
colnames(OOB_plot)[1] = "Species"

# group species
ions = c("Na.", "K.", "NH4.", "NO3", "SO4")
OOB_plot$class = "Element"
OOB_plot$class[OOB_plot$Species %in% ions] = "Ion"
OOB_plot$class[grepl("OC", OOB_plot$Species, fixed = T) |
                 grepl("EC", OOB_plot$Species, fixed = T) |
                 grepl("OP", OOB_plot$Species, fixed = T)] = "OC.EC"

## plotting
theme.obb = theme(axis.title.y.right = element_blank(),
                  panel.spacing = unit(10, "mm"),   
                  legend.background = element_blank(),
                  strip.text = element_text(face="bold", size=rel(1.5)),
                  strip.background = element_rect(fill="lightblue", colour="grey", size=16),
                  axis.title.x = element_text(color="grey25", size = 16, vjust=-2, margin=margin(0,0,0,300)), 
                  axis.title.y = element_text(color="grey25", size = 16, vjust=2, margin=margin(0,2,0,0)),
                  plot.title=element_text(size=rel(2)), 
                  axis.text.x = element_text(color="grey25", size = 14, angle = 90, hjust = 0.5, vjust = 0.5), plot.margin = unit(c(2,1,2, 2), "lines"),
                  axis.text.y = element_text(color="grey25", size = 14, angle = 0, hjust = 0.5))

# facet_grid with "free" space
ggplot(OOB_plot, aes(Species, OOB_error)) +
  geom_boxplot() +
  geom_jitter(color="black", size=1, alpha=0.5) +
  facet_grid(Year ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("IMPROVE_OOB_Error") +
  theme_bw() +
  theme.obb

# exclude OC/EC subgroups
ggplot(
  subset(
    OOB_plot, 
    !(grepl("88", OOB_plot$Species, fixed = T))), 
  aes(Species, OOB_error)) +
  geom_boxplot() +
  geom_jitter(color="black", size=1, alpha=0.5) +
  facet_grid(Year ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("IMPROVE_OOB_Error") +
  theme_bw() +
  theme.obb

OOB_final_plot = 
  subset(
    OOB_plot, 
    !(grepl("88", OOB_plot$Species, fixed = T)) &
      !(grepl("PM", OOB_plot$Species, fixed = T)))
colnames(OOB_final_plot)[1] = "Species"

OOB_final_plot$Species[OOB_final_plot$Species == "K."] = "KIon"
OOB_final_plot$Species[OOB_final_plot$Species == "Na."] = "NaIon"
OOB_final_plot$Species[OOB_final_plot$Species == "NH4."] = "NH4Ion"
OOB_final_plot$Species[OOB_final_plot$Species == "NO3"] = "NO3Ion"
OOB_final_plot$Species[OOB_final_plot$Species == "SO4"] = "SO4Ion"


ggplot(OOB_final_plot, 
       aes(Species, OOB_error)) +
  geom_boxplot() +
  geom_jitter(color="black", size=1, alpha=0.5) +
  facet_grid(Year ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("IMPROVE_OOB_Error") +
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme_bw() +
  theme(panel.spacing.x = unit(0.2, "cm")) +
  theme(panel.spacing.y = unit(0.2, "cm")) +
  theme(axis.title.y.right = element_blank(),
        panel.spacing = unit(10, "mm"),   
        legend.background = element_blank(),
        strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="grey", size=16),
        axis.title.x = element_text(color="grey25", size = 16, vjust=-2, 
                                    margin=margin(0,0,0,300),
                                    family = "Arial Unicode MS"), 
        axis.title.y = element_text(color="grey25", size = 16, vjust=2, 
                                    margin=margin(0,2,0,0), 
                                    family = "Arial Unicode MS"),
        plot.title=element_text(size=rel(2)), 
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 14, angle = 90, 
                                   hjust = 0.5, vjust = 0.5,
                                   family = "Arial Unicode MS"), 
        axis.text.y = element_text(color="grey25", size = 14, angle = 0, 
                                   hjust = 0.5, family = "Arial Unicode MS"))

ggplot(OOB_final_plot, 
       aes(Species, OOB_error, 
           color = class)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(size=1, alpha=0.3) +
  facet_grid(. ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("IMPROVE_OOB_Error") +
  scale_color_nejm() + 
  xlab(format_variable("PM25 Species")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme_bw() +
  theme(panel.spacing.x = unit(0.2, "cm")) +
  theme(axis.title.y.right = element_blank(),
        panel.spacing = unit(10, "mm"),   
        legend.background = element_blank(),
        strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="grey", size=16),
        axis.title.x = element_text(color="grey25", size = 16, vjust=-2, 
                                    margin=margin(0,0,0,300),
                                    family = "Arial Unicode MS"), 
        axis.title.y = element_text(color="grey25", size = 16, vjust=2, 
                                    margin=margin(0,2,0,0), 
                                    family = "Arial Unicode MS"),
        plot.title=element_text(size=rel(2)), 
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 14, angle = 90, 
                                   hjust = 0.5, vjust = 0.5,
                                   family = "Arial Unicode MS"), 
        axis.text.y = element_text(color="grey25", size = 14, angle = 0, 
                                   hjust = 0.5, family = "Arial Unicode MS"))

# Calculate percentiles and corresponding values
percentiles <- seq(0, 100, 10)
percentiles = append(c(95, 98), percentiles)
values <- quantile(OOB_plot$OOB_error, 
                   probs = percentiles/100)

values <- quantile(subset(OOB_plot, Variables != "PM25")$OOB_error, 
                   probs = percentiles/100)

# Plot values at each percentile
ggplot(data.frame(percentiles, values), 
       aes(x = percentiles, y = values)) +
  geom_line(linetype = "dashed", color = "burlywood3") +
  geom_text(aes(label = round(values, 2)), vjust = -0.1, size = 5) +
  scale_x_continuous(breaks = percentiles) +
  labs(x = "Percentile", y = "Value") +
  ylim(0, 21) +
  theme_bw() +
  theme.obb

## combine two dataframe & remove rownames to save
OOB_RowCol_bef_t = data.frame(t(OOB_commonRowCol_bef))
OOB_RowCol_bef_t = subset(OOB_RowCol_bef_t, 
                          row.names(OOB_RowCol_bef_t) != 
                            "Variables")
OOB_RowCol_bef_t$Year = "2011-15"

OOB_RowCol_aft_t = data.frame(t(OOB_commonRowCol_aft))
OOB_RowCol_aft_t = subset(OOB_RowCol_aft_t, 
                          row.names(OOB_RowCol_aft_t) != 
                            "Variables")
OOB_RowCol_aft_t$Year = "2016-20"

# change row.names to SiteCode
OOB_RowCol_bef_t$SiteCode = row.names(OOB_RowCol_bef_t)
OOB_RowCol_aft_t$SiteCode = row.names(OOB_RowCol_aft_t)

OOB_comb = rbind(OOB_RowCol_bef_t, OOB_RowCol_aft_t)

# remove "X" in SiteCode
OOB_comb$SiteCode = gsub("X", "", OOB_comb$SiteCode)
row.names(OOB_comb) = NULL

# Remove variables not needed for PMF (C-subgroups)
IMPROVE_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
               "OC1.unadjusted.88", "OC2.unadjusted.88", 
               "OC3.unadjusted.88", "OC4.unadjusted.88",
               "OC.unadjusted.88", "EC.unadjusted.88", "OPC.unadjusted.88",
               "OP")
OOB_comb[ ,IMPROVE_remove] <- list(NULL)

# Move grouped columns to the right of the dataframe
# match(), select columns that match a specific pattern in their names
# everything(), include all remaining columns not selected by matches()
OC.EC = c("EC", "OC")
ions = c("Na.", "K.", "NH4.", "NO3", "SO4")
OOB_comb = OOB_comb %>%
  select(!(matches(OC.EC)), 
         everything())
OOB_comb = OOB_comb %>%
  select(!(matches(ions)), 
         everything())

# OOB_comb$PM25 = -999

# Replace other columns
OOB_comb = OOB_comb %>% relocate(PM25, .after = SO4)
OOB_comb = OOB_comb %>% relocate(Year, .after = PM25)
OOB_comb = OOB_comb %>% relocate(SiteCode, .before = Ag)

write.csv(OOB_comb, "IMPROVE_OOBerror_random-forest_All.csv")

#### Plot: Missing percent file merge ####
miss_bef = read.csv("IMPROVE_Missing_Rate_Site_until_2015_2023.03.csv")
miss_aft = read.csv("IMPROVE_Missing_Rate_Site_from_2016_2023.03.csv")
miss_bef$X = miss_aft$X = NULL

# convert values in first column to row names
rownames(miss_bef) <- miss_bef[,1]
rownames(miss_aft) <- miss_aft[,1]

# check the potential duplicates
"EC" %in% rownames(miss_bef)
"OC" %in% rownames(miss_bef)
"OP" %in% rownames(miss_bef)

"EC" %in% rownames(miss_aft)
"OC" %in% rownames(miss_aft)
"OP" %in% rownames(miss_aft)

# "OC" already existed in rownames after 2015, so delete it first
miss_aft = 
  miss_aft[!(
    row.names(miss_aft) == "OC"),]

# select rows used as OC, EC & OP for PMF
dup_row_bef = c("EC.TOR.unadjust.88", 
                "OC.TOR.unadjusted.88", 
                "OP.TOR.unadjusted.88",
                "Accept.PM2.5")
dup_row_aft = c("EC.TOR.88", 
                "OC.88", 
                "OPC.TOR.88",
                "PM2.5RC")

# Identify the rows to rename
row_to_change_bef <- 
  rownames(miss_bef) %in% 
  dup_row_bef
summary(row_to_change_bef)

row_to_change_aft <- 
  rownames(miss_aft) %in% 
  dup_row_aft
summary(row_to_change_aft)

# Rename the rows
rownames(miss_bef)[row_to_change_bef] <- c("PM25", "EC", "OC", "OP")
rownames(miss_aft)[row_to_change_aft] <- c("EC", "OC", "OP","PM25")

# get common rownames & colnames
obb_common_rows <- intersect(row.names(miss_bef), 
                             row.names(miss_aft))

obb_common_cols <- intersect(names(miss_bef), 
                             names(miss_aft))

# Subset data frames using common rownames and column names
miss_commonRowCol_bef <- miss_bef[obb_common_rows, 
                                  obb_common_cols]
miss_commonRowCol_aft <- miss_aft[obb_common_rows, 
                                  obb_common_cols]

## combine two dataframe & remove rownames to save
miss_RowCol_bef_t = data.frame(t(miss_commonRowCol_bef))
miss_RowCol_bef_t = subset(miss_RowCol_bef_t, 
                           row.names(miss_RowCol_bef_t) != 
                             "Variables")
miss_RowCol_bef_t$Year = "2011-15"

miss_RowCol_aft_t = data.frame(t(miss_commonRowCol_aft))
miss_RowCol_aft_t = subset(miss_RowCol_aft_t, 
                           row.names(miss_RowCol_aft_t) != 
                             "Variables")
miss_RowCol_aft_t$Year = "2016-20"

# change row.names to SiteCode
miss_RowCol_bef_t$SiteCode = row.names(miss_RowCol_bef_t)
miss_RowCol_aft_t$SiteCode = row.names(miss_RowCol_aft_t)

miss_comb = rbind(miss_RowCol_bef_t, miss_RowCol_aft_t)

# remove "X" in SiteCode
miss_comb$SiteCode = gsub("X", "", miss_comb$SiteCode)
row.names(miss_comb) = NULL

# Remove variables not needed for PMF (C-subgroups)
IMPROVE_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
               "OC1.unadjusted.88", "OC2.unadjusted.88", 
               "OC3.unadjusted.88", "OC4.unadjusted.88",
               "OC.unadjusted.88", "EC.unadjusted.88", "OPC.unadjusted.88",
               "OP")
miss_comb[ ,IMPROVE_remove] <- list(NULL)

# Move grouped columns to the right of the dataframe
# match(), select columns that match a specific pattern in their names
# everything(), include all remaining columns not selected by matches()
OC.EC = c("EC", "OC")
ions = c("Na.", "K.", "NH4.", "NO3", "SO4")
miss_comb = miss_comb %>%
  select(!(matches(OC.EC)), 
         everything())
miss_comb = miss_comb %>%
  select(!(matches(ions)), 
         everything())

# Replace other columns
miss_comb = miss_comb %>% relocate(PM25, .after = SO4)
miss_comb = miss_comb %>% relocate(Year, .after = PM25)
miss_comb = miss_comb %>% relocate(SiteCode, .before = Ag)

miss_comb$State = miss_comb$Date = NULL

# change numemic rate to the XX in XX%
miss_comb[, 2:(ncol(miss_comb)-1)] = lapply(
  miss_comb[, 2:(ncol(miss_comb)-1)], 
  as.numeric)
# multiple 100
miss_comb[, 2:(ncol(miss_comb)-1)] = miss_comb[, 2:(ncol(miss_comb)-1)]*100

write.csv(miss_comb, "IMPROVE_Missing_Qualifier_interpolation_All.csv")

# combine two dataframe & remove rownames
miss_commonRowCol_bef$Variables = rownames(miss_commonRowCol_bef)
miss_commonRowCol_aft$Variables = rownames(miss_commonRowCol_aft)
miss_plot = rbind(miss_commonRowCol_bef, miss_commonRowCol_aft)
rownames(miss_plot) <- NULL

# gather dataframe 
miss_plot = gather(miss_plot, 
                   "SiteCode", "miss_error",
                   -Variables)
colnames(miss_plot)[1] = "Species"

# the 1st appearance of sitecode-variable group is from "2011-15" and 2nd "2016-20"
miss_plot$Year = "2011-15"
miss_plot$Year[duplicated(miss_plot[, 1:2])] = "2016-20"

# group species
miss_plot$class = "Element"
miss_plot$class[miss_plot$Species %in% ions] = "Ion"
miss_plot$class[grepl("OC", miss_plot$Species, fixed = T) |
                  grepl("EC", miss_plot$Species, fixed = T) |
                  grepl("OP", miss_plot$Species, fixed = T)] = "OC.EC"

# facet_grid with "free" space
ggplot(miss_plot, aes(Species, miss_error)) +
  geom_boxplot() +
  geom_jitter(color="black", size=1, alpha=0.5) +
  facet_grid(Year ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("IMPROVE_miss_Error") +
  theme_bw() +
  theme.obb

# exclude OC/EC subgroups
ggplot(
  subset(
    miss_plot, 
    !(grepl("88", miss_plot$Species, fixed = T))), 
  aes(Species, miss_error)) +
  geom_boxplot() +
  geom_jitter(color="black", size=1, alpha=0.5) +
  facet_grid(Year ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("IMPROVE_miss_qualifier_interpolation") +
  theme_bw() +
  # ylim(0, 0.5) +
  ylim(0, 0.2) +
  theme.obb

# Calculate percentiles and corresponding values
percentiles <- seq(0, 100, 10)
percentiles = append(c(95, 98), percentiles)
miss_values <- quantile(miss_plot$miss_error, 
                        probs = percentiles/100)*100

miss_values <- quantile(subset(miss_plot, Variables != "PM25")$miss_error, 
                        probs = percentiles/100)*100

# Plot miss_values at each percentile
ggplot(data.frame(percentiles, miss_values), 
       aes(x = percentiles, y = miss_values)) +
  geom_line(linetype = "dashed", color = "burlywood3") +
  geom_text(aes(label = round(miss_values, 2)), vjust = -0.1, size = 5) +
  scale_x_continuous(breaks = percentiles) +
  labs(x = "Percentile", y = "Value") +
  ylim(0, 21) +
  ggtitle("IMPROVE_miss_qualifier_interpolation") +
  theme_bw() +
  theme.obb


missing_final_plot = 
  subset(
    miss_plot, 
    !(grepl("88", miss_plot$Species, fixed = T)) &
      !(grepl("PM", miss_plot$Species, fixed = T)) &
      !(grepl("State", miss_plot$Species, fixed = T)) &
      !(grepl("SiteCode", miss_plot$Species, fixed = T)) &
      !(grepl("Date", miss_plot$Species, fixed = T)))

missing_final_plot$Species[missing_final_plot$Species == "K."] = "KIon"
missing_final_plot$Species[missing_final_plot$Species == "Na."] = "NaIon"
missing_final_plot$Species[missing_final_plot$Species == "NH4."] = "NH4Ion"
missing_final_plot$Species[missing_final_plot$Species == "NO3"] = "NO3Ion"
missing_final_plot$Species[missing_final_plot$Species == "SO4"] = "SO4Ion"

ggplot(missing_final_plot, 
       aes(Species, miss_error, 
           color = class)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(size=1, alpha=0.3) +
  facet_grid(. ~ class, scales = "free", space = "free") + 
  # scale_fill_npg() + 
  ggtitle("IMPROVE_OOB_Error") +
  scale_color_nejm() + 
  xlab(format_variable("PM25 Species")) +
  ylab(format_variable("Missing rate")) +
  # to display the superscript & subscript in axis labels
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme_bw() +
  theme(panel.spacing.x = unit(0.2, "cm")) +
  theme(axis.title.y.right = element_blank(),
        panel.spacing = unit(10, "mm"),   
        legend.background = element_blank(),
        strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="grey", size=16),
        axis.title.x = element_text(color="grey25", size = 16, vjust=-2, 
                                    margin=margin(0,0,0,300),
                                    family = "Arial Unicode MS"), 
        axis.title.y = element_text(color="grey25", size = 16, vjust=2, 
                                    margin=margin(0,2,0,0), 
                                    family = "Arial Unicode MS"),
        plot.title=element_text(size=rel(2)), 
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 14, angle = 90, 
                                   hjust = 0.5, vjust = 0.5,
                                   family = "Arial Unicode MS"), 
        axis.text.y = element_text(color="grey25", size = 14, angle = 0, 
                                   hjust = 0.5, family = "Arial Unicode MS"))

