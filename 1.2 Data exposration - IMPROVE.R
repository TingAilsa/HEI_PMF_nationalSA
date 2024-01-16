##clear environment
# rm(list=ls())

##set working directory
setwd("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE")
getwd()
data.dir <- "Users/ztttttt/Documents/HEI PMF/R - original IMPROVE"

##packages in need
require(tidyr) # separate{tidyr}, gather{tidyr}, spread{tidyr},  spread is VIP function, str_split_fixed{stringr} is better than separate
require(stats) # aggregate{stats}, VIP function
require(ggplot2)
require(scales) # percent{}
require(stringr) # str_split_fixed, separate one column into multiple
require(dplyr)
require(plyr)
require(lubridate)
require(gridExtra) #grid.arrange{}
require(grid) #textGrob{}

library(maps) 
library(usmap)
library(ggsci) 

##########################################################################################
####### read data #########
imp_data = read.csv("IMPROVE component only 10092022.csv")
imp_data$X = NULL
imp_data$Date = as.Date(imp_data$Date)
imp_data$CompName[is.na(imp_data$CompName)] = "Na"
# comp_name = read.csv("component name reference.csv")
# imp_data = join(imp_data, comp_name)
# imp_data$CompName = imp_data$CompName.use
# imp_data$CompName.use = NULL
# write.csv(imp_data, "IMPROVE component only 10092022.csv")

state_site = select(imp_data, State, SiteCode)
state_site = ddply(state_site, .( State, SiteCode), count)
state_count = select(state_site, State)
state_count = ddply(state_count, .( State), count)
colnames(state_count)[2] = "site.in.state"
state_nrow = data.frame(table(imp_data$State))
colnames(state_nrow) = c("State", "nrow.data")

method_component = select(imp_data, MethodSimple, CompName)
method_component = method_component[!duplicated(method_component), ]

imp_meta_sites = read.csv("IMPROVE metadata 196 sample sites info 2010-20.csv")
imp_meta_sites$StartDate = as.Date(imp_meta_sites$StartDate)
imp_meta_sites$EndDate = as.Date(imp_meta_sites$EndDate)

imp_meta_para = read.csv("IMPROVE metadata 109 parameters & methods 2010-20.csv")
imp_meta_flags = read.csv("IMPROVE metadata 19 flags 2010-20.csv")

##########################################################################################
####### primary exploration & process #########
imp_meta_sites = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/IMPROVE sites.txt",
                          sep = ",", dec = ".")
imp_meta_sites$StartDate = as.Date(imp_meta_sites$StartDate, format = "%m/%d/%Y")
imp_meta_sites$EndDate = as.Date(imp_meta_sites$EndDate, format = "%m/%d/%Y")
imp_meta_sites_use = subset(imp_meta_sites, EndDate > as.Date("2010-01-01"))
imp_meta_sites_use = imp_meta_sites_use[with(imp_meta_sites_use, order(Country, State, County, Latitude)), ]
imp_meta_sites_use = subset(imp_meta_sites_use, Country == "US")
# one site SOGP1, the DemographicCode is Rural, LandUseCode is Agricultural, for the rest sites, these two code are -999/unknow
colnames(imp_meta_sites_use)[2] = "SiteCode"
meta_sites_use = as.character(imp_meta_sites_use$SiteCode)
imp_meta_sites_use.1 = subset(imp_meta_sites_use, EndDate > as.Date("2011-01-01"))

imp_meta_site_history = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CNS original/IMPROVE sites history.txt",
                                 sep = ",", dec = ".")

imp_meta_flags = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/IMPROVE flags.txt",
                          sep = ",", dec = ".")
imp_meta_para = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/IMPROVE parameters.txt",
                         sep = ",", dec = ".")
imp_meta_para[c("CompName", "anyname")] = str_split_fixed(imp_meta_para$ParamCode, "f", 2)
imp_meta_para$CompName = ifelse(imp_meta_para$Units != "ug/m^3", imp_meta_para$ParamCode, imp_meta_para$CompName)
imp_meta_para$anyname = NULL
imp_meta_para$CompName[imp_meta_para$CompName == "MF"] = "PM2.5"
imp_meta_para$CompName[imp_meta_para$CompName == "MT"] = "PM10"
imp_meta_para$CompName[imp_meta_para$CompName == "RCFM"] = "RC.PM2.5"
imp_meta_para$CompName[imp_meta_para$CompName == "RCTM"] = "RC.PM10"

write.csv(imp_meta_sites_use, "IMPROVE metadata 196 sample sites info 2010-20.csv")
write.csv(imp_meta_flags, "IMPROVE metadata 19 flags 2010-20.csv")
write.csv(imp_meta_para, "IMPROVE metadata 109 parameters 2010-20.csv")

imp_data = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/ailsa2be_20221008_205315_uNQ0v IMPROVE.txt",
                    sep = ",", dec = ".")
head(imp_data)
imp_data$Date = as.Date(imp_data$Date, format = "%m/%d/%Y")
dim(imp_data) # 18003337, 25
imp_data = subset(imp_data, SiteCode %in% meta_sites_use) # some sites have no gps/location information
nrow(imp_data) # 17730878 
imp_data = subset(imp_data, Unit == "ug/m^3")
nrow(imp_data) # 12659632 
imp_data_1 = imp_data

imp_data_no_V0 = subset(imp_data, Status != "V0")
# imp_no_V0_providerFlag = unique(imp_data_no_V0[, c("Status", "ProviderFlag")])
imp_no_V0_providerFlag = ddply(imp_data_no_V0, .(Status, ProviderFlag), summarise,
                               count = length(Status))
imp_providerFlag = ddply(imp_data, .(Status, ProviderFlag), summarise,
                               count = length(Status))
imp_data_V4_V5 = subset(imp_data, 
                        Status == "V4" | Status == "V5")
imp_data_V4_V5_exp = subset(imp_data_V4_V5, 
                            SiteCode == "BALD1" | SiteCode == "DOME1" )
write.csv(imp_data_V4_V5_exp, "IMPROVE_StatusFlag_V4_V5_example.csv")
write.csv(imp_data_V4_V5, "IMPROVE_StatusFlag_V4_V5.csv")

imp_data$year = year(imp_data$Date)
imp_data$month = month(imp_data$Date)

if(length(unique(imp_data$Dataset)) <= 10){unique(imp_data$Dataset)}else{head(unique(imp_data$Dataset))} # only IMPRHR3
if(length(unique(imp_data$SiteCode)) <= 10){unique(imp_data$SiteCode)}else{head(unique(imp_data$SiteCode))}; length(unique(imp_data$SiteCode)) # 196
if(length(unique(imp_data$POC)) <= 10){unique(imp_data$POC)}else{head(unique(imp_data$POC))} # 1 2
if(length(unique(imp_data$Date)) <= 10){unique(imp_data$Date)}else{head(unique(imp_data$Date))}; length(unique(imp_data$Date)) # 1663
if(length(unique(imp_data$ParamCode)) <= 10){unique(imp_data$ParamCode)}else{head(unique(imp_data$ParamCode))}; length(unique(imp_data$ParamCode)) # 64
if(length(unique(imp_data$Method)) <= 10){unique(imp_data$Method)}else{head(unique(imp_data$Method))}; length(unique(imp_data$Method)) # 20
# for the IMPROVE Preliminary dataset, only two methods, one for IMPROVE, one for the volcano adjusted in Hawaii
if(length(unique(imp_data$Unit)) <= 10){unique(imp_data$Unit)}else{head(unique(imp_data$Unit))}; # "ug/m^3"  
if(length(unique(imp_data$AuxID)) <= 10){unique(imp_data$AuxID)}else{head(unique(imp_data$AuxID))} #  0
if(length(unique(imp_data$Status)) <= 10){unique(imp_data$Status)}else{head(unique(imp_data$Status))} # "V0" "M1" "M2" "V6" "M3" "V5" "V4" "V2"
flag.status = data.frame(table(imp_data$Status)); colnames(flag.status)[1] = "Status.Flag"
flag.status = join(flag.status, imp_meta_flags)
if(length(unique(imp_data$ProviderFlag)) <= 10){unique(imp_data$ProviderFlag)}else{head(unique(imp_data$ProviderFlag))}; length(unique(imp_data$ProviderFlag)) # 22
if(length(unique(imp_data$ObjectiveCode)) <= 10){unique(imp_data$ObjectiveCode)}else{head(unique(imp_data$ObjectiveCode))}; # "---" "RT"  "CL"  "RS" 
if(length(unique(imp_data$ModuleTypeCode)) <= 10){unique(imp_data$ModuleTypeCode)}else{head(unique(imp_data$ModuleTypeCode))}; # "---" "A"   "B"   "C"   "D"  
if(length(unique(imp_data$Val3)) <= 10){unique(imp_data$Val3)}else{head(unique(imp_data$Val3))} # -999
if(length(unique(imp_data$SiteName)) <= 10){unique(imp_data$SiteName)}else{head(unique(imp_data$SiteName))}; length(unique(imp_data$SiteName)) # 193
if(length(unique(imp_data$Latitude)) <= 10){unique(imp_data$Latitude)}else{head(unique(imp_data$Latitude))}; length(unique(imp_data$Latitude)) # 191
if(length(unique(imp_data$Longitude)) <= 10){unique(imp_data$Longitude)}else{head(unique(imp_data$Longitude))}; length(unique(imp_data$Longitude)) # 191
if(length(unique(imp_data$Elevation)) <= 10){unique(imp_data$Elevation)}else{head(unique(imp_data$Elevation))}; length(unique(imp_data$Elevation)) # 185
imp_data_gps = select(imp_data, Longitude, Latitude, Elevation)
imp_data_gps_nodup = imp_data_gps[!duplicated(imp_data_gps), ]  
nrow(imp_data_gps_nodup) # 191
# nrow(imp_data_gps_nodup) != length(unique(imp_data$SiteCode)), need to check the sites with duplicated GPS
if(length(unique(imp_data$State)) <= 10){unique(imp_data$State)}else{head(unique(imp_data$State))}; length(unique(imp_data$State)) # 49
if(length(unique(imp_data$CountyFIPS)) <= 10){unique(imp_data$CountyFIPS)}else{head(unique(imp_data$CountyFIPS))}; length(unique(imp_data$CountyFIPS)) # 166
if(length(unique(imp_data$EPACode)) <= 10){unique(imp_data$EPACode)}else{head(unique(imp_data$EPACode))}; length(unique(imp_data$EPACode)) # 172


#### method used for each component ####
method_comp = select(imp_data, Method, ParamCode)
method_comp = ddply(method_comp, .(Method, ParamCode), count)
method_comp$MethodSimple = "CIRA"
method_comp$MethodSimple[11] = "A-GRAV"
method_comp$MethodSimple[12:35] = "A-XRF"
method_comp$MethodSimple[36:39] = "B-IC"
method_comp$MethodSimple[40:47] = "C-TOR"
method_comp$MethodSimple[48] = "D-GRAV"
method_comp$MethodSimple[49:54] = "TOR_UCD"
method_comp$MethodSimple[55:61] = "TOT_UCD"
method_comp$MethodSimple[62:64] = "UCD_Cal"
imp_data = join(imp_data, method_comp)
imp_data$Method = imp_data$method.comp = imp_data$freq = imp_data$AuxID = NULL

para_comp_name = select(imp_meta_para, ParamCode, CompName)
imp_data = join(imp_data, para_comp_name)
write.csv(imp_data, "IMPROVE component only 10092022.csv")

# less_used_method = unique(subset(method_comp, freq < 200000)$MethodSimple)
# imp_less_used_method = subset(imp_data, MethodSimple %in% less_used_method)
method_comp$start.date = method_comp$end.date = "1900-01-01"
method_comp$start.date = as.Date(method_comp$start.date)
method_comp$end.date = as.Date(method_comp$end.date)

method_used = select(method_comp, MethodSimple, start.date, end.date)
method_used = method_used[!duplicated(method_used), ]
method_used$state.exclude = 0
method_used$site.exclude = 0
method_used$influenced.state = 0
method_used$other.site.in.state = 0

total.state = unique(imp_data$State)
total.site = unique(imp_data$SiteCode)
state.site = select(imp_data, State, SiteCode)
state.site = state.site[!duplicated(state.site), ]

library(rlang)
for (i in 1:nrow(method_used)){
  method.check = method_used$MethodSimple[i] 
  imp_given_method = subset(imp_data, imp_data$MethodSimple == method.check)
  
  method_used$start.date[i] = min(imp_given_method$Date)
  method_used$end.date[i] = max(imp_given_method$Date)
  
  state.in.method = unique(imp_given_method$State)
  site.in.method = unique(imp_given_method$SiteCode)
  
  state.detect = list(total.state[which(!(total.state %in% state.in.method))])
  site.detect = list(total.site[which(!(total.site %in% site.in.method))])
  influenced.state.detect = list(state.site$State[state.site$SiteCode %in% site.detect[[1]]])
  influenced.state.detect = list(unlist(influenced.state.detect)[!duplicated(unlist(influenced.state.detect))])
  # we can only fill the data.frame cell with a "list" element
  if(!is_empty(influenced.state.detect[[1]])){
    potential.influenced.site = state.site$SiteCode[state.site$State %in% influenced.state.detect[[1]]]
    other.site.in.state = list(potential.influenced.site[which(!(potential.influenced.site %in% method_used$site.exclude[i][[1]]))])
  }
  
  method_used$state.exclude[i] = ifelse(is_empty(state.detect[[1]]), 0, state.detect)
  method_used$site.exclude[i] = ifelse(is_empty(site.detect[[1]]), 0, site.detect)
  method_used$influenced.state[i] = ifelse(is_empty(influenced.state.detect[[1]]), 0, influenced.state.detect)
  method_used$other.site.in.state[i] = ifelse(is_empty(other.site.in.state[[1]]), 0, other.site.in.state)
  
  method_used$number.state.exclude[i] = length(unlist(method_used$state.exclude[i]))
  method_used$number.site.exclude[i] = length(unlist(method_used$site.exclude[i]))
  method_used$number.influenced.state[i] = length(unlist(method_used$influenced.state[i]))
  method_used$number.other.site.in.state[i] = length(unlist(method_used$other.site.in.state[i]))
}
method_used.1 = method_used
####??????? how to write a data.frame with list and partially columns into csv file???????####
write.csv(method_used.1, "Application status of method across sites and states IMPROVE.csv")

method_used = read.csv("Application status of method across sites and states IMPROVE.csv")
method_used$X  = NULL
imp_meta_para = join(imp_meta_para, method_used)
write.csv(imp_meta_para,"IMPROVE metadata 109 parameters & methods 2010-20.csv")


##########################################################################################
#### detect the distribution of -999 in Val ####
imp_missing_val = imp_data[(imp_data$Val == -999), ]
nrow(imp_missing_val)/nrow(imp_data)
if(length(unique(imp_missing_val$State)) <= 10){unique(imp_missing_val$State)}else{head(unique(imp_missing_val$State))}; length(unique(imp_missing_val$State)) # 49, same as all data
if(length(unique(imp_missing_val$SiteCode)) <= 10){unique(imp_missing_val$SiteCode)}else{head(unique(imp_missing_val$SiteCode))}; length(unique(imp_missing_val$SiteCode)) # 194, <196
if(length(unique(imp_missing_val$Date)) <= 10){unique(imp_missing_val$Date)}else{head(unique(imp_missing_val$Date))}; length(unique(imp_missing_val$Date)) # 1663, same as all data
length(unique(imp_missing_val$year)); length(unique(imp_missing_val$month))
if(length(unique(imp_missing_val$ParamCode)) <= 10){unique(imp_missing_val$ParamCode)}else{head(unique(imp_missing_val$ParamCode))}; length(unique(imp_missing_val$ParamCode)) # 64, same as all data
if(length(unique(imp_missing_val$Status)) <= 10){unique(imp_missing_val$Status)}else{head(unique(imp_missing_val$Status))} # "M1" "M2" "V0" "M3" "V4" "V6", all("V0" "M1" "M2" "V6" "M3" "V5" "V4" "V2")
data.frame(table(imp_missing_val$Status))
if(length(unique(imp_missing_val$ProviderFlag)) <= 10){unique(imp_missing_val$ProviderFlag)}else{head(unique(imp_missing_val$ProviderFlag))} # "EP" NA "XX" "PO" "OL" "DA"
# ALL imp_data,  "NM" "EP" NA   "XX" "SW" "LF" "PO" "CG" "OL" "DA" "UN" "NS" "TO" "BI" "CL" "TU" "PC" "SP" "RF" "SA" "QD" "DE"
provider.status = select(imp_missing_val, Status, ProviderFlag)
provider.status = ddply(provider.status, .(Status, ProviderFlag), count)
# write.csv(provider.status, "joint Status ProviderFlag for -999 Val in IMPROVE.csv")
if(length(unique(imp_missing_val$ObjectiveCode)) <= 10){unique(imp_missing_val$ObjectiveCode)}else{head(unique(imp_missing_val$ObjectiveCode))}; # "---" "RT"  "CL"  "RS", same as all data
if(length(unique(imp_missing_val$ModuleTypeCode)) <= 10){unique(imp_missing_val$ModuleTypeCode)}else{head(unique(imp_missing_val$ModuleTypeCode))}; # "---" "A"   "B"   "C"   "D", same as all data

imp_missing_val_only_measure = subset(imp_missing_val, !(MethodSimple %in% c("CIRA", "A-GRAV", "D-GRAV", "UCD_Cal")))
year_missing = data.frame(table(imp_missing_val_only_measure$year))
month_missing = data.frame(table(imp_missing_val_only_measure$month))
year_month_missing = select(imp_missing_val_only_measure, year, month)
year_month_missing = ddply(year_month_missing, .( year, month), count)
ggplot(year_month_missing, 
       aes(year, month, fill= freq)) + 
  scale_x_continuous(breaks=seq(2010, 2020, 1)) +
  scale_y_continuous(breaks=seq(1, 12, 1)) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red") +
  theme.2
component_missing = data.frame(table(imp_missing_val_only_measure$CompName))
state_missing = data.frame(table(imp_missing_val_only_measure$State))
colnames(state_missing)[1] = "State"
state_missing = join(state_missing, state_nrow)
state_missing$percent.in.state = state_missing$Freq/state_missing$nrow.data * 100
state_missing$group = rep(1:7, each = 7)
ggplot(state_missing, 
       aes(State, percent.in.state, color= percent.in.state)) + 
  geom_point(size = 1.5, alpha = 0.8) + 
  ylim(0, 30) +
  theme.2

#### detect the distribution of Val marked with other flags ####

imp_no_missing_val = imp_data[(imp_data$Val != -999), ]
nrow(imp_no_missing_val)
if(length(unique(imp_no_missing_val$Status)) <= 10){unique(imp_no_missing_val$Status)}else{head(unique(imp_no_missing_val$Status))} 
# "V0" "V6" "V5" "M2" "V4" "V2", missing("M1" "M2" "V0" "M3" "V4" "V6"), all("V0" "M1" "M2" "V6" "M3" "V5" "V4" "V2")
imp_no_missing_flag = imp_no_missing_val[(imp_no_missing_val$Status != "V0"), ] # V0 not V0!!!
data.frame(table(imp_no_missing_flag$Status))
# ddply(imp_no_missing_flag, .(State, MethodSimple, CompName, Status), count)

imp_no_missing_val_mean = ddply(imp_no_missing_val, .(Status, MethodSimple, CompName), summarise, 
                                Val.mean = mean(Val), 
                                Val.median = median(Val),
                                Val.max = max(Val))
# CompName = PM2.5, PM10, MethodSimple = CIRA, UCD_Cal, 
PM = c("PM2.5", "PM10")
Cal.RC.method = c("CIRA", "UCD_Cal")
imp_no_missing_val_plot = subset(imp_no_missing_val, 
                                 !(imp_no_missing_val$CompName %in% PM) & 
                                   !(imp_no_missing_val$MethodSimple %in% Cal.RC.method))
unique(imp_no_missing_val_plot$MethodSimple) # "A-XRF"   "B-IC"    "C-TOR"   "TOT_UCD" "TOR_UCD"

imp_no_missing_val_plot_mean = ddply(imp_no_missing_val_plot, .(MethodSimple, CompName, Status), summarise, 
                                     Val.mean = mean(Val), 
                                     Val.median = median(Val))
imp_no_missing_V0 = subset(imp_no_missing_val_plot_mean, Status == "V0")
imp_no_missing_MVflag = subset(imp_no_missing_val_plot_mean, Status != "V0")
imp_no_missing_MVflag$mean.vs.V0 = imp_no_missing_MVflag$median.vs.V0 = 0

for(j in 1:nrow(imp_no_missing_MVflag)){
  comp.use = imp_no_missing_MVflag$CompName[j]
  imp_no_missing_MVflag$mean.vs.V0[j] = imp_no_missing_MVflag$Val.mean[j]/imp_no_missing_V0$Val.mean[imp_no_missing_V0$CompName == comp.use]
  imp_no_missing_MVflag$median.vs.V0[j] = imp_no_missing_MVflag$Val.median[j]/imp_no_missing_V0$Val.median[imp_no_missing_V0$CompName == comp.use]
}


theme.1 = theme(plot.title = element_text(hjust = 0.05, vjust = -25, size = 16),
                axis.title.x = element_text(color="grey25", size = 14, vjust=0, margin=margin(0,0,0,300)), 
                axis.title.y = element_text(color="grey25", size = 14, vjust=1, margin=margin(0,2,0,0)),
                axis.text.x = element_text(color="grey25", size = 11, angle = 90, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
                axis.text.y = element_text(color="grey25", size = 11, angle = 0, hjust = 0.5))

theme.2 = theme(plot.title = element_text(hjust = 0.05, vjust = -25, size = 16),
                axis.title.x = element_text(color="grey25", size = 14, vjust=0, margin=margin(0,0,0,300)), 
                axis.title.y = element_text(color="grey25", size = 14, vjust=1, margin=margin(0,2,0,0)),
                axis.text.x = element_text(color="grey25", size = 12, angle = 90, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
                axis.text.y = element_text(color="grey25", size = 12, angle = 0, hjust = 0.5))

# distribution of mean and median ratio of flagged vs. non-flagged data
## median
ggplot(subset(imp_no_missing_MVflag, MethodSimple == "A-XRF"), 
       aes(x=CompName, y=median.vs.V0, color = Status)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(Status ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  ylim(0, 4) +
  theme.1

ggplot(subset(imp_no_missing_MVflag, MethodSimple == "B-IC"), 
       aes(x=CompName, y=median.vs.V0, color = Status)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(Status ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  ylim(0, 4) +
  theme.1

ggplot(subset(imp_no_missing_MVflag, MethodSimple != "A-XRF" & MethodSimple != "B-IC"), 
       aes(x=CompName, y=median.vs.V0, color = Status)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(Status ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  ylim(0, 4) +
  theme.1

## mean
ggplot(subset(imp_no_missing_MVflag, MethodSimple == "A-XRF"), 
       aes(x=CompName, y=mean.vs.V0, color = Status)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(Status ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  ylim(0, 4) +
  theme.1

ggplot(subset(imp_no_missing_MVflag, MethodSimple == "B-IC"), 
       aes(x=CompName, y=mean.vs.V0, color = Status)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(Status ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  ylim(0, 4) +
  theme.1

ggplot(subset(imp_no_missing_MVflag, MethodSimple != "A-XRF" & MethodSimple != "B-IC"), 
       aes(x=CompName, y=mean.vs.V0, color = Status)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(Status ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  ylim(0, 4) +
  theme.1

# distribution of concentrations of flagged & non-flagged data
ggplot(subset(imp_no_missing_val_plot, MethodSimple == "A-XRF"), 
       aes(x=CompName, y=Val, fill = Status)) +
  geom_boxplot() + 
  facet_grid(Status ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  ylim(0, 1) +
  theme.1

ggplot(subset(imp_no_missing_val_plot, MethodSimple == "B-IC"), 
       aes(x=CompName, y=Val, fill = Status)) +
  geom_boxplot() + 
  facet_grid(Status ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  ylim(0, 1) +
  theme.1

ggplot(subset(imp_no_missing_val_plot, MethodSimple != "A-XRF" & MethodSimple != "B-IC"), 
       aes(x=CompName, y=Val, fill = Status)) +
  geom_boxplot() + 
  facet_grid(Status ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  ylim(0, 1) +
  theme.1

#### detect the distribution of Val of V0 but with ProviderFlag ####
imp_V0_val = imp_data[(imp_data$Status == "V0"), ]
data.frame(table(imp_V0_val$ProviderFlag))

imp_V0_val_site_provdier = select(imp_V0_val, State, ProviderFlag)
imp_V0_val_site_provdier = ddply(imp_V0_val_site_provdier, .( State, ProviderFlag), count)
imp_V0_val_site_provdier = join(imp_V0_val_site_provdier, state_count)
imp_V0_val_site_provdier$flag.freq.per.site = imp_V0_val_site_provdier$freq/imp_V0_val_site_provdier$site.in.state

imp_V0_val_year_provdier = select(imp_V0_val, year, ProviderFlag)
imp_V0_val_year_provdier = ddply(imp_V0_val_year_provdier, .( year, ProviderFlag), count)

imp_V0_val_month_provdier = select(imp_V0_val, month, ProviderFlag)
imp_V0_val_month_provdier = ddply(imp_V0_val_month_provdier, .( month, ProviderFlag), count)

imp_V0_val.1 = imp_V0_val
imp_V0_val.1$Val[imp_V0_val.1$Val == -999] = NA
imp_V0_val_mean = ddply(imp_V0_val.1, .(ProviderFlag, MethodSimple, CompName), summarize,
                        Val.mean = mean(Val, na.rm = T), Val.median = median(Val, na.rm = T))

imp_V0_val_NM = subset(imp_V0_val_mean, ProviderFlag == "NM")
imp_V0_val_NM$ProviderFlag = NULL
colnames(imp_V0_val_NM)[3:4] = c("Val.mean.NM", "Val.median.NM")
imp_V0_val_Proflag = subset(imp_V0_val_mean, ProviderFlag != "NM")
imp_V0_val_Proflag = join(imp_V0_val_Proflag, imp_V0_val_NM)
imp_V0_val_Proflag$mean.vs.V0 = imp_V0_val_Proflag$Val.mean/imp_V0_val_Proflag$Val.mean.NM
imp_V0_val_Proflag$median.vs.V0 = imp_V0_val_Proflag$Val.median/imp_V0_val_Proflag$Val.median.NM


# distribution of concentrations of different ProviderFlags
ProviderFlag.10K = "NM"
ProviderFlag.K = c("LF", "SW") 
ProviderFlag.h = c("SP", "TO") 
ProviderFlag.T = c("CG", "QD", "SA", "TU") 

state.10K = ggplot(subset(imp_V0_val_site_provdier, imp_V0_val_site_provdier$ProviderFlag %in% ProviderFlag.10K), aes(State, ProviderFlag, fill= flag.freq.per.site)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "red")
state.k = ggplot(subset(imp_V0_val_site_provdier, imp_V0_val_site_provdier$ProviderFlag %in% ProviderFlag.K), aes(State, ProviderFlag, fill= flag.freq.per.site)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "purple")
state.h = ggplot(subset(imp_V0_val_site_provdier, imp_V0_val_site_provdier$ProviderFlag %in% ProviderFlag.h), aes(State, ProviderFlag, fill= flag.freq.per.site)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "deepskyblue3")
state.T = ggplot(subset(imp_V0_val_site_provdier, imp_V0_val_site_provdier$ProviderFlag %in% ProviderFlag.T), aes(State, ProviderFlag, fill= flag.freq.per.site)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "green")
# multiplot(state.most, state.more.10k, state.1k.10k, state.less.1k, cols=2)

year.10K = ggplot(subset(imp_V0_val_year_provdier, imp_V0_val_year_provdier$ProviderFlag %in% ProviderFlag.10K), aes(factor(year), ProviderFlag, fill= freq)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "red") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
year.k = ggplot(subset(imp_V0_val_year_provdier, imp_V0_val_year_provdier$ProviderFlag %in% ProviderFlag.K), aes(factor(year), ProviderFlag, fill= freq)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "purple") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
year.h = ggplot(subset(imp_V0_val_year_provdier, imp_V0_val_year_provdier$ProviderFlag %in% ProviderFlag.h), aes(factor(year), ProviderFlag, fill= freq)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "deepskyblue3") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
year.T = ggplot(subset(imp_V0_val_year_provdier, imp_V0_val_year_provdier$ProviderFlag %in% ProviderFlag.T), aes(factor(year), ProviderFlag, fill= freq)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "green") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

month.10K = ggplot(subset(imp_V0_val_month_provdier, imp_V0_val_month_provdier$ProviderFlag %in% ProviderFlag.10K), aes(factor(month), ProviderFlag, fill= freq)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "red") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
month.k = ggplot(subset(imp_V0_val_month_provdier, imp_V0_val_month_provdier$ProviderFlag %in% ProviderFlag.K), aes(factor(month), ProviderFlag, fill= freq)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "purple") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
month.h = ggplot(subset(imp_V0_val_month_provdier, imp_V0_val_month_provdier$ProviderFlag %in% ProviderFlag.h), aes(factor(month), ProviderFlag, fill= freq)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "deepskyblue3") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
month.T = ggplot(subset(imp_V0_val_month_provdier, imp_V0_val_month_provdier$ProviderFlag %in% ProviderFlag.T), aes(factor(month), ProviderFlag, fill= freq)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "green") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# distribution of concentrations of different ProviderFlags
ggplot(subset(imp_V0_val, MethodSimple == "A-XRF"), 
       aes(x=CompName, y=Val, fill = ProviderFlag)) +
  geom_boxplot() + 
  facet_grid(ProviderFlag ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  ylim(0, 1) +
  theme.1

ggplot(subset(imp_V0_val, MethodSimple == "B-IC"), 
       aes(x=CompName, y=Val, fill = ProviderFlag)) +
  geom_boxplot() + 
  facet_grid(ProviderFlag ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  ylim(0, 1) +
  theme.1

ggplot(subset(imp_V0_val, MethodSimple != "A-XRF" & MethodSimple != "B-IC"), 
       aes(x=CompName, y=Val, fill = ProviderFlag)) +
  geom_boxplot() + 
  facet_grid(ProviderFlag ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  # ylim(0, 1) +
  scale_y_continuous(breaks=c(0.5, 1), limits = c(0, 1)) +
  theme.2

## distribution of ratios of Providerflagged vs. NM data

## median
ggplot(subset(imp_V0_val_Proflag, MethodSimple == "A-XRF"), 
       aes(x=CompName, y=median.vs.V0, color = ProviderFlag)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(ProviderFlag ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  ylim(0, 4) +
  theme.2

ggplot(subset(imp_V0_val_Proflag, MethodSimple == "B-IC"), 
       aes(x=CompName, y=median.vs.V0, color = ProviderFlag)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(ProviderFlag ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  ylim(0, 4) +
  theme.2

proflag.for.oc.ec = c("CG", "LF", "QD", "SA", "TO", "TU")
ggplot(subset(imp_V0_val_Proflag, MethodSimple != "A-XRF" & MethodSimple != "B-IC"& !(ProviderFlag %in% proflag.for.oc.ec)), 
       aes(x=CompName, y=median.vs.V0, color = ProviderFlag)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(ProviderFlag ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  scale_y_continuous(breaks=c(2,4), limits = c(0, 4)) +
  theme.2

ggplot(subset(imp_V0_val_Proflag, MethodSimple != "A-XRF" & MethodSimple != "B-IC" & ProviderFlag %in% proflag.for.oc.ec), 
       aes(x=CompName, y=median.vs.V0, color = ProviderFlag)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(ProviderFlag ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  # scale_y_continuous(breaks=c(2,4), limits = c(0, 4)) +
  theme.2

## mean
ggplot(subset(imp_V0_val_Proflag, MethodSimple == "A-XRF"), 
       aes(x=CompName, y=mean.vs.V0, color = ProviderFlag)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(ProviderFlag ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  ylim(0, 4) +
  theme.2

ggplot(subset(imp_V0_val_Proflag, MethodSimple == "B-IC"), 
       aes(x=CompName, y=mean.vs.V0, color = ProviderFlag)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(ProviderFlag ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  ylim(0, 4) +
  theme.2

ggplot(subset(imp_V0_val_Proflag, MethodSimple != "A-XRF" & MethodSimple != "B-IC"& !(ProviderFlag %in% proflag.for.oc.ec)), 
       aes(x=CompName, y=mean.vs.V0, color = ProviderFlag)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(ProviderFlag ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  scale_y_continuous(breaks=c(2,4), limits = c(0, 4)) +
  theme.2

ggplot(subset(imp_V0_val_Proflag, MethodSimple != "A-XRF" & MethodSimple != "B-IC" & ProviderFlag %in% proflag.for.oc.ec), 
       aes(x=CompName, y=mean.vs.V0, color = ProviderFlag)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  facet_grid(ProviderFlag ~ .) +  # facet_grid(.~Freq.Flag.Counts) +
  geom_hline(yintercept=1, linetype='dotted') + 
  # scale_y_continuous(breaks=c(2,4), limits = c(0, 4)) +
  theme.2

#### check the 3 sites (RENOs in NV) with no GPS ####
imp_no_gps = subset(imp_data, SiteCode %in% c("RENO1", "RENO2", "RENO3"))
nrow(imp_no_gps)
imp_no_gps_real_Val = subset(imp_no_gps, Val != -999)


ggplot(subset(imp_no_gps_real_Val, !(CompName %in% c("PM2.5", "ammSO4",  "ammNO3",  "SOIL",  "SeaSalt", "PM10"))), 
       aes(x = CompName, y = Val, fill = SiteCode)) +
  geom_boxplot() + 
  facet_grid(SiteCode ~ .) + 
  ylim(0, 5) + 
  # ggtitle("RENOs - component concentrations") +
  theme.2


### find unique combos of SiteCode, SiteName, EPACode, Latitude, and Longitude used during our study period ####
imp_location = select(imp_data, State, SiteCode, SiteName, EPACode, CountyFIPS, Latitude, Longitude)
#also, imp_location = imp_data[,c("SiteCode", "SiteName", "EPACode", "Latitude", "Longitude")]
imp_location_nodup = imp_location[!duplicated(imp_location), ]
nrow(imp_location_nodup) # 196
imp_location_nodup = imp_location_nodup[with(imp_location_nodup, order(State, CountyFIPS, EPACode, SiteName, SiteCode)), ]
imp_location_nodup$gps = paste(imp_location_nodup$Longitude, imp_location_nodup$Latitude)
imp_location_nodup = imp_location_nodup[with(imp_location_nodup, order(CountyFIPS, SiteCode, gps)), ]
imp_location_nodup$gps_dup = duplicated(imp_location_nodup$gps)
imp_location_nodup = join(imp_location_nodup, imp_meta_sites_use)


#### determine special sites and detect their component distribution ####
## 1. those with duplicated GPS
site_dup_gps.1 = imp_location_nodup$SiteCode[imp_location_nodup$gps_dup]
site_dup_gps.2 = imp_location_nodup$SiteCode[which(imp_location_nodup$gps_dup)-1]
site_dup_gps = c(site_dup_gps.1, site_dup_gps.2)
un_dup_site = unlist(site_dup_gps)
site_dup_gps_qestion = un_dup_site[!duplicated(un_dup_site)] # remove duplicated states in the list
site_dup_gps_qestion # nothing like RHTS or volcano adjusted here 
imp_same_location_nodup = imp_location_nodup[(imp_location_nodup$SiteCode %in% site_dup_gps_qestion), ]
imp_same_location_nodup$flag = "same GPS"


## 2. to detect if there is a close site to those with SiteCode ended with "2"
site_doubt_gps.1 = imp_location_nodup$SiteCode[grepl(2, imp_location_nodup$SiteCode, fixed = TRUE)]
site_doubt_gps.1 = site_doubt_gps.1[!(site_doubt_gps.1 %in% site_dup_gps_qestion)]
site_doubt_gps.2 = site_doubt_gps.1
regmatches(site_doubt_gps.2, gregexpr("2",site_doubt_gps.2)) <- 1 # replace 2 by 1 in selected SiteCode, Matching and Replacement
site_doubt_gps.3 = site_doubt_gps.2[site_doubt_gps.2 %in% imp_location_nodup$SiteCode]
site_doubt_gps.4 = site_doubt_gps.3
regmatches(site_doubt_gps.4, gregexpr("1",site_doubt_gps.4)) <- 2
site_doubt_gps = c(site_doubt_gps.3, site_doubt_gps.4)
imp_location_nodup_doubt = imp_location_nodup[(imp_location_nodup$SiteCode %in% site_doubt_gps), ]
imp_location_nodup_doubt$flag = "similar SiteCode ending with 2"
imp_question_location = rbind(imp_same_location_nodup, imp_location_nodup_doubt)
# write.csv(imp_question_location, "IMPROVE SiteCode in Question.csv")

## 3. component distribution in marked sites
imp_same_location = imp_data[(imp_data$SiteCode %in% unique(imp_same_location_nodup$SiteCode)[4:9]), ]

imp_question_location = imp_data[(imp_data$SiteCode %in% site_doubt_gps), ]

ggplot(imp_same_location, aes(x = ParamCode, y = Val, fill = SiteCode)) +
  geom_boxplot() + ylim(0, 20) + ggtitle("unit - ??g/m^3 or ng/m^3 for flags")





# imp_data$Date[max(which(imp_data$comp.collect.analysis == comp.collect.analysis$comp.collect.analysis[i]))]










