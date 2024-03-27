##clear environment
rm(list=ls())

##set working directory
setwd("/Users/ztttttt/Documents/HEI PMF/R - original CSN")
getwd()
data.dir <- "Users/ztttttt/Documents/HEI PMF/R - original CSN"

##packages in need
library(tidyr) 
library(ggplot2)
library(scales) 
library(dplyr)
library(plyr)
library(lubridate)
library(gridExtra)
library(grid) #textGrob{}


#############################################################################################################
####### 1. Earlier exploration of the data ######
#############################################################################################################

#### read and save original datasets ####
csn_meta_sites = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/CSN sites.txt",
                          sep = ",", dec = ".")
csn_meta_flags = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/csn flags.txt",
                          sep = ",", dec = ".")
csn_meta_para = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/CSN parameter.txt",
                         sep = ",", dec = ".")

write.csv(csn_meta_sites, "CSN metadata 157 sample sites info 2010-20.csv")
write.csv(csn_meta_flags, "CSN metadata 18 flags 2010-20.csv")
write.csv(csn_meta_para, "CSN metadata 118 parameters 2010-20.csv")

csn_data = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/ailsa2be_20220816_221221_OJ1LO csn.txt", sep = ",", dec = ".")
csn_data$Date = as.Date(csn_data$Date, format = "%m/%d/%Y")
csn_data$year = year(csn_data$Date)
csn_data$month = month(csn_data$Date)
csn_data$day = day(csn_data$Date)
# csn_data$dow = weekdays(csn_data$Date); csn_data$dow = NULL
View(csn_data)

#### detect and remove variables barely changed in value ####
# ChatGPT: I have a dataset with multiple columns, for those that are not numeric, I want to get 1, the number of unique info in this column, and 2) list the first 10 info in the column. The info can be get via unique(dataset$XX) or so. I want to create a three column dataset based on the result, variable, number of unique info, and the first 10 unique info.

if(length(unique(csn_data$Dataset)) <= 10){unique(csn_data$Dataset)}else{head(unique(csn_data$Dataset))} # only EPACSN
if(length(unique(csn_data$SiteCode)) <= 10){unique(csn_data$SiteCode)}else{head(unique(csn_data$SiteCode))}; length(unique(csn_data$SiteCode)) # 156
if(length(unique(csn_data$POC)) <= 10){unique(csn_data$POC)}else{head(unique(csn_data$POC))} # 5, 7, 6
if(length(unique(csn_data$Date)) <= 10){unique(csn_data$Date)}else{head(unique(csn_data$Date))}; length(unique(csn_data$Date)) # 1993
if(length(unique(csn_data$ParamCode)) <= 10){unique(csn_data$ParamCode)}else{head(unique(csn_data$ParamCode))}; length(unique(csn_data$ParamCode)) #95
if(length(unique(csn_data$Method)) <= 10){unique(csn_data$Method)}else{head(unique(csn_data$Method))}; length(unique(csn_data$Method)) # 127
if(length(unique(csn_data$Unit)) <= 10){unique(csn_data$Unit)}else{head(unique(csn_data$Unit))}; # "ug/m^3"  "Deg C"   "mm (HG)" "%"       "m^3"     "ppb C"  
if(length(unique(csn_data$AuxID)) <= 10){unique(csn_data$AuxID)}else{head(unique(csn_data$AuxID))} # 0, 7
if(length(unique(csn_data$Status)) <= 10){unique(csn_data$Status)}else{head(unique(csn_data$Status))} # only V0
if(length(unique(csn_data$NullDataCode)) <= 10){unique(csn_data$NullDataCode)}else{head(unique(csn_data$NullDataCode))} # only ---
if(length(unique(csn_data$Qualifier1)) <= 10){unique(csn_data$Qualifier1)}else{head(unique(csn_data$Qualifier1))}; length(unique(csn_data$Qualifier1)) # 41
if(length(unique(csn_data$Qualifier2)) <= 10){unique(csn_data$Qualifier2)}else{head(unique(csn_data$Qualifier2))}; length(unique(csn_data$Qualifier2)) # 36
if(length(unique(csn_data$Qualifier3)) <= 10){unique(csn_data$Qualifier3)}else{head(unique(csn_data$Qualifier3))}; length(unique(csn_data$Qualifier3)) # 27
if(length(unique(csn_data$SampleDuration)) <= 10){unique(csn_data$SampleDuration)}else{head(unique(csn_data$SampleDuration))}; length(unique(csn_data$SampleDuration)) #  7 811 838 812 819 818 810 816 149 901 902
if(length(unique(csn_data$SiteName)) <= 10){unique(csn_data$SiteName)}else{head(unique(csn_data$SiteName))}; length(unique(csn_data$SiteName)) # 156
if(length(unique(csn_data$Latitude)) <= 10){unique(csn_data$Latitude)}else{head(unique(csn_data$Latitude))}; length(unique(csn_data$Latitude))
if(length(unique(csn_data$Longitude)) <= 10){unique(csn_data$Longitude)}else{head(unique(csn_data$Longitude))}; length(unique(csn_data$Longitude))
if(length(unique(csn_data$Elevation)) <= 10){unique(csn_data$Elevation)}else{head(unique(csn_data$Elevation))}; length(unique(csn_data$Elevation))
if(length(unique(csn_data$State)) <= 10){unique(csn_data$State)}else{head(unique(csn_data$State))}; length(unique(csn_data$State)) # 51
if(length(unique(csn_data$CountyFIPS)) <= 10){unique(csn_data$CountyFIPS)}else{head(unique(csn_data$CountyFIPS))}; length(unique(csn_data$CountyFIPS)) # 131
if(length(unique(csn_data$EPACode)) <= 10){unique(csn_data$EPACode)}else{head(unique(csn_data$EPACode))}; length(unique(csn_data$EPACode)) # 156

csn_data$AuxID = csn_data$Status = csn_data$NullDataCode = NULL


# write.csv(csn_data, "CSN data with date 09252022.csv")

#### detect and remove variables offering duplicated info ####
# detect variables that are location-specific so as to reduce the original dataset 
csn_sites = select(csn_data, State, SiteCode, 
                   Longitude, Latitude, 
                   EPACode, CountyFIPS)
csn_sites = csn_sites[!duplicated(csn_sites), ] 
write.csv(csn_sites, "CSN 156 sample sites info 2010-20.csv")

#### detect the distribution trend of MDL for a given component with a specific region or time ####
# From previous observation of the whole dataset, we found MDL varies between sampling sites & across study period, 
# now we try to detect if there is an trend
## randomly select a component from one monitoring site

# ChatGPT, instead, plot the MDL, Val, and Unc for each component in each site 
# There should be such code, if so, remove/combine below code

csn_Br = subset(csn_data, ParamCode == "BRf" & 
                  Method == "Intermittent|Hi-Vol-Wedding Inlet|X-Ray Fluorescence||||0.09|0.0||1|R|Nanograms/cubic meter (25 C)")
csn_Br$dup = duplicated(csn_Br); summary(csn_Br$dup)

plot(csn_Br$day, csn_Br$MDL) 
plot(csn_Br$month, csn_Br$MDL) 
plot(csn_Br$year, csn_Br$MDL) 

theme_mdl = theme(axis.title.y.right = element_blank(),
                  panel.spacing = unit(10, "mm"),   
                  strip.background = element_rect(fill="white", colour="white",size=0.55),
                  legend.background = element_blank(),
                  axis.title.x = element_text(color="grey25", size = 16, vjust=0, margin=margin(0,0,0,300)), 
                  axis.title.y = element_text(color="grey25", size = 16, vjust=1, margin=margin(0,2,0,0)),
                  plot.title=element_text(size=rel(1)), 
                  axis.text.x = element_text(color="grey25", size = 14, angle = 0, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
                  axis.text.y = element_text(color="grey25", size = 14, angle = 0, hjust = 0.5))

ggplot(subset(csn_Br, SiteCode == 10730023), aes(x = day, y = MDL, shape = as.factor(year)))+ # color = as.factor(year)
  geom_point(alpha = 0.8, size = 2) +
  ggtitle("Br - MDL") + theme_mdl

ggplot(subset(csn_Br, SiteCode == 10730023), aes(x = as.factor(year), y = MDL, color = day))+
  geom_violin() + 
  geom_point(position=position_jitter(w=0.15)) +
  ggtitle("Br - MDL") + theme_mdl


csn_Al = subset(csn_data, ParamCode == "ALf" & 
                  Method == "Intermittent|Met One SASS/SuperSASS Teflon|Energy dispersive XRF||||0.01088|||3|R|Micrograms/cubic meter (LC)")
csn_Al$dup = duplicated(csn_Al); summary(csn_Al$dup)

csn_Al_420710007 = subset(csn_Al, SiteCode == 420710007)
plot(csn_Al_420710007$day, csn_Al_420710007$MDL) 
plot(csn_Al_420710007$month, csn_Al_420710007$MDL) 
plot(csn_Al_420710007$year, csn_Al_420710007$MDL) 
ggplot(csn_Al_420710007, aes(x = as.factor(year), y = MDL, color = day))+
  geom_violin() + 
  geom_point(position=position_jitter(w=0.15)) +
  ggtitle("Al - MDL") + theme_mdl

summary(csn_Al_420710007$MDL/csn_Al_420710007$Val)
summary(csn_Al_420710007$Val)
subset(csn_Al_420710007, csn_Al_420710007$Val < 0) 

#### Ignore MDL, focus on detection methods for the components #### 
csn_component_method_use = select(csn_data, ParamCode, Method, Unit)
csn_component_method_use = csn_component_method_use[!duplicated(csn_component_method_use), ]
nrow(csn_component_method_use)
head(csn_component_method_use)

csn_component_use = data.frame(ParamCode = csn_component_method_use$ParamCode)
csn_component_use$dup = duplicated(csn_component_use)
sum(csn_component_use$dup) > 0
# then, there are the situation that some components were tested with more than one methods

# detect the number of methods used for each component

# ChatGPT, simplify the code, I have dataframe with columns ParamCode & Method, I want to find out the number of different methods applied for each ParamCode and add the number as a new column in the dataset. 

csn_component_use$dup.1 = duplicated(csn_component_use)
csn_component_use$dup.2 = duplicated(csn_component_use); sum(csn_component_use$dup.2)
csn_component_use$dup.3 = duplicated(csn_component_use); sum(csn_component_use$dup.3)
csn_component_use$dup.3 = NULL # in that  sum(csn_component_use$dup.3) == 0

csn_component_use$method.No = csn_component_use$dup + csn_component_use$dup.1 + csn_component_use$dup.2 + 1
csn_component_use$method.No[csn_component_use$ParamCode %in% csn_component_use$ParamCode[csn_component_use$method.No==4]] = 4
csn_component_use$method.No[csn_component_use$ParamCode %in% csn_component_use$ParamCode[csn_component_use$method.No==3]] = 3
csn_component_use$method.No[csn_component_use$ParamCode %in% csn_component_use$ParamCode[csn_component_use$method.No==2]] = 2
csn_component_method_use$method.No = csn_component_use$method.No
unique(csn_component_method_use$method.No)

csn_para_method_para = read.csv("CSN ParamCode Method code.csv")
head(csn_para_method_para)
csn_component_method_use = join(csn_component_method_use, csn_para_method_para)
head(csn_component_method_use)
# write.csv(csn_component_method_use, "CSN method component reorder.csv")

csn_data = join(csn_data, csn_para_method_para)
colnames(csn_data)
csn_data$Method = NULL

csn_data$State[csn_data$State == -999] = "US.Mex"

write.csv(csn_data,"CSN data for analysis 10232022.csv")


#############################################################################################################
####### 2. explore data with qualifiers #######
#############################################################################################################

csn_data = read.csv("CSN data for analysis 10232022.csv") ## with extracted collection, analysis methods
csn_data$Date = as.Date(csn_data$Date)
csn_data$X = csn_data$X.1 = csn_data$X.2 = NULL

#### combine csn qualifier with EPA codes, calculated appearance frequency separately ####
Qualifier1_freq = as.data.frame(table(csn_data$Qualifier1))
colnames(Qualifier1_freq) = c("Qualifier1", "Freq1")
Qualifier1_freq$flag = Qualifier1_freq$Qualifier1

Qualifier2_freq = as.data.frame(table(csn_data$Qualifier2))
colnames(Qualifier2_freq) = c("Qualifier2", "Freq2")
Qualifier2_freq$flag = Qualifier2_freq$Qualifier2

Qualifier3_freq = as.data.frame(table(csn_data$Qualifier3))
colnames(Qualifier3_freq) = c("Qualifier3", "Freq3")
Qualifier3_freq$flag = Qualifier3_freq$Qualifier3

Qualifier_freq = join(Qualifier1_freq, Qualifier2_freq)
Qualifier_freq = join(Qualifier_freq, Qualifier3_freq)
colnames(Qualifier_freq)[1] = "Qualifier.Code"

# EPA AQS Reference Table: https://aqs.epa.gov/aqsweb/documents/codetables/qualifiers.html
EPA_qualifier = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/EPA qualifiers.csv")
head(EPA_qualifier)
Qualifier_freq_EPA = join(Qualifier_freq, EPA_qualifier)
head(Qualifier_freq_EPA)
Qualifier_freq_EPA$Legacy.Code = NULL
# write.csv(Qualifier_freq_EPA, "CSN Qualifier flags - EPA Explanation.csv")

#### detect the flagged data ####
##### to detect if the qualifier1,2,3 are overlapped
csn_qualifier = select(csn_data, Qualifier1, Qualifier2, Qualifier3)
csn_qualifier1 = subset(csn_qualifier, Qualifier3 != "---")
sum(csn_qualifier1$Qualifier1 != "---") == nrow(csn_qualifier1); sum(csn_qualifier1$Qualifier2 != "---") == nrow(csn_qualifier1)
csn_qualifier2 = subset(csn_qualifier, Qualifier2 != "---")
sum(csn_qualifier2$Qualifier1 != "---") == nrow(csn_qualifier2)
#### the results suggest that some data has one problem, qualifier1, some can have up to 3 problems, from qualifier1 to 3

#### detect the appearance frequency of flags ####
csn_qualifier_only1 = subset(csn_qualifier, Qualifier1 != "---" & Qualifier2 == "---")
csn_qualifier_only1$Qualifier2 = csn_qualifier_only1$Qualifier3 = NULL
csn_qualifier_only1$serial.No = 1:nrow(csn_qualifier_only1)
csn_qualifier_only1 = subset(csn_qualifier_only1, select = c(serial.No, Qualifier1))

csn_qualifier_both12 = subset(csn_qualifier, csn_qualifier$Qualifier2 != "---" & csn_qualifier$Qualifier3 == "---")
csn_qualifier_both12$Qualifier3 = NULL
csn_qualifier_both12$serial.No = 1:nrow(csn_qualifier_both12)
csn_qualifier_both12 = subset(csn_qualifier_both12, select = c(serial.No, Qualifier1, Qualifier2))

csn_qualifier_all123 = csn_qualifier1
csn_qualifier_all123$serial.No = 1:nrow(csn_qualifier_all123)
csn_qualifier_all123 = subset(csn_qualifier_all123, select = c(serial.No, Qualifier1, Qualifier2, Qualifier3))

csn_qualifier_only1$qualifier.code = "Qualifier1"; colnames(csn_qualifier_only1)[2] = "qualifier.type"
csn_qualifier_only1 = subset(csn_qualifier_only1, select = c(serial.No, qualifier.code, qualifier.type))
csn_qualifier_both12 = gather(csn_qualifier_both12, "qualifier.code", "qualifier.type", -serial.No)
csn_qualifier_all123 = gather(csn_qualifier_all123, "qualifier.code", "qualifier.type", -serial.No)

head(csn_qualifier_only1)
head(csn_qualifier_both12)
head(csn_qualifier_all123)
Qualifier_1flag = as.data.frame(table(csn_qualifier_only1$qualifier.type))
colnames(Qualifier_1flag) = c("Qualifier.Code", "Freq.1flag")
Qualifier_2flag = as.data.frame(table(csn_qualifier_both12$qualifier.type))
colnames(Qualifier_2flag) = c("Qualifier.Code", "Freq.2flag")
Qualifier_3flag = as.data.frame(table(csn_qualifier_all123$qualifier.type))
colnames(Qualifier_3flag) = c("Qualifier.Code", "Freq.3flag")


Qualifier_freq_EPA$Freq.total = rowSums(Qualifier_freq_EPA[, c("Freq1", "Freq2", "Freq3")], na.rm=T)
Qualifier_freq_plot = Qualifier_freq_EPA
Qualifier_freq_plot = join(Qualifier_freq_plot, Qualifier_1flag)
Qualifier_freq_plot = join(Qualifier_freq_plot, Qualifier_2flag)
Qualifier_freq_plot = join(Qualifier_freq_plot, Qualifier_3flag)
Qualifier_freq_plot = subset(Qualifier_freq_plot, Qualifier_freq_plot$Qualifier.Code != "---")

Qualifier_freq_plot_use_1 = select(Qualifier_freq_plot, Qualifier.Code, Qaulifier.Type.Code,
                                   Freq.total, Freq.1flag, 
                                   Freq.2flag, Freq.3flag)
Qualifier_freq_INFORM_use = subset(Qualifier_freq_plot_use_1, Qaulifier.Type.Code == "INFORM")
Qualifier_freq_QA_use = subset(Qualifier_freq_plot_use_1, Qaulifier.Type.Code == "QA")
Qualifier_freq_INFORM_use$Qaulifier.Type.Code = Qualifier_freq_QA_use$Qaulifier.Type.Code = NULL

Qualifier_freq_INFORM_use = gather(Qualifier_freq_INFORM_use, "Freq_flag_counts", "Frequency", -Qualifier.Code)
Qualifier_freq_QA_use = gather(Qualifier_freq_QA_use, "Freq_flag_counts", "Frequency", -Qualifier.Code)

theme.qualifier = theme(axis.title.y.right = element_blank(),
                        panel.spacing = unit(10, "mm"),   
                        legend.background = element_blank(),
                        strip.text = element_text(face="bold", size=rel(1.5)),
                        strip.background = element_rect(fill="lightblue", colour="grey", size=16),
                        axis.title.x = element_text(color="grey25", size = 20, vjust=0, margin=margin(0,0,0,300)), 
                        axis.title.y = element_text(color="grey25", size = 20, vjust=1, margin=margin(0,2,0,0)),
                        plot.title=element_text(size=rel(1)), 
                        axis.text.x = element_text(color="grey25", size = 16, angle = 0, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
                        axis.text.y = element_text(color="grey25", size = 16, angle = 0, hjust = 0.5))

# plot with number of data with INFORM flag 
flag_inform_number = 
  ggplot(Qualifier_freq_INFORM_use, 
         aes(x=Qualifier.Code, y=Frequency, 
             fill = Freq_flag_counts)) +
  geom_bar(stat="identity") + 
  facet_grid(Freq_flag_counts ~ .) +  # facet_grid(.~Freq_flag_counts) +
  geom_text(aes(label = Frequency), 
            hjust = -0.1, vjust = 0.2, size = 5, 
            angle = 90, colour = "grey25") + 
  theme.qualifier

# plot without number of data with INFORM flag 
flag_inform_none = 
  ggplot(Qualifier_freq_INFORM_use, 
         aes(x=Qualifier.Code, y=Frequency, 
             fill = Freq_flag_counts)) +
  geom_bar(stat="identity") + 
  facet_grid(Freq_flag_counts ~ .) + 
  theme.qualifier

# plot with number of data with QA flag 
flag_qa_number = 
  ggplot(Qualifier_freq_QA_use, 
         aes(x=Qualifier.Code, y=Frequency, 
             fill = Freq_flag_counts)) +
  geom_bar(stat="identity") + 
  facet_grid(Freq_flag_counts ~ .) +  # facet_grid(.~Freq_flag_counts) +
  geom_text(aes(label = Frequency), hjust = -0.1, 
            vjust = 0.2, size = 5, angle = 90, colour = "grey25") + 
  theme.qualifier

# plot without number of data with QA flag 
flag_qa_none = 
  ggplot(Qualifier_freq_QA_use, 
         aes(x=Qualifier.Code, y=Frequency, fill = Freq_flag_counts)) +
  geom_bar(stat="identity") + 
  facet_grid(Freq_flag_counts ~ .) + 
  theme.qualifier

# zoom in plot without number of data with QA flag 
flag_qa_none_zoom = 
  ggplot(Qualifier_freq_QA_use, 
         aes(x=Qualifier.Code, y=Frequency, fill = Freq_flag_counts)) +
  geom_bar(stat="identity") + 
  facet_grid(Freq_flag_counts ~ .) + 
  ylim(0, 50000) + 
  theme.qualifier

pdf(file = paste("Number of data with one or multiple flags.pdf"), height = 10, width = 16)
par(mfrow = c(2,1), mar = c(5, 5, 2, 2)) #mfrow, c(nr, nc); mar, c(bottom, left, top, right)
flag_inform_number; flag_inform_none; flag_qa_number; flag_qa_none; flag_qa_none_zoom
dev.off()

#### plot the concentration distribution within each QA flag, Quality Assurance Qualifier ####

csn_flag = subset(csn_data, Qualifier1 != "---" & Unit == "ug/m^3")
csn_no_flag = subset(csn_data, Qualifier1 == "---" & Unit == "ug/m^3")

qa_flag = unique(Qualifier_freq_QA_use$Qualifier.Code)
length(qa_flag)

#### Below are flags verified to be INFLUENCING the component values!!!!! according to EPA qualifier explanation
qa_flag_influence = c("1",	"2",	"3",	"4",	"6",	"FX",	"HT",	"LJ",	"MX",
                      "ND",	"QX",	"SX",	"TT",	"V",	"VB",	"W",	"X",	"Y")
## Excluding "MD",	"SQ" # substitute by half of MDL
## Excluding "5",	# not influential
## Excluding "QP",	"QT",	# may only influence the P/T values 
length(qa_flag_influence)

csn_flag$qa1 = ifelse(csn_flag$Qualifier1 %in% qa_flag_influence, T, F)
csn_flag$qa2 = ifelse(csn_flag$Qualifier2 %in% qa_flag_influence, T, F)
csn_flag$qa3 = ifelse(csn_flag$Qualifier3 %in% qa_flag_influence, T, F)
sum(csn_flag$qa1); sum(csn_flag$qa2); sum(csn_flag$qa3)

# find all data with flags
csn_flag$QA = NA
csn_flag$QA[csn_flag$qa1] = csn_flag$Qualifier1
csn_flag$QA[!csn_flag$qa1 & csn_flag$qa2]  = csn_flag$Qualifier2
csn_flag$QA[!csn_flag$qa1 & !csn_flag$qa2 & csn_flag$qa3]  = csn_flag$Qualifier3
summary(is.na(csn_flag$QA))


csn_flag_use = subset(csn_flag, class != "other.parameter")
csn_flag_use = subset(csn_flag_use, CompName != "Accept.PM2.5" & CompName != "PM2.5RC") 
csn_no_flag_use = subset(csn_no_flag, class != "other.parameter")
csn_no_flag_use = subset(csn_no_flag_use, CompName != "Accept.PM2.5" & CompName != "PM2.5RC") 


flag_comp_summary = 
  ddply(csn_flag_use, .(QA, CompName), summarise, 
        Val.mean = mean(Val, na.rm = T), 
        Val.sd = sd(Val, na.rm = T),
        Val.median = median(Val, na.rm = T), 
        Val.25 = quantile(Val, 0.25, na.rm = T), 
        Val.10 = quantile(Val, 0.1, na.rm = T))

flag_No_comp_summary = 
  ddply(csn_no_flag_use, .(CompName), summarise, 
        no.flag.Val.mean = mean(Val, na.rm = T), 
        no.flag.Val.sd = sd(Val, na.rm = T),
        no.flag.Val.90 = quantile(Val, 0.9, na.rm = T), 
        no.flag.Val.75 = quantile(Val, 0.75, na.rm = T), 
        no.flag.Val.median = median(Val, na.rm = T), 
        no.flag.Val.25 = quantile(Val, 0.25, na.rm = T), 
        no.flag.Val.10 = quantile(Val, 0.1, na.rm = T))

comp_summary = join(flag_comp_summary, flag_No_comp_summary)
comp_summary$mean.flag.vs.no = comp_summary$Val.mean/comp_summary$no.flag.Val.mean  
comp_summary$median.flag.vs.no = comp_summary$Val.median/comp_summary$no.flag.Val.median  

#### Plot concentration of each components - Not flagged

theme.conc.ratio =
  theme(axis.title.y.right = element_blank(),
        panel.spacing = unit(10, "mm"),   
        legend.background = element_blank(),
        strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="grey", size=16),
        axis.title.x = element_text(color="grey25", size = 16, vjust=0), 
        axis.title.y = element_text(color="grey25", size = 16, vjust=1),
        plot.title=element_text(size=rel(2)), 
        axis.text.x = element_text(color="grey25", size = 14,
                                   angle = 90, hjust = 0, vjust = 0.3), 
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.y = element_text(color="grey25", size = 14, angle = 0, hjust = 0.5))


no_flag_conc = 
  ggplot(csn_no_flag_use, aes(x = CompName, y = Val)) +
  geom_boxplot() +
  ggtitle("Value of components with No Flags")  + 
  theme.conc.ratio

no_flag_conc_zoom = 
  ggplot(csn_no_flag_use, aes(x = CompName, y = Val)) +
  geom_boxplot() +
  ylim(0, 1) +
  ggtitle("Value of components with No Flags - zoom in")  + 
  theme.conc.ratio


#### Check the concentration ratio of flagged vs. non-flagged data
qa_flag_1 = as.factor(qa_flag[1:6])
qa_flag_2 = qa_flag[7:12]
qa_flag_3 = qa_flag[13:18]
qa_flag_4 = qa_flag[19:23]

# qa_flag_1
qa_flag_1_ratio_mean = 
  ggplot(subset(comp_summary, comp_summary$QA %in% qa_flag_1), 
         aes(x = CompName, y = mean.flag.vs.no)) +
  geom_point(color = "blue3") +
  facet_grid(QA ~ .) +  
  ylim(-5, 5) +
  geom_hline(yintercept=1, color = "brown3") + 
  ggtitle("Flagged/None Ratio - Mean - Quality Assurance series 1")  + 
  theme.conc.ratio

qa_flag_1_ratio_median = 
  ggplot(subset(comp_summary, comp_summary$QA %in% qa_flag_1), 
         aes(x = CompName, y = median.flag.vs.no)) +
  geom_point(color = "green4") +
  facet_grid(QA ~ .) +  
  ylim(-5, 5) +
  geom_hline(yintercept=1, color = "brown3") + 
  ggtitle("Flagged/None Ratio - Median - Quality Assurance series 1")  + 
  theme.conc.ratio

# qa_flag_2
qa_flag_2_ratio_mean = 
  ggplot(subset(comp_summary, comp_summary$QA %in% qa_flag_2),
         aes(x = CompName, y = mean.flag.vs.no)) +
  geom_point(color = "blue3") +
  facet_grid(QA ~ .) +  
  ylim(-5, 5) +
  geom_hline(yintercept=1, color = "brown3") + 
  ggtitle("Flagged/None Ratio - Mean - Quality Assurance series 2")  + 
  theme.conc.ratio

qa_flag_2_ratio_median = 
  ggplot(subset(comp_summary, comp_summary$QA %in% qa_flag_2), 
         aes(x = CompName, y = median.flag.vs.no)) +
  geom_point(color = "green4") +
  facet_grid(QA ~ .) +  
  ylim(-5, 5) +
  geom_hline(yintercept=1, color = "brown3") + 
  ggtitle("Flagged/None Ratio - Median - Quality Assurance series 2")  + 
  theme.conc.ratio

# qa_flag_3
qa_flag_3_ratio_mean = 
  ggplot(subset(comp_summary, comp_summary$QA %in% qa_flag_3), 
         aes(x = CompName, y = mean.flag.vs.no)) +
  geom_point(color = "blue3") +
  facet_grid(QA ~ .) +  
  ylim(-5, 5) +
  geom_hline(yintercept=1, color = "brown3") + 
  ggtitle("Flagged/None Ratio - Mean - Quality Assurance series 3")  + 
  theme.conc.ratio

qa_flag_3_ratio_median = 
  ggplot(subset(comp_summary, comp_summary$QA %in% qa_flag_3), 
         aes(x = CompName, y = median.flag.vs.no)) +
  geom_point(color = "green4") +
  facet_grid(QA ~ .) +  
  ylim(-5, 5) +
  geom_hline(yintercept=1, color = "brown3") + 
  ggtitle("Flagged/None Ratio - Median - Quality Assurance series 3")  + 
  theme.conc.ratio

# qa_flag_4
qa_flag_4_ratio_mean = 
  ggplot(subset(comp_summary, comp_summary$QA %in% qa_flag_4), 
         aes(x = CompName, y = mean.flag.vs.no)) +
  geom_point(color = "blue3") +
  facet_grid(QA ~ .) +  
  ylim(-5, 5) +
  geom_hline(yintercept=1, color = "brown3") + 
  ggtitle("Flagged/None Ratio - Mean - Quality Assurance series 4")  + 
  theme.conc.ratio

qa_flag_4_ratio_median = 
  ggplot(subset(comp_summary, comp_summary$QA %in% qa_flag_4), 
         aes(x = CompName, y = median.flag.vs.no)) +
  geom_point(color = "green4") +
  facet_grid(QA ~ .) +  
  ylim(-5, 5) +
  geom_hline(yintercept=1, color = "brown3") + 
  ggtitle("Flagged/None Ratio - Median - Quality Assurance series 4")  + 
  theme.conc.ratio

pdf(file = paste("concentration of unflagged data & ratio of flagged vs. unflagged.pdf"), height = 10, width = 16)
par(mfrow = c(2,1), mar = c(5, 5, 2, 2)) #mfrow, c(nr, nc); mar, c(bottom, left, top, right)
no_flag_conc; no_flag_conc_zoom
qa_flag_1_ratio_mean; qa_flag_1_ratio_median
qa_flag_2_ratio_mean; qa_flag_2_ratio_median
qa_flag_3_ratio_mean; qa_flag_3_ratio_median
qa_flag_4_ratio_mean; qa_flag_4_ratio_median
dev.off()

#### checking the distribution of each flag - spatiotemporal & for each component ####
comp_concen_no_flag = ddply(csn_no_flag_use, .(CompName), summarize,
                            Val.mean = mean(Val, na.rm = T), 
                            Val.median = median(Val, na.rm = T))
comp_concen_no_flag = comp_concen_no_flag[with(comp_concen_no_flag, order(CompName)), ]

csn_flag_check = csn_flag_use

all_flag = unique(Qualifier_freq_EPA$Qualifier.Code)  
all_flag = all_flag[all_flag != "---"]
INFORM_flag = unique(Qualifier_freq_EPA$Qualifier.Code[Qualifier_freq_EPA$Qaulifier.Type.Code == "INFORM"])  
INFORM_flag = INFORM_flag[!is.na(INFORM_flag)]

Qualifier_freq_EPA$percent = label_percent()(round(Qualifier_freq_EPA$Freq.total/nrow(csn_data), 5))
qualifier.info = Qualifier_freq_EPA
qualifier.info = subset(qualifier.info, Qualifier.Code != "---")

year = rep(2010:2020, each = 12)
month = rep(1:12)
Date_year_month = data.frame(year, month)

state_site = select(csn_data, State, SiteCode)
state_site = state_site[!duplicated(state_site), ]

component_name = data.frame(unique(csn_flag_check$CompName)); colnames(component_name) = "CompName"
component_name$No. = 1:nrow(component_name)
component_name = component_name[with(component_name, order(CompName)), ]
component_name$No. = NULL
state_list = data.frame(table(csn_flag$State)); colnames(state_list)[1] = "State"; state_list$Freq = NULL
year_list = data.frame(unique(Date_year_month$year)); colnames(year_list)[1] = "year"; 
month_list = data.frame(unique(Date_year_month$month));  colnames(month_list)[1] = "month"

Date_year_month_summary = Date_year_month
state_site_summary = state_site
component_summary = comp_concentration = comp_ratio_mean = comp_ratio_median = comp_concen_summary = component_name
state_summary = state_list
year_summary = year_list
month_summary = month_list

#pdf(file = paste("Val distribution by Qualifier - state year component 20221003.pdf"), height = 10, width = 16)
#par(mfrow = c(2,1), mar = c(5, 5, 2, 2)) #mfrow, c(nr, nc); mar, c(bottom, left, top, right)

# for(k in 1:2){
for(k in 1:length(all_flag)){
  flag_qa = all_flag[k]
  percent_summary = qualifier.info$percent[qualifier.info$Qualifier.Code == flag_qa]
  qualifier_expl = qualifier.info$Qualifier.Description[qualifier.info$Qualifier.Code == flag_qa]
  single_qa = subset(csn_flag_check, Qualifier1 == flag_qa | Qualifier2 == flag_qa | Qualifier3 == flag_qa)
  
  month_year_count = data.frame(ddply(single_qa, .(year, month), nrow))
  colnames(month_year_count)[3] = flag_qa
  Date_year_month_summary = join(Date_year_month_summary, month_year_count)
  
  year_count = data.frame(ddply(single_qa, .(year), nrow))
  colnames(year_count)[2] = flag_qa
  year_summary = join(year_summary, year_count)
  
  month_count = data.frame(ddply(single_qa, .(month), nrow))
  colnames(month_count)[2] = flag_qa
  month_summary = join(month_summary, month_count)
  
  state_Site.count = data.frame(ddply(single_qa, .(State, SiteCode), nrow))
  colnames(state_Site.count)[3] = flag_qa
  state_site_summary = join(state_site_summary, state_Site.count)
  
  state.count = data.frame(ddply(single_qa, .(State), nrow))
  colnames(state.count)[2] = flag_qa
  state_summary = join(state_summary, state.count)
  
  component_count = data.frame(ddply(single_qa, .(CompName), nrow))
  colnames(component_count)[2] = flag_qa
  component_summary = join(component_summary, component_count)
  
  component_con = ddply(single_qa, .(CompName), summarize,
                        Val.mean = mean(Val, na.rm = T), 
                        Val.median = median(Val, na.rm = T))
  comp_concentration_qa = join(comp_concentration, component_con)
  comp_ratio_cal = cbind(comp_concentration_qa[1], round(comp_concentration_qa[-1]/comp_concen_no_flag[-1],3))
  comp_ratio_cal_mean = comp_ratio_cal_median = comp_ratio_cal
  comp_ratio_cal_mean$Val.median = comp_ratio_cal_median$Val.mean = NULL
  
  colnames(comp_concentration_qa) = c("CompName", paste0(flag_qa, "-mean.C"), paste0(flag_qa, "-median.C"))
  colnames(comp_ratio_cal_mean) = c("CompName", paste0(flag_qa, "-mean.R"))
  colnames(comp_ratio_cal_median) = c("CompName", paste0(flag_qa, "-median.R"))
  
  comp_concen_summary = join(comp_concen_summary, comp_concentration_qa)
  comp_ratio_mean = join(comp_ratio_mean, comp_ratio_cal_mean)
  comp_ratio_median = join(comp_ratio_median, comp_ratio_cal_median)
}

# dev.off()

write.csv(Date_year_month_summary, "qualifier count - year month.csv")
write.csv(year_summary, "qualifier count - year.csv")
write.csv(month_summary, "qualifier count - month.csv")
write.csv(state_site_summary, "qualifier count - state site.csv")
write.csv(state_summary, "qualifier count - state.csv")
write.csv(component_summary, "qualifier count - component.csv")
write.csv(comp_concen_summary, "qualifier count - component concentration.csv")
write.csv(comp_ratio_mean, "qualifier count - component concentration ratio-mean.csv")
write.csv(comp_ratio_median, "qualifier count - component concentration ratio-median.csv")

#### plot the distribution of each flag - spatiotemporal & for each component ####
comp_ratio_mean_ga = comp_ratio_mean %>% gather(QA, mean.ratio, -CompName)
comp_ratio_mean_ga$qualifier = sub("\\-.*", "", comp_ratio_mean_ga$QA)
# boxplot(mean.ratio~qualifier, comp_ratio_mean_ga, las=2, cex.axis=1.5, cex.lab=1.5, 
#         main = "Ratio of mean value - flagged vs. none", par(mar = c(5, 10, 2, 2)))
boxplot(mean.ratio~qualifier, comp_ratio_mean_ga, las=2, 
        main = "Ratio of mean value - flagged vs. none")
boxplot(mean.ratio~qualifier, comp_ratio_mean_ga, las=2, 
        main = "Ratio of mean value - flagged vs. none", ylim = range(0,10))

comp_ratio_median_ga = comp_ratio_median %>% gather(QA, median.ratio, -CompName)
comp_ratio_median_ga$qualifier = sub("\\-.*", "", comp_ratio_median_ga$QA)
# boxplot(median.ratio~qualifier, comp_ratio_median_ga, las=2, cex.axis=1.5, cex.lab=1.5, 
#         main = "Ratio of median value - flagged vs. none", par(mar = c(5, 10, 2, 2)))
boxplot(median.ratio~qualifier, comp_ratio_median_ga, las=2, 
        main = "Ratio of median value - flagged vs. none")
boxplot(median.ratio~qualifier, comp_ratio_median_ga, las=2, 
        main = "Ratio of median value - flagged vs. none", ylim = range(0,10))

qa_most = c("MD",	"MX",	"J",	"5",	"TT",	"FX")
qa_more_10k = c("2",	"3",	"4",	"IE",	"QP",	"LJ",	"X")
qa_1k_10k = c("1",	"IT",	"IA",	"IF",	"IH",	"II",	"IJ",	"IL",	"IM",	"QT",	"QX",	"SX",	"W",	"Y")
qa_less_1k = c("6",	"HT",	"IC",	"IG",	"IK",	"IN",	"IP",	"IR",	"IS",	"ND",	"SQ",	"V",	"VB")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

qa_year_sum = year_summary %>% gather(QA, count, -year)
qa_year_sum$count[is.na(qa_year_sum$count)] = 0
year_most = ggplot(subset(qa_year_sum, qa_year_sum$QA %in% qa_most), aes(year, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "red")
year_more_10k = ggplot(subset(qa_year_sum, qa_year_sum$QA %in% qa_more_10k), aes(year, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "purple")
year_1k_10k = ggplot(subset(qa_year_sum, qa_year_sum$QA %in% qa_1k_10k), aes(year, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "deepskyblue3")
year_less_1k = ggplot(subset(qa_year_sum, qa_year_sum$QA %in% qa_less_1k), aes(year, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "green")
multiplot(year_most, year_more_10k, year_1k_10k, year_less_1k, cols=2)

qa_month_sum = month_summary %>% gather(QA, count, -month)
qa_month_sum$count[is.na(qa_month_sum$count)] = 0
month_most = ggplot(subset(qa_month_sum, qa_month_sum$QA %in% qa_most), aes(month, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "red")
month_more_10k = ggplot(subset(qa_month_sum, qa_month_sum$QA %in% qa_more_10k), aes(month, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "purple")
month_1k_10k = ggplot(subset(qa_month_sum, qa_month_sum$QA %in% qa_1k_10k), aes(month, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "deepskyblue3")
month_less_1k = ggplot(subset(qa_month_sum, qa_month_sum$QA %in% qa_less_1k), aes(month, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "green")
multiplot(month_most, month_more_10k, month_1k_10k, month_less_1k, cols=2)

qa_state_sum = state_summary %>% gather(QA, count, -State)
qa_state_sum = subset(qa_state_sum, qa_state_sum$State != "US.Mex")
qa_state_sum$count[is.na(qa_state_sum$count)] = 0
state_most = ggplot(subset(qa_state_sum, qa_state_sum$QA %in% qa_most), aes(State, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "red")
state_more_10k = ggplot(subset(qa_state_sum, qa_state_sum$QA %in% qa_more_10k), aes(State, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "purple")
state_1k_10k = ggplot(subset(qa_state_sum, qa_state_sum$QA %in% qa_1k_10k), aes(State, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "deepskyblue3")
state_less_1k = ggplot(subset(qa_state_sum, qa_state_sum$QA %in% qa_less_1k), aes(State, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "green")
multiplot(state_most, state_more_10k, state_1k_10k, state_less_1k, cols=1)

qa_comp_sum = component_summary %>% gather(QA, count, -CompName)
qa_comp_sum$count[is.na(qa_comp_sum$count)] = 0
theme.1 = theme(plot.title = element_text(hjust = 0.05, vjust = -25, size = 16),
                plot.margin = unit(c(2,1,2, 2), "lines"), 
                axis.title.y = element_text(color="grey25", size = 12, vjust=1, margin=margin(0,2,0,0)),
                axis.text.y = element_text(color="grey25", size = 11, angle = 0, hjust = 0.5),
                axis.text.x = element_text(size = 0), axis.title.x = element_text(size = 0))
theme.2 = theme(plot.title = element_text(hjust = 0.05, vjust = -25, size = 16),
                axis.title.x = element_text(color="grey25", size = 12, vjust=0, margin=margin(0,0,0,300)), 
                axis.title.y = element_text(color="grey25", size = 12, vjust=1, margin=margin(0,2,0,0)),
                axis.text.x = element_text(color="grey25", size = 11, angle = 90, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
                axis.text.y = element_text(color="grey25", size = 11, angle = 0, hjust = 0.5))
comp_most = ggplot(subset(qa_comp_sum, qa_comp_sum$QA %in% qa_most), aes(CompName, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "red") + theme.2
comp_more_10k = ggplot(subset(qa_comp_sum, qa_comp_sum$QA %in% qa_more_10k), aes(CompName, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "purple") + theme.2
comp_1k_10k = ggplot(subset(qa_comp_sum, qa_comp_sum$QA %in% qa_1k_10k), aes(CompName, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "deepskyblue3") + theme.1
comp_less_1k = ggplot(subset(qa_comp_sum, qa_comp_sum$QA %in% qa_less_1k), aes(CompName, QA, fill= count)) + 
  geom_tile() + scale_fill_gradient(low = "white", high = "green") + theme.1

multiplot(comp_less_1k, comp_most, cols=1)
multiplot(comp_1k_10k, comp_more_10k, cols=1)

########### plot the distribution of NONE flag - spatiotemporal #########
library(maps) 
library(usmap)
library(ggrepel)

concen_no_flag = ddply(csn_no_flag_use, .(class, CompName), summarize,
                       Val.mean = mean(Val, na.rm = T), 
                       Val.median = median(Val, na.rm = T))
no_use_comp = c("Galactosan",  "Levoglucosan",  "Mannosan",  "Soil",  "CS2")
concen_no_flag = subset(concen_no_flag, !(CompName%in%no_use_comp))
concen_no_flag = concen_no_flag[with(concen_no_flag, order(class, Val.median, CompName)), ]
csn_no_flag_2 = csn_no_flag
concen_no_flag_state_mean = ddply(csn_no_flag, .(class, CompName, State), summarize,
                                  Val.mean = mean(Val, na.rm = T))
concen_no_flag_state_median = ddply(csn_no_flag, .(class, CompName, State), summarize,
                                    Val.median = median(Val, na.rm = T))

concern.a.small = as.character(concen_no_flag$CompName[1:15])
concern.a.med = as.character(concen_no_flag$CompName[16:34])
concern.a.large = as.character(concen_no_flag$CompName[35:38])
concern.b = as.character(concen_no_flag$CompName[39:71])

state_b = 
  ggplot(subset(concen_no_flag_state_mean, 
                concen_no_flag_state_mean$CompName %in% concern.b), 
         aes(State, CompName, fill= Val.mean)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red")
state_a_small = 
  ggplot(subset(concen_no_flag_state_mean, 
                concen_no_flag_state_mean$CompName %in% concern.a.small), 
         aes(State, CompName, fill= Val.mean)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "green")
state_a_med = 
  ggplot(subset(concen_no_flag_state_mean, 
                concen_no_flag_state_mean$CompName %in% concern.a.med), 
         aes(State, CompName, fill= Val.mean)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "deepskyblue3")
state_a_large = 
  ggplot(subset(concen_no_flag_state_mean, 
                concen_no_flag_state_mean$CompName %in% concern.a.large), 
         aes(State, CompName, fill= Val.mean)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "purple")

multiplot(state_a_small, state_a_med, state_a_large, cols=1)

state_b = 
  ggplot(subset(concen_no_flag_state_median, 
                concen_no_flag_state_median$CompName %in% concern.b), 
         aes(State, CompName, fill= Val.median)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red")
state_a_small = 
  ggplot(subset(concen_no_flag_state_median, 
                concen_no_flag_state_median$CompName %in% concern.a.small), 
         aes(State, CompName, fill= Val.median)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "green")
state_a_med = 
  ggplot(subset(concen_no_flag_state_median, 
                concen_no_flag_state_median$CompName %in% concern.a.med), 
         aes(State, CompName, fill= Val.median)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "deepskyblue3")
state_a_large = 
  ggplot(subset(concen_no_flag_state_median, 
                concen_no_flag_state_median$CompName %in% concern.a.large), 
         aes(State, CompName, fill= Val.median)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "purple")

multiplot(state_a_small, state_a_med, state_a_large, cols=1)

site_gps = read.csv("site GPS question or not.csv")
csn_no_flag_1 = join(csn_no_flag_1, site_gps)

# exclude data from AK, HI, PR
exclude_state = c("AK", "HI", "PR", "WY", "US.Mex")

MainStates <- map_data("state")

csn_no_flag_1_plot = subset(csn_no_flag_1, !(csn_no_flag_1$State %in% exclude_state))
csn_no_flag_1_plot$start.year_use = ifelse(csn_no_flag_1_plot$start.year >2014,csn_no_flag_1_plot$start.year, NA)

csn_no_flag_1_plot$X <- 6371e3 * cos(csn_no_flag_1_plot$Latitude) * cos(csn_no_flag_1_plot$Longitude)
csn_no_flag_1_plot$Y <- 6371e3 * cos(csn_no_flag_1_plot$Latitude) * sin(csn_no_flag_1_plot$Longitude)

us_centroids = ddply(csn_no_flag_1_plot, .(State), summarize,
                     state_long = round(mean(Longitude),1),
                     state_lat = round(mean(Latitude),1))
csn_no_flag_1_plot = join(csn_no_flag_1_plot, us_centroids)

no_flag_SO4_median = ddply(subset(csn_no_flag_1_plot, csn_no_flag_1$CompName == "SO4"), 
                           .(State, SiteCode,start.year_use,question.site), summarize,
                           Val.median = median(Val, na.rm = T),
                           Longitude = round(mean(Longitude),1),
                           Latitude = round(mean(Latitude),1),
                           state_long = round(mean(state_long),1),
                           state_lat = round(mean(state_lat),1))

ggplot(no_flag_SO4_median, aes(Longitude, Latitude, color= Val.median)) + 
  geom_point() + scale_color_continuous(low = "white", high = "purple")

theme_species_spatial =
  theme(plot.title = element_text(hjust = 0.05, vjust = -25, size = 16),
        axis.title.x = element_text(color="grey25", size = 12, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 12, vjust=1, margin=margin(0,2,0,0)),
        axis.text.x = element_text(color="grey25", size = 11, angle = 0, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.y = element_text(color="grey25", size = 11, angle = 0, hjust = 0.5))

SO4_median = 
  ggplot(subset(no_flag_SO4_median, 
                no_flag_SO4_median$Val.median < 0.2 & 
                  is.na(no_flag_SO4.median$start.year_use)), 
                    aes(Longitude, Latitude, color= Val.median)) + 
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="darkgrey", fill="lightblue", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_continuous(low = "blue", high = "red") +
  ggtitle("SO4(2-)") + 
  theme_species_spatial

# geom_text(aes(label = start.year_use), hjust = -0.1, vjust = 0.2, size = 2, angle = 90, colour = "black")
# geom_text(data=csn_no_flag_1_plot, aes(x=state_long, y=state_lat, label = State))

## NO3
no_flag_NO3_median = ddply(subset(csn_no_flag_1_plot, csn_no_flag_1$CompName == "NO3"), 
                           .(State, SiteCode,start.year_use,question.site), summarize,
                           Val.median = median(Val, na.rm = T),
                           Longitude = round(mean(Longitude),1),
                           Latitude = round(mean(Latitude),1),
                           state_long = round(mean(state_long),1),
                           state_lat = round(mean(state_lat),1))

ggplot(no_flag_NO3_median, aes(Longitude, Latitude, color= Val.median)) + 
  geom_point() + scale_color_continuous(low = "white", high = "purple")

NO3_median = ggplot(subset(no_flag_NO3_median, 
                           no_flag_NO3_median$Val.median < 0.1 & 
                             is.na(no_flag_NO3.median$start.year_use)), 
                    aes(Longitude, Latitude, color= Val.median)) + 
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="darkgrey", fill="lightblue", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_continuous(low = "blue", high = "red") +
  ggtitle("NO3(-)") + 
  theme_species_spatial

## Na
no_flag_Na_median = ddply(subset(csn_no_flag_1_plot, csn_no_flag_1$CompName == "Na"), 
                          .(State, SiteCode,start.year_use,question.site), summarize,
                          Val.median = median(Val, na.rm = T),
                          Longitude = round(mean(Longitude),1),
                          Latitude = round(mean(Latitude),1),
                          state_long = round(mean(state_long),1),
                          state_lat = round(mean(state_lat),1))

ggplot(no_flag_Na_median, aes(Longitude, Latitude, color= Val.median)) + 
  geom_point() + scale_color_continuous(low = "white", high = "purple")

Na_median = 
  ggplot(subset(no_flag_Na_median, 
                no_flag_Na_median$Val.median < 0.2 & 
                  is.na(no_flag_Na.median$start.year_use)), 
                   aes(Longitude, Latitude, color= Val.median)) + 
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="darkgrey", fill="lightblue", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_continuous(low = "blue", high = "red") +
  ggtitle("Na") + 
  theme_species_spatial

## Fe
no_flag_Fe_median = ddply(subset(csn_no_flag_1_plot, csn_no_flag_1$CompName == "Fe"), 
                          .(State, SiteCode,start.year_use,question.site), summarize,
                          Val.median = median(Val, na.rm = T),
                          Longitude = round(mean(Longitude),1),
                          Latitude = round(mean(Latitude),1),
                          state_long = round(mean(state_long),1),
                          state_lat = round(mean(state_lat),1))

ggplot(no_flag_Fe_median, aes(Longitude, Latitude, color= Val.median)) + 
  geom_point() + scale_color_continuous(low = "white", high = "purple")

Fe.median =  ggplot(subset(no_flag_Fe_median, 
                           no_flag_Fe_median$Val.median < 0.2 & 
                             is.na(no_flag_Fe.median$start.year_use)), 
                    aes(Longitude, Latitude, color= Val.median)) + 
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="darkgrey", fill="lightblue", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_continuous(low = "blue", high = "red") +
  ggtitle("Fe") + 
  theme_species_spatial

## Cd
no_flag_Cd_median = ddply(subset(csn_no_flag_1_plot, csn_no_flag_1$CompName == "Cd"), 
                          .(State, SiteCode,start.year_use,question.site), summarize,
                          Val.median = median(Val, na.rm = T),
                          Longitude = round(mean(Longitude),1),
                          Latitude = round(mean(Latitude),1),
                          state_long = round(mean(state_long),1),
                          state_lat = round(mean(state_lat),1))

ggplot(no_flag_Cd_median, aes(Longitude, Latitude, color= Val.median)) + 
  geom_point() + scale_color_continuous(low = "white", high = "purple")

Cd_median = ggplot(subset(no_flag_Cd_median, no_flag_Cd_median$Val.median < 0.35 & is.na(no_flag_Cd.median$start.year_use)), 
                   aes(Longitude, Latitude, color= Val.median)) + 
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="darkgrey", fill="lightblue", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_continuous(low = "blue", high = "red") +
  ggtitle("Cd") + 
  theme_species_spatial

## As
no_flag_As_median = ddply(subset(csn_no_flag_1_plot, csn_no_flag_1$CompName == "As"), 
                          .(State, SiteCode,start.year_use,question.site), summarize,
                          Val.median = median(Val, na.rm = T),
                          Longitude = round(mean(Longitude),1),
                          Latitude = round(mean(Latitude),1),
                          state_long = round(mean(state_long),1),
                          state_lat = round(mean(state_lat),1))

ggplot(no_flag_As_median, aes(Longitude, Latitude, color= Val.median)) + 
  geom_point() + scale_color_continuous(low = "white", high = "purple")

As_median = ggplot(subset(no_flag_As_median, no_flag_As_median$Val.median < 0.3 & is.na(no_flag_As.median$start.year_use)), 
                   aes(Longitude, Latitude, color= Val.median)) + 
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="darkgrey", fill="lightblue", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_continuous(low = "blue", high = "red") +
  ggtitle("As") + 
  theme_species_spatial

multiplot(SO4_median, NO3_median, Na_median, Fe_median, Cd_median, As_median, cols=2)


## EC88
no_flag_EC88_median = ddply(subset(csn_no_flag_1_plot, csn_no_flag_1$CompName == "EC.88"), 
                            .(State, SiteCode,start.year_use,question.site), summarize,
                            Val.median = median(Val, na.rm = T),
                            Longitude = round(mean(Longitude),1),
                            Latitude = round(mean(Latitude),1),
                            state_long = round(mean(state_long),1),
                            state_lat = round(mean(state_lat),1))

ggplot(no_flag_EC88_median, aes(Longitude, Latitude, color= Val.median)) + 
  geom_point() + scale_color_continuous(low = "white", high = "purple")

EC88_median = ggplot(subset(no_flag_EC88_median, no_flag_EC88_median$Val.median < 0.5 & is.na(no_flag_EC88.median$start.year_use)), 
                     aes(Longitude, Latitude, color= Val.median)) + 
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="darkgrey", fill="lightgrey", alpha = 0.6) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_continuous(low = "yellow", high = "purple") +
  ggtitle("EC88") + 
  theme_species_spatial

## OC88
no_flag_OC88_median = ddply(subset(csn_no_flag_1_plot, csn_no_flag_1$CompName == "OC.88"), 
                            .(State, SiteCode,start.year_use,question.site), summarize,
                            Val.median = median(Val, na.rm = T),
                            Longitude = round(mean(Longitude),1),
                            Latitude = round(mean(Latitude),1),
                            state_long = round(mean(state_long),1),
                            state_lat = round(mean(state_lat),1))

ggplot(no_flag_OC88_median, aes(Longitude, Latitude, color= Val.median)) + 
  geom_point() + scale_color_continuous(low = "white", high = "purple")

OC88_median = ggplot(subset(no_flag_OC88_median, 
                            no_flag_OC88_median$Val.median < 0.3 & is.na(no_flag_OC88.median$start.year_use)), 
                     aes(Longitude, Latitude, color= Val.median)) + 
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="darkgrey", fill="lightgrey", alpha = 0.6) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_continuous(low = "yellow", high = "purple") +
  ggtitle("OC88") + 
  theme_species_spatial

multiplot(OC88_median, EC88_median, cols=2)

#plot_usmap("states", fill = "white", alpha = 0.25, labels = TRUE) +
#  geom_point(subset(no_flag_SO4_median, no_flag_SO4_median$Val.median < 0.2), 
#             aes(X, Y, color= Val.median), color = "purple", alpha = 0.5) +
#  labs(title = "Median SO4(2-) Concentration", subtitle = "concentration < 0.2",
#       size = "SO4") +
#  theme(legend.position = "right")

########### Mapping the source based on chemical mass reconstruction #########
csn_no_flag_3 = csn_no_flag
site_gps = read.csv("site GPS question or not.csv")
csn_no_flag_3 = join(csn_no_flag_3, site_gps)
head(csn_no_flag_3)
# csn_no_flag_3 = csn_no_flag_3[with(csn_no_flag_3, order(Date, State, SiteCode, class, Serial.No.compo)), ]

cmr_no_flag = ddply(csn_no_flag_3, .(State, CompName), summarize, Val = median(Val, na.rm = T)) 
state_gps = ddply(csn_no_flag_3, .(State), summarize, Longitude = median(Longitude, na.rm = T), Latitude = median(Val, na.rm = T))

cmr_no_flag = cmr_no_flag %>% spread(CompName, Val)
head(cmr_no_flag)
head(cmr_no_flag$Soil)
cmr_no_flag$Sea.salt = cmr_no_flag$`Cl-` + 1.448*cmr_no_flag$`Na+` # Chow 2015 AQAH
cmr_no_flag$BC = cmr_no_flag$EC.88
cmr_no_flag$POC = 1.8 * cmr_no_flag$OC.88 # CS Liang_2016_EI
cmr_no_flag = join(cmr_no_flag, state_gps)

MainStates <- map_data("state")
ggplot(cmr_no_flag, aes(Longitude, Latitude, fill = State)) + 
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="darkgrey") +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_continuous(low = "blue", high = "red") +
  ggtitle("SO4(2-)") + 
  theme_species_spatial

cmr_no_flag %>%
  ggplot(aes(Longitude, Latitude, group = State, fill = Soil)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 20, lat1 = 50) +
  labs(fill = "Reconstructed Soil")

class(statepov)
head(statepov)
cmr_no_flag_source = data.frame(abbr=cmr_no_flag$State, Soil=cmr_no_flag$Soil, Sea.salt=cmr_no_flag$Sea.salt, BC=cmr_no_flag$BC, POC=cmr_no_flag$POC)
statepov_use = as_tibble(join(statepov, cmr_no_flag_source))
class(statepov_use)
head(statepov_use)

plot_usmap(data = statepov_use, values = "Soil", labels = T, label_color = "white") +
  labs(title = "Reconstructed Soil") + # subtitle = "Based on function from Chow et al. 2015"
  theme(legend.position = "right")
# scale_fill_continuous(low = "white", high = "blue", name = "Poverty Percentage Estimates", label = scales::comma) + 

plot_usmap(data = statepov_use, values = "Sea.salt", labels = T, label_color = "white") +
  labs(title = "Reconstructed Sea salt") + # subtitle = "Based on function from Chow et al. 2015"
  theme(legend.position = "right")
# scale_fill_continuous(low = "white", high = "blue", name = "Poverty Percentage Estimates", label = scales::comma) + 

plot_usmap(data = statepov_use, values = "BC", labels = T, label_color = "white") +
  labs(title = "BC - EC") + # subtitle = "Based on function from Chow et al. 2015"
  theme(legend.position = "right")
# scale_fill_continuous(low = "white", high = "blue", name = "Poverty Percentage Estimates", label = scales::comma) + 

plot_usmap(data = statepov_use, values = "POC", labels = T, label_color = "white") +
  labs(title = "Reconstructed POC") + # subtitle = "Based on function from Chow et al. 2015"
  theme(legend.position = "right")
# scale_fill_continuous(low = "white", high = "blue", name = "Poverty Percentage Estimates", label = scales::comma) + 

plot_usmap(data = statepov_use, values = "Soil", labels = T, label_color = "white") +
  labs(title = "Reconstructed Soil") + # subtitle = "Based on function from Chow et al. 2015"
  theme(legend.position = "right")
# scale_fill_continuous(low = "white", high = "blue", name = "Poverty Percentage Estimates", label = scales::comma) + 

########### plot NONE flag - MDL vs. Uncertainty vs. Val #########
head(csn_no_flag_2)

csn_no_flag_2 = csn_no_flag_2[with(csn_no_flag_2, order(Date, State, SiteCode, class, Serial.No.compo)), ]
csn_no_flag_2_a = subset(csn_no_flag_2, csn_no_flag_2$class == "a")
csn_no_flag_2_a = subset(csn_no_flag_2_a, csn_no_flag_2_a$CompName != "Soil")

compo_plot = as.character(unique(csn_no_flag_2_a$CompName))

pdf(file = paste("Val MD & Unc of each Ion & Element 20221004.pdf"), height = 7, width = 16)
par(mfrow = c(2,1), mar = c(5, 5, 2, 2)) #mfrow, c(nr, nc); mar, c(bottom, left, top, right)

for(l in 1:length(compo_plot)){
  # for(l in 1:2){
  comp_name = compo_plot[l]
  no_flag_2_a_plot = subset(csn_no_flag_2_a, csn_no_flag_2_a$CompName == comp_name)
  p_unc_mdl = ggplot(no_flag_2_a_plot, aes(MDL, Unc, color = as.integer(year))) + 
    geom_point(alpha = 0.2,size = 3.5) + theme(legend.position="none") +
    theme_species_spatial
  
  p_val_mdl = ggplot(no_flag_2_a_plot, aes(Val, MDL, color = as.integer(year))) + 
    geom_point(alpha = 0.2,size = 3.5) + theme(legend.position="none") +
    theme_species_spatial
  
  p_val_unc = ggplot(no_flag_2_a_plot, aes(Val, Unc, color = as.integer(year))) + 
    geom_point(alpha = 0.2,size = 3.5) +
    theme_species_spatial +
    theme(legend.text = element_text(colour="grey25",  size = 10),
          legend.title = element_text(colour="grey25", size = 0))
  
  p_con_year = ggplot(ddply(no_flag_2_a_plot, .(year), summarize, Val = mean(Val, na.rm = T)), aes(year, Val)) + 
    geom_line(alpha = 0.8) + geom_point(alpha = 0.4,size = 5, shape = 5) + 
    scale_x_continuous(breaks=c(2010, 2012, 2014, 2016, 2018, 2020)) +
    theme_species_spatial
  
  p_con_month = ggplot(ddply(no_flag_2_a_plot, .(month), summarize, Val = mean(Val, na.rm = T)), aes(month, Val)) + 
    geom_line(color = "darkgrey") + geom_point(alpha = 0.6,size = 5, shape = 8)  + 
    scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11)) +
    theme_species_spatial
  
  grid.arrange(p_con_year, p_con_month, nrow = 1, 
               top = textGrob(paste("annual and month change of", comp_name), gp=gpar(fontsize=28)))
  
  grid.arrange(p_unc_mdl, p_val_mdl, p_val_unc, nrow = 1,
               top = textGrob(paste("Relation between Val, MDL & Unc -", comp_name), gp=gpar(fontsize=28)))
  # bottom = textGrob("this footnote is right-justified", gp = gpar(fontface = 3, fontsize = 9), hjust = 1,x = 1)
}

dev.off()


### OC, EC
OC_EC_plot = c("EC.unadjusted.88",	"EC1.unadjusted.88",	"EC2.unadjusted.88",	"EC3.unadjusted.88",	
               "OPC.unadjusted.88",	"OC.unadjusted.88",	"OC1.unadjusted.88",	
               "OC2.unadjusted.88",	"OC3.unadjusted.88",	"OC4.unadjusted.88",
               "OC.88", "EC.88")
csn_no_flag_2_C = subset(csn_no_flag_2, csn_no_flag_2$CompName %in% OC_EC_plot)

pdf(file = paste("Val MD & Unc of each OC & EC 20221004.pdf"), height = 7, width = 16)
par(mfrow = c(2,1), mar = c(5, 5, 2, 2)) #mfrow, c(nr, nc); mar, c(bottom, left, top, right)

for(m in 1:length(OC_EC_plot)){
  # for(m in 1:2){
  comp_name = OC_EC_plot[m]
  no_flag_2_C_plot = subset(csn_no_flag_2_C, csn_no_flag_2_C$CompName == comp_name)
  p_unc_mdl = ggplot(no_flag_2_C_plot, aes(MDL, Unc, color = as.integer(year))) + 
    geom_point(alpha = 0.2,size = 3.5) + theme(legend.position="none") +
    theme(plot.title = element_text(hjust = 0.05, vjust = -50, size = 30),
          axis.title.x = element_text(color="grey25", size = 24, vjust=0, margin=margin(0,0,0,300)), 
          axis.title.y = element_text(color="grey25", size = 24, vjust=1, margin=margin(0,2,0,0)),
          axis.text.x = element_text(color="grey25", size = 22, angle = 0, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
          axis.text.y = element_text(color="grey25", size = 22, angle = 0, hjust = 0.5))
  
  p_val_mdl = ggplot(no_flag_2_C_plot, aes(Val, MDL, color = as.integer(year))) + 
    geom_point(alpha = 0.2,size = 3.5) + theme(legend.position="none") +
    theme(plot.title = element_text(hjust = 0.05, vjust = -50, size = 30),
          axis.title.x = element_text(color="grey25", size = 24, vjust=0, margin=margin(0,0,0,300)), 
          axis.title.y = element_text(color="grey25", size = 24, vjust=1, margin=margin(0,2,0,0)),
          axis.text.x = element_text(color="grey25", size = 22, angle = 0, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
          axis.text.y = element_text(color="grey25", size = 22, angle = 0, hjust = 0.5))
  p_val_unc = ggplot(no_flag_2_C_plot, aes(Val, Unc, color = as.integer(year))) + 
    geom_point(alpha = 0.2,size = 3.5) +
    theme(plot.title = element_text(hjust = 0.05, vjust = -50, size = 30),
          axis.title.x = element_text(color="grey25", size = 24, vjust=0, margin=margin(0,0,0,300)), 
          axis.title.y = element_text(color="grey25", size = 24, vjust=1, margin=margin(0,2,0,0)),
          axis.text.x = element_text(color="grey25", size = 22, angle = 0, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
          axis.text.y = element_text(color="grey25", size = 22, angle = 0, hjust = 0.5),
          legend.text = element_text(colour="grey25",  size = 10),
          legend.title = element_text(colour="grey25", size = 0))
  
  p_con_year = ggplot(ddply(no_flag_2_C_plot, .(year), summarize, Val = mean(Val, na.rm = T)), aes(year, Val)) + 
    geom_line(alpha = 0.8) + geom_point(alpha = 0.4,size = 5, shape = 5) + 
    scale_x_continuous(breaks=c(2010, 2012, 2014, 2016, 2018, 2020)) +
    theme(plot.title = element_text(hjust = 0.05, vjust = -50, size = 30),
          axis.title.x = element_text(color="grey25", size = 24, vjust=0, margin=margin(0,0,0,300)), 
          axis.title.y = element_text(color="grey25", size = 24, vjust=1, margin=margin(0,2,0,0)),
          axis.text.x = element_text(color="grey25", size = 22, angle = 0, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
          axis.text.y = element_text(color="grey25", size = 22, angle = 0, hjust = 0.5))
  p_con_month = ggplot(ddply(no_flag_2_C_plot, .(month), summarize, Val = mean(Val, na.rm = T)), aes(month, Val)) + 
    geom_line(color = "darkgrey") + geom_point(alpha = 0.6,size = 5, shape = 8)  + 
    scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11)) +
    theme(plot.title = element_text(hjust = 0.05, vjust = -50, size = 30),
          axis.title.x = element_text(color="grey25", size = 24, vjust=0, margin=margin(0,0,0,300)), 
          axis.title.y = element_text(color="grey25", size = 24, vjust=1, margin=margin(0,2,0,0)),
          axis.text.x = element_text(color="grey25", size = 22, angle = 0, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
          axis.text.y = element_text(color="grey25", size = 22, angle = 0, hjust = 0.5))
  
  grid.arrange(p_con_year, p_con_month, nrow = 1, 
               top = textGrob(paste("annual and month change of", comp_name), gp=gpar(fontsize=28)))
  
  grid.arrange(p_unc_mdl, p_val_mdl, p_val_unc, nrow = 1,
               top = textGrob(paste("Relation between Val, MDL & Unc -", comp_name), gp=gpar(fontsize=28)))
  # bottom = textGrob("this footnote is right-justified", gp = gpar(fontface = 3, fontsize = 9), hjust = 1,x = 1)
}
dev.off()

###### check data when treat MD as non-flag ######

csn_flag_MD = csn_flag
csn_no_MD = csn_no_flag
csn_flag_MD$multi_qa = rowSums(csn_flag_MD[, c("qa1", "qa2", "qa3")])

csn_no_flag$Q.MD = NA
csn_flag_MD$Q.MD = ifelse(csn_flag_MD$Qualifier1 == "MD" | csn_flag_MD$Qualifier2 == "MD" | 
                            csn_flag_MD$Qualifier3 == "MD" & csn_flag_MD$multi_qa == 1, T, F)
csn_flag_MD_use = csn_flag_MD
csn_flag_MD_use$qa1 = csn_flag_MD_use$qa2 = csn_flag_MD_use$qa3 = csn_flag_MD_use$QA = csn_flag_MD_use$multi_qa = NULL
csn_MD = rbind(csn_flag_MD_use, csn_no_flag)

length(unique(csn_no_flag$SiteCode)); nrow(csn_no_flag)
length(unique(csn_MD$SiteCode)); nrow(csn_MD)
summary(csn_MD$Q.MD) # F, 1462906; T, 1795233; NA, 5182465
length(unique(subset(csn_MD, csn_MD$Q.MD | is.na(csn_MD$Q.MD))$SiteCode))

csn_only_md = subset(csn_MD, csn_MD$Q.MD | is.na(csn_MD$Q.MD)); nrow(csn_only_md)
csn_other_qa = subset(csn_MD, !csn_MD$Q.MD); nrow(csn_other_qa)
state_other_qa = data.frame(table(csn_other_qa$State))
colnames(state_other_qa) = c("State", "QA_count")
state_only_md = data.frame(table(csn_only_md$State))
colnames(state_only_md) = c("State", "No_flag_count")
state_qa = join(state_only_md, state_other_qa)
state_qa$qa_percent = state_qa$QA_count/(state_qa$No_flag_count + state_qa$QA_count) * 100
head(state_qa)

US_state = unique(csn_MD$State)
state_site_method = ddply(csn_MD, .(State, SiteCode, comp.collect.analysis), count)
site_method =  ddply(csn_MD, .(comp.collect.analysis, SiteCode), count)
state_method = ddply(csn_MD, .(comp.collect.analysis, State), count)
site_method_count = data.frame(table(site_method$comp.collect.analysis))
colnames(site_method_count) = c("comp.collect.analysis", "Site.count")
state_method_count = data.frame(table(state_method$comp.collect.analysis))
colnames(state_method_count) = c("comp.collect.analysis", "state.count")
head(site_method_count); nrow(site_method_count)
head(state_method_count); nrow(state_method_count)
summary(site_method_count)
summary(state_method_count)



#############################################################################################################
##### find the temporal coverage to sampling collection/analysis methods #####
csn_data = read.csv("CSN data with date 09252022.csv")
csn_data$Date = as.POSIXct(as.character(csn_data$Date), format="%m/%d/%Y", tz="EST")
csn_data$Date = as.Date(csn_data$Date)
csn_data$X = NULL
csn_data$State[csn_data$State == "-999"] = "US.Mex"

csn_data_1 = csn_data
head(csn_data)
summary(csn_data)
ncol(csn_data); nrow(csn_data)
colnames(csn_data)

csn_data_Q = csn_data
csn_data_Q$X = csn_data_Q$Unit = csn_data_Q$AuxID = csn_data_Q$Status = csn_data_Q$NullDataCode = 
  csn_data_Q$Longitude = csn_data_Q$Latitude = csn_data_Q$Elevation = NULL
head(csn_data_Q)

csn_data_Q$Q1 = ifelse(csn_data_Q$Qualifier1 == "---", F, T)
csn_data_Q$Q2 = ifelse(csn_data_Q$Qualifier2 == "---", F, T)
csn_data_Q$Q3 = ifelse(csn_data_Q$Qualifier3 == "---", F, T)
csn_data_Q$Qualifier_count = csn_data_Q$Q1 + csn_data_Q$Q2 + csn_data_Q$Q3
sum(csn_data_Q$Q1); sum(csn_data_Q$Q2); sum(csn_data_Q$Q3)
table(csn_data_Q$Qualifier_count)

inform_flag = c("IA",	"IC",	"IE",	"IF",	"IG",	"IH",	"II",	"IJ",	"IK",	"IL",	"IM",	"IN",	"IP",	"IR",	"IS",	"IT",	"J")
csn_data_Q$Qaulifier.Infm1 = ifelse(csn_data_Q$Qualifier1 %in% inform_flag, T, F)
csn_data_Q$Qaulifier.Infm2 = ifelse(csn_data_Q$Qualifier2 %in% inform_flag, T, F)
csn_data_Q$Qaulifier.Infm3 = ifelse(csn_data_Q$Qualifier3 %in% inform_flag, T, F)
csn_data_Q$Qualifier.multi.Inform = rowSums(csn_data_Q[, c("Qaulifier.Infm1", "Qaulifier.Infm2", "Qaulifier.Infm3")])
table(csn_data_Q$Qualifier.multi.Inform) 
# 8,307,501, 121,034, 11,710, 359 
csn_data_Q$Qualifier.inform.only = ifelse(csn_data_Q$Qualifier_count == csn_data_Q$Qualifier.multi.Inform & csn_data_Q$Q1, T, F)
sum(csn_data_Q$Qualifier.inform.only) # TRUE, 56371
sum(csn_data_Q$Qaulifier.Infm1); sum(csn_data_Q$Qaulifier.Infm2); sum(csn_data_Q$Qaulifier.Infm3)
# 127,868, 17,174, 489
sum(csn_data_Q$Qaulifier.Infm1)/sum(csn_data_Q$Q1); sum(csn_data_Q$Qaulifier.Infm2)/sum(csn_data_Q$Q2); sum(csn_data_Q$Qaulifier.Infm3)/sum(csn_data_Q$Q3)
# 0.03924572, 0.01629801, 0.003421207

csn_data_Q_1 = csn_data_Q
csn_component_method_use = read.csv("CSN method component reorder 09262022.csv"); csn_component_method_use$X = NULL
csn_data_Q = join(csn_data_Q, csn_component_method_use)
nrow(csn_data_Q)
csn_method = data.frame(State = csn_data_Q$State, SiteCode = csn_data_Q$SiteCode, Date = csn_data_Q$Date, 
                        CompName = csn_data_Q$CompName, class = csn_data_Q$class, Serial.No.compo = csn_data_Q$Serial.No.compo,
                        Collection = csn_data_Q$Collection, Analysis = csn_data_Q$Analysis, Qualifier1 = csn_data_Q$Qualifier1,
                        Qualifier_count = csn_data_Q$Qualifier_count, Qualifier.inform.only = csn_data_Q$Qualifier.inform.only)
# csn_method$Date = as.POSIXct(as.character(csn_method$Date), format="%m/%d/%Y", tz="EST")
csn_method = csn_method[with(csn_method, order(Date, State, SiteCode, class, Serial.No.compo)), ]
head(csn_method)
csn_method$comp.collect.analysis = paste(csn_method$CompName, csn_method$Collection, csn_method$Analysis)
comp_collect_analysis = data.frame(table(csn_method$comp.collect.analysis))
colnames(comp_collect_analysis)[1] = "comp.collect.analysis"
comp_collect_analysis$start.date = comp_collect_analysis$end.date = "1900-00-00"
comp_collect_analysis$start.date = as.Date(as.POSIXct(as.character(comp_collect_analysis$start.date), format="%Y%m/%d/", tz="EST"))
comp_collect_analysis$end.date = as.Date(as.POSIXct(as.character(comp_collect_analysis$end.date), format="%Y%m/%d/", tz="EST"))
comp_collect_analysis$state.count = comp_collect_analysis$Sample_site.count = 0

US_state = unique(csn_data$State)
state_site_method = ddply(csn_method, .(State, SiteCode, comp.collect.analysis), count)
site_method =  ddply(csn_method, .(comp.collect.analysis, SiteCode), count)
state_method = ddply(csn_method, .(comp.collect.analysis, State), count)
site_method_count = data.frame(table(site_method$comp.collect.analysis))
colnames(site_method_count) = c("comp.collect.analysis", "Site.count")
state_method_count = data.frame(table(state_method$comp.collect.analysis))
colnames(state_method_count) = c("comp.collect.analysis", "state.count")
head(site_method_count); nrow(site_method_count)
head(state_method_count); nrow(state_method_count)
summary(site_method_count)
summary(state_method_count)

###### data check
csn_accept_PM_canister = subset(csn_data, csn_data$Analysis == "GC-MS" & csn_data$Collection == "SS.Canister")
csn_accept_PM_MetOne = subset(csn_data, csn_data$Analysis == "Gravemetric" & csn_data$Collection == "MetOne")
length(unique(csn_accept_PM_canister$State)); length(unique(csn_accept_PM_canister$SiteCode))
length(unique(csn_accept_PM_MetOne$State)); length(unique(csn_accept_PM_MetOne$SiteCode))
length(unique(csn_data$State)); length(unique(csn_data$SiteCode))
# 51, 156
####### end of data check

for (i in 1:nrow(comp_collect_analysis)){
  comp_collect_analysis$start.date[i] = csn_method$Date[min(which(csn_method$comp.collect.analysis == comp_collect_analysis$comp.collect.analysis[i]))]
  comp_collect_analysis$end.date[i] = csn_method$Date[max(which(csn_method$comp.collect.analysis == comp_collect_analysis$comp.collect.analysis[i]))]
}
sum(comp_collect_analysis$Freq)

comp_collect_analysis = join(comp_collect_analysis, state_method_count)
comp_collect_analysis = join(comp_collect_analysis, site_method_count)

comp_collect_analysis.sep = data.frame(str_split_fixed(comp_collect_analysis$comp.collect.analysis, " ", 3))
head(comp_collect_analysis.sep)
colnames(comp_collect_analysis.sep) = c("CompName", "Collection", "Analysis")
comp_collect_analysis = cbind(comp_collect_analysis, comp_collect_analysis.sep)
head(comp_collect_analysis)

# comp_reorder = read.csv("CSN method component reorder 09262022.csv"); comp_reorder$X = NULL
comp_collect_analysis_plot = join(comp_collect_analysis, csn_component_method_use); comp_collect_analysis_plot$Method = NULL
comp_collect_analysis_plot = comp_collect_analysis_plot[with(comp_collect_analysis_plot, order(class, method.No, Serial.No.compo, start.date, Collection, Analysis)), ]
comp_collect_analysis_plot$method.samples = paste(comp_collect_analysis_plot$Collection, comp_collect_analysis_plot$Analysis, comp_collect_analysis_plot$Freq)
comp_collect_analysis_plot$comp.collect.analysis = comp_collect_analysis_plot$Freq = comp_collect_analysis_plot$Collection = 
  comp_collect_analysis_plot$Analysis = comp_collect_analysis_plot$appear.sequence = comp_collect_analysis_plot$ParamCode = 
  comp_collect_analysis_plot$Method = comp_collect_analysis_plot$Unit = comp_collect_analysis_plot$original_list = NULL
head(comp_collect_analysis_plot)
# comp_collect_analysis_plot$start.date = as.Date(comp_collect_analysis_plot$start.date)
# comp_collect_analysis_plot$end.date = as.Date(comp_collect_analysis_plot$end.date)

##### find the states and sites used or not used certain method #######
US_state = unique(csn_data$State)
Sample_site = unique(csn_data$SiteCode)
head(comp_site)
comp_site$comp.collect.analysis = paste(comp_site$CompName, comp_site$Collection, comp_site$Analysis)
summary(comp_site$comp.collect.analysis %in% state_site_method$comp.collect.analysis)
summary(state_site_method$comp.collect.analysis %in% comp_site$comp.collect.analysis)
# comp_site_check = data.frame(table(comp_site$comp.collect.analysis))
# state_site_check = data.frame(table(state_site_method$comp.collect.analysis))
state_site_method$comp.collect.analysis[state_site_method$comp.collect.analysis == "OPC.TOR.88 MetOne IMPROVE_a_TOT "] = "OPC.TOR.88 MetOne IMPROVE_a_TOT"

comp_site$state_list = comp_site$site_list = 0
for(j in 1:nrow(comp_site)){
  method.check = comp_site$comp.collect.analysis[j] 
  site.in.method = subset(state_site_method, state_site_method$comp.collect.analysis == method.check)
  if(comp_site$Site.count[j] <= 40){
    state_detect = I(list(US_state[which((US_state %in% site.in.method$State))]))
    site.detect = I(list(Sample_site[which((Sample_site %in% site.in.method$SiteCode))]))
  }else{
    state_detect = I(list(US_state[which(!(US_state %in% site.in.method$State))]))
    site.detect = I(list(Sample_site[which(!(Sample_site %in% site.in.method$SiteCode))]))
  }
  comp_site$state_list[j] = state_detect
  comp_site$site_list[j] = site.detect
}
comp_site$state_list = as.character(comp_site$state_list)
comp_site$site_list = as.character(comp_site$site_list)
write.csv(comp_site, "sampling Date Site State for methods 09272022.csv")

##### plot the temporal method change for components #######
comp_site = read.csv("sampling date for methods 09272022.csv"); comp_site$X = NULL
comp_site$start.date = as.Date(comp_site$start.date)
comp_site$end.date = as.Date(comp_site$end.date)

comp_coll_anly_1 = subset(comp_site, comp_site$method.No == 1)
comp_coll_anly_2 = subset(comp_site, comp_site$method.No == 2)
comp_coll_anly_3 = subset(comp_site, comp_site$method.No == 3)
comp_coll_anly_4 = subset(comp_site, comp_site$method.No == 4)

# extra_row_2 = seq(1, nrow(comp_coll_anly_2), 2) 
extra_row_2 = seq_len(nrow(comp_coll_anly_2)) %% 2
comp_coll_anly_2_extra <- comp_coll_anly_2[extra_row_2 == 0, ]
comp_coll_anly_2_extra$start.date = comp_coll_anly_2_extra$end.date
combine_2_use = rbind(comp_coll_anly_2, comp_coll_anly_2_extra)
combine_2_use = combine_2_use[with(combine_2_use, order(class, Serial.No.compo, start.date)), ]

extra_row_3 = seq_len(nrow(comp_coll_anly_3)) %% 3
comp_coll_anly_3_extra <- comp_coll_anly_3[extra_row_3 == 0, ]
comp_coll_anly_3_extra$start.date = comp_coll_anly_3_extra$end.date
combine_3_use = rbind(comp_coll_anly_3, comp_coll_anly_3_extra)
combine_3_use = combine_3_use[with(combine_3_use, order(class, Serial.No.compo, start.date)), ]

extra_row_4 = seq_len(nrow(comp_coll_anly_4)) %% 4
comp_coll_anly_4_extra <- comp_coll_anly_4[extra_row_4 == 0, ]
comp_coll_anly_4_extra$start.date = comp_coll_anly_4_extra$end.date
combine_4_use = rbind(comp_coll_anly_4, comp_coll_anly_4_extra)
combine_4_use = combine_4_use[with(combine_4_use, order(class, Serial.No.compo, start.date)), ]

collect_analysis_date = rbind(combine_2_use, combine_3_use, combine_4_use)

collect_analysis_date = collect_analysis_date[with(collect_analysis_date, order(class, Serial.No.compo, start.date)), ]
collect_analysis_date$end.date = NULL
colnames(collect_analysis_date)[1] = "date"

collect_analysis_date_a = subset(collect_analysis_date, collect_analysis_date$class == "a"); collect_analysis_date_a$class = NULL
collect_analysis_date_b = subset(collect_analysis_date, collect_analysis_date$class == "b"); collect_analysis_date_b$class = NULL
collect_analysis_date_c = subset(collect_analysis_date, collect_analysis_date$class == "c"); collect_analysis_date_c$class = NULL

####### Number of sites, counties in each states ########
state_county_sites = read.csv("State County & Sites.csv"); state_county_sites$X = NULL
colnames(state_county_sites)
state_info = data.frame(table(state_county_sites$State))
colnames(state_info)[2] = "Site Number"
state_county = data.frame(table(ddply(state_county_sites, .(State, County), count)$State))
colnames(state_county)[2] = "County Number"
state_info = join(state_county, state_info)

###### 

