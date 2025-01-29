library(lubridate)
library(base)
library(dplyr)

setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/emis_dates_2014/201X")
getwd()
# data.dir <- "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/emis_dates_2014/201X"

smk_2014_files = list.files(pattern = ".*2010.*") # 2010, 2011, 2012, 2013, 2014, 2015
smk_2016_files = list.files(pattern = ".*2016.*")
smk_2014_files; smk_2016_files

for(i in 1:12){
  # Read files of the same month from 2014 & 2016
  smk_2014 = read.csv(smk_2014_files[i])
  smk_2016 = read.csv(smk_2016_files[i])
  smk_2014_name = basename(smk_2014_files[i])
  
  # Get DOW of the date for 2014
  smk_2014$mwdss_N14 = smk_2014$mwdss_N
  smk_2014$mwdss_N14_date = as.Date(as.character(smk_2014$mwdss_N14), format = "%Y%m%d")
  smk_2014$mwdss_N_dow = weekdays(smk_2014$mwdss_N14_date)

  # Get DOW of the date for 2016
  smk_2016$mwdss_N_16 = smk_2016$mwdss_N
  smk_2016$mwdss_N_16_date = as.Date(as.character(smk_2016$mwdss_N_16), format = "%Y%m%d")
  smk_2016$mwdss_N_dow = weekdays(smk_2016$mwdss_N_16_date)
  
  # Get the date of selected DOW in 2016
  smk_2016_dow = dplyr::select(smk_2016, mwdss_N_16, mwdss_N_dow)
  smk_2016_dow = smk_2016_dow[!duplicated(smk_2016_dow), ]

  # Match to add corresponding mwdss_N_16 in smk_2014 via DOW
  smk_2014_update = merge(smk_2014, smk_2016_dow)
  smk_2014_update$mwdss_N = smk_2014_update$mwdss_N_16
  
  # Reorder the file and remove columns not to be used
  smk_2014_update$mwdss_N_dow = smk_2014_update$mwdss_N_16 = smk_2014_update$mwdss_N14_date = NULL
  smk_2014_update = smk_2014_update[with(smk_2014_update, order(Date)), ]
  
  # Format column names to have width of 8, right-justified
  smk_2014_update_formatted = smk_2014_update
  # names(smk_2014_update_formatted) <- 
  #   format(names(smk_2014_update_formatted), width = 8, justify = "right")
  names(smk_2014_update_formatted) <- 
    sapply(names(smk_2014_update_formatted), 
           function(x) format(x, width = 8, justify = "right"))
  names(smk_2014_update_formatted); names(smk_2014_update)
  
  # Format each column to have width of 9, right-justified
  # smk_2014_update_formatted =
  #   as.data.frame(lapply(smk_2014_update_formatted, function(x) {
  #     format(x, width = 9, justify = "right")
  #   }))
  # smk_2014_update_formatted
  
  smk_2014_update_formatted$mwdss_N14 = NULL
  
  write.table(smk_2014_update_formatted, 
              file = file.path(
                "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/emis_dates_2014/",
                smk_2014_name), 
              sep = ", ",
              quote = FALSE,   # Add quotes to protect column structure
              row.names = FALSE)
  
}

