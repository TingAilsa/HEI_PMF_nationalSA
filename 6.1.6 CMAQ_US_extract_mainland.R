# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran

library(base)
library(dplyr)
library(data.table)
library(fst)
library(mice) # for traffic interpolation

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
getwd()
base_dir = getwd()

#### 1. Basic info ####

# All date related predictors 
date_use = 
  read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Date_DOW_Holiday_2011-20.fst")
us_point_coord = 
  read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Long_lat_Mainland_US_0.1_degree.fst")
# dim(us_point_coord)
# head(date_use)

source_all_names = 
  c("Dust", "Sulfate", "Biomass", "Traffic", "Nitrate", "PM25") # "Traffic", 

# cmaq_period = "2011-01_2011-12"; cmaq_year = 2011
# cmaq_period = "2012-01_2012-12"; cmaq_year = 2012
# cmaq_period = "2013-01_2013-12"; cmaq_year = 2013
# cmaq_period = "2014-01_2014-12"; cmaq_year = 2014
# cmaq_period = "2015-01_2015-12"; cmaq_year = 2015
# cmaq_period = "2016-01_2016-12"; cmaq_year = 2016
# cmaq_period = "2017-01_2017-12"; cmaq_year = 2017

#### 2. Extract data and match new date info ####

for(source_name in source_all_names){ # source_name = "Biomass"; source_name = "Traffic"
  
  # Read CMAQ data 
  # model_input_all_grid = 
  #   read_fst(
  #     paste0(source_name, "_all_CMAQ_points_input_", cmaq_period, ".fst"))
  # head(model_input_all_grid)
  # 
  # model_input_ini = 
  #   read_fst(
  #     paste0(source_name, "_only_PMF_points_input_", cmaq_period, ".fst"))
  # head(model_input_ini)
  
  model_input_all_grid = 
    read_fst(
      paste0(source_name, "_all_CMAQ_points_Daily_", cmaq_period, ".fst"))
  head(model_input_all_grid)
  
  model_input_ini = 
    read_fst(
      paste0(source_name, "_only_PMF_points_Daily_", cmaq_period, ".fst"))
  head(model_input_ini)
  
  # Check if there are duplicates
  model_input_all_grid_dup = 
    dplyr::select(model_input_all_grid, 
                  Date, Longitude, Latitude, SiteCode, Concentration)
  model_input_all_grid_dup$dup = duplicated(model_input_all_grid_dup)
  summary(model_input_all_grid_dup$dup)
  
  model_input_ini_dup = 
    dplyr::select(model_input_ini, 
                  Date, Longitude, Latitude, SiteCode, Concentration)
  model_input_ini_dup$dup = duplicated(model_input_ini_dup)
  summary(model_input_ini_dup$dup)
  
  subset(model_input_ini_dup, Longitude == -102.15 & Latitude == 36.85)
  subset(model_input_all_grid_dup, Longitude == -102.15 & Latitude == 36.85)
  
  ###### Within mainland US CMAQ variable Extraction ######
  
  # Filter points within mainland US
  model_cmaq_allVar_all_grid_us <- 
    merge(model_input_all_grid, us_point_coord, all.y = TRUE)
  # summary(model_cmaq_allVar_all_grid_us); dim(model_cmaq_allVar_all_grid_us)
  
  ###### Update the year month day of week info ###### 
  # Remove duplicated columns
  model_allVar_grid_us_date = model_cmaq_allVar_all_grid_us
  model_allVar_grid_us_date$year = model_allVar_grid_us_date$month = 
    model_allVar_grid_us_date$day_of_week = model_allVar_grid_us_date$is_holiday = NULL
  
  # Match new date info
  date_model_use = subset(date_use, Date %in% model_allVar_grid_us_date$Date)

  model_allVar_grid_us_date =
    merge(model_allVar_grid_us_date, date_model_use, all.x = TRUE)
  # summary(model_allVar_grid_us_date); dim(model_allVar_grid_us_date)
  
  ###### Add and remove columns dependently ######
  # Get PMF_conc & CMAQ_conc for use
  model_input_ini <-
    plyr::rename(model_input_ini,
                 c("Concentration" = "PMF_conc"))
  model_allVar_grid_us_date <-
    plyr::rename(model_allVar_grid_us_date,
                 c("Concentration" = "PMF_conc"))
  
  if(source_name == "Nitrate"){
    model_input_ini <-
      plyr::rename(model_input_ini,
                   c("PM25_TOT_NRD" = "CMAQ_conc"))
    model_allVar_grid_us_date <-
      plyr::rename(model_allVar_grid_us_date,
                   c("PM25_TOT_NRD" = "CMAQ_conc"))
  } else{
    model_input_ini <- 
      model_input_ini %>%
      rename_with(~ "CMAQ_conc", 
                  .cols = intersect(names(model_input_ini),
                                    c("PM25_TOT_EGU", "PM25_TOT_NRD", 
                                      "PM25_TOT_AFI", "PM25_TOT_ARS")))  
    
    model_allVar_grid_us_date <- 
      model_allVar_grid_us_date %>%
      rename_with(~ "CMAQ_conc", 
                  .cols = intersect(names(model_allVar_grid_us_date),
                                    c("PM25_TOT_EGU", "PM25_TOT_NRD", 
                                      "PM25_TOT_AFI", "PM25_TOT_ARS")))
  }
  
  # Get grid_ID
  model_input_ini$grid_ID = 
    paste0("grid_", model_input_ini$Longitude, "_", model_input_ini$Latitude)
  model_input_ini$grid_ID = as.factor(model_input_ini$grid_ID)
  
  model_allVar_grid_us_date$grid_ID = 
    paste0("grid_", model_allVar_grid_us_date$Longitude, "_", model_allVar_grid_us_date$Latitude)
  model_allVar_grid_us_date$grid_ID = as.factor(model_allVar_grid_us_date$grid_ID)
  print("-----Check how the grid_ID look like, if correct-----")
  model_allVar_grid_us_date$grid_ID[1]; model_input_ini$grid_ID[1]
  
  #### Not to used for now, due to the day-by-day prediction needs
  # ### Extract full grid dataset that having no PMF_conc
  # print("Exclude data with PMF records for overall prediction")
  # model_allVar_grid_us_date_noPMF = 
  #   subset(model_allVar_grid_us_date, is.na(PMF_conc))
  # print("-----Make suer there is only NAs in model_allVar_grid_us_date$PMF_conc-----")
  # summary(model_allVar_grid_us_date_noPMF$PMF_conc)
  # 
  # #summary(model_input_ini); dim(model_input_ini)
  # #summary(model_allVar_grid_us_date_noPMF); dim(model_allVar_grid_us_date_noPMF)
  
  # Get column order from model_input_ini
  col_order <- names(model_input_ini)
  
  # Reorder model_allVar_grid_us_date columns
  model_allVar_grid_us_date <- 
    model_allVar_grid_us_date %>%
    dplyr::select(all_of(col_order))
  print("-----If column names are the same in two input datasets-----")
  all(names(model_input_ini) == names(model_allVar_grid_us_date))
  
  dim(model_input_ini); dim(model_allVar_grid_us_date)
  
  # Remove long and lat
  model_input_ini$long = model_input_ini$lat = 
    model_allVar_grid_us_date$long = model_allVar_grid_us_date$lat = NULL
  print("Check summary of model_input_ini")
  summary(model_input_ini)
  print("Check summary of model_allVar_grid_us_date")
  summary(model_allVar_grid_us_date)
  
  ###### Interpolation for census commute for Traffic ######
  model_allVar_grid_us_date_imputed = NULL
  
  if(source_name == "Traffic") {
    
    ##### Interpolation for census commute, use mice
    model_input_all_commute_NA = 
      dplyr::select(model_allVar_grid_us_date,  
                    Longitude, Latitude, 
                    land_type:commute_time)
    
    # Input should be factor or numeric in missForest
    model_input_all_commute_NA = 
      model_input_all_commute_NA %>%
      dplyr::group_by(Longitude, Latitude) %>%
      dplyr::summarise(
        land_type = first(land_type),
        dplyr::across(where(is.numeric), mean, na.rm = TRUE),
        .group = "drop"
      )
    print("Check if all inputs are factor or numeric")
    sapply(model_input_all_commute_NA, class)
    
    ### Conduct interpolation
    # Define mice algorithm
    mice_meth <- 
      sapply(model_input_all_commute_NA, function(x) {
        if (is.factor(x)) "polyreg" else "pmm"
      })
    
    # mice interpolation
    mice_imputed_data <- mice(
      model_input_all_commute_NA,
      m = 5,        
      method = mice_meth,
      maxit = 10,
      printFlag = TRUE
    )
    
    # Use the first dataset
    model_input_all_commute_mice <-
      complete(mice_imputed_data, 1)
    print("Check if there is still NA in census commute data or not")
    summary(model_input_all_commute_mice)
    dim(model_input_all_commute_mice); dim(model_input_all_commute_NA)
    
    # Extract data that only contains commute variables
    model_input_commute_mice <-
      model_input_all_commute_mice %>%
      dplyr::select(Longitude, Latitude, car_truck_van:commute_time)
    
    # Replace by the new values
    # model_allVar_grid_us_date_1 = model_allVar_grid_us_date
    # model_allVar_grid_us_date = model_allVar_grid_us_date_1
    common_cols <- 
      setdiff(names(model_input_commute_mice), c("Longitude", "Latitude"))
    
    model_allVar_grid_us_date_imputed <- 
      model_allVar_grid_us_date %>%
      dplyr::left_join(model_input_commute_mice, 
                by = c("Longitude", "Latitude"), 
                suffix = c("", "_imputed")) %>%
      dplyr::mutate(
        dplyr::across(all_of(common_cols), 
                    ~ ifelse(is.na(.x), 
                             get(paste0(cur_column(), "_imputed")), 
                             .x))) %>%
      dplyr::select(-ends_with("_imputed"))
    
    print("-----Last check if there are NAs, or unused columns in model inputs-----")
    print("-----model_allVar_grid_us_date_imputed----")
    summary(model_allVar_grid_us_date_imputed)
  }
  
  # Decide the final output file
  # Check the existance of a R project, exists(), if checking a file, then file.exists()
  if (!is.null(model_allVar_grid_us_date_imputed)) { 
    all_grid_final = model_allVar_grid_us_date_imputed
  } else {
    all_grid_final = model_allVar_grid_us_date
  }
  
  # Get the number of day
  day_count = length(unique(all_grid_final$Date))
  
  ###### Output files ###### 
  write_fst(all_grid_final, 
            paste0(source_name, "_ML_input_mainlandUS_", day_count, "_days_", cmaq_period, ".fst"))
  write_fst(model_input_ini, 
            paste0(source_name, "_ML_input_only_PMF_sites_", day_count, "_days_", cmaq_period, ".fst"))
}


#### Extract files for CMAQ & PMF comparison, Sumaiya ####

source_all_names = 
  c("Traffic", "Dust", "Sulfate", "Biomass", "Nitrate") # 

cmaq_period_all = 
  c("2014-01_2014-12") # , "2014-01_2014-12"


for(cmaq_period in cmaq_period_all){ # cmaq_period = "2012-01_2012-12"
  
  # Define the name pattern
  all_grid_pattern <- 
    paste0("_ML_input_only_PMF_sites_.*_days_", cmaq_period, "\\.fst$")
  all_grid_path <- 
    list.files(path = ".", pattern = all_grid_pattern, full.names = TRUE)
  print(paste("CMAQ_period", cmaq_period))
  print("All files under this period")
  all_grid_path
  
  # Read files of all grids by source
  Biomass_all_grid = read_fst(all_grid_path[1])
  Dust_all_grid =  read_fst(all_grid_path[2])
  Sulfate_all_grid = read_fst(all_grid_path[3])
  Traffic_all_grid = read_fst(all_grid_path[4])
  
  # Select CMAQ & PMF variables
  head(Biomass_all_grid)
  Biomass_all_grid_cmaq = 
    dplyr::select(Biomass_all_grid, 
                  Date, Longitude, Latitude, CMAQ_conc, PM25_TOT_BIOG, PMF_conc)
  
  head(Dust_all_grid)
  Dust_all_grid_cmaq = 
    dplyr::select(Dust_all_grid, 
                  Date, Longitude, Latitude, CMAQ_conc, PMF_conc)

  head(Sulfate_all_grid)
  Sulfate_all_grid_cmaq = 
    dplyr::select(Sulfate_all_grid, 
                  Date, Longitude, Latitude, CMAQ_conc, PM25_TOT_OTA, NH3, SO2, O3, PMF_conc)

  head(Traffic_all_grid)
  Traffic_all_grid_cmaq = 
    dplyr::select(Traffic_all_grid, 
                  Date, Longitude, Latitude, CMAQ_conc, PMF_conc)
  
  # Write file
  write_fst(Biomass_all_grid,
            file.path("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_annual",
                      paste0("Biomass_CMAQ_PMF_", cmaq_period, ".fst")))
  write_fst(Dust_all_grid,
            file.path("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_annual",
                      paste0("Dust_CMAQ_PMF_", cmaq_period, ".fst")))
  write_fst(Sulfate_all_grid,
            file.path("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_annual",
                      paste0("Sulfate_CMAQ_PMF_", cmaq_period, ".fst")))
  write_fst(Traffic_all_grid,
            file.path("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_annual",
                      paste0("Traffic_CMAQ_PMF_", cmaq_period, ".fst")))
  
}






