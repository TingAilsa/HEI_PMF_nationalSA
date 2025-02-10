# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran

library(data.table)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_monthly")
getwd()

# Define the list of cmaq_var values
cmaq_var_rds_list = c("PM25_TOT_EGU", "PM25_TOT_OTA",
                     "PM25_TOT_ONR", "PM25_TOT_NRD",  "PM25_TOT_ACM",
                     "PM25_TOT_ASEA", "PM25_TOT_ARS", # "PM25_TOT_DUST", 
                     "PM25_TOT_BIOG", "PM25_TOT_AFI",
                     "O3", "NH3", "SO2", "NO2")

# cmaq_var_rds_list = c("PM25_TOT_OTA",
#                       "PM25_TOT_ACM",
#                       "PM25_TOT_ARS",
#                       "PM25_TOT_BIOG"#, "PM25_TOT_AFI"
# )
# 
# cmaq_var_rds_list = "PM25_TOT_ARS"

cmaq_var_years = c( 2017) # 2011,

for (cmaq_year in cmaq_var_years){ # cmaq_year = cmaq_var_years[2]
  
  # Loop through each cmaq_var
  for (cmaq_var in cmaq_var_rds_list) { # cmaq_var = cmaq_var_rds_list[10]
    
    # Name pattern
    name_pattern = paste0("_cmaq_", cmaq_year, ".*\\.rds$")
    
    # List all .rds files corresponding to the current cmaq_var
    cmaq_rds_files <- 
      list.files(pattern = paste0(cmaq_var, name_pattern), 
                 full.names = TRUE)
    
    # Initialize an empty list to store the data frames
    cmaq_data_list <- list()
    
    # Read all .rds files for the current cmaq_var and store them in cmaq_data_list
    for (cmaq_rds_file in cmaq_rds_files) {
      rds_data <- readRDS(cmaq_rds_file)
      cmaq_data_list[[cmaq_rds_file]] <- rds_data
    }
    
    # Combine all data frames using rbind
    combined_data <- rbindlist(cmaq_data_list)
    
    # Change the column name "cmaq_values" to the first value in "cmaq_variable"
    setnames(combined_data, "cmaq_values", cmaq_var)
    
    # Remove the "cmaq_variable" column
    combined_data[, cmaq_variable := NULL]
    
    # Extract the first and last dates from the "Date" column
    first_date <- format(min(combined_data$Date), "%Y-%m")
    last_date <- format(max(combined_data$Date), "%Y-%m")
    
    # Define the output filename
    output_file <- paste0(cmaq_var, "_cmaq_", first_date, "_", last_date, ".rds")
    output_path <- "/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_annual"
    
    # Save the combined data to a new .rds file
    saveRDS(combined_data, file = file.path(output_path, output_file))
  }
}
