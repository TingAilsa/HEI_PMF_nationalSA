# rm(list = ls())

library( data.table)
library( ggplot2)
library( here)
library( janitor)
library( tigris)
library( tidyr)
library( sf)
library( tidycensus)
library( fst)
library( plyr)
library( dplyr)
library( terra)
library( purrr)
library( raster)
library( fst)
library( magrittr)
library(USAboundaries)
# install.packages('tidycensus')  janitor tigris tidycensus
options(tigris_use_cache = TRUE)


# setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ej_pwe")
setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/")
getwd()

# # Census GEOID
# census_acs_geo_mainland =
#   st_read("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/ACS_census_tract_geoid_geometry_4326_USmainland.fgb")
# length(unique(census_acs_geo_mainland$GEOID))
# head(census_acs_geo_mainland); class(census_acs_geo_mainland)

# # GEOID & EJ metrics
# census_acs_ej =
#   read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_Census_ACS_tract_EJ_2011-2020.fst")
# # head(census_acs_ej)
# # dim(census_acs_ej)

# census_out_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS"

# census_grid = read_fst("US_census/ACS/Census_EJ_geoid_Extract_2017.fst")
# head(census_grid); dim(census_grid)
# length(unique(census_grid$Longitude)); length(unique(census_grid$Latitude))
# 
# us_point_coord = 
#   read.fst("MLresult_RF/Long_lat_Mainland_US_0.1_degree.fst")
# head(us_point_coord); dim(us_point_coord)
# 
# census_grid_us <- 
#   merge(census_grid, us_point_coord, all.y = TRUE)
# head(census_grid_us); dim(census_grid_us)
# summary(census_grid_us)
# write_fst(census_grid_us, "US_census/ACS/Census_EJ_geoid_Extract_2017.fst")

# census_grid_us = read_fst("US_census/ACS/Census_EJ_geoid_Extract_2017.fst")


# # GEOID & grid match
# geoid_grid =
#   read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/GEOID_US_Grid_01_match.fst")
# names(geoid_grid)[1:2] = c("Longitude", "Latitude") 
# length(unique(geoid_grid$GEIOD)); length(unique(geoid_grid$Longitude)); length(unique(geoid_grid$Latitude))
# head(geoid_grid)
# dim(geoid_grid)
# 
# # Match GEOID, grid, and EJ, with chunks
# 
# # Optimized function with more fields and progress tracking
# geiod_census_match <- 
#   function(census_acs_ej, 
#            geoid_grid, 
#            chunk_size, 
#            output_file,
#            resume_from) {
#     
#     # chunk_size = 200; resume_from = 1; output_file = "ej_geoid_grid"
#     
#     # Total number of points to process
#     total_points <- nrow(geoid_grid)
#     total_chunks <- ceiling((total_points - resume_from + 1) / chunk_size)
#     print(paste("Total points & total chunks", total_points, total_chunks))
#     
#     # Create empty list for batch results
#     result_batches <- list()
#     current_batch <- 1
#     
#     # Track timing for progress estimates
#     start_time <- Sys.time()
#     last_time <- start_time
#     
#     # Process each chunk
#     for (chunk in 1:total_chunks) { # chunk = 3
#       # Calculate chunk indices
#       start_idx <- resume_from + (chunk - 1) * chunk_size
#       end_idx <- min(resume_from + chunk * chunk_size - 1, total_points)
#       
#       # Extract current points chunk
#       points_chunk <- geoid_grid[start_idx:end_idx, ]
#       
#       # Merge with EJ
#       census_ej_grid_chunk <- merge(points_chunk, census_acs_ej)
#       
#       # Create results
#       results <- census_ej_grid_chunk
#       
#       # Store in result batch
#       result_batches[[length(result_batches) + 1]] <- results
#       
#       # Save to disk every 5 chunks or on the last chunk
#       if (chunk %% 5 == 0 || chunk == total_chunks) {
#         # Combine batches
#         batch_results <- rbindlist(result_batches)
#         
#         # Write part file, .fst
#         part_file <- paste0(output_file, "_part", current_batch, ".fst")
#         write_fst(batch_results, part_file)
#         cat("Saved batch to:", part_file, "\n")
#         
#         # Clear batch data and increment batch counter
#         result_batches <- list()
#         current_batch <- current_batch + 1
#         
#         # Force garbage collection
#         gc(full = TRUE)
#       }
#       
#       # Clean up to free memory
#       rm(points_chunk, nearest_idx, results, coords)
#       gc(full = TRUE)
#       
#       # Calculate and report progress with time estimates
#       current_time <- Sys.time()
#       elapsed <- as.numeric(difftime(current_time, start_time, units = "secs"))
#       chunk_time <- as.numeric(difftime(current_time, last_time, units = "secs"))
#       last_time <- current_time
#       
#       pct_complete <- 100 * chunk / total_chunks
#       est_remaining <- elapsed / pct_complete * (100 - pct_complete)
#       
#       cat(sprintf("Chunk %d/%d (%.1f%%) - This chunk: %.1f sec - Est. remaining: %.1f min\n", 
#                   chunk, total_chunks, pct_complete,
#                   chunk_time, est_remaining/60))
#     }
#     
#     # Reminder about combining files
#     cat("\nProcessing complete! To combine all part files, run:\n")
#     cat("part_files <- list.files(pattern = \"", basename(output_file), "_part.*\\.fst$\", full.names=TRUE)\n", sep="")
#     cat("all_results <- rbindlist(lapply(part_files, read_fst))\n")
#     cat("write_fst(all_results, \"", output_file, "\")\n", sep="")
#   }


#### Read data ####

census_period = "2011-01_2011-12"; census_year = 2011
# census_period = "2012-01_2012-12"; census_year = 2012
# census_period = "2013-01_2013-12"; census_year = 2013
# census_period = "2014-01_2014-12"; census_year = 2014
# census_period = "2015-01_2015-12"; census_year = 2015
# census_period = "2016-01_2016-12"; census_year = 2016
# census_period = "2017-01_2017-12"; census_year = 2017

census_periods = 
  c("2012-01_2012-12", "2013-01_2013-12", # "2011-01_2011-12", 
    "2014-01_2014-12", "2015-01_2015-12") # , "2016-01_2016-12"
census_years =
  c(2012, 2013, 2014, 2015) # 2011, 2016


# exp_grid_out_path = getwd()
exp_grid_out_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_Results/Aim4_EJ"
print(exp_grid_out_path)

##### Census data

# census_grid_us = read_fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Census_EJ_geoid_Extract_2017.fst")
census_grid_us = read_fst("US_census/ACS/Census_EJ_geoid_Extract_2017.fst")

#### US boundary

us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

##### Exposure data 
# path of output files from ML
# loc <- '/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/Annual_combine/'
loc <- '/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_Results/Aim3_prediction_data/'
exposure_prefix = "RF_Pred_HD_US_01_grids_noUnc_noCoords_"

for(i in 1:length(census_periods)){
  
  ####### Data Preparation #######
  # Get period & year
  census_period = census_periods[i]
  census_year = census_years[i]
  
  print(paste("census_period:", census_period))
  print(paste("census_year:", census_year))
  
  # read rf predictions
  bb <- read.fst( 
    file.path( loc, paste0(exposure_prefix, 'Biomass_', census_period, '.fst')),
    as.data.table = TRUE)
  ss <- read.fst( 
    file.path( loc, paste0(exposure_prefix, 'Sulfate_', census_period, '.fst')),
    as.data.table = TRUE)
  dust <- read.fst( 
    file.path( loc, paste0(exposure_prefix, 'Dust_', census_period, '.fst')),
    as.data.table = TRUE)
  traffic <- read.fst( 
    file.path( loc, paste0(exposure_prefix, 'Traffic_', census_period, '.fst')),
    as.data.table = TRUE)
  
  # remove duplicates
  bb <- bb[ !duplicated( bb[,.( Date, Longitude, Latitude)])]
  ss <- ss[ !duplicated( ss[,.( Date, Longitude, Latitude)])]
  dust <- dust[ !duplicated( dust[,.( Date, Longitude, Latitude)])]
  traffic <- traffic[ !duplicated( traffic[,.( Date, Longitude, Latitude)])]
  
  # take annual average
  bb.avg <- 
    bb[, .( Biomass = mean( Predictions)), by = .( Longitude, Latitude)]
  ss.avg <- 
    ss[, .( Sulfate = mean( Predictions)), by = .( Longitude, Latitude)]
  traffic.avg <- 
    traffic[, .( Traffic = mean( Predictions)), by = .( Longitude, Latitude)]
  dust.avg <- 
    dust[, .( Dust = mean( Predictions)), by = .( Longitude, Latitude)]
  
  # Clear memory
  rm(bb, ss, dust, traffic)
  gc()
  
  # Merge sources
  all_sources_dt <- 
    merge.data.table( bb.avg, ss.avg, by = c( "Longitude", "Latitude")) %>%
    merge.data.table( traffic.avg, by = c( "Longitude", "Latitude")) %>%
    merge.data.table( dust.avg, by = c( "Longitude", "Latitude"))
  
  # Clear memory
  rm(bb.avg, ss.avg, dust.avg, traffic.avg)
  gc()
  
  ##### Merge with Census
  all_sources_census_grid <-
    merge(all_sources_dt, census_grid_us)
  all_sources_census_grid$cell = NULL
  head(all_sources_census_grid); dim(all_sources_census_grid)
  summary(all_sources_census_grid)
  
  # Clear memory
  rm(all_sources_dt)
  gc()
  
  #### Grid population exposure: data preparation #### 
  
  ####### Grid PE 1: data for each caterogy ####### 
  
  all_sources_census_grid_byClass = all_sources_census_grid

  sources_census_grid_race = 
    dplyr::select(all_sources_census_grid_byClass, 
                  Longitude:Dust, WA:H)
  sources_census_grid_race$Total_Pop = 
    rowSums(sources_census_grid_race[, which(colnames(sources_census_grid_race) == "WA"):
                                       which(colnames(sources_census_grid_race) == "H")])
  sources_census_grid_race = na.omit(sources_census_grid_race)
  head(sources_census_grid_race)
  
  tot_pop_us = sum(all_sources_census_grid_byClass$Total_Pop)
  
  # Melt the dataset
  sources_census_grid_race = 
    sources_census_grid_race %>%
    melt(
      id.vars = c("Longitude", "Latitude", 
                  "Biomass", "Sulfate", "Traffic", "Dust", "Total_Pop"),
      measure.vars = c("WA", "BA", "IA", "AA", "NAH", "SO", 
                       "TOM", "TOM_SOR", "TOM_SOR_ThOM", "NH", "H")
    )
  head(sources_census_grid_race); dim(sources_census_grid_race)
  
  ###### Grid PE 2: exposure estimate ######  
  
  # Income, Age, Race, Gender, Marriage, All
  # race
  race_grid_exposure <-
    ddply(sources_census_grid_race, 
          .(Longitude, Latitude, variable),
          summarise,
          total_pop = sum(Total_Pop),
          var_pop = sum(value),
          Biomass = mean(Biomass),
          Sulfate = mean(Sulfate),
          Traffic = mean(Traffic),
          Dust = mean(Dust)) %>%
    mutate(
      Biomass_grid_exp = Biomass * var_pop,
      Sulfate_grid_exp = Sulfate * var_pop,
      Traffic_grid_exp = Traffic * var_pop,
      Dust_grid_exp = Dust * var_pop
    )
  head(race_grid_exposure); dim(race_grid_exposure)
  race_grid_exposure$Year = census_year
  
  # View(race_grid_exposure)
  write_fst(race_grid_exposure, 
            file.path(exp_grid_out_path, paste0("grid_exp_race_", census_year, ".fst")))
  
  # Clear memory
  rm(all_sources_census_grid, all_sources_census_grid_byClass, 
     sources_census_grid_race, race_grid_exposure)
  gc()
  
}

# # read rf predictions
# bb <- read.fst( 
#   file.path( loc, paste0(exposure_prefix, 'Biomass_', census_period, '.fst')),
#   as.data.table = TRUE)
# ss <- read.fst( 
#   file.path( loc, paste0(exposure_prefix, 'Sulfate_', census_period, '.fst')),
#   as.data.table = TRUE)
# dust <- read.fst( 
#   file.path( loc, paste0(exposure_prefix, 'Dust_', census_period, '.fst')),
#   as.data.table = TRUE)
# traffic <- read.fst( 
#   file.path( loc, paste0(exposure_prefix, 'Traffic_', census_period, '.fst')),
#   as.data.table = TRUE)
# 
# # remove duplicates
# bb <- bb[ !duplicated( bb[,.( Date, Longitude, Latitude)])]
# ss <- ss[ !duplicated( ss[,.( Date, Longitude, Latitude)])]
# dust <- dust[ !duplicated( dust[,.( Date, Longitude, Latitude)])]
# traffic <- traffic[ !duplicated( traffic[,.( Date, Longitude, Latitude)])]
# 
# # take annual average
# bb.avg <- 
#   bb[, .( Biomass = mean( Predictions)), by = .( Longitude, Latitude)]
# ss.avg <- 
#   ss[, .( Sulfate = mean( Predictions)), by = .( Longitude, Latitude)]
# traffic.avg <- 
#   traffic[, .( Traffic = mean( Predictions)), by = .( Longitude, Latitude)]
# dust.avg <- 
#   dust[, .( Dust = mean( Predictions)), by = .( Longitude, Latitude)]
# 
# # Merge sources
# all_sources_dt <- 
#   merge.data.table( bb.avg, ss.avg, by = c( "Longitude", "Latitude")) %>%
#   merge.data.table( traffic.avg, by = c( "Longitude", "Latitude")) %>%
#   merge.data.table( dust.avg, by = c( "Longitude", "Latitude"))
# 
# ##### Merge with Census
# all_sources_census_grid <-
#   merge(all_sources_dt, census_grid_us)
# all_sources_census_grid$cell = NULL
# head(all_sources_census_grid); dim(all_sources_census_grid)
# summary(all_sources_census_grid)
# 

#### Grid population exposure: data preparation #### 

####### Grid PE 1: data for each caterogy ####### 

# Define age groups and their component columns
age_groups <- list(
  Children = c("0.5", "5.9"),
  Adolescents = c("10.14", "15.17", "18_19"),
  YoungAdults = c("20", "21", "22.24", "25.29", "30.34", "35.39", "40.44"),
  MiddleAge = c("45.49", "50.54", "55.59", "60_61", "62.64"),
  Senior = c("65_66", "67.69", "70.74", "75.79", "80.84", "85.over")
)

all_sources_census_grid_byClass = all_sources_census_grid

# Create columns for both genders
for (gender in c("M", "F")) {
  for (group_name in names(age_groups)) {
    # Create column names to sum
    cols_to_sum <- paste0(gender, "_", age_groups[[group_name]])
    
    # Create the new column by summing the component columns
    all_sources_census_grid_byClass[, paste0(gender, "_", group_name) := rowSums(.SD, na.rm = TRUE), 
                           .SDcols = cols_to_sum]
  }
  
  # Get list of all detailed age columns to remove
  cols_to_remove <- unlist(lapply(age_groups, function(x) paste0(gender, "_", x)))
  
  # Remove the original detailed columns
  all_sources_census_grid_byClass[, (cols_to_remove) := NULL]
}

# Define age groups
age_groups <- c("Children", "Adolescents", "YoungAdults", "MiddleAge", "Senior")

# Create combined columns for each age group (both genders)
for (group in age_groups) {
  all_sources_census_grid_byClass[[group]] <- 
    all_sources_census_grid_byClass[[paste0("M_", group)]] + 
    all_sources_census_grid_byClass[[paste0("F_", group)]]
}

all_sources_census_grid_byClass$Total_Pop = 
  all_sources_census_grid_byClass$F_all + all_sources_census_grid_byClass$M_all
all_sources_census_grid_byClass = na.omit(all_sources_census_grid_byClass)
head(all_sources_census_grid_byClass)

tot_pop_us = sum(all_sources_census_grid_byClass$Total_Pop)

sources_census_grid_race = 
  dplyr::select(all_sources_census_grid_byClass, 
                Longitude:Dust, Total_Pop, WA:H)
sources_census_grid_age_gender = 
  dplyr::select(all_sources_census_grid_byClass, 
                Longitude:Dust, Total_Pop, M_Children:F_Senior)
sources_census_grid_age = 
  dplyr::select(all_sources_census_grid_byClass, 
                Longitude:Dust, Total_Pop, Children:Senior)
sources_census_grid_gender = 
  dplyr::select(all_sources_census_grid_byClass, 
                Longitude:Dust, Total_Pop, M_all:F_all)
sources_census_grid_marriage = 
  dplyr::select(all_sources_census_grid_byClass, 
                Longitude:Dust, Total_Pop, M_married, F_married)

sources_census_grid_income = 
  dplyr::select(all_sources_census_grid_byClass, 
                Longitude:Dust, Household_Income)
summary(sources_census_grid_income$Household_Income)
  
# Melt the dataset
sources_census_grid_race = 
  sources_census_grid_race %>%
  melt(
    id.vars = c("Longitude", "Latitude", 
                "Biomass", "Sulfate", "Traffic", "Dust", "Total_Pop"),
    measure.vars = c("WA", "BA", "IA", "AA", "NAH", "SO", 
                     "TOM", "TOM_SOR", "TOM_SOR_ThOM", "NH", "H")
  )
head(sources_census_grid_race); dim(sources_census_grid_race)


sources_census_grid_age = 
  sources_census_grid_age %>%
  melt(
    id.vars = c("Longitude", "Latitude", 
                "Biomass", "Sulfate", "Traffic", "Dust", "Total_Pop"),
    measure.vars = c("Children", "Adolescents", "YoungAdults", "MiddleAge", "Senior")
  )
head(sources_census_grid_age); dim(sources_census_grid_age)


sources_census_grid_gender = 
  sources_census_grid_gender %>%
  melt(
    id.vars = c("Longitude", "Latitude", 
                "Biomass", "Sulfate", "Traffic", "Dust", "Total_Pop"),
    measure.vars = c("M_all", "F_all")
  )
head(sources_census_grid_gender); dim(sources_census_grid_gender)

sources_census_grid_marriage = 
  sources_census_grid_marriage %>%
  melt(
    id.vars = c("Longitude", "Latitude", 
                "Biomass", "Sulfate", "Traffic", "Dust", "Total_Pop"),
    measure.vars = c("M_married", "F_married")
  )
head(sources_census_grid_marriage); dim(sources_census_grid_marriage)

sources_census_grid_income 
head(sources_census_grid_income); dim(sources_census_grid_income)


###### Grid PE 2: exposure estimate ######  

# Income, Age, Race, Gender, Marriage, All
# race
race_grid_exposure <-
  ddply(sources_census_grid_race, 
        .(Longitude, Latitude, variable),
        summarise,
        total_pop = sum(Total_Pop),
        var_pop = sum(value),
        Biomass = mean(Biomass),
        Sulfate = mean(Sulfate),
        Traffic = mean(Traffic),
        Dust = mean(Dust)) %>%
  mutate(
    Biomass_grid_exp = Biomass * var_pop,
    Sulfate_grid_exp = Sulfate * var_pop,
    Traffic_grid_exp = Traffic * var_pop,
    Dust_grid_exp = Dust * var_pop
  )
head(race_grid_exposure); dim(race_grid_exposure)
race_grid_exposure$Year = census_year

# View(race_grid_exposure)
write_fst(race_grid_exposure, 
          file.path(exp_grid_out_path, paste0("grid_exp_race_", census_year, ".fst")))

# age
age_grid_exposure <-
  ddply(sources_census_grid_age, 
        .(Longitude, Latitude, variable),
        summarise,
        total_pop = sum(Total_Pop),
        var_pop = sum(value),
        Biomass = mean(Biomass),
        Sulfate = mean(Sulfate),
        Traffic = mean(Traffic),
        Dust = mean(Dust)) %>%
  mutate(
    Biomass_grid_exp = Biomass * var_pop,
    Sulfate_grid_exp = Sulfate * var_pop,
    Traffic_grid_exp = Traffic * var_pop,
    Dust_grid_exp = Dust * var_pop
  )
head(age_grid_exposure); dim(age_grid_exposure)
age_grid_exposure$Year = census_year

write_fst(age_grid_exposure, 
          file.path(exp_grid_out_path, paste0("grid_exp_age_", census_year, ".fst")))

# gender
gender_grid_exposure <-
  ddply(sources_census_grid_gender, 
        .(Longitude, Latitude, variable),
        summarise,
        total_pop = sum(Total_Pop),
        var_pop = sum(value),
        Biomass = mean(Biomass),
        Sulfate = mean(Sulfate),
        Traffic = mean(Traffic),
        Dust = mean(Dust)) %>%
  mutate(
    Biomass_grid_exp = Biomass * var_pop,
    Sulfate_grid_exp = Sulfate * var_pop,
    Traffic_grid_exp = Traffic * var_pop,
    Dust_grid_exp = Dust * var_pop
  )
head(gender_grid_exposure); dim(gender_grid_exposure)
gender_grid_exposure$Year = census_year

write_fst(gender_grid_exposure, 
          file.path(exp_grid_out_path, paste0("grid_exp_gender_", census_year, ".fst")))

# marriage
marriage_grid_exposure <-
  ddply(sources_census_grid_marriage, 
        .(Longitude, Latitude, variable),
        summarise,
        total_pop = sum(Total_Pop),
        var_pop = sum(value),
        Biomass = mean(Biomass),
        Sulfate = mean(Sulfate),
        Traffic = mean(Traffic),
        Dust = mean(Dust)) %>%
  mutate(
    Biomass_grid_exp = Biomass * var_pop,
    Sulfate_grid_exp = Sulfate * var_pop,
    Traffic_grid_exp = Traffic * var_pop,
    Dust_grid_exp = Dust * var_pop
  )
head(marriage_grid_exposure); dim(marriage_grid_exposure)
marriage_grid_exposure$Year = census_year

write_fst(marriage_grid_exposure, 
          file.path(exp_grid_out_path, paste0("grid_exp_marriage_", census_year, ".fst")))

# # income
# income_grid_exposure <-
#   ddply(sources_census_grid_income, 
#         .(Longitude, Latitude, variable),
#         summarise,
#         total_pop = sum(value),
#         mean_pop = mean(value),
#         Biomass = mean(Biomass),
#         Sulfate = mean(Sulfate),
#         Traffic = mean(Traffic),
#         Dust = mean(Dust)) %>%
#   mutate(
#     Biomass_grid_exp = Biomass * var_pop,
#     Sulfate_grid_exp = Sulfate * var_pop,
#     Traffic_grid_exp = Traffic * var_pop,
#     Dust_grid_exp = Dust * var_pop
#   )
# head(income_grid_exposure); dim(income_grid_exposure)
# income_grid_exposure$Year = census_year
# 
# write_fst(income_grid_exposure, 
#           file.path(exp_grid_out_path, paste0("grid_exp_income_", census_year, ".fst")))

#### PWE: Population Weighted Exposure Estimation #### 

## read grid level exposure files
race_grid_exposure =
  read_fst( file.path(exp_grid_out_path, paste0("grid_exp_race_", census_year, ".fst")))
age_grid_exposure =
  read_fst( file.path(exp_grid_out_path, paste0("grid_exp_age_", census_year, ".fst")))
gender_grid_exposure =
  read_fst( file.path(exp_grid_out_path, paste0("grid_exp_gender_", census_year, ".fst")))
marriage_grid_exposure =
  read_fst( file.path(exp_grid_out_path, paste0("grid_exp_marriage_", census_year, ".fst")))
# income_grid_exposure =
#   read_fst( file.path(exp_grid_out_path, paste0("grid_exp_income_", census_year, ".fst")))

ej_path <- "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_Results/Aim4_EJ"

# List all .fst files matching the pattern
race_fst_files <- 
  list.files(path = ej_path, 
             pattern = "grid_exp_race_20\\d{2}\\.fst$", 
             full.names = TRUE)
race_fst_files

# Combine all files
race_overall <- 
  do.call(
    rbind, 
    (lapply(
      race_fst_files, 
      read_fst)))
head(race_overall)

# Get annual PWE
race_pwe_us = 
  ddply(race_overall, 
        .(Year, variable),
        summarise, 
        Biomass = sum(Biomass_grid_exp) / sum(var_pop),
        Sulfate = sum(Sulfate_grid_exp) / sum(var_pop),
        Traffic = sum(Traffic_grid_exp) / sum(var_pop),
        Dust = sum(Dust_grid_exp) / sum(var_pop)
  )

# Change the data style
race_pwe_us_long = 
  race_pwe_us %>%
  pivot_longer(
    cols = Biomass:Dust,
    names_to = "Sources",
    values_to = "PWE"
  ) %>%
  mutate(Sources = gsub("_pwe$", "", Sources))
head(race_pwe_us_long)

race_pwe_us_long = 
  subset(race_pwe_us_long, 
         !(variable %in% c("SO", "TOM", "TOM_SOR", "TOM_SOR_ThOM")))
head(race_pwe_us_long)
race_pwe_us_long =
  plyr::rename(race_pwe_us_long,
               c("variable" = "Race"))

race_pwe_us_long <- 
  race_pwe_us_long %>%
  mutate(Race = case_when(
    Race == "WA" ~ "White",
    Race == "BA" ~ "Black",
    TRUE ~ as.character(Race)  # Keep other values as they are
  ))

ggplot(data = subset(race_pwe_us_long, Race %in% c("White", "Black")), 
       aes(x = as.factor(Year), y = PWE, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  facet_wrap(Sources ~ ., ncol = 2)  + 
  scale_fill_manual(values = c("#ff7f00", "#377eb8")) + 
  # y start from 0, and number of breaks, pretty function
  scale_y_continuous(limits = c(0, NA),
                     breaks = function(x) pretty(x, n = 3)) + 
  scale_x_discrete(breaks = 2011:2017) +
  labs(title = "PWE_race") +
  ylab(format_variable("PWE µg/m3")) +
  xlab(format_variable("Year")) +
  theme_base(base_size = 26) + 
  theme(
    legend.position = "bottom",
    panel.spacing = unit(10, "mm"),
    strip.background = element_rect(fill = "grey85"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text( angle = 45, hjust = 1, vjust = 1))

# race_pwe_bar <-
  ggplot(data = race_pwe_us_long, 
         aes(x = variable, y = PWE)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.4) +
  facet_wrap(Sources ~ Year, ncol = 7)  + 
  # y start from 0, and number of breaks, pretty function
  scale_y_continuous(limits = c(0, NA),
                     breaks = function(x) pretty(x, n = 3)) + 
  labs(title = "PWE_race") +
  ylab(format_variable("PWE µg/m3")) +
  theme_base(base_size = 22) + 
  theme(panel.spacing = unit(10, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text( angle = 45, hjust = 1, vjust = 1))
# race_pwe_bar

# ggsave(
#   file.path("EJ_plot",
#             paste0("Race_nation_PWE_US_01_grid_", cmaq_period, "_2.pdf")), 
#   plot = race_pwe_bar, width = 8, height = 6)


#### PWE: Nationwide #### 

race_pwe_us = 
  ddply(race_grid_exposure, 
        .(variable),
        summarise, 
        Biomass = sum(Biomass_grid_exp) / sum(var_pop),
        Sulfate = sum(Sulfate_grid_exp) / sum(var_pop),
        Traffic = sum(Traffic_grid_exp) / sum(var_pop),
        Dust = sum(Dust_grid_exp) / sum(var_pop)
  )

age_pwe_us = 
  ddply(age_grid_exposure, 
        .(variable),
        summarise, 
        Biomass = sum(Biomass_grid_exp) / sum(var_pop),
        Sulfate = sum(Sulfate_grid_exp) / sum(var_pop),
        Traffic = sum(Traffic_grid_exp) / sum(var_pop),
        Dust = sum(Dust_grid_exp) / sum(var_pop)
  )

gender_pwe_us = 
  ddply(gender_grid_exposure, 
        .(variable),
        summarise, 
        Biomass = sum(Biomass_grid_exp) / sum(var_pop),
        Sulfate = sum(Sulfate_grid_exp) / sum(var_pop),
        Traffic = sum(Traffic_grid_exp) / sum(var_pop),
        Dust = sum(Dust_grid_exp) / sum(var_pop)
  )

marriage_pwe_us = 
  ddply(marriage_grid_exposure, 
        .(variable),
        summarise, 
        Biomass = sum(Biomass_grid_exp) / sum(var_pop),
        Sulfate = sum(Sulfate_grid_exp) / sum(var_pop),
        Traffic = sum(Traffic_grid_exp) / sum(var_pop),
        Dust = sum(Dust_grid_exp) / sum(var_pop)
  )

genderMarry_pwe_us = rbind(gender_pwe_us, marriage_pwe_us)

# income_grid_pwe_us = 
#   ddply(income_grid_exposure, 
#         .(variable),
#         summarise, 
#         Biomass = sum(Biomass_grid_exp) / sum(var_pop),
#         Sulfate = sum(Sulfate_grid_exp) / sum(var_pop),
#         Traffic = sum(Traffic_grid_exp) / sum(var_pop),
#         Dust = sum(Dust_grid_exp) / sum(var_pop)
#   )

##### Plotting
# Income, Age, Race, Gender, Marriage, All
# race
race_pwe_us_long = 
  race_pwe_us %>%
  pivot_longer(
    cols = Biomass:Dust,
    names_to = "Sources",
    values_to = "PWE"
  ) %>%
  mutate(Sources = gsub("_pwe$", "", Sources))
head(race_pwe_us_long)

race_pwe_us_long = 
  subset(race_pwe_us_long, 
         !(variable %in% c("SO", "TOM", "TOM_SOR", "TOM_SOR_ThOM")))

race_pwe_bar <-
  ggplot(data = race_pwe_us_long, 
         aes(x = variable, y = PWE)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.4) +
  facet_wrap(Sources ~ .)  + 
  # y start from 0, and number of breaks, pretty function
  scale_y_continuous(limits = c(0, NA),
                     breaks = function(x) pretty(x, n = 3)) + 
  labs(title = "PWE_race") +
  ylab(format_variable("PWE µg/m3")) +
  theme_base(base_size = 22) + 
  theme(panel.spacing = unit(10, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text( angle = 45, hjust = 1, vjust = 1))
# race_pwe_bar

ggsave(
  file.path("EJ_plot",
            paste0("Race_nation_PWE_US_01_grid_", cmaq_period, "_2.pdf")), 
  plot = race_pwe_bar, width = 8, height = 6)

# age
age_pwe_us_long = 
  age_pwe_us %>%
  pivot_longer(
    cols = Biomass:Dust,
    names_to = "Sources",
    values_to = "PWE"
  ) %>%
  mutate(Sources = gsub("_pwe$", "", Sources))
head(age_pwe_us_long)

age_pwe_bar <-
  ggplot(data = age_pwe_us_long, 
         aes(x = variable, y = PWE)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.4) +
  facet_wrap(Sources ~ .)  + 
  # y start from 0, and number of breaks, pretty function
  scale_y_continuous(limits = c(0, NA),
                     breaks = function(x) pretty(x, n = 3)) + 
  labs(title = "PWE_age") +
  theme_base(base_size = 22) + 
  theme(panel.spacing = unit(10, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text( angle = 45, hjust = 1, vjust = 1))
age_pwe_bar

ggsave(
  file.path("EJ_plot",
            paste0("Age_nation_PWE_US_01_grid_", cmaq_period, ".pdf")), 
  plot = age_pwe_bar, width = 8, height = 6)

# genderMarry
genderMarry_pwe_us_long = 
  genderMarry_pwe_us %>%
  pivot_longer(
    cols = Biomass:Dust,
    names_to = "Sources",
    values_to = "PWE"
  ) %>%
  mutate(Sources = gsub("_pwe$", "", Sources))
head(genderMarry_pwe_us_long)

genderMarry_pwe_bar <-
  ggplot(data = genderMarry_pwe_us_long, 
         aes(x = variable, y = PWE)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.4) +
  facet_wrap(Sources ~ .)  + 
  # y start from 0, and number of breaks, pretty function
  scale_y_continuous(limits = c(0, NA),
                     breaks = function(x) pretty(x, n = 3)) + 
  labs(title = "PWE_genderMarry") +
  theme_base(base_size = 22) + 
  theme(panel.spacing = unit(10, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text( angle = 45, hjust = 1, vjust = 1))
genderMarry_pwe_bar

ggsave(
  file.path("EJ_plot",
            paste0("GenderMarriage_nation_PWE_US_01_grid_", cmaq_period, ".pdf")), 
  plot = genderMarry_pwe_bar, width = 8, height = 6)

# # income
# income_pwe_us_long = 
#   income_pwe_us %>%
#   pivot_longer(
#     cols = Biomass:Dust,
#     names_to = "Sources",
#     values_to = "PWE"
#   ) %>%
#   mutate(Sources = gsub("_pwe$", "", Sources))
# head(income_pwe_us_long)
# 
# income_pwe_bar <-
#   ggplot(data = income_pwe_us_long, 
#          aes(x = variable, y = PWE)) +
#   geom_bar(stat = "identity", fill = "steelblue", width = 0.4) +
#   facet_wrap(Sources ~ .)  + 
#   # y start from 0, and number of breaks, pretty function
#   scale_y_continuous(limits = c(0, NA),
#                      breaks = function(x) pretty(x, n = 3)) + 
#   labs(title = "PWE_income") +
#   theme_base(base_size = 22) + 
#   theme(panel.spacing = unit(10, "mm"),
#         strip.background = element_rect(fill = "grey85"),
#         strip.text = element_text(face = "bold"),
#         axis.text.x = element_text( angle = 45, hjust = 1, vjust = 1))
# # income_pwe_bar
# 
# ggsave(
#   file.path("EJ_plot",
#             paste0("Income_nation_PWE_US_01_grid_", cmaq_period, ".pdf")), 
#   plot = income_pwe_bar, width = 8, height = 6)




#### PWE: Census Tract #### 
# Census GEOID
census_acs_geo_mainland =
  st_read("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/ACS_census_tract_geoid_geometry_4326_USmainland.fgb")
# census_acs_geo_mainland =
#   st_read("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/ACS_census_tract_geoid_geometry_4326_USmainland.fgb")

length(unique(census_acs_geo_mainland$GEOID))
head(census_acs_geo_mainland); class(census_acs_geo_mainland)

### GEOID of each year
census_acs_ej = read_fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/US_Census_ACS_tract_EJ_2011-2020.fst")
# census_acs_ej = read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_Census_ACS_tract_EJ_2011-2020.fst")
# head(census_acs_ej)
# dim(census_acs_ej)

census_acs_ej_year = subset(census_acs_ej, year == census_year)

# Extract GEOID of the census_year
census_acs_geo_mainland_year = 
  subset(census_acs_geo_mainland, GEOID %in% census_acs_ej_year$GEOID)
dim(census_acs_geo_mainland); dim(census_acs_geo_mainland_year)

# race_grid_exposure  age_grid_exposure  gender_grid_exposure
# marriage_grid_exposure  income_grid_exposure

### race
# make it spacial
p4s <- 4326

for(race_var in unique(race_grid_exposure$variable)) { # race_var = 'WA'
  race_var_grid_exposure <-
    subset(race_grid_exposure, variable == race_var)
  race_var_grid_exposure$variable = NULL
  
  race_var_grid_exp_r <- rasterFromXYZ( race_var_grid_exposure, crs = p4s)
  
  race_var_sources_tract <- 
    raster::extract( race_var_grid_exp_r,
                     census_acs_geo_mainland_year,
                     fun = mean,
                     weights = TRUE,
                     na.rm = TRUE
    ) %>% as.data.table()
  
  race_var_sources_tract_exp <- 
    cbind( census_acs_geo_mainland_year[,c( 'GEOID')],
           race_var_sources_tract)
  
  save( race_var_sources_tract_exp,
        file = paste0('tracts_race_', race_var, '_', census_year, '.RData'))
}







