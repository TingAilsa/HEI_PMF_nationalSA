library(base)
library(stringr)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)
library(sf)
library(lubridate)
library(timeDate) #holidayNYSE{}
library(fst)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(USAboundaries)
library(viridis)
# library(ggforce)
library(ggh4x)

# Function to calculate performance metrics
modeling_perform_metrics <- function(observed, predicted) {
  # Correlation coefficient
  r <- cor(observed, predicted)
  
  # Coefficient of determination (R squared)
  SS_tot <- sum((observed - mean(observed))^2)
  SS_res <- sum((observed - predicted)^2)
  R2 <- 1 - (SS_res/SS_tot)
  
  # Error metrics
  RMSE <- sqrt(mean((observed - predicted)^2))
  MAE <- mean(abs(observed - predicted))
  MB <- mean(predicted - observed)
  NMB <- mean(predicted - observed) / mean(observed) * 100
  
  return(list(
    r = r,
    R2 = R2,
    RMSE = RMSE,
    MAE = MAE,
    MB = MB,
    NMB = NMB
  ))
}

## Extract long & points with the continental US
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

#### Data estimates, uncertainty, overall, annual, monthly ####

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/Annual_combine")
# setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_Results/Aim3_prediction_data")
# setwd("/Users/ztttttt/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nationwide_SA/data/outputs/Aim3_inputs_predictions_10year_model")
getwd()


###### Combine single source averages to that of all sources ######
# Define the categories to combine
categories <- c("allinOne", "overall", "annual", "year-month", "month")

for (cat in categories) { # cat = categories[1]
  # Find all files matching the current category
  # This looks for files ending in e.g., "_overall.fst"
  pattern <- paste0("_Pred_US_01_", cat, "\\.fst$")
  cat_files <- list.files(pattern = pattern, full.names = TRUE)
  
  if (length(cat_files) > 0) {
    message("Combining ", length(cat_files), " files for category: ", cat)
    
    # Read all files in the category and stack them
    # as.data.table = TRUE ensures we keep the speed advantage
    combined_dt <- rbindlist(lapply(cat_files, read_fst, as.data.table = TRUE), use.names = TRUE)
    
    # Save the master file
    output_name <- paste0("Combined_US_01_", cat, ".fst")
    write_fst(combined_dt, output_name) #
    
    message("Saved: ", output_name)
    
    # Clean up memory before next category
    rm(combined_dt)
    gc()
  }
}

#### Plot Uncertainty & Concentration overall for all sources ####
overall_conc_unc = 
  read_fst("Combined_US_01_overall.fst", as.data.table = TRUE)

overall_source_conc <-
  daily_source_conc[, .(Pred.sd = mean(Pred.sd), 
                        Predictions = mean(Predictions), 
                        Uncertainty = mean(Uncertainty)), 
                    by = .(Model, Source, Dataset, Longitude, Latitude)]

overall_source_conc$Model_Data = 
  paste(overall_source_conc$Model, overall_source_conc$Source)

overall_conc_unc_long = 
  dplyr::select(overall_conc_unc, -Pred.sd, -Model, -Dataset) %>%
  pivot_longer(
    cols = Predictions:Uncertainty,
    names_to = "Variable",
    values_to = "Values"
  )

all_grid_predictions_one <-
  ggplot() +
  geom_tile(data = overall_conc_unc_long,
            aes(x = Longitude, y = Latitude, fill = Values),
            width = 0.1, height = 0.1) +  # Adjust width/height based on your grid resolution
  geom_sf(data = us_states,
          fill = NA, color = "grey70", linewidth = 0.3) +
  scale_fill_viridis_c(
    name = "Avg. Concentration", 
    limits = c(0, quantile(grid_predictions_avg$Values, 0.95)),
    oob = scales::squish,
    option = source_fill_col) +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude", 
       y = "Latitude", 
       title = paste("Average Prediction Across the US via", 
                     used_model, data_coords, cmaq_period, "of", used_source)) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 19, angle = 90, vjust = 0.5),
    plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )







## Parameter settings
pred_sources = c("Sulfate", "Traffic", "Dust", "Biomass", "Nitrate", "PM25") # , "Traffic", "Dust"
applied_model_data = c("RF", "GBM", "CSN")

# Define the years you want to include (can be 2 to 10 years)
cmaq_years = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020) 
# Create period strings for each year
cmaq_periods = paste0(cmaq_years, "-01_", cmaq_years, "-12")

# Create strings for titles and file names
included_years = paste(cmaq_years, collapse = "&")
included_year_title = paste(min(cmaq_years), max(cmaq_years), sep = "-")

cat("Study periods:", paste(cmaq_periods, collapse = ", "), "\n")
cat("Included years:", included_years, "\n")

for(used_model in applied_model_data) { # used_model = applied_model_data[3]
    
  # Get the dataset
  if(used_model == "CSN") { 
    dataset = "CSN" 
    used_model = "RF"
    } else { 
      dataset = "Both"
      used_model = used_model
      }
  
    cat("Used Model:", used_model, "\n")
    cat("Used Data:", dataset, "\n")
    
    # # Define filled color
    # source_fill_col =
    #   case_when(
    #     used_source == "Sulfate" ~ "magma",
    #     used_source == "Traffic" ~ "viridis",
    #     used_source == "Dust" ~ "cividis",
    #     used_source == "Biomass" ~ "plasma"
    #   )
    
    n_years = length(cmaq_years)
    n_source = length(pred_sources)
    cat("Total included years:", n_years, "\n")
    
    
    ###### Overall distribution, data preparation ###### 
    # Create a list to store all the data frames
    pred_ml_file_list = list()
    pred_ml_annual_list = list()
    pred_ml_annual_month_list = list()
    
    ## Loop through each year and read in the data
    for (i in 1:n_years * n_source) {
      
      # i = 24;  i %/% n_source; i %/% n_years
      year = cmaq_years[i %/% n_source]
      period = cmaq_periods[i %/% n_source]
      checked_source = pred_sources[i %/% n_years]
      cat("Reading file for year & source:", year, period, checked_source, "\n")
      
      ## Generate file name & read fst
      pred_ml_file_name = 
        paste0(used_model, "_Pred_US_01_Daily_mean_", 
               checked_source, "_", year, ".fst")
      
      ## Read file and selected columns to use
      pred_ml_file = read_fst(pred_ml_file_name)
      pred_ml_file = 
        dplyr::select(pred_ml_file, 
                      Date, Longitude, Latitude, SiteCode,
                      CMAQ_conc, PMF_conc, Predictions, month)
      pred_ml_file$Year = year
      pred_ml_file = 
        plyr::rename(pred_ml_file, c("month" = "Month"))
      pred_ml_file$Source = checked_source
      
      ## Generate annual averages and medians
      pred_annual_site =
        pred_ml_file %>%
        dplyr::group_by(Source, Longitude, Latitude, SiteCode, Year) %>%
        dplyr::summarise(
          
          # CMAQ
          CMAQ_mean = round(mean(CMAQ_conc), 2), 
          CMAQ_sd = round(sd(CMAQ_conc), 2),
          CMAQ_median = median(CMAQ_conc), 
          CMAQ_995th = quantile(CMAQ_conc, 0.995),
          CMAQ_005th = quantile(CMAQ_conc, 0.005),
          
          # PMF
          PMF_mean = round(mean(PMF_conc), 2), 
          PMF_sd = round(sd(PMF_conc), 2),
          PMF_median = median(PMF_conc), 
          PMF_995th = quantile(PMF_conc, 0.995),
          PMF_005th = quantile(PMF_conc, 0.005),
          
          # ML predictions
          Predictions_mean = round(mean(Predictions), 2), 
          Predictions_sd = round(sd(Predictions), 2),
          Predictions_median = median(Predictions), 
          Predictions_995th = quantile(Predictions, 0.995),
          Predictions_005th = quantile(Predictions, 0.005),
          
          .groups = "drop"
        )
      
      ## Generate annual-monthly averages and medians
      pred_annual_month_site =
        pred_ml_file %>%
        dplyr::group_by(Source, Longitude, Latitude, SiteCode, Year, Month) %>%
        dplyr::summarise(
          
          # CMAQ
          CMAQ_mean = round(mean(CMAQ_conc), 2), 
          CMAQ_sd = round(sd(CMAQ_conc), 2),
          CMAQ_median = median(CMAQ_conc), 
          CMAQ_995th = quantile(CMAQ_conc, 0.995),
          CMAQ_005th = quantile(CMAQ_conc, 0.005),
          
          # PMF
          PMF_mean = round(mean(PMF_conc), 2), 
          PMF_sd = round(sd(PMF_conc), 2),
          PMF_median = median(PMF_conc), 
          PMF_995th = quantile(PMF_conc, 0.995),
          PMF_005th = quantile(PMF_conc, 0.005),
          
          # ML predictions
          Predictions_mean = round(mean(Predictions), 2), 
          Predictions_sd = round(sd(Predictions), 2),
          Predictions_median = median(Predictions), 
          Predictions_995th = quantile(Predictions, 0.995),
          Predictions_005th = quantile(Predictions, 0.005),
          .groups = "drop"
        )
      
      # Save files into list
      pred_ml_file_list[[i]] = pred_ml_file
      pred_ml_annual_list[[i]] = pred_annual_site
      pred_ml_annual_month_list[[i]] = pred_annual_month_site
      
    }
    
    # Combine all data frames
    combined_pred_ml_daily = rbindlist(pred_ml_file_list)
    combined_pred_ml_month = rbindlist(pred_ml_annual_month_list)
    combined_pred_ml_annual = rbindlist(pred_ml_annual_list)
    
    head(combined_pred_ml_daily); dim(combined_pred_ml_daily)
    head(combined_pred_ml_month); dim(combined_pred_ml_month)
    head(combined_pred_ml_annual); dim(combined_pred_ml_annual)
    
    # Save files
    write_fst(combined_pred_ml_daily,
              paste0(used_model, "_", dataset, "_Pred_US_01_", 
                     included_year_title, "_", "all_source_Daily.fst"))
    
    write_fst(combined_pred_ml_month,
              paste0(used_model, "_", dataset, "_Pred_US_01_", 
                     included_year_title, "_", "all_source_Monthly.fst"))
    
    write_fst(combined_pred_ml_annual,
              paste0(used_model, "_", dataset, "_Pred_US_01_", 
                     included_year_title, "_", "all_source_Annual.fst"))
    
    ###### Plotting ###### 
    
    combined_pred_ml_daily = 
      read_fst(paste0(used_model, "_", dataset, "_Pred_US_01_", 
                      included_year_title, "_", "all_source_Daily.fst"))
    
    combined_pred_ml_month = 
      read_fst(paste0(used_model, "_", dataset, "_Pred_US_01_", 
                      included_year_title, "_", "all_source_Monthly.fst"))
    
    combined_pred_ml_annual = 
      read_fst(paste0(used_model, "_", dataset, "_Pred_US_01_", 
                      included_year_title, "_", "all_source_Annual.fst"))
    
    ##### Annual
    # Create labels for facets with mean and SD information
    year_labels = setNames(
      paste0(annual_site_summary$Year, "\nMean: ", 
             annual_site_summary$Predictions_mean, 
             " ± SD: ", annual_site_summary$Predictions_sd),
      annual_site_summary$Year
    )
    
    annual_source_box <-
      ggplot() +
      geom_boxplot(combined_pred_ml_daily,
                   aes(x = factor(Year), y = Predictions)) +
      geom_line(annual_site_summary,
                aes(x = factor(Year), y = PMF_mean)) +
      facet_wrap(Year ~ Source) + # scales = "free_y", ncol = 4,
      # scale_fill_manual(values = color_source_noF) +
      # scale_x_continuous(breaks = 1:12) +
      # scale_x_discrete(labels = month.abb) +  # Use month abbreviations instead of numbers
      scale_y_continuous(limits = c(0, NA),
                         breaks = function(x) pretty(x, n = 3)) +
      labs(
        x = "Year",
        y = "Predicted Concentrations",
        title = "RF Predicted concentrations by year and by source"
      ) +
      theme_minimal(base_size = 36) + 
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 1)  # Angled text for better readability
      )
    
    # Output figures
    ggsave(
      file.path("ML_plot",
                paste0(used_model, "_Pred_US_01_", 
                       included_year_title, "_", "All_source_box.png")),
      plot = annual_source_box, 
      width = min(16, 5 + n_years * 1.5),  # Increased width to accommodate larger labels
      height = min(10, 3 + n_source * 3)) # Increased height slightly
}


