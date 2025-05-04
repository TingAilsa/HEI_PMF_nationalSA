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


#### Pre-setting #### 

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/Annual_combine")
# setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_Results/Aim3_prediction_data")
getwd()

## Extract long & points with the continental US
library(USAboundaries)
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

## Parameter settings
pred_sources = c("Sulfate", "Traffic", "Dust", "Biomass") # , "Traffic", "Dust"
# pred_sources = c("Traffic") # , "Traffic", "Dust"

## CMAQ/Study Period
# cmaq_period = "2011-01_2011-12"; cmaq_year = 2011
# cmaq_period = "2012-01_2012-12"; cmaq_year = 2012
# cmaq_period = "2013-01_2013-12"; cmaq_year = 2013
cmaq_period = "2014-01_2014-12"; cmaq_year = 2014
# cmaq_period = "2015-01_2015-12"; cmaq_year = 2015
# cmaq_period = "2016-01_2016-12"; cmaq_year = 2016
# cmaq_period = "2017-01_2017-12"; cmaq_year = 2017

# applied_model = c("RF", "GBM")
applied_model = c("RF")
dataset = c("noCoords_") # "", "No_coords_"

print(paste0("Study period ", cmaq_period, " & Year ", cmaq_year, 
             ", with df ", dataset, " from model ", applied_model))

#### Mapping & other plotting, a single year #### 
# used_model = applied_model[1]; used_source = pred_sources[4]; used_data = dataset[1]

for(used_model in applied_model){ # used_model = applied_model[1]
  for(used_source in pred_sources){ # used_source = pred_sources[1]
    for(used_data in dataset){ # used_data = dataset[1]
      
      # Get used data
      data_coords = ifelse(used_data == "", "With-coords", "No-coords")
      print(data_coords)
      
      # Define filled color  
      source_fill_col =
        case_when(
          used_source == "Sulfate" ~ "magma",
          used_source == "Traffic" ~ "viridis",
          used_source == "Dust" ~ "cividis",
          used_source == "Biomass" ~ "plasma"
        )
      print(source_fill_col)
      
      ####### Overall distribution ####### 
      
      # Generate file name & read fst
      fst_allpred_name = 
        paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", used_data, used_source, "_", cmaq_period, ".fst")
      
      # fst_allpred_name = "/Users/TingZhang/Dropbox/HEI_US_PMF/Aim3_fine-scale_results/RF_overall_No_coords_Traffic_2011-01_2011-12.fst"
      
      print(fst_allpred_name)
      all_pred_fst = read_fst(fst_allpred_name)
      
      # Rename
      all_pred_fst = plyr::rename(all_pred_fst, 
                                  c("month" = "Month"))
      # all_pred_fst = plyr::rename(all_pred_fst, 
      #                             c("rf_predictions" = "Predictions"))
      head(all_pred_fst); dim(all_pred_fst)
      # summary(all_pred_fst)
      
      ###### Overall-1: Annual and monthly distribution, based on data from all grids ######
      
      # Get monthly average and whole study period average
      grid_predictions_avg = 
        all_pred_fst %>%
        group_by(Longitude, Latitude) %>%
        dplyr::summarize(
          mean_predction = mean(Predictions, na.rm = TRUE),
          PMF_conc = mean(PMF_conc, na.rm = TRUE),
          .groups = "drop"
        )
      head(grid_predictions_avg); dim(grid_predictions_avg)
      
      month_predictions_avg = 
        all_pred_fst %>%
        group_by(Longitude, Latitude, Month) %>%
        dplyr::summarize(
          mean_predction = mean(Predictions, na.rm = TRUE),
          PMF_conc = mean(PMF_conc, na.rm = TRUE),
          .groups = "drop"
        )
      head(month_predictions_avg); dim(month_predictions_avg)
      
      
      all_grid_predictions_one <-
        ggplot() +
        geom_tile(data = grid_predictions_avg,
                  aes(x = Longitude, y = Latitude, fill = mean_predction),
                  width = 0.1, height = 0.1) +  # Adjust width/height based on your grid resolution
        geom_sf(data = us_states,
                fill = NA, color = "grey70", linewidth = 0.3) +
        scale_fill_viridis_c(
          name = "Avg. Concentration", 
          limits = c(0, quantile(grid_predictions_avg$mean_predction, 0.95)),
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
      
      # all_grid_predictions_one
      
      month_grid_predictions_plot <-
        ggplot() +
        geom_tile(data = month_predictions_avg,
                  aes(x = Longitude, y = Latitude, fill = mean_predction),
                  width = 0.1, height = 0.1, color = NA) + 
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", linewidth = 0.3) + 
        facet_wrap(~Month, ncol = 4) + 
        scale_fill_viridis_c(
          name = "Avg. Concentration", 
          limits = c(0, quantile(grid_predictions_avg$mean_predction, 0.95)),
          oob = scales::squish,
          option = source_fill_col) +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 16) +
        labs(x = "Longitude", 
             y = "Latitude", 
             title = paste("Monthly Prediction Across the US via", 
                           used_model, data_coords, cmaq_period, "of", used_source)) +
        theme(
          legend.position = "bottom",
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 15, angle = 90, vjust = 0.5),
          plot.title = element_text(size = 18, face = "bold", vjust = 1.2),
          # plot.subtitle = element_text(size = 18),
          strip.text = element_text(size = 15, face = "bold", color = "grey25"),  # Adjust facet label size
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank()
        )
      # month_grid_predictions_plot
      
      # Output figures
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", cmaq_period, "_", used_source, "_Overall.png")), 
        plot = all_grid_predictions_one, width = 14.5, height = 8.5)

      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", cmaq_period, "_", used_source, "_Monthly.png")), 
        plot = month_grid_predictions_plot, width = 14.5, height = 8.5)
      
      ###### Overall-2: One day result ######
      
      #### Plotting
      select_day = unique(all_pred_fst$Date)[18]
      select_one_day_data = 
        subset(all_pred_fst, Date == select_day)
      
      one_day_prediction_plot <-
        ggplot() +
        geom_tile(data = select_one_day_data, 
                  aes(x = Longitude, y = Latitude, fill = Predictions),
                  width = 0.1, height = 0.1, color = NA) + 
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", linewidth = 0.3) + 
        scale_fill_viridis_c(
          name = "Daily Concentration", 
          limits = c(0, quantile(select_one_day_data$Predictions, 0.95)),
          oob = scales::squish,
          option = source_fill_col) +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 16) +
        labs(x = "Longitude", 
             y = "Latitude", 
             title = paste0("One-day Prediction (", select_day, ") via ", used_model)) +
        theme(
          legend.position = "bottom",
          # strip.text = element_text(size = 21),
          legend.title = element_text(size = 21),
          legend.text = element_text(size = 18, angle = 90, vjust = 0.5),
          plot.title = element_text(size = 21, face = "bold", vjust = 1.2),
          # plot.subtitle = element_text(size = 22),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank()
        )
      # one_day_prediction_plot
      
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", select_day, "_", used_source, "_Daily.png")), 
        plot = one_day_prediction_plot, width = 14.5, height = 8.5)
      
      ###### Model Performance ###### 
      
      ###### Performance-1, iteration, train & test results ######

      # Predictions for train and test data from each iterations
      train_test_pred_name = 
        paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", used_data, 
               "train_test_", used_source, "_", cmaq_period, ".fst")
      print(paste0("train_test_pred_name: ", train_test_pred_name))
      train_test_pred_fst = read_fst(train_test_pred_name)
      summary(train_test_pred_fst)
      
      # Calculate metrics for each monitoring site
      # train_test_pred_fst = subset(all_pred_fst, !is.na(PMF_conc))
      train_test_site_perform <- 
        train_test_pred_fst %>%
        dplyr::group_by(grid_ID) %>%
        dplyr::summarise(
          metrics = list(modeling_perform_metrics(PMF_conc, Predictions))
        ) %>%
        unnest_wider(metrics)
      head(train_test_site_perform)
      names(train_test_site_perform)[2:3] = c("R_Pearson", "R_Determine")
      
      train_test_site_pmf <-
        train_test_pred_fst %>%
        group_by(grid_ID) %>%
        dplyr::summarise(
          PMF_conc = median(PMF_conc), 
          .groups = "drop"
        )
      
      train_test_site_perform <-
        merge(train_test_site_perform, train_test_site_pmf)
      
      # Get to long and lat 
      train_test_site_perform <- 
        train_test_site_perform %>%
        mutate(
          Longitude = as.numeric(str_extract(grid_ID, "-?\\d+\\.\\d+")),
          Latitude = as.numeric(str_extract(grid_ID, "\\d+\\.\\d+$"))
        )
      
      print("Check the train_test_site_perform")
      summary(train_test_site_perform)
      dim(train_test_site_perform)
      
      ######## Plotting
      PMF_conc <-
        ggplot() +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", size = 0.3) + 
        geom_point(data = train_test_site_perform, 
                   aes(x = Longitude, y = Latitude, color = PMF_conc),
                   size = 1.2, alpha = 0.8) +
        scale_color_viridis_c(
          name = "PMF_conc", option = "plasma",
          limits = c(0, quantile(train_test_site_perform$PMF_conc, 0.995)),
          oob = scales::squish
        ) +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 14) +
        labs(title = "PMF_conc") +
        theme(
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14, angle = 90, vjust = 0.5),
          plot.title = element_text(size = 16, face = "bold", vjust = 1.2),
          axis.title = element_blank(),
          axis.text = element_blank()
        )
      # PMF_conc
      
      # Output figures
      # ggsave(
      #   file.path("ML_plot",
      #             paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", cmaq_period, "_", used_source, "_PMF_conc.png")), 
      #   plot = PMF_conc, width = 12, height = 9)
      
      
      perform_NMB <-
        ggplot() +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", linewidth = 0.3) + 
        geom_point(data = train_test_site_perform, 
                   aes(x = Longitude, y = Latitude, color = NMB),
                   size = 1.2, alpha = 0.8) +
        scale_color_viridis_c(
          name = "NMB", option = "viridis",
          limits = c(0, quantile(train_test_site_perform$NMB, 0.95))) +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 14) +
        labs(title = "NMB") +
        theme(
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14, angle = 90, vjust = 0.5),
          plot.title = element_text(size = 16, face = "bold", vjust = 1.2),
          axis.title = element_blank(),
          axis.text = element_blank()
        )
      # perform_NMB
      
      perform_RMSE <-
        ggplot() +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", linewidth = 0.3) + 
        geom_point(data = train_test_site_perform, 
                   aes(x = Longitude, y = Latitude, color = RMSE),
                   size = 1.2, alpha = 0.8) +
        scale_color_viridis_c(
          name = "RMSE", option = "plasma",
          limits = c(0, quantile(train_test_site_perform$RMSE, 0.95)),
          oob = scales::squish
        ) +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 14) +
        labs(title = "RMSE") +
        theme(
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14, angle = 90, vjust = 0.5),
          plot.title = element_text(size = 16, face = "bold", vjust = 1.2),
          axis.title = element_blank(),
          axis.text = element_blank()
        )
      # perform_RMSE
      
      perform_R_determine <-
        ggplot() +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", linewidth = 0.3) + 
        geom_point(data = train_test_site_perform, 
                   aes(x = Longitude, y = Latitude, color = R_Determine),
                  size = 1.2, alpha = 0.8) +
        scale_color_viridis_c(
          name = "R_determine", option = "viridis",
          limits = c(0, quantile(train_test_site_perform$R_Determine, 0.95))) +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 14) +
        labs(title = "R_determine") +
        theme(
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14, angle = 90, vjust = 0.5),
          plot.title = element_text(size = 16, face = "bold", vjust = 1.2),
          axis.title = element_blank(),
          axis.text = element_blank()
        )
      # perform_R_determine
      
      perform_R_Pearson <-
        ggplot() +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", linewidth = 0.3) + 
        geom_point(data = train_test_site_perform, 
                   aes(x = Longitude, y = Latitude, color = R_Pearson),
                  width = 0.5, height = 0.5, 
                  alpha = 0.8) +
        scale_color_viridis_c(
          name = "R_Pearson", option = "viridis",
          limits = c(0, quantile(train_test_site_perform$R_Pearson, 0.95))) +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 14) +
        labs(title = "R_Pearson") +
        theme(
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14, angle = 90, vjust = 0.5),
          plot.title = element_text(size = 16, face = "bold", vjust = 1.2),
          axis.title = element_blank(),
          axis.text = element_blank()
        )
      # perform_R_Pearson
      
      title_text = paste(used_model, "based on", data_coords, "during", cmaq_period, "of", used_source)
      
      perform_overall_plot = 
        (perform_NMB + PMF_conc)/(perform_R_Pearson + perform_RMSE) +
        plot_annotation(title = title_text,
                        theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
      # perform_overall_plot
      
      # Output figures
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", cmaq_period, "_", used_source, "_model_performance.png")), 
        plot = perform_overall_plot, width = 12, height = 9)
      
    
      # calculate the limits based on the range of the data
      max_rf <- 
        max(c(train_test_pred_fst$PMF_conc, 
              train_test_pred_fst$Predictions), 
            na.rm = TRUE)
      min_rf <-
        min(c(train_test_pred_fst$PMF_conc, 
              train_test_pred_fst$Predictions), 
            na.rm = TRUE)
      max_rf; min_rf
      
      pmf_vs_predict <-
        ggplot(data = train_test_pred_fst, 
               aes(x = PMF_conc, y = Predictions)) +
        geom_point(color = "grey35", alpha = 0.6, shape = 1) +
        coord_equal() + # enforces an equal scaling between the axes
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "forestgreen", size = 1) +
        geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid") +
        labs(title = "PMF vs. ML Predictions",
             x = "PMF concentration µg/m^3",
             y = "ML prediction µg/m^3") +
        theme_minimal(base_size = 24) +
        xlim(min_rf, max_rf) +
        ylim(min_rf, max_rf)
      # pmf_vs_predict
      
      # cmaq_vs_predict <-
      #   ggplot(data = train_test_pred_fst, 
      #          aes(x = CMAQ_conc, y = Predictions)) +
      #   geom_point(color = "grey35", alpha = 0.6, shape = 1) +
      #   coord_equal() + # enforces an equal scaling between the axes
      #   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "forestgreen", size = 1) +
      #   geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid") +
      #   labs(title = "CMAQ vs. ML Predictions",
      #        x = "CMAQ concentration µg/m^3",
      #        y = "ML prediction µg/m^3") +
      #   theme_minimal(base_size = 24) +
      #   xlim(min_rf, max_rf) +
      #   ylim(min_rf, max_rf)
      # # cmaq_vs_predict
      
      # pmf_cmaq_pred = pmf_vs_predict + cmaq_vs_predict
      
      # Output figures
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", cmaq_period, "_", used_source, "_PMF_CMAQ_ML_scatter.png")), 
        plot = pmf_vs_predict, width = 6, height = 6)
      
      ###### Performance-2, iteration, predictor importance ######
      
      # Predictor importance from each iterations
      predictor_perform_name = 
        paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", used_data, 
               "predictor_perform_", used_source, "_", cmaq_period, ".fst")
      print(paste0("train_test_pred_fst: ", predictor_perform_name))
      predictor_perform_fst = read_fst(predictor_perform_name)
      summary(predictor_perform_fst)
      
      # The mean influence level
      predictor_perform_fst_avg = 
        predictor_perform_fst %>%
        group_by(Variable) %>%
        dplyr::summarise(
          Relative_Influence_med = median(Relative_Influence), 
          Relative_Influence_mean = mean(Relative_Influence))
      
      # Plot of predictor influence
      rf_var_influence_p <- 
        ggplot(predictor_perform_fst_avg, 
               aes(x = reorder(Variable, Relative_Influence_mean), 
                   y = Relative_Influence_mean)) +
        geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
        coord_flip() +
        labs(title = "Variable Performance",
             x = "Predictor",
             y = "Relative Influence (%)") +
        theme_minimal(base_size = 16)
      
      # Save plot
      ggsave(
        plot = rf_var_influence_p, 
        file.path("ML_plot",
             paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_",
                    cmaq_period, "_", used_source, "_variable_Influence.png")), 
             width = 9, height = 10.5)
      
    }
  }
}


#### Plot, comparison among different years (flexible number of years) ####

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/Annual_combine")
# setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_Results/Aim3_prediction_data")
getwd()

## Parameter settings
pred_sources = c("Sulfate", "Traffic", "Dust", "Biomass")
applied_model = c("RF")
dataset = c("noCoords_")

# Define the years you want to include (can be 2 to 10 years)
cmaq_years = c(2011, 2012, 2013, 2014, 2015, 2016, 2017) 
# Create period strings for each year
cmaq_periods = paste0(cmaq_years, "-01_", cmaq_years, "-12")

# Create strings for titles and file names
included_years = paste(cmaq_years, collapse = "&")
included_year_title = paste(min(cmaq_years), max(cmaq_years), sep = "-")

print(paste("Study periods:", paste(cmaq_periods, collapse = ", ")))
print(paste("Included years:", included_years))
print(paste0("Study datasets, ", dataset, "; from applied model, ", applied_model))

# used_model = applied_model[1]; used_source = pred_sources[1]; used_data = dataset[1]

###### Mapping, based on overall predictions (includes grids without monitoring) ###### 
for(used_model in applied_model) {
  for(used_source in pred_sources) {
    for(used_data in dataset) {
      
      # Get used data
      data_coords = ifelse(used_data == "", "With-coords", "No-coords")

      # Define filled color  
      source_fill_col =
        case_when(
          used_source == "Sulfate" ~ "magma",
          used_source == "Traffic" ~ "viridis",
          used_source == "Dust" ~ "cividis",
          used_source == "Biomass" ~ "plasma"
        )
      print(data_coords); print(used_source); print(source_fill_col)
      
      ###### Overall distribution ###### 
      
      # Create a list to store all the data frames
      pred_fst_list = list()
      year_stats = data.frame(
        Year = integer(),
        Mean = numeric(),
        SD = numeric(),
        stringsAsFactors = FALSE
      )
      
      # Loop through each year and read in the data
      for(i in 1:length(cmaq_years)) {
        year = cmaq_years[i]
        period = cmaq_periods[i]
        
        # Generate file name & read fst
        fst_allpred_name = 
          paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", used_data, 
                 used_source, "_", period, ".fst")
        
        pred_fst = read_fst(fst_allpred_name)
        pred_fst = 
          dplyr::select(pred_fst, 
                        Date, Longitude, Latitude, SiteCode,
                        CMAQ_conc, PMF_conc, Predictions, month)
        pred_fst$Year = year
        
        # Calculate and store mean and SD for this year
        mean_val = mean(pred_fst$Predictions, na.rm = TRUE)
        sd_val = sd(pred_fst$Predictions, na.rm = TRUE)
        
        year_stats = rbind(year_stats, data.frame(
          Year = year,
          Mean = mean_val,
          SD = sd_val
        ))
        
        pred_fst_list[[i]] = pred_fst
        
        print(paste("Reading file for year", year))
        print(paste("Mean:", mean_val, "SD:", sd_val))
      }
      
      # Combine all data frames
      combined_pred_fst = rbindlist(pred_fst_list)      
      # Rename
      combined_pred_fst = plyr::rename(combined_pred_fst, c("month" = "Month"))
      
      write_fst(combined_pred_fst, 
                paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", used_data, 
                       used_source, "_", included_year_title, "_Daily.fst"))
      
      ### Get annual and monthly averages
      # Annual average
      combined_grid_predictions_avg = 
        combined_pred_fst %>%
        dplyr::group_by(Year, Longitude, Latitude) %>%
        dplyr::summarize(
          
          # CMAQ
          CMAQ_mean = round(mean(CMAQ_conc), 2), 
          CMAQ_sd = round(sd(CMAQ_conc), 2),
          CMAQ_median = median(CMAQ_conc), 
          CMAQ_995th = quantile(CMAQ_conc, 0.995),
          CMAQ_005th = quantile(CMAQ_conc, 0.005),
          
          # PMF
          PMF_mean = round(mean(PMF_conc, na.rm = TRUE), 2), 
          PMF_sd = round(sd(PMF_conc, na.rm = TRUE), 2),
          PMF_median = median(PMF_conc, na.rm = TRUE), 
          PMF_995th = quantile(PMF_conc, na.rm = TRUE, 0.995),
          PMF_005th = quantile(PMF_conc, na.rm = TRUE, 0.005),
          
          # ML predictions
          Predictions_mean = round(mean(Predictions), 2), 
          Predictions_sd = round(sd(Predictions), 2),
          Predictions_median = median(Predictions), 
          Predictions_995th = quantile(Predictions, 0.995),
          Predictions_005th = quantile(Predictions, 0.005),
          
          .groups = "drop"
        )
      
      # Monthly average
      combined_month_predictions_avg = 
        combined_pred_fst %>%
        dplyr::group_by(Year, Longitude, Latitude, Month) %>%
        dplyr::summarize(
          
          # CMAQ
          CMAQ_mean = round(mean(CMAQ_conc), 2), 
          CMAQ_sd = round(sd(CMAQ_conc), 2),
          CMAQ_median = median(CMAQ_conc), 
          CMAQ_995th = quantile(CMAQ_conc, 0.995),
          CMAQ_005th = quantile(CMAQ_conc, 0.005),
          
          # PMF
          PMF_mean = round(mean(PMF_conc, na.rm = TRUE), 2), 
          PMF_sd = round(sd(PMF_conc, na.rm = TRUE), 2),
          PMF_median = median(PMF_conc, na.rm = TRUE), 
          PMF_995th = quantile(PMF_conc, na.rm = TRUE, 0.995),
          PMF_005th = quantile(PMF_conc, na.rm = TRUE, 0.005),
          
          # ML predictions
          Predictions_mean = round(mean(Predictions), 2), 
          Predictions_sd = round(sd(Predictions), 2),
          Predictions_median = median(Predictions), 
          Predictions_995th = quantile(Predictions, 0.995),
          Predictions_005th = quantile(Predictions, 0.005),

          .groups = "drop"
        )
      
      write_fst(combined_grid_predictions_avg, 
                paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", used_data, 
                       used_source, "_", included_year_title, "_Annual.fst"))
      
      write_fst(combined_month_predictions_avg, 
                paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", used_data, 
                       used_source, "_", included_year_title, "_Monthly.fst"))
      
      # Choose select month for different sources to compare
      plot_months = c(1, 4, 7, 10)
      # plot_months <- case_when(
      #   used_source == "Sulfate" ~ c(1, 7, 8), 
      #   used_source %in% c("Traffic", "Dust", "Biomass") ~ c(4, 7, 12) 
      # )
      print("Plot months for the source:")
      print(plot_months)
      
      #### Plotting
      ######## Annual ########  
      # Adjust facet wrap based on number of years
      n_years = length(cmaq_years)
      print(paste("included years:", n_years))
      n_cols = min(3, n_years)  # Maximum 3 columns
      
      # Round the mean and SD to 2 decimal places
      year_stats$Mean_rounded = round(year_stats$Mean, 2)
      year_stats$SD_rounded = round(year_stats$SD, 2)
      
      # Create labels for facets with mean and SD information
      year_labels = setNames(
        paste0(year_stats$Year, "\nMean: ", year_stats$Mean_rounded, 
               " ± SD: ", year_stats$SD_rounded),
        year_stats$Year
      )
      
      annual_compare <-
        ggplot() +
        geom_tile(data = combined_grid_predictions_avg, 
                  aes(x = Longitude, y = Latitude, fill = Predictions_mean),
                  width = 0.1, height = 0.1, color = NA) + 
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", linewidth = 0.3) + 
        scale_fill_viridis_c(
          name = "Avg. Concentration", 
          limits = c(0, quantile(combined_grid_predictions_avg$Predictions_mean, 0.95)),
          oob = scales::squish,
          option = source_fill_col) +
        facet_wrap(. ~ Year, ncol = n_cols, labeller = labeller(Year = year_labels)) +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 16) +
        labs(x = "Longitude", 
             y = "Latitude", 
             title = paste("Comparison in Prediction across the US via", 
                           used_model, data_coords, "between", included_year_title, "of", used_source)) +
        theme(
          legend.position = "bottom",
          strip.text = element_text(size = 18),  # Reduced size to fit more text
          legend.title = element_text(size = 21),
          legend.text = element_text(size = 18, angle = 90, vjust = 0.5),
          plot.title = element_text(size = 21, face = "bold", vjust = 1.2),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank()
        )
      
      ######## Monthly ########  
      # Calculate mean and SD for each year-month combination
      # month_stats <- 
      #   combined_month_predictions_avg %>%
      #   dplyr::group_by(Year, Month) %>%
      #   dplyr::summarize(
      #     Mean = mean(Predictions_mean, na.rm = TRUE),
      #     SD = sd(Predictions_mean, na.rm = TRUE),
      #     .groups = "drop"
      #   )
      # 
      # # Round to 2 decimal places
      # month_stats$Mean_rounded = round(month_stats$Mean, 2)
      # month_stats$SD_rounded = round(month_stats$SD, 2)
      # 
      # # Create a lookup function for facet labels
      # get_month_label <- function(year, month) {
      #   stats <- month_stats[month_stats$Year == year & month_stats$Month == month, ]
      #   if (nrow(stats) > 0) {
      #     return(paste0("Month: ", month, "\nMean: ", stats$Mean_rounded[1], 
      #                   " ± SD: ", stats$SD_rounded[1]))
      #   } else {
      #     return(paste0("Month: ", month))
      #   }
      # }
      # 
      # # Create month labels data frame
      # month_labels <- expand.grid(Year = unique(month_data$Year), Month = unique(month_data$Month))
      # month_labels$label <- mapply(get_month_label, month_labels$Year, month_labels$Month)
      # 
      # # Convert to named vector for facet labeller
      # month_label_vector <- setNames(
      #   month_labels$label,
      #   paste(month_labels$Year, month_labels$Month, sep = "_")
      # )
      # 
      # # Custom labeller function for facet_grid
      # custom_labeller <- function(variable, value) {
      #   if (variable == "Year") {
      #     return(year_labels[as.character(value)])
      #   } else if (variable == "Month") {
      #     month_names <- c("1" = "Jan", "4" = "Apr", "7" = "Jul", "8" = "Aug", "12" = "Dec")
      #     month_val <- month_names[as.character(value)]
      #     return(paste("Month:", month_val))
      #   }
      #   return(value)
      # }
      
      select_month_compare <-
        ggplot() +
        geom_tile(data = subset(combined_month_predictions_avg, 
                                Month %in% plot_months), 
                  aes(x = Longitude, y = Latitude, 
                      fill = Predictions_mean),
                  width = 0.1, height = 0.1) + 
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", linewidth = 0.3) + 
        facet_grid(Year ~ Month, labeller = labeller(Year = year_labels)) +
        scale_fill_viridis_c(
          name = "Avg. Concentration", 
          limits = c(0, quantile(combined_month_predictions_avg$Predictions_mean, 0.95)),
          oob = scales::squish,
          option = source_fill_col) +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 16) +
        labs(x = "Longitude", 
             y = "Latitude", 
             title = paste("Selected Month Prediction Comparison via", 
                           used_model, data_coords, "between", included_year_title, "of", used_source)) +
        theme(
          legend.position = "bottom",
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 15, angle = 90, vjust = 0.5),
          plot.title = element_text(size = 18, face = "bold", vjust = 1.2),
          strip.text = element_text(size = 13, face = "bold", color = "grey25"),  # Reduced size for more text
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank()
        )
      
      # Output figures
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", 
                         included_year_title, "_", used_source, "_compare_annual.png")),
        plot = annual_compare, 
        width = min(16, 5 + n_cols * 4),  # Increased width to accommodate larger labels
        height = min(10, 3 + ceiling(n_years/n_cols) * 3)) # Increased height slightly
      
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", 
                         included_year_title, "_", used_source, "_compare_month.png")), 
        plot = select_month_compare, 
        width = 16,  # Increased width
        height = min(16, 5 + n_years * 3)) 
    }
  }
}

#### Plot, all years, all sources ####

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/Annual_combine")
# setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_Results/Aim3_prediction_data")
getwd()


## Parameter settings
pred_sources = c("Sulfate", "Traffic", "Dust", "Biomass")
applied_model = c("RF")
dataset = c("noCoords_")

# Define the years you want to include (can be 2 to 10 years)
cmaq_years = c(2011, 2012, 2013, 2014, 2015, 2016, 2017) 
# Create period strings for each year
cmaq_periods = paste0(cmaq_years, "-01_", cmaq_years, "-12")

# Create strings for titles and file names
included_years = paste(cmaq_years, collapse = "&")
included_year_title = paste(min(cmaq_years), max(cmaq_years), sep = "-")

print(paste("Study periods:", paste(cmaq_periods, collapse = ", ")))
print(paste("Included years:", included_years))
print(paste0("Study datasets, ", dataset, "; from applied model, ", applied_model))

used_model = applied_model[1]; used_data = dataset[1]

for(used_model in applied_model) {
  for(used_data in dataset) {
    
    # Get used data
    data_coords = ifelse(used_data == "", "With-coords", "No-coords")
    print(data_coords); print(used_model)
    
    # Define filled color  
    # source_fill_col =
    #   case_when(
    #     used_source == "Sulfate" ~ "magma",
    #     used_source == "Traffic" ~ "viridis",
    #     used_source == "Dust" ~ "cividis",
    #     used_source == "Biomass" ~ "plasma"
    #   )
    
    n_years = length(cmaq_years)
    n_source = length(pred_sources)
    print(paste("Total included years:", n_years))
    
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
      print(paste("Reading file for year & source:", 
                  year, period, checked_source))
      
      ## Generate file name & read fst
      pred_ml_file_name = 
        paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", used_data, 
               checked_source, "_", period, ".fst")
      
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
          PMF_mean = round(mean(PMF_conc, na.rm = TRUE), 2), 
          PMF_sd = round(sd(PMF_conc, na.rm = TRUE), 2),
          PMF_median = median(PMF_conc, na.rm = TRUE), 
          PMF_995th = quantile(PMF_conc, na.rm = TRUE, 0.995),
          PMF_005th = quantile(PMF_conc, na.rm = TRUE, 0.005),
          
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
          PMF_mean = round(mean(PMF_conc, na.rm = TRUE), 2), 
          PMF_sd = round(sd(PMF_conc, na.rm = TRUE), 2),
          PMF_median = median(PMF_conc, na.rm = TRUE), 
          PMF_995th = quantile(PMF_conc, na.rm = TRUE, 0.995),
          PMF_005th = quantile(PMF_conc, na.rm = TRUE, 0.005),
          
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
              paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_",
                     included_year_title, "_", "all_source_year_Daily.fst"))
    
    write_fst(combined_pred_ml_month,
              paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", 
                     included_year_title, "_", "all_source_year_Monthly.fst"))
    
    write_fst(combined_pred_ml_annual,
              paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", 
                     included_year_title, "_", "all_source_year_Annual.fst"))
    
    ###### Plotting ###### 
    
    combined_pred_ml_daily = 
      read_fst(paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", 
                      included_year_title, "_", "all_source_year_Daily.fst"))
    
    combined_pred_ml_month = 
      read_fst(paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", 
                      included_year_title, "_", "all_source_year_Monthly.fst"))
    
    combined_pred_ml_annual = 
      read_fst(paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", 
                      included_year_title, "_", "all_source_year_Annual.fst"))
    
    ##### Annual
    # Create labels for facets with mean and SD information
    year_labels = setNames(
      paste0(annual_site_summary$Year, "\nMean: ", 
             annual_site_summary$Predictions_mean, 
             " ± SD: ", annual_site_summary$Predictions_mean),
      annual_site_summary$Year
    )
    
    all_source_year_box <-
      ggplot() +
      geom_boxplot(combined_pred_ml_daily,
                   aes(x = factor(Year), y = Predictions)) +
      geom_line(annual_site_summary,
                aes(x = factor(Year), y = PMF_median)) +
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
                paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords, "_", 
                       included_year_title, "_", "All_source_year_box.png")),
      plot = all_source_year_box, 
      width = min(16, 5 + n_years * 1.5),  # Increased width to accommodate larger labels
      height = min(10, 3 + n_source * 3)) # Increased height slightly
  }
}


