library(base)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)
library(sf)
library(lubridate)
library(timeDate) #holidayNYSE{}
library(fst)
library(ggplot2)
library(patchwork)
library(USAboundaries)
library(viridis)

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

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
getwd()

## Extract long & points with the continental US
library(USAboundaries)
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]


#### Plot for a single year ####

## Parameter settings
pred_sources = c("Secondary_Sulfate", "Traffic") # , "Traffic", "Dust"
# cmaq_period = "2011-01_2011-12"; cmaq_year = 2011
cmaq_period = "2017-01_2017-11"; cmaq_year = 2017
# applied_model = c("RF", "GBM")
applied_model = c("RF")
dataset = c("No_coords_") # "", 

print(paste0("Study period ", cmaq_period, " & Year ", cmaq_year, 
             ", with datasets ", dataset, " from applied model ", applied_model))

###### Mapping, based on overall predictions (includes grids without monitoring) ###### 
for(used_model in applied_model){ # used_model = applied_model[2]
  for(used_source in pred_sources){ # used_source = pred_sources[1]
    for(used_data in dataset){ # used_data = dataset[1]; used_data = dataset[2]
      
      # Generate file name & read fst
      fst_allpred_name = 
        paste0(used_model, "_overall_", used_data, used_source, "_", cmaq_period, ".fst")
      print(fst_allpred_name)
      all_pred_fst = read_fst(fst_allpred_name)
      
      # Get used data
      data_coords = ifelse(used_data == "", "With-coords", "No-coords")
      print(data_coords)
      
      # Change colname
      ifelse(used_model == "GBM", 
             all_pred_fst <-
               plyr::rename(all_pred_fst,
                            c("gbm_predictions" = "prediction")),
             all_pred_fst <-
               plyr::rename(all_pred_fst,
                            c("rf_predictions" = "prediction")))
      
      # Get monthly average and whold study period average
      grid_predictions_avg = 
        all_pred_fst %>%
        group_by(grid_ID, Longitude, Latitude) %>%
        dplyr::summarize(
          mean_predction = mean(prediction, na.rm = TRUE),
          PMF_conc = mean(PMF_conc, na.rm = TRUE),
          .groups = "drop"
        )
      head(grid_predictions_avg); dim(grid_predictions_avg)
      
      month_predictions_avg = 
        all_pred_fst %>%
        group_by(grid_ID, Longitude, Latitude, Month) %>%
        dplyr::summarize(
          mean_predction = mean(prediction, na.rm = TRUE),
          PMF_conc = mean(PMF_conc, na.rm = TRUE),
          .groups = "drop"
        )
      head(month_predictions_avg); dim(month_predictions_avg)
      
      # Define filled color  
      source_fill_col =
        case_when(
          used_source == "Secondary_Sulfate" ~ "plasma",
          used_source == "Traffic" ~ "viridis",
          used_source == "Dust" ~ "cividis"
        )
      print(source_fill_col)
      
      all_grid_predictions_one <-
        ggplot() +
        geom_point(data = grid_predictions_avg, 
                   aes(x = Longitude, y = Latitude, color = mean_predction),
                   size = 0.35, alpha = 0.8) +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", size = 0.3) + 
        scale_color_viridis_c(
          name = "Avg. Concentration", 
          limits = c(0, max(grid_predictions_avg$mean_predction)),
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
          # plot.subtitle = element_text(size = 22),
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
        geom_point(data = month_predictions_avg, 
                   aes(x = Longitude, y = Latitude, color = mean_predction),
                   size = 0.35, alpha = 0.8) +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", size = 0.3) + 
        facet_wrap(~Month, ncol = 4) + 
        scale_color_viridis_c(
          name = "Avg. Concentration", 
          limits = c(0, max(month_predictions_avg$mean_predction)),
          option = source_fill_col) +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 16) +
        labs(x = "Longitude", 
             y = "Latitude", 
             title = paste("Average Prediction Across the US via", 
                           used_model, data_coords, cmaq_period, "of", used_source)) +
        theme(
          legend.position = "bottom",
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 15, angle = 90, vjust = 0.5),
          plot.title = element_text(size = 18, face = "bold", vjust = 1.2),
          # plot.subtitle = element_text(size = 18),
          strip.text = element_text(size = 15, face = "bold", color = "grey25"),  # Adjust facet label size
          axis.title = element_text(size = 18),
          # plot.subtitle = element_text(size = 22),
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
                  paste0(used_model, "_", data_coords, "_", cmaq_period, "_", used_source, "_Overall.pdf")), 
        plot = all_grid_predictions_one, width = 14.5, height = 8.5)
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_", data_coords, "_", cmaq_period, "_", used_source, "_Monthly.pdf")), 
        plot = month_grid_predictions_plot, width = 14.5, height = 8.5)
    }
  }
}


###### Model performance, based on grids with monitoring and PMF results ###### 

for(used_model in applied_model){ # used_model = applied_model[2]
  for(used_source in pred_sources){ # used_source = pred_sources[1]
    for(used_data in dataset){ # used_data = dataset[1]; used_data = dataset[2]
      
      # Generate file name & read fst
      fst_pmf_cmaq_name = 
        paste0(used_model, "_SiteID_Uncertainty_", used_data, used_source, "_", cmaq_period, ".fst")
      print(fst_pmf_cmaq_name)
      pmf_cmaq_fst = read_fst(fst_pmf_cmaq_name)
      
      # Get used data
      data_coords = ifelse(used_data == "", "With-coords", "No-coords")
      print(data_coords)
      summary(pmf_cmaq_fst); head(pmf_cmaq_fst)
      
      # Change colname
      ifelse(used_model == "GBM", 
             pmf_cmaq_fst <-
               plyr::rename(pmf_cmaq_fst,
                            c("gbm_predictions" = "prediction")),
             pmf_cmaq_fst <-
               plyr::rename(pmf_cmaq_fst,
                            c("rf_predictions" = "prediction")))
      
      # Get long/lat
      pmf_cmaq_fst =
        pmf_cmaq_fst %>%
        dplyr::mutate(
          Longitude = as.numeric(str_extract(grid_ID, "-?\\d+\\.\\d+")),
          Latitude = as.numeric(str_extract(grid_ID, "\\d+\\.\\d+$"))
        )
      
      pmf_cmaq_model_perform =
        modeling_perform_metrics(pmf_cmaq_fst$PMF_conc, pmf_cmaq_fst$prediction)
      
      pmf_cmaq_site_perform = 
        pmf_cmaq_fst %>%
        group_by(grid_ID, Longitude, Latitude) %>%
        dplyr::summarise(
          mean_PMF = mean(PMF_conc), 
          median_PMF = median(PMF_conc), 
          mean_ML = mean(prediction), 
          median_ML = median(prediction), 
          R_square = cor(PMF_conc, prediction, use = "complete.obs")^2,
          RMSE = sqrt(mean((PMF_conc - prediction)^2)),
          R_determine = 
            1 - sum((PMF_conc - prediction)^2) / sum((PMF_conc - mean(PMF_conc))^2),
          MAE = mean(abs(PMF_conc - prediction)),
          NMB = mean(prediction - PMF_conc) / mean(PMF_conc) * 100,
          .group = "drop"
        )
      
      pmf_cmaq_site_perform_month = 
        pmf_cmaq_fst %>%
        group_by(grid_ID, Longitude, Latitude, month) %>%
        dplyr::summarise(
          mean_PMF = mean(PMF_conc), 
          median_PMF = median(PMF_conc), 
          mean_ML = mean(prediction), 
          median_ML = median(prediction), 
          R_square = cor(PMF_conc, prediction, use = "complete.obs")^2,
          RMSE = sqrt(mean((PMF_conc - prediction)^2)),
          R_determine = 
            1 - sum((PMF_conc - prediction)^2) / sum((PMF_conc - mean(PMF_conc))^2),
          MAE = mean(abs(PMF_conc - prediction)),
          NMB = mean(prediction - PMF_conc) / mean(PMF_conc) * 100,
          .group = "drop"
        )
      
      cocn_range = c(0, 8)
      
      median_PMF <-
        ggplot() +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", size = 0.3) + 
        geom_point(data = pmf_cmaq_site_perform, 
                   aes(x = Longitude, y = Latitude, color = median_PMF),
                   size = 1.2, alpha = 0.8) +
        scale_color_viridis_c(
          name = "median_PMF", option = "plasma",
          limits = cocn_range,
          oob = scales::squish
        ) +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 14) +
        labs(title = "Median_PMF") +
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
      # median_PMF
      
      perform_NMB <-
        ggplot() +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", size = 0.3) + 
        geom_point(data = pmf_cmaq_site_perform, 
                   aes(x = Longitude, y = Latitude, color = NMB),
                   size = 1.2, alpha = 0.8) +
        scale_color_viridis_c(name = "NMB", option = "viridis") +
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
                fill = NA, color = "grey70", size = 0.3) + 
        geom_point(data = pmf_cmaq_site_perform, 
                   aes(x = Longitude, y = Latitude, color = RMSE),
                   size = 1.2, alpha = 0.8) +
        scale_color_viridis_c(
          name = "RMSE", option = "plasma",
          limits = cocn_range,
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
                fill = NA, color = "grey70", size = 0.3) + 
        geom_point(data = pmf_cmaq_site_perform, 
                   aes(x = Longitude, y = Latitude, color = R_determine),
                   size = 1.2, alpha = 0.8) +
        scale_color_viridis_c(name = "R_determine", option = "viridis") +
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
      
      perform_R_square <-
        ggplot() +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", size = 0.3) + 
        geom_point(data = pmf_cmaq_site_perform, 
                   aes(x = Longitude, y = Latitude, color = R_square),
                   size = 1.2, alpha = 0.8) +
        scale_color_viridis_c(name = "R_square", option = "viridis") +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 14) +
        labs(title = "R_square") +
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
      # perform_R_square
      
      title_text = paste(used_model, "based on", data_coords, "during", cmaq_period, "of", used_source)
      
      perform_overall_plot = 
        (perform_NMB + median_PMF)/(perform_R_square + perform_RMSE) +
        plot_annotation(title = title_text,
                        theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
      # perform_overall_plot
      
      # Output figures
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_", data_coords, "_", cmaq_period, "_", used_source, "_model_performance.pdf")), 
        plot = perform_overall_plot, width = 12, height = 9)
      
      
      # calculate the limits based on the range of the data
      max_rf <- 
        max(c(pmf_cmaq_fst$PMF_conc, 
              pmf_cmaq_fst$prediction), 
            na.rm = TRUE)
      min_rf <-
        min(c(pmf_cmaq_fst$PMF_conc, 
              pmf_cmaq_fst$prediction), 
            na.rm = TRUE)
      
      pmf_vs_predict <-
        ggplot(data = pmf_cmaq_fst, 
               aes(x = PMF_conc, y = prediction)) +
        geom_point(color = "grey35", alpha = 0.6, shape = 1) +
        coord_equal() + # enforces an equal scaling between the axes
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "forestgreen", size = 1) +
        geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid") +
        labs(title = "PMF vs. RF Predictions",
             x = "PMF concentration µg/m^3",
            y = "RF prediction µg/m^3") +
        theme_minimal(base_size = 24) +
        xlim(min_rf, max_rf) +
        ylim(min_rf, max_rf)
      
      # pmf_vs_predict
      
      # Output figures
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_", data_coords, "_", cmaq_period, "_", used_source, "_scatter_model_performance.pdf")), 
        plot = pmf_vs_predict, width = 6, height = 6)
      
    }
  }
}


#### Plot, comparison among different years ####

## Parameter settings
pred_sources = c("Secondary_Sulfate", "Traffic") # , "Traffic", "Dust"
# applied_model = c("RF", "GBM")
applied_model = c("RF")
dataset = c("No_coords_") # "", 

cmaq_period_1 = "2011-01_2011-12"; cmaq_year_1 = 2011
cmaq_period_2 = "2017-01_2017-11"; cmaq_year_2 = 2017

included_years = paste0(cmaq_year_1, "&", cmaq_year_2)
included_year_title = paste0(cmaq_year_1, "-", cmaq_year_2)

print(paste("Study period:", cmaq_period_1, cmaq_period_2))
print(paste("Included years:", included_years))
print(paste0("Study datasets, ", dataset, "; from applied model, ", applied_model))

###### Mapping, based on overall predictions (includes grids without monitoring) ###### 
for(used_model in applied_model){ # used_model = applied_model[1]
  for(used_source in pred_sources){ # used_source = pred_sources[2]; used_source
    for(used_data in dataset){ # used_data = dataset[1]; used_data = dataset[2]
      
      # Generate file name & read fst
      fst_allpred_name_1 = 
        paste0(used_model, "_overall_", used_data, used_source, "_", cmaq_period_1, ".fst")
      pred_fst_1 = read_fst(fst_allpred_name_1)
      pred_fst_1$Year = cmaq_year_1
      # pred_fst_pmf_only = subset(pred_fst_1, !is.na(PMF_conc))
      # modeling_perform_metrics(pred_fst_pmf_only$PMF_conc, pred_fst_pmf_only$rf_predictions)
      
      fst_allpred_name_2 = 
        paste0(used_model, "_overall_", used_data, used_source, "_", cmaq_period_2, ".fst")
      pred_fst_2 = read_fst(fst_allpred_name_2)
      pred_fst_2$Year = cmaq_year_2
      
      print(paste("Files to read:")) 
      head(pred_fst_1)
      head(pred_fst_2)
      mean(pred_fst_1$rf_predictions); sd(pred_fst_1$rf_predictions)
      mean(pred_fst_2$rf_predictions); sd(pred_fst_2$rf_predictions)
      
      # Get used data
      data_coords = ifelse(used_data == "", "With-coords", "No-coords")
      print(data_coords)
      
      # Combine fst
      combined_pred_fst = 
        rbindlist(
          list(pred_fst_1, pred_fst_2))
      
      # Change colname
      ifelse(used_model == "GBM", 
             combined_pred_fst <-
               plyr::rename(combined_pred_fst,
                            c("gbm_predictions" = "Prediction")),
             combined_pred_fst <-
               plyr::rename(combined_pred_fst,
                            c("rf_predictions" = "Prediction")))
      
      # Change colname
      ifelse(used_model == "GBM", 
             pred_fst_1 <-
               plyr::rename(pred_fst_1,
                            c("gbm_predictions" = "Prediction")),
             pred_fst_1 <-
               plyr::rename(pred_fst_1,
                            c("rf_predictions" = "Prediction")))
      
      # Define filled color  
      source_fill_col =
        case_when(
          used_source == "Secondary_Sulfate" ~ "plasma",
          used_source == "Traffic" ~ "viridis",
          used_source == "Dust" ~ "cividis"
        )
      print(source_fill_col)
        
      print("Check the combined file")
      head(combined_pred_fst); dim(combined_pred_fst)
      
      # Dataset included only monitoring sites
      combined_pred_pmf = 
        subset(combined_pred_fst, !is.na(PMF_conc))
      summary(combined_pred_pmf); dim(combined_pred_pmf)
      
      combined_pmf_cmaq_perform =
        modeling_perform_metrics(combined_pred_pmf$PMF_conc, combined_pred_pmf$Prediction)
      print(combined_pmf_cmaq_perform)
      
      ### Get monthly average and whole study period average
      # whole study period average
      combined_grid_predictions_avg = 
        combined_pred_fst %>%
        group_by(Year, grid_ID, Longitude, Latitude) %>%
        dplyr::summarize(
          mean_predction = mean(Prediction, na.rm = TRUE),
          PMF_conc = mean(PMF_conc, na.rm = TRUE),
          .groups = "drop"
        )
      head(combined_grid_predictions_avg); dim(combined_grid_predictions_avg)
      
      # monthly average
      combined_month_predictions_avg = 
        combined_pred_fst %>%
        group_by(Year, grid_ID, Longitude, Latitude, Month) %>%
        dplyr::summarize(
          mean_predction = mean(Prediction, na.rm = TRUE),
          PMF_conc = mean(PMF_conc, na.rm = TRUE),
          .groups = "drop"
        )
      head(combined_month_predictions_avg); dim(combined_month_predictions_avg)
      
      # Choose select month for different sources to compare
      plot_months <- case_when(
        used_source == "Secondary_Sulfate" ~ c(1, 7, 8),
        used_source %in% c("Traffic", "Dust") ~ c(4, 7, 12)
      )
      print("Plot months for the source:"); plot_months
      
      
      #### Plotting
      select_day = "2011-07-26"
      # head(subset(pred_fst_1, Date == as.Date(select_day)))
      one_day_prediction_plot <-
        ggplot() +
        geom_point(data = subset(pred_fst_1, Date == as.Date(select_day)), 
                   aes(x = Longitude, y = Latitude, color = Prediction),
                   size = 0.35, alpha = 0.8) +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", size = 0.3) + 
        scale_color_viridis_c(
          name = "Avg. Concentration", 
          limits = c(0, percentile(subset(pred_fst_1, Date == as.Date(select_day))$Prediction, 0.95)),
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
      
      # Plotting
      combined_grid_predictions_one <-
        ggplot() +
        geom_point(data = combined_grid_predictions_avg, 
                   aes(x = Longitude, y = Latitude, color = mean_predction),
                   size = 0.35, alpha = 0.8) +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", size = 0.3) + 
        scale_color_viridis_c(
          name = "Avg. Concentration", 
          limits = c(0, max(combined_grid_predictions_avg$mean_predction)),
          option = source_fill_col) +
        facet_wrap(. ~ Year) +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 16) +
        labs(x = "Longitude", 
             y = "Latitude", 
             title = paste("Comparison in Prediction across the US via", 
                           used_model, data_coords, "between", included_years, "of", used_source)) +
        theme(
          legend.position = "bottom",
          strip.text = element_text(size = 21),
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
      # combined_grid_predictions_one
      
      
      cocn_range = c(0, 8)
      
      select_month_grid_predictions_plot <-
        ggplot() +
        geom_point(data = subset(combined_month_predictions_avg,
                                 Month %in% plot_months), 
                   aes(x = Longitude, y = Latitude, color = mean_predction),
                   size = 0.35, alpha = 0.8) +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", size = 0.3) + 
        facet_wrap(Year~Month) + 
        scale_color_viridis_c(
          name = "Avg. Concentration", 
          limits = c(0, max(combined_month_predictions_avg$mean_predction)),
          option = source_fill_col) +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 16) +
        labs(x = "Longitude", 
             y = "Latitude", 
             title = paste("Selected Month Prediction Comparison via", 
                           used_model, data_coords, "between", included_years, "of", used_source)) +
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
      # select_month_grid_predictions_plot
      
      # Output figures
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_", data_coords, "_", select_day, "_", used_source, "_Daily.pdf")), 
        plot = one_day_prediction_plot, width = 14.5, height = 8.5)
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_", data_coords, "_", included_year_title, "_", used_source, "_Overall.pdf")), 
        plot = combined_grid_predictions_one, width = 14.5, height = 8.5)
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_", data_coords, "_", included_year_title, "_", used_source, "_Select_Month.pdf")), 
        plot = select_month_grid_predictions_plot, width = 14.5, height = 8.5)
    }
  }
}

