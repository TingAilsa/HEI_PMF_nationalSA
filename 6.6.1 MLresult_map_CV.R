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
library(ggthemes)
library(patchwork)
library(USAboundaries)
library(viridis)
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

# setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_prediction/Annual_combine")
getwd()

## Extract long & points with the continental US
library(USAboundaries)
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]


#### Plot for a single year ####

## Parameter settings
pred_sources = c("Sulfate", "Traffic", "Dust", "Biomass") # , "Traffic", "Dust"
# cmaq_period = "2011-01_2011-12"; cmaq_year = 2011
cmaq_period = "2012-01_2012-12"; cmaq_year = 2012
# cmaq_period = "2013-01_2013-12"; cmaq_year = 2013
# cmaq_period = "2015-01_2015-12"; cmaq_year = 2015
# cmaq_period = "2016-01_2016-12"; cmaq_year = 2016
# cmaq_period = "2017-01_2017-12"; cmaq_year = 2017

# applied_model = c("RF", "GBM")
applied_model = c("RF")
dataset = c("noCoords_") # "", "No_coords_"

print(paste0("Study period ", cmaq_period, " & Year ", cmaq_year, 
             ", with datasets ", dataset, " from applied model ", applied_model))

# used_model = applied_model[1]; used_source = pred_sources[1]; used_data = dataset[1]
# used_model; used_source; used_data

###### Mapping, based on overall predictions (includes grids without monitoring) ###### 
for(used_model in applied_model){ # used_model = applied_model[1]
  for(used_source in pred_sources){ # used_source = pred_sources[1]
    for(used_data in dataset){ # used_data = dataset[1]
      
      # Generate file name & read fst
      # fst_allpred_name = 
      #   paste0(used_model, "_overall_", used_data, used_source, "_", cmaq_period, ".fst")
      fst_allpred_name = 
        paste0(used_model, "_Pred_mainland_US_01_grids_noUnc_", used_data, used_source, "_", cmaq_period, ".fst")
      
      print(fst_allpred_name)
      all_pred_fst = read_fst(fst_allpred_name)
      head(all_pred_fst)
      
      # Get used data
      data_coords = ifelse(used_data == "", "With-coords", "No-coords")
      print(data_coords)
      
      # Rename
      all_pred_fst = plyr::rename(all_pred_fst, 
                                  c("month" = "Month"))
      # head(all_pred_fst)
      
      # # Get long, lat
      # all_pred_fst$Longitude = as.numeric(sub("^grid_([^_]+)_.*$", "\\1", all_pred_fst$grid_ID))
      # all_pred_fst$Latitude = as.numeric(sub("^grid_[^_]+_(.*)$", "\\1", all_pred_fst$grid_ID))

      # Define filled color  
      source_fill_col =
        case_when(
          used_source == "Sulfate" ~ "magma",
          used_source == "Traffic" ~ "viridis",
          used_source == "Dust" ~ "cividis",
          used_source == "Biomass" ~ "plasma"
        )
      print(source_fill_col)
      
      # # Change colname
      # ifelse(used_model == "GBM", 
      #        all_pred_fst <-
      #          plyr::rename(all_pred_fst,
      #                       c("gbm_predictions" = "Predictions")),
      #        all_pred_fst <-
      #          plyr::rename(all_pred_fst,
      #                       c("rf_predictions" = "Predictions")))
      
      ###### Annual and monthly distribution, based on data from all grids ######
      
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
                  width = 0.1, height = 0.1, color = NA) +  
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
        geom_tile(data = month_predictions_avg, 
                   aes(x = Longitude, y = Latitude, fill = mean_predction),
                   width = 0.1, height = 0.1, color = NA) +  
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", linewidth = 0.3) + 
        facet_wrap(~Month, ncol = 4) + 
        scale_fill_viridis_c(
          name = "Avg. Concentration", 
          limits = c(0, quantile(month_predictions_avg$mean_predction, 0.95)),
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
                  paste0(used_model, "_US_01_grid_", data_coords, "_", cmaq_period, "_", used_source, "_Overall.png")), 
        plot = all_grid_predictions_one, width = 14.5, height = 8.5)
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_US_01_grid_", data_coords, "_", cmaq_period, "_", used_source, "_Monthly.png")), 
        plot = month_grid_predictions_plot, width = 14.5, height = 8.5)
      
      ###### One day result ######
      
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
                  paste0(used_model, "_US_01_grid_", data_coords, "_", select_day, "_", used_source, "_Daily.png")), 
        plot = one_day_prediction_plot, width = 14.5, height = 8.5)
      
      
      ###### Performance, based on data with PMF results only ######
      pmf_cmaq_fst = subset(all_pred_fst, !is.na(PMF_conc))
      summary(pmf_cmaq_fst)
      
      # Calculate the percentiles for sites
      pmf_cmaq_site_metric = 
        pmf_cmaq_fst %>%
        group_by(Longitude, Latitude) %>%
        dplyr::summarise(
          metrics = list(modeling_perform_metrics(PMF_conc, Predictions)),
          .groups = "drop"
        ) %>%
        unnest_wider(metrics)
      names(pmf_cmaq_site_metric)[3:4] = c("R_Pearson", "R_Determine")
      head(pmf_cmaq_site_metric); dim(pmf_cmaq_site_metric)
      
      pmf_cmaq_site_conc = 
        pmf_cmaq_fst %>%
        group_by(Longitude, Latitude) %>%
        dplyr::summarise(
          mean_PMF = mean(PMF_conc), 
          median_PMF = median(PMF_conc), 
          mean_ML = mean(Predictions), 
          median_ML = median(Predictions),
          .groups = "drop"
        )
      
      # Combine metric with conc
      pmf_cmaq_site_perform = 
        merge(pmf_cmaq_site_metric, pmf_cmaq_site_conc)
      summary(pmf_cmaq_site_perform$median_PMF)
      # head(pmf_cmaq_site_perform)

      median_PMF <-
        ggplot() +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", linewidth = 0.3) + 
        geom_point(data = pmf_cmaq_site_perform, 
                   aes(x = Longitude, y = Latitude, color = median_PMF),
                   size = 1.2, alpha = 0.8) +
        scale_color_viridis_c(
          name = "median_PMF", option = "plasma",
          # limits = conc_range,
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
                fill = NA, color = "grey70", linewidth = 0.3) + 
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
                fill = NA, color = "grey70", linewidth = 0.3) + 
        geom_point(data = pmf_cmaq_site_perform, 
                   aes(x = Longitude, y = Latitude, color = RMSE),
                   size = 1.2, alpha = 0.8) +
        scale_color_viridis_c(
          name = "RMSE", option = "plasma",
          # limits = conc_range,
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
      
      perform_R_Determine <-
        ggplot() +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", linewidth = 0.3) + 
        geom_point(data = pmf_cmaq_site_perform, 
                   aes(x = Longitude, y = Latitude, color = R_Determine),
                   size = 1.2, alpha = 0.8) +
        scale_color_viridis_c(name = "R_Determine", option = "viridis") +
        coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
        theme_minimal(base_size = 14) +
        labs(title = "R_Determine") +
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
      # perform_R_Determine
      
      perform_R_Pearson <-
        ggplot() +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", linewidth = 0.3) + 
        geom_point(data = pmf_cmaq_site_perform, 
                   aes(x = Longitude, y = Latitude, color = R_Pearson),
                   size = 1.2, alpha = 0.8) +
        scale_color_viridis_c(name = "R_Pearson", option = "viridis") +
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
        (perform_NMB + median_PMF)/(perform_R_Pearson + perform_RMSE) +
        plot_annotation(title = title_text,
                        theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
      # perform_overall_plot
      
      
      # Output figures
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_US_01_grid_", data_coords, "_", cmaq_period, "_", used_source, "_model_performance.png")), 
        plot = perform_overall_plot, width = 12, height = 9)
      
      
      ##### Histgram
      site_perform_long <-
        dplyr::select(pmf_cmaq_site_perform,
                      Longitude:NMB) %>%
        pivot_longer(
          cols = R_Pearson:NMB,
          values_to = "Values",
          names_to = "Metrics"
        )
      head(site_perform_long)
      
      # performance metrics by site, save dataset

      # Output figures
      write_fst(site_perform_long,
        file.path(paste0(used_model, "_CV_US_01_grids_", data_coords, "_", 
                         cmaq_period, "_", used_source, "_boxplot_performance.fst")))
      
      
      # calculate the limits based on the range of the data
      max_rf <- 
        max(c(pmf_cmaq_fst$PMF_conc, 
              pmf_cmaq_fst$Predictions), 
            na.rm = TRUE)
      min_rf <-
        min(c(pmf_cmaq_fst$PMF_conc, 
              pmf_cmaq_fst$Predictions), 
            na.rm = TRUE)
      
      pmf_vs_predict <-
        ggplot(data = pmf_cmaq_fst, 
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
      
      cmaq_vs_predict <-
        ggplot(data = pmf_cmaq_fst, 
               aes(x = CMAQ_conc, y = Predictions)) +
        geom_point(color = "grey35", alpha = 0.6, shape = 1) +
        coord_equal() + # enforces an equal scaling between the axes
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "forestgreen", size = 1) +
        geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid") +
        labs(title = "CMAQ vs. ML Predictions",
             x = "CMAQ concentration µg/m^3",
             y = "ML prediction µg/m^3") +
        theme_minimal(base_size = 24) +
        xlim(min_rf, max_rf) +
        ylim(min_rf, max_rf)
      # cmaq_vs_predict
      
      pmf_cmaq_pred = pmf_vs_predict + cmaq_vs_predict
      
      # Output figures
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_US_01_grid_", data_coords, "_", cmaq_period, "_", used_source, "_PMF_CMAQ_ML_scatter.png")), 
        plot = pmf_cmaq_pred, width = 6, height = 6)
      
    }
  }
}


#### Plot, comparison among different years ####

## Parameter settings
pred_sources = c("Sulfate", "Traffic", "Dust", "Biomass") # , "Traffic", "Dust"
# applied_model = c("RF", "GBM")
applied_model = c("RF")
dataset = c("No_coords_") # "", 

# cmaq_period_1 = "2011-01_2011-12"; cmaq_year_1 = 2011
cmaq_period_1 = "2013-01_2013-12"; cmaq_year_1 = 2013
cmaq_period_2 = "2017-01_2017-12"; cmaq_year_2 = 2017

included_years = paste0(cmaq_year_1, "&", cmaq_year_2)
included_year_title = paste0(cmaq_year_1, "-", cmaq_year_2)

print(paste("Study period:", cmaq_period_1, cmaq_period_2))
print(paste("Included years:", included_years))
print(paste0("Study datasets, ", dataset, "; from applied model, ", applied_model))

# used_model = applied_model[1]; used_source = pred_sources[2]; used_data = dataset[1]

###### Mapping, based on overall predictions (includes grids without monitoring) ###### 
for(used_model in applied_model){ # used_model = applied_model[1]
  for(used_source in pred_sources){ # used_source = pred_sources[2]; used_source
    for(used_data in dataset){ # used_data = dataset[1]; used_data = dataset[2]
      
      # Generate file name & read fst
      fst_allpred_name_1 = 
        # paste0(used_model, "_overall_", used_data, used_source, "_", cmaq_period_1, ".fst")
        paste0(used_model, "_Pred_mainland_US_01_grids_noUnc_", used_data, 
               used_source, "_", cmaq_period_1, ".fst")
      
      pred_fst_1 = read_fst(fst_allpred_name_1)
      pred_fst_1$Year = cmaq_year_1
      # pred_fst_pmf_only = subset(pred_fst_1, !is.na(PMF_conc))
      # modeling_perform_metrics(pred_fst_pmf_only$PMF_conc, pred_fst_pmf_only$rf_predictions)
      
      fst_allpred_name_2 = 
        # paste0(used_model, "_overall_", used_data, used_source, "_", cmaq_period_2, ".fst")
        paste0(used_model, "_Pred_mainland_US_01_grids_noUnc_", used_data, 
               used_source, "_", cmaq_period_2, ".fst")

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
                            c("gbm_predictions" = "Predictions")),
             combined_pred_fst <-
               plyr::rename(combined_pred_fst,
                            c("rf_predictions" = "Predictions")))
      
      # Change colname
      ifelse(used_model == "GBM", 
             pred_fst_1 <-
               plyr::rename(pred_fst_1,
                            c("gbm_predictions" = "Predictions")),
             pred_fst_1 <-
               plyr::rename(pred_fst_1,
                            c("rf_predictions" = "Predictions")))
      
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
          mean_predction = mean(Predictions, na.rm = TRUE),
          PMF_conc = mean(PMF_conc, na.rm = TRUE),
          .groups = "drop"
        )
      head(combined_grid_predictions_avg); dim(combined_grid_predictions_avg)
      
      # monthly average
      combined_month_predictions_avg = 
        combined_pred_fst %>%
        group_by(Year, grid_ID, Longitude, Latitude, Month) %>%
        dplyr::summarize(
          mean_predction = mean(Predictions, na.rm = TRUE),
          PMF_conc = mean(PMF_conc, na.rm = TRUE),
          .groups = "drop"
        )
      head(combined_month_predictions_avg); dim(combined_month_predictions_avg)
      
      # Choose select month for different sources to compare
      plot_months <- case_when(
        used_source == "Sulfate" ~ c(1, 4, 7，10),
        used_source %in% c("Traffic", "Dust") ~ c(1, 4, 7，10) # c(4, 7, 12)
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
                fill = NA, color = "grey70", linewidth = 0.3) + 
        scale_color_viridis_c(
          name = "Avg. Concentration", 
          limits = c(0, quantile(subset(pred_fst_1, Date == as.Date(select_day))$Prediction, 0.95)),
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
                fill = NA, color = "grey70", linewidth = 0.3) + 
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
      
      
      # conc_range = c(0, 8)
      
      select_month_grid_predictions_plot <-
        ggplot() +
        geom_point(data = subset(combined_month_predictions_avg,
                                 Month %in% plot_months), 
                   aes(x = Longitude, y = Latitude, color = mean_predction),
                   size = 0.35, alpha = 0.8) +
        geom_sf(data = us_states, 
                fill = NA, color = "grey70", linewidth = 0.3) + 
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
                  paste0(used_model, "_", data_coords, "_", select_day, "_", used_source, "_Daily.png")), 
        plot = one_day_prediction_plot, width = 14.5, height = 8.5)
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_", data_coords, "_", included_year_title, "_", used_source, "_Overall.png")), 
        plot = combined_grid_predictions_one, width = 14.5, height = 8.5)
      ggsave(
        file.path("ML_plot",
                  paste0(used_model, "_", data_coords, "_", included_year_title, "_", used_source, "_Select_Month.png")), 
        plot = select_month_grid_predictions_plot, width = 14.5, height = 8.5)
    }
  }
}

