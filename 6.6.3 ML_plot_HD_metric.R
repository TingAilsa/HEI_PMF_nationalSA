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


#### Plot, model perfomance metrics ####
# setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_Results/Aim3_prediction_data")

## Parameter settings
# pred_sources = c("Sulfate", "Traffic", "Dust", "Biomass") # , "Traffic", "Dust"

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

used_model = applied_model[1]
data_coords = dataset[1]


###### Read & prepare data ######
# test_train files for each sources
test_train_Biomass <- paste0("_.*train_test_Biomass_.*", cmaq_period, ".*\\.fst$")
tt_Biomass <- 
  read_fst(list.files(pattern = test_train_Biomass, full.names = TRUE), 
           as.data.table = TRUE) %>%
  dplyr::select(Iteration, grid_ID, PMF_conc, Predictions, group)  %>% 
  mutate(Source = "Biomass")
head(tt_Biomass)

test_train_Sulfate <- paste0("_.*train_test_Sulfate_.*", cmaq_period, ".*\\.fst$")
tt_Sulfate <- 
  read_fst(list.files(pattern = test_train_Sulfate, full.names = TRUE), 
           as.data.table = TRUE) %>%
  dplyr::select(Iteration, grid_ID, PMF_conc, Predictions, group)  %>% 
  mutate(Source = "Sulfate")
head(tt_Sulfate)

test_train_Dust <- paste0("_.*train_test_Dust_.*", cmaq_period, ".*\\.fst$")
tt_Dust <- 
  read_fst(list.files(pattern = test_train_Dust, full.names = TRUE), 
           as.data.table = TRUE) %>%
  dplyr::select(Iteration, grid_ID, PMF_conc, Predictions, group)  %>% 
  mutate(Source = "Dust")
head(tt_Dust)

test_train_Traffic <- paste0("_.*train_test_Traffic_.*", cmaq_period, ".*\\.fst$")
tt_Traffic <- 
  read_fst(list.files(pattern = test_train_Traffic, full.names = TRUE), 
           as.data.table = TRUE) %>%
  dplyr::select(Iteration, grid_ID, PMF_conc, Predictions, group)  %>% 
  mutate(Source = "Traffic")
head(tt_Traffic)

## Combine files into one
print("Check combined train test file from all sources")

tt_all <- 
  rbindlist(
    list(
      tt_Biomass,
      tt_Sulfate,
      tt_Dust, 
      tt_Traffic))
head(tt_all); dim(tt_all)
summary(tt_all)

# ddply(tt_all, .(Source), summarise,
#       PMF_conc_mean = mean(PMF_conc),
#       PMF_conc_med = median(PMF_conc))

## Calculate metrics for each iteration
tt_iteration_perform <- 
  tt_all %>%
  group_by(Iteration, Source) %>%
  dplyr::summarise(
    metrics = list(modeling_perform_metrics(PMF_conc, Predictions))
  ) %>%
  unnest_wider(metrics)
head(tt_iteration_perform)
names(tt_iteration_perform)[3:4] = c("R_Pearson", "R_Determine")

print("Check the tt_iteration_perform")
summary(tt_iteration_perform)
dim(tt_iteration_perform)

# ddply(tt_iteration_perform, .(Source), summarise,
#       R_Pearson_mean = mean(R_Pearson),
#       R_Pearson_med = median(R_Pearson),
#       R_Pearson_max = max(R_Pearson),
#       R_Pearson_min = min(R_Pearson),
#       RMSE_mean = mean(RMSE),
#       RMSE_med = median(RMSE),
#       RMSE_max = max(RMSE),
#       RMSE_min = min(RMSE),
#       MB_mean = mean(MB),
#       MB_med = median(MB),
#       MB_max = max(MB),
#       MB_min = min(MB),
#       NMB_mean = mean(NMB),
#       NMB_med = median(NMB),
#       NMB_max = max(NMB),
#       NMB_min = min(NMB))


# Calculate metrics for each monitoring site
# tt_all = subset(all_pred_fst, !is.na(PMF_conc))
tt_site_perform <- 
  tt_all %>%
  group_by(grid_ID, Source) %>%
  dplyr::summarise(
    metrics = list(modeling_perform_metrics(PMF_conc, Predictions))
  ) %>%
  unnest_wider(metrics)
head(tt_site_perform)
names(tt_site_perform)[3:4] = c("R_Pearson", "R_Determine")

print("Check the tt_site_perform")
summary(tt_site_perform)
dim(tt_site_perform)

# Convert to long 
tt_iteration_perf_long <-
  tt_iteration_perform %>%
  pivot_longer(
    cols = R_Pearson:NMB,
    values_to = "Values",
    names_to = "Metrics"
  )
head(tt_iteration_perf_long)

tt_site_perf_long <-
  tt_site_perform %>%
  pivot_longer(
    cols = R_Pearson:NMB,
    values_to = "Values",
    names_to = "Metrics"
  )
head(tt_site_perf_long)

###### Plot performance metrics by site and interation ######
print("Plotting")

### By site
# Define the positions of Hlines
metric_hlines <- data.frame(
  Metrics = c("MB", "NMB", "R_Pearson", "RMSE"),
  yintercept = c(0, 0, 1, 0))

# plot
tt_site_plot <-
  ggplot(data = 
           subset(tt_site_perf_long,
                  !(Metrics %in% c("R_Determine", "MAE")) &
                    Values < quantile(tt_site_perf_long$Values, 0.975)), 
         aes(x = Source, y = Values)) +
  geom_hline(data = metric_hlines, aes(yintercept = yintercept), 
             color = "#0073C2FF", size = 0.8, linetype = "dashed") +
  geom_jitter(width = 0.18, alpha = 0.35, color = "gray50") +
  geom_boxplot(outlier.shape = NA, fill = NA,
               linewidth = 0.6, width = 0.5, alpha = 0.65) +
  labs(title = "Metrics: Monitoring sites") +
  facet_grid(Metrics ~ ., scales = "free_y") +
  # Use facetted_pos_scales for secondary customization
  facetted_pos_scales(
    y = list(
      # For R_Pearson, force y-axis to start at 0
      Metrics == "R_Pearson" ~ scale_y_continuous(limits = c(0, NA), breaks = function(x) pretty(x, n = 3)),
      # For other metrics, use default scales with adjusted breaks
      TRUE ~ scale_y_continuous(breaks = function(x) pretty(x, n = 3))
    )
  ) +
  theme_base(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text( size = 20),
        axis.text.x = element_text( angle = 0, hjust = 0.5, vjust = 1))
# tt_site_plot

tt_iteration_plot <-
  ggplot(data = 
           subset(tt_iteration_perf_long,
                  !(Metrics %in% c("R_Determine", "MAE")) &
                    Values < quantile(tt_site_perf_long$Values, 0.975)), 
         aes(x = Source, y = Values)) +
  geom_hline(data = metric_hlines, aes(yintercept = yintercept), 
             color = "#0073C2FF", size = 0.8, linetype = "dashed") +
  geom_jitter(width = 0.18, alpha = 0.35, color = "gray50") +
  geom_boxplot(outlier.shape = NA, fill = NA,
               linewidth = 0.6, width = 0.5, alpha = 0.65) +
  labs(title = "Metrics: Holdout iterations") +
  facet_grid(Metrics ~ ., scales = "free_y") +
  # Use facetted_pos_scalesfor secondary customization
  facetted_pos_scales(
    y = list(
      # For R_Pearson, force y-axis to start at 0
      Metrics == "R_Pearson" ~ scale_y_continuous(limits = c(0, NA), breaks = function(x) pretty(x, n = 3)),
      # For other metrics, use default scales with adjusted breaks
      TRUE ~ scale_y_continuous(breaks = function(x) pretty(x, n = 3))
    )
  ) +
  theme_base(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text( size = 20),
        # axis.title.y = element_text( size = 0),
        axis.text.x = element_text( angle = 0, hjust = 0.5, vjust = 1))
# tt_iteration_plot

tt_sum_plot = tt_site_plot + tt_iteration_plot
# tt_sum_plot

ggsave(
  file.path("ML_plot",
            paste0(used_model, "_Pred_HD_US_01_grids_noUnc_",
                   data_coords, cmaq_year, "_train_test_Metrics.png")), 
  plot = tt_sum_plot, width = 11, height = 9)


print("Prepare summary data (conc and metrics) for output")
# Get summary data of the conc for each iteration
head(tt_all); dim(tt_all)
tt_iteration_conc <-
  tt_all %>%
  dplyr::group_by(Iteration, Source) %>%
  dplyr::summarise(
    # cannot use PMF_conc for the mean, otherwise the rest estimations is based on this single value
    PMF_conc_mean = mean(PMF_conc), 
    PMF_conc_sd = sd(PMF_conc), 
    PMF_conc_med = median(PMF_conc), 
    PMF_conc_95 = quantile(PMF_conc, 0.995), 
    PMF_conc_5 = quantile(PMF_conc, 0.005), 
    Predictions_mean = mean(Predictions), 
    Predictions_sd = sd(Predictions), 
    Predictions_med = median(Predictions), 
    Predictions_95 = quantile(Predictions, 0.995), 
    Predictions_5 = quantile(Predictions, 0.005), 
    .groups = "drop"
  )
head(tt_iteration_conc); summary(tt_iteration_conc)

train_test_iteration_perf_conc <-
  merge(tt_iteration_conc, tt_iteration_perform)

# Get summary data of the conc for each site
tt_site_conc <-
  tt_all %>%
  dplyr::group_by(grid_ID, Source) %>%
  dplyr::summarise(
    # cannot use PMF_conc for the mean, otherwise the rest estimations is based on this single value
    PMF_conc_mean = mean(PMF_conc), 
    PMF_conc_sd = sd(PMF_conc), 
    PMF_conc_med = median(PMF_conc), 
    PMF_conc_95 = quantile(PMF_conc, 0.995), 
    PMF_conc_5 = quantile(PMF_conc, 0.005), 
    Predictions_mean = mean(Predictions), 
    Predictions_sd = sd(Predictions), 
    Predictions_med = median(Predictions), 
    Predictions_95 = quantile(Predictions, 0.995), 
    Predictions_5 = quantile(Predictions, 0.005), 
    .groups = "drop"
  )
head(tt_site_conc); summary(tt_site_conc)

train_test_site_perf_conc <-
  merge(tt_site_conc, tt_site_perform)

# Get summary data of the conc for ALL iterations
head(tt_all); dim(tt_all)
tt_all_conc <-
  tt_all %>%
  dplyr::group_by(Source) %>%
  dplyr::summarise(
    # cannot use PMF_conc for the mean, otherwise the rest estimations is based on this single value
    PMF_conc_mean = mean(PMF_conc), 
    PMF_conc_sd = sd(PMF_conc), 
    PMF_conc_med = median(PMF_conc), 
    PMF_conc_95 = quantile(PMF_conc, 0.995), 
    PMF_conc_5 = quantile(PMF_conc, 0.005), 
    Predictions_mean = mean(Predictions), 
    Predictions_sd = sd(Predictions), 
    Predictions_med = median(Predictions), 
    Predictions_95 = quantile(Predictions, 0.995), 
    Predictions_5 = quantile(Predictions, 0.005), 
    .groups = "drop"
  )
head(tt_all_conc); summary(tt_all_conc)

tt_all_perform <- 
  tt_all %>%
  group_by(Source) %>%
  dplyr::summarise(
    metrics = 
      list(modeling_perform_metrics(PMF_conc, Predictions))
  ) %>%
  unnest_wider(metrics)
head(tt_all_perform); summary(tt_all_perform)
names(tt_all_perform)[2:3] = c("R_Pearson", "R_Determine")

train_test_all_perf_conc <-
  merge(tt_all_conc, tt_all_perform)

##  Combine files
# Change colname and relocate position
train_test_all_perf_conc$Iteration = "All"
train_test_all_perf_conc = 
  relocate(train_test_all_perf_conc, Iteration, .before = Source)
train_test_all_perf_conc

# Combine with iteration file
print("Check if all names match")
all(names(train_test_iteration_perf_conc) == names(train_test_all_perf_conc))
train_test_iteration_perf_conc = 
  rbind(train_test_iteration_perf_conc, train_test_all_perf_conc)

# Change colname and combine with site file
names(train_test_all_perf_conc)[1] = "grid_ID"

print("Check if all names match")
all(names(train_test_all_perf_conc) == names(train_test_all_perf_conc))
train_test_site_perf_conc = 
  rbind(train_test_site_perf_conc, train_test_all_perf_conc)

# Output files
write_fst(train_test_iteration_perf_conc, 
          paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords,  
                 cmaq_year, "_train_test_iteration_summary.fst"))
write_fst(train_test_site_perf_conc, 
          paste0(used_model, "_Pred_HD_US_01_grids_noUnc_", data_coords,  
                 cmaq_year, "_train_test_site_summary.fst"))
