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
library(tidytext)
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

# setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/Annual_combine")
setwd("/Users/TingZhang/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nationwide_SA/data/outputs/Aim3_inputs_predictions_10year_model")
getwd()

## Parameter settings
pred_sources = c("Sulfate", "Traffic", "Dust", "Biomass", "Nitrate", "PM25")
cmaq_year = 2011-2020

#### Train & Test I, model performance ####

###### Read & prepare data ######
# test_train files 
source_file_pattern <- "RF_Pred_HD_US_01_train_test_.*_2011-2020\\.fst$"
source_test_files <- list.files(pattern = source_file_pattern, full.names = TRUE)
print(source_test_files)

#### Read all files and combine with source names
tt_all <- 
  map_dfr(source_test_files, function(file) {
    # Extract source name from filename
    source_name <- extract_source_name(file, "train_test", "2011-2020")
    
    # Read the fst file
    data <- 
      read_fst(file) %>%
      dplyr::select(Iteration, grid_ID, PMF_conc, Predictions)
    
    # Add source name column
    data$Source <- source_name
    
    # Print progress
    cat("Processed:", source_name, "- Rows:", nrow(data), "\n")
    
    return(data)
  })

head(tt_all); dim(tt_all)
# summary(tt_all)

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
                    Values < quantile(tt_site_perf_long$Values, 0.975)) %>%
           mutate(Source = factor(Source, # Placing PM25 by the end
                                  levels = c(setdiff(unique(Source), "PM25"), "PM25"))), 
         aes(x = Source, y = Values)) +
  geom_hline(data = metric_hlines, aes(yintercept = yintercept), 
             color = "#0073C2FF", size = 0.8, linetype = "dashed") +
  geom_jitter(width = 0.18, alpha = 0.35, color = "gray50") +
  geom_boxplot(outlier.shape = NA, fill = NA,
               linewidth = 0.6, width = 0.5, alpha = 0.65) +
  scale_x_discrete(labels = c("Biomass" = "Biomass\n urning",
                              "Dust" = "Soil/Dust", 
                              "Nitrate" = "Secondary\n nitrate",
                              "Sulfate" = "Secondary\n sulfate",
                              "Traffic" = "Traffic\n exhaust",
                              "PM25" = format_variable("PM2.5"))) +
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
        panel.grid.major.y = element_line(color = "gray90", size = 0.5, linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text( size = 20),
        axis.text.x = element_text( angle = 45, hjust = 0.5, vjust = 0.7, 
                                    family = "Arial Unicode MS"))
tt_site_plot

tt_iteration_plot <-
  ggplot(data = 
           subset(tt_iteration_perf_long,
                  !(Metrics %in% c("R_Determine", "MAE")) &
                    Values < quantile(tt_iteration_perf_long$Values, 0.975)) %>%
           mutate(Source = factor(Source, # Placing PM25 by the end
                                  levels = c(setdiff(unique(Source), "PM25"), "PM25"))), 
         aes(x = Source, y = Values)) +
  geom_hline(data = metric_hlines, aes(yintercept = yintercept), 
             color = "#0073C2FF", size = 0.8, linetype = "dashed") +
  geom_jitter(width = 0.18, alpha = 0.35, color = "gray50") +
  geom_boxplot(outlier.shape = NA, fill = NA,
               linewidth = 0.6, width = 0.5, alpha = 0.65) +
  scale_x_discrete(labels = c("Biomass" = "Biomass\n burning",
                              "Dust" = "Soil/Dust", 
                              "Nitrate" = "Secondary\n nitrate",
                              "Sulfate" = "Secondary\n sulfate",
                              "Traffic" = "Traffic\n exhaust",
                              "PM25" = format_variable("PM2.5"))) +
  labs(title = "Metrics: Holdout iterations") +
  facet_grid(Metrics ~ ., scales = "free_y") +
  # Use facetted_pos_scalesfor secondary customization
  facetted_pos_scales(
    y = list(
      # For R_Pearson, force y-axis to start at 0
      Metrics == "R_Pearson" ~ scale_y_continuous(limits = c(NA, NA), breaks = function(x) pretty(x, n = 3)),
      # For other metrics, use default scales with adjusted breaks
      TRUE ~ scale_y_continuous(breaks = function(x) pretty(x, n = 3))
    )
  ) +
  theme_base(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5, linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text( size = 20),
        # axis.title.y = element_text( size = 0),
        axis.text.x = element_text( angle = 45, hjust = 0.5, vjust = 0.7, 
                                    family = "Arial Unicode MS"))
tt_iteration_plot

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



#### Train & Test II, site coverage ####

###### Read & prepare data ######
# test_train files 
source_file_pattern <- "RF_Pred_HD_US_01_train_test_.*_2011-2020\\.fst$"
source_test_files <- list.files(pattern = source_file_pattern, full.names = TRUE)
print(source_test_files)

#### Read all files and combine with source names
site_all <- 
  map_dfr(source_test_files, function(file) {
    # Extract source name from filename
    source_name <- extract_source_name(file, "train_test", "2011-2020")
    
    # Read the fst file
    data <- 
      read_fst(file) %>%
      dplyr::select(Iteration, grid_ID, group)
    
    # Add source name column
    data$Source <- source_name
    
    # Print progress
    cat("Processed:", source_name, "- Rows:", nrow(data), "\n")
    
    return(data)
  })

head(site_all); dim(site_all)

# Get unique grid_ID count for each group and source combination
site_grid_count <- 
  site_all %>%
  dplyr::group_by(group, Source) %>%
  dplyr::summarise(
    unique_grid_count = n_distinct(grid_ID),
    .groups = "drop"
  )
head(site_grid_count); dim(site_grid_count)





#### Variable importance ####

###### Read & prepare data, all variable, and top 10 most influential variables ######
# test_train files 
var_source_file_pattern <- "RF_Pred_HD_US_01_predictor_perform_.*_2011-2020\\.fst$"
varialbe_importance_files <- list.files(pattern = var_source_file_pattern, full.names = TRUE)
print(varialbe_importance_files)

#### Read all files and combine with source names
var_imp_all <- 
  map_dfr(varialbe_importance_files, function(file) {
    # Extract source name from filename
    source_name <- extract_source_name(file, "predictor_perform", "2011-2020")
    
    # Read the fst file
    data <- 
      read_fst(file)
    
    # Add source name column
    data$Source <- source_name
    
    # Print progress
    cat("Processed:", source_name, "- Rows:", nrow(data), "\n")
    
    return(data)
  })

head(var_imp_all); dim(var_imp_all)
# summary(var_imp_all)

# ddply(var_imp_all, .(Source), summarise,
#       PMF_conc_mean = mean(PMF_conc),
#       PMF_conc_med = median(PMF_conc))

## Calculate metrics for each iteration
var_imp_iteration_perform <- 
  var_imp_all %>%
  group_by(Source, Variable) %>%
  dplyr::summarise(
    Relative_Influence = median(Relative_Influence),
    .groups = "drop"
  )
head(var_imp_iteration_perform)

print("Check the var_imp_iteration_perform")
summary(var_imp_iteration_perform)
dim(var_imp_iteration_perform)

# Select top 10 variables for each source
var_imp_top10 <- var_imp_iteration_perform %>%
  group_by(Source) %>%
  slice_max(order_by = Relative_Influence, n = 10) %>%
  ungroup() %>%
  # Reorder Source factor to put PM25 at the end
  mutate(Source = factor(Source, 
                         levels = c(setdiff(unique(Source), "PM25"), "PM25")))

head(var_imp_top10)
print("Check the var_imp_top10")
summary(var_imp_top10)
dim(var_imp_top10)

###### plot the relative influence of all variables and top 10 ######

rf_var_influence_p <-
  ggplot(var_imp_iteration_perform, 
         aes(y = reorder(Variable, 
                         Relative_Influence), 
             x = Relative_Influence)) +
  geom_bar(stat = "identity",
           fill = "steelblue",
           width = 0.5) +
  facet_wrap(.~Source, scales = "free_y") +
  # coord_flip() +  # Flip coordinates to make a horizontal bar plot
  labs(title = "Variable Performance",
       x = "Predictor",
       y = "Relative Influence (%)") +
  theme_minimal(base_size = 16)
rf_var_influence_p


rf_var_top10_inf_p <-
  ggplot(var_imp_top10, 
         # Order the bar sequence by variable and Relative_Influence for each source
         aes(y = reorder_within(Variable, 
                                Relative_Influence, 
                                Source), 
             x = Relative_Influence)) +
  geom_bar(stat = "identity",
           fill = "steelblue",
           width = 0.6) +
  facet_wrap(~Source, scales = "free_y") +
  scale_y_reordered() +  # This removes the source suffix from labels
  labs(title = "Top 10 Most Influential Variables by Source",
       x = "Relative Influence (%)",
       y = "Predictor Variables") +
  theme_minimal(base_size = 16)
rf_var_top10_inf_p
