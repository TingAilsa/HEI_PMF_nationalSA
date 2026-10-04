# AY_TrainTest_summary.R

library(data.table)
library(fst)
library(stringr)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ML_Final_Input_001/Annual_combine")

# Function to calculate performance metrics
modeling_perform_metrics <- 
  function(obs, pred) {
    
    err <- pred - obs
    list(
      N    = length(obs),
      r2   = cor(obs, pred)^2,
      R2   = 1 - sum(err^2) / sum((obs - mean(obs))^2),
      RMSE = sqrt(mean(err^2)),
      MAE  = mean(abs(err)),
      MB   = mean(err),
      NMB  = sum(err) / sum(obs) * 100
    )
  }

# Function to summary obs and pred for each group
conc_summary <-
  function(obs, pred) {
    list(
      PMF_conc_mean    = mean(obs),
      PMF_conc_sd      = sd(obs),
      PMF_conc_med     = median(obs),
      PMF_conc_p995    = quantile(obs, 0.995, names = FALSE),
      PMF_conc_p005    = quantile(obs, 0.005, names = FALSE),
      Predictions_mean = mean(pred),
      Predictions_sd   = sd(pred),
      Predictions_med  = median(pred),
      Predictions_p995 = quantile(pred, 0.995, names = FALSE),
      Predictions_p005 = quantile(pred, 0.005, names = FALSE)
    )
  }

#### Read data & Get performance by Site & Iteration each ####
# # Source to process
# pred_sources = c("sulfate", "traffic", "dust", "bbsoa", "nitrate", "pm")

# test_train files 
source_file_pattern <- "RF_US_train_test_2011-20_001_.*\\.fst$"
source_test_files <- list.files(pattern = source_file_pattern, full.names = TRUE)
print(source_test_files)

#### Read all files and combine with source names
perf_list <- 
  lapply(source_test_files, function(file) {
    # Extract source name from filename
    source_name <- str_match(basename(file), "_([^_]+)\\.fst$")[, 2]
    
    # Read the fst file
    data <- 
      read_fst(file,
               columns = c("Iteration", "grid_ID", "PMF_conc", "Predictions", "group"),
               as.data.table = TRUE)
    data <- data[group == "Train"]; data$Train = NULL
    
    # Estimate performance indexes
    iter_perf <- 
      data[, modeling_perform_metrics(PMF_conc, Predictions), by = Iteration][, Source := source_name]
    site_perf <- 
      data[, modeling_perform_metrics(PMF_conc, Predictions), by = grid_ID][, Source := source_name]
    
    # Concentration summaries
    iter_conc <- 
      data[, conc_summary(PMF_conc, Predictions), by = .(Iteration)][, Source := source_name]
    site_conc <- 
      data[, conc_summary(PMF_conc, Predictions), by = .(grid_ID)][, Source := source_name]
    
    cat("Processed:", source_name, "- Rows:", nrow(dt), "\n")
    rm(data)
    
    list(iter = iter_perf, site = site_perf,
         iter_conc = iter_conc, site_conc = site_conc)
  })

# Combine the performance of all sources
tt_iteration_perform <- rbindlist(lapply(perf_list, `[[`, "iter"))
tt_site_perform <- rbindlist(lapply(perf_list, `[[`, "site"))

tt_iteration_conc    <- rbindlist(lapply(perf_list, `[[`, "iter_conc"))
tt_site_conc         <- rbindlist(lapply(perf_list, `[[`, "site_conc"))

iteration_perform_conc <- 
  merge(tt_iteration_perform, tt_iteration_conc,
        by = c("Iteration", "Source"))
site_perform_conc <- 
  merge(tt_site_perform, tt_site_conc,
        by = c("grid_ID", "Source"))

write.csv(iteration_perform_conc, "RF_US_train_test_2011-20_001_by_Iteration.csv")
write.csv(site_perform_conc, "RF_US_train_test_2011-20_001_by_Site.csv")


#### Variable importance ####

###### Read & prepare data ######
# test_train files 
source_file_pattern <- "RF_US_predictor_perform_2011-20_001_.*\\.fst$"
var_importance_files <- list.files(pattern = source_file_pattern, full.names = TRUE)
print(var_importance_files)

var_imp_all <- 
  map_dfr(var_importance_files, function(file) {
    # Extract source name from filename
    source_name <- str_match(basename(file), "_([^_]+)\\.fst$")[, 2]
    
    # Read the fst file
    data <- read_fst(file, as.data.table = TRUE)
    
    # Add source name column
    data$Source <- source_name
    
    # Print progress
    cat("Processed:", source_name, "- Rows:", nrow(data), "\n")
    
    return(data)
  })
head(var_imp_all)
summary(var_imp_all)
dim(var_imp_all)

write.csv(var_imp_all, "RF_US_Var_Importance_2011-20_001.csv")




#### Plot #### 
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
library(tidytable)

###### Prediction validation, by Site by Iteration ######

tt_iteration_perform = 
  fread("/Users/ztttttt/Downloads/RF_US_train_test_2011-20_001_by_Iteration.csv")
tt_site_perform = 
  fread("/Users/ztttttt/Downloads/RF_US_train_test_2011-20_001_by_Site.csv")
tt_iteration_perform$V1 = tt_site_perform$V1 = NULL

# Convert to long 
tt_iteration_perf_long <-
  tt_iteration_perform %>%
  dplyr::select(Source, r2:NMB) %>%
  pivot_longer(
    cols = r2:NMB,
    values_to = "Values",
    names_to = "Metrics"
  )
head(tt_iteration_perf_long)

tt_site_perf_long <-
  tt_site_perform %>%
  dplyr::select(Source, r2:NMB) %>%
  pivot_longer(
    cols = r2:NMB,
    values_to = "Values",
    names_to = "Metrics"
  )
head(tt_site_perf_long)

iteration_perf_sum <-
  tt_iteration_perf_long %>%
  dplyr::group_by(Source, Metrics) %>%
  dplyr::summarise(
    mean   = mean(Values),
    sd     = sd(Values),
    median = median(Values),
    min    = min(Values),
    max    = max(Values),
    p5     = unname(quantile(Values, 0.05)),
    p95    = unname(quantile(Values, 0.95)),
    .groups = "drop"
  ) %>%
  as.data.frame()

tt_iteration_conc <-
  tt_iteration_perform %>%
  dplyr::select(Source, PMF_conc_mean, PMF_conc_med, Predictions_mean, Predictions_med) %>%
  pivot_longer(
    cols = PMF_conc_mean:Predictions_med,
    values_to = "Concentrations",
    names_to = "Metrics"
  )

iteration_conc_sum <-
  tt_iteration_conc %>%
  dplyr::group_by(Source, Metrics) %>%
  dplyr::summarise(
    min   = min(Concentrations),
    max = max(Concentrations),
    .groups = "drop"
  ) %>%
  as.data.frame()

ggplot(
  # tt_iteration_perform,
  subset(tt_iteration_perform, Source != "pm"),
  aes(x = PMF_conc_mean, y = Predictions_mean, color = Source)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey50", linewidth = 0.6) +
  geom_point(alpha = 0.35, size = 2.5, stroke = 0) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "PMF concentration (mean)",
       y = "Predicted concentration (mean)",
       color = NULL) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 4))) +
  theme_bw(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    # legend.position  = "bottom",
    legend.text      = element_text(size = 14),
    axis.title       = element_text(face = "bold"),
    plot.margin      = margin(10, 15, 10, 10)
  )

    
###### Plot performance metrics by site and interation ######
print("Plotting")

### By site
# Define the positions of Hlines
metric_hlines <- data.frame(
  Metrics = c("MB", "NMB", "r2", "RMSE"),
  yintercept = c(0, 0, 1, 0))

# plot
tt_site_plot <-
  ggplot(data = 
           subset(tt_site_perf_long,
                  !(Metrics %in% c("R2", "MAE")) &
                    Values < quantile(tt_site_perf_long$Values, 0.975)) %>%
           # mutate(Source = factor(Source, # Placing PM25 by the end
           #                        levels = c(setdiff(unique(Source), "PM25"), "PM25"))), 
           mutate(Source = factor(Source,
                                  levels = c("traffic", "nitrate", "sulfate", "bbsoa", "dust", "pm"))),
         aes(x = Source, y = Values)) +
  geom_hline(data = metric_hlines, aes(yintercept = yintercept), 
             color = "#0073C2FF", size = 0.8, linetype = "dashed") +
  geom_jitter(width = 0.18, alpha = 0.35, color = "gray50") +
  geom_boxplot(outlier.shape = NA, fill = NA,
               linewidth = 0.6, width = 0.5, alpha = 0.65) +
  scale_x_discrete(labels = c("bbsoa" = "Biomass\n burning/\nOA",
                              "dust" = "Dust", 
                              "nitrate" = "Secondary\n nitrate",
                              "sulfate" = "Sulfate",
                              "traffic" = "Traffic\n exhaust",
                              "pm" = format_variable("PM2.5"))) +
  labs(title = "Metrics: Monitoring sites") +
  facet_grid(Metrics ~ ., scales = "free_y") +
  # Use facetted_pos_scales for secondary customization
  facetted_pos_scales(
    y = list(
      # For r2, force y-axis to start at 0
      Metrics == "r2" ~ scale_y_continuous(limits = c(0, NA), breaks = function(x) pretty(x, n = 3)),
      # For other metrics, use default scales with adjusted breaks
      TRUE ~ scale_y_continuous(breaks = function(x) pretty(x, n = 3))
    )
  ) +
  theme_base(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        panel.spacing = unit(5, "mm"),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5, linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text( size = 20),
        axis.text.x = element_text( angle = 90, hjust = 0.5, vjust = 0.5, 
                                    family = "Arial Unicode MS"))
tt_site_plot

tt_iteration_plot <-
  ggplot(data = 
           subset(tt_iteration_perf_long,
                  !(Metrics %in% c("R2", "MAE")) &
                    Values < quantile(tt_iteration_perf_long$Values, 0.975)) %>%
           # mutate(Source = factor(Source, # Placing PM25 by the end
           #                        levels = c(setdiff(unique(Source), "PM25"), "PM25"))), 
           mutate(Source = factor(Source,
                                  levels = c("traffic", "nitrate", "sulfate", "bbsoa", "dust", "pm"))),
         aes(x = Source, y = Values)) +
  geom_hline(data = metric_hlines, aes(yintercept = yintercept), 
             color = "#0073C2FF", size = 0.8, linetype = "dashed") +
  geom_jitter(width = 0.18, alpha = 0.35, color = "gray50") +
  geom_boxplot(outlier.shape = NA, fill = NA,
               linewidth = 0.6, width = 0.5, alpha = 0.65) +
  scale_x_discrete(labels = c("bbsoa" = "Biomass\n burning/\nOA",
                              "dust" = "Dust", 
                              "nitrate" = "Secondary\n nitrate",
                              "sulfate" = "Sulfate",
                              "traffic" = "Traffic\n exhaust",
                              "pm" = format_variable("PM2.5"))) +
  labs(title = "Metrics: Holdout iterations") +
  facet_grid(Metrics ~ ., scales = "free_y") +
  # Use facetted_pos_scalesfor secondary customization
  facetted_pos_scales(
    y = list(
      # For r2, force y-axis to start at 0
      Metrics == "r2" ~ scale_y_continuous(limits = c(NA, NA), breaks = function(x) pretty(x, n = 3)),
      # For other metrics, use default scales with adjusted breaks
      TRUE ~ scale_y_continuous(breaks = function(x) pretty(x, n = 3))
    )
  ) +
  theme_base(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        panel.spacing = unit(5, "mm"),
        panel.background = element_rect(fill = "white", color = NA),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5, linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text( size = 20),
        # axis.title.y = element_text( size = 0),
        axis.text.x = element_text( angle = 90, hjust = 0.5, vjust = 0.5, 
                                    family = "Arial Unicode MS"))
tt_iteration_plot

tt_sum_plot = tt_site_plot + tt_iteration_plot
tt_sum_plot

ggsave(
  file.path("ML_plot",
            paste0(used_model, "_", data, "_Pred_US_01_", cmaq_year, 
                   "_train_test_Metrics.png")), 
  plot = tt_sum_plot, width = 14, height = 8)


###### Varibale Importance ######

var_imp_all = fread("/Users/ztttttt/Downloads/RF_US_Var_Importance_2011-20_001.csv")
var_imp_all$V1 = NULL
names(var_imp_all)[1] = "PctIncMSE"

## Update source name & change sequence
var_imp_all_plot <- 
  var_imp_all %>%
  dplyr::mutate(
    Source = case_when(
      Source == "traffic" ~ "Traffic Exhaust",
      Source == "nitrate" ~ "Secondary Nitrate",
      Source == "sulfate" ~ "Sulfate",
      Source == "pm" ~ "PM25",
      Source == "bbsoa" ~ "Biomass Burning/OA",
      Source == "dust" ~ "Dust",
      TRUE ~ Source  # Keep original value if no match
    ))

sort(unique(var_imp_all_plot$Source))

# Define the sequence of sources for later plotting
source_sequence <- 
  c("Traffic Exhaust", "Secondary Nitrate", "Sulfate", 
    "Biomass Burning/OA", "Dust", "PM25")
var_imp_all_plot <-
  var_imp_all_plot %>%
  mutate(Source = factor(Source, levels = source_sequence))

## Calculate metrics for each iteration
var_imp_iteration_perform <- 
  var_imp_all_plot %>%
  dplyr::group_by(Source, Variable) %>%
  dplyr::summarise(
    PctIncMSE = median(PctIncMSE),
    .groups = "drop"
  )
head(var_imp_iteration_perform)

## Get relative importance, ONLY FOR RF!!!
var_imp_iteration_perform %>%
  group_by(Source) %>%
  summarise(
    n = n(),
    sum_pos = sum(pmax(PctIncMSE, 0)),
    max_rel = max(pmax(PctIncMSE, 0) / sum(pmax(PctIncMSE, 0)) * 100)
  )

var_imp_iteration_perform <- 
  var_imp_iteration_perform %>%
  group_by(Source) %>%
  mutate(
    # Clip negatives to 0 before normalizing
    PctIncMSE_pos = pmax(PctIncMSE, 0),
    # Normalize to sum to 100 within each source
    Relative_Influence = PctIncMSE_pos / sum(PctIncMSE_pos) * 100
  ) %>%
  ungroup()


head(var_imp_iteration_perform)
print("Check the var_imp_iteration_perform")
summary(var_imp_iteration_perform)
dim(var_imp_iteration_perform)

# Select top 15 variables for each source
var_imp_top15 <- 
  var_imp_iteration_perform %>%
  dplyr::group_by(Source) %>%
  dplyr::slice_max(order_by = Relative_Influence, n = 15) %>%
  ungroup() %>%
  # Reorder Source factor to put PM25 at the end
  mutate(Source = factor(Source, 
                         levels = c(setdiff(unique(Source), "PM25"), "PM25")))

# head(var_imp_top10)
# unique(var_imp_top10$Source)
# print("Check the var_imp_top10")
# summary(var_imp_top10)
# dim(var_imp_top10)

###### plot the relative influence of all variables and top 10 ######

rf_var_influence_p <-
  ggplot(var_imp_iteration_perform, 
         aes(y = reorder(Variable, 
                         Relative_Influence), 
             x = Relative_Influence)) +
  geom_bar(stat = "identity",
           fill = "steelblue",
           width = 0.5) +
  # facet_wrap(.~Source, scales = "free_y") +
  facet_wrap(~Source, scales = "free_y",
             labeller = labeller(Source = c(
               "Biomass Burning/OA" = "Biomass Burning/\nOA",
               "Secondary Nitrate"   = "Secondary Nitrate",
               "Traffic Exhaust"     = "Traffic Exhaust",
               "PM25"               = "PM2.5"
             ))) +
  # coord_flip() +  # Flip coordinates to make a horizontal bar plot
  labs(title = "Variable Performance",
       y = "Predictors",
       x = "Relative Influence (%)") + 
  theme_minimal(base_size = 18)
rf_var_influence_p


rf_var_top15_inf_p <-
  ggplot(var_imp_top15, 
         # Order the bar sequence by variable and Relative_Influence for each source
         aes(y = reorder_within(Variable, 
                                Relative_Influence, 
                                Source), 
             x = Relative_Influence)) +
  geom_bar(stat = "identity",
           fill = "steelblue",
           width = 0.6) +
  # ylim(0, 100) +
  facet_wrap(~Source, scales = "free_y",
             labeller = labeller(Source = c(
               "Biomass Burning/SOA" = "Biomass Burning/\nSOA",
               "Secondary Nitrate"   = "Secondary Nitrate",
               "Traffic Exhaust"     = "Traffic Exhaust",
               "PM25"               = "PM2.5"
             ))) +
  scale_y_reordered() +  # This removes the source suffix from labels
  labs(title = "Top 15 Most Influential Variables by Source",
       y = "Predictors",
       x = "Relative Influence (%)") + 
  theme_minimal(base_size = 20)
rf_var_top15_inf_p

ggsave(
  file.path("ML_plot",
            paste0(used_model, "_", data, "_Pred_US_01_", cmaq_year, 
                   "_importance_top10.png")), 
  plot = rf_var_top15_inf_p, width = 12, height = 8)


combined_pred_perform <-
  Map(function(file, iter) {
    read_fst(file) %>%
      mutate(Iteration = iter)
  }, pred_perform_files, 1:50) %>%
  bind_rows()



