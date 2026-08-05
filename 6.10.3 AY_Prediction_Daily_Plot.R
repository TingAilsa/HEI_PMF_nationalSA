# AY_Prediction_Daily_Plot.R

library(fst)
library(data.table)
library(ggplot2)
library(sf)
library(dplyr)
library(scales)

# ---- Directory ----
# setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/Annual_combine")
setwd("/Users/ztttttt/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nationwide_SA/data/outputs/Aim3_inputs_predictions_10year_model")
getwd()

# ---- Settings ----
sources      <- c("Traffic", "Dust", "Sulfate", "Biomass", "Nitrate")
models       <- list(
  RF  = list(prefix = "RF",  model_data = "RF_Both"),
  GBM = list(prefix = "GBM", model_data = "GBM_Both"),
  CSN = list(prefix = "CSN", model_data = "RF_CSN")
)

orange_set <- c("#F7F4F0", "#FDD49E", "#FDBB84", "#FC8D59", "#E34A33", "#7A2103")
purple_set <- c("#F5F4F7", "#D8D0E4", "#B5A8CF", "#8874B3", "#5C4490", "#2E1760")
model_colors <- c("RF_Both" = "#0072B2", "GBM_Both" = "#E69F00", "RF_CSN" = "#009E73")

# ---- Load us_states (adjust path as needed) ----
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

# ---- Source name mapping ----
rename_sources <- function(dt) {
  dt %>% dplyr::mutate(
    Source = case_when(
      Source == "Biomass"           ~ "Biomass\n Burning/\n SOA",
      Source == "Traffic"           ~ "Traffic\n Exhaust",
      Source == "Sulfate"           ~ "Sulfate",
      Source == "Dust"              ~ "Dust",
      Source == "Nitrate"           ~ "Secondary\n Nitrate",
      TRUE ~ Source
    ))
}

source_levels <- c("Traffic\n Exhaust", "Secondary\n Nitrate", "Sulfate",
                   "Biomass\n Burning/\n SOA", "Dust")
model_levels  <- c("RF_Both", "GBM_Both", "RF_CSN")

# ---- Read and subset only 4 days ----
target_days <- as.Date(c("2020-01-15", "2020-04-15", "2020-07-15", "2020-10-15"))
# target_days <- as.Date(c("2019-01-15", "2019-04-15", "2019-07-15", "2019-10-15"))
# target_days <- as.Date(c("2019-02-15", "2019-05-15", "2019-08-15", "2019-11-15"))
target_year <- format(target_days[1], "%Y")  # "2019" — all 4 days same year

target_days; target_year

dt_4days <- rbindlist(lapply(sources, function(s) {
  rbindlist(lapply(names(models), function(m) {
    prefix     <- models[[m]]$prefix
    model_data <- models[[m]]$model_data
    f  <- paste0(prefix, "_Pred_US_01_Daily_mean_", s, "_", target_year, ".fst")  # 1-year file
    dt <- read_fst(f, as.data.table = TRUE)
    dt <- dt[as.Date(Date) %in% target_days]
    dt[, `:=`(Source = s, Model_Data = model_data)]
    dt
  }))
}))

write_fst(dt_4days, paste0("One_Day_6source_3model_", target_year, ".fst"))
# write_fst(dt_4days, paste0("One_Day_6source_3model_", target_year, "_2.fst"))

dt_4days_pm <- rbindlist(lapply("PM25", function(s) {
  rbindlist(lapply(names(models), function(m) {
    prefix     <- models[[m]]$prefix
    model_data <- models[[m]]$model_data
    f  <- paste0(prefix, "_Pred_US_01_Daily_mean_", s, "_", target_year, ".fst")  # 1-year file
    dt <- read_fst(f, as.data.table = TRUE)
    dt <- dt[as.Date(Date) %in% target_days]
    dt[, `:=`(Source = s, Model_Data = model_data)]
    dt
  }))
}))
write_fst(dt_4days_pm, paste0("One_Day_PM25_3model_", target_year, ".fst"))
# write_fst(dt_4days_pm, paste0("One_Day_PM25_3model_", target_year, "_2.fst"))


gc()  # clean up after all lapply iterations


# ---- Process ----

# dt_4days = read_fst("/Users/ztttttt/Downloads/One_Day_6source_3model_2019.fst")
dt_4days = read_fst("One_Day_6source_3model_2020.fst")
# head(dt_4days); dim(dt_4days); unique(dt_4days$Source)

dt_4days = rename_sources(dt_4days)
unique(dt_4days$Source)

dt_4days$Uncertainty <- dt_4days$Pred.sd / dt_4days$Predictions * 100
dt_4days             <- rename_sources(dt_4days)
dt_4days$Source      <- factor(dt_4days$Source,     levels = source_levels)
dt_4days$Model_Data  <- factor(dt_4days$Model_Data, levels = model_levels)
dt_4days$Date        <- as.Date(dt_4days$Date)
dt_4days$Day         <- factor(format(dt_4days$Date, "%b %d"),
                               levels = format(target_days, "%b %d"))
unique(dt_4days$Day)

# Use the absolute value of Uncertainty
dt_4days$Uncertainty <- abs(dt_4days$Uncertainty)
summary(dt_4days)
head(dt_4days)

# ---- Global color limits shared across figures 1 and 2 ----
pred_upper <- quantile(dt_4days$Predictions, 0.999, na.rm = TRUE)
unct_upper <- quantile(dt_4days$Uncertainty, 0.99, na.rm = TRUE)
cat("Pred limits: 0 to", pred_upper, "\n")
cat("Unct limits: 0 to", unct_upper, "\n")

# ---- Shared theme ----
map_theme <- function() {
  list(
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE),
    theme_void(base_size = 27),
    theme(
      strip.text.x      = element_text(size = 25, margin = margin(b = 6)), # face = "bold",
      strip.text.y.left = element_text(size = 25, angle = 90, margin = margin(r = 6)), # face = "bold",
      strip.placement   = "outside",
      panel.spacing     = unit(0.3, "cm"),
      panel.border      = element_blank(),
      plot.title        = element_text(size = 36, hjust = 0.5, margin = margin(b = 10)), # face = "bold",
      plot.margin       = margin(10, 10, 10, 10),
      legend.position   = "bottom",
      legend.title      = element_text(size = 27, angle = 0, hjust = 0.5), # face = "bold",
      legend.margin     = margin(t = 8)
    )
  )
}

# ---- Scale builders ----
pred_scale <- function(upper) {
  scale_fill_gradientn(
    name   = expression("SI"[ML]~"\n \u00b5g/m"^3), # unicode µ
    colors = purple_set,
    limits = c(0, upper),
    oob    = scales::squish,
    guide  = guide_colorbar(
      barwidth       = unit(10, "cm"),
      barheight      = unit(0.5, "cm"),
      title.position = "left",
      title.hjust    = 0.5,
      label.theme    = element_text(size = 25, angle = 0)))
}

unct_scale <- function(upper) {
  scale_fill_gradientn(
    name   = "Uncertainty\n (%)",
    colors = orange_set,
    limits = c(0, upper),
    oob    = scales::squish,
    guide  = guide_colorbar(
      barwidth       = unit(10, "cm"),
      barheight      = unit(0.5, "cm"),
      title.position = "left",
      title.hjust    = 0.5,
      label.theme    = element_text(size = 25, angle = 0)))
}

# ==============================================================================
# ---- FIGURE 1: One figure per day — facet Model_Data x Source ---- 
# ==============================================================================
cat("Making Figure 1...\n")

for (d in target_days) {
  d       <- as.Date(d) # d = as.Date("2019-01-15")
  d_label <- format(d, "%b %d")
  dt_day  <- subset(dt_4days, Date == d)
  
  # dt_dust = subset(dt_day, Source == "Dust")
  # dt_dust_exeUnc = subset(dt_dust, Uncertainty > 1000 | Uncertainty < -1000)
  # dt_no_dust = subset(dt_day, Source != "Dust")
  
  fig1_pred <-
    ggplot() +
    geom_tile(data = dt_day,
              aes(x = Longitude, y = Latitude, fill = Predictions),
              width = 0.1, height = 0.1) +
    geom_sf(data = us_states, fill = NA, color = "grey70", linewidth = 0.1) +
    pred_scale(pred_upper) +
    facet_grid(Model_Data ~ Source, switch = "y") +
    labs(title = paste("Daily Predictions —", target_year, d_label)) +
    map_theme()
  
  fig1_unct <-
    ggplot() +
    geom_tile(data = dt_day,
              aes(x = Longitude, y = Latitude, fill = Uncertainty),
              width = 0.1, height = 0.1) +
    geom_sf(data = us_states, fill = NA, color = "grey70", linewidth = 0.1) +
    unct_scale(unct_upper) +
    facet_grid(Model_Data ~ Source, switch = "y") +
    labs(title = paste("Daily Uncertainty —", target_year, d_label)) +
    map_theme()
  
  ggsave(paste0("ML_plot/Fig1_Daily_Pred_", target_year, "_",
                format(d, "%m%d"), ".png"),
         fig1_pred, width = 18, height = 10, dpi = 300)
  ggsave(paste0("ML_plot/Fig1_Daily_Unct_", target_year, "_",
                format(d, "%m%d"), ".png"),
         fig1_unct, width = 18, height = 10, dpi = 300)
  cat("Saved Fig1 for", as.character(d), "\n")
}

# ==============================================================================
# ---- FIGURE 2: All 4 days per modeling approach — facet Source x Day ---- 
# ==============================================================================
cat("Making Figure 2...\n")

for (md in model_levels) { # md = model_levels[1]
  dt_md <- subset(dt_4days, Model_Data == md)
  
  fig2_pred <-
    ggplot() +
    geom_tile(data = dt_md,
              aes(x = Longitude, y = Latitude, fill = Predictions),
              width = 0.1, height = 0.1) +
    geom_sf(data = us_states, fill = NA, color = "grey70", linewidth = 0.1) +
    pred_scale(pred_upper) +
    facet_grid(Source ~ Day, switch = "y") +
    labs(title = paste("Daily Predictions —", target_year, "—", md)) +
    map_theme()
  
  fig2_unct <-
    ggplot() +
    geom_tile(data = dt_md,
              aes(x = Longitude, y = Latitude, fill = Uncertainty),
              width = 0.1, height = 0.1) +
    geom_sf(data = us_states, fill = NA, color = "grey70", linewidth = 0.1) +
    unct_scale(unct_upper) +
    facet_grid(Source ~ Day, switch = "y") +
    labs(title = paste("Daily Uncertainty —", target_year, "—", md)) +
    map_theme()
  
  ggsave(paste0("ML_plot/Fig2_4days_Pred_", target_year, "_", md, ".png"),
         fig2_pred, width = 16, height = 14, dpi = 300)
  ggsave(paste0("ML_plot/Fig2_4days_Unct_", target_year, "_", md, ".png"),
         fig2_unct, width = 16, height = 14, dpi = 300)
  cat("Saved Fig2 for", md, "\n")
}

# ==============================================================================
# ---- FIGURE 3: Time series — US mean across all grids per day ---- 
# ==============================================================================
cat("Making Figure 3...\n")

dt_ts <- dt_4days[, .(
  Predictions = mean(Predictions, na.rm = TRUE),
  Uncertainty = mean(Uncertainty, na.rm = TRUE)
), by = .(Date, Day, Source, Model_Data)]

fig3_pred <-
  ggplot(dt_ts, aes(x = Day, y = Predictions,
                    color = Model_Data, group = Model_Data)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(values = model_colors, name = "Model") +
  facet_wrap(~ Source, ncol = 2, scales = "free_y") +
  labs(title = paste("US Mean Daily Predictions —", target_year),
       x = NULL, y = expression("Prediction (\u00b5g/m"^3*")")) +
  theme_bw(base_size = 14) +
  theme(
    strip.text       = element_text(size = 14, face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

fig3_unct <-
  ggplot(dt_ts, aes(x = Day, y = Uncertainty,
                    color = Model_Data, group = Model_Data)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(values = model_colors, name = "Model") +
  facet_wrap(~ Source, ncol = 2, scales = "free_y") +
  labs(title = paste("US Mean Daily Uncertainty —", target_year),
       x = NULL, y = "Uncertainty (%)") +
  theme_bw(base_size = 14) +
  theme(
    strip.text       = element_text(size = 14, face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(paste0("ML_plot/Fig3_TimeSeries_Pred_", target_year, ".png"),
       fig3_pred, width = 14, height = 16, dpi = 300)
ggsave(paste0("ML_plot/Fig3_TimeSeries_Unct_", target_year, ".png"),
       fig3_unct, width = 14, height = 16, dpi = 300)

cat("All done for year:", target_year, "\n")