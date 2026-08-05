# AY_Prediction_ym_ts_Plot.R

library(fst)
library(data.table)
library(ggplot2)
library(sf)
library(dplyr)
library(scales)

# ---- Arguments from SLURM ----
args           <- commandArgs(trailingOnly = TRUE)
current_source <- args[1]   # e.g. "Traffic"
current_prefix <- args[2]   # e.g. "RF"
current_md     <- args[3]   # e.g. "RF_Both"

# ---- Directory ----
# setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/Annual_combine")
setwd("/Users/ztttttt/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nationwide_SA/data/outputs/Aim3_inputs_predictions_10year_model")
getwd()

# ---- Settings ----
years  <- 2011:2020
months <- 1:12

orange_set <- c("#F7F4F0", "#FDD49E", "#FDBB84", "#FC8D59", "#E34A33", "#7A2103")
purple_set <- c("#F5F4F7", "#D8D0E4", "#B5A8CF", "#8874B3", "#5C4490", "#2E1760")

us_states  <- sf::st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))
dir.create("ML_plot/Annual",  recursive = TRUE, showWarnings = FALSE)
dir.create("ML_plot/Monthly", recursive = TRUE, showWarnings = FALSE)

# ---- Source name mapping ----
rename_source <- function(s) {
  case_when(
    s == "Biomass"  ~ "Biomass Burning/SOA",
    s == "Traffic"  ~ "Traffic Exhaust",
    s == "Sulfate"  ~ "Sulfate",
    s == "Dust"     ~ "Dust",
    s == "Nitrate"  ~ "Secondary Nitrate",
    s == "PM25"     ~ "PM2.5",
    TRUE ~ s
  )
}

# ---- Shared theme ----
map_theme <- function(base = 13) {
  list(
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE),
    theme_void(base_size = base),
    theme(
      strip.text        = element_text(size = base, face = "bold",
                                       margin = margin(b = 2, t = 4)),
      strip.placement   = "outside",
      panel.spacing     = unit(0.2, "cm"),
      panel.border      = element_blank(),
      plot.title        = element_text(size = base + 2, face = "bold",
                                       hjust = 0.5, margin = margin(b = 4)),
      plot.subtitle     = element_text(size = base, hjust = 0.5,
                                       margin = margin(b = 6)),
      plot.margin       = margin(10, 10, 10, 10),
      legend.position   = "bottom",
      legend.title      = element_text(size = base, face = "bold"),
      legend.margin     = margin(t = 6)
    )
  )
}

# ---- Scale builders ----
pred_scale <- function(upper) {
  scale_fill_gradientn(
    name   = expression("\u00b5g/m"^3), # unicode µ
    colors = purple_set,
    limits = c(0, upper),
    oob    = scales::squish,
    guide  = guide_colorbar(
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.4, "cm"),
      title.position = "left",
      title.hjust    = 0.5,
      label.theme    = element_text(size = 10)))
}

unct_scale <- function(upper) {
  scale_fill_gradientn(
    name   = "Uncertainty (%)",
    colors = orange_set,
    limits = c(0, upper),
    oob    = scales::squish,
    guide  = guide_colorbar(
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.4, "cm"),
      title.position = "left",
      title.hjust    = 0.5,
      label.theme    = element_text(size = 10)))
}

# ---- Compute mean +/- sd label per panel ----
# mean and sd across all grids for each Label (year or month)
make_labels <- function(dt, fill_var) {
  dt[, .(
    mean_val = mean(get(fill_var), na.rm = TRUE),
    sd_val   = sd(get(fill_var),   na.rm = TRUE)
  ), by = Label][
    , stat_label := paste0(round(mean_val, 2), " \u00b1 ", round(sd_val, 2))
  ]
}

# ---- Generic plot function with mean +/- sd ----
make_facet_map <- function(dt, fill_var, scale_fn, upper,
                           title, subtitle, ncol, base = 13) {
  # compute stats per panel
  stat_dt <- make_labels(dt, fill_var)
  
  # dummy coordinates for label placement (center of map)
  stat_dt[, Longitude := -97.5]   # center of US longitude
  stat_dt[, Latitude  :=  23.0]   # just below the map bottom (ylim starts at 24)
  
  ggplot() +
    geom_tile(data = dt,
              aes(x = Longitude, y = Latitude, fill = .data[[fill_var]]),
              width = 0.1, height = 0.1) +
    geom_sf(data = us_states, fill = NA, color = "grey70", linewidth = 0.1) +
    # mean +/- sd label below each map, above the panel bottom
    geom_text(data = stat_dt,
              aes(x = Longitude, y = Latitude, label = stat_label),
              size = base * 0.22,    # scale with base font size
              fontface = "plain",
              hjust = 0.5, vjust = 1) +
    scale_fn(upper) +
    facet_wrap(~ Label, ncol = ncol) +
    labs(title = title, subtitle = subtitle) +
    map_theme(base)
}

source_label <- rename_source(current_source)
title_base   <- paste0(source_label, " — ", current_md)

# ==============================================================================
# ANNUAL
# ==============================================================================
cat("Reading annual data...\n")

dt_annual <- 
  rbindlist(lapply(years, function(y) {
  f  <- paste0(current_prefix, "_Pred_US_01_Daily_mean_", current_source, "_", y, ".fst")
  dt <- read_fst(f, as.data.table = TRUE)
  dt <- dt[, .(Predictions = mean(Predictions, na.rm = TRUE),
               Pred.sd     = mean(Pred.sd,     na.rm = TRUE)),
           by = .(Longitude, Latitude)]
  dt[, Year := y]
  dt
}))
gc()

# dt_annual <- read_fst("Combined_US_01_annual.fst")
head(dt_annual)

dt_annual$Uncertainty <- dt_annual$Pred.sd / dt_annual$Predictions * 100
dt_annual$Label       <- factor(dt_annual$Year, levels = years)

pred_upper_a <- quantile(dt_annual$Predictions, 0.999, na.rm = TRUE)
unct_upper_a <- quantile(dt_annual$Uncertainty, 0.995, na.rm = TRUE)

fig_ann_pred <- make_facet_map(
  dt       = dt_annual,
  fill_var = "Predictions",
  scale_fn = pred_scale,
  upper    = pred_upper_a,
  title    = paste(title_base, "| Annual Predictions"),
  subtitle = "2011\u20132020",
  ncol     = 5
)

fig_ann_unct <- make_facet_map(
  dt       = dt_annual,
  fill_var = "Uncertainty",
  scale_fn = unct_scale,
  upper    = unct_upper_a,
  title    = paste(title_base, "| Annual Uncertainty"),
  subtitle = "2011\u20132020",
  ncol     = 5
)

ggsave(paste0("ML_plot/Annual/Ann_Pred_", current_source, "_", current_md, ".png"),
       fig_ann_pred, width = 20, height = 9, dpi = 300)
ggsave(paste0("ML_plot/Annual/Ann_Unct_", current_source, "_", current_md, ".png"),
       fig_ann_unct, width = 20, height = 9, dpi = 300)
cat("Saved annual figures\n")

rm(dt_annual); gc()

# ==============================================================================
# MONTHLY
# ==============================================================================
cat("Reading monthly data...\n")

dt_monthly <- rbindlist(lapply(years, function(y) {
  f  <- paste0(current_prefix, "_Pred_US_01_Daily_mean_", current_source, "_", y, ".fst")
  dt <- read_fst(f, as.data.table = TRUE)
  dt[, Date  := as.Date(Date)]
  dt[, Month := as.integer(format(Date, "%m"))]
  dt <- dt[, .(Predictions = mean(Predictions, na.rm = TRUE),
               Pred.sd     = mean(Pred.sd,     na.rm = TRUE)),
           by = .(Longitude, Latitude, Month)]
  dt
}))
gc()

dt_monthly <- dt_monthly[, .(Predictions = mean(Predictions, na.rm = TRUE),
                             Pred.sd     = mean(Pred.sd,     na.rm = TRUE)),
                         by = .(Longitude, Latitude, Month)]

dt_monthly$Uncertainty <- dt_monthly$Pred.sd / dt_monthly$Predictions * 100
dt_monthly$Label       <- factor(dt_monthly$Month,
                                 levels = 1:12,
                                 labels = month.abb)

pred_upper_m <- quantile(dt_monthly$Predictions, 0.999, na.rm = TRUE)
unct_upper_m <- quantile(dt_monthly$Uncertainty, 0.995, na.rm = TRUE)

fig_mon_pred <- make_facet_map(
  dt       = dt_monthly,
  fill_var = "Predictions",
  scale_fn = pred_scale,
  upper    = pred_upper_m,
  title    = paste(title_base, "| Monthly Predictions"),
  subtitle = "2011\u20132020 average",
  ncol     = 4
)

fig_mon_unct <- make_facet_map(
  dt       = dt_monthly,
  fill_var = "Uncertainty",
  scale_fn = unct_scale,
  upper    = unct_upper_m,
  title    = paste(title_base, "| Monthly Uncertainty"),
  subtitle = "2011\u20132020 average",
  ncol     = 4
)

ggsave(paste0("ML_plot/Monthly/Mon_Pred_", current_source, "_", current_md, ".png"),
       fig_mon_pred, width = 16, height = 13, dpi = 300)
ggsave(paste0("ML_plot/Monthly/Mon_Unct_", current_source, "_", current_md, ".png"),
       fig_mon_unct, width = 16, height = 13, dpi = 300)
cat("Saved monthly figures\n")

cat("All done — Source:", current_source, "| Model:", current_md, "\n")