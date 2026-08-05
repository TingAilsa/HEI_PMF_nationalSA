# AY_Prediction_annual_month_Plot.R

library(fst)
library(data.table)
library(ggplot2)
library(sf)
library(dplyr)
library(scales)

# ---- Arguments from SLURM ----
args     <- commandArgs(trailingOnly = TRUE)
map_type <- args[1]             # map_type = "annual" or map_type = "monthly"
index    <- as.integer(args[2]) # year (2011-2020) or month (1-12), e.g., index = 2020

# ---- Temp dir ----
tmpdir <- file.path(Sys.getenv("SCRATCH"), "rtmp", Sys.getenv("SLURM_ARRAY_TASK_ID"))
dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TMPDIR = tmpdir, TMP = tmpdir, TEMP = tmpdir)

# ---- Directory ----
# setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/Annual_combine")
setwd("/Users/ztttttt/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nationwide_SA/data/outputs/Aim3_inputs_predictions_10year_model")
getwd()

# ---- Load us_states (adjust path as needed) ----
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

# ---- Color sets ----
orange_set <- c("#F7F4F0", "#FDD49E", "#FDBB84", "#FC8D59", "#E34A33", "#7A2103")
purple_set <- c("#F5F4F7", "#D8D0E4", "#B5A8CF", "#8874B3", "#5C4490", "#2E1760")

# ---- Shared processing function ----
# ---- Name changing, reordering, and exclude PM2.5 ----
process_dt <- function(dt) {
  dt$Model_Data  <- paste0(dt$Model, "_", dt$Dataset)
  dt$Uncertainty <- dt$Pred.sd / dt$Predictions * 100
  dt <- dt %>%
    dplyr::mutate(
      Source = case_when(
        Source == "Biomass"           ~ "Biomass Burning/\n SOA",
        Source == "Traffic"           ~ "Traffic Exhaust",
        Source == "Secondary Sulfate" ~ "Sulfate",
        Source == "Soil/Dust"         ~ "Dust",
        Source == "Nitrate"           ~ "Secondary Nitrate",
        TRUE ~ Source
      ))
  dt$Model_Data <- factor(dt$Model_Data,
                          levels = c("RF_Both", "GBM_Both", "RF_CSN"))
  dt$Source     <- factor(dt$Source,
                          levels = c("Traffic Exhaust", "Secondary Nitrate", "Sulfate",
                                     "Biomass Burning/\n SOA", "Dust", "PM25"))
  subset(dt, Source != "PM25")
}

# ---- Shared theme function ----
map_theme <- function() {
  list(
    facet_grid(Model_Data ~ Source, switch = "y"),
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE),
    theme_void(base_size = 19),
    theme(
      strip.text.x      = element_text(size = 19, face = "bold", margin = margin(b = 6)),
      strip.text.y.left = element_text(size = 19, face = "bold", angle = 90, margin = margin(r = 6)),
      strip.placement   = "outside",
      panel.spacing     = unit(0.3, "cm"),
      panel.border      = element_blank(),
      plot.title        = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(b = 10)),
      plot.margin       = margin(10, 10, 10, 10),
      legend.position   = "bottom",
      legend.title      = element_text(size = 19, face = "bold", angle = 0, hjust = 0.5),
      legend.text       = element_text(size = 17),
      legend.margin     = margin(t = 8)
    )
  )
}

# ---- Plotting function ----
make_maps <- function(dt_noPM, label, out_pred, out_unct) {
  
  # Prediction map
  pred_map <-
    ggplot() +
    geom_tile(data = dt_noPM,
              aes(x = Longitude, y = Latitude, fill = Predictions),
              width = 0.1, height = 0.1) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", linewidth = 0.1) +
    scale_fill_gradientn(
      name   = expression("SI"[ML]~"\n \u00b5g/m"^3), # unicode µ
      colors = purple_set,
      limits = c(0, quantile(dt_noPM$Predictions, 0.95, na.rm = TRUE)),
      oob    = scales::squish,
      guide  = guide_colorbar(
        barwidth       = unit(10, "cm"),
        barheight      = unit(0.5, "cm"),
        # label.theme    = element_text(size = 11, angle = 0),
        title.position = "left",
        title.hjust    = 0.5
      )) +
    labs(title = paste("Predictions by source —", label)) +
    map_theme()
  
  # Uncertainty map
  unct_map <-
    ggplot() +
    geom_tile(data = dt_noPM,
              aes(x = Longitude, y = Latitude, fill = Uncertainty),
              width = 0.1, height = 0.1) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", linewidth = 0.1) +
    scale_fill_gradientn(
      name   = "Uncertainty\n (%)",
      colors = orange_set,
      limits = c(0, quantile(dt_noPM$Uncertainty, 0.85, na.rm = TRUE)),
      oob    = scales::squish,
      guide  = guide_colorbar(
        barwidth       = unit(10, "cm"),
        barheight      = unit(0.5, "cm"),
        # label.theme    = element_text(size = 11, angle = 0),
        title.position = "left",
        title.hjust    = 0.5
      )) +
    labs(title = paste("Uncertainty by source —", label)) +
    map_theme()
  
  ggsave(out_pred, pred_map, width = 18, height = 10, dpi = 300)
  ggsave(out_unct, unct_map, width = 18, height = 10, dpi = 300)
  cat("Saved:", out_pred, "\n")
  cat("Saved:", out_unct, "\n")
}

# ---- Load and subset data ----
if (map_type == "annual") {
  dt  <- read_fst("Combined_US_01_annual.fst", as.data.table = TRUE)
  dt  <- dt[Year == index]
  # dt <- data.table:::`[.data.table`(dt, Year == 2020)
  label    <- as.character(index)
  out_pred <- paste0("ML_plot/Map_Annual_Predictions_", index, ".png")
  out_unct <- paste0("ML_plot/Map_Annual_Uncertainty_", index, ".png")
  
} else {
  dt  <- read_fst("Combined_US_01_month.fst", as.data.table = TRUE)
  dt  <- dt[Month == index]
  label    <- month.name[index]
  out_pred <- paste0("ML_plot/Map_Monthly_Predictions_", sprintf("%02d", index), "_", label, ".png")
  out_unct <- paste0("ML_plot/Map_Monthly_Uncertainty_", sprintf("%02d", index), "_", label, ".png")
}

dt_noPM <- process_dt(dt)
rm(dt); gc()

make_maps(dt_noPM, label, out_pred, out_unct)
cat("Done:", map_type, index, "\n")
