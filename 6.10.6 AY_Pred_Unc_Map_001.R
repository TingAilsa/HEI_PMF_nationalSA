# AY_Pred_Unc_Map_001.R

# RF_001_Map_Plot.R
# Maps of 0.01-degree RF predictions (Pred) and uncertainty (Unc), native grid
#
# Usage (e.g., from a SLURM array):
#   Rscript RF_001_Map_Plot.R annual  2020   # one year (2011-2020)
#   Rscript RF_001_Map_Plot.R monthly 7      # one calendar month (1-12), 2011-2020 average
#   Rscript RF_001_Map_Plot.R overall        # whole-period 2011-2020 average
#   Rscript RF_001_Map_Plot.R daily          # the four selected days in 2020
#   Rscript RF_001_Map_Plot.R annual_by_source bbsoa   # one source (or pm), 10 years in one figure
#
# Each run makes one figure with the five sources (PM2.5 excluded)
# and a separate figure for PM2.5.

library(fst)
library(data.table)
library(ggplot2)
library(sf)
library(scales)

# ---- Arguments from SLURM ----
args      <- commandArgs(trailingOnly = TRUE)
map_type  <- args[1]
index_raw <- if (length(args) >= 2) args[2] else NA_character_     # year, month or source name
index     <- suppressWarnings(as.integer(index_raw))
stopifnot(map_type %in% c("annual", "monthly", "overall", "daily", "annual_by_source"))

# ---- Temp dir ----
task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID", unset = "local")
tmpdir  <- file.path(Sys.getenv("SCRATCH"), "rtmp", task_id)
dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TMPDIR = tmpdir, TMP = tmpdir, TEMP = tmpdir)

# ---- Directory ----
setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ML_Final_Input_001/Annual_combine")
getwd()
data_path <- getwd()
plot_path <- "ML_plot/"
# dir.create(plot_path, recursive = TRUE, showWarnings = FALSE)

# ---- Output formats ----
# PDF keeps the native 0.01-deg grid: zoom in to see every grid cell.
# PNG at 300 dpi is fine for slides/quick viewing, but cells are smaller than a pixel.
out_formats <- c("png", "pdf")
png_dpi     <- 300

# ---- Source names: file name -> label ----
# One-line labels for top strips, two-line labels for the rotated left strips
src_lab1 <- c(traffic = "Traffic Exhaust",  nitrate = "Secondary Nitrate", sulfate = "Sulfate",
              bbsoa   = "Biomass Burning/OA", dust  = "Dust")
src_lab2 <- c(traffic = "Traffic\nExhaust", nitrate = "Secondary\nNitrate", sulfate = "Sulfate",
              bbsoa   = "Biomass\nBurning/OA", dust = "Dust")
two_line_labeller <- as_labeller(setNames(unname(src_lab2), unname(src_lab1)))
pm_file <- "pm"

# ---- US states ----
us_states <- USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c("HI", "AK", "AS", "GU", "MP", "PR", "VI")), ]

# ---- Color sets ----
orange_set <- c("#F7F4F0", "#FDD49E", "#FDBB84", "#FC8D59", "#E34A33", "#7A2103")
purple_set <- c("#F5F4F7", "#D8D0E4", "#B5A8CF", "#8874B3", "#5C4490", "#2E1760")

# ---- Legend titles ----
pred_name_src <- expression("SI"[ML]~"\n µg/m"^3)   # unicode µ
pred_name_pm  <- expression("PM"[2.5]~"\n µg/m"^3)
unct_name     <- "Uncertainty\n (%)"

# ==============================================================================
# ---- Helpers ----
# ==============================================================================

# Read one Pred and one Unc column per source from the averaged files
read_avg <- function(srcs, file_fmt, pred_col, unc_col) {
  rbindlist(lapply(srcs, function(s) {
    dt <- read_fst(file.path(data_path, sprintf(file_fmt, s)),
                   columns = c("Longitude", "Latitude", pred_col, unc_col),
                   as.data.table = TRUE)
    setnames(dt, c(pred_col, unc_col), c("Predictions", "Uncertainty"))
    dt[, `:=`(Longitude   = round(Longitude, 3),   # exact regular grid for geom_raster
              Latitude    = round(Latitude, 3),
              Uncertainty = abs(Uncertainty))]     # negative predictions give negative Unc
    if (s %in% names(src_lab1)) dt[, Source := factor(src_lab1[[s]], levels = unname(src_lab1))]
    cat("Read:", s, "-", nrow(dt), "rows\n")
    dt
  }))
}

# Read the four selected days for one source, in long format (one row per grid point and day)
read_4days <- function(s, sel_days, day_labels) {
  dt <- read_fst(file.path(data_path, sprintf("%s_US_RF_Pred_Unc_4days_in_2020.fst", s)),
                 columns = c("Longitude", "Latitude",
                             paste0("Pred_", sel_days), paste0("Unc_", sel_days)),
                 as.data.table = TRUE)
  dt <- melt(dt, id.vars = c("Longitude", "Latitude"),
             measure.vars = list(paste0("Pred_", sel_days), paste0("Unc_", sel_days)),
             value.name = c("Predictions", "Uncertainty"), variable.name = "Day")
  dt[, `:=`(Day         = factor(day_labels[as.integer(Day)], levels = day_labels),
            Longitude   = round(Longitude, 3),
            Latitude    = round(Latitude, 3),
            Uncertainty = abs(Uncertainty))]
  if (s %in% names(src_lab1)) dt[, Source := factor(src_lab1[[s]], levels = unname(src_lab1))]
  cat("Read:", s, "\n")
  dt
}

# ---- Shared theme ----
map_theme <- function(bs = 19) {
  list(
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE),
    theme_void(base_size = bs),
    theme(
      strip.text.x      = element_text(size = bs, face = "bold", margin = margin(b = 6)),
      strip.text.y.left = element_text(size = bs, face = "bold", angle = 90, margin = margin(r = 6)),
      strip.placement   = "outside",
      panel.spacing     = unit(0.3, "cm"),
      panel.border      = element_blank(),
      plot.title        = element_text(size = bs + 3, face = "bold", hjust = 0.5, margin = margin(b = 10)),
      plot.margin       = margin(10, 10, 10, 10),
      legend.position   = "bottom",
      legend.title      = element_text(size = bs, face = "bold", angle = 0, hjust = 0.5),
      legend.text       = element_text(size = bs - 2),
      legend.margin     = margin(t = 8)
    )
  )
}

# ---- Scale builders (na.value = NA keeps cells without data transparent) ----
fill_scale <- function(upper, name, colors) {
  scale_fill_gradientn(
    name     = name,
    colors   = colors,
    limits   = c(0, upper),
    oob      = scales::squish,
    na.value = NA,
    guide    = guide_colorbar(barwidth = unit(10, "cm"), barheight = unit(0.5, "cm"),
                              title.position = "left", title.hjust = 0.5))
}
pred_scale <- function(upper, name = pred_name_src) fill_scale(upper, name, purple_set)
unct_scale <- function(upper)                       fill_scale(upper, unct_name, orange_set)

# ---- Map builder: native grid with geom_raster ----
make_map <- function(dt, fill_var, scale, title, facet = NULL, bs = 19) {
  ggplot() +
    geom_raster(data = dt, aes(x = Longitude, y = Latitude, fill = .data[[fill_var]]),
                interpolate = FALSE) +
    geom_sf(data = us_states, fill = NA, color = "grey70", linewidth = 0.1) +
    scale +
    facet +
    labs(title = title) +
    map_theme(bs)
}

# ---- Save in all requested formats ----
save_fig <- function(p, stub, width, height) {
  for (fmt in out_formats) {
    f <- paste0(plot_path, stub, ".", fmt)
    if (fmt == "pdf") {
      ggsave(f, p, width = width, height = height, device = cairo_pdf)
    } else {
      ggsave(f, p, width = width, height = height, dpi = png_dpi)
    }
    cat("Saved:", f, "\n")
  }
}

# PM2.5 titles with subscript, e.g. "PM2.5 Predictions — 2020"
pm_title <- function(what, label) bquote(PM[2.5]~.(what)~"—"~.(label))

# ==============================================================================
# ---- Annual / monthly / overall maps ----
# ==============================================================================
if (map_type %in% c("annual", "monthly", "overall")) {
  
  if (map_type == "annual") {
    stopifnot(index %in% 2011:2020)
    file_fmt <- "%s_US_RF_Annual_Avg_Pred_Unc_2011-2020.fst"
    pred_col <- paste0("Pred_", index); unc_col <- paste0("Unc_", index)
    label    <- as.character(index)
    tag      <- paste0("Annual_", index)
    
  } else if (map_type == "monthly") {
    stopifnot(index %in% 1:12)
    file_fmt <- "%s_US_RF_Monthly_Avg_Pred_Unc_2011-2020.fst"
    pred_col <- paste0("Pred_", month.abb[index]); unc_col <- paste0("Unc_", month.abb[index])
    label    <- paste(month.name[index], "(2011-2020)")
    tag      <- sprintf("Monthly_%02d_%s", index, month.name[index])
    
  } else {
    file_fmt <- "%s_US_RF_Overall_Avg_Pred_Unc_2011-2020.fst"
    pred_col <- "Pred_2011_2020"; unc_col <- "Unc_2011_2020"
    label    <- "2011-2020"
    tag      <- "Overall_2011-2020"
  }
  
  # ---- Five sources in one figure ----
  dt <- read_avg(names(src_lab1), file_fmt, pred_col, unc_col)
  pred_upper <- quantile(dt$Predictions, 0.95, na.rm = TRUE)
  unct_upper <- quantile(dt$Uncertainty, 0.85, na.rm = TRUE)
  cat("Sources - Pred limits: 0 to", pred_upper, "| Unct limits: 0 to", unct_upper, "\n")
  
  facet <- facet_wrap(~ Source, ncol = 3)
  save_fig(make_map(dt, "Predictions", pred_scale(pred_upper),
                    paste("Predictions by source —", label), facet),
           paste0("RF_001_Map_Pred_Sources_", tag), 18, 9)
  save_fig(make_map(dt, "Uncertainty", unct_scale(unct_upper),
                    paste("Uncertainty by source —", label), facet),
           paste0("RF_001_Map_Unct_Sources_", tag), 18, 9)
  rm(dt); gc()
  
  # ---- PM2.5 separately ----
  dt_pm <- read_avg(pm_file, file_fmt, pred_col, unc_col)
  pm_pred_upper <- quantile(dt_pm$Predictions, 0.95, na.rm = TRUE)
  pm_unct_upper <- quantile(dt_pm$Uncertainty, 0.85, na.rm = TRUE)
  cat("PM2.5 - Pred limits: 0 to", pm_pred_upper, "| Unct limits: 0 to", pm_unct_upper, "\n")
  
  save_fig(make_map(dt_pm, "Predictions", pred_scale(pm_pred_upper, pred_name_pm),
                    pm_title("Predictions", label)),
           paste0("RF_001_Map_Pred_PM25_", tag), 12, 6.5)
  save_fig(make_map(dt_pm, "Uncertainty", unct_scale(pm_unct_upper),
                    pm_title("Uncertainty", label)),
           paste0("RF_001_Map_Unct_PM25_", tag), 12, 6.5)
  rm(dt_pm); gc()
}

# ==============================================================================
# ---- Daily maps: four selected days in 2020 ----
# ==============================================================================
if (map_type == "daily") {
  
  sel_days    <- c("20200117", "20200417", "20200717", "20201017")
  target_year <- substr(sel_days[1], 1, 4)
  day_labels  <- format(as.Date(sel_days, "%Y%m%d"), "%b %d")
  
  ts_theme <- theme_bw(base_size = 14) +
    theme(strip.text       = element_text(size = 14, face = "bold"),
          axis.text.x      = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank())
  
  # ---- Five sources ----
  dt_4days <- rbindlist(lapply(names(src_lab1), read_4days,
                               sel_days = sel_days, day_labels = day_labels))
  gc()
  
  # US mean per day (native grid)
  dt_ts <- dt_4days[, .(Predictions = mean(Predictions, na.rm = TRUE),
                        Uncertainty = mean(Uncertainty, na.rm = TRUE)), by = .(Source, Day)]
  
  # Global color limits shared across Figures 1 and 2
  pred_upper <- quantile(dt_4days$Predictions, 0.999, na.rm = TRUE)
  unct_upper <- quantile(dt_4days$Uncertainty, 0.99,  na.rm = TRUE)
  cat("Sources - Pred limits: 0 to", pred_upper, "| Unct limits: 0 to", unct_upper, "\n")
  
  # FIGURE 1: one figure per day, facet by source
  for (k in seq_along(sel_days)) {
    dt_day <- dt_4days[Day == day_labels[k]]
    facet  <- facet_wrap(~ Source, ncol = 3)
    save_fig(make_map(dt_day, "Predictions", pred_scale(pred_upper),
                      paste("Daily Predictions —", target_year, day_labels[k]), facet),
             paste0("RF_001_Fig1_Daily_Pred_Sources_", sel_days[k]), 18, 9)
    save_fig(make_map(dt_day, "Uncertainty", unct_scale(unct_upper),
                      paste("Daily Uncertainty —", target_year, day_labels[k]), facet),
             paste0("RF_001_Fig1_Daily_Unct_Sources_", sel_days[k]), 18, 9)
    rm(dt_day); gc()
  }
  
  # FIGURE 2: all 4 days, facet Source x Day (two-line source names on the left)
  facet <- facet_grid(Source ~ Day, switch = "y",
                      labeller = labeller(Source = two_line_labeller))
  save_fig(make_map(dt_4days, "Predictions", pred_scale(pred_upper),
                    paste("Daily Predictions —", target_year), facet, bs = 25),
           paste0("RF_001_Fig2_4days_Pred_Sources_", target_year), 16, 14)
  save_fig(make_map(dt_4days, "Uncertainty", unct_scale(unct_upper),
                    paste("Daily Uncertainty —", target_year), facet, bs = 25),
           paste0("RF_001_Fig2_4days_Unct_Sources_", target_year), 16, 14)
  rm(dt_4days); gc()
  
  # FIGURE 3: US mean per day
  fig3_pred <-
    ggplot(dt_ts, aes(x = Day, y = Predictions, group = Source)) +
    geom_line(linewidth = 0.8, color = "#5C4490") +
    geom_point(size = 3, color = "#5C4490") +
    facet_wrap(~ Source, ncol = 2, scales = "free_y") +
    labs(title = paste("US Mean Daily Predictions —", target_year),
         x = NULL, y = expression("Prediction (µg/m"^3*")")) +
    ts_theme
  fig3_unct <-
    ggplot(dt_ts, aes(x = Day, y = Uncertainty, group = Source)) +
    geom_line(linewidth = 0.8, color = "#E34A33") +
    geom_point(size = 3, color = "#E34A33") +
    facet_wrap(~ Source, ncol = 2, scales = "free_y") +
    labs(title = paste("US Mean Daily Uncertainty —", target_year),
         x = NULL, y = "Uncertainty (%)") +
    ts_theme
  save_fig(fig3_pred, paste0("RF_001_Fig3_TimeSeries_Pred_Sources_", target_year), 10, 10)
  save_fig(fig3_unct, paste0("RF_001_Fig3_TimeSeries_Unct_Sources_", target_year), 10, 10)
  
  # ---- PM2.5 separately ----
  dt_pm <- read_4days(pm_file, sel_days, day_labels)
  pm_ts <- dt_pm[, .(Predictions = mean(Predictions, na.rm = TRUE),
                     Uncertainty = mean(Uncertainty, na.rm = TRUE)), by = Day]
  
  pm_pred_upper <- quantile(dt_pm$Predictions, 0.999, na.rm = TRUE)
  pm_unct_upper <- quantile(dt_pm$Uncertainty, 0.99,  na.rm = TRUE)
  cat("PM2.5 - Pred limits: 0 to", pm_pred_upper, "| Unct limits: 0 to", pm_unct_upper, "\n")
  
  # Four days in one figure
  facet <- facet_wrap(~ Day, ncol = 2)
  save_fig(make_map(dt_pm, "Predictions", pred_scale(pm_pred_upper, pred_name_pm),
                    pm_title("Daily Predictions", target_year), facet),
           paste0("RF_001_Fig2_4days_Pred_PM25_", target_year), 14, 9)
  save_fig(make_map(dt_pm, "Uncertainty", unct_scale(pm_unct_upper),
                    pm_title("Daily Uncertainty", target_year), facet),
           paste0("RF_001_Fig2_4days_Unct_PM25_", target_year), 14, 9)
  rm(dt_pm); gc()
  
  # US mean per day
  fig3_pm_pred <-
    ggplot(pm_ts, aes(x = Day, y = Predictions, group = 1)) +
    geom_line(linewidth = 0.8, color = "#5C4490") +
    geom_point(size = 3, color = "#5C4490") +
    labs(title = pm_title("US Mean Daily Predictions", target_year),
         x = NULL, y = expression("PM"[2.5]~"(µg/m"^3*")")) +
    ts_theme
  fig3_pm_unct <-
    ggplot(pm_ts, aes(x = Day, y = Uncertainty, group = 1)) +
    geom_line(linewidth = 0.8, color = "#E34A33") +
    geom_point(size = 3, color = "#E34A33") +
    labs(title = pm_title("US Mean Daily Uncertainty", target_year),
         x = NULL, y = "Uncertainty (%)") +
    ts_theme
  save_fig(fig3_pm_pred, paste0("RF_001_Fig3_TimeSeries_Pred_PM25_", target_year), 6, 5)
  save_fig(fig3_pm_unct, paste0("RF_001_Fig3_TimeSeries_Unct_PM25_", target_year), 6, 5)
}

# ==============================================================================
# ---- Annual maps by source: 10 years (2011-2020) in one figure ----
# ==============================================================================
if (map_type == "annual_by_source") {
  
  s <- index_raw
  stopifnot(s %in% c(names(src_lab1), pm_file))
  years   <- 2011:2020
  is_pm   <- s == pm_file
  s_label <- if (is_pm) "PM2.5" else src_lab1[[s]]
  s_tag   <- if (is_pm) "PM25"  else gsub("[^A-Za-z]", "", src_lab1[[s]])
  f_in    <- file.path(data_path, sprintf("%s_US_RF_Annual_Avg_Pred_Unc_2011-2020.fst", s))
  
  # Read one variable (Pred or Unc) for all 10 years, in long format
  read_years <- function(prefix, value_name) {
    dt <- read_fst(f_in, columns = c("Longitude", "Latitude", paste0(prefix, years)),
                   as.data.table = TRUE)
    dt <- melt(dt, id.vars = c("Longitude", "Latitude"),
               variable.name = "Year", value.name = value_name)
    dt[, `:=`(Year      = factor(sub(prefix, "", Year), levels = as.character(years)),
              Longitude = round(Longitude, 3),
              Latitude  = round(Latitude, 3))]
    dt
  }
  
  title_for <- function(what) {
    if (is_pm) pm_title(paste("Annual", what), "2011-2020")
    else paste(s_label, "— Annual", what, "2011-2020")
  }
  
  facet <- facet_wrap(~ Year, ncol = 5)
  
  # ---- Predictions (shared color scale across the 10 years) ----
  dt <- read_years("Pred_", "Predictions")
  pred_upper <- quantile(dt$Predictions, 0.95, na.rm = TRUE)
  cat(s_label, "- Pred limits: 0 to", pred_upper, "\n")
  save_fig(make_map(dt, "Predictions",
                    pred_scale(pred_upper, if (is_pm) pred_name_pm else pred_name_src),
                    title_for("Predictions"), facet),
           paste0("RF_001_Map_Pred_Annual_2011-2020_", s_tag), 24, 8)
  rm(dt); gc()
  
  # ---- Uncertainty ----
  dt <- read_years("Unc_", "Uncertainty")
  dt[, Uncertainty := abs(Uncertainty)]   # negative predictions give negative Unc
  unct_upper <- quantile(dt$Uncertainty, 0.85, na.rm = TRUE)
  cat(s_label, "- Unct limits: 0 to", unct_upper, "\n")
  save_fig(make_map(dt, "Uncertainty", unct_scale(unct_upper),
                    title_for("Uncertainty"), facet),
           paste0("RF_001_Map_Unct_Annual_2011-2020_", s_tag), 24, 8)
  rm(dt); gc()
}

cat("Done:", map_type, ifelse(is.na(index_raw), "", index_raw), "\n")