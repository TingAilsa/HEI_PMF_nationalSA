# ============================================================
# CMAQ-ISAM Annual Distribution Plots & Percentile Summaries
# ============================================================
# File naming convention: XX_cmaq_YYYY-01_YYYY-12.rds
# Output: boxplots and percentile tables saved to ./annual_trend/
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# ---- Configuration ----------------------------------------------------------

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_annual/annual_trend")
getwd()

data_dir   <- getwd()
out_dir    <- "annual_trend"
cmaq_years      <- 2011:2020

# Percentiles to summarise
PROBS <- c(0.0001, 0.001, 0.0025, 0.005, 0.010,
           0.025,  0.05,  0.10,   0.25,  0.50,
           0.90,   0.95,  0.975,  0.99,  0.995,
           0.9975, 0.999, 0.9999)

PROB_LABELS <- c("0.01%", "0.1%", "0.25%", "0.5%", "1%",
                 "2.5%",  "5%",   "10%",   "25%",  "50%",
                 "90%",   "95%",  "97.5%", "99%",  "99.5%",
                 "99.75%","99.9%","99.99%")

dir.create(out_dir, showWarnings = FALSE)

# ---- Helper: extract species prefix from filename ---------------------------
# Matches everything before "_cmaq_"
extract_species <- function(fname) {
  sub("_cmaq_.*", "", basename(fname))
}

# ---- Discover all species present across the year range ---------------------
all_files <- list.files(data_dir, pattern = "_cmaq_\\d{4}-01_\\d{4}-12\\.rds$",
                        full.names = TRUE)

# Keep only files whose year falls in cmaq_years
year_from_file <- function(f) as.integer(sub(".*_cmaq_(\\d{4})-01_\\d{4}-12\\.rds", "\\1", f))
all_files <- all_files[year_from_file(all_files) %in% cmaq_years]

species_list <- unique(sapply(all_files, extract_species))
cat("Species found:", paste(species_list, collapse = ", "), "\n\n")

# ---- Main loop over species -------------------------------------------------
for (sp in species_list) {
  
  cat("Processing:", sp, "\n")
  
  # -- 1. Load all cmaq_years for this species -------------------------------------
  sp_files <- all_files[extract_species(all_files) == sp]
  sp_files <- sp_files[order(year_from_file(sp_files))]   # chronological
  
  year_data <- list()
  
  for (f in sp_files) {
    yr  <- year_from_file(f)
    dat <- readRDS(f)
    
    # Flatten to numeric vector regardless of array / matrix / data.frame input
    vals <- as.numeric(dat[[sp]])
    vals <- vals[is.finite(vals)]   # drop NA / Inf / NaN
    
    year_data[[as.character(yr)]] <- data.frame(
      year  = as.factor(yr),
      value = vals
    )
  }
  
  if (length(year_data) == 0) {
    cat("  No files found, skipping.\n")
    next
  }
  
  combined <- bind_rows(year_data)
  
  # -- 2. Boxplot -------------------------------------------------------------
  p <- ggplot(combined, aes(x = year, y = value)) +
    geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3,
                 fill = "#4393c3", colour = "#2166ac", alpha = 0.7) +
    labs(
      title    = paste0(sp, " — Annual Distribution (", min(cmaq_years), "–", max(cmaq_years), ")"),
      subtitle = "Daily gridded CMAQ-ISAM values",
      x        = "Year",
      y        = paste0(sp, " concentration")
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold"),
      axis.text.x   = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  fig_path <- file.path(out_dir, paste0(sp, "_annual_boxplot.png"))
  ggsave(fig_path, plot = p, width = 10, height = 6, dpi = 150)
  cat("  Saved:", fig_path, "\n")
  
  # -- 3. Percentile summary --------------------------------------------------
  pct_list <- lapply(names(year_data), function(yr_chr) {
    vals <- year_data[[yr_chr]]$value
    pct  <- quantile(vals, probs = PROBS, na.rm = TRUE)
    data.frame(
      species    = sp,
      year       = as.integer(yr_chr),
      percentile = PROB_LABELS,
      value      = as.numeric(pct),
      stringsAsFactors = FALSE
    )
  })
  
  pct_long <- bind_rows(pct_list)
  
  # Wide format: rows = percentile, columns = year
  pct_wide <- pct_long %>%
    pivot_wider(names_from = year, values_from = value,
                names_prefix = "Y") %>%
    arrange(match(percentile, PROB_LABELS))   # keep logical order
  
  csv_path <- file.path(out_dir, paste0(sp, "_annual_percentiles.csv"))
  write_csv(pct_wide, csv_path)
  cat("  Saved:", csv_path, "\n")
}

cat("\nDone. All outputs written to:", out_dir, "\n")

################################################
############# Extreme day conc map ############# 
################################################

library(ggplot2)
library(dplyr)
library(maps)
library(lubridate)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_annual/")
data_dir <- getwd()

out_dir  <- "annual_trend"
cmaq_years    <- 2011:2020

TARGET_SPECIES <- c("PM25_TOT_ACM", "PM25_TOT_ASEA", "NH3", "SO2")

# Ranks to map (1 = highest, 10 = 10th highest)
RANKS <- c(1, 10)

dir.create(out_dir, showWarnings = FALSE)

# ---- US state map for overlay -----------------------------------------------
us_states <- map_data("state")

# ---- Helper: build expected filename ----------------------------------------
make_filename <- function(sp, yr) {
  file.path(data_dir,
            paste0(sp, "_cmaq_", yr, "-01_", yr, "-12.rds"))
}

# ---- Helper: safe rank (handle ties, short vectors) -------------------------
safe_rank_index <- function(x, rank_n) {
  # returns index of the rank_n-th largest value in vector x
  if (length(x) < rank_n) return(NA_integer_)
  order(x, decreasing = TRUE)[rank_n]
}

###### MAIN LOOP ######

for (sp in TARGET_SPECIES) { # sp = TARGET_SPECIES[1]
  cat("\n========== Species:", sp, "==========\n")
  
  for (yr in cmaq_years) { # yr = cmaq_years[1]
    fpath <- make_filename(sp, yr)
    
    if (!file.exists(fpath)) {
      cat("  [SKIP] Not found:", fpath, "\n")
      next
    }
    
    cat("  Loading:", basename(fpath), "\n")
    dat <- readRDS(fpath)
    
    ##### Parse RDS into a data.frame with columns, and detect peak value day
    
    # Within mainland US CONUS
    long_df <- dat %>%
      select(lon = x, lat = y, value = all_of(sp), date = Date) %>%
      filter(is.finite(value),
             lon >= -125, lon <= -66,
             lat >=   24, lat <=  50)
    
    ## Peak value
    peak_value <- max(long_df$value)
    peak_date  <- long_df$date[long_df$value == peak_value]
    
    peak_day_df <- long_df %>% filter(date == peak_date)
    
    #### Map
    
    peak_day_map <- 
      ggplot() +
      # Concentration raster
      geom_tile(data = peak_day_df,
                aes(x = lon, y = lat, fill = value)) +
      # State boundaries
      geom_polygon(data = us_states,
                   aes(x = long, y = lat, group = group),
                   fill  = NA,
                   colour = "grey30",
                   linewidth = 0.35) +
      # Colour scale
      scale_fill_viridis_c(
        option   = "plasma",
        name     = sp,
        na.value = "white"
      ) +
      coord_fixed(1.3,
                  xlim = range(peak_day_df$lon, na.rm = TRUE),
                  ylim = range(peak_day_df$lat, na.rm = TRUE)) +
      labs(
        title    = paste0(sp, "  |  ", peak_date,
                          "  (rank-", rank_n, " peak day, ", yr, ")"),
        subtitle = paste0("Domain-mean: ",
                          round(peak_row$mean_val, 3), " ppb/µg m⁻³"),
        x = "Longitude", y = "Latitude"
      ) +
      theme_bw(base_size = 11) +
      theme(
        plot.title      = element_text(face = "bold", size = 12),
        legend.position = "right",
        panel.grid      = element_blank()
      )
    
    #### Save
    
    out_name <- paste0("PeakDay_", sp, "_", peak_date, ".png")
    out_path <- file.path(out_dir, out_name)
    ggsave(out_path, plot = peak_day_map, width = 10, height = 6, dpi = 150)
    cat("  Saved:", out_path, "\n")
    
  }
}

cat("\nDone. Maps saved to:", out_dir, "\n")


