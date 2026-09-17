#################################################################
# CONUS PM2.5 Predictor Gridding Pipeline
# 0.01 deg (~1km), daily, 2011-2020
#
# Single-file version. Run top to bottom, or source() and call the
# section functions individually. Sections mirror the original
# multi-file layout (utils, then 01-12), each still self-contained.
#
# See the header comment at the top of each section for input file
# assumptions - most reference /scratch/raw/<source>/... filenames
# that you'll need to confirm/edit to match what you actually have.
#################################################################

suppressMessages({
  library(terra)
  library(fst)
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# =================================================================
# SECTION 0: utils - shared paths, template/points, helper functions
# =================================================================

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds")

PATH_ROOT <- getwd()
PATH_TEMPLATE <- file.path(PATH_ROOT, "/pmf_ncld_meteo_census/CONUS_raster_001.tif")
PATH_POINTS   <- file.path(PATH_ROOT, "/pmf_ncld_meteo_census/Long_lat_CONUS_0.01_degree.fst")
PATH_RAW_GRID <- file.path(PATH_ROOT, "/base_raster_grid_sf/us_grid_raster_001.tif")
PATH_PRED     <- file.path(PATH_ROOT, "Predictors_001")    # per-variable, per-day fst shards
PATH_DAILY    <- file.path(PATH_ROOT, "Daily_grids_001")   # final assembled daily predictor files

dir.create(PATH_PRED,  recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_DAILY, recursive = TRUE, showWarnings = FALSE)

get_template <- 
  function() {
  r <- rast(PATH_TEMPLATE)
  crs(r) <- "EPSG:4326"
  r
}

# Returns both a plain data.frame (Longitude, Latitude - row order is the
# canonical point order used across the whole pipeline) and a SpatVector.
get_points <- 
  function() {
  pts_df <- read_fst(PATH_POINTS)   # columns: Longitude, Latitude
  pts_v  <- vect(pts_df, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
  list(df = pts_df, vect = pts_v)
}

# method: "near" (categorical), "bilinear" (continuous, smooth fields),
# "average" (continuous, area-aggregating fields like elevation/NDVI/NTL)
resample_to_template <-
  function(r, template, method) {
  if (!compareGeom(r, template, stopOnError = FALSE)) {
    r <- resample(r, template, method = method)
  }
  r
}

# Each shard is a single-column fst file ("value"), row order identical
# to get_points()$df. Keeping shards this small/skinny (rather than one
# huge points x dates matrix) keeps memory bounded and makes fst's
# columnar compression very effective.
save_var_day <- 
  function(values, varname, date) {
  outdir <- file.path(PATH_PRED, varname)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  write_fst(
    data.frame(value = values),
    file.path(outdir, paste0(varname, "_", format(as.Date(date), "%Y%m%d"), ".fst")),
    compress = 100
  )
}

# r_batch: a multi-layer SpatRaster ALREADY resampled/aligned to template,
#          whose layers correspond 1:1 to `dates`. Writes one shard per date.
extract_and_write_batch <- 
  function(r_batch, dates, pts_v, varname) {
  m <- terra::extract(r_batch, pts_v, ID = FALSE)  # data.frame: points x days
  for (i in seq_along(dates)) {
    save_var_day(m[[i]], varname, dates[i])
  }
  invisible(NULL)
}

read_val <- function(path) {
  if (file.exists(path)) read_fst(path)$value else NA_real_
}

# Load once, reused by every section below
template <- get_template()
pts      <- get_points()


# =================================================================
# SECTION 1: GridMET daily meteorology
# temp_max, temp_min, rh_max, rh_min, precip, srad, burn_index (bilinear)
# + wind_speed/wind_dir -> u/v components (processed together)
#
# Assumes: /scratch/raw/gridmet/{file_prefix}_{year}.nc
# e.g. tmmx_2011.nc, rmax_2011.nc, vs_2011.nc, th_2011.nc ...
# =================================================================

run_gridmet <- function() {
  gridmet_vars <- tibble::tribble(
    ~varname,     ~file_prefix,
    "temp_max",   "tmmx",
    "temp_min",   "tmmn",
    "rh_max",     "rmax",
    "rh_min",     "rmin",
    "precip",     "pr",
    "srad",       "srad",
    "burn_index", "bi"
  )
  
  years      <- 2011:2020
  batch_days <- 31   # ~monthly chunks to bound memory during resample+extract
  
  process_gridmet_var <- 
    function(file_prefix, varname, year) {
    f <- file.path(PATH_ROOT, "raw/gridmet", paste0(file_prefix, "_", year, ".nc"))
    if (!file.exists(f)) { message("missing: ", f); return(invisible(NULL)) }
    
    r     <- rast(f)
    dates <- as.Date(time(r))
    n     <- nlyr(r)
    
    for (start in seq(1, n, by = batch_days)) {
      end   <- min(start + batch_days - 1, n)
      idx   <- start:end
      r_sub <- resample_to_template(r[[idx]], template, method = "bilinear")
      extract_and_write_batch(r_sub, dates[idx], pts$vect, varname)
    }
    message(varname, " ", year, " done")
  }
  
  for (yr in years) {
    for (i in seq_len(nrow(gridmet_vars))) {
      process_gridmet_var(gridmet_vars$file_prefix[i], gridmet_vars$varname[i], yr)
    }
  }
  
  # Wind speed (vs) + direction (th) -> u/v. Meteorological convention:
  # th = direction wind is coming FROM. u = -speed*sin(dir), v = -speed*cos(dir)
  process_wind <- 
    function(year) {
    f_vs <- file.path(PATH_ROOT, "raw/gridmet", paste0("vs_", year, ".nc"))
    f_th <- file.path(PATH_ROOT, "raw/gridmet", paste0("th_", year, ".nc"))
    if (!file.exists(f_vs) || !file.exists(f_th)) {
      message("missing wind files for ", year); return(invisible(NULL))
    }
    
    r_vs  <- rast(f_vs)
    r_th  <- rast(f_th)
    dates <- as.Date(time(r_vs))
    n     <- nlyr(r_vs)
    
    for (start in seq(1, n, by = batch_days)) {
      end <- min(start + batch_days - 1, n)
      idx <- start:end
      
      vs_sub <- resample_to_template(r_vs[[idx]], template, method = "bilinear")
      th_sub <- resample_to_template(r_th[[idx]], template, method = "bilinear")
      
      vs_m <- terra::extract(vs_sub, pts$vect, ID = FALSE)
      th_m <- terra::extract(th_sub, pts$vect, ID = FALSE)
      
      u <- -vs_m * sin(th_m * pi / 180)
      v <- -vs_m * cos(th_m * pi / 180)
      
      for (i in seq_along(idx)) {
        save_var_day(u[[i]], "wind_u", dates[idx[i]])
        save_var_day(v[[i]], "wind_v", dates[idx[i]])
      }
    }
    message("wind u/v ", year, " done")
  }
  
  for (yr in years) process_wind(yr)
}


# =================================================================
# SECTION 2: NLCD land use (nearest available year) + LCZ (static)
# both categorical -> "near"
#
# Assumes: /scratch/raw/lcz/lcz_conus.tif
#          /scratch/raw/nlcd/nlcd_{year}.tif
# =================================================================

run_static_categorical <- function() {
  # LCZ: one file, all years/days
  r_lcz <- rast(file.path(PATH_ROOT, "raw/lcz/lcz_conus.tif"))
  r_lcz <- resample_to_template(r_lcz, template, method = "near")
  val_lcz <- terra::extract(r_lcz, pts$vect, ID = FALSE)[[1]]
  
  outdir <- file.path(PATH_PRED, "lcz")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  write_fst(data.frame(value = val_lcz), file.path(outdir, "lcz_static.fst"), compress = 100)
  message("LCZ done")
  
  # NLCD: available years only; map every study year to nearest
  nlcd_years_available <- c(2011, 2013, 2016, 2019)  # edit to match what you have
  years_needed         <- 2011:2020
  nearest_year <- function(y, available) available[which.min(abs(available - y))]
  
  for (y in years_needed) {
    src_year <- nearest_year(y, nlcd_years_available)
    f <- file.path(PATH_ROOT, "raw/nlcd", paste0("nlcd_", src_year, ".tif"))
    if (!file.exists(f)) { message("missing NLCD source: ", f); next }
    
    r   <- rast(f)
    r   <- resample_to_template(r, template, method = "near")
    val <- terra::extract(r, pts$vect, ID = FALSE)[[1]]
    
    outdir <- file.path(PATH_PRED, "nlcd")
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    write_fst(data.frame(value = val), file.path(outdir, paste0("nlcd_", y, ".fst")), compress = 100)
    message("NLCD ", y, " <- source year ", src_year)
  }
}


# =================================================================
# SECTION 3: Elevation (average) + road density (bilinear), static
#
# Assumes: /scratch/raw/dem/us_dem_full.tif        (mosaic already built)
#          /scratch/raw/road/road_density_2017.tif
#
# If the DEM mosaic isn't saved yet:
#   tiles       <- lapply(list.files("/scratch/raw/dem/tiles", full.names = TRUE, pattern = "\\.tif$"), rast)
#   us_dem_full <- do.call(mosaic, tiles)
#   writeRaster(us_dem_full, "/scratch/raw/dem/us_dem_full.tif", overwrite = TRUE)
# =================================================================

run_static_continuous <- function() {
  process_static_continuous <- function(path_in, varname, method) {
    if (!file.exists(path_in)) { message("missing: ", path_in); return(invisible(NULL)) }
    r   <- rast(path_in)
    r   <- resample_to_template(r, template, method = method)
    val <- terra::extract(r, pts$vect, ID = FALSE)[[1]]
    
    outdir <- file.path(PATH_PRED, varname)
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    write_fst(data.frame(value = val), file.path(outdir, paste0(varname, "_static.fst")), compress = 100)
    message(varname, " done")
  }
  
  process_static_continuous(file.path(PATH_ROOT, "raw/dem/us_dem_full.tif"),
                            "elevation", "average")
  process_static_continuous(file.path(PATH_ROOT, "raw/road/road_density_2017.tif"),
                            "road_density", "bilinear")
}


# =================================================================
# SECTION 4: VIIRS nighttime lights, annual, "average"
# 2011 has no product -> copied from 2012
#
# Assumes: /scratch/raw/ntl/vnl_{year}.tif
# =================================================================

run_annual_ntl <- function() {
  process_ntl_year <- function(year, source_year = year) {
    f <- file.path(PATH_ROOT, "raw/ntl", paste0("vnl_", source_year, ".tif"))
    if (!file.exists(f)) { message("missing: ", f); return(invisible(NULL)) }
    
    r   <- rast(f)
    r   <- resample_to_template(r, template, method = "average")
    val <- terra::extract(r, pts$vect, ID = FALSE)[[1]]
    
    outdir <- file.path(PATH_PRED, "ntl")
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    write_fst(data.frame(value = val), file.path(outdir, paste0("ntl_", year, ".fst")), compress = 100)
    message("NTL ", year, " <- source ", source_year)
  }
  
  process_ntl_year(2011, source_year = 2012)
  for (y in 2012:2020) process_ntl_year(y)
}


# =================================================================
# SECTION 5: MODIS NDVI, monthly, "average" - expanded to every day
#
# Assumes: /scratch/raw/ndvi/ndvi_{year}_{month:02d}.tif
# =================================================================

run_monthly_ndvi <- function() {
  years  <- 2011:2020
  months <- 1:12
  
  for (y in years) {
    for (m in months) {
      f <- file.path(PATH_ROOT, "raw/ndvi", sprintf("ndvi_%d_%02d.tif", y, m))
      if (!file.exists(f)) { message("missing NDVI ", y, "-", m); next }
      
      r   <- rast(f)
      r   <- resample_to_template(r, template, method = "average")
      val <- terra::extract(r, pts$vect, ID = FALSE)[[1]]
      
      month_start   <- as.Date(sprintf("%d-%02d-01", y, m))
      days_in_month <- seq(month_start, by = "day",
                           length.out = lubridate::days_in_month(month_start))
      
      for (d in days_in_month) {
        save_var_day(val, "ndvi", as.Date(d, origin = "1970-01-01"))
      }
      message("NDVI ", y, "-", sprintf("%02d", m), " expanded to ", length(days_in_month), " days")
    }
  }
}


# =================================================================
# SECTION 6: MAIAC AOD (MCD19A2 via AppEEARS), daily, "average"
# Missing days (cloud/orbit gaps) are skipped, not filled
#
# Assumes: /scratch/raw/maiac_aod/MCD19A2_AOD_{YYYYMMDD}.tif
# =================================================================

run_maiac_aod <- function() {
  years <- 2011:2020
  
  for (y in years) {
    dates <- seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-12-31")), by = "day")
    n_found <- 0
    
    for (d in dates) {
      f <- file.path(PATH_ROOT, "raw/maiac_aod",
                     paste0("MCD19A2_AOD_", format(d, "%Y%m%d"), ".tif"))
      if (!file.exists(f)) next
      
      r   <- rast(f)
      r   <- resample_to_template(r, template, method = "average")
      val <- terra::extract(r, pts$vect, ID = FALSE)[[1]]
      save_var_day(val, "maiac_aod", d)
      n_found <- n_found + 1
    }
    message("MAIAC AOD ", y, ": ", n_found, "/", length(dates), " days found")
  }
}


# =================================================================
# SECTION 7: ERA5 boundary layer height, 0.25 deg, daily avg, bilinear
# Already processed to one .nc per year per your notes
#
# Assumes: /scratch/raw/era5_blh/blh_{year}.nc
# =================================================================

run_era5_blh <- function() {
  years <- 2011:2020
  
  for (y in years) {
    f <- file.path(PATH_ROOT, "raw/era5_blh", paste0("blh_", y, ".nc"))
    if (!file.exists(f)) { message("missing BLH: ", f); next }
    
    r     <- rast(f)
    dates <- as.Date(time(r))
    r     <- resample_to_template(r, template, method = "bilinear")
    m     <- terra::extract(r, pts$vect, ID = FALSE)
    
    for (i in seq_along(dates)) save_var_day(m[[i]], "blh", dates[i])
    message("BLH ", y, " done")
  }
}


# =================================================================
# SECTION 8: NOAA HMS smoke (density classes) + dust (binary)
# Daily polygons rasterized onto template
#
# Assumes: /scratch/raw/hms_smoke/hms_smoke_{YYYYMMDD}.shp
#          /scratch/raw/hms_dust/hms_dust_{YYYYMMDD}.shp
# Edit `field` to match your actual density-class attribute name.
# =================================================================

run_hms_smoke_dust <- function() {
  smoke_density_code <- c(Light = 1, Medium = 2, Heavy = 3)
  
  process_hms_day <- function(shp_path, is_binary, field = "Density") {
    if (!file.exists(shp_path)) return(NULL)
    v <- vect(shp_path)
    v <- project(v, crs(template))
    
    if (is_binary) {
      r <- rasterize(v, template, field = 1, background = 0)
    } else {
      v[[field]] <- smoke_density_code[unlist(v[[field, drop = TRUE]])]
      r <- rasterize(v, template, field = field, background = 0, fun = "max")
    }
    terra::extract(r, pts$vect, ID = FALSE)[[1]]
  }
  
  years <- 2011:2020
  
  for (y in years) {
    dates <- seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-12-31")), by = "day")
    
    for (d in dates) {
      dstr <- format(d, "%Y%m%d")
      
      f_smoke   <- file.path(PATH_ROOT, "raw/hms_smoke", paste0("hms_smoke_", dstr, ".shp"))
      val_smoke <- process_hms_day(f_smoke, is_binary = FALSE)
      if (!is.null(val_smoke)) save_var_day(val_smoke, "hms_smoke", d)
      
      f_dust   <- file.path(PATH_ROOT, "raw/hms_dust", paste0("hms_dust_", dstr, ".shp"))
      val_dust <- process_hms_day(f_dust, is_binary = TRUE)
      if (!is.null(val_dust)) save_var_day(val_dust, "hms_dust", d)
    }
    message("HMS smoke/dust ", y, " done")
  }
  # NOTE: a day with no shard means "no smoke/dust reported" (fill 0 at
  # assembly), unlike AOD where a missing shard means "no retrieval" (NA).
}


# =================================================================
# SECTION 9: FHWA traffic volume - IDW spatial base x temporal factor
#
# *** Working starting point, not finished - edit column names below ***
# Assumes: /scratch/raw/traffic/fhwa_stations.csv
#            (station_id, Longitude, Latitude, base_volume)
#          /scratch/raw/traffic/temporal_factors.csv
#            (date, factor)
# =================================================================

run_traffic_idw <- function() {
  library(gstat)   # install.packages("gstat") if not available
  
  stations <- read.csv(file.path(PATH_ROOT, "raw/traffic/fhwa_stations.csv"))
  stations_sf <- sf::st_as_sf(stations, coords = c("Longitude", "Latitude"), crs = 4326)
  
  template_xy <- as.data.frame(template, xy = TRUE)[, c("x", "y")]
  grid_sf     <- sf::st_as_sf(template_xy, coords = c("x", "y"), crs = 4326)
  
  idw_fit  <- gstat(formula = base_volume ~ 1, data = stations_sf, set = list(idp = 2))
  idw_pred <- predict(idw_fit, grid_sf)
  
  r_base <- template
  values(r_base) <- idw_pred$var1.pred
  val_base <- terra::extract(r_base, pts$vect, ID = FALSE)[[1]]
  
  outdir <- file.path(PATH_PRED, "traffic_base")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  write_fst(data.frame(value = val_base), file.path(outdir, "traffic_base_static.fst"), compress = 100)
  
  temporal_factor <- read.csv(file.path(PATH_ROOT, "raw/traffic/temporal_factors.csv"))
  
  years <- 2011:2020
  for (y in years) {
    dates <- seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-12-31")), by = "day")
    for (d in dates) {
      f <- temporal_factor$factor[temporal_factor$date == as.character(d)]
      if (length(f) == 0) f <- 1
      save_var_day(val_base * f, "traffic_volume", d)
    }
    message("traffic volume ", y, " done")
  }
}


# =================================================================
# SECTION 10: CMAQ ISAM sectors from existing 0.1 deg files, bilinear
#
# Assumes: /scratch/raw/cmaq/cmaq_{sector}_{year}.nc
# Edit cmaq_vars to list every ISAM sector you have.
# =================================================================

run_cmaq <- function() {
  cmaq_vars  <- c("OTA", "AFI", "BIOG", "solvents")  # edit: full sector list
  years      <- 2011:2020
  batch_days <- 31
  
  for (sector in cmaq_vars) {
    for (y in years) {
      f <- file.path(PATH_ROOT, "raw/cmaq", paste0("cmaq_", sector, "_", y, ".nc"))
      if (!file.exists(f)) { message("missing CMAQ ", sector, " ", y); next }
      
      r     <- rast(f)
      dates <- as.Date(time(r))
      n     <- nlyr(r)
      
      for (start in seq(1, n, by = batch_days)) {
        end   <- min(start + batch_days - 1, n)
        idx   <- start:end
        r_sub <- resample_to_template(r[[idx]], template, method = "bilinear")
        extract_and_write_batch(r_sub, dates[idx], pts$vect, paste0("cmaq_", sector))
      }
      message("CMAQ ", sector, " ", y, " done")
    }
  }
}


# =================================================================
# SECTION 11: ACS commute (nearest 5-yr vintage), areal interpolation
#
# NOTE: rasterize(field=...) assigns each cell whatever polygon its
# center falls in - it's NOT true area-weighted interpolation. For
# real areal-weighting swap in sf::st_interpolate_aw() where marked.
#
# Assumes: /scratch/raw/acs/acs_tracts_{acs_year}.shp
# Edit `field` to your actual ACS attribute name.
# =================================================================

run_census_acs <- function() {
  field <- "commute_time"  # edit to your actual ACS attribute name
  
  acs_years_available <- c(2011, 2013, 2015, 2017, 2019)  # edit to your vintages
  years_needed        <- 2011:2020
  nearest_year <- function(y, available) available[which.min(abs(available - y))]
  
  process_acs_year <- function(acs_year) {
    f <- file.path(PATH_ROOT, "raw/acs", paste0("acs_tracts_", acs_year, ".shp"))
    if (!file.exists(f)) { message("missing: ", f); return(invisible(NULL)) }
    
    tracts <- vect(f)
    tracts <- project(tracts, crs(template))
    
    # --- TODO: swap this rasterize() for a true areal-weighted step ---
    r <- rasterize(tracts, template, field = field)
    
    val <- terra::extract(r, pts$vect, ID = FALSE)[[1]]
    
    outdir <- file.path(PATH_PRED, "acs_commute")
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    write_fst(data.frame(value = val),
              file.path(outdir, paste0("acs_commute_", acs_year, ".fst")), compress = 100)
    message("ACS ", acs_year, " done")
  }
  
  for (ay in acs_years_available) process_acs_year(ay)
  
  acs_year_map <- setNames(sapply(years_needed, nearest_year, available = acs_years_available),
                           years_needed)
  saveRDS(acs_year_map, file.path(PATH_PRED, "acs_commute", "year_map.rds"))
}


# =================================================================
# SECTION 12: Final assembly - join every shard into daily predictor
# files: /scratch/daily_grids/predictors_YYYYMMDD.fst
# Run this LAST, after sections 1-11 have populated /scratch/predictors.
# =================================================================

run_assemble_daily <- function() {
  acs_year_map <- readRDS(file.path(PATH_PRED, "acs_commute", "year_map.rds"))
  
  daily_vars <- c(
    "temp_max", "temp_min", "rh_max", "rh_min", "precip", "srad", "burn_index",
    "wind_u", "wind_v", "blh", "maiac_aod",
    "cmaq_OTA", "cmaq_AFI", "cmaq_BIOG", "cmaq_solvents",
    "traffic_volume"
  )
  hms_vars <- c("hms_smoke", "hms_dust")  # missing shard -> fill 0, not NA
  
  assemble_day <- function(d) {
    dstr <- format(d, "%Y%m%d")
    y    <- lubridate::year(d)
    
    out <- pts$df
    out$date <- d
    
    out[["lcz"]]          <- read_val(file.path(PATH_PRED, "lcz", "lcz_static.fst"))
    out[["elevation"]]    <- read_val(file.path(PATH_PRED, "elevation", "elevation_static.fst"))
    out[["road_density"]] <- read_val(file.path(PATH_PRED, "road_density", "road_density_static.fst"))
    
    out[["nlcd"]] <- read_val(file.path(PATH_PRED, "nlcd", paste0("nlcd_", y, ".fst")))
    out[["ntl"]]  <- read_val(file.path(PATH_PRED, "ntl",  paste0("ntl_",  y, ".fst")))
    
    acs_yr <- acs_year_map[as.character(y)]
    out[["acs_commute"]] <- read_val(file.path(PATH_PRED, "acs_commute",
                                               paste0("acs_commute_", acs_yr, ".fst")))
    
    out[["ndvi"]] <- read_val(file.path(PATH_PRED, "ndvi", paste0("ndvi_", dstr, ".fst")))
    
    for (v in daily_vars) {
      out[[v]] <- read_val(file.path(PATH_PRED, v, paste0(v, "_", dstr, ".fst")))
    }
    
    for (v in hms_vars) {
      p <- file.path(PATH_PRED, v, paste0(v, "_", dstr, ".fst"))
      out[[v]] <- if (file.exists(p)) read_fst(p)$value else 0
    }
    
    write_fst(out, file.path(PATH_DAILY, paste0("predictors_", dstr, ".fst")), compress = 100)
  }
  
  all_dates <- seq(as.Date("2011-01-01"), as.Date("2020-12-31"), by = "day")
  
  for (d in all_dates) {
    assemble_day(d)
    if (lubridate::day(d) == 1) message("assembled through ", d)
  }
  message("All daily predictor files written to ", PATH_DAILY)
}


# =================================================================
# MAIN - uncomment the steps you want to run
# =================================================================

# run_gridmet()
# run_static_categorical()
# run_static_continuous()
# run_annual_ntl()
# run_monthly_ndvi()
# run_maiac_aod()
# run_era5_blh()
# run_hms_smoke_dust()
# run_traffic_idw()
# run_cmaq()
# run_census_acs()
# run_assemble_daily()     # run this one last