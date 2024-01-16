library(sf)
library(dplyr)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(gganimate)
library(ggplot2)

setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/results_R_data/")


########################################################
###############  1. pdf process ###################
########################################################

library(pdftools)
library(gridExtra)
# library(magick)

# CSN
# setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_base_DISPres1")
pdf_folder = "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_base_DISPres1"
data.prefix = "CSN_noCsub_noExtreme_"

# IMPROVE
# setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/IMPROVE_base_DISPres1")
pdf_folder = "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/IMPROVE_base_DISPres1"
data.prefix = "IMPROVE_noCsub_noExtreme_"


cluster.Nos <- 1:25
factor.Nos <- 6:11
plot.series <- c("factor_pairs.pdf", "source_profile.pdf")
#plot.series <- c("factor_pairs.pdf", "source_profile.pdf", "overall.pdf", "daily.pdf", "annual.pdf", "month.pdf")

# Iterate over each combination and combine the PDFs
for (cluster.No in cluster.Nos) {
  for (plot_suffix in plot.series) {
    
    # Initialize a list to store PDF paths
    pdf_files_to_combine <- list()
    
    for (factor.No in factor.Nos) {
      cluster.factor.pre <- paste0("C_", cluster.No, "_F_", factor.No, "_")
      name.prefix <- paste0(data.prefix, cluster.factor.pre)
      
      # Construct the file name
      file_name <- paste0(name.prefix, plot_suffix)
      
      # Check if the file exists
      if (file.exists(file.path(pdf_folder, file_name))) {
        pdf_files_to_combine <- c(pdf_files_to_combine, file.path(pdf_folder, file_name))
      }
    }
    
    # Combine the PDFs if there's more than one file
    if (length(pdf_files_to_combine) > 1) {
      output_file <- file.path(pdf_folder, paste0("combined_", data.prefix, "C_", cluster.No, "_", plot_suffix))
      pdf_combine(pdf_files_to_combine, output_file)
    }
    
  }
}


###########################################################################
#######  2. Merge - Overall contribution #######
###########################################################################

library(dplyr)
library(readr)

setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/results_R_data/")

#### combine the overall csv ####
# Combine all same-pattern csv into one 
dir_path <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_base_DISPres1"
csn_overall_list <- list.files(dir_path, pattern = ".*overall\\.csv$", full.names = TRUE)
csn_overall <- 
  do.call(
    rbind, 
    (lapply(
      csn_overall_list, 
      read.csv)))
csn_overall$Dataset = "CSN"

dir_path <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/IMPROVE_base_DISPres1"
improve_overall_list <- list.files(dir_path, pattern = ".*overall\\.csv$", full.names = TRUE)
improve_overall <- 
  do.call(
    rbind, 
    (lapply(
      improve_overall_list, 
      read.csv)))
improve_overall$Dataset = "IMPROVE"

improve_csn_overall = rbind(csn_overall, improve_overall)
improve_csn_overall$X = improve_csn_overall$Factor = improve_csn_overall$lm.beta.site = NULL
improve_csn_overall = 
  plyr::rename(
    improve_csn_overall, 
    c("Cluster.No" = "Cluster",
      "Factor.No" = "Factor"))

# reoder the columns
# get the rest of the column names, excluding those already in desired_order
adjusted_columns <- c("Dataset", "Cluster", "Factor")
remaining_columns <- setdiff(
  names(improve_csn_overall), 
  adjusted_columns)

improve_csn_overall <- 
  improve_csn_overall[, 
                  c(adjusted_columns, remaining_columns)]

improve_csn_overall <- 
  improve_csn_overall[
    with(improve_csn_overall, 
         order(Dataset, Cluster, Factor, Source.No, -Factor.contribution)), ]

#### reorder the overall csv ####
# Prepare the data with the automatically assigned sources
overall_sourceAssigned = subset(improve_csn_overall, Source.No != "F")
overall_sourceAssigned = 
  select(overall_sourceAssigned,
         Dataset, Cluster, Factor, 
         Source_reference, Main_Species, Factor.contr)

# Apply the transformation to each group
tans_sourceAssigned <- 
  overall_sourceAssigned %>%
  group_by(Dataset, Cluster, Factor) %>%
  group_modify(~ transform_group(.x, "Source_reference", "Main_Species", "Factor.contr")) %>%
  ungroup()


overall_toAssign = subset(improve_csn_overall, Source.No == "F")
overall_toAssign = 
  select(overall_toAssign,
         Dataset, Cluster, Factor, 
         Factor_source, Main_Species, Factor.contr)
colnames(overall_toAssign)[4] = "Source_reference"

# Apply the transformation to each group
tans_toAssign <- 
  overall_toAssign %>%
  group_by(Dataset, Cluster, Factor) %>%
  group_modify(~ transform_group(.x, "Source_reference", "Main_Species", "Factor.contr")) %>%
  ungroup()

## Reorder the columns
tans_sourceAssigned <- tans_sourceAssigned[, reorder_col_number(tans_sourceAssigned)]
tans_toAssign <- tans_toAssign[, reorder_col_number(tans_toAssign)]

write.csv(overall_sourceAssigned, "Source_assigned.csv")
write.csv(overall_toAssign, "Source_to_assign.csv")

write.csv(tans_sourceAssigned, "Source_assigned_reorder.csv")
write.csv(tans_toAssign, "Source_to_assign_reorder.csv")

########################################################
###############  3. summary - data combination ###################
########################################################

setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/results_R_data/")

datasets = read.csv("IMPROVE_CSN_datasets.csv")
traffic_county = read.csv("cluster_traffic_dock_rail_airport.csv")
corre_rp = read.csv("Between_factor_correlation.csv")
overall_Assigned_30plus = read.csv("Source_assigned.csv")
datasets$X = traffic_county$X = correlation$X = overall_Assigned_30plus$X = NULL

# time = "2023.11"
time = Sys.Date()

#####  summary - overall #####

datasets = read.csv("IMPROVE_CSN_datasets.csv")
base_disp = read.csv(paste0("IMPROVE_CSN_base_DISP_", time, ".csv"))
base_disp = merge(datasets, base_disp, all.x = T)
base_disp_tra = merge(base_disp, traffic_county, all.x = T)

#####  summary - get the factor with overall contribution > 30% #####

overall_Assigned_30plus$Factor.contr.numeric <- 
  as.numeric(sub("%", "", overall_Assigned_30plus$Factor.contr))
overall_Assigned_30plus =
  subset(overall_Assigned_30plus, 
         Factor.contr.numeric > 30)
overall_Assigned_30plus$Factor.contr.numeric = overall_Assigned_30plus$Main_Species = NULL
colnames(overall_Assigned_30plus)[4:5] =
  c("Source_contri_30plus", "Contribution_Source_contri_30plus")

tans_Assigned_30plus <- 
  overall_Assigned_30plus %>%
  group_by(Dataset, Cluster, Factor) %>%
  group_modify(~ transform_group(.x, "Source_contri_30plus", "Contribution_Source_contri_30plus")) %>%
  ungroup()

tans_Assigned_30plus <- tans_Assigned_30plus[, reorder_col_number(tans_Assigned_30plus)]
base_disp_tra_contr30 = merge(base_disp_tra, tans_Assigned_30plus, all.x = T)

#####  summary - factor groups with correlations > 0.45 #####

corre_rp$factors_highly_correlated = paste(corre_rp$Var1, "&", corre_rp$Var2)
corre_rp = select(corre_rp, 
                  Dataset, Cluster, Factor,
                  factors_highly_correlated, Correlation)
corre_rp = subset(corre_rp, Correlation > 0.45)

tans_corre_rp <- 
  corre_rp %>%
  group_by(Dataset, Cluster, Factor) %>%
  group_modify(~ transform_group(.x, "factors_highly_correlated", "Correlation")) %>%
  ungroup()
tans_corre_rp <- tans_corre_rp[, reorder_col_number(tans_corre_rp)]

base_disp_tra_contr30_correl = merge(base_disp_tra_contr30, tans_corre_rp, all.x = T)

#####  summary - other factors contribution #####
tans_sourceAssigned = read.csv("Source_assigned_reorder.csv")
tans_toAssign = read.csv("Source_to_assign_reorder.csv")
tans_sourceAssigned$X = tans_toAssign$X = NULL

colnames(tans_sourceAssigned)[4:ncol(tans_sourceAssigned)] = 
  paste0("Assigned_", colnames(tans_sourceAssigned)[4:ncol(tans_sourceAssigned)])
colnames(tans_toAssign)[4:ncol(tans_toAssign)] = 
  paste0("toAssign_", colnames(tans_toAssign)[4:ncol(tans_toAssign)])

base_disp_tra_contr30_correl_Assigned = merge(base_disp_tra_contr30_correl, tans_sourceAssigned, all.x = T)
base_disp_tra_contr30_correl_Assigned_notAssign = merge(base_disp_tra_contr30_correl_Assigned, tans_toAssign, all.x = T)
colnames(base_disp_tra_contr30_correl_Assigned_notAssign)

#####  summary - output #####

write.csv(base_disp_tra_contr30_correl_Assigned_notAssign, 
          paste0("IMPROVE_CSN_base_DISP_", 
                 "traffic_contribution_correlation_", 
                 Sys.Date(), 
                 ".csv"))


###########################################################################
#######  4. Merge files and National - Annual #######
###########################################################################


# Define directory (you can change this to your directory path)
dir_path <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/annual_results"
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/annual_results")

##### Merge files #####

# List all CSV files in the directory that end with "annual.csv"
all_files <- list.files(dir_path, pattern = ".*annual\\.csv$", full.names = TRUE)

# Filter out files that don't match the specific naming patterns
filtered_files <- grep(".*_noCsub_noExtreme_C_\\d+_F_\\d+_annual\\.csv$", all_files, value = TRUE)

# Function to read and append Dataset, Cluster, and Factor columns
read_and_append_info <- function(file_name) {
  df <- read.csv(file_name)
  df$X1 = df$X = NULL
  
  df$SiteCode = as.character(df$SiteCode)
  df = subset(df, Source.No != "F")
  
  # Extract dataset, cluster and factor numbers from the filename using regex
  dataset_name <- gsub("([^_]*)_noCsub_noExtreme_.*", "\\1", basename(file_name))
  cluster_num <- as.numeric(gsub(".*_C_([0-9]+)_F_.*", "\\1", file_name))
  factor_num <- as.numeric(gsub(".*_F_([0-9]+)_.*", "\\1", file_name))
  
  # Add these extracted values as new columns to the dataframe
  df$Dataset <- dataset_name
  df$Cluster.No <- cluster_num
  df$Factor.No <- factor_num
  
  return(df)
}

# Apply function to each file and combine them
combined_annual <- bind_rows(lapply(filtered_files, 
                                    read_and_append_info))

# If you want to save the combined data
write_csv(combined_annual, "combined_annual_data.csv")


##### National level Plotting #####

combined_annual = read.csv("combined_annual_data.csv")
cty_rural_urban = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$X.1 = NULL
cty_rural_urban = cty_rural_urban[!duplicated(cty_rural_urban$SiteCode), ] 


annual_contri_gps = merge(combined_annual, 
                          cty_rural_urban,
                          by = "SiteCode",
                          all.x = T)


annual_contri_gps$geoid = ifelse(annual_contri_gps$geoid < 10000, 
                                 paste0("0", annual_contri_gps$geoid), 
                                 annual_contri_gps$geoid)


library(USAboundaries)
# generate the US county boundary data
us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr[, 13] = NULL # there are two "state_name"
head(us_cty_bdr)
us_cty_bdr_geo = dplyr::select(us_cty_bdr, geoid, stusps, geometry)

# MainStates <- map_data("state")
UScounty <- map_data("county")

us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr <- us_cty_bdr[!(us_cty_bdr$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

annual_source_gps = merge(us_cty_bdr_geo, 
                          annual_contri_gps)

annual_Biomass = subset(annual_source_gps, 
                        Source_reference == "F8-Biomass")
annual_FreshSeaSalt = subset(annual_source_gps, 
                             Source_reference == "F6-Fresh Sea Salt")
annual_AgedSeaSalt = subset(annual_source_gps, 
                            Source_reference == "F4-Aged Sea Salt")
annual_SedNitrate = subset(annual_source_gps, 
                           Source_reference == "F2-Secondary Nitrate")
annual_SedSulfate = subset(annual_source_gps, 
                           Source_reference == "F3-Secondary Sulfate")
annual_SoilDust = subset(annual_source_gps, 
                         Source_reference == "F9-Soil/Dust")

###### 1. Annual changes in 6 sources ######
color_npg = pal_npg("nrc")(10)

middle_position = 
  data.frame(
    Year = rep(as.factor(2016), 
                      length(unique(annual_source_gps$Source_reference))),
    Source_reference = unique(annual_source_gps$Source_reference))


ggplot(subset(annual_source_gps, 
              Contribution < quantile(Contribution, 0.995)),
       aes(as.factor(Year), Contribution),
       color = Source_reference) +
  geom_boxplot(aes(color = Source_reference), 
               outlier.shape = NA, 
               linewidth = 0.3, width = 0.5) +
  geom_jitter(aes(color = Source_reference), 
              width = 0.15, alpha = 0.15)+
  facet_grid(Source_reference ~.) + # , scales = "free_y"
  # ylim(0,4) +
  scale_y_continuous(breaks = c(0, 1, 2)) +
  # scale_x_discrete(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
  xlab("Year") +
  scale_color_manual(values = color_npg[-c(1, 5, 7)]) +
  geom_text(data = middle_position, size = 5.5,
            aes(x = Year, y = 2.5, label = Source_reference), 
            inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
  theme_bw() +
  theme(panel.grid = element_line(colour = "white"),
        # plot.title = element_text(hjust = 0.05, vjust = -25, size = 0),
        strip.background = element_blank(), strip.text = element_blank(), # facet title
        legend.position = "none",
        axis.title.x = element_text(color="grey25", size = 20, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 20, vjust=1, margin=margin(0,2,0,0)),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 18, angle = 45, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5))


###### 2. Spatial distribution of sources in 2011 & 2020 ######

show_col(color_npg)

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = subset(annual_SedNitrate, 
                           Year %in% c(2011, 2019)), 
             aes(x = Longitude, y = Latitude, color = Contribution), # alpha = Contribution, 
             size = 4, alpha = 0.8) +
  scale_alpha_continuous(range = c(0.1, 0.99), guide = "legend") +
  scale_color_gradient(low = "white", high = color_npg[2]) +
  coord_sf(datum = NA) +
  facet_wrap(~Year, ncol = 1) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "transparent"))


ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = subset(annual_SedSulfate, 
                           Year %in% c(2011, 2019)), 
             aes(x = Longitude, y = Latitude, color = Contribution),
             size = 4, alpha = 0.8) +
  scale_alpha_continuous(range = c(0.1, 0.99), guide = "legend") +
  scale_color_gradient(low = "white", high = color_npg[3]) +
  coord_sf(datum = NA) +
  facet_wrap(~Year, ncol = 1) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "transparent"))

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = subset(annual_SoilDust, 
                           Year %in% c(2011, 2019)), 
             aes(x = Longitude, y = Latitude, color = Contribution),
             size = 4, alpha = 0.8) +
  scale_alpha_continuous(range = c(0.1, 0.99), guide = "legend") +
  scale_color_gradient(low = "white", high = color_npg[9]) +
  coord_sf(datum = NA) +
  facet_wrap(~Year, ncol = 1) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "transparent"))


summary(subset(annual_SedNitrate, Year == 2011)$Contribution)
summary(subset(annual_SedNitrate, Year == 2019)$Contribution)
summary(subset(annual_SedNitrate, Year == 2020)$Contribution)
summary(subset(annual_SedSulfate, Year == 2011)$Contribution)
summary(subset(annual_SedSulfate, Year == 2019)$Contribution)
summary(subset(annual_SedSulfate, Year == 2020)$Contribution)
summary(subset(annual_Biomass, Year == 2011)$Contribution)
summary(subset(annual_Biomass, Year == 2019)$Contribution)
summary(subset(annual_Biomass, Year == 2020)$Contribution)
summary(subset(annual_FreshSeaSalt, Year == 2011)$Contribution)
summary(subset(annual_FreshSeaSalt, Year == 2019)$Contribution)
summary(subset(annual_FreshSeaSalt, Year == 2020)$Contribution)
summary(subset(annual_AgedSeaSalt, Year == 2011)$Contribution)
summary(subset(annual_AgedSeaSalt, Year == 2019)$Contribution)
summary(subset(annual_AgedSeaSalt, Year == 2020)$Contribution)
summary(subset(annual_SoilDust, Year == 2011)$Contribution)
summary(subset(annual_SoilDust, Year == 2019)$Contribution)
summary(subset(annual_SoilDust, Year == 2020)$Contribution)



######  2.2 Spatial-map, gif, animate{} ##########

p <-
  ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) + # base map
  geom_point(data = subset(annual_SedNitrate, Year %in% 2011:2019), 
             aes(x = Longitude, y = Latitude, color = Contribution, group = Year), 
             size = 4, alpha = 0.8) +
  scale_alpha_continuous(range = c(0.1, 0.99), guide = "legend") +
  scale_color_gradient(low = "white", high = color_npg[2]) +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "transparent"))

p + transition_manual(Year) +
  labs(title = 'Year: {closest_state}', x = '', y = '') +
  ease_aes('linear')

p + transition_states(Year, transition_length = 2, state_length = 1)

# Animate the plot
animated_plot <- 
  p + 
  transition_states(Year, transition_length = 2, state_length = 1) +
  labs(title = 'Year: {closest_state}', x = '', y = '') +
  ease_aes('linear')

# Render the animation and save it as a GIF
anim_save("sednitrate_contribution.gif", animation = animated_plot)


######  2.3 Spatial-map, gif, magick{} ##########
# tutorial: https://arbor-analytics.com/post/making-animated-gif-maps-in-r-visualizing-ten-years-of-emerald-ash-borer-spread-in-minnesota/

library(magick)

plot1 = image_read("gganim_plot0001.png")
plot2 = image_read("gganim_plot0002.png")
plot3 = image_read("gganim_plot0003.png")
plot4 = image_read("gganim_plot0004.png")
plot5 = image_read("gganim_plot0005.png")
plot6 = image_read("gganim_plot0006.png")
plot7 = image_read("gganim_plot0007.png")
plot8 = image_read("gganim_plot0008.png")
plot9 = image_read("gganim_plot0009.png")

img = c(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9)
image_append(image_scale(img, "x800"))
my_animation<-image_animate(image_scale(img, "2000x2000"), fps = 1, dispose = "previous")
#my_animation
image_write(my_animation, "sedNitrate-spread.gif")


#### example from lucas - animation - start #####

## =========================================================== ##
# dataset manipulator function
## =========================================================== ##
## read in plant Bowen's hysplit locations

manipulator <- 
  function( id_string = '703-1BLR',
            dat_path = '/home/lhennem/lhennem/disperseR/main/output/hysplit/1999',
            out.mov.path = '/home/lhennem/data/EmPOWER/figures/',
            units_and_facs.dat = units_and_facs_match
  ){
    message( paste( 'Starting', id_string))
    
    # movie file location
    units_and_facs.dat1 <- units_and_facs.dat[ID == id_string]
    out.mov.name <- paste0( 'dispersion_', units_and_facs.dat1$ID,'.mp4')
    
    dat_path_mo <- file.path( dat_path, '11')
    
    hysp_feb_mo.f <- list.files( dat_path_mo, pattern = id_string, full.names = T)
    
    hysp_mo <- lapply( hysp_feb_mo.f, reader_fn) %>% rbindlist
    
    hysp_mo[, tot_hour := as.vector( Edate - as.Date( '2018-11-08')) * 24 + hour + Phour]
    
    # merge the datasets
    hysp_mo[, date := 'November 2018']
    hysp <- hysp_mo
    
    # get the lat/lon coordinates
    loc <- units_and_facs.dat1[, .( Latitude, Longitude)] %>% unique
    
    ## get states spatial data
    states <- us_states()
    
    p <-
      ggplot( ) +
      geom_sf( data = states,
               aes( geometry = geometry),
               color = 'grey50') +
      geom_point( data = loc,
                  aes( x = Longitude, y = Latitude),
                  color = 'red', size = 4) +
      geom_point( data = hysp, #[Edate %in% as.Date( c( '1999-02-01', '1999-02-02', '1999-02-03'))],
                  aes(x = lon, y = lat,
                      color = height)) +
      scale_color_viridis( name = 'Height (m)') +
      guides(color = guide_colorbar(title.position = "top")) +
      expand_limits( color = 0) +
      coord_sf( xlim = c( -125, -69), ylim = c( 25, 49)) +
      facet_wrap( . ~ date, ncol = 2) +
      transition_states( tot_hour,
                         transition_length = 1,
                         state_length = 1
      ) +
      labs(caption = 'Day of month = {floor( as.numeric( closest_state) / 24) + 8}') +
      theme_minimal() + 
      theme( axis.title = element_blank(),
             axis.text = element_blank(),
             legend.direction = 'horizontal',
             legend.position = c( .2, .1),
             legend.text = element_text( angle = 30),
             plot.caption = element_text( size = 18),
             panel.grid = element_blank(),
             strip.text = element_text( size = 20))
    a <- animate( p,
                  nframes = 672 * 2,
                  fps = 50,
                  renderer = av_renderer( file.path( out.mov.path,
                                                     out.mov.name)),
                  height = 500,
                  width = 600
    )
    
    message( paste( 'Movie for', id_string, 'saved to', 
                    file.path( out.mov.path,
                               out.mov.name)))
    
    return( file.path( out.mov.path,
                       out.mov.name))
  }


## =========================================================== ##
# run it!
## =========================================================== ##

# giffer_dat <- lapply( units_and_facs_match[State %in% c( 'OH', 'GA')]$ID,
#                       manipulator)
giffer_dat <- manipulator( 
  id_string = 'CAMP',
  dat_path = "~/Dropbox/Rpackages/DisperseRmain/main/output/hysplit/2018",
  out.mov.path = '~/Dropbox/GeorgeMason/Research/People/CaseyJ/CAMP_fire',
  units_and_facs.dat = units.CAMP)



library( gganimate)
p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point()

anim <- p + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)

animate(
  anim,
  renderer = av_renderer( file = '~/movie.mp4'),
)

#### example from lucas - animation - end #####




###### 3. Spatial distribution of Changes between 2011 & 2020 ######

# Load the dplyr package
library(dplyr)

# Filter the dataset for the years 2011 and 2019
annual_source_selectd <- annual_source_gps %>% 
  filter(Year %in% c(2011, 2019))

map_info = select(annual_source_selectd,
                  SiteCode, Longitude, Latitude,
                  geoid, state_abbr, geometry)

# Calculate the differences in Contribution for each Source_reference and SiteCode
contribution_diff <- 
  annual_source_selectd %>%
  dplyr::group_by(Source_reference, SiteCode) %>%
  dplyr::summarise(
    diff_contribution = ifelse(n() > 1, diff(Contribution), NA),
    Longitude = last(Longitude),
    Latitude = last(Latitude),
    geoid = last(geoid),
    state_abbr = last(state_abbr),
    # geometry = last(geometry),
    .groups = 'drop'  # This will automatically ungroup the data
  )


contribution_diff$col = contribution_diff$row = 1

contribution_diff$row[
  contribution_diff$Source_reference %in%
    c("F6-Fresh Sea Salt", "F8-Biomass", "F9-Soil/Dust")] = 2

contribution_diff$col[
  contribution_diff$Source_reference %in%
    c("F3-Secondary Sulfate", "F8-Biomass")] = 2

contribution_diff$col[
  contribution_diff$Source_reference %in%
    c("F4-Aged Sea Salt", "F9-Soil/Dust")] = 3

# Create a new variable to uniquely identify each facet
contribution_diff <- contribution_diff %>%
  mutate(facet_id = interaction(col, row, lex.order = TRUE))

# Create the plot
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = contribution_diff, 
             aes(x = Longitude, y = Latitude, alpha = diff_contribution, color = as.factor(facet_id)),
             size = 4) +
  scale_alpha_continuous(range = c(0.1, 0.99)) +
  scale_color_manual(values = color_npg[-c(1, 5, 7)]) +
  coord_sf(datum = NA) +
  facet_wrap(facet_id ~ .) +
  theme_minimal() +
  theme(# strip.text = element_text(color = "transparent"),
    panel.background = element_blank())


ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = contribution_diff, 
             aes(x = Longitude, y = Latitude, alpha = diff_contribution, color = as.factor(facet_id)),
             size = 4) +
  scale_alpha_continuous(range = c(0.1, 0.99)) +
  scale_color_manual(values = color_npg[-c(1, 5, 7)]) +
  coord_sf(datum = NA) +
  facet_wrap(facet_id ~ .) +
  theme_minimal() +
  theme(# strip.text = element_text(color = "transparent"),
    panel.background = element_blank())


contribution_diff <- contribution_diff %>%
  mutate(facet_id = interaction(Source_reference, col, row, lex.order = TRUE))

# Create the plot
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = subset(contribution_diff, 
                           !is.na(diff_contribution)), 
             aes(x = Longitude, y = Latitude, 
                 color = diff_contribution),
             size = 2.5, alpha = 0.8) +
  scale_color_gradient2(low = color_npg[2], 
                        high = color_npg[8], 
                        midpoint = 0) +
  coord_sf(datum = NA) +
  facet_wrap(~ facet_id, 
             labeller = labeller(facet_id = 
                                   as_labeller(as.character, 
                                               default = label_value))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16))


###### 3. Temporal trends for differnt parts of the mainland US ######

# Define the grouping for the regions using state abbreviations
# Census Regions and Divisions of the U.S.
state_regions <- tibble(
  state_abbr = c(
    "CT", "ME", "MA", "NH", "RI", "VT", # New England (right, up 1)
    "NJ", "NY", "PA", # Mid-Atlantic (right, up 2)
    "DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", # South Atlantic (bottom, right 3) 
    "AL", "KY", "MS", "TN", # East South Central (bottom, right 2) 
    "AR", "LA", "OK", "TX", # West South Central (bottom, right 1) 
    "IL", "IN", "MI", "OH", "WI", # East North Central (top, right 2)
    "IA", "KS", "MN", "MO", "NE", "ND", "SD", # West North Central (top, right 1)
    "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", # Mountain (left, up 2)
    "CA", "OR", "WA" # Pacific (left, up 1)
  ),
  region = rep(c("New England", "Mid-Atlantic", "South Atlantic", "East South Central", 
                 "West South Central", "East North Central", "West North Central",
                 "Mountain", "Pacific"), c(6, 3, 8, 4, 4, 5, 7, 8, 3))
)

# map place: left, "Pacific" (up, 1) & "Mountain" (down, 2); bottom, "West South Central"(left, 1), "East South Central" (right, 2) & "South Atlantic" (right, 3)
# map place: top, ; right, "New England" (up, 1) & "Mid-Atlantic" (down, 2); top "West North Central" (left, 1) & "East North Central" (right, 2)


# shape is a bit weird, from the Nature paper, not use
# state_regions <- tibble(
#   state_abbr = c(
#     "WA", "OR", "ID", #Northwest
#     "MT", "WY", "ND", "SD", #Northern Rockies
#     "MN", "WI", "MI", "IA", "IL", #Upper Midwest
#     "VT", "NH", "ME", "MA", "CT", "RI", "NY", #Northeast
#     "CA", "NV", "UT", "CO", #West
#     "AZ", "NM", "TX", "OK", #Southwest
#     "AR", "LA", "MS", "AL", #South
#     "MO", "KS", "NE", "KY", "IN", "OH", "WV", #Ohio Valley
#     "VA", "NC", "SC", "GA", "FL", "TN", "DE", "MD" #Southeast
#   ),
#   region = rep(c("Northwest", "Northern Rockies", "Upper Midwest", "Northeast", 
#                  "West", "Southwest", "South", "Ohio Valley", "Southeast"),
#                c(3, 4, 5, 7, 4, 4, 4, 7, 8))
# )



us_states_region = merge(us_states, state_regions)
annual_Biomass_region = merge(annual_Biomass, state_regions)

# Map
biomass_point_region <-
  ggplot() +
  geom_sf(data = us_states_region, aes(fill = region), color = "white") +
  geom_point(data = subset(annual_Biomass_region, Year %in% c(2011)), 
             aes(x = Longitude, y = Latitude),
             color = "white", alpha = 0.5, size = 2) +
  theme_minimal() +
  scale_fill_npg() +
  theme(legend.position = "bottom") +
  facet_wrap(~ region, ncol = 3) +
  theme_map() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text( size = 0))

biomass_point_region

# dissolve states into regions
us_states_region$region <- as.factor(us_states_region$region)

regions_dissolved = 
  st_as_sf(
    ddply(
      us_states_region, 
      .(region), 
      summarise, 
      geometry = st_union(geometry)))

dim(regions_dissolved)

# check and set CRS for both datasets (example uses EPSG:4326, but use the appropriate CRS for your data)
crs_to_use <- st_crs(4326)
us_states_region <- st_set_crs(us_states_region, crs_to_use)
regions_dissolved <- st_set_crs(regions_dissolved, crs_to_use)

# Optionally, simplify geometries if they are too complex
# regions_dissolved <- st_simplify(regions_dissolved, preserveTopology = TRUE)

ggplot() +
  geom_sf(data = us_states_region, aes(fill = region), 
          color = "white", lwd = 2) 

# Plotting
ggplot() +
  geom_sf(data = regions_dissolved, fill = NA, color = "white", lwd = 2) +  # Thicker borders for regions
  geom_sf(data = us_states_region, aes(fill = region), color = NA) +  # Fill color for states without border
  geom_point(data = subset(annual_Biomass_region, Year %in% c(2011)), 
             aes(x = Longitude, y = Latitude),
             color = "white", alpha = 0.5, size = 2) +
  theme_minimal() +
  scale_fill_npg() +
  theme(legend.position = "bottom") +
  theme_map() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 10))




ggsave("map_points_biomass.pdf", plot = biomass_point_region, width = 16, height = 12)


# Calculate the centroid of each region
# Compute the centroid for each region
region_centroids <- 
  us_states_region %>%
  group_by(region) %>%
  summarize(geometry = 
              st_centroid(st_union(geometry)), 
            .groups = "keep")

ggplot() +
  geom_sf(data = us_states_region, aes(fill = region), 
          color = "white") +
  geom_point(data = subset(annual_Biomass_region, Year %in% c(2011)), 
             aes(x = Longitude, y = Latitude),
             color = "white", alpha = 0.5, size = 2) +
  geom_text(data = labels_df, aes(x = x, y = y, label = region), size = 4) +
  theme_map() +
  scale_fill_npg() +
  theme(legend.position = "bottom")

# Temporal trends for each region
annual_Biomass_region_plot = 
  ddply(annual_Biomass_region, 
        .(region, Year, Source_reference),
        summarise,
        med.contri = median(Contribution, na.rm = T),
        up.contri = quantile(Contribution, 0.975),
        down.contri = quantile(Contribution, 0.025),
        Longitude = last(Longitude),
        Latitude = last(Latitude),
        geoid = last(geoid),
        state_abbr = last(state_abbr)
  )

ggplot(annual_Biomass_region_plot, 
       aes(x = Year, y = med.contri)) +
  geom_line(color = "red") +
  geom_point(shape = 3) +
  geom_errorbar(aes(ymin = down.contri, ymax = up.contri), width = 0.2) +
  facet_wrap(~ region, ncol = 3, scales='free') +
  scale_x_continuous(breaks = c(2011, 2014, 2017, 2020)) +
  # scale_y_continuous(breaks = c(0, 1, 2, 3)) +
  labs(title = "Annual Change in Biomass Contributions for Each Region",
       x = "Year",
       y = "Median Contribution") +
  # theme_minimal() +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text( size = 16), # facet text, face = "bold",
        axis.text.x = element_text(size = 14, hjust = 0.5, vjust = 0), 
        axis.text.y = element_text(size = 14, hjust = 0.5),
        axis.title.y = element_text(size = 15, hjust = 0.5, angle = 90))

###########################################################################
#######  5. Merge files and National - Month #######
###########################################################################

library(sf)
library(dplyr)
library(readr)

# Define directory (you can change this to your directory path)
dir_path <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/month_results"
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/month_results")


##### Merge files #####

# List all CSV files in the directory that end with "month.csv"
all_files <- list.files(dir_path, pattern = ".*month\\.csv$", full.names = TRUE)

# Filter out files that don't match the specific naming patterns
filtered_files <- grep(".*_noCsub_noExtreme_C_\\d+_F_\\d+_month\\.csv$", all_files, value = TRUE)

# Function to read and append Dataset, Cluster, and Factor columns
read_and_append_info <- function(file_name) {
  df <- read.csv(file_name)
  df$X1 = df$X = NULL
  
  df$SiteCode = as.character(df$SiteCode)
  df = subset(df, Source.No != "F")
  
  # Extract dataset, cluster and factor numbers from the filename using regex
  dataset_name <- gsub("([^_]*)_noCsub_noExtreme_.*", "\\1", basename(file_name))
  cluster_num <- as.numeric(gsub(".*_C_([0-9]+)_F_.*", "\\1", file_name))
  factor_num <- as.numeric(gsub(".*_F_([0-9]+)_.*", "\\1", file_name))
  
  # Add these extracted values as new columns to the dataframe
  df$Dataset <- dataset_name
  df$Cluster.No <- cluster_num
  df$Factor.No <- factor_num
  
  return(df)
}

# Apply function to each file and combine them
combined_month <- bind_rows(lapply(filtered_files, 
                                   read_and_append_info))

# If you want to save the combined data
write_csv(combined_month, "combined_month_data.csv")


##### National level Plotting #####

combined_month = read.csv("combined_month_data.csv")
cty_rural_urban = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$X.1 = NULL
cty_rural_urban = cty_rural_urban[!duplicated(cty_rural_urban$SiteCode), ] 


month_contri_gps = merge(combined_month, 
                         cty_rural_urban,
                         by = "SiteCode",
                         all.x = T)


month_contri_gps$geoid = ifelse(month_contri_gps$geoid < 10000, 
                                paste0("0", month_contri_gps$geoid), 
                                month_contri_gps$geoid)


library(USAboundaries)
# generate the US county boundary data
us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr[, 13] = NULL # there are two "state_name"
head(us_cty_bdr)
us_cty_bdr_geo = dplyr::select(us_cty_bdr, geoid, stusps, geometry)

# MainStates <- map_data("state")
UScounty <- map_data("county")

us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr <- us_cty_bdr[!(us_cty_bdr$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

month_source_gps = merge(us_cty_bdr_geo, 
                         month_contri_gps)

month_Biomass = subset(month_source_gps, 
                       Source_reference == "F8-Biomass")
month_FreshSeaSalt = subset(month_source_gps, 
                            Source_reference == "F6-Fresh Sea Salt")
month_AgedSeaSalt = subset(month_source_gps, 
                           Source_reference == "F4-Aged Sea Salt")
month_SedNitrate = subset(month_source_gps, 
                          Source_reference == "F2-Secondary Nitrate")
month_SedSulfate = subset(month_source_gps, 
                          Source_reference == "F3-Secondary Sulfate")
month_SoilDust = subset(month_source_gps, 
                        Source_reference == "F9-Soil/Dust")


###### 1. Month changes in 6 sources ######
color_npg = pal_npg("nrc")(10)

ggplot(month_source_gps,
       aes(as.factor(Month), Contribution),
       color = Source_reference) +
  geom_boxplot(aes(color = Source_reference), 
               outlier.shape = NA, 
               linewidth = 0.3, width = 0.5) +
  geom_jitter(aes(color = Source_reference), 
              width = 0.15, alpha = 0.15)+
  facet_grid(Source_reference ~., 
             scales = "free_y") +
  ylim(0,5) +
  scale_y_continuous(breaks = c(0, 2, 4)) +
  xlab("Month") +
  scale_color_manual(values = color_npg[-c(1, 5, 7)]) +
  theme_bw() +
  theme(panel.grid = element_line(colour = "white"),
        # plot.title = element_text(hjust = 0.05, vjust = -25, size = 0),
        strip.background = element_blank(), strip.text = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(color="grey25", size = 20, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 20, vjust=1, margin=margin(0,2,0,0)),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5, vjust = 0), 
        axis.text.y = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5))


