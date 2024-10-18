

#### source profile for Tacoma site, PMF & DN-PMF results #### 

###### 1. prepare Source profile data, conc_percent_bsDisp ######

### source profile 
conc_percent_bsDisp_pmf = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15t1mdl0unc/base_DISPres1/CSN_noCsub_15t1mdl0unc_S_126_F_7_source_profile.csv")
conc_percent_bsDisp_dnpmf = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15t1mdl0unc_DN/base_DISPres1/CSN_noCsub_15t1mdl0unc_DN_S_126_F_7_source_profile.csv")
conc_percent_bsDisp_pmf$X = conc_percent_bsDisp_dnpmf$X = NULL

### overall contribution
source_overall_pmf = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15t1mdl0unc/base_DISPres1/CSN_noCsub_15t1mdl0unc_S_126_F_7_overall.csv")
source_overall_dnpmf = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_NonGUI/CSN_Site_15t1mdl0unc_DN/base_DISPres1/CSN_noCsub_15t1mdl0unc_DN_S_126_F_7_overall.csv")

source_overall_pmf = 
  select(source_overall_pmf, 
         Factor_source, Main_Species, Faraction_conc_contri)
source_overall_dnpmf = 
  select(source_overall_dnpmf, 
         Factor_source, Main_Species, Faraction_conc_contri)

### assign final source apportionment for PMF results
final_source_pmf = 
  select(conc_percent_bsDisp_pmf, Factor_source, Main_Species)
final_source_pmf = final_source_pmf[!duplicated(final_source_pmf), ]
final_source_pmf$final_SA = final_source_pmf$Factor_source

# reassign based on final determination
final_source_pmf$final_SA[final_source_pmf$Main_Species == "NO3Ion Cr Mn Pb Zn"] = "F3-Vehicle"
final_source_pmf$final_SA[final_source_pmf$Factor_source == "Factor5"] = "F9-Soil/Dust"
final_source_pmf$final_SA[final_source_pmf$Factor_source == "Factor6"] = "F7-Industry"

### assign final source apportionment for DN-PMF results
final_source_dnpmf = 
  select(conc_percent_bsDisp_dnpmf, Factor_source, Main_Species)
final_source_dnpmf = final_source_dnpmf[!duplicated(final_source_dnpmf), ]
final_source_dnpmf$final_SA = final_source_dnpmf$Factor_source

# reassign based on final determination
final_source_dnpmf$final_SA[final_source_dnpmf$Main_Species == "NO3Ion Cr Pb Zn Mn"] = "F3-Vehicle"
final_source_dnpmf$final_SA[final_source_dnpmf$Factor_source == "Factor2"] = "F9-Soil/Dust"
final_source_dnpmf$final_SA[final_source_dnpmf$Factor_source == "Factor6"] = "F7-Industry"

### match with source profile
conc_percent_bsDisp_pmf = merge(conc_percent_bsDisp_pmf, final_source_pmf)
conc_percent_bsDisp_pmf = merge(conc_percent_bsDisp_pmf, source_overall_pmf)
dim(conc_percent_bsDisp_pmf)

conc_percent_bsDisp_dnpmf = merge(conc_percent_bsDisp_dnpmf, final_source_dnpmf)
conc_percent_bsDisp_dnpmf = merge(conc_percent_bsDisp_dnpmf, source_overall_dnpmf)
dim(conc_percent_bsDisp_dnpmf)

###### 2. other info for plotting, text position, scales, etc. ######

############### choose data for plotting
conc_percent_bsDisp_use = conc_percent_bsDisp_pmf
conc_percent_bsDisp_use = conc_percent_bsDisp_dnpmf
head(conc_percent_bsDisp_use)

# Convert 0 to 1e-10 for columns to be used for y-axis, there is log transfer later
conc_percent_bsDisp_use <- 
  conc_percent_bsDisp_use %>%
  mutate(across(Concentration:disp_conc_up, 
                ~replace(., . == 0, 1e-10)))

##### Convert percent values to make the scale pattern similar to log concentration
# set the Percent == 0 to a low value before log
conc_percent_bsDisp_use$Percent[conc_percent_bsDisp_use$Percent == 0] = 1e-05

# define the breakpoints and corresponding values in percent and exponent values
percent_values <- c(100, 80, 60, 40, 20, 0)
exponent_values <- c(1e-00, 1e-01, 1e-02, 1e-03, 1e-04, 1e-05)
# exponent_values <- c(1e+01, 1e-00, 1e-01, 1e-02, 1e-03, 1e-04)

# get the mapped percent
conc_percent_bsDisp_use$MappedPercent  = 
  map_percent_to_exponent(percent_values, 
                          exponent_values, 
                          conc_percent_bsDisp_use$Percent)

#### Step trying to identify the trans for sec.axis

#### rename sources following the final apportionment
# replace the source names for new plotting
replacement_sourcename <- 
  c("F3-Vehicle" = "F1-Traffic",
    "F7-Non-tailpipe" = "F4-Non-tailpipe",
    "F7-Industry" = "F5-Industry",
    "F6-Fresh Sea Salt" = "F6-Salt",
    "F4-Aged Sea Salt" = "F6-Salt",
    "F8-Biomass" = "F7-Biomass",
    "F9-Soil/Dust" = "F8-Soil/Dust")

conc_percent_bsDisp_use <- 
  conc_percent_bsDisp_use %>%
  mutate(final_SA = 
           ifelse(final_SA %in% names(replacement_sourcename), 
                  replacement_sourcename[final_SA], 
                  final_SA))


# find the middle position of the factor/source names on x-axis
middle_positions <- 
  conc_percent_bsDisp_use %>%
  dplyr::group_by(Factor) %>%
  dplyr::summarize(final_SA = final_SA[1],
                   middle = custom_median(as.numeric(sequence)),
                   Factor_conc_fr = paste(unique(Faraction_conc_contri), 
                                          collapse = ", ")) %>%
  dplyr::arrange(Factor)

middle_species = 
  conc_percent_bsDisp_use$Species[
    conc_percent_bsDisp_use$sequence == 
      middle_positions$middle[1]]

middle_positions$Species = middle_species[1]

### Mark on figure, ggtext
middle_positions$Factor_source_contr = 
  paste0(middle_positions$final_SA, ", ", 
         "conc_fr ", middle_positions$Factor_conc_fr)

###### 3. source profile plot ######

source_profile_pmf <-
# source_profile_dnpmf <-
  ggplot(conc_percent_bsDisp_use,
         aes(x = reorder(Species, sequence), 
             group = final_SA)) +
  # Bar plot for Concentration
  geom_bar(aes(y = Concentration, fill = final_SA), 
           stat = "identity", width = 0.6, alpha = 0.8) +
  # Point plot for transformed Percent
  geom_point(aes(y = MappedPercent, group =1), 
             color = "grey25", shape = 15, size = 1.5) +
  geom_point(aes(y = disp_conc_mean), 
             color = "grey25", shape = 1, size = 1.5) +
  geom_errorbar(aes(ymin = disp_conc_down, 
                    ymax = disp_conc_up), 
                width = 0.3, color = "grey25") +
  ggtitle("PMF") +
  # ggtitle("DN-PMF") +
  facet_grid(final_SA ~ ., switch = "y") +
  scale_y_continuous(
    name = format_variable("Concentration µg/m3"),
    breaks = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01, 1e-00, 1e+01),
    # "", only show the break but not text on y axis
    labels = c(expression(10^"-5"), expression(""), 
               expression(10^"-3"), expression(""), 
               expression(10^"-1"), expression(""), expression(10^"1")),
    sec.axis = sec_axis(
      # set the trans the same as the main y axis
      transform = ~., 
      breaks = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01, 1e+00),
      name = "Explained Variance %",
      labels = c(0, "", 40, "", 80, "")
    ),
    transform = mylog_trans(base=10, from=-5),
    limits = c(1E-5, max(conc_percent_bsDisp_use$Concentration))) +
  scale_fill_manual(values = color_source) +
  xlab(format_variable("PM25 Species")) +
  ylab(format_variable("Concentration µg/m3")) +
  scale_x_discrete(labels = function(x) format_variable(x)) +
  geom_text(data = middle_positions, size = 5,
            aes(x = Species, y = 1e-01, label = Factor_source_contr), 
            inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
  theme_bw() +
  theme(axis.title.x = element_text(color="grey25", size = 0,
                                    family = "Arial Unicode MS", 
                                    vjust=0), 
        axis.title.y = element_text(color="grey25", size = 17,
                                    family = "Arial Unicode MS", 
                                    vjust=1),
        axis.text.x = element_text(color="grey25", size = 14.5,
                                   family = "Arial Unicode MS",
                                   angle = 90, hjust = 0, vjust = 0.3),
        axis.text.y = element_text(color="grey25", size = 14.5,
                                   family = "Arial Unicode MS",
                                   angle = 0, hjust = 0.5)) +
  theme(
    panel.grid = element_line(colour = "white"),
    plot.title = element_text(hjust = 0.05, vjust = 0, size = 16),
    strip.background = element_blank(), strip.text = element_blank(),
    legend.position = "none"
  )

# source_profile_compare = source_profile_pmf + source_profile_dnpmf
# source_profile_compare

source_profile_compare = 
  ggarrange(source_profile_pmf, NULL, source_profile_dnpmf, 
            widths = c(1, 0.05, 1), # add space between figure by NULL and widths setting
            nrow=1, align = "v" # Align vertically
            )
source_profile_compare


