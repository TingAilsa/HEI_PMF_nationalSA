##########################################################################################
############ 1. PLOTTING ############ 
##########################################################################################

#### Plot theme & function for font of ions ####
# function to format ion variables
# \u208+#, subscript#; \u00B+#, superscript#; \u207A, \u207B, superscript +/-

# Here name the ions with "Ion" instead of adding symbols inside cause this cause confusion for gsub
format_variable <- function(variable) {
  variable <- gsub("ClIon", "Cl\u207B", variable)
  variable <- gsub("NO3Ion", "NO\u2083\u207B", variable)
  variable <- gsub("SO4Ion", "SO\u2084\u00B2\u207B", variable)
  variable <- gsub("CO3Ion", "CO\u2083\u00B2\u207B", variable)
  variable <- gsub("NH4Ion", "NH\u2084\u207A", variable)
  variable <- gsub("NaIon", "Na\u207A", variable)
  variable <- gsub("KIon", "K\u207A", variable)
  variable <- gsub("PM25", "PM\u2082.\u2085", variable)
  variable <- gsub("PM2.5", "PM\u2082.\u2085", variable)
  variable <- gsub("m3", "m\u00B3", variable)
  return(variable)
}

# theme setting for the super or sub scripts in format_variable
library(ggplot2)
theme_text_speciesName = theme(axis.title.x = element_text(color="grey25", size = 12,
                                                           family = "Arial Unicode MS", 
                                                           vjust=0), 
                               axis.title.y = element_text(color="grey25", size = 12,
                                                           family = "Arial Unicode MS", 
                                                           vjust=1),
                               axis.text.x = element_text(color="grey25", size = 10.5,
                                                          family = "Arial Unicode MS",
                                                          angle = 90, hjust = 0, vjust = 0.3),
                               axis.text.y = element_text(color="grey25", size = 10.5,
                                                          family = "Arial Unicode MS",
                                                          angle = 0, hjust = 0.5))

theme_ts = theme(axis.title.x = element_text(color="grey25", size = 12,
                                             family = "Arial Unicode MS", 
                                             vjust=0), 
                 axis.title.y = element_text(color="grey25", size = 12,
                                             family = "Arial Unicode MS", 
                                             vjust=1),
                 axis.text.x = element_text(color="grey25", size = 10.5,
                                            family = "Arial Unicode MS",
                                            angle = 0, hjust = 0, vjust = 0.3),
                 axis.text.y = element_text(color="grey25", size = 10.5,
                                            family = "Arial Unicode MS",
                                            angle = 0, hjust = 0.5))

# Figures arrangement
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Set the color for each site
site_color = function(date_site_pair) {
  library(RColorBrewer)
  
  n_sites <- length(unique(date_site_pair$SiteCode))
  color_pal <- brewer.pal(n_sites, "Paired")
  
  date_site_pair$color <- color_pal[as.factor(date_site_pair$SiteCode)]

  return(date_site_pair)
}

# pairs plot 1, correlation calculation
panel.corr <- function(x, y, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 5/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * abs(r))
}

panel.corr <- function(x, y, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  # Calculate correlation and p-value
  test <- cor.test(x, y)
  r <- round(test$estimate, digits=3)
  p_value <- round(test$p.value, digits=5)
  
  # Create correlation text and calculate size
  txt <- paste0("R = ", r)
  cex.cor <- 2.5/strwidth(txt)
  
  # Determine color based on significance of p-value
  if (p_value < 0.05) {
    col <- "red"
  } else {
    col <- "black"
  }
  
  # Place the correlation text on the plot
  text(0.5, 0.7, txt, cex = cex.cor * abs(r), col = col)
  
  # Create p-value text and calculate size
  txt_p <- paste0("p = ", round(p_value, 3))
  cex.p <- 1
  
  # Place the p-value text on the plot below the correlation
  text(0.5, 0.3, txt_p, cex = cex.p, col = col)
  
  # Return correlation and p-value
  return(list(correlation = r, p_value = p_value))
}

# Function to calculate correlation and create a label
calculate_corr_label <- function(data, variable1, variable2) {
  # not data$variable1 directly
  # R interprets variable1 and variable2 literally rather than as variable names passed to the function.
  var1 <- data[[variable1]] 
  var2 <- data[[variable2]]
  corr_test <- cor.test(var1, var2, method = "pearson")
  
  paste("R =", signif(corr_test$estimate, 2), 
        "\np =", round(corr_test$p.value, 3))
}


# !!! Important for pairs plot 1&2 is, pairs function provides only the x and y arguments to the upper.panel and lower.panel functions
# !!! For other setting such as pch and col, take ... (ellipsis), and explicitly state them outside the function 

# pairs plot 2, upper or lower panel
panel.scatter <- function(x, y, ...){
  points(x, y, 
         pch=point_char, 
         size=point_size,
         col=adjustcolor(point_col, 
                         alpha.f=point_alpha))
}


#### arrange the position of multiple plots ####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


##########################################################################################
############ 2. DATA PROCESS ############ 
##########################################################################################

#### Identify columns with PM2.5 species and PM2.5 ####

col_comp = function(df, start.comp, end.comp) {
  
  start.col <- which(names(df) == start.comp)
  end.col <- which(names(df) == end.comp)
  col_comp = start.col:end.col
  
  return(col_comp)
}

#### Get the percentile summary for each column  ####
quantile_col <- function(col_range, dataset) {
  quantile( x = unlist( dataset[,  col_range] ), 
            c(0.05, .1, 0.25, .5, .75, .8, 0.95),
            na.rm = T )
}

#### Identify seasons based on date column ####
season_date <- function(dates) {
  # ensure the input is of Date type
  dates = as.Date(dates)
  
  months <- month(dates)
  
  seasons <- ifelse(months %in% c(12, 1, 2), "Winter",
                    ifelse(months %in% c(3, 4, 5), "Spring",
                           ifelse(months %in% c(6, 7, 8), "Summer", 
                                  "Autumn")))
  
  return(seasons)
}


#### detect the percent of missing concentration values for column #### 
p_miss_with_neg <- function(dataset){
  (p_miss_with_neg <- 
     data.frame(
       unlist(
         lapply(
           dataset, 
           function(x) 
             sum(is.na(x))))/nrow(dataset)))
  
  return(p_miss_with_neg)
}


#### detect the percent of negative concentration values for column #### 
p_neg <- function(dataset){
  (p_neg <- 
     data.frame(
       unlist(
         lapply(
           dataset, 
           function(x) 
             sum(x<0)))/nrow(dataset)))
  
  return(p_neg)
}

#### detect weak & strong species based on prepared file for PMF CMD runs #### 
strong_weak = function(cluster_info, start.species, end.species) {
  col_comp_all = col_comp(cluster_info, start.species, end.species)
  col_selected = colnames(cluster_info)[col_comp_all]
  
  strong_weak_species = 
    col_selected[which(
    !is.na(cluster_info[1, col_comp_all]))]
  
  strong_weak_col = 
    data.frame(
      Species = strong_weak_species,
      strong_weak_set = matrix(cluster_info[, strong_weak_species]))
  strong_weak_col$strong_weak = 
    ifelse(strong_weak_col$strong_weak_set == 1,
           "Strong", "Weak")
  
  return(list(
    strong_weak_species = strong_weak_species,
    strong_weak_col = strong_weak_col))
}

strong_species = function(cluster_info, start.species, end.species) {
  col_comp_all = col_comp(cluster_info, start.species, end.species)
  
  col_selected = colnames(cluster_info)[col_comp_all]
  
  strong_species = 
    col_selected[which(
      cluster_info[1, col_comp_all] == 1)]
  
  return(strong_species)
}

weak_species = function(cluster_info, start.species, end.species) {
  col_comp_all = col_comp(cluster_info, start.species, end.species)
  
  col_selected = colnames(cluster_info)[col_comp_all]
  
  weak_species = 
    col_selected[which(
      cluster_info[1, col_comp_all] == 0)]
  
  return(weak_species)
}


####### Force the bar to start from a given value ####### 
# https://stackoverflow.com/questions/22295253/force-bars-to-start-from-a-lower-value-than-0-in-ggplot-geom-bar-in-r
require(scales)
mylog_trans <- function(base=exp(1), from=0) {
  trans <- function(x) log(x, base)-from
  inv <- function(x) base^(x+from)
  trans_new("mylog", trans, inv, log_breaks(base=base), 
            domain = c(base^from, Inf))
}

####### Define the function to map percent (0-100) to exponent values of a given range  ####### 
map_percent_to_exponent <- 
  function(percent_values, exponent_values, percent_to_convert) {
    # Interpolate in log10 scale for precise control over exponential mapping
    log_exponent <- 
      approx(percent_values, 
             log10(exponent_values), 
             xout = percent_to_convert, 
             rule = 2)$y
    convert_percent <- 10^log_exponent
    
    return(convert_percent)
  }

####### Source Assignment based on source-specific N characteried species from reference ####### 

source_ref = function(base_percent, N){
  main_species = Nmain_Species(base_percent, N)

  main_sources <- main_species %>%
    mutate(Source_reference = case_when(
      grepl("Al", Main_Species) & grepl("Si", Main_Species) & grepl("Ca", Main_Species) ~ "F9-Soil/Dust",
      
      (grepl("NaIon", Main_Species) & grepl("Cl", Main_Species)) | 
        grepl("Na", Main_Species) & grepl("Cl", Main_Species) ~ "F6-Fresh Sea Salt",
      
      grepl("Mg", Main_Species) & grepl("SO4Ion", Main_Species) ~ "F4-Aged Sea Salt",
      
      (grepl("NH4Ion", Main_Species) & (grepl("NO3Ion", Main_Species)) | 
         startsWith(Main_Species, "NO3Ion")) ~ "F2-Secondary Nitrate",
      
      (grepl("NH4Ion", Main_Species) & (grepl("SO4Ion", Main_Species)) | 
         startsWith(Main_Species, "SO4Ion")) ~ "F3-Secondary Sulfate",
      
      (grepl("KIon", Main_Species) & grepl("OC", Main_Species)) | 
        grepl("K", Main_Species) & grepl("OC", Main_Species) ~ "F8-Biomass",

      TRUE ~ "F-" # Default value if no condition is met
    ))
  
  return(main_sources)
}


#### Convert the dataset in PMF_bunch_plotting  ####

# create a function to dynamically generate new column names
create_new_column_names <- function(prefix, n) {
  paste0(prefix, "_", seq_len(n))
}

# function to transform each group with variable base names
transform_group <- function(data, ...) {
  base_names <- list(...)
  max_rows <- nrow(data)
  
  # initialize an empty list to store column data
  dynamic_data <- list()
  
  # loop through each base name, create dynamic column names, and assign data
  for (base_name in base_names) {
    cols <- create_new_column_names(base_name, max_rows)
    
    for (i in seq_along(cols)) {
      
      # ensure the index does not exceed the length of data[[base_name]]
      if (i <= length(data[[base_name]])) {
        dynamic_data[[cols[i]]] <- data[[base_name]][i]
      } else {
        dynamic_data[[cols[i]]] <- NA  
      }
    }
  }
  
  # convert the list to a dataframe
  new_data <- as.data.frame(dynamic_data, stringsAsFactors = FALSE)
  
  return(new_data)
}

#### Reorder the columns for those with pattern col_A_1, col_B_2, col_A_3, col_B_2, col_A_2, col_B_3, ...  ####

reorder_col_number <- function(dataset) {
  col_names_assign = colnames(dataset)
  
  pattern <- "(.*_)([0-9]+)"
  matches <- regexpr(pattern, col_names_assign)
  fix_names <- col_names_assign[matches < 0]
  to_order_names <- unique(regmatches(col_names_assign, matches, invert = F))
  repeat_number <- unique(as.numeric(sub(pattern, "\\2", col_names_assign[matches > 0])))
  base_names <- unique(sub(pattern, "\\1", to_order_names))
  
  # Generate new order of names
  ordered_names <- fix_names
  for (num in repeat_number) {
    for (base in base_names) {
      new_name <- paste0(base, num)
      if (new_name %in% col_names_assign) {
        ordered_names <- c(ordered_names, new_name)
      }
    }
  }
  
  return(ordered_names)
}

##########################################################################################
############ 3. TXT FILE PROCESS ############ 
##########################################################################################

#### Determine the line number in txt file including given string  ####

line_number <- function(lines, string) {
  # Loop through the lines to find the one containing the specific string
  for (i in seq_along(lines)) {
    if (grepl(string, lines[i])) {
      return(i) # Return the line number if found
    }
  }
  return(NULL) # Return NULL if not found
}

#### Transfer txt lines into dataframe  ####
line_to_df <- function(lines_txt, nrow, ncol){
  # split the lines into individual elemenconc
  lines_txt = unlist(strsplit(lines_txt, "\\s+"))
  
  # create a matrix to store the individual elemenconc and convert to data.frame
  lines_df = data.frame(
    matrix(lines_txt, 
           nrow = nrow, 
           ncol = ncol, 
           byrow = T))
  
  return(lines_df)
}

#### Identify segments that are not spaces #### 
non_space_segments <- function(line_to_replace) {
  # Split the line into segments (numbers and spaces)
  # (?<=...): a lookbehind assertion; (?=...): a lookahead assertion.
  # a space character (\\s), a non-space character (\\S)
  # together, splits the string exactly at the positions where a space transitions into a non-space or a non-space transitions into a space
  segments <- unlist(strsplit(line_to_replace, 
                              "(?<=\\s)(?=\\S)|(?<=\\S)(?=\\s)", 
                              perl = TRUE))
  
  # Identify segments that are not spaces
  non_space_segments <- sapply(segments, 
                               function(x) 
                                 !grepl("^\\s+$", x))
  
  return(non_space_segments)
}

#### Determine the task number of the lowest Qm from base PMF runs  ####
# base_file = readLines("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_PMF_noGUI_noCsub_AllData/CSN_C_6_F_9_2011-17_base_result.txt")

lowest_Qm_task <- function(base_report_file) {
  
  ## Determine the number of lines to read
  # correlations was quoted with "correlations", use slash "\" to write a double quote character
  "factor \"correlations\" with Best-fit factors"
  line.start = line_number(base_report_file,
                           "Qmtrue, gradnorm, #posoutl, #negoutl, endcode")
  
  end.line = line.start + 21
  start.line = line.start + 2
  
  # Extract the lines including Q values and task numbers
  Q_lines <- base_report_file[start.line:end.line]
  
  # Convert the selected lines (with Q values & task number) into a data frame
  Q_task <- read.table(text = Q_lines,
                       header = F,
                       quote = "\"'")
  # colnames(Q_task) = c("TaskNum", "Qmain", "Qaux", "Q.XX")
  colnames(Q_task) = c("TaskNum", "iterNum", "Q", "Qmain", "Qaux", "Qmtrue",
                       "gradnorm", "posoutlNum", "negoutlNum", "endcode")
  
  Q_task_converge = subset(Q_task, endcode == 4)
  
  # Find the number of task when the value of Qm is the lowest
  lowest_Qm_task <- Q_task_converge$TaskNum[which.min(Q_task_converge$Qmain)]
  
  lowest_Qm = Q_task_converge$Qmain[Q_task_converge$TaskNum == lowest_Qm_task]
  converge_percent = nrow(Q_task_converge)/nrow(Q_task)
  
  return(list(lowest_Qm_task = lowest_Qm_task, 
              lowest_Qm = lowest_Qm,
              converge_percent = converge_percent))
}

#### Extract data in Base Model results for selected task number ####
# time series info from base output
base_results <- function(base_output, task, input.row) {
  # Extract Factor matrix AA & BB from base result
  # the regular expression pattern
  pattern_start <- paste0("Results written by postprocessing for task #\\s+", task, "\\s+---------------------------")
  pattern_end <- paste0("Results written by postprocessing for task #\\s+", task+1, "\\s+---------------------------")
  pattern_end_20 = "task#\\s+Qmain\\s+Qaux\\s+Q\\(XX\\)\\s+dQ<0"
  pattern_end = ifelse(task < 20, pattern_end, pattern_end_20)
  
  # detect the line number where the pattern appears
  base.start.line = grep(pattern_start, base_output)
  base.end.line = grep(pattern_end, base_output)
  
  # data for the selected task number
  # the base run results for the selected task
  base_selectQtask = 
    base_output[
      (base.start.line+3):
        (base.end.line-4)]
  
  #### Extract time series normalized contributions
  ts.start = grep("Factor matrix AA", 
                  base_selectQtask) + 1
  
  # Identify segments that are not spaces
  info_segments = non_space_segments(base_selectQtask[ts.start])
  
  # Estimate the factor number
  factor.No <- sum(info_segments) - 1
  
  base_ts_txt = 
    base_selectQtask[
      ts.start : 
        (ts.start + input.row - 1)]
  
  # lines to dataframe
  base_ts = line_to_df(base_ts_txt, input.row, factor.No+2)
  base_ts$X1 = NULL
  base_ts = mutate_all(base_ts, as.numeric)
  
  # rename the columns and replace the first column values
  colnames(base_ts)[1] = "Serial.No"
  colnames(base_ts)[2:(factor.No+1)] = Factor.serial
  
  #### Extract & estimate base concentration & percent 
  conc.start = grep("Factor matrix BB", 
                    base_selectQtask) + 1
  conc.end = grep("Factor matrix CC", 
                    base_selectQtask) - 4
  
  base_conc_txt = 
    base_selectQtask[
      conc.start:conc.end]
  
  # lines to dataframe
  base_conc = line_to_df(base_conc_txt, 
                         conc.end - conc.start + 1, 
                         factor.No+2)
  sapply(base_conc, class)
  base_conc = mutate_all(base_conc, as.numeric)
  base_conc$X1 = NULL

  # rename the columns and replace the first column values
  colnames(base_conc)[1] = "Species"
  colnames(base_conc)[2:(factor.No+1)] = Factor.serial
  
  ##### Extract factor-specific contribution
  
  ## overall fraction contribution
  factor_concen_contri = base_conc[nrow(base_conc), ]
  factor_concen_contri_sum = sum(factor_concen_contri[1, -1])
  factor_fraction_contri = factor_concen_contri/factor_concen_contri_sum
  factor_fraction_contri$Species = factor_concen_contri$Species
  
  ##  factor-specific overall concentration
  factor_concen <- as.numeric(factor_concen_contri[1, -1])
  
  # apply the multiply to base_ts, excluding the first column
  base_ts_conc <- base_ts
  base_ts_conc[,-1] <- sweep(base_ts[,-1], 2, factor_concen, "*")
  
  # estimate overall fraction contribution based on base_conc
  base_fraction = factor_concen_contri[1, -1] / sum(factor_concen)
  
  #### Estimate the predicted daily contribution of each species

  # initialize an empty matrix to store the species-specific daily concentraion contributions 
  daily_contribution_matrix <- 
    matrix(0, nrow = nrow(base_ts), 
           ncol = nrow(base_conc))
  
  # column names for the matrix will be species names from base_conc
  colnames(daily_contribution_matrix) <- base_conc$Species # to be replaced in later process
  
  # Loop through each species to calculate daily contributions
  # multiply and sum the contributions
  base_ts_factors <- base_ts[-1]
  
  for(i in 1:nrow(base_conc)) {
    factor_species_conc <- as.numeric(base_conc[i, -1])
    
    daily_contribution_matrix[, i] <- 
      rowSums(t(t(base_ts_factors) * 
                  factor_species_conc))
  }

  predict_daily_species_conc <- 
    data.frame(Serial.No = base_ts$Serial.No, 
               daily_contribution_matrix)
  
  #### Extract fitted G vs. reference G Regression matrix
  # fitted G vs. reference G, regression
  #G.correl.start = grep("Regression matrix T1 of fitted G vs. reference G:", 
  #                      base_selectQtask) + 1
  
  # fitted G vs. reference G regression matrix
  #base_G_correl_txt = 
  #  base_selectQtask[
  #    G.correl.start : 
  #      (G.correl.start + factor.No - 1)]
  
  # lines to dataframe
  #base_G_cor = line_to_df(base_G_correl_txt, factor.No, factor.No+2)
  #base_G_cor$X1 = NULL
  #base_G_cor = mutate_all(base_G_cor, as.numeric)
  
  # rename the columns and replace the first column values
  #colnames(base_G_cor)[1] = "Factors"
  #Factor.serial = paste0("Factor", 1:factor.No)
  #colnames(base_G_cor)[2:(factor.No+1)] = Factor.serial
  #base_G_cor$Factors = Factor.serial
  
  
  # Return a list containing both data frames
  return(list(#base_G_cor = base_G_cor, 
              base_ts = base_ts, 
              base_contri = base_conc,
              base_ts_conc = base_ts_conc, 
              base_fraction = base_fraction,
              predict_daily_species_conc = predict_daily_species_conc))
}

#### Match site & date, preparing for base model result plotting  #### 

time_series = function(base_ts, site_date, factor.No){
  base_ts_all = base_ts_date = base_ts
  
  # match "Date", "SiteCode", "PM2.5", "State" info
  # base_ts_all[c("Date", "SiteCode", "PM2.5", "State")] <- 
  #   site_date[c("Date", "SiteCode", "PM2.5", "State")]
  base_ts_all[c("Date", "PM2.5")] <- 
    site_date[c("Date", "PM2.5")]
  base_ts_all$species.sum = rowSums(base_ts_all[, 2:(factor.No+1)])
  sapply(base_ts_all, class)
  cor_PM_sumSpecies = cor(base_ts_all$PM2.5, 
                          base_ts_all$species.sum)
  
  # base_ts_date[c("Date", "SiteCode")] <- 
  #   site_date[c("Date", "SiteCode")]
  base_ts_date$Date = site_date$Date
  base_ts_date$Serial.No = NULL
  
  #### 1. Gather the data for time series plotting
  base_ts_gather = gather(base_ts_date, 
                          "Factor", 
                          "Normalize_contri", 
                          -Date) # , -SiteCode
  
  #### 2. Linear regression resutls - contributions 
  # OLS multiple regression model without an intercept using all other columns
  ts_PM_lm = lm(PM2.5 ~ . - 1, 
                data = base_ts_all[, 
                                   c("PM2.5", 
                                     paste0("Factor", 1:factor.No))])
  ts_PM_lm_beta = summary(ts_PM_lm)$coefficients[, 1]
  ts_PM_lm_beta = data.frame(ts_PM_lm_beta)
  colnames(ts_PM_lm_beta) = "lm.beta.site"
  ts_PM_lm_beta$Factor = rownames(ts_PM_lm_beta)
  ts_PM_lm_beta$Factor.contribution = (ts_PM_lm_beta$lm.beta.site/
                                         sum(ts_PM_lm_beta$lm.beta.site))*100
  # keep three significant digits
  ts_PM_lm_beta$Factor.contr = paste0(
    signif(ts_PM_lm_beta$Factor.contribution, 
           3),
    "%")
  
  base_ts_gather$Date = as.Date(base_ts_gather$Date)
  
  # Return a list containing both data frames
  return(list(base_ts_nmContri_spread = base_ts_date,
              base_ts_nmContri_gather = base_ts_gather, 
              ts_PM_lm_beta = ts_PM_lm_beta,
              cor_PM_sumSpecies = cor_PM_sumSpecies,
              base_ts_nmContri_all = base_ts_all))
}


####### Convert concentration contribution to fraction/percent contribution ####### 

conc_percent_contri = function(conc_contribution){
  
  # get the percent contribution
  all_rows = data.frame(Rows = conc_contribution[, 1])
  percent_value = 
    signif(
      conc_contribution[, -1] *100 / 
        rowSums(conc_contribution[, -1]), 
      2)
  percent_contribution = cbind(all_rows, 
                               percent_value)
  
  return(percent_contribution)
}




#### Extract info in Displacement results DISP and prepare for plotting #### 

disp_analysis = function(disp_output){
  # Extract lines with values
  disp_output_noNull = disp_output[! grepl('^\\s*$', disp_output)]
  
  # Get the second line in DISPres1.txt file to get the two numbers evaluating DISP performance
  DISPres1_sum = disp_output_noNull[1]
  
  # Split the string and remove empty elements
  disp.numbers <- strsplit(DISPres1_sum, "\\s+")
  disp.numbers <- disp.numbers[[1]][disp.numbers[[1]] != ""]
  
  # Convert to numeric
  disp.numbers <- as.numeric(disp.numbers)
  
  # Extract the two numbers
  disp.error.code <- disp.numbers[1]
  disp.qdrop <- disp.numbers[2]
  
  # Extract lines with values
  disp_output_matrix = disp_output_noNull[6:length(disp_output_noNull)]
  matrix_line = length(disp_output_matrix)/4
  
  # Extract up and down CI from DISP
  disp_down = disp_output_matrix[1:(matrix_line-1)]
  disp_up = disp_output_matrix[(matrix_line+1):(matrix_line*2-1)]
  
  # Identify segments that are not spaces
  info_segments = non_space_segments(disp_down[1])
  
  # Estimate the factor number
  factor.No <- sum(info_segments)
  
  Factor.serial = paste0("Factor", 1:factor.No)
  
  # lines to dataframe
  disp_down_df = line_to_df(disp_down, 
                            matrix_line - 1, 
                            factor.No + 1)
  disp_down_df = mutate_all(disp_down_df, as.numeric)
  colnames(disp_down_df)[1] = "Species"
  colnames(disp_down_df)[2:(factor.No+1)] = Factor.serial
  
  disp_up_df = line_to_df(disp_up, 
                          matrix_line - 1, 
                          factor.No + 1)
  disp_up_df = mutate_all(disp_up_df, as.numeric)
  colnames(disp_up_df)[1] = "Species"
  colnames(disp_up_df)[2:(factor.No+1)] = Factor.serial
  
  return(list(disp.error.code, disp.qdrop,
              disp_down = disp_down_df, 
              disp_up = disp_up_df))
}

#### BS mapping fraction #### 

bs_map <- function(bs_output, bs.number, factor.No, r.threshold) {
  
  ## Determine the number of lines to read
  # correlations was quoted with "correlations", use slash "\" to write a double quote character
  "factor \"correlations\" with Best-fit factors"
  line.start = line_number(bs_output, 
                           "factor \"correlations\" with Best-fit factors")
  
  end.line = line.start + bs.number 
  start.line = line.start + 1 
  
  # Extract the lines including Q values and task numbers
  corr_Q_lines <- bs_output[start.line:end.line]
  
  # Convert the selected lines (with Q values & task number) into a data frame
  corr_lines <- read.table(text = corr_Q_lines, 
                           header = F, 
                           quote = "\"'")[, 5:(5+factor.No-1)]
  
  
  # Estimate the fraction of BS-r above the threshold 
  percent_less_than_0_6 <- 
    sapply(corr_lines, 
           function(column) {
             mean(column > r.threshold) 
           })
  
  # percent_less_than_0_6 = data.frame(matrix(percent_less_than_0_6, 1))
  percent_less_than_0_6 = data.frame(percent_less_than_0_6)
  colnames(percent_less_than_0_6) = "BS_map"
  percent_less_than_0_6$Factor = paste0("Factor", 1:factor.No)
  percent_less_than_0_6$BS_map_fra = 
    paste0(round(percent_less_than_0_6$BS_map * 100, 0), "%")

  BS_overall = mean(percent_less_than_0_6$BS_map)
  
  return(list(
    percent_less_than_0_6 = percent_less_than_0_6,
    BS_overall = BS_overall))
}

#### N main species #### 

Nmain_Species = function(percent_contribution, N){
  
  percent_contribution = subset(percent_contribution,
                                !(Species %in% c("PM25", "PM2.5")))
  
  rownames(percent_contribution) = percent_contribution[, 1]
  percent_contribution[, 1] = NULL
  
  # get the rownames of which the %ofSpecies ranks top N of the column
  N_main_Species = data.frame(
    apply(
      percent_contribution, 
      2, 
      function(x) 
        rownames(percent_contribution)
      [order(x, decreasing = T)[1:N]]
    ))
  
  # combine the selected rownames into one cell
  mainN_Species = data.frame(
    apply(N_main_Species, 2, 
          function(x) 
            paste(x, collapse = " ")
    ))
  
  colnames(mainN_Species)[1] = paste0("Main_Species")

  # Making the row names as the first column by rearranging the columns
  mainN_Species$Factor = rownames(mainN_Species)
  rownames(mainN_Species) = NULL
  mainN_Species <- mainN_Species[, c(ncol(mainN_Species), 
                                     1:(ncol(mainN_Species) - 1))]
  
  return(mainN_Species)
}


Nmain_Species_with_Contribution <- function(percent_contribution, N) {
  percent_contribution = subset(percent_contribution,
                                !(Species %in% c("PM25", "PM2.5")))
  
  rownames(percent_contribution) = percent_contribution[, 1]
  percent_contribution[, 1] = NULL
  
  N_main_Species = data.frame(
    apply(
      percent_contribution, 
      2, 
      function(x) 
        rownames(percent_contribution)[
          order(x, decreasing = T)[1:N]]
    ))
  
  N_main_Contribution = data.frame(
    apply(
      percent_contribution, 
      2, 
      function(x) 
        x[
          order(x, decreasing = T)[1:N]]
    ))
  
  colnames(N_main_Species)[1] = paste0("Main_Species")
  colnames(N_main_Contribution)[1] = paste0("Contribution")
  
  combined_data = cbind(N_main_Species, N_main_Contribution)
  
  combined_data$Factor = rownames(combined_data)
  rownames(combined_data) = NULL
  
  combined_data <- combined_data[, c(ncol(combined_data), 1:(ncol(combined_data) - 1))]
  
  return(combined_data)
}

Determine_Source <- function(main_species_data, percent_thresholds) {
  # Assuming the structure is similar to what's outputted by `Nmain_Species_with_Contribution`
  # and that `percent_thresholds` is a named vector with species names and corresponding thresholds
  
  source_determined = apply(main_species_data, 1, function(row) {
    species_list = unlist(strsplit(as.character(row["Main_Species"]), " "))
    contribution_list = as.numeric(unlist(strsplit(as.character(row["Contribution"]), " ")))
    names(contribution_list) = species_list
    
    surpass_threshold = contribution_list > percent_thresholds[species_list]
    
    # Returns TRUE if all species surpass their thresholds, FALSE otherwise
    return(all(surpass_threshold))
  })
  
  return(source_determined)
}

#### Replace characters in txt file on given position #### 

# for test
# base_par = readLines("/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp/CSN_CMD_txt_2023.05.15/Cluster_1/Factor_6/iniparams_base_C_1_F_6.txt")
# base_par = readLines("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_no_GUI/iniparams_templates/iniparams_DISP_3.txt")

# For matching a "0" only when it's the last character on the line:
pattern <- "0$"
# For matching a standalone "0" that's not part of another number:
pattern <- "\\b0\\b"

#  function to replace a substring within a specific range in a string
substr_replace <- function(string, replacement, start, stop) {
  paste0(substr(string, 1, start - 1), 
         replacement, 
         substr(string, 
                stop + 1, 
                nchar(string)))
}

# function to replace the numoldsol
numoldsol = 
  function(params_file, minQ_task_No){ # minQ_task_No is the task number with minimum Q value
    # Find the one containing "numoldsol"
    line.numoldsol = line_number(params_file, "numoldsol")
    
    # Get the next line (the one below "numoldsol")
    line_to_replace <- params_file[line.numoldsol + 1]
    
    # Find the position of the last space before the "0"
    position_to_replace <- regexpr("\\s0\\s*$", line_to_replace)
    
    # Replace the "0" with the replacement value if found
    if (position_to_replace > 0) {
      params_file[line.numoldsol + 1] <- substr_replace(
        line_to_replace, 
        minQ_task_No, 
        position_to_replace + 1, 
        position_to_replace + 1)
    }

return(params_file)
}


replace_nth_segment <- function(line_to_replace, n, new_number) {
  segments <- unlist(strsplit(line_to_replace, 
                              "(?<=\\s)(?=\\S)|(?<=\\S)(?=\\s)", 
                              perl = TRUE))
  
  # Identify segments that are not spaces
  non_space_segments_info <- sapply(segments, 
                               function(x) 
                                 !grepl("^\\s+$", x))
  
  # Find the index of the nth non-space segment
  segment_to_replace <- which(non_space_segments_info)[n]
  
  # Determine the new value, keeping the original amount of space
  # Not in need
  # replacement <- sprintf("%*s", nchar(segments[segment_to_replace]), new_number)
  
  # Replace the segment
  # segments[segment_to_replace] <- replacement
  segments[segment_to_replace] <- new_number
  
  # Reconstruct the line
  line_replaced <- paste0(segments, collapse = "")
  
  return(line_replaced)
}


# Function to replace the row number
num_row <- function(params_file, row.number) {
  line.detect <- line_number(params_file, "n1,")
  line_to_replace <- params_file[line.detect + 1]
  line_replaced <- replace_nth_segment(line_to_replace, 1, row.number) # 1st number
  params_file[line.detect + 1] <- line_replaced
  return(params_file)
}

# Function to replace the variable number
num_var <- function(params_file, var.number) {
  line.detect <- line_number(params_file, "n2,")
  line_to_replace <- params_file[line.detect + 1]
  line_replaced <- replace_nth_segment(line_to_replace, 2, var.number) # 2nd number
  params_file[line.detect + 1] <- line_replaced
  return(params_file)
}

# Function to replace the factor number
num_factor <- function(params_file, factor.number) {
  line.detect <- line_number(params_file, "np,")
  line_to_replace <- params_file[line.detect + 1]
  line_replaced <- replace_nth_segment(line_to_replace, 3, factor.number) # 3rd number
  params_file[line.detect + 1] <- line_replaced
  return(params_file)
}

# Function to replace the row number, variable number, and factor number
row_var_factor <- function(params_file, row.number, var.number, factor.number, unc.level) {
  line.detect <- line_number(params_file, "n1,")
  line_to_replace <- params_file[line.detect + 1]
  
  line_replaced_n1 <- replace_nth_segment(line_to_replace, 1, row.number) # n1
  line_replaced_n12 <- replace_nth_segment(line_replaced_n1, 2, var.number) # n2
  line_replaced_n12p <- replace_nth_segment(line_replaced_n12, 3, factor.number) # np
  line_replaced_n12pu <- replace_nth_segment(line_replaced_n12p, 5, unc.level) # np
  
  params_file[line.detect + 1] <- line_replaced_n12pu
  return(params_file)
}


# base_par[28:32]
# num_row(base_par, 294)[28:32]
# num_var(base_par, 294)[28:32]
# num_factor(base_par, 294)[28:32]
# row_var_factor(base_par, 294, 456, 9)[28:32]

# Function to replace the input and output file names for Base Model Run
base_input_output <- function(params_file, input.csv, output.prefix) {
  line.detect.csv <- line_number(params_file, "main data file")
  csv_to_replace <- params_file[line.detect.csv + 1]
  input.csv = paste0("'", input.csv, "'")
  line_replaced_csv <- replace_nth_segment(csv_to_replace, 2, "'PMF_ab_base.dat'") 
  line_replaced_csv <- replace_nth_segment(line_replaced_csv, 1, input.csv) 
  params_file[line.detect.csv + 1] <- line_replaced_csv
  
  line.detect.output <- line_number(params_file, "naming for output")
  output_to_replace <- params_file[line.detect.output + 1]
  output.prefix = paste0("'", output.prefix, "'")
  line_replaced_output <- replace_nth_segment(output_to_replace, 1, output.prefix) 
  params_file[line.detect.output + 1] <- line_replaced_output
  
  return(params_file)
}

# base_par[36:40]
# base_input_output(base_par, "dddd.csv", "aaa")[36:40]


# Function to replace the input and output file names for Bootstrap and Displacement Model Runs
bs_disp_input_output <- function(params_file, input.csv, input.dat, output.prefix) {
  # Detect the line for input files
  line.detect.csv <- line_number(params_file, "main data file")
  csv_to_replace <- params_file[line.detect.csv + 1]
  
  # Change the line for input files
  input.csv = paste0("'", input.csv, "'")
  input.dat = paste0("'", input.dat, "'")
  line_replaced_csv <- replace_nth_segment(csv_to_replace, 1, input.csv) 
  line_replaced_csv_dat <- replace_nth_segment(line_replaced_csv, 2, input.dat) 
  params_file[line.detect.csv + 1] <- line_replaced_csv_dat
  
  
  line.detect.output <- line_number(params_file, "naming for output")
  output_to_replace <- params_file[line.detect.output + 1]
  output.prefix = paste0("'", output.prefix, "'")
  line_replaced_output <- replace_nth_segment(output_to_replace, 1, output.prefix) 
  params_file[line.detect.output + 1] <- line_replaced_output
  
  return(params_file)
}

# base_par[36:40]
# bs_disp_input_output(base_par, "dddd.csv", "aewe2.dat", "aaa_")[36:40]

# Function to rewrite the matrix DISPBCMASK of dimensions np x n2
dispbcmask_rp = function(params_file, dispbcmask, n){
  # dispbcmask is the weak, strong combination pattern, "0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0"
  # n is the factor number
  line_start = line_number(params_file, "Insert matrix DISPBCMASK") + 6
  line_end = line_number(params_file, "If dobspull=-2, insert also 4") - 2
  
  dispbcmask = rep(dispbcmask, n)
  
  params_file_new = 
    c(params_file[1:line_start], 
    dispbcmask, 
    params_file[line_end:length(params_file)])
  
  return(params_file_new)
}

# dispbcmask = "0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t"
# dispbcmask_rp(base_par, dispbcmask, 6)

#### source contribution estimation of each time period ####
source_time_periods <- function(df, time_col) {
  # Unique time periods
  unique_periods <- unique(df[[time_col]])
  
  # Create a workbook
  period_source <- NULL
  
  # Process each time period
  for (period in unique_periods) {
    # Subset data for the period
    base_period <- subset(df, df[[time_col]] == period)
    base_period[[time_col]] <- NULL  # Remove the time period column
    
    # Perform linear regression
    # OLS multiple regression model without an intercept using all other columns
    period_PM_lm <- lm(PM25 ~ . - 1, data = base_period)
    period_PM_lm_beta <- summary(period_PM_lm)$coefficients[, 1]
    period_PM_lm_beta <- data.frame(period_PM_lm_beta[-1])
    colnames(period_PM_lm_beta) <- "lm.beta.site"
    period_PM_lm_beta$Source <- rownames(period_PM_lm_beta)
    rownames(period_PM_lm_beta) = 1:nrow(period_PM_lm_beta)
    period_PM_lm_beta$Source.percent <- 
      (period_PM_lm_beta$lm.beta.site / sum(period_PM_lm_beta$lm.beta.site)) * 100
    period_PM_lm_beta$Source.concentration <-
      period_PM_lm_beta$Source.percent * mean(base_period$PM25) / 100
    
    period_PM_lm_beta$mean.PM25 = mean(base_period$PM25) 
    period_PM_lm_beta$median.PM25 = median(base_period$PM25) 
    period_PM_lm_beta$PM.range = 
      paste0(min(base_period$PM25), "-", max(base_period$PM25))
    period_PM_lm_beta$Period = period
    
    # Add to summary
    period_source = rbind(period_source, period_PM_lm_beta)
  }
  
  return(period_source)
}

##########################################################################################
############ 4. DATA MANAGEMENT ############ 
##########################################################################################

#### median that exists in the dataset  ####

custom_median <- function(x) {
  sorted_x <- sort(x)
  n <- length(sorted_x)
  if (n %% 2 == 1) {
    # Odd number of elements
    return(sorted_x[(n + 1) / 2])
  } else {
    # Even number of elements, return lower or upper middle element
    # Change the following line to return sorted_x[n / 2 + 1] for the upper middle element
    return(sorted_x[n / 2])
  }
}


#### Create cluster & sub-factor folders  ####

create_dir = function (pathway, clusterNum, factorNum) {
  # example of input, pathway = "/CSN_CMD_txt/"
  # example of input, clusterNum = paste0("Cluster_", 1:25)
  # example of input, factorNum = paste0("Factor_", 6:11)
  for (i in clusterNum){
    if(!dir.exists(
      paste0(
        pathway, i))){
      dir.create(
        file.path(
          paste0(
            pathway, i)),
        recursive = TRUE)
    }
    for (j in factorNum){
      if(!dir.exists(
        paste0(
          pathway, i, "/", j))){
        dir.create(
          file.path(
            paste0(
              pathway, i, "/", j)), 
          recursive = TRUE)
      }
    }
  }
}

#### Combine files with the same name pattern  ####

# function to generate the file name based on Cluster and Factor
generate_filename <- function(cluster, factor, patternPre, patternPost) {
  sprintf(paste0(patternPre, "C_%d_F_%d", patternPost), 
          cluster, factor)
}

# function to read and combine files
read_and_combine_files <- function(combination, patternPre, patternPost, file_path) { 
  
  # combination includes all combinations of Cluster and Factor
  file_name <- generate_filename(combination$Cluster, combination$Factor, patternPre, patternPost)
  
  file_paths_complete <- paste0(file_path, file_name)
  
  # Initialize an empty list to store data frames
  data_frames <- list()
  
  # Iterate over each file path and read the file if it exists
  for (file_path_complete in file_paths_complete) {
    if (file.exists(file_path_complete)) {
      data_frames[[length(data_frames) + 1]] <- read_csv(file_path_complete)
    }
  }
  
  # Combine all data frames into one and return
  if (length(data_frames) > 0) {
    return(bind_rows(data_frames))
  } else {
    return(NULL)
  }
}


#### Source profile figure, Transfer data (percent contribution) to logged value, and match the scale used in logged normalized contribution #### 
# assuming the value is in percentage (0-100) and needs to be scaled between log_min (log(1e-05)) to log_max (e.g., log(1e-01))
trans_log <- function(value, log_max, log_min) {
  log_trans = log(value + 1) / 
    log(101) * 
    (log_max - log_min) + 
    log_min
  
  return(log_trans)
}


####  Perform linear regression and return the slope #### 
get_slope <- function(df, x, y) {
  formula <- as.formula(paste(y, "~", x))
  model <- lm(formula, data = df)
  slope <- coef(model)[x]
  return(slope)
}

#### Reorder columns for datasets with PM2.5 and species  ####

species_col_reorder = function(species_df) {
  
  species_df = plyr::rename(species_df, 
                            c("PM2.5" = "PM25", 
                              "Cl-" = "ClIon",
                              "NO3" = "NO3Ion",
                              "SO4" = "SO4Ion",
                              "NH4" = "NH4Ion",
                              "NH4+" = "NH4Ion",
                              "K+" = "KIon",
                              "Na+" = "NaIon",
                              "NH4." = "NH4Ion",
                              "K." = "KIon",
                              "Na." = "NaIon"))
  
  # rearrange the sequence of columns
  OC.EC = c("EC", "EC1", "EC2", "EC3", 
            "OC", "OC1", "OC2", "OC3", "OC4",
            "OP")
  
  ions = c("ClIon", "NaIon", "KIon", "NH4Ion", "NO3", "SO4")
  
  species_df = species_df %>%
    select(!(matches(OC.EC)), 
           everything())
  
  species_df = species_df %>%
    select(!(matches(ions)), 
           everything())
  
  # Place PM2.5 at the end
  species_df = species_df %>% relocate(PM25, .after = SO4Ion)
  
  return(species_df)
}

