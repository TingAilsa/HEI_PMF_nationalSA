# Install package(s) if not existing
# if (!requireNamespace("dplyr", quietly = TRUE)) {
#  install.packages("dplyr")
#}

# Load required libraries
# library(dplyr)
# library(readr)

#### 0. functions to use ####
# Determine the line number in txt file including given string  
line_number <- function(lines, string) {
  # Loop through the lines to find the one containing the specific string
  for (i in seq_along(lines)) {
    if (grepl(string, lines[i])) {
      return(i) # Return the line number if found
    }
  }
  return(NULL) # Return NULL if not found
}

# Determine the task number of the lowest Qm from base PMF runs
# base_file = readLines("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_PMF_noGUI_noCsub_AllData/CSN_C_6_F_9_2011-17_base_result.txt")

lowest_Qm_task <- function(base_file) {
  
  ## Determine the number of lines to read
  # correlations was quoted with "correlations", use slash "\" to write a double quote character
  "factor \"correlations\" with Best-fit factors"
  line.start = line_number(base_file, 
                           "factor \"correlations\" with Best-fit factors")
  
  end.line = line.start + 20 
  start.line = line.start + 1 
  
  # Extract the lines including Q values and task numbers
  Q_lines <- base_file[start.line:end.line]
  
  # Convert the selected lines (with Q values & task number) into a data frame
  Q_task <- read.table(text = Q_lines, 
                       header = F, 
                       quote = "\"'")[, 1:4]
  colnames(Q_task) = c("TaskNum", "Qmain", "Qaux", "Q.XX")
  
  # Find the number of task when the value of Qm is the lowest
  lowest_Qm_task <- Q_task$TaskNum[which.min(Q_task$Qmain)]
  
  return(lowest_Qm_task)
}

#  function to replace a substring within a specific range in a string
substr_replace <- function(string, replacement, start, stop) {
  paste0(substr(string, 1, start - 1), 
         replacement, 
         substr(string, 
                stop + 1, 
                nchar(string)))
}

# function to replace the numoldsol
numoldsol_rp = 
  function(params_file, minQ_task_No) { # minQ_task_No is the task number with minimum Q value
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

#### 1. task number when the lowest Q-value is generated ####
# Access the input file name passed as an argument
args <- commandArgs(trailingOnly = TRUE)

base_file <- args[1]
cluster_site_num <- args[2]
factor_num <- args[3]
folder_path <- args[4]

# Read the content of the input base file
base_output = readLines(base_file)

# Find the number of task when the value of Qm is the lowest
lowest_Qm_task = lowest_Qm_task(base_output)

# Output the the number of selected task
cat("The number of Task when the value of Qm is the lowest:", lowest_Qm_task)
# write(lowest_Qm_task, "/Users/TingZhang/Downloads/lowest_Qm_task.txt")
write(lowest_Qm_task, paste0("CSN_C_", cluster_site_num, "_F_", factor_num, "_lowest_Qm_task.txt"))

#### 2. Update the value of numoldsol for other iniparams.txt files ####
# Define the file paths
# param_files <- c("iniparams_BS.txt", 
#                  "iniparams_DISP.txt", 
#                  "iniparams_before_dual.txt", 
#                  "iniparams_BS_DISP.txt")

param_files <- paste0("iniparams_", 
                      c("BS", "DISP"), #, "before_dual", "BS_DISP"
                      "_C_", cluster_site_num, 
                      "_F_", factor_num, 
                      ".txt")

# Replace the value of numoldsol in each file
for (file_name in param_files) {
  # Read the file
  file_path = paste0(folder_path, "/", file_name)
  param_lines <- readLines(file_path)
  
  param_lines_Qm = numoldsol_rp(param_lines, 
                                lowest_Qm_task)

  # Use new names for updated filens
  new_file_name <- sub(".txt", "_use.txt", file_name)
  new_file_path <- paste0(folder_path, "/", new_file_name)
  
  # Write the updated lines back to the file
  write.table(param_lines_Qm, 
              file = new_file_path, 
              sep = "\t",
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE)
}
