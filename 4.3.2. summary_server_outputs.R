# Load required libraries
library(base)

args <- commandArgs(trailingOnly = T)

# Read the temporary results file
# temp_file = "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/temp_file_count.txt"
temp_file <- args[1]
lines <- readLines(temp_file)

# Split lines into pmf_output_sum frame
pmf_output_sum <- do.call(rbind, strsplit(lines, ","))
colnames(pmf_output_sum) <- c("Cluster.No", "Factor.No",
                              "total_files", "files_base",
                              "files_BS", "files_DISP",
                              "files_DISPres1","files_QmTaskIniparam")
pmf_output_sum <- as.data.frame(pmf_output_sum, stringsAsFactors = FALSE)

# Extract the number part from Cluster and Factor columns
pmf_output_sum$Cluster.No <- gsub("\\D", "", 
                                  pmf_output_sum$Cluster.No)
pmf_output_sum$Factor.No <- gsub("\\D", "", 
                                 gsub(".*/", "", 
                                      pmf_output_sum$Factor.No))

# Write the CSV file
write.csv(pmf_output_sum, "PMF_Hopper_output_number_summary_2023.08.csv", row.names = F)