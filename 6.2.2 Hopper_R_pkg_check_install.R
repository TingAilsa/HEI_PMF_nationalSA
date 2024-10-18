# Ensure renv environment is initialized
renv::init()

# Ensure the custom library path is included
# .libPaths(c("/projects/R_pkg_check/R/libs", .libPaths()))

# List of libraries to check and install if missing
required_libraries <- c("sf", "data.table", "raster", "gbm", "caret", "lubridate", "dplyr", "plyr", "base", "ggplot2", "USAboundaries", "terra", "ncdf4", "fst", "grdiExtra", "scales", "tidyr", "deming", "patchwork", "ggsci", "ggpatern", "readxl", "missForest")

# install.packages("raster")

# Remove duplicate entries from the required_libraries
required_libraries <- unique(required_libraries)
length(required_libraries)

# Initialize lists to store libraries that were missing and those that could not be installed
missing_libraries <- c()
failed_installations <- c()


# Function to check and install missing libraries
check_and_install <- function(libraries) {
  for (lib in libraries) {
    if (!requireNamespace(lib, quietly = TRUE)) {
      # If the library is missing, add it to the missing list
      missing_libraries <<- c(missing_libraries, lib)
      
      cat(paste("Attempting to install", lib, "...\n"))
      tryCatch({
        renv::install(lib)
      }, error = function(e) {
        # If the installation fails, add to the failed list
        failed_installations <<- c(failed_installations, lib)
        cat(paste("Failed to install", lib, "\n"))
      })
    } else {
      cat(paste(lib, "is already installed.\n"))
    }
  }
}

# Run the function to check and install libraries
check_and_install(required_libraries)

# Write the missing libraries and failed installations to a file
output_file <- "R_library_install_report.txt"

# Ensure report includes both missing and failed installations, formatted clearly
report <- c("Libraries that were not originally installed:", missing_libraries,
            "\nLibraries that failed to install:", failed_installations)

writeLines(report, output_file)

# Take a snapshot of the renv environment to record the installed packages
renv::snapshot()

# Print a message indicating that the report has been written
cat(paste("Report saved to", output_file))
