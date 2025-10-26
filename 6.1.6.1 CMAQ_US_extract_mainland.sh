#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=getMainland

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=50GB
#SBATCH --cpus-per-task=4

#SBATCH --time=01-00:00

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_data_process/err_out/%x_%A.out # output file
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_data_process/err_out/%x_%A.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

#SBATCH --array=1-50

# Load necessary modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# Define arrays of sources and periods and iterations
declare -a include_years=("2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019" "2020")
declare -a sources=("Traffic" "Dust" "Sulfate" "Biomass" "Nitrate")

# Calculate indices for current task 
year_idx=$(( (SLURM_ARRAY_TASK_ID - 1) / 5 ))
source_idx=$(( (SLURM_ARRAY_TASK_ID - 1) % 5 ))

# Get current source and year
current_source=${sources[$source_idx]}
current_year=${include_years[year_idx]}

Rscript CMAQ_US_extract_mainland_allYear.R "${current_year}" "${current_source}" 