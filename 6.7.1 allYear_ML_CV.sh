#!/bin/bash
#SBATCH --partition=normal
#SBATCH --job-name=ay_RF_CV
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem-per-cpu=50G

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_prediction/err_out/%x.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_prediction/err_out/%x.err
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=tzhang23@gmu.edu
#SBATCH --time=05-00:00 

#SBATCH --array=1-4

# Load necessary modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# Define arrays of sources and periods
declare -a sources=("Sulfate" "Traffic" "Dust" "Biomass") # "Traffic"

# Calculate indices for current task, get the source to estimate
source_idx=$(( (SLURM_ARRAY_TASK_ID - 1) % 4 ))
current_source=${sources[$source_idx]}

included_year="2011-2017"

# Run R script with current parameters
Rscript allYear_ML_CV_model "$current_source" "$included_year"