#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=HD_para

#SBATCH --nodes=1  # number of nodes
#SBATCH --ntasks-per-node=1  # tasks per node, up to 128;
#SBATCH --mem-per-cpu=50G  # memory per CORE

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x_%A.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x_%A.err

#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

#SBATCH --time=05-00:00 

#SBATCH --array=1-27,36-54,65-216  # No space # 6 sources × 6 mtry × 6 ntree = 216 combinations
## SBATCH --array=1-216  # 6 sources × 6 mtry × 6 ntree = 216 combinations

# Load necessary modules
# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran
module load gnu10 openmpi r

# Define arrays of sources and periods and iterations
declare -a sources=("Sulfate" "Traffic" "Dust" "Biomass" "Nitrate" "PM25")
declare -a rfmtrys=(3 4 5 6 7 8)
declare -a rfntrees=(300 400 500 600 700 800)

# Calculate indices for current task
total_combinations=$((6 * 6))  # mtry × ntree = 36
source_idx=$(( (SLURM_ARRAY_TASK_ID - 1) / total_combinations ))
remainder=$(( (SLURM_ARRAY_TASK_ID - 1) % total_combinations ))
mtry_idx=$(( remainder / 6 ))
ntree_idx=$(( remainder % 6 ))

# Get current values
SOURCE_TEST="${sources[$source_idx]}"
MTRY_USE="${rfmtrys[$mtry_idx]}"
NTREE_USE="${rfntrees[$ntree_idx]}"
CMAQ_YEAR="2011-2020"

# Print for debugging
echo "Task ID: ${SLURM_ARRAY_TASK_ID}"
echo "Source: ${SOURCE_TEST}"
echo "mtry: ${MTRY_USE}"
echo "ntree: ${NTREE_USE}"

# Run R script
Rscript allYear_ML_HD_parameter_detect.R "${SOURCE_TEST}" "${CMAQ_YEAR}" "${MTRY_USE}" "${NTREE_USE}"
