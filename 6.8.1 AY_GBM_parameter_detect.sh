#!/bin/bash
#SBATCH --partition=normal
#SBATCH --job-name=GBM_para

#SBATCH --nodes=1  # number of nodes
#SBATCH --ntasks-per-node=1  # tasks per node, up to 128;
#SBATCH --mem-per-cpu=50G  # memory per CORE

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x_%a.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x_%a.err

#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

#SBATCH --time=05-00:00 

#SBATCH --array=1-576  # 6 sources*8*2*6 = 576 combinations

# Load necessary modules
# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran
module load gnu10 openmpi r

# Define arrays
declare -a sources=("Sulfate" "Traffic" "Dust" "Biomass" "Nitrate" "PM25")
declare -a gbm_interaction_depths=(1 2 3 4 5 6 7 8)
declare -a gbm_shrinkage=(0.1 0.01)

# Logic to pick ntree based on shrinkage
# We use indices 0-5 for ntree in both cases
declare -a ntrees_fast=(300 500 800 1000 1500 2000)      # For 0.1
declare -a ntrees_slow=(2000 3000 4000 5000 6000 8000)   # For 0.01

# Calculate indices (Total combinations: 6 sources * 8 depths * 2 shrinkages * 6 ntrees = 576)
# Simplified Indexing:
total_per_source=$((8 * 2 * 6)) # 96
source_idx=$(( (SLURM_ARRAY_TASK_ID - 1) / total_per_source ))
remainder=$(( (SLURM_ARRAY_TASK_ID - 1) % total_per_source ))

depth_idx=$(( remainder / 12 ))
remainder2=$(( remainder % 12 ))

shrinkage_idx=$(( remainder2 / 6 ))
ntrees_idx=$(( remainder2 % 6 ))

# Get current values
CMAQ_YEAR="2011-2020"
SOURCE_TEST="${sources[$source_idx]}"
DEPTH_USE="${gbm_interaction_depths[$depth_idx]}"
SHRINKAGE_USE="${gbm_shrinkage[$shrinkage_idx]}"

# Pick the correct ntree array based on shrinkage_idx
if [ "$shrinkage_idx" -eq 0 ]; then
    NTREES_USE="${ntrees_fast[$ntrees_idx]}"
else
    NTREES_USE="${ntrees_slow[$ntrees_idx]}"
fi

# Run R script (Note the extra argument for shrinkage)
Rscript AY_GBM_HD_parameter_detect.R "${SOURCE_TEST}" "${CMAQ_YEAR}" "${NTREES_USE}" "${DEPTH_USE}" "${SHRINKAGE_USE}"
