#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=Train_test

#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=50G  # memory per CORE

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x_%A_%a.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x_%A_%a.err

#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

#SBATCH --time=05-00:00 

#SBATCH --array=1-4  # 4 sources × N years

# Load necessary modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# Define arrays of sources and periods and iterations
declare -a sources=("Sulfate" "Traffic" "Dust" "Biomass")
# declare -a periods=("2011-01_2011-12" "2012-01_2012-12" "2013-01_2012-12" "2014-01_2014-12"
#                    "2015-01_2015-12" "2016-01_2016-12" "2017-01_2017-12" "2018-01_2018-12"
#                    "2019-01_2019-12" "2020-01_2020-12")
declare -a periods=("2017-01_2017-12")

# Calculate indices for current task
source_idx=$(( (SLURM_ARRAY_TASK_ID - 1) % 4 )) # cycles through 0-3 for the 4 sources
period_idx=$(( (SLURM_ARRAY_TASK_ID - 1) / 4 ))  # 200 = 4 sources × 50 iterations
# iteration_idx=$(( ((SLURM_ARRAY_TASK_ID - 1) / 4) % 50 + 1 ))  # Get iteration 1-50

# Get current source and period
current_source=${sources[$source_idx]}
current_period=${periods[$period_idx]}
# current_iteration=$iteration_idx

Rscript ML_hold_train_test.R "${current_source}" "${current_period}" # "${current_iteration}"
