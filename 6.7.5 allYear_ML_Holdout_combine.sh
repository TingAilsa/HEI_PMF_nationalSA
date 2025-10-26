#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=HD_combine

#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node, up to 128;
#SBATCH --mem-per-cpu=20G  # memory per CORE

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.err

#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

#SBATCH --time=05-00:00 

#SBATCH --array=1-10  # 5 sources × 2 runs for each

# Load necessary modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# Define arrays of sources and periods and iterations
declare -a sources=("Sulfate" "Traffic" "Dust" "Biomass" "Nitrate")
declare -a iterations=(1 2)

# Calculate indices for current task
source_idx=$(( (SLURM_ARRAY_TASK_ID - 1) % 5 )) 
iteration_idx=$(( (SLURM_ARRAY_TASK_ID - 1) / 5 ))  

# Get current source and period
current_source=${sources[$source_idx]}
iteration_group=${iterations[iteration_idx]}
cmaq_year="2011-2020"

Rscript allYear_ML_Holdout_combine.R "${current_source}" "${cmaq_year}" "${iteration_group}"