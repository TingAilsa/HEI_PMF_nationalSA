#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=Train_test

#SBATCH --nodes=1  # number of nodes
#SBATCH --ntasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=80G  # memory per CORE

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.out 

#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

#SBATCH --time=05-00:00 

#SBATCH --array=1-6  # 4 sources × N years

# Load necessary modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0$
module load netcdf-c netcdf-fortran

# Define arrays of sources and periods and iterations
declare -a sources=("Sulfate" "Traffic" "Dust" "Biomass" "Nitrate" "PM25")

# Calculate indices for current task
source_idx=$(( (SLURM_ARRAY_TASK_ID - 1) % 6 ))

# Get current source and period
current_source=${sources[$source_idx]}
cmaq_year="2011-2020"

## If for a single source
#current_source="Nitrate"
#cmaq_year="2011-2020"

Rscript allYear_ML_hold_train_test.R "${current_source}" "${current_period}" 
