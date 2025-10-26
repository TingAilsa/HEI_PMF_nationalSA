#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=Tra_HDpred

#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node, up to 128;
#SBATCH --mem-per-cpu=20G  # memory per CORE

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.err

#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

#SBATCH --time=05-00:00 

#SBATCH --array=1-6250  # 125 days × 10 years × 50 iterations / 10 days = 6250

# Load necessary modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# Get current source and year
SOURCE_TEST="Traffic"
CMAQ_YEAR="2011-2020"

# Run R script
Rscript allYear_ML_holdout_daily_prediction.R "${SLURM_ARRAY_TASK_ID}" "${SOURCE_TEST}" "${CMAQ_YEAR}"