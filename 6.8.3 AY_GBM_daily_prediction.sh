#!/bin/bash

#SBATCH --partition=bigmem
#SBATCH --job-name=PM_HDpred

#SBATCH --nodes=1  # number of nodes
#SBATCH --ntasks-per-node=1  # tasks per node, up to 128;
#SBATCH --mem-per-cpu=80G  # memory per CORE

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x_%A.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x_%A.err

#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

#SBATCH --time=05-00:00 

#SBATCH --array=1-18500  # 366 days * 10 years * 50 iterations / 10 days per task = 18500

# Load necessary modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# Get current source and year
SOURCE_TEST="PM25"
shrinkage=0.01

declare -a cmaq_years=(2011 2012 2013 2014 2015 2016 2017 2018 2019 2020)
year_idx=$(( (SLURM_ARRAY_TASK_ID - 1) / 1850 ))
CMAQ_YEAR=${cmaq_years[$year_idx]}

# Within-year task ID (1 to 1850)
WITHIN_YEAR_TASK=$(( (SLURM_ARRAY_TASK_ID - 1) % 1850 + 1 ))

# Run R script
Rscript AY_GBM_daily_prediction.R "${WITHIN_YEAR_TASK}" "${SOURCE_TEST}" "${CMAQ_YEAR}" "${ntree_detect}"