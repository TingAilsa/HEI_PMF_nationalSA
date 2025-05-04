#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=CV_comb

#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=50G  # memory per CORE

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_prediction/err_out/%x_%a.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_prediction/err_out/%x_%a.err

#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

#SBATCH --time=05-00:00 

#SBATCH --array=1-4  # 4 sources

# Load necessary modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# Set source and period
# SOURCE_TEST="Sulfate"  # "Sulfate", "Traffic", "Dust", "Biomass", "Industry", "Nitrate"
CMAQ_PERIOD="2016-01_2016-12"  

Rscript ML_combine_CV_daily.R "${SLURM_ARRAY_TASK_ID}" "${CMAQ_PERIOD}"