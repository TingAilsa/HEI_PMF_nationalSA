#!/bin/bash
#SBATCH --partition=normal
#SBATCH --job-name=Bio_hold
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem-per-cpu=50G

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.err

#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=tzhang23@gmu.edu
#SBATCH --time=05-00:00 

#SBATCH --array=1-50  # 50 iterations
##SBATCH --array=1-2

# Load necessary modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# Set source and period
SOURCE_TEST="Biomass" # "Sulfate", "Traffic", "Dust", "Biomass", "Industry", "Nitrate"
CMAQ_PERIOD="2011-2017" # 1-10， 2011-2020,

# Run R script
Rscript allYear_ML_Holdout_model.R "${SLURM_ARRAY_TASK_ID}" "${SOURCE_TEST}" "${CMAQ_PERIOD}"
