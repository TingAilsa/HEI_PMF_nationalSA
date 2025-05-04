#!/bin/bash
#SBATCH --partition=normal
#SBATCH --job-name=Sul_hold
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem-per-cpu=50G

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x_%A_%a.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x_%A_%a.err
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=tzhang23@gmu.edu
#SBATCH --time=05-00:00 

#SBATCH --array=1-50  # 50 iterations

# Load necessary modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# Set source and period
SOURCE_TEST="Sulfate" # "Sulfate", "Traffic", "Dust", "Biomass", "Industry", "Nitrate"
CMAQ_PERIOD="2017-01_2017-12" # 1-10， 2011-2020, "2017-01_2017-12"

# cmaq_period = c("2011-01_2011-12", 
#                 "2012-01_2012-12", 
#                 "2013-01_2013-12",  
#                 "2014-01_2014-12", 
#                 "2015-01_2015-12",  
#                 "2016-01_2016-12", 
#                 "2017-01_2017-12", 
#                 "2018-01_2018-12",  
#                 "2019-01_2019-12", 
#                 "2020-01_2020-12")

# Run R script
Rscript RF_holdout_noUnc_noCoords.R "${SLURM_ARRAY_TASK_ID}" "${SOURCE_TEST}" "${CMAQ_PERIOD}"

