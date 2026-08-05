#!/bin/bash
#SBATCH --partition=bigmem
#SBATCH --job-name=PM_gbm_hd
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=170G
#SBATCH --cpus-per-task=4

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.err

#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=tzhang23@gmu.edu
#SBATCH --time=05-00:00 

#SBATCH --array=1-50  # 50 iterations

# Load necessary modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w

# Set source and period
SOURCE_TEST="PM25" # "Sulfate", "Traffic", "Dust", "Biomass", "Industry", "Nitrate"
CMAQ_PERIOD="2011-2020" # 1-10  ^l 2011-2020,
ntree_detect=1000
interaction_depth_detect=6
shrinkage_detect=0.1

# Run R script
Rscript AY_GBM_HD_model.R "${SLURM_ARRAY_TASK_ID}" "${SOURCE_TEST}" "${CMAQ_PERIOD}" "${ntree_detect}" "${interaction_depth_detect}" "${shrinkage_detect}"
