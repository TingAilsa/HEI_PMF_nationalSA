#!/bin/bash

#SBATCH --partition=bigmem
#SBATCH --job-name=YM_ts_P

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=60GB
#SBATCH --cpus-per-task=4

#SBATCH --time=01-00:00

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.out # output file
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

#SBATCH --array=1-36

module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# Define source x model combinations
declare -a sources=("Traffic" "Dust" "Sulfate" "Biomass" "Nitrate" "PM25")
declare -a model_keys=("RF" "GBM" "CSN")
declare -a model_data=("RF_Both" "GBM_Both" "RF_CSN")

# Map task_id to source and model
source_idx=$(( (SLURM_ARRAY_TASK_ID - 1) % 6 ))
model_idx=$(( (SLURM_ARRAY_TASK_ID - 1) / 6 ))

current_source=${sources[$source_idx]}
current_prefix=${model_keys[$model_idx]}
current_md=${model_data[$model_idx]}

echo "Task: ${SLURM_ARRAY_TASK_ID} | Source: ${current_source} | Model: ${current_md}"

Rscript AY_Prediction_ym_ts_Plot.R "${current_source}" "${current_prefix}" "${current_md}"
