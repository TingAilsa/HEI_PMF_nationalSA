#!/bin/bash

#SBATCH --partition=bigmem
#SBATCH --job-name=MonPlot

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=60GB
#SBATCH --cpus-per-task=4

#SBATCH --time=01-00:00

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x_%a.out # output file
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x_%a.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

#SBATCH --array=1-12

export TMPDIR=/scratch/$USER/tmp/$SLURM_ARRAY_JOB_ID/$SLURM_ARRAY_TASK_ID
mkdir -p $TMPDIR

module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# Array task 1-12 directly maps to months 1-12
Rscript AY_Prediction_annual_month_Plot.R "monthly" "${SLURM_ARRAY_TASK_ID}"

rm -rf $TMPDIR