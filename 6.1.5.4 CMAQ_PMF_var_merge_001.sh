#!/bin/bash
#SBATCH --partition=bigmem
#SBATCH --job-name=MLvar001

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=64G
#SBATCH --cpus-per-task=4

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_data_process/err_out/%MLvar_%a.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_data_process/err_out/%xMLvar_%a.err

#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=tzhang23@gmu.edu
#SBATCH --time=8:00:00 

## Array jobs
#SBATCH --array=1-120

# Load necessary modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# each array task processes ONE of the 120 monthly files.
TASK=${SLURM_ARRAY_TASK_ID}
YEAR=$(( 2011 + (TASK - 1) / 12 ))
MONTH_NUM=$(( (TASK - 1) % 12 + 1 ))
MONTH=$(printf "%02d" $MONTH_NUM)

Rscript CMAQ_PMF_var_merge_001.R "$YEAR" "$MONTH"
