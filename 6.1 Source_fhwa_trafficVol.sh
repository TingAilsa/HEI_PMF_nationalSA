#!/bin/bash
#SBATCH --partition=bigmem
#SBATCH --job-name=traffic_idw
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=64G
#SBATCH --cpus-per-task=4

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/FHWA_annual_projected/err_out/%x.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/FHWA_annual_projected/err_out/%x.err

#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=tzhang23@gmu.edu
#SBATCH --time=01-00:00 

## Array jobs
#SBATCH --array=2012-2020

# Load necessary modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w

# SLURM_ARRAY_TASK_ID is set to each value in the --array range, one per parallel task
YEAR=${SLURM_ARRAY_TASK_ID}

echo "Starting traffic IDW for year: $YEAR on $(hostname) at $(date)"

# pass YEAR to R as a command-line argument 
Rscript traffic_daily_idw.R "${YEAR}"

echo "Finished year: $YEAR at $(date)"