#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=cmaq_annual

## Specify the needed settings from the server
#SBATCH --nodes=1 
#SBATCH --tasks-per-node=1
#SBATCH --mem-per-cpu=20G
#SBATCH --cpus-per-task=4

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_data_process/err_out/%x_%A_%a.out # output file
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_data_process/err_out/%x_%A_%a.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

## Specify the maximum of time in need
#SBATCH --time=05-00:00  # Total time needed for job: Days-Hours:Minutes


#load modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# run R 
Rscript CMAQ_var_merge_by_variable.R